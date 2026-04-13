//! Runtime environment: variable scopes, constants, globals, and call frames.

use std::collections::{HashMap, HashSet};

use petgraph::stable_graph::NodeIndex;

use crate::lexer::strings::ParsedString;
use crate::parser::ast::DeclKind;
use crate::runtime::value::RuntimeValue;

use std::fmt;
use std::sync::Arc;

use super::{DefaultDiceRoller, DiceRoller, VmError};

// ─── Environment ──────────────────────────────────────────────────────────────

/// The runtime variable environment.
///
/// Variables live in a stack of scopes; each [`IrNodeKind::EnterScope`] pushes
/// a new scope and the matching [`IrNodeKind::ExitScope`] pops it.  Globals
/// (`global x = …`) are stored in a separate flat map and are never popped.
#[derive(Default, Clone)]
pub struct Environment {
    /// Stack of local scopes; `scopes.last()` is the innermost one.
    scopes: Vec<HashMap<String, RuntimeValue>>,
    /// Scope-parallel fluent variable bindings.
    ///
    /// Each entry corresponds to the same-indexed entry in `scopes`.
    /// When a variable declared with `@fluent` is assigned, its current value is
    /// also stored here under the configured fluent key.
    ///
    /// On [`Self::push_scope`] a new empty `HashMap` is pushed; on
    /// [`Self::pop_scope`] the innermost map is popped, making `@fluent`
    /// bindings automatically scope-bound.
    fluent_bindings: Vec<std::collections::HashMap<String, crate::runtime::value::RuntimeValue>>,
    /// Registered enum types — maps enum name → ordered variant name list.
    enums: HashMap<String, Vec<String>>,
    /// Registered struct types — maps struct name → ordered field name list.
    structs: HashMap<String, Vec<String>>,
    /// Global variables declared with `global`.
    /// These represent persistent game state (e.g. reputation, flags) and are
    /// the values a save-file system would serialise and restore.
    globals: HashMap<String, RuntimeValue>,
    /// Per-scope sets of names declared with `const`.
    ///
    /// One `HashSet` per entry in `scopes`; pushed and popped in lock-step with
    /// `scopes` so that a `const x` inside a label block does not permanently
    /// mark `x` as constant — once the scope is popped the name guard is gone.
    constant_frames: Vec<HashSet<String>>,
    /// Runtime-provided extern values injected by the host before execution.
    /// These are read-only from script code — only the runtime can write them
    /// via [`Environment::provide_extern`]. Never part of a save file.
    externs: HashMap<String, RuntimeValue>,
    /// Pluggable dice-rolling backend; `None` means dice expressions will error.
    ///
    /// Stored behind an [`Arc`] so that cloning an `Environment` (e.g. for
    /// script-decorator sub-environments) propagates the roller cheaply rather
    /// than severing it.
    roller: Option<Arc<dyn DiceRoller>>,
    /// Current function call recursion depth.
    pub(crate) call_depth: usize,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Environment")
            .field("scopes", &self.scopes)
            .field("fluent_bindings", &self.fluent_bindings)
            .field("enums", &self.enums)
            .field("structs", &self.structs)
            .field("globals", &self.globals)
            .field("constant_frames", &self.constant_frames)
            .field("externs", &self.externs)
            .field("roller", &self.roller.as_ref().map(|_| "<DiceRoller>"))
            .field("call_depth", &self.call_depth)
            .finish()
    }
}

impl Environment {
    /// Creates a new environment with a single root scope pre-pushed.
    pub fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
            fluent_bindings: vec![std::collections::HashMap::new()],
            constant_frames: vec![HashSet::new()],
            externs: HashMap::new(),
            roller: Some(Arc::new(DefaultDiceRoller)),
            call_depth: 0,
            ..Default::default()
        }
    }

    /// Returns the current scope nesting depth (number of active scopes).
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Pushes a fresh local scope onto the scope stack.
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.fluent_bindings.push(std::collections::HashMap::new());
        self.constant_frames.push(HashSet::new());
    }

    /// Pops the innermost local scope, discarding all variables in it.
    ///
    /// # Errors
    /// Returns [`VmError::TypeError`](super::VmError::TypeError) if called
    /// when only the root scope is present (`scopes.len() == 1`) — indicating
    /// an `EnterScope`/`ExitScope` imbalance — or if the internal scope
    /// vectors have drifted out of sync.
    pub fn pop_scope(&mut self) -> Result<(), super::VmError> {
        if self.scopes.len() <= 1 {
            return Err(super::VmError::TypeError(
                "pop_scope: already at root scope (EnterScope/ExitScope imbalance)".into(),
            ));
        }
        if self.scopes.len() != self.fluent_bindings.len()
            || self.scopes.len() != self.constant_frames.len()
        {
            return Err(super::VmError::TypeError(format!(
                "pop_scope invariant violated: scopes.len()={} fluent_bindings.len()={} \
                 constant_frames.len()={} — scope vecs have drifted",
                self.scopes.len(),
                self.fluent_bindings.len(),
                self.constant_frames.len()
            )));
        }
        self.scopes.pop();
        self.fluent_bindings.pop();
        self.constant_frames.pop();
        Ok(())
    }

    /// Registers a Fluent variable binding in the innermost scope.
    ///
    /// Called by the VM when processing an `Assign` node whose IR has
    /// `fluent_alias = Some(key)`. The binding is automatically discarded
    /// when the scope is popped.
    ///
    /// Silently overwrites any previous binding with the same `key` in the
    /// innermost scope.
    pub fn set_fluent_binding(&mut self, key: &str, value: crate::runtime::value::RuntimeValue) {
        if let Some(scope) = self.fluent_bindings.last_mut() {
            scope.insert(key.to_string(), value);
        }
    }

    /// Collects all active Fluent variable bindings from every scope.
    ///
    /// Scopes are iterated from outermost to innermost, so inner-scope bindings
    /// overwrite outer-scope ones for the same key — exactly mirroring the
    /// variable lookup behaviour of [`Self::get`].
    ///
    /// Returns a flat `HashMap` ready to be passed to a [`crate::loc::Localizer`].
    pub fn collect_fluent_bindings(
        &self,
    ) -> std::collections::HashMap<String, crate::runtime::value::RuntimeValue> {
        let mut result = std::collections::HashMap::new();
        for scope in &self.fluent_bindings {
            for (k, v) in scope {
                result.insert(k.clone(), v.clone());
            }
        }
        result
    }

    /// Declares or assigns `name` according to `kind`.
    ///
    /// - [`DeclKind::Variable`] — unconditionally insert into the innermost
    ///   scope, implementing **shadowing** (even over constants).
    /// - [`DeclKind::Assignment`] — search outward through the scope chain for
    ///   an existing binding and mutate it in-place; if not found in scopes,
    ///   check globals; if not found anywhere, create in the innermost scope.
    ///   Returns [`VmError::TypeError`] if `name` is a constant.
    /// - [`DeclKind::Global`] — store in the globals map. Returns
    ///   [`VmError::TypeError`] if `name` is a constant.
    /// - [`DeclKind::Constant`] — store in the innermost scope; returns
    ///   [`VmError::TypeError`] if the name was already declared as a constant.
    pub fn set(&mut self, name: &str, value: RuntimeValue, kind: &DeclKind) -> Result<(), VmError> {
        if self.externs.contains_key(name) {
            return Err(VmError::TypeError(format!(
                "cannot assign to extern '{name}' — extern values are controlled by the runtime"
            )));
        }
        if matches!(kind, DeclKind::Assignment | DeclKind::Global)
            && self.constant_frames.iter().any(|f| f.contains(name))
        {
            return Err(VmError::TypeError(format!(
                "cannot assign to constant '{name}'"
            )));
        }

        match kind {
            DeclKind::Global => {
                self.globals.insert(name.to_string(), value);
            }
            DeclKind::Constant => {
                if self.constant_frames.iter().any(|f| f.contains(name)) {
                    return Err(VmError::TypeError(format!(
                        "cannot reassign constant '{name}'"
                    )));
                }
                self.constant_frames
                    .last_mut()
                    .ok_or_else(|| {
                        VmError::TypeError("no active scope for constant declaration".to_string())
                    })?
                    .insert(name.to_string());
                let scope = self.scopes.last_mut().ok_or_else(|| {
                    VmError::TypeError("no active scope for constant declaration".to_string())
                })?;
                scope.insert(name.to_string(), value);
            }
            DeclKind::Variable => {
                // `let` declarations always shadow — unconditionally insert
                // into the innermost scope.
                let scope = self
                    .scopes
                    .last_mut()
                    .ok_or_else(|| VmError::TypeError("no active scope".to_string()))?;
                scope.insert(name.to_string(), value);
            }
            DeclKind::Assignment => {
                // Bare assignment — search innermost local scope first, then
                // outward, then globals.  This mirrors the lookup order used
                // by `get()` so that reads and writes resolve to the same
                // binding.
                for scope in self.scopes.iter_mut().rev() {
                    if scope.contains_key(name) {
                        scope.insert(name.to_string(), value);
                        return Ok(());
                    }
                }
                if self.globals.contains_key(name) {
                    self.globals.insert(name.to_string(), value);
                    return Ok(());
                }
                // No existing binding — create in the innermost scope
                // (permissive implicit declaration).
                let scope = self
                    .scopes
                    .last_mut()
                    .ok_or_else(|| VmError::TypeError("no active scope".to_string()))?;
                scope.insert(name.to_string(), value);
            }
        }
        Ok(())
    }

    /// Looks up `name`, searching from the innermost scope outward, then
    /// globals, then externs.
    ///
    /// # Errors
    /// Returns [`VmError::UndefinedVariable`] if `name` is not found anywhere.
    pub fn get(&self, name: &str) -> Result<RuntimeValue, VmError> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Ok(v.clone());
            }
        }
        if let Some(v) = self.globals.get(name) {
            return Ok(v.clone());
        }
        if let Some(v) = self.externs.get(name) {
            return Ok(v.clone());
        }
        Err(VmError::UndefinedVariable(name.to_string()))
    }

    /// Registers an enum type with its ordered variant names.
    pub fn define_enum(&mut self, name: String, variants: Vec<String>) {
        self.enums.insert(name, variants);
    }

    /// Looks up an enum variant by `(enum_name, variant_name)`, returning the
    /// variant as a [`RuntimeValue::Str`].
    ///
    /// # Errors
    /// Returns [`VmError::UndefinedVariable`] if the enum or variant is missing.
    pub fn get_enum_variant(
        &self,
        enum_name: &str,
        variant_name: &str,
    ) -> Result<RuntimeValue, VmError> {
        let variants = self
            .enums
            .get(enum_name)
            .ok_or_else(|| VmError::UndefinedVariable(format!("enum '{enum_name}'")))?;
        if variants.iter().any(|v| v == variant_name) {
            Ok(RuntimeValue::Str(ParsedString::new_plain(variant_name)))
        } else {
            Err(VmError::UndefinedVariable(format!(
                "variant '{variant_name}' on enum '{enum_name}'"
            )))
        }
    }

    /// Returns a reference to the registered enums (name → variant list).
    pub fn enums(&self) -> &HashMap<String, Vec<String>> {
        &self.enums
    }

    /// Registers a struct type with its ordered field names.
    ///
    /// Executed when the VM processes a [`crate::ir::IrNodeKind::DefineStruct`]
    /// node so that subsequent constructor calls can build
    /// [`crate::runtime::value::RuntimeValue::Struct`] instances.
    pub fn define_struct(&mut self, name: String, fields: Vec<String>) {
        self.structs.insert(name, fields);
    }

    /// Looks up a struct type's ordered field name list.
    ///
    /// Returns `None` if no struct with `name` has been registered via
    /// [`Self::define_struct`].
    pub fn get_struct_schema(&self, name: &str) -> Option<&Vec<String>> {
        self.structs.get(name)
    }

    /// Returns a reference to all registered struct schemas (name → field list).
    pub fn structs(&self) -> &HashMap<String, Vec<String>> {
        &self.structs
    }

    /// Inject a runtime-provided extern value into the environment.
    ///
    /// Must be called by the host runtime **before** the first [`crate::vm::Vm::next`]
    /// call for every name declared with `extern` in the script. May also be called
    /// between steps to update a live extern value.
    ///
    /// Extern values are stored in a dedicated map separate from locals, globals, and
    /// constants. Scripts cannot overwrite them — any `set` attempt on an extern name
    /// returns a [`VmError::TypeError`].
    pub fn provide_extern(&mut self, name: &str, value: RuntimeValue) {
        self.externs.insert(name.to_string(), value);
    }

    /// Replace the dice-rolling backend.
    ///
    /// The supplied [`Box`] is converted into an [`Arc`] internally so that
    /// cloned sub-environments (e.g. for script decorators) share the same
    /// roller without requiring it to be `Clone`.
    pub fn set_dice_roller(&mut self, roller: Box<dyn DiceRoller>) {
        self.roller = Some(Arc::from(roller));
    }

    /// Replace the dice-rolling backend from an existing [`Arc`].
    ///
    /// This is useful when propagating a roller from one environment to
    /// another without re-boxing (e.g. from a caller env to an isolated
    /// function-body env).
    pub(crate) fn set_dice_roller_arc(&mut self, roller: Arc<dyn DiceRoller>) {
        self.roller = Some(roller);
    }

    /// Returns a clone of the current dice-roller [`Arc`], if one is set.
    ///
    /// Callers can pass this to a freshly-created [`Environment`] via
    /// [`Self::set_dice_roller_arc`] to propagate the roller without
    /// requiring the trait to be `Clone`.
    pub(crate) fn dice_roller(&self) -> Option<Arc<dyn DiceRoller>> {
        self.roller.clone()
    }

    /// Roll `count`d`sides` using the registered roller and return the total sum.
    ///
    /// Returns `Err(`[`VmError::NotImplemented`]`)` when no roller is
    /// registered (i.e. the roller was explicitly removed or never set).
    ///
    /// Prefer [`Self::roll_dice_individual`] when per-die results are needed.
    /// This method is retained as a convenience for embedders and tests.
    #[allow(dead_code)]
    pub(crate) fn roll_dice(&self, count: u32, sides: u32) -> Result<i64, VmError> {
        match &self.roller {
            Some(r) => Ok(r.roll(count, sides)),
            None => Err(VmError::NotImplemented(format!(
                "dice evaluation ({}d{}) — no dice roller registered",
                count, sides
            ))),
        }
    }

    /// Roll `count`d`sides` using the registered roller and return each individual die result.
    ///
    /// Returns `Err(`[`VmError::NotImplemented`]`)` when no roller is
    /// registered (i.e. the roller was explicitly removed or never set).
    pub(crate) fn roll_dice_individual(&self, count: u32, sides: u32) -> Result<Vec<i64>, VmError> {
        match &self.roller {
            Some(r) => Ok(r.roll_individual(count, sides)),
            None => Err(VmError::NotImplemented(format!(
                "dice evaluation ({}d{}) — no dice roller registered",
                count, sides
            ))),
        }
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn provide_extern_is_readable() {
        let mut env = Environment::new();
        env.provide_extern("MAX", RuntimeValue::Int(100));
        assert_eq!(env.get("MAX").unwrap(), RuntimeValue::Int(100));
    }

    #[test]
    fn provide_extern_blocks_script_reassignment() {
        let mut env = Environment::new();
        env.provide_extern("score", RuntimeValue::Int(0));
        // Scripts cannot overwrite extern values
        let result = env.set("score", RuntimeValue::Int(42), &DeclKind::Assignment);
        assert!(result.is_err(), "expected error when assigning to extern");
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("extern"),
            "error should mention 'extern': {err_msg}"
        );
    }

    #[test]
    fn provide_extern_runtime_can_update() {
        // The runtime itself can call provide_extern multiple times to update a value
        let mut env = Environment::new();
        env.provide_extern(
            "player_name",
            RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain("Alice")),
        );
        env.provide_extern(
            "player_name",
            RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain("Bob")),
        );
        if let RuntimeValue::Str(s) = env.get("player_name").unwrap() {
            assert_eq!(s.to_string(), "Bob");
        } else {
            panic!("expected Str");
        }
    }

    #[test]
    fn fluent_binding_set_and_collect() {
        let mut env = Environment::new();
        env.set_fluent_binding("gold", RuntimeValue::Int(42));
        let bindings = env.collect_fluent_bindings();
        assert_eq!(bindings.get("gold"), Some(&RuntimeValue::Int(42)));
    }

    #[test]
    fn fluent_binding_is_scope_bound() {
        let mut env = Environment::new();
        env.set_fluent_binding("outer", RuntimeValue::Int(1));
        env.push_scope();
        env.set_fluent_binding("inner", RuntimeValue::Int(2));
        {
            let b = env.collect_fluent_bindings();
            assert_eq!(b.get("outer"), Some(&RuntimeValue::Int(1)));
            assert_eq!(b.get("inner"), Some(&RuntimeValue::Int(2)));
        }
        env.pop_scope().unwrap();
        let b = env.collect_fluent_bindings();
        assert_eq!(b.get("outer"), Some(&RuntimeValue::Int(1)));
        assert_eq!(
            b.get("inner"),
            None,
            "inner-scope fluent binding should be gone after pop"
        );
    }

    #[test]
    fn fluent_binding_inner_shadows_outer() {
        let mut env = Environment::new();
        env.set_fluent_binding("score", RuntimeValue::Int(10));
        env.push_scope();
        env.set_fluent_binding("score", RuntimeValue::Int(99));
        let b = env.collect_fluent_bindings();
        assert_eq!(
            b.get("score"),
            Some(&RuntimeValue::Int(99)),
            "inner scope should shadow outer"
        );
    }

    #[test]
    fn fluent_bindings_empty_by_default() {
        let env = Environment::new();
        assert!(env.collect_fluent_bindings().is_empty());
    }

    /// Popping past the root scope must return an error (both debug and release).
    #[test]
    fn pop_scope_underflow_returns_error() {
        let mut env = Environment::new();
        let err = env.pop_scope().unwrap_err();
        assert!(
            err.to_string().contains("already at root scope"),
            "expected root-scope error, got: {err}"
        );
        assert_eq!(env.depth(), 1, "root scope must survive underflow attempt");
    }

    #[test]
    fn push_and_pop_scope_are_balanced() {
        let mut env = Environment::new();
        assert_eq!(env.depth(), 1);
        env.push_scope();
        assert_eq!(env.depth(), 2);
        env.push_scope();
        assert_eq!(env.depth(), 3);
        env.pop_scope().unwrap();
        assert_eq!(env.depth(), 2);
        env.pop_scope().unwrap();
        assert_eq!(env.depth(), 1);
    }
}

// ─── CallFrame ────────────────────────────────────────────────────────────────

/// Saved execution context pushed when entering a labeled block.
///
/// When [`crate::ir::IrNodeKind::Return`] fires, the top frame is popped and
/// execution resumes at `return_cursor`.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// The node to resume execution at after the return.
    ///
    /// `None` means "end the script" — analogous to the old `NODE_END` sentinel.
    pub return_cursor: Option<NodeIndex>,
    /// The scope depth at the time the frame was pushed (used by Return to
    /// unwind extra scopes).
    pub scope_depth: usize,
    /// If `Some(name)`, store the return value in this variable when returning.
    pub assign_to_var: Option<String>,
}
