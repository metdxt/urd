//! Runtime environment: variable scopes, constants, globals, and call frames.

use std::collections::{HashMap, HashSet};

use crate::ir::NodeId;
use crate::lexer::strings::ParsedString;
use crate::parser::ast::DeclKind;
use crate::runtime::value::RuntimeValue;

use super::VmError;

// ─── Environment ──────────────────────────────────────────────────────────────

/// The runtime variable environment.
///
/// Variables live in a stack of scopes; each [`IrNodeKind::EnterScope`] pushes
/// a new scope and the matching [`IrNodeKind::ExitScope`] pops it.  Globals
/// (`global x = …`) are stored in a separate flat map and are never popped.
#[derive(Debug, Default, Clone)]
pub struct Environment {
    /// Stack of local scopes; `scopes.last()` is the innermost one.
    scopes: Vec<HashMap<String, RuntimeValue>>,
    /// Registered enum types — maps enum name → ordered variant name list.
    enums: HashMap<String, Vec<String>>,
    /// Global variables declared with `global`.
    /// These represent persistent game state (e.g. reputation, flags) and are
    /// the values a save-file system would serialise and restore.
    globals: HashMap<String, RuntimeValue>,
    /// Names that were declared with `const` (immutable after first assignment).
    constants: HashSet<String>,
    /// Read-only structural values injected by the VM — never writable from
    /// script code and never part of a save file.
    ///
    /// Currently used to expose compiled label names as [`RuntimeValue::Label`]
    /// values so they can be passed as arguments (e.g. to decorators) without
    /// being treated as saveable game state.
    builtins: HashMap<String, RuntimeValue>,
}

impl Environment {
    /// Creates a new environment with a single root scope pre-pushed.
    pub fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
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
    }

    /// Pops the innermost local scope, discarding all variables in it.
    ///
    /// Always keeps at least one root scope alive.
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Declares or assigns `name` according to `kind`.
    ///
    /// - [`DeclKind::Variable`] — update an existing binding (searching
    ///   outward from the innermost scope, then globals) or create a new one in
    ///   the innermost scope.
    /// - [`DeclKind::Global`] — store in the globals map.
    /// - [`DeclKind::Constant`] — store in the innermost scope; returns
    ///   [`VmError::TypeError`] if the name was already declared as a constant.
    pub fn set(&mut self, name: &str, value: RuntimeValue, kind: &DeclKind) -> Result<(), VmError> {
        match kind {
            DeclKind::Global => {
                self.globals.insert(name.to_string(), value);
            }
            DeclKind::Constant => {
                if self.constants.contains(name) {
                    return Err(VmError::TypeError(format!(
                        "cannot reassign constant '{}'",
                        name
                    )));
                }
                self.constants.insert(name.to_string());
                let scope = self.scopes.last_mut().ok_or_else(|| {
                    VmError::TypeError("no active scope for constant declaration".to_string())
                })?;
                scope.insert(name.to_string(), value);
            }
            DeclKind::Variable => {
                // Update an existing binding in-place — search innermost local
                // scope first, then outward, then globals.  This mirrors the
                // lookup order used by `get()` so that reads and writes resolve
                // to the same binding.
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
                // No existing binding — create in the innermost scope.
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
    /// globals, then builtins.
    ///
    /// Builtins are consulted last so that script-declared variables and globals
    /// can shadow them, but builtins are never writable via [`Self::set`].
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
        if let Some(v) = self.builtins.get(name) {
            return Ok(v.clone());
        }
        Err(VmError::UndefinedVariable(name.to_string()))
    }

    /// Inserts a read-only builtin value.
    ///
    /// Builtins are structural values injected by the VM itself (e.g. label
    /// references). They are never writable from script code and are never
    /// serialised into a save file.
    ///
    /// Silently overwrites any previous builtin with the same name.
    pub fn define_builtin(&mut self, name: impl Into<String>, value: RuntimeValue) {
        self.builtins.insert(name.into(), value);
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
            .ok_or_else(|| VmError::UndefinedVariable(format!("enum '{}'", enum_name)))?;
        if variants.iter().any(|v| v == variant_name) {
            Ok(RuntimeValue::Str(ParsedString::new_plain(variant_name)))
        } else {
            Err(VmError::UndefinedVariable(format!(
                "variant '{}' on enum '{}'",
                variant_name, enum_name
            )))
        }
    }

    /// Returns a reference to the registered enums (name → variant list).
    pub fn enums(&self) -> &HashMap<String, Vec<String>> {
        &self.enums
    }
}

// ─── CallFrame ────────────────────────────────────────────────────────────────

/// Saved execution context pushed when entering a labeled block.
///
/// When [`IrNodeKind::Return`] fires, the top frame is popped and execution
/// resumes at `return_cursor`.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// The node to resume execution at after the return.
    pub return_cursor: NodeId,
    /// The scope depth at the time the frame was pushed (used by Return to
    /// unwind extra scopes).
    pub scope_depth: usize,
    /// If `Some(name)`, store the return value in this variable when returning.
    pub assign_to_var: Option<String>,
}
