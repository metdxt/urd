//! # VM Module
//!
//! This module implements the Urd virtual machine — the runtime that walks a
//! compiled [`IrGraph`] and emits [`Event`]s for the consumer (game engine,
//! test harness, etc.) to handle.
//!
//! ## Entry points
//!
//! - [`Vm::new`] — create a VM from a compiled [`IrGraph`] and a
//!   [`DecoratorRegistry`]; performs a validation pass over all decorators.
//! - [`Vm::next`] — advance execution by one *observable* step, returning
//!   the next [`Event`] (or `None` when the script ends).
//!
//! ## Architecture
//!
//! The VM is a simple interpreter loop over a flat node arena.  Internal nodes
//! (branches, assignments, jumps …) are consumed silently; only
//! [`IrNodeKind::Dialogue`] and [`IrNodeKind::Choice`] surface as [`Event`]s.
//! The caller drives the loop by repeatedly calling [`Vm::next`], supplying
//! `choice: Some(idx)` only when responding to a [`Event::Choice`].

pub mod loader;

use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    compiler::CompilerError,
    ir::{ChoiceEvent, Event, IrGraph, IrNodeKind, NODE_END, NodeId},
    lexer::strings::{ParsedString, StringPart},
    parser::ast::{AstContent, DeclKind, Decorator, MatchPattern, Operator, UnaryOperator},
    runtime::value::RuntimeValue,
};

// ─── Error type ───────────────────────────────────────────────────────────────

/// Errors that can occur during VM construction or execution.
#[derive(Debug, Error)]
pub enum VmError {
    /// A decorator name used in the script was not registered in the
    /// [`DecoratorRegistry`].
    #[error("unknown decorator '@{name}' used at node {node_id:?}")]
    UnknownDecorator {
        /// The unregistered decorator name.
        name: String,
        /// The IR node at which the unknown decorator appeared.
        node_id: NodeId,
    },

    /// A `jump` targeted a label not present in the compiled graph.
    ///
    /// Should never occur after a successful compilation pass, but is kept as
    /// a defensive runtime error.
    #[error("unknown label '{0}'")]
    UnknownLabel(String),

    /// A variable was referenced before it was declared.
    #[error("undefined variable '{0}'")]
    UndefinedVariable(String),

    /// A runtime type mismatch (e.g. adding a string to an integer).
    #[error("type error: {0}")]
    TypeError(String),

    /// An AST node appeared in an expression context where it is not valid.
    #[error("invalid expression context: {0}")]
    InvalidExpression(String),

    /// The player supplied a choice index that is out of bounds.
    #[error("choice index {index} out of bounds (len={len})")]
    ChoiceOutOfBounds {
        /// The supplied index.
        index: usize,
        /// The number of available options.
        len: usize,
    },

    /// An error propagated from the compiler (e.g. when constructing test graphs).
    #[error("compiler error: {0}")]
    CompilerError(#[from] CompilerError),
}

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
                // Update an existing binding in-place (innermost scope first,
                // then globals) so that re-assignment respects the original scope.
                if self.globals.contains_key(name) {
                    self.globals.insert(name.to_string(), value);
                    return Ok(());
                }
                for scope in self.scopes.iter_mut().rev() {
                    if scope.contains_key(name) {
                        scope.insert(name.to_string(), value);
                        return Ok(());
                    }
                    // keep searching outward
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

// ─── DecoratorRegistry ────────────────────────────────────────────────────────

/// Registry of named decorator handlers.
///
/// Decorator handlers are plain Rust closures that receive the evaluated
/// decorator arguments and return a map of key → value pairs to be merged
/// into the event's `fields`.
/// Type alias for a boxed decorator handler function.
type DecoratorHandler = Box<dyn Fn(&[RuntimeValue]) -> HashMap<String, RuntimeValue> + Send + Sync>;

/// Registry of named decorator handlers used to evaluate `@decorator` annotations
/// attached to [`IrNodeKind::Dialogue`] and [`IrNodeKind::Choice`] nodes.
///
/// Register handlers with [`DecoratorRegistry::register`] before constructing
/// a [`Vm`]; the VM's validation pass will reject any decorator name that is
/// not present in the registry.
pub struct DecoratorRegistry {
    handlers: HashMap<String, DecoratorHandler>,
}

impl std::fmt::Debug for DecoratorRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DecoratorRegistry")
            .field("handlers", &self.handlers.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl Default for DecoratorRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl DecoratorRegistry {
    /// Creates an empty decorator registry.
    pub fn new() -> Self {
        DecoratorRegistry {
            handlers: HashMap::new(),
        }
    }

    /// Registers a decorator handler under `name`.
    ///
    /// The handler receives a slice of evaluated [`RuntimeValue`] arguments
    /// and returns a [`HashMap`] of fields to merge into the emitted event.
    pub fn register(
        &mut self,
        name: impl Into<String>,
        handler: impl Fn(&[RuntimeValue]) -> HashMap<String, RuntimeValue> + Send + Sync + 'static,
    ) {
        self.handlers.insert(name.into(), Box::new(handler));
    }

    /// Returns an iterator over all registered decorator names.
    pub fn known_names(&self) -> impl Iterator<Item = &str> {
        self.handlers.keys().map(String::as_str)
    }

    /// Registers a script-defined decorator by name so that the VM validation
    /// pass accepts `@name(args)` usages in dialogue/choice nodes.
    ///
    /// The handler is a stub that returns an empty fields map; full body
    /// evaluation at apply-time is a future VM extension.
    ///
    /// # Errors
    /// Currently infallible; returns `Ok(())` always.  The `Result` return
    /// type is kept for forward compatibility with richer error handling.
    pub fn define_script_decorator(
        &mut self,
        name: String,
        _event_constraint: crate::parser::ast::EventConstraint,
        _params: Vec<String>,
        _body: crate::parser::ast::Ast,
    ) -> Result<(), VmError> {
        // Register a stub handler so `@name` passes the validation pass in
        // `Vm::new` and can be used in dialogue/choice nodes without a native
        // Rust closure.  Full body evaluation is deferred to a future milestone.
        self.handlers
            .entry(name)
            .or_insert_with(|| Box::new(|_args: &[RuntimeValue]| HashMap::new()));
        Ok(())
    }

    /// Evaluates `decorator`'s arguments and invokes the registered handler.
    ///
    /// # Errors
    /// Returns [`VmError::UnknownDecorator`] (with sentinel node id) if the
    /// decorator name is not registered.  Callers that know the real node id
    /// should override it in the error.
    pub fn apply(
        &self,
        decorator: &Decorator,
        env: &Environment,
    ) -> Result<HashMap<String, RuntimeValue>, VmError> {
        let handler = self.handlers.get(decorator.name()).ok_or_else(|| {
            VmError::UnknownDecorator {
                name: decorator.name().to_string(),
                node_id: NODE_END, // sentinel; callers may override
            }
        })?;

        // Evaluate every argument in the ExprList.
        let args = match decorator.args().content() {
            AstContent::ExprList(items) => {
                let mut evaluated = Vec::with_capacity(items.len());
                for item in items {
                    evaluated.push(eval_expr(item, env)?);
                }
                evaluated
            }
            _ => vec![eval_expr(decorator.args(), env)?],
        };

        Ok(handler(&args))
    }
}

// ─── Expression evaluator ─────────────────────────────────────────────────────

/// Evaluate an [`crate::parser::ast::Ast`] expression to a [`RuntimeValue`].
///
/// This function is pure with respect to the *environment* — it does not
/// mutate it.  Statement-level assignments are handled by
/// [`IrNodeKind::Assign`] nodes and never reach this function in normal
/// compiled code; the `Operator::Assign` arm in expression context simply
/// evaluates the rhs and returns it without storing.
///
/// # Errors
/// Returns [`VmError`] on type errors, undefined variables, or AST nodes that
/// are invalid in an expression context.
pub fn eval_expr(
    ast: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<RuntimeValue, VmError> {
    match ast.content() {
        // ── Literal / identifier ─────────────────────────────────────────────
        AstContent::Value(rv) => eval_runtime_value(rv, env),

        // ── Binary operation ─────────────────────────────────────────────────
        AstContent::BinOp { op, left, right } => eval_binop(op, left, right, env),

        // ── Unary operation ──────────────────────────────────────────────────
        AstContent::UnaryOp { op, expr } => {
            let val = eval_expr(expr, env)?;
            eval_unary(op, val)
        }

        // ── Expression list — evaluate each, return the last ─────────────────
        AstContent::ExprList(items) => {
            let mut last = RuntimeValue::Null;
            for item in items {
                last = eval_expr(item, env)?;
            }
            Ok(last)
        }

        // ── Function call — not yet implemented ──────────────────────────────
        AstContent::Call { func_path, params } => {
            // Evaluate arguments so their side-effects (if any) still run.
            if let AstContent::ExprList(args) = params.content() {
                for arg in args {
                    let _ = eval_expr(arg, env)?;
                }
            }
            let path_str = match func_path.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
                _ => "<unknown>".to_string(),
            };
            log::warn!(
                "function call to '{}' is not yet implemented; returning Null",
                path_str
            );
            Ok(RuntimeValue::Null)
        }

        // ── Collection literals — not yet supported ──────────────────────────
        AstContent::List(_) => {
            log::warn!("List literals are not yet supported; returning Null");
            Ok(RuntimeValue::Null)
        }
        AstContent::Map(pairs) => {
            let mut map = std::collections::HashMap::new();
            for (key_ast, val_ast) in pairs {
                // Map keys written as bare identifiers (e.g. `name:` in
                // `:{name: "x"}`) must be treated as literal string keys, NOT
                // as variable lookups.  Only fall through to full expression
                // evaluation when the key is something else (e.g. a computed
                // string expression).
                let key_str = match key_ast.content() {
                    AstContent::Value(RuntimeValue::IdentPath(p)) => {
                        p.last().cloned().unwrap_or_default()
                    }
                    _ => {
                        let key_val = eval_expr(key_ast, env)?;
                        match &key_val {
                            RuntimeValue::Str(ps) => ps.to_string(),
                            RuntimeValue::IdentPath(p) => p.last().cloned().unwrap_or_default(),
                            other => {
                                return Err(VmError::TypeError(format!(
                                    "map key must be Str or identifier, got {:?}",
                                    other
                                )));
                            }
                        }
                    }
                };
                let val = eval_expr(val_ast, env)?;
                map.insert(key_str, Box::new(val));
            }
            Ok(RuntimeValue::Map(map))
        }

        // ── Invalid expression contexts ──────────────────────────────────────
        AstContent::Block(_) => Err(VmError::InvalidExpression(
            "Block cannot appear in expression context".to_string(),
        )),
        AstContent::If { .. } => Err(VmError::InvalidExpression(
            "If cannot appear in expression context".to_string(),
        )),
        AstContent::Declaration { .. } => Err(VmError::InvalidExpression(
            "Declaration cannot appear in expression context".to_string(),
        )),
        AstContent::LabeledBlock { .. } => Err(VmError::InvalidExpression(
            "LabeledBlock cannot appear in expression context".to_string(),
        )),
        AstContent::Dialogue { .. } => Err(VmError::InvalidExpression(
            "Dialogue cannot appear in expression context".to_string(),
        )),
        AstContent::Menu { .. } => Err(VmError::InvalidExpression(
            "Menu cannot appear in expression context".to_string(),
        )),
        AstContent::MenuOption { .. } => Err(VmError::InvalidExpression(
            "MenuOption cannot appear in expression context".to_string(),
        )),
        AstContent::Return { .. } => Err(VmError::InvalidExpression(
            "Return cannot appear in expression context".to_string(),
        )),
        AstContent::Jump { .. } => Err(VmError::InvalidExpression(
            "Jump cannot appear in expression context".to_string(),
        )),
        AstContent::LetCall { .. } => Err(VmError::InvalidExpression(
            "LetCall cannot appear in expression context".to_string(),
        )),
        AstContent::EnumDecl { .. } => Err(VmError::InvalidExpression(
            "EnumDecl cannot appear in expression context".to_string(),
        )),
        AstContent::Match { .. } => Err(VmError::InvalidExpression(
            "Match cannot appear in expression context".to_string(),
        )),
        AstContent::DecoratorDef { .. } => Err(VmError::InvalidExpression(
            "DecoratorDef cannot appear in expression context".to_string(),
        )),
        AstContent::Subscript { object, key } => {
            let obj_val = eval_expr(object, env)?;
            let key_val = eval_expr(key, env)?;
            let key_str = match &key_val {
                RuntimeValue::Str(ps) => ps.to_string(),
                RuntimeValue::Int(i) => i.to_string(),
                other => {
                    return Err(VmError::TypeError(format!(
                        "subscript key must be Str or Int, got {:?}",
                        other
                    )));
                }
            };
            match obj_val {
                RuntimeValue::Map(map) => map.get(&key_str).map(|v| *v.clone()).ok_or_else(|| {
                    VmError::UndefinedVariable(format!("key '{}' not found in map", key_str))
                }),
                other => Err(VmError::TypeError(format!(
                    "subscript requires Map, got {:?}",
                    other
                ))),
            }
        }
        AstContent::SubscriptAssign { .. } => Err(VmError::InvalidExpression(
            "SubscriptAssign must be used as a statement, not in expression context".to_string(),
        )),

        // Import is a top-level directive, not an evaluable expression.
        AstContent::Import { .. } => Err(VmError::InvalidExpression(
            "Import is not allowed in expression context".to_string(),
        )),
    }
}

/// Fully evaluate an [`AstContent::ExprList`] (or any other node) as a
/// `Vec<RuntimeValue>`, returning *all* elements rather than only the last.
///
/// Used when the IR stores an ExprList of speakers or dialogue lines and the
/// consumer needs every individual value.
pub fn eval_expr_list(
    ast: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<Vec<RuntimeValue>, VmError> {
    match ast.content() {
        AstContent::ExprList(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(eval_expr(item, env)?);
            }
            Ok(out)
        }
        _ => Ok(vec![eval_expr(ast, env)?]),
    }
}

// ── Internal expression helpers ───────────────────────────────────────────────

/// Evaluate a [`RuntimeValue`] that may be an [`RuntimeValue::IdentPath`]
/// (variable or enum-variant lookup) or a [`RuntimeValue::Str`] (which may
/// contain interpolation segments).
fn eval_runtime_value(rv: &RuntimeValue, env: &Environment) -> Result<RuntimeValue, VmError> {
    match rv {
        RuntimeValue::IdentPath(path) => match path.len() {
            0 => Err(VmError::UndefinedVariable("<empty path>".to_string())),
            1 => {
                // Single-segment: try variable lookup first, then search all
                // registered enums for a matching variant name.
                match env.get(&path[0]) {
                    Ok(v) => Ok(v),
                    Err(VmError::UndefinedVariable(_)) => {
                        // Try treating as a bare enum variant (e.g. `North`
                        // when there is only one enum with that variant).
                        for variants in env.enums().values() {
                            if variants.iter().any(|v| v == &path[0]) {
                                return Ok(RuntimeValue::Str(ParsedString::new_plain(&path[0])));
                            }
                        }
                        Err(VmError::UndefinedVariable(path[0].clone()))
                    }
                    Err(e) => Err(e),
                }
            }
            2 => {
                // Two-segment: try `EnumName.Variant` first, then try
                // module-namespaced variable "alias::var_name", then fall back
                // to plain variable lookup of the first segment.
                if let Ok(variant) = env.get_enum_variant(&path[0], &path[1]) {
                    return Ok(variant);
                }
                // Try module-namespaced variable: `alias.var` → `alias::var`
                let namespaced = format!("{}::{}", path[0], path[1]);
                if let Ok(v) = env.get(&namespaced) {
                    return Ok(v);
                }
                // Fallback: plain first-segment lookup (legacy behaviour).
                env.get(&path[0])
                    .map_err(|_| VmError::UndefinedVariable(format!("{}.{}", path[0], path[1])))
            }
            _ => {
                // Multi-segment: treat first two as enum name + variant.
                env.get_enum_variant(&path[0], &path[1])
            }
        },
        RuntimeValue::Str(ps) => {
            // Resolve any interpolation placeholders.
            Ok(RuntimeValue::Str(interpolate_string(ps, env)))
        }
        other => Ok(other.clone()),
    }
}

/// Resolve all [`StringPart::Interpolation`] segments in `ps` by looking up
/// their variable paths in `env`.
///
/// Unresolvable paths emit a [`log::warn!`] and are left as their original
/// `{path}` placeholder text.
fn interpolate_string(ps: &ParsedString, env: &Environment) -> ParsedString {
    use crate::lexer::strings::Interpolation;

    let mut new_parts: Vec<StringPart> = Vec::new();

    for part in ps.parts() {
        match part {
            StringPart::Interpolation(interp) => {
                let segments: Vec<&str> = interp.path.split('.').collect();
                let resolved = match segments.len() {
                    0 => Err(VmError::UndefinedVariable("<empty>".to_string())),
                    1 => env.get(segments[0]),
                    2 => env
                        .get_enum_variant(segments[0], segments[1])
                        .or_else(|_| env.get(segments[0])),
                    _ => env.get(segments[0]),
                };

                match resolved {
                    Ok(val) => {
                        let s = format_runtime_value(&val, interp.format.as_deref());
                        new_parts.push(StringPart::Literal(s));
                    }
                    Err(_) => {
                        log::warn!(
                            "string interpolation: undefined variable path '{}'",
                            interp.path
                        );
                        // Preserve the placeholder as-is.
                        new_parts.push(StringPart::Interpolation(Interpolation {
                            path: interp.path.clone(),
                            format: interp.format.clone(),
                        }));
                    }
                }
            }
            other => new_parts.push(other.clone()),
        }
    }

    ParsedString::new_from_parts(new_parts)
}

/// Format a [`RuntimeValue`] as a string, applying a simple format specifier
/// when provided.
///
/// Supported specifiers:
/// - `"02"` → zero-pad integer to at-least-2 digits
/// - `".2"` → float to 2 decimal places
#[allow(clippy::collapsible_if)]
fn format_runtime_value(val: &RuntimeValue, format: Option<&str>) -> String {
    match (val, format) {
        (RuntimeValue::Int(i), Some(fmt)) => {
            if let Some(width_str) = fmt.strip_prefix('0') {
                if let Ok(width) = width_str.parse::<usize>() {
                    return format!("{:0>width$}", i, width = width);
                }
            }
            i.to_string()
        }
        (RuntimeValue::Float(f), Some(fmt)) => {
            if let Some(prec_str) = fmt.strip_prefix('.') {
                if let Ok(prec) = prec_str.parse::<usize>() {
                    return format!("{:.prec$}", f, prec = prec);
                }
            }
            f.to_string()
        }
        (rv, _) => match rv {
            RuntimeValue::Null => "null".to_string(),
            RuntimeValue::Bool(b) => b.to_string(),
            RuntimeValue::Int(i) => i.to_string(),
            RuntimeValue::Float(f) => f.to_string(),
            RuntimeValue::Str(ps) => ps.to_string(),
            RuntimeValue::Dice(count, sides) => format!("{}d{}", count, sides),
            RuntimeValue::IdentPath(path) => path.join("."),
            RuntimeValue::Label { name, .. } => name.clone(),
            RuntimeValue::Map(m) => format!("map({})", m.len()),
            RuntimeValue::ScriptDecorator { .. } => "<decorator>".to_string(),
        },
    }
}

/// Evaluate a binary operation node.
fn eval_binop(
    op: &Operator,
    left: &crate::parser::ast::Ast,
    right: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<RuntimeValue, VmError> {
    // Short-circuit logical operators before evaluating both sides.
    match op {
        Operator::And => {
            let lv = eval_expr(left, env)?;
            if !is_truthy(&lv) {
                return Ok(RuntimeValue::Bool(false));
            }
            let rv = eval_expr(right, env)?;
            return Ok(RuntimeValue::Bool(is_truthy(&rv)));
        }
        Operator::Or => {
            let lv = eval_expr(left, env)?;
            if is_truthy(&lv) {
                return Ok(RuntimeValue::Bool(true));
            }
            let rv = eval_expr(right, env)?;
            return Ok(RuntimeValue::Bool(is_truthy(&rv)));
        }
        // Assignment in expression context: evaluate rhs, return it
        // (mutation is handled by IrNodeKind::Assign; this path is a no-op store).
        Operator::Assign => {
            return eval_expr(right, env);
        }
        _ => {}
    }

    let lv = eval_expr(left, env)?;
    let rv_val = eval_expr(right, env)?;

    match op {
        Operator::Plus => numeric_binop(lv, rv_val, |a, b| a + b, |a, b| a + b),
        Operator::Minus => numeric_binop(lv, rv_val, |a, b| a - b, |a, b| a - b),
        Operator::Multiply => numeric_binop(lv, rv_val, |a, b| a * b, |a, b| a * b),
        Operator::Divide => numeric_div(lv, rv_val),
        Operator::DoubleSlash => numeric_floordiv(lv, rv_val),
        Operator::Percent => numeric_int_binop(lv, rv_val, |a, b| a % b),
        Operator::Equals => Ok(RuntimeValue::Bool(values_equal(&lv, &rv_val))),
        Operator::NotEquals => Ok(RuntimeValue::Bool(!values_equal(&lv, &rv_val))),
        Operator::GreaterThan => numeric_cmp(lv, rv_val, |a, b| a > b, |a, b| a > b),
        Operator::LessThan => numeric_cmp(lv, rv_val, |a, b| a < b, |a, b| a < b),
        Operator::GreaterThanOrEquals => numeric_cmp(lv, rv_val, |a, b| a >= b, |a, b| a >= b),
        Operator::LessThanOrEquals => numeric_cmp(lv, rv_val, |a, b| a <= b, |a, b| a <= b),
        Operator::BitwiseAnd => int_bitop(lv, rv_val, |a, b| a & b),
        Operator::BitwiseOr => int_bitop(lv, rv_val, |a, b| a | b),
        Operator::BitwiseXor => int_bitop(lv, rv_val, |a, b| a ^ b),
        Operator::LeftShift => int_bitop(lv, rv_val, |a, b| a << b),
        Operator::RightShift => int_bitop(lv, rv_val, |a, b| a >> b),
        // Handled above via early return.
        Operator::And | Operator::Or | Operator::Assign => unreachable!(),
    }
}

/// Evaluate a unary operation.
fn eval_unary(op: &UnaryOperator, val: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match op {
        UnaryOperator::Not => Ok(RuntimeValue::Bool(!is_truthy(&val))),
        UnaryOperator::Negate => match val {
            RuntimeValue::Int(i) => Ok(RuntimeValue::Int(-i)),
            RuntimeValue::Float(f) => Ok(RuntimeValue::Float(-f)),
            other => Err(VmError::TypeError(format!(
                "cannot negate non-numeric value {:?}",
                other
            ))),
        },
        UnaryOperator::BitwiseNot => match val {
            RuntimeValue::Int(i) => Ok(RuntimeValue::Int(!i)),
            other => Err(VmError::TypeError(format!(
                "bitwise NOT requires Int, got {:?}",
                other
            ))),
        },
    }
}

// ── Arithmetic helpers ────────────────────────────────────────────────────────

fn to_float(v: &RuntimeValue) -> Option<f64> {
    match v {
        RuntimeValue::Float(f) => Some(*f),
        RuntimeValue::Int(i) => Some(*i as f64),
        _ => None,
    }
}

fn numeric_binop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    int_op: impl Fn(i64, i64) -> i64,
    float_op: impl Fn(f64, f64) -> f64,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Int(int_op(*a, *b))),
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
            Ok(RuntimeValue::Float(float_op(*a, *b)))
        }
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            Ok(RuntimeValue::Float(float_op(a, b)))
        }
    }
}

fn numeric_int_binop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    op: impl Fn(i64, i64) -> i64,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Int(op(*a, *b))),
        _ => Err(VmError::TypeError(format!(
            "integer-only operation requires two Int values, got {:?} and {:?}",
            lv, rv
        ))),
    }
}

fn numeric_div(lv: RuntimeValue, rv: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
            if *b == 0 {
                return Err(VmError::TypeError("integer division by zero".to_string()));
            }
            Ok(RuntimeValue::Int(a / b))
        }
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Ok(RuntimeValue::Float(a / b)),
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            Ok(RuntimeValue::Float(a / b))
        }
    }
}

fn numeric_floordiv(lv: RuntimeValue, rv: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
            if *b == 0 {
                return Err(VmError::TypeError("floor division by zero".to_string()));
            }
            Ok(RuntimeValue::Int(a.div_euclid(*b)))
        }
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            Ok(RuntimeValue::Float((a / b).floor()))
        }
    }
}

fn numeric_cmp(
    lv: RuntimeValue,
    rv: RuntimeValue,
    int_cmp: impl Fn(i64, i64) -> bool,
    float_cmp: impl Fn(f64, f64) -> bool,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Bool(int_cmp(*a, *b))),
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
            Ok(RuntimeValue::Bool(float_cmp(*a, *b)))
        }
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            Ok(RuntimeValue::Bool(float_cmp(a, b)))
        }
    }
}

fn int_bitop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    op: impl Fn(i64, i64) -> i64,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Int(op(*a, *b))),
        _ => Err(VmError::TypeError(format!(
            "bitwise operation requires two Int values, got {:?} and {:?}",
            lv, rv
        ))),
    }
}

/// Returns `true` if `v` is truthy.
///
/// - `Null` → `false`
/// - `Bool(b)` → `b`
/// - `Int(0)` → `false`; any other `Int` → `true`
/// - `Float(0.0)` → `false`; any other `Float` → `true`
/// - Everything else → `true`
fn is_truthy(v: &RuntimeValue) -> bool {
    match v {
        RuntimeValue::Null => false,
        RuntimeValue::Bool(b) => *b,
        RuntimeValue::Int(i) => *i != 0,
        RuntimeValue::Float(f) => *f != 0.0,
        _ => true,
    }
}

/// Returns `true` if two [`RuntimeValue`]s are structurally equal.
fn values_equal(a: &RuntimeValue, b: &RuntimeValue) -> bool {
    match (a, b) {
        (RuntimeValue::Null, RuntimeValue::Null) => true,
        (RuntimeValue::Bool(x), RuntimeValue::Bool(y)) => x == y,
        (RuntimeValue::Int(x), RuntimeValue::Int(y)) => x == y,
        (RuntimeValue::Float(x), RuntimeValue::Float(y)) => x == y,
        // Cross-type numeric equality.
        (RuntimeValue::Int(x), RuntimeValue::Float(y)) => (*x as f64) == *y,
        (RuntimeValue::Float(x), RuntimeValue::Int(y)) => *x == (*y as f64),
        (RuntimeValue::Str(x), RuntimeValue::Str(y)) => x.to_string() == y.to_string(),
        (RuntimeValue::Label { node_id: x, .. }, RuntimeValue::Label { node_id: y, .. }) => x == y,
        _ => false,
    }
}

// ─── Vm ──────────────────────────────────────────────────────────────────────

/// The Urd virtual machine.
///
/// The VM owns a compiled [`IrGraph`] and an [`Environment`], and steps
/// through the graph one observable event at a time when [`Vm::next`] is
/// called.  Internal nodes are processed silently without returning to the
/// caller.
#[derive(Debug)]
pub struct Vm {
    /// The compiled IR graph.
    graph: IrGraph,
    /// The node currently being processed.
    cursor: NodeId,
    /// The runtime variable environment.
    env: Environment,
    /// The call stack for labeled-block "function" calls.
    call_stack: Vec<CallFrame>,
    /// Set when the last emitted event was a [`Event::Choice`]; `None`
    /// otherwise.  Cleared when the player provides a valid choice index.
    pending_choice: Option<NodeId>,
    /// Registry of named decorator handlers.
    registry: DecoratorRegistry,
}

impl Vm {
    /// Creates a new VM from a compiled [`IrGraph`] and a [`DecoratorRegistry`].
    ///
    /// Performs a **validation pass** over every [`IrNodeKind::Dialogue`] and
    /// [`IrNodeKind::Choice`] node, checking that all decorator names are
    /// registered in `registry`.
    ///
    /// # Errors
    /// Returns [`VmError::UnknownDecorator`] if any decorator used in the
    /// script is not registered.
    pub fn new(graph: IrGraph, registry: DecoratorRegistry) -> Result<Self, VmError> {
        // Pass 1: collect all names defined by DefineScriptDecorator nodes so
        // that the validation pass below can accept them without a Rust handler.
        let script_defined: HashSet<&str> = graph
            .nodes
            .iter()
            .filter_map(|n| match &n.kind {
                IrNodeKind::DefineScriptDecorator { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect();

        // Pass 2: validate every decorator used in Dialogue / Choice nodes.
        for node in &graph.nodes {
            match &node.kind {
                IrNodeKind::Dialogue { decorators, .. } => {
                    for dec in decorators {
                        if !registry.handlers.contains_key(dec.name())
                            && !script_defined.contains(dec.name())
                        {
                            return Err(VmError::UnknownDecorator {
                                name: dec.name().to_string(),
                                node_id: node.id,
                            });
                        }
                    }
                }
                IrNodeKind::Choice {
                    decorators,
                    options,
                } => {
                    for dec in decorators {
                        if !registry.handlers.contains_key(dec.name())
                            && !script_defined.contains(dec.name())
                        {
                            return Err(VmError::UnknownDecorator {
                                name: dec.name().to_string(),
                                node_id: node.id,
                            });
                        }
                    }
                    for opt in options {
                        for dec in &opt.decorators {
                            if !registry.handlers.contains_key(dec.name())
                                && !script_defined.contains(dec.name())
                            {
                                return Err(VmError::UnknownDecorator {
                                    name: dec.name().to_string(),
                                    node_id: node.id,
                                });
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Pre-seed every compiled label name as a RuntimeValue::Label builtin
        // so scripts can reference labels as values (e.g. pass them to decorator
        // arguments) without polluting the globals map that a save-file system
        // would serialise.
        let mut env = Environment::new();
        for (name, node_id) in &graph.labels {
            env.define_builtin(
                name.clone(),
                RuntimeValue::Label {
                    name: name.clone(),
                    node_id: *node_id,
                },
            );
        }

        let cursor = graph.entry;
        Ok(Vm {
            graph,
            cursor,
            env,
            call_stack: Vec::new(),
            pending_choice: None,
            registry,
        })
    }

    /// Advance the VM by one observable step and return the next [`Event`].
    ///
    /// Pass `choice: Some(idx)` when responding to a [`Event::Choice`] event;
    /// pass `None` for all other steps.
    ///
    /// Returns `None` when the script has ended.
    pub fn next(&mut self, choice: Option<usize>) -> Option<Result<Event, VmError>> {
        loop {
            // NODE_END sentinel → treat as End.
            if self.cursor == NODE_END {
                return None;
            }

            let node_id = self.cursor;

            // We need to read from self.graph while also mutating self.cursor,
            // self.env, etc.  Rather than cloning the entire IrNodeKind, we use
            // a raw pointer to the node kind, which is safe because graph.nodes
            // is never resized during execution.
            let node_kind: &IrNodeKind = {
                let node = self.graph.nodes.get(node_id.0 as usize)?;
                // SAFETY: `self.graph.nodes` is a Vec that is never pushed to or
                // truncated during VM execution, so the reference stays valid
                // for the duration of the match arm below.
                unsafe { &*(&node.kind as *const IrNodeKind) }
            };

            match node_kind {
                // ── Terminal ─────────────────────────────────────────────────
                IrNodeKind::End => return None,

                // ── Nop / merge point ────────────────────────────────────────
                IrNodeKind::Nop { next } => {
                    self.cursor = *next;
                }

                // ── Variable assignment ──────────────────────────────────────
                IrNodeKind::Assign {
                    var,
                    scope,
                    expr,
                    next,
                } => {
                    // Clone minimal data so we can release the graph borrow.
                    let var = var.clone();
                    let scope = scope.clone();
                    let next_id = *next;
                    let value = match eval_expr(expr, &self.env) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };
                    if let Err(e) = self.env.set(&var, value, &scope) {
                        return Some(Err(e));
                    }
                    self.cursor = next_id;
                }

                // ── Side-effecting expression ────────────────────────────────
                IrNodeKind::Eval { expr, next } => {
                    let next_id = *next;
                    // Clone the expr to release the graph borrow before we
                    // take &mut self.env in eval_subscript_assign.
                    let expr_cloned = expr.clone();
                    match expr_cloned.content() {
                        AstContent::SubscriptAssign { object, key, value } => {
                            let (obj, k, v) = (*object.clone(), *key.clone(), *value.clone());
                            if let Err(e) = eval_subscript_assign(&obj, &k, &v, &mut self.env) {
                                return Some(Err(e));
                            }
                        }
                        _ => {
                            if let Err(e) = eval_expr(&expr_cloned, &self.env) {
                                return Some(Err(e));
                            }
                        }
                    }
                    self.cursor = next_id;
                }

                // ── Conditional branch ───────────────────────────────────────
                IrNodeKind::Branch {
                    condition,
                    then_node,
                    else_node,
                } => {
                    let (then_id, else_id) = (*then_node, *else_node);
                    let cond_val = match eval_expr(condition, &self.env) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };
                    self.cursor = if is_truthy(&cond_val) {
                        then_id
                    } else {
                        else_id
                    };
                }

                // ── Multi-way pattern match ──────────────────────────────────
                IrNodeKind::Switch {
                    scrutinee,
                    arms,
                    default,
                } => {
                    let scrutinee_val = match eval_expr(scrutinee, &self.env) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };

                    // Clone arm data so the graph borrow is no longer needed
                    // while we update self.cursor.
                    let arms_data: Vec<(MatchPattern, NodeId)> =
                        arms.iter().map(|a| (a.pattern.clone(), a.target)).collect();
                    let default_target = *default;

                    let mut matched = false;
                    for (pattern, target) in &arms_data {
                        let is_match = match pattern {
                            MatchPattern::Wildcard => true,
                            MatchPattern::Value(pat_ast) => match eval_expr(pat_ast, &self.env) {
                                Ok(pat_val) => values_equal(&scrutinee_val, &pat_val),
                                Err(_) => false,
                            },
                        };
                        if is_match {
                            self.cursor = *target;
                            matched = true;
                            break;
                        }
                    }

                    if !matched {
                        self.cursor = default_target.unwrap_or(NODE_END);
                    }
                }

                // ── Unconditional jump ───────────────────────────────────────
                IrNodeKind::Jump { target } => {
                    // Jump does NOT push a call frame — it is a tail transfer.
                    self.cursor = *target;
                }

                // ── Return ───────────────────────────────────────────────────
                IrNodeKind::Return { value } => {
                    // Evaluate return value first (before popping the frame).
                    let return_val = if let Some(val_ast) = value {
                        match eval_expr(val_ast, &self.env) {
                            Ok(v) => Some(v),
                            Err(e) => return Some(Err(e)),
                        }
                    } else {
                        None
                    };

                    match self.call_stack.pop() {
                        Some(frame) => {
                            // Unwind any extra scopes pushed since the call.
                            while self.env.depth() > frame.scope_depth {
                                self.env.pop_scope();
                            }
                            // Store return value if the call frame has a binding.
                            if let (Some(var_name), Some(val)) =
                                (&frame.assign_to_var, return_val)
                            {
                                if let Err(e) =
                                    self.env.set(var_name, val, &DeclKind::Variable)
                                {
                                    return Some(Err(e));
                                }
                            }
                            self.cursor = frame.return_cursor;
                        }
                        None => return None, // No frame → script ends.
                    }
                }

                // ── Subroutine call (with optional result binding) ────────────
                //
                // We bypass the target's EnterScope node to avoid a double frame
                // on the call stack (EnterScope always pushes its own frame).
                // Instead, LetCall:
                //   1. Reads body_entry directly from the EnterScope node.
                //   2. Pushes its own scope + frame (with return_cursor = next).
                //   3. Jumps to body_entry.
                //
                // ExitScope (on fall-through) and Return (on explicit return) both
                // pop the LetCall frame and land at `next` (the call-site continuation).
                IrNodeKind::LetCall { var, target, next } => {
                    let var_name = var.clone();
                    let target_id = *target;
                    let next_id = *next;

                    // Resolve body_entry: if the target is an EnterScope node,
                    // jump past it into the body directly.
                    let body_entry =
                        match self.graph.nodes.get(target_id.0 as usize).map(|n| &n.kind) {
                            Some(IrNodeKind::EnterScope { next: body, .. }) => *body,
                            _ => target_id, // fallback: jump to target as-is
                        };

                    let assign = if var_name.is_empty() {
                        None
                    } else {
                        Some(var_name)
                    };

                    // Push a scope to match the eventual ExitScope's pop.
                    self.env.push_scope();
                    self.call_stack.push(CallFrame {
                        return_cursor: next_id,
                        scope_depth: self.env.depth() - 1,
                        assign_to_var: assign,
                    });
                    self.cursor = body_entry;
                }

                // ── Enter labeled-block scope ────────────────────────────────
                IrNodeKind::EnterScope { label, next } => {
                    let label_name = label.clone();
                    let body_start = *next;

                    // Find the continuation after the matching ExitScope so
                    // that Return inside this block jumps past the whole block.
                    let return_cursor = self.find_exit_scope_next(&label_name);

                    self.env.push_scope();
                    self.call_stack.push(CallFrame {
                        return_cursor,
                        scope_depth: self.env.depth() - 1,
                        assign_to_var: None,
                    });
                    self.cursor = body_start;
                }

                // ── Exit labeled-block scope ─────────────────────────────────
                IrNodeKind::ExitScope { .. } => {
                    self.env.pop_scope();
                    // Pop the matching call frame (pushed by either EnterScope
                    // on normal fall-through, or LetCall on a subroutine call).
                    // Use the frame's `return_cursor` rather than ExitScope's
                    // `next` field so that LetCall fall-throughs land at the
                    // call-site continuation (LetCall.next), not back at the
                    // LetCall node itself.
                    //
                    // For plain EnterScope fall-through, frame.return_cursor ==
                    // ExitScope.next (both set by find_exit_scope_next), so
                    // behaviour is unchanged.
                    let next_id = match self.call_stack.pop() {
                        Some(frame) => frame.return_cursor,
                        None => NODE_END,
                    };
                    self.cursor = next_id;
                }

                // ── Enum declaration ─────────────────────────────────────────
                IrNodeKind::DefineEnum {
                    name,
                    variants,
                    next,
                } => {
                    let name = name.clone();
                    let variants = variants.clone();
                    let next_id = *next;
                    self.env.define_enum(name, variants);
                    self.cursor = next_id;
                }

                // ── Script-defined decorator registration ────────────────────
                //
                // At execution time we store a `RuntimeValue::ScriptDecorator`
                // in the environment under `name`.  When a `@name(args)` usage
                // is encountered during Dialogue/Choice evaluation, `apply_decorator`
                // finds it there and calls `apply_script_decorator` to run the body.
                IrNodeKind::DefineScriptDecorator {
                    name,
                    event_constraint,
                    params,
                    body,
                    next,
                } => {
                    let name = name.clone();
                    let next_id = *next;
                    let decorator_val = RuntimeValue::ScriptDecorator {
                        event_constraint: event_constraint.clone(),
                        params: params.clone(),
                        body: Box::new(body.clone()),
                    };
                    if let Err(e) = self.env.set(&name, decorator_val, &DeclKind::Variable) {
                        return Some(Err(e));
                    }
                    self.cursor = next_id;
                }

                // ── Dialogue event ───────────────────────────────────────────
                IrNodeKind::Dialogue {
                    speakers,
                    lines,
                    decorators,
                    next,
                } => {
                    let next_id = *next;

                    let speakers_vec = match eval_expr_list(speakers, &self.env) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };
                    let lines_vec = match eval_expr_list(lines, &self.env) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };

                    // Evaluate decorators and merge their fields.
                    // `apply_decorator` checks the env first (for script-defined
                    // decorators), then falls back to the Rust registry.
                    let decorators_cloned: Vec<Decorator> = decorators.to_vec();
                    let mut fields: HashMap<String, RuntimeValue> = HashMap::new();
                    for dec in &decorators_cloned {
                        match apply_decorator(dec, &self.env, &self.registry, fields) {
                            Ok(new_fields) => fields = new_fields,
                            Err(e) => return Some(Err(e)),
                        }
                    }

                    self.cursor = next_id;
                    return Some(Ok(Event::Dialogue {
                        speakers: speakers_vec,
                        lines: lines_vec,
                        fields,
                    }));
                }

                // ── Choice event ─────────────────────────────────────────────
                IrNodeKind::Choice {
                    options,
                    decorators,
                } => {
                    match choice {
                        // ── Player provided a choice index ───────────────────
                        Some(idx) => {
                            if idx >= options.len() {
                                log::warn!(
                                    "Choice index {} out of bounds (len={}); \
                                         re-emitting Choice event",
                                    idx,
                                    options.len()
                                );
                                // Re-emit without advancing.
                                return Some(build_choice_event(
                                    options,
                                    decorators,
                                    &self.env,
                                    &self.registry,
                                ));
                            }
                            let entry = options[idx].entry;
                            self.pending_choice = None;
                            self.cursor = entry;
                            // Continue loop — the choice selection itself is
                            // not an observable event.
                        }

                        // ── No choice provided ───────────────────────────────
                        None => {
                            if self.pending_choice == Some(node_id) {
                                // Already waiting for a choice at this node — re-emit.
                                log::warn!(
                                    "Choice at node {:?} is already pending; \
                                         re-emitting Event::Choice without advancing",
                                    node_id
                                );
                            } else {
                                self.pending_choice = Some(node_id);
                            }
                            return Some(build_choice_event(
                                options,
                                decorators,
                                &self.env,
                                &self.registry,
                            ));
                        }
                    }
                }
            }
        }
    }

    /// Returns a reference to the VM's current [`Environment`].
    pub fn env(&self) -> &Environment {
        &self.env
    }

    /// Returns a reference to the compiled [`IrGraph`].
    pub fn graph(&self) -> &IrGraph {
        &self.graph
    }

    // ── Private helpers ───────────────────────────────────────────────────────

    /// Scan `graph.nodes` for the [`IrNodeKind::ExitScope`] node whose `label`
    /// matches `label_name` and return its `next` field.
    ///
    /// This is used by [`IrNodeKind::EnterScope`] handling to determine the
    /// correct `return_cursor` for the call frame so that [`IrNodeKind::Return`]
    /// can jump past the entire labeled block.
    ///
    /// Falls back to [`NODE_END`] if no matching ExitScope is found.
    #[allow(clippy::collapsible_if)]
    fn find_exit_scope_next(&self, label_name: &str) -> NodeId {
        for node in &self.graph.nodes {
            if let IrNodeKind::ExitScope { label, next } = &node.kind {
                if label == label_name {
                    return *next;
                }
            }
        }
        NODE_END
    }
}

// ─── Free helpers ─────────────────────────────────────────────────────────────

/// Execute a subscript assignment: `ident[key] = value`.
///
/// The `object` must resolve to a simple variable name (single-segment
/// `IdentPath`) in the environment holding a [`RuntimeValue::Map`].
/// Mutates the map in-place by reading the current value, inserting the new
/// entry, then writing back.
fn eval_subscript_assign(
    object: &crate::parser::ast::Ast,
    key: &crate::parser::ast::Ast,
    value: &crate::parser::ast::Ast,
    env: &mut Environment,
) -> Result<(), VmError> {
    // 1. Extract variable name from object.
    let var_name = match object.content() {
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => path[0].clone(),
        _ => {
            return Err(VmError::TypeError(
                "subscript assignment target must be a simple variable name".to_string(),
            ));
        }
    };

    // 2. Evaluate key and value (immutable borrow — env not mutated yet).
    let key_val = eval_expr(key, env)?;
    let new_val = eval_expr(value, env)?;

    let key_str = match &key_val {
        RuntimeValue::Str(ps) => ps.to_string(),
        RuntimeValue::Int(i) => i.to_string(),
        other => {
            return Err(VmError::TypeError(format!(
                "subscript key must be Str or Int, got {:?}",
                other
            )));
        }
    };

    // 3. Get the map, mutate it, write back.
    let mut map_val = env.get(&var_name)?;
    match &mut map_val {
        RuntimeValue::Map(map) => {
            map.insert(key_str, Box::new(new_val));
        }
        other => {
            return Err(VmError::TypeError(format!(
                "subscript assignment requires Map variable '{}', got {:?}",
                var_name, other
            )));
        }
    }
    env.set(&var_name, map_val, &DeclKind::Variable)
}

/// Execute an AST block synchronously in a mutable environment.
///
/// Used to run decorator bodies without creating a full VM execution loop.
/// Forbidden constructs (Dialogue, Menu, Jump, Return, etc.) return an error.
fn exec_block_sync(ast: &crate::parser::ast::Ast, env: &mut Environment) -> Result<(), VmError> {
    use crate::parser::ast::AstContent as AC;
    match ast.content() {
        AC::Block(stmts) => {
            for stmt in stmts {
                exec_block_sync(stmt, env)?;
            }
            Ok(())
        }
        AC::Declaration {
            kind,
            decl_name,
            decl_defs,
            ..
        } => {
            let var_name = match decl_name.content() {
                AC::Value(RuntimeValue::IdentPath(p)) if p.len() == 1 => p[0].clone(),
                _ => {
                    return Err(VmError::TypeError(
                        "expected identifier in declaration".to_string(),
                    ));
                }
            };
            let val = eval_expr(decl_defs, env)?;
            env.set(&var_name, val, kind)?;
            Ok(())
        }
        AC::BinOp {
            op: Operator::Assign,
            left,
            right,
        } => {
            let var_name = match left.content() {
                AC::Value(RuntimeValue::IdentPath(p)) if p.len() == 1 => p[0].clone(),
                _ => {
                    return Err(VmError::TypeError(
                        "expected identifier in assignment".to_string(),
                    ));
                }
            };
            let val = eval_expr(right, env)?;
            env.set(&var_name, val, &DeclKind::Variable)?;
            Ok(())
        }
        AC::SubscriptAssign { object, key, value } => {
            eval_subscript_assign(object, key, value, env)
        }
        AC::If {
            condition,
            then_block,
            else_block,
        } => {
            let cond = eval_expr(condition, env)?;
            if is_truthy(&cond) {
                exec_block_sync(then_block, env)?;
            } else if let Some(eb) = else_block {
                exec_block_sync(eb, env)?;
            }
            Ok(())
        }
        AC::Call { .. } => {
            eval_expr(ast, env)?;
            Ok(())
        }
        // Pure expressions: evaluate and discard the result.
        AC::BinOp { .. }
        | AC::UnaryOp { .. }
        | AC::Value(_)
        | AC::Subscript { .. }
        | AC::List(_)
        | AC::Map(_)
        | AC::ExprList(_) => {
            eval_expr(ast, env)?;
            Ok(())
        }
        // Forbidden inside decorator bodies.
        AC::Dialogue { .. }
        | AC::Menu { .. }
        | AC::MenuOption { .. }
        | AC::LabeledBlock { .. }
        | AC::Jump { .. }
        | AC::LetCall { .. }
        | AC::Return { .. }
        | AC::DecoratorDef { .. }
        | AC::Match { .. }
        | AC::EnumDecl { .. }
        | AC::Import { .. } => Err(VmError::InvalidExpression(format!(
            "{:?} is not allowed inside a decorator body",
            std::mem::discriminant(ast.content())
        ))),
    }
}

/// Invoke a script-defined decorator body.
///
/// Builds a temporary local environment with `event` (the current event's
/// fields as a [`RuntimeValue::Map`]) and the decorator's parameter bindings,
/// runs the body synchronously, then extracts and returns the (possibly
/// mutated) event fields map.
///
/// # Errors
/// Returns [`VmError`] if the body execution fails or if the decorator body
/// replaces `event` with a non-`Map` value.
fn apply_script_decorator(
    params: &[String],
    body: &crate::parser::ast::Ast,
    args: &[RuntimeValue],
    outer_env: &Environment,
    event_fields: HashMap<String, RuntimeValue>,
) -> Result<HashMap<String, RuntimeValue>, VmError> {
    let mut local_env = outer_env.clone();
    local_env.push_scope();

    // Bind `event` as a Map so the body can read/write event["key"].
    let event_map: HashMap<String, Box<RuntimeValue>> = event_fields
        .into_iter()
        .map(|(k, v)| (k, Box::new(v)))
        .collect();
    local_env.set("event", RuntimeValue::Map(event_map), &DeclKind::Variable)?;

    // Bind each declared parameter to the corresponding argument value.
    for (name, val) in params.iter().zip(args.iter()) {
        local_env.set(name, val.clone(), &DeclKind::Variable)?;
    }

    // Execute the body synchronously.
    exec_block_sync(body, &mut local_env)?;

    // Extract the (possibly mutated) event map.
    match local_env.get("event")? {
        RuntimeValue::Map(map) => Ok(map.into_iter().map(|(k, v)| (k, *v)).collect()),
        other => Err(VmError::TypeError(format!(
            "decorator body replaced `event` with a non-Map value: {:?}",
            other
        ))),
    }
}

/// Apply a single decorator: checks the environment first for a
/// script-defined [`RuntimeValue::ScriptDecorator`], then falls back to the
/// Rust [`DecoratorRegistry`].
///
/// `existing_fields` is the accumulated fields map so far; the decorator may
/// add new fields or overwrite existing ones.  The returned map is the merged
/// result.
fn apply_decorator(
    dec: &Decorator,
    env: &Environment,
    registry: &DecoratorRegistry,
    existing_fields: HashMap<String, RuntimeValue>,
) -> Result<HashMap<String, RuntimeValue>, VmError> {
    match env.get(dec.name()) {
        Ok(RuntimeValue::ScriptDecorator { params, body, .. }) => {
            // Evaluate arguments.
            let args = match dec.args().content() {
                AstContent::ExprList(items) => items
                    .iter()
                    .map(|a| eval_expr(a, env))
                    .collect::<Result<Vec<_>, _>>()?,
                _ => vec![eval_expr(dec.args(), env)?],
            };
            apply_script_decorator(&params, &body, &args, env, existing_fields)
        }
        Ok(_) => Err(VmError::TypeError(format!(
            "'{}' is defined in scope but is not a decorator",
            dec.name()
        ))),
        Err(VmError::UndefinedVariable(_)) => {
            // Fall back to the Rust registry.
            let dec_fields = registry.apply(dec, env)?;
            let mut merged = existing_fields;
            merged.extend(dec_fields);
            Ok(merged)
        }
        Err(e) => Err(e),
    }
}

/// Build a [`Event::Choice`] from an options list, evaluating all decorators.
fn build_choice_event(
    options: &[crate::ir::IrChoiceOption],
    decorators: &[Decorator],
    env: &Environment,
    registry: &DecoratorRegistry,
) -> Result<Event, VmError> {
    // Merge top-level (Menu) decorator fields via apply_decorator so that
    // script-defined decorators are handled correctly.
    let mut fields: HashMap<String, RuntimeValue> = HashMap::new();
    for dec in decorators {
        fields = apply_decorator(dec, env, registry, fields)?;
    }

    // Build per-option ChoiceEvent entries.
    let mut choice_options = Vec::with_capacity(options.len());
    for opt in options {
        let mut opt_fields: HashMap<String, RuntimeValue> = HashMap::new();
        for dec in &opt.decorators {
            opt_fields = apply_decorator(dec, env, registry, opt_fields)?;
        }
        choice_options.push(ChoiceEvent {
            label: opt.label.clone(),
            fields: opt_fields,
        });
    }

    Ok(Event::Choice {
        options: choice_options,
        fields,
    })
}

// ─── Unit tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::{
        compiler::Compiler,
        lexer::strings::ParsedString,
        parser::ast::{Ast, AstContent, DeclKind, Decorator, MatchArm, MatchPattern},
        runtime::value::RuntimeValue,
    };

    // ── Shared helpers ────────────────────────────────────────────────────────

    fn ident(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_string()]))
    }

    fn int(n: i64) -> Ast {
        Ast::value(RuntimeValue::Int(n))
    }

    fn str_lit(s: &str) -> Ast {
        Ast::value(RuntimeValue::Str(ParsedString::new_plain(s)))
    }

    fn decl(name: &str, val: Ast) -> Ast {
        Ast::decl(DeclKind::Variable, ident(name), val)
    }

    fn empty_registry() -> DecoratorRegistry {
        DecoratorRegistry::new()
    }

    fn build_vm(ast: Ast) -> Vm {
        let graph = Compiler::compile(&ast).expect("compile failed");
        Vm::new(graph, empty_registry()).expect("vm construction failed")
    }

    // ── Tests ─────────────────────────────────────────────────────────────────

    /// A script `let x = 1` followed by a Dialogue emits exactly one
    /// `Event::Dialogue` and then ends.
    #[test]
    fn test_let_then_dialogue_emits_event() {
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hello!")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![decl("x", int(1)), dialogue]);

        let mut vm = build_vm(ast);

        let ev = vm.next(None).expect("expected an event").expect("no error");
        match ev {
            Event::Dialogue {
                speakers, lines, ..
            } => {
                assert_eq!(speakers.len(), 1);
                assert_eq!(lines.len(), 1);
                // Line should be the literal "Hello!".
                match &lines[0] {
                    RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "Hello!"),
                    other => panic!("expected Str, got {:?}", other),
                }
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }

        assert!(vm.next(None).is_none(), "script should end after dialogue");
    }

    /// A `Branch` on `true` follows `then_node`; the dialogue reads the
    /// variable set in the then-block.
    #[test]
    fn test_branch_true_follows_then() {
        let condition = Ast::value(RuntimeValue::Bool(true));
        let then_block = Ast::block(vec![decl("x", int(1))]);
        let else_block = Ast::block(vec![decl("x", int(2))]);
        let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

        let speakers = Ast::expr_list(vec![str_lit("Bob")]);
        let lines = Ast::expr_list(vec![ident("x")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![if_ast, dialogue]);

        let mut vm = build_vm(ast);
        let ev = vm.next(None).expect("event expected").expect("no error");

        match ev {
            Event::Dialogue { lines, .. } => {
                assert_eq!(
                    lines,
                    vec![RuntimeValue::Int(1)],
                    "then-branch should set x=1"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// A `Branch` on `false` follows `else_node`.
    #[test]
    fn test_branch_false_follows_else() {
        let condition = Ast::value(RuntimeValue::Bool(false));
        let then_block = Ast::block(vec![decl("result", int(1))]);
        let else_block = Ast::block(vec![decl("result", int(2))]);
        let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

        let speakers = Ast::expr_list(vec![str_lit("Narrator")]);
        let lines = Ast::expr_list(vec![ident("result")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![if_ast, dialogue]);

        let mut vm = build_vm(ast);
        let ev = vm.next(None).expect("event expected").expect("no error");
        match ev {
            Event::Dialogue { lines, .. } => {
                assert_eq!(
                    lines,
                    vec![RuntimeValue::Int(2)],
                    "else-branch should set result=2"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `next(None)` on a Menu emits `Event::Choice`; calling `next(None)` again
    /// re-emits it (with a log warning); `next(Some(0))` clears the pending
    /// choice and advances into the option body.
    #[test]
    fn test_choice_flow() {
        let opt_a = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![decl("picked", int(1))]),
        );
        let opt_b = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![decl("picked", int(2))]),
        );
        let ast = Ast::menu(vec![opt_a, opt_b]);

        let mut vm = build_vm(ast);

        // First call — emits Choice and sets pending_choice.
        let ev1 = vm.next(None).expect("first event").expect("no error");
        assert!(
            matches!(ev1, Event::Choice { .. }),
            "first next(None) should emit Choice, got {:?}",
            ev1
        );
        let choice_node = vm.cursor;
        assert_eq!(
            vm.pending_choice,
            Some(choice_node),
            "pending_choice must be set after first next(None)"
        );

        // Second call with None — re-emits Choice.
        let ev2 = vm.next(None).expect("second event").expect("no error");
        assert!(
            matches!(ev2, Event::Choice { ref options, .. } if options.len() == 2),
            "second next(None) should re-emit same Choice"
        );

        // Provide a valid choice (option 0).
        // Option 0's body has `let picked = 1` — internal, so the loop ends.
        let ev3 = vm.next(Some(0));
        assert!(
            vm.pending_choice.is_none(),
            "pending_choice must be cleared after valid choice"
        );
        // Option body has no dialogue → script ends → None.
        assert!(
            ev3.is_none(),
            "expected script end after choosing option 0, got {:?}",
            ev3
        );
    }

    /// An out-of-bounds choice index re-emits the Choice event with a warning
    /// and does NOT advance the cursor.
    #[test]
    fn test_choice_out_of_bounds_reemits() {
        let opt = Ast::menu_option("Only".to_string(), Ast::block(vec![]));
        let ast = Ast::menu(vec![opt]);
        let mut vm = build_vm(ast);

        // Consume the initial None-choice emission.
        let _ = vm.next(None).expect("event").expect("no error");

        // Provide an out-of-bounds index.
        let ev = vm
            .next(Some(99))
            .expect("should re-emit")
            .expect("no error");
        assert!(
            matches!(ev, Event::Choice { .. }),
            "out-of-bounds choice should re-emit Choice, got {:?}",
            ev
        );
    }

    /// A `Jump` to a known label advances the cursor and the dialogue inside
    /// the label is reached.
    #[test]
    fn test_jump_advances_to_label() {
        // block { jump scene1; label scene1 { Alice: "Jumped here!" } }
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Jumped here!")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let labeled = Ast::labeled_block("scene1".to_string(), Ast::block(vec![dialogue]));
        let jump = Ast::jump_stmt("scene1".to_string(), false);
        // Compiler block is compiled right-to-left; jump comes first in source.
        let ast = Ast::block(vec![jump, labeled]);

        let mut vm = build_vm(ast);

        let ev = vm.next(None).expect("expected event").expect("no error");
        match ev {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(ps.to_string(), "Jumped here!");
                }
                other => panic!("expected Str line, got {:?}", other),
            },
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `Vm::new` returns `VmError::UnknownDecorator` when the compiled script
    /// uses a decorator name that is not registered.
    #[test]
    fn test_unknown_decorator_fails_validation() {
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hi")]);
        let deco = Decorator::bare("mystery_deco".to_string());
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let graph = Compiler::compile(&dialogue).expect("compile failed");
        let result = Vm::new(graph, empty_registry());

        assert!(
            matches!(
                result,
                Err(VmError::UnknownDecorator { ref name, .. }) if name == "mystery_deco"
            ),
            "expected UnknownDecorator, got {:?}",
            result
        );
    }

    /// A `Vm::new` call succeeds when all decorators used in the script are
    /// registered.
    #[test]
    fn test_known_decorator_passes_validation() {
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hi")]);
        let deco = Decorator::bare("mood".to_string());
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let graph = Compiler::compile(&dialogue).expect("compile failed");
        let mut registry = DecoratorRegistry::new();
        registry.register("mood", |_args| HashMap::new());

        assert!(
            Vm::new(graph, registry).is_ok(),
            "registered decorator should pass validation"
        );
    }

    /// `Return` inside a labeled block unwinds back to the continuation after
    /// the block (the dialogue that follows it in the script).
    #[test]
    fn test_return_exits_to_continuation() {
        // block {
        //   label myblock { return }
        //   Alice: "After block"
        // }
        // Return exits myblock → dialogue is emitted.
        let labeled = Ast::labeled_block(
            "myblock".to_string(),
            Ast::block(vec![Ast::return_stmt(None)]),
        );
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("After block")]);
        let after_dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![labeled, after_dialogue]);

        let mut vm = build_vm(ast);

        // Return should jump to the continuation (the dialogue).
        let ev = vm.next(None).expect("expected event").expect("no error");
        match ev {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "After block"),
                other => panic!("expected Str, got {:?}", other),
            },
            other => panic!("expected Dialogue after return, got {:?}", other),
        }
        assert!(vm.next(None).is_none(), "script should end after dialogue");
    }

    /// A `Return` with no call frame on the stack ends the script.
    #[test]
    fn test_return_with_empty_call_stack_ends_script() {
        let ast = Ast::return_stmt(None);
        let mut vm = build_vm(ast);
        assert!(
            vm.next(None).is_none(),
            "Return with empty call stack should end script"
        );
    }

    // ── Expression evaluator unit tests ───────────────────────────────────────

    /// Basic arithmetic: Int+Int, Float+Int coercion, negation.
    #[test]
    fn test_eval_arithmetic() {
        let env = Environment::new();

        let add = Ast::add_op(int(3), int(4));
        assert_eq!(eval_expr(&add, &env).expect("3+4"), RuntimeValue::Int(7));

        let mixed = Ast::add_op(Ast::value(RuntimeValue::Float(2.5)), int(1));
        assert_eq!(
            eval_expr(&mixed, &env).expect("2.5+1"),
            RuntimeValue::Float(3.5)
        );

        let neg = Ast::negate_op(int(5));
        assert_eq!(eval_expr(&neg, &env).expect("-5"), RuntimeValue::Int(-5));

        let not_true = Ast::not_op(Ast::value(RuntimeValue::Bool(true)));
        assert_eq!(
            eval_expr(&not_true, &env).expect("not true"),
            RuntimeValue::Bool(false)
        );
    }

    /// Comparison operators produce Bool results.
    #[test]
    fn test_eval_comparison() {
        let env = Environment::new();

        let gt = Ast::greater_than_op(int(5), int(3));
        assert_eq!(eval_expr(&gt, &env).expect("5>3"), RuntimeValue::Bool(true));

        let eq = Ast::equals_op(int(4), int(4));
        assert_eq!(
            eval_expr(&eq, &env).expect("4==4"),
            RuntimeValue::Bool(true)
        );

        let neq = Ast::not_equals_op(int(4), int(5));
        assert_eq!(
            eval_expr(&neq, &env).expect("4!=5"),
            RuntimeValue::Bool(true)
        );
    }

    /// Short-circuit `And` and `Or`.
    #[test]
    fn test_eval_logical_short_circuit() {
        let env = Environment::new();

        // false and <anything> → false (rhs must NOT be evaluated to cause an error)
        let and_short = Ast::and_op(
            Ast::value(RuntimeValue::Bool(false)),
            // This would error if evaluated (undefined variable):
            ident("undefined_var"),
        );
        assert_eq!(
            eval_expr(&and_short, &env).expect("false and x"),
            RuntimeValue::Bool(false)
        );

        // true or <anything> → true
        let or_short = Ast::or_op(
            Ast::value(RuntimeValue::Bool(true)),
            ident("also_undefined"),
        );
        assert_eq!(
            eval_expr(&or_short, &env).expect("true or x"),
            RuntimeValue::Bool(true)
        );
    }

    /// Variable lookup succeeds when defined; returns `UndefinedVariable`
    /// otherwise.
    #[test]
    fn test_variable_lookup() {
        let mut env = Environment::new();
        env.set("hp", RuntimeValue::Int(100), &DeclKind::Variable)
            .expect("set failed");

        assert_eq!(
            eval_expr(&ident("hp"), &env).expect("hp"),
            RuntimeValue::Int(100)
        );
        assert!(
            matches!(
                eval_expr(&ident("missing"), &env),
                Err(VmError::UndefinedVariable(_))
            ),
            "undefined variable should error"
        );
    }

    /// Decorator registry: registered decorator fields are applied; unknown
    /// decorator returns an error.
    #[test]
    fn test_decorator_registry_apply() {
        let mut registry = DecoratorRegistry::new();
        registry.register("mood", |args| {
            let mut m = HashMap::new();
            if let Some(RuntimeValue::Str(s)) = args.first() {
                m.insert("mood".to_string(), RuntimeValue::Str(s.clone()));
            }
            m
        });

        let env = Environment::new();
        let deco = Decorator::new("mood".to_string(), Ast::expr_list(vec![str_lit("happy")]));

        let fields = registry.apply(&deco, &env).expect("apply should succeed");
        assert!(fields.contains_key("mood"), "fields should contain 'mood'");

        let unknown_deco = Decorator::bare("ghost".to_string());
        assert!(
            registry.apply(&unknown_deco, &env).is_err(),
            "unknown decorator should error"
        );
    }

    /// Environment: `const` cannot be reassigned.
    #[test]
    fn test_const_immutability() {
        let mut env = Environment::new();
        env.set("MAX", RuntimeValue::Int(100), &DeclKind::Constant)
            .expect("first const set");
        let result = env.set("MAX", RuntimeValue::Int(200), &DeclKind::Constant);
        assert!(
            matches!(result, Err(VmError::TypeError(_))),
            "reassigning a const should be a TypeError"
        );
    }

    /// Environment: globals are accessible from nested scopes.
    #[test]
    fn test_globals_visible_in_nested_scope() {
        let mut env = Environment::new();
        env.set("score", RuntimeValue::Int(0), &DeclKind::Global)
            .expect("set global");
        env.push_scope();
        assert_eq!(
            env.get("score").expect("global visible in inner scope"),
            RuntimeValue::Int(0)
        );
        env.pop_scope();
    }

    /// A complete script with `DefineEnum` + `Switch` reaches the right arm.
    #[test]
    fn test_switch_on_enum_variant() {
        // enum Direction { North, South }
        // let dir = Direction.North
        // match dir { North { Alice: "going north" } _ { Alice: "other" } }
        let enum_decl = Ast::enum_decl(
            "Direction".to_string(),
            vec!["North".to_string(), "South".to_string()],
        );

        // dir = Direction.North (2-segment ident path)
        let north_path = Ast::value(RuntimeValue::IdentPath(vec![
            "Direction".to_string(),
            "North".to_string(),
        ]));
        let dir_decl = Ast::decl(DeclKind::Variable, ident("dir"), north_path);

        let north_arm = MatchArm::new(
            MatchPattern::Value(Ast::value(RuntimeValue::Str(ParsedString::new_plain(
                "North",
            )))),
            Ast::block(vec![Ast::dialogue(
                Ast::expr_list(vec![str_lit("Alice")]),
                Ast::expr_list(vec![str_lit("going north")]),
            )]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![Ast::dialogue(
                Ast::expr_list(vec![str_lit("Alice")]),
                Ast::expr_list(vec![str_lit("other")]),
            )]),
        );
        let match_stmt = Ast::match_stmt(ident("dir"), vec![north_arm, wild_arm]);

        let ast = Ast::block(vec![enum_decl, dir_decl, match_stmt]);
        let mut vm = build_vm(ast);

        let ev = vm.next(None).expect("event").expect("no error");
        match ev {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(ps.to_string(), "going north");
                }
                other => panic!("expected 'going north', got {:?}", other),
            },
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `eval_expr_list` returns all elements of an ExprList.
    #[test]
    fn test_eval_expr_list_all_elements() {
        let env = Environment::new();
        let list = Ast::expr_list(vec![int(1), int(2), int(3)]);
        let result = eval_expr_list(&list, &env).expect("eval");
        assert_eq!(
            result,
            vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3)
            ]
        );
    }

    /// A Dialogue with multiple speakers correctly emits all of them.
    #[test]
    fn test_dialogue_multiple_speakers() {
        let speakers = Ast::expr_list(vec![str_lit("Alice"), str_lit("Bob")]);
        let lines = Ast::expr_list(vec![str_lit("Together!")]);
        let ast = Ast::dialogue(speakers, lines);

        let mut vm = build_vm(ast);
        let ev = vm.next(None).expect("event").expect("no error");
        match ev {
            Event::Dialogue { speakers, .. } => {
                assert_eq!(speakers.len(), 2, "expected 2 speakers");
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    // ── Script-decorator integration tests ────────────────────────────────────

    /// A script-defined decorator that writes `event["camera_shake"] = amount`
    /// should produce a Dialogue event with that field set.
    ///
    /// Script equivalent:
    /// ```
    /// decorator shake(amount) { event["camera_shake"] = amount }
    /// @shake(0.5)
    /// Alice: "Watch out!"
    /// ```
    #[test]
    fn test_script_decorator_mutates_event_fields() {
        use crate::parser::ast::{DecoratorParam, EventConstraint};

        // Build the decorator body: `event["camera_shake"] = amount`
        let event_ident = Ast::value(RuntimeValue::IdentPath(vec!["event".to_string()]));
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("camera_shake"),
        ));
        let amount_ident = Ast::value(RuntimeValue::IdentPath(vec!["amount".to_string()]));
        let subscript_assign = Ast::subscript_assign(event_ident, key_ast, amount_ident);
        let body = Ast::block(vec![subscript_assign]);

        let decorator_def = Ast::decorator_def(
            "shake".to_string(),
            EventConstraint::Any,
            vec![DecoratorParam {
                name: "amount".to_string(),
                type_annotation: None,
            }],
            body,
        );

        // @shake(0.5) Alice: "Watch out!"
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Watch out!")]);
        let deco = Decorator::new(
            "shake".to_string(),
            Ast::expr_list(vec![Ast::value(RuntimeValue::Float(0.5))]),
        );
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let ast = Ast::block(vec![decorator_def, dialogue]);
        let mut vm = build_vm(ast);

        let ev = vm.next(None).expect("expected event").expect("no error");
        match ev {
            Event::Dialogue { fields, .. } => {
                assert!(
                    fields.contains_key("camera_shake"),
                    "expected 'camera_shake' in fields, got {:?}",
                    fields
                );
                assert_eq!(
                    fields.get("camera_shake"),
                    Some(&RuntimeValue::Float(0.5)),
                    "camera_shake should be Float(0.5)"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// A VM built with an empty Rust registry but a script-defined decorator
    /// must NOT return `VmError::UnknownDecorator` — the definition in the
    /// script is sufficient.
    #[test]
    fn test_script_decorator_does_not_require_rust_registration() {
        use crate::parser::ast::EventConstraint;

        // decorator noop() { }
        let decorator_def = Ast::decorator_def(
            "noop".to_string(),
            EventConstraint::Any,
            vec![],
            Ast::block(vec![]),
        );

        // @noop() Alice: "Hi"
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hi")]);
        let deco = Decorator::new("noop".to_string(), Ast::expr_list(vec![]));
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let ast = Ast::block(vec![decorator_def, dialogue]);
        let graph = Compiler::compile(&ast).expect("compile ok");

        // Empty registry — must still succeed.
        let result = Vm::new(graph, empty_registry());
        assert!(
            result.is_ok(),
            "script-defined decorator should not require Rust registration, got {:?}",
            result
        );
    }

    /// Evaluating a Map literal `:{\"key\": 42}` returns a `RuntimeValue::Map`.
    #[test]
    fn test_map_literal_eval() {
        let env = Environment::new();
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("key"),
        ));
        let val_ast = int(42);
        let map_ast = Ast::map(vec![(key_ast, val_ast)]);

        let result = eval_expr(&map_ast, &env).expect("map eval");
        match result {
            RuntimeValue::Map(m) => {
                assert_eq!(m.len(), 1);
                assert_eq!(*m["key"], RuntimeValue::Int(42));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    /// `Map[key]` subscript read returns the correct value.
    #[test]
    fn test_subscript_read() {
        let mut env = Environment::new();

        // let m = :{"a": 99}
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        ));
        let val_ast = int(99);
        let map_ast = Ast::map(vec![(key_ast, val_ast)]);
        env.set("m", eval_expr(&map_ast, &env).unwrap(), &DeclKind::Variable)
            .unwrap();

        // m["a"]
        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()])),
            Ast::value(RuntimeValue::Str(
                crate::lexer::strings::ParsedString::new_plain("a"),
            )),
        );

        let result = eval_expr(&subscript, &env).expect("subscript read");
        assert_eq!(result, RuntimeValue::Int(99));
    }

    /// Subscript assignment `m["a"] = 99` mutates the map stored in the env.
    #[test]
    fn test_subscript_assign_mutates_map() {
        let mut env = Environment::new();

        // let m = :{"a": 1}
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        ));
        let val_ast = int(1);
        let map_ast = Ast::map(vec![(key_ast, val_ast)]);
        env.set("m", eval_expr(&map_ast, &env).unwrap(), &DeclKind::Variable)
            .unwrap();

        // m["a"] = 99
        let obj = Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()]));
        let key = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        ));
        let val = int(99);
        eval_subscript_assign(&obj, &key, &val, &mut env).expect("subscript assign");

        // Read back m["a"]
        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()])),
            Ast::value(RuntimeValue::Str(
                crate::lexer::strings::ParsedString::new_plain("a"),
            )),
        );
        let result = eval_expr(&subscript, &env).expect("read back");
        assert_eq!(
            result,
            RuntimeValue::Int(99),
            "m[\"a\"] should be 99 after assign"
        );
    }

    #[test]
    fn test_label_values_preseeded_in_env() {
        // Build a script with a labeled block, then verify the label name
        // is accessible as RuntimeValue::Label in the environment before
        // any script execution.
        let body = Ast::block(vec![]);
        let labeled = Ast::labeled_block("scene_one".to_string(), body);
        let script = Ast::block(vec![labeled]);

        let graph = Compiler::compile(&script).expect("compile");
        let vm = Vm::new(graph, empty_registry()).expect("vm construction");

        // The label "scene_one" should be pre-seeded in the environment
        // Extract the NodeId the compiler assigned to "scene_one" so we can
        // construct the expected value with the correct concrete reference.
        let expected_node_id = *vm.graph().labels.get("scene_one").expect("label in graph");
        let val = vm.env().get("scene_one").expect("label should be in env");
        assert_eq!(
            val,
            RuntimeValue::Label {
                name: "scene_one".to_string(),
                node_id: expected_node_id,
            }
        );
    }

    #[test]
    fn test_label_value_equality() {
        use crate::ir::NodeId;
        // Equality is by node_id — the canonical execution reference.
        assert!(values_equal(
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeId(0)
            },
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeId(0)
            },
        ));
        // Different node_ids → not equal, even if names happened to match.
        assert!(!values_equal(
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeId(0)
            },
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeId(1)
            },
        ));
    }

    #[test]
    fn test_label_value_in_decorator_fields() {
        // Full end-to-end: define a decorator that stores a label in event fields,
        // then verify the emitted Event::Dialogue has the Label value in fields.
        //
        // Script:
        //   label fallback { }
        //   decorator timed(fallback_label) {
        //       event["next"] = fallback_label
        //   }
        //   @timed(fallback)
        //   <Alice>: "Hurry!"
        use crate::parser::ast::{DecoratorParam, EventConstraint};

        // label fallback { }
        let fallback_label = Ast::labeled_block("fallback".to_string(), Ast::block(vec![]));

        // decorator timed(fallback_label) { event["next"] = fallback_label }
        let event_ident = Ast::value(RuntimeValue::IdentPath(vec!["event".to_string()]));
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("next"),
        ));
        let param_ident = Ast::value(RuntimeValue::IdentPath(vec!["fallback_label".to_string()]));
        let subscript_assign = Ast::subscript_assign(event_ident, key_ast, param_ident);
        let dec_body = Ast::block(vec![subscript_assign]);

        let decorator_def = Ast::decorator_def(
            "timed".to_string(),
            EventConstraint::Any,
            vec![DecoratorParam {
                name: "fallback_label".to_string(),
                type_annotation: None,
            }],
            dec_body,
        );

        // @timed(fallback) Alice: "Hurry!"
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hurry!")]);
        let deco = Decorator::new(
            "timed".to_string(),
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "fallback".to_string(),
            ]))]),
        );
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let ast = Ast::block(vec![fallback_label, decorator_def, dialogue]);
        let mut vm = build_vm(ast);

        let ev = vm.next(None).expect("expected event").expect("no error");
        match ev {
            Event::Dialogue { fields, .. } => {
                assert!(
                    fields.contains_key("next"),
                    "expected 'next' in fields, got {:?}",
                    fields
                );
                // Check that the value is a Label pointing at "fallback".
                // We match on the name rather than constructing the full value
                // to avoid coupling this test to the compiler's NodeId assignment.
                match fields.get("next") {
                    Some(RuntimeValue::Label { name, .. }) => assert_eq!(
                        name, "fallback",
                        "expected Label pointing at 'fallback', got name '{name}'"
                    ),
                    other => panic!("expected Label for 'next' field, got {:?}", other),
                }
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `jump label and return` (compiled to LetCall with empty var) pushes a
    /// call frame, executes the label body, and after `return` resumes at the
    /// continuation after the call site.
    #[test]
    fn test_subroutine_call_and_return() {
        // Execution order in source (mirrors a real Urd script):
        //
        //   jump greet and return      ← entry: LetCall
        //   Alice: "After call"        ← continuation after return
        //   return                     ← explicit end so VM doesn't fall into greet
        //   label greet {
        //       Alice: "Hello from greet"
        //       return
        //   }
        //
        // AST block is compiled right-to-left, so `call_jump` becomes the
        // graph entry and `greet_label` is at the tail (never reached
        // sequentially from the main flow).
        let speakers1 = Ast::expr_list(vec![str_lit("Alice")]);
        let lines1 = Ast::expr_list(vec![str_lit("Hello from greet")]);
        let greet_dialogue = Ast::dialogue(speakers1, lines1);
        let greet_body = Ast::block(vec![greet_dialogue, Ast::return_stmt(None)]);
        let greet_label = Ast::labeled_block("greet".to_string(), greet_body);

        // jump greet and return  (expects_return = true → compiles to LetCall)
        let call_jump = Ast::jump_stmt("greet".to_string(), true);

        let speakers2 = Ast::expr_list(vec![str_lit("Alice")]);
        let lines2 = Ast::expr_list(vec![str_lit("After call")]);
        let after_dialogue = Ast::dialogue(speakers2, lines2);

        // Explicit return ends the main flow so the VM doesn't fall through
        // into the greet label definition.
        let main_return = Ast::return_stmt(None);

        // Layout: [call_jump → after_dialogue → main_return → greet_label]
        // Entry = call_jump (leftmost in vec = first compiled = graph entry).
        let ast = Ast::block(vec![call_jump, after_dialogue, main_return, greet_label]);
        let mut vm = build_vm(ast);

        // First event: dialogue emitted from inside greet.
        let ev1 = vm.next(None).expect("expected first event").expect("no error");
        match ev1 {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => assert_eq!(
                    ps.to_string(),
                    "Hello from greet",
                    "first event should be the greet dialogue"
                ),
                other => panic!("expected Str, got {:?}", other),
            },
            other => panic!("expected Dialogue for greet, got {:?}", other),
        }

        // Second event: the continuation dialogue after the call returns.
        let ev2 = vm.next(None).expect("expected second event").expect("no error");
        match ev2 {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => assert_eq!(
                    ps.to_string(),
                    "After call",
                    "second event should be the post-call dialogue"
                ),
                other => panic!("expected Str, got {:?}", other),
            },
            other => panic!("expected Dialogue after return, got {:?}", other),
        }

        assert!(vm.next(None).is_none(), "script should end after second dialogue");
    }

    /// `let result = jump double and return` binds the subroutine's return
    /// value to `result` and execution continues after the call site.
    #[test]
    fn test_let_call_captures_return_value() {
        // label double {
        //     return 42
        // }
        // let result = jump double and return
        // Alice: result
        let double_body = Ast::block(vec![Ast::return_stmt(Some(int(42)))]);
        let double_label = Ast::labeled_block("double".to_string(), double_body);

        let let_call = Ast::let_call("result".to_string(), "double".to_string());

        let speakers = Ast::expr_list(vec![str_lit("Bot")]);
        let lines = Ast::expr_list(vec![ident("result")]);
        let dialogue = Ast::dialogue(speakers, lines);

        // Layout: [let_call → dialogue → double_label]
        // Entry = let_call (leftmost). The explicit return inside double means
        // the VM never falls sequentially into double_label from dialogue.
        let ast = Ast::block(vec![let_call, dialogue, double_label]);
        let mut vm = build_vm(ast);

        let ev = vm.next(None).expect("expected event").expect("no error");
        match ev {
            Event::Dialogue { lines, .. } => {
                assert_eq!(
                    lines,
                    vec![RuntimeValue::Int(42)],
                    "result should be the return value 42"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }

        assert!(vm.next(None).is_none(), "script should end after dialogue");
    }

    /// A plain `jump label` (expects_return=false) compiles to
    /// `IrNodeKind::Jump`, never to `IrNodeKind::LetCall`.
    ///
    /// This is the compiler-level invariant that guarantees no LetCall frame
    /// is ever pushed at runtime for a plain unconditional jump.
    #[test]
    fn test_jump_without_return_does_not_push_frame() {
        // label dest { }
        // jump dest          ← plain jump, expects_return = false
        let dest_label = Ast::labeled_block("dest".to_string(), Ast::block(vec![]));
        let jump = Ast::jump_stmt("dest".to_string(), false);
        let ast = Ast::block(vec![dest_label, jump]);

        let graph = Compiler::compile(&ast).expect("compile failed");

        // Plain jump must emit IrNodeKind::Jump, never LetCall.
        let has_let_call = graph
            .nodes
            .iter()
            .any(|n| matches!(n.kind, IrNodeKind::LetCall { .. }));
        assert!(
            !has_let_call,
            "plain `jump label` must not emit a LetCall node; graph = {graph:?}"
        );

        let has_jump = graph
            .nodes
            .iter()
            .any(|n| matches!(n.kind, IrNodeKind::Jump { .. }));
        assert!(
            has_jump,
            "plain `jump label` must emit a Jump node; graph = {graph:?}"
        );
    }
}
