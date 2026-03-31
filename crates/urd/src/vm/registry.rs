//! Decorator registry: named handlers that enrich dialogue/choice events.

use std::collections::HashMap;

use petgraph::stable_graph::NodeIndex;

use crate::parser::ast::{AstContent, Decorator};
use crate::runtime::value::RuntimeValue;

use super::VmError;
use super::env::Environment;
use super::eval::eval_expr;

// ─── DecoratorRegistry ────────────────────────────────────────────────────────

/// Type alias for a boxed decorator handler function.
type DecoratorHandler = Box<dyn Fn(&[RuntimeValue]) -> HashMap<String, RuntimeValue> + Send + Sync>;

/// Registry of named decorator handlers used to evaluate `@decorator` annotations
/// attached to [`crate::ir::IrNodeKind::Dialogue`] and
/// [`crate::ir::IrNodeKind::Choice`] nodes.
///
/// Register handlers with [`DecoratorRegistry::register`] before constructing
/// a [`crate::vm::Vm`]; the VM's validation pass will reject any decorator name
/// that is not present in the registry.
pub struct DecoratorRegistry {
    pub(super) handlers: HashMap<String, DecoratorHandler>,
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

    /// Registers a no-op Rust-side stub handler under `name` so that the
    /// validation pass in [`crate::vm::Vm::new`] accepts `@name(args)` on
    /// dialogue/choice nodes.
    ///
    /// Script-defined decorator bodies are already evaluated automatically by
    /// the VM via [`crate::runtime::value::RuntimeValue::ScriptDecorator`]
    /// stored in the environment; this method is only needed when you also want
    /// a native Rust handler to run for the same decorator name.
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
        // Rust closure.
        self.handlers
            .entry(name)
            .or_insert_with(|| Box::new(|_args: &[RuntimeValue]| HashMap::new()));
        Ok(())
    }

    /// Evaluates `decorator`'s arguments and invokes the registered handler.
    ///
    /// # Errors
    /// Returns [`VmError::UnknownDecorator`] (with a sentinel [`NodeIndex::end()`])
    /// if the decorator name is not registered.  Callers that know the real node
    /// index should override it in the error.
    pub fn apply(
        &self,
        decorator: &Decorator,
        env: &Environment,
    ) -> Result<HashMap<String, RuntimeValue>, VmError> {
        let handler = self.handlers.get(decorator.name()).ok_or_else(|| {
            VmError::UnknownDecorator {
                name: decorator.name().to_string(),
                // Use NodeIndex::end() as the sentinel "no node" value.
                // This mirrors the former NODE_END sentinel.  Callers that know
                // the actual node index (e.g. the VM's validation pass) should
                // supply a real NodeIndex instead.
                node_id: NodeIndex::end(),
            }
        })?;

        // Evaluate every argument in the ExprList (or a bare single argument).
        let args = eval_decorator_args(decorator.args(), env)?;

        Ok(handler(&args))
    }
}

/// Evaluates all arguments of a decorator's argument AST, returning them as a
/// `Vec<RuntimeValue>`.
///
/// `args_ast` is the AST node returned by [`Decorator::args()`]; it is either
/// an [`AstContent::ExprList`] (the common case) or a bare single-expression
/// node.
pub(super) fn eval_decorator_args(
    args_ast: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<Vec<RuntimeValue>, VmError> {
    match args_ast.content() {
        AstContent::ExprList(items) => items
            .iter()
            .map(|a| eval_expr(a, env))
            .collect::<Result<Vec<_>, _>>(),
        _ => Ok(vec![eval_expr(args_ast, env)?]),
    }
}
