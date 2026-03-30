//! Decorator registry: named handlers that enrich dialogue/choice events.

use std::collections::HashMap;

use crate::ir::NODE_END;
use crate::parser::ast::{AstContent, Decorator};
use crate::runtime::value::RuntimeValue;

use super::VmError;
use super::env::Environment;
use super::eval::eval_expr;

// ─── DecoratorRegistry ────────────────────────────────────────────────────────

/// Type alias for a boxed decorator handler function.
type DecoratorHandler = Box<dyn Fn(&[RuntimeValue]) -> HashMap<String, RuntimeValue> + Send + Sync>;

/// Registry of named decorator handlers used to evaluate `@decorator` annotations
/// attached to [`IrNodeKind::Dialogue`] and [`IrNodeKind::Choice`] nodes.
///
/// Register handlers with [`DecoratorRegistry::register`] before constructing
/// a [`Vm`]; the VM's validation pass will reject any decorator name that is
/// not present in the registry.
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
    /// validation pass in [`Vm::new`] accepts `@name(args)` on
    /// dialogue/choice nodes.
    ///
    /// Script-defined decorator bodies are already evaluated automatically by
    /// the VM via [`RuntimeValue::ScriptDecorator`] stored in the environment;
    /// this method is only needed when you also want a native Rust handler to
    /// run for the same decorator name.
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
        let args = eval_decorator_args(decorator, env)?;

        Ok(handler(&args))
    }
}

/// Evaluate all arguments of a decorator, returning them as a `Vec<RuntimeValue>`.
pub(super) fn eval_decorator_args(
    dec: &Decorator,
    env: &Environment,
) -> Result<Vec<RuntimeValue>, VmError> {
    match dec.args().content() {
        AstContent::ExprList(items) => items
            .iter()
            .map(|a| eval_expr(a, env))
            .collect::<Result<Vec<_>, _>>(),
        _ => Ok(vec![eval_expr(dec.args(), env)?]),
    }
}
