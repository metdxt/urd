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
///
/// The handler receives a slice of evaluated [`RuntimeValue`] arguments and
/// returns a [`HashMap`] of fields to merge into the emitted event, or a
/// [`VmError`] if execution fails (e.g. arity mismatch in a script-body
/// handler).  Infallible Rust handlers registered via
/// [`DecoratorRegistry::register`] are wrapped in `Ok(...)` automatically.
type DecoratorHandler =
    Box<dyn Fn(&[RuntimeValue]) -> Result<HashMap<String, RuntimeValue>, VmError> + Send + Sync>;

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

    /// Registers an infallible decorator handler under `name`.
    ///
    /// The handler receives a slice of evaluated [`RuntimeValue`] arguments
    /// and returns a [`HashMap`] of fields to merge into the emitted event.
    /// The infallible closure is wrapped in `Ok(...)` internally so that it
    /// satisfies the fallible [`DecoratorHandler`] storage type.
    pub fn register(
        &mut self,
        name: impl Into<String>,
        handler: impl Fn(&[RuntimeValue]) -> HashMap<String, RuntimeValue> + Send + Sync + 'static,
    ) {
        self.handlers
            .insert(name.into(), Box::new(move |args| Ok(handler(args))));
    }

    /// Returns an iterator over all registered decorator names.
    pub fn known_names(&self) -> impl Iterator<Item = &str> {
        self.handlers.keys().map(String::as_str)
    }

    /// Registers a passthrough decorator handler under `name`.
    ///
    /// The passthrough handler converts the decorator's argument list to a
    /// `HashMap` with sequential integer string keys (`"0"`, `"1"`, …) mapping
    /// to the argument [`RuntimeValue`]s. Additionally, the full argument list
    /// is stored under the `"_args"` key as a `RuntimeValue::List`.
    ///
    /// This is useful for engine integrations that want to expose raw decorator
    /// metadata without writing per-decorator Rust handlers.
    ///
    /// If an entry for `name` already exists in the registry it is left
    /// unchanged (first registration wins).
    pub fn register_passthrough(&mut self, name: impl Into<String>) {
        let name_str = name.into();
        self.handlers.entry(name_str).or_insert_with(|| {
            Box::new(|args: &[RuntimeValue]| {
                let mut fields = HashMap::new();
                // Positional keys "0", "1", …
                for (i, v) in args.iter().enumerate() {
                    fields.insert(i.to_string(), v.clone());
                }
                // "_args" as a List — replace non-serialisable variants with
                // their display string so the List invariant is upheld.
                let safe_args: Vec<RuntimeValue> = args
                    .iter()
                    .map(|v| match v {
                        RuntimeValue::Roll(_)
                        | RuntimeValue::Function { .. }
                        | RuntimeValue::ScriptDecorator { .. }
                        | RuntimeValue::Struct { .. }
                        | RuntimeValue::Map(_) => {
                            RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain(
                                &format!("{:?}", v),
                            ))
                        }
                        other => other.clone(),
                    })
                    .collect();
                fields.insert("_args".to_string(), RuntimeValue::list(safe_args));
                Ok(fields)
            })
        });
    }

    /// Registers a script-defined decorator body as a Rust-side handler.
    ///
    /// When the handler fires, it executes `body` as a **pure** function body
    /// (via [`crate::vm::eval::exec_fn_body`]) with `params` bound to the
    /// evaluated call-site arguments.  If the body returns a
    /// [`RuntimeValue::Map`], its entries are used as the extra event fields;
    /// any other return value is treated as an empty field set.
    ///
    /// Unlike script-defined decorators executed through the environment (via
    /// `apply_decorator`'s env-lookup path), this handler path does **not**
    /// expose or mutate an `event` map — the body must return a Map literal
    /// explicitly.
    ///
    /// If an entry for `name` already exists in the registry it is left
    /// unchanged (first registration wins).
    ///
    /// # Errors
    ///
    /// Returns `Ok(())` unconditionally.  The `Result` return type is kept for
    /// forward compatibility with richer validation.
    pub fn define_script_decorator(
        &mut self,
        name: String,
        _event_constraint: crate::parser::ast::EventConstraint,
        params: Vec<String>,
        body: crate::parser::ast::Ast,
    ) -> Result<(), VmError> {
        self.handlers.entry(name).or_insert_with(|| {
            // Both `params` and `body` are moved into the handler closure.
            // `exec_fn_body` creates an isolated environment, binds each
            // parameter to the corresponding argument, and evaluates the body.
            Box::new(
                move |args: &[RuntimeValue]| -> Result<HashMap<String, RuntimeValue>, VmError> {
                    match super::eval::exec_fn_body(&body, &params, args) {
                        Ok(RuntimeValue::Map(m)) => {
                            // Convert HashMap<String, Box<RuntimeValue>> →
                            // HashMap<String, RuntimeValue>.
                            Ok(m.into_iter().map(|(k, v)| (k, *v)).collect())
                        }
                        // Body returned something other than a Map — treat as
                        // no additional event fields.
                        Ok(_) => Ok(HashMap::new()),
                        // Propagate execution errors (e.g. arity mismatch).
                        Err(e) => Err(e),
                    }
                },
            )
        });
        Ok(())
    }

    /// Evaluates `decorator`'s arguments and invokes the registered handler.
    ///
    /// # Errors
    /// Returns [`VmError::UnknownDecorator`] (with a sentinel [`NodeIndex::end()`])
    /// if the decorator name is not registered.  Callers that know the real node
    /// index should override it in the error.
    ///
    /// Also propagates any [`VmError`] returned by the handler itself (e.g. an
    /// arity mismatch when a script-body handler is invoked with the wrong
    /// number of arguments).
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

        // Handler is now fallible — propagate any error it produces.
        handler(&args)
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
