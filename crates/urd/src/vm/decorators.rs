//! Decorator validation, definition, and dispatch.
//!
//! Contains the helpers that check decorator registration at VM construction
//! time and apply both script-defined and Rust-registered decorators at
//! runtime.

use std::collections::{HashMap, HashSet};

use petgraph::stable_graph::NodeIndex;

use crate::parser::ast::{AstContent, DeclKind, Decorator, EventConstraint};
use crate::runtime::value::RuntimeValue;

use super::VmError;
use super::env::Environment;
use super::eval::eval_expr;
use super::exec::exec_block_sync;
use super::registry::DecoratorRegistry;

// ─── Decorator validation helper ─────────────────────────────────────────────

/// Check that a single decorator name is registered (either in the Rust
/// registry or as a script-defined decorator). Returns `Err` on the first
/// unregistered name.
pub(super) fn check_decorator_known(
    dec: &Decorator,
    registry: &DecoratorRegistry,
    script_defined: &HashSet<&str>,
    node_id: NodeIndex,
) -> Result<(), VmError> {
    if !registry.handlers.contains_key(dec.name()) && !script_defined.contains(dec.name()) {
        return Err(VmError::UnknownDecorator {
            name: dec.name().to_string(),
            node_id,
        });
    }
    Ok(())
}

// ─── Script-decorator execution ──────────────────────────────────────────────

/// Bundles the compile-time definition data for a script-defined decorator.
///
/// Passed by reference to [`apply_script_decorator`] so that function stays
/// within Clippy's `too_many_arguments` limit.
struct ScriptDecoratorDef<'a> {
    /// The decorator's registered name (used in error messages).
    name: &'a str,
    /// The event-kind constraint declared on the decorator.
    event_constraint: &'a EventConstraint,
    /// Ordered parameter names.
    params: &'a [String],
    /// The decorator body block.
    body: &'a crate::parser::ast::Ast,
}

/// Runs a script-defined decorator body against `event`, mutating its fields.
///
/// # Errors
///
/// Returns [`VmError::ConstraintViolation`] when `def.event_constraint` is
/// `Dialogue` but `actual_kind` is `Choice`, or vice-versa.
fn apply_script_decorator(
    def: &ScriptDecoratorDef<'_>,
    args: &[crate::parser::ast::Ast],
    env: &Environment,
    mut event: HashMap<String, RuntimeValue>,
    actual_kind: &EventConstraint,
) -> Result<HashMap<String, RuntimeValue>, VmError> {
    // Enforce the event-kind constraint declared on the decorator.
    match def.event_constraint {
        EventConstraint::Any => {}
        EventConstraint::Dialogue => {
            if matches!(actual_kind, EventConstraint::Choice) {
                return Err(VmError::ConstraintViolation {
                    decorator: def.name.to_string(),
                    constraint: "dialogue".to_string(),
                    actual_event: "choice".to_string(),
                });
            }
        }
        EventConstraint::Choice => {
            if matches!(actual_kind, EventConstraint::Dialogue) {
                return Err(VmError::ConstraintViolation {
                    decorator: def.name.to_string(),
                    constraint: "choice".to_string(),
                    actual_event: "dialogue".to_string(),
                });
            }
        }
    }

    // Bind arguments to parameter names in a fresh inner scope.
    let mut inner_env = env.clone();
    inner_env.push_scope();

    // Arity check: parameter count must match argument count exactly.
    if def.params.len() != args.len() {
        return Err(VmError::TypeError(format!(
            "decorator '{}' expects {} argument(s), got {}",
            def.name,
            def.params.len(),
            args.len()
        )));
    }

    for (param, arg_ast) in def.params.iter().zip(args.iter()) {
        if param == "event" {
            return Err(VmError::TypeError(
                "decorator parameter cannot be named 'event' (reserved)".into(),
            ));
        }
        let val = eval_expr(arg_ast, env)?;
        inner_env.set(param.as_str(), val, &DeclKind::Variable)?;
    }
    // Expose the mutable `event` map as an IdentPath value so the body can
    // read existing fields.  Mutations are applied via `exec_block_sync`.
    let event_snapshot: HashMap<String, Box<RuntimeValue>> = event
        .iter()
        .map(|(k, v)| (k.clone(), Box::new(v.clone())))
        .collect();
    inner_env.set(
        "event",
        RuntimeValue::Map(crate::runtime::value::shared(event_snapshot)),
        &DeclKind::Variable,
    )?;

    exec_block_sync(def.body, &mut inner_env, &mut event)?;
    Ok(event)
}

// ─── Decorator dispatch ───────────────────────────────────────────────────────

/// Applies one decorator to the running `event` field map.
///
/// Checks the environment for a script-defined decorator first; falls back to
/// the Rust [`DecoratorRegistry`].
///
/// `actual_kind` indicates the kind of event being decorated (`Dialogue` or
/// `Choice`); it is forwarded to [`apply_script_decorator`] for constraint
/// checking.  Pass [`EventConstraint::Any`] when the kind is irrelevant.
pub(super) fn apply_decorator(
    dec: &Decorator,
    env: &Environment,
    registry: &DecoratorRegistry,
    event: HashMap<String, RuntimeValue>,
    actual_kind: &EventConstraint,
) -> Result<HashMap<String, RuntimeValue>, VmError> {
    // Check whether this decorator is script-defined (stored in env).
    if let Ok(RuntimeValue::ScriptDecorator {
        event_constraint,
        params,
        body,
    }) = env.get(dec.name())
    {
        // dec.args() always returns &Ast; unpack ExprList items as argument ASTs.
        let args_ast = dec.args();
        let args: Vec<crate::parser::ast::Ast> = match args_ast.content() {
            AstContent::ExprList(items) => items.clone(),
            _ => vec![args_ast.clone()],
        };
        return apply_script_decorator(
            &ScriptDecoratorDef {
                name: dec.name(),
                event_constraint: &event_constraint,
                params: &params,
                body: &body,
            },
            &args,
            env,
            event,
            actual_kind,
        );
    }

    // Fall back to Rust-registered handler.
    let extra_fields = registry.apply(dec, env)?;
    Ok(event.into_iter().chain(extra_fields).collect())
}
