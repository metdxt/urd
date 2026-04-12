//! Synchronous execution helpers for the VM.
//!
//! These free functions handle subscript assignment and the synchronous
//! block/statement executor used by script-defined decorator bodies.

use std::collections::HashMap;

use crate::parser::ast::{AstContent, DeclKind};
use crate::runtime::value::RuntimeValue;

use super::VmError;
use super::env::Environment;
use super::eval::eval_expr;

// ─── Subscript-assign helper ──────────────────────────────────────────────────

/// Handles `object[key] = value` mutations by modifying the map stored under
/// `object`'s identifier path in `env`.
pub(super) fn eval_subscript_assign(
    object: &crate::parser::ast::Ast,
    key: &crate::parser::ast::Ast,
    value: &crate::parser::ast::Ast,
    env: &mut Environment,
) -> Result<(), VmError> {
    // Resolve the variable name that holds the map or list.
    let var_name = match object.content() {
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => path[0].clone(),
        _ => {
            return Err(VmError::InvalidExpression(
                "subscript assign: object must be a simple identifier".into(),
            ));
        }
    };

    // Evaluate the key and new value.
    let key_val = eval_expr(key, env)?;
    let new_val = eval_expr(value, env)?;

    // Fetch the current value of the variable.
    let current = env
        .get(&var_name)
        .map_err(|_| VmError::UndefinedVariable(var_name.clone()))?;

    match current {
        RuntimeValue::Map(mut map) => {
            let key_str = match key_val {
                RuntimeValue::Str(ref ps) => ps.to_string(),
                RuntimeValue::Int(i) => i.to_string(),
                other => {
                    return Err(VmError::TypeError(format!(
                        "map key must be Str or Int, got {other:?}"
                    )));
                }
            };
            map.insert(key_str, Box::new(new_val));
            env.set(&var_name, RuntimeValue::Map(map), &DeclKind::Variable)
        }
        RuntimeValue::List(mut list) => {
            let idx = match key_val {
                RuntimeValue::Int(i) => i,
                other => {
                    return Err(VmError::TypeError(format!(
                        "list index must be Int, got {other:?}"
                    )));
                }
            };
            let len = list.len();
            // Support Python-style negative indexing.
            let actual = if idx < 0 {
                let pos = len as i64 + idx;
                if pos < 0 {
                    return Err(VmError::IndexOutOfBounds { index: idx, len });
                }
                pos as usize
            } else {
                let pos = idx as usize;
                if pos >= len {
                    return Err(VmError::IndexOutOfBounds { index: idx, len });
                }
                pos
            };
            list[actual] = new_val;
            env.set(&var_name, RuntimeValue::List(list), &DeclKind::Variable)
        }
        RuntimeValue::Extern(handle) => {
            let key_str = match key_val {
                RuntimeValue::Str(ref ps) => ps.to_string(),
                other => {
                    return Err(VmError::TypeError(format!(
                        "extern field key must be a Str, got {other:?}"
                    )));
                }
            };
            handle.set(&key_str, new_val).map_err(VmError::TypeError)
        }
        other => Err(VmError::TypeError(format!(
            "subscript assign: {var_name} is not a map, list, or extern, got {other:?}"
        ))),
    }
}

// ─── Sync block executor ─────────────────────────────────────────────────────

/// Executes a sequence of AST statements synchronously, collecting any
/// side-effects into `env`.
///
/// Used by script-defined decorator bodies and other inline-evaluated blocks
/// that must run to completion before the VM can continue.
pub(super) fn exec_block_sync(
    block: &crate::parser::ast::Ast,
    env: &mut Environment,
    event: &mut HashMap<String, RuntimeValue>,
) -> Result<(), VmError> {
    let stmts = match block.content() {
        AstContent::Block(stmts) => stmts,
        _ => {
            // Single statement.
            exec_stmt_sync(block, env, event)?;
            refresh_event_snapshot(env, event)?;
            return Ok(());
        }
    };

    for stmt in stmts {
        exec_stmt_sync(stmt, env, event)?;
        // After each statement, refresh the `event` binding in `env` so that
        // subsequent reads (e.g. `event["x"]`) see the latest mutations
        // applied by `exec_stmt_sync` to the canonical `event` map.
        refresh_event_snapshot(env, event)?;
    }
    Ok(())
}

/// Re-synchronises the `"event"` variable in `env` with the canonical `event`
/// map so that expression evaluation sees the most recent mutations.
pub(super) fn refresh_event_snapshot(
    env: &mut Environment,
    event: &HashMap<String, RuntimeValue>,
) -> Result<(), VmError> {
    let snapshot: HashMap<String, Box<RuntimeValue>> = event
        .iter()
        .map(|(k, v)| (k.clone(), Box::new(v.clone())))
        .collect();
    env.set("event", RuntimeValue::Map(snapshot), &DeclKind::Variable)
}

/// Executes a single AST statement inside a decorator body.
pub(super) fn exec_stmt_sync(
    stmt: &crate::parser::ast::Ast,
    env: &mut Environment,
    event: &mut HashMap<String, RuntimeValue>,
) -> Result<(), VmError> {
    match stmt.content() {
        AstContent::Declaration {
            kind,
            decl_name,
            decl_defs,
            ..
        } => {
            let name_str = match decl_name.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
                _ => {
                    return Err(VmError::InvalidExpression(
                        "declaration name must be an identifier".into(),
                    ));
                }
            };
            let v = eval_expr(decl_defs, env)?;
            env.set(&name_str, v, kind)?;
        }
        AstContent::SubscriptAssign { object, key, value } => {
            // Check if the object is `event` — if so, write into the event map
            // instead of the variable environment.
            let is_event = matches!(
                object.content(),
                AstContent::Value(RuntimeValue::IdentPath(p)) if p.len() == 1 && p[0] == "event"
            );
            if is_event {
                let key_val = eval_expr(key, env)?;
                let new_val = eval_expr(value, env)?;
                let key_str = match key_val {
                    RuntimeValue::Str(ref ps) => ps.to_string(),
                    RuntimeValue::Int(i) => i.to_string(),
                    other => {
                        return Err(VmError::TypeError(format!(
                            "event key must be Str or Int, got {other:?}"
                        )));
                    }
                };
                event.insert(key_str, new_val);
            } else {
                eval_subscript_assign(object, key, value, env)?;
            }
        }
        _ => {
            eval_expr(stmt, env)?;
        }
    }
    Ok(())
}
