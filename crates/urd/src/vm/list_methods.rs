//! # List Methods
//!
//! This module implements all built-in methods callable on `RuntimeValue::List`
//! via method-call syntax (e.g. `list.len()`, `list.append(x)`).
//!
//! ## Pure-functional semantics
//!
//! All "mutating" methods — `append`, `prepend`, `pop`, `concat`, `reversed`,
//! `with`, `slice` — return a **new** `RuntimeValue::List` and leave the
//! original list untouched.  To update a variable the caller must reassign:
//!
//! ```urd
//! xs = xs.append(v)
//! xs = xs.pop()
//! ```
//!
//! This is consistent with Urd's immutable-value model: a list variable holds
//! a value; methods produce new values; assignment is how you "update" state.
//!
//! The single public entry point is [`dispatch`], which is called by the
//! evaluator whenever it encounters a method call whose receiver is a
//! `RuntimeValue::List`.

use crate::lexer::strings::ParsedString;
use crate::runtime::value::RuntimeValue;

// ─── Public entry point ───────────────────────────────────────────────────────

/// Dispatch a method call on a `RuntimeValue::List`.
///
/// # Parameters
///
/// - `list`   — the owned inner `Vec` extracted from the receiver
/// - `method` — the method name (e.g. `"len"`, `"append"`)
/// - `args`   — already-evaluated argument values
///
/// # Errors
///
/// Returns a [`super::VmError`] on type mismatches, out-of-bounds accesses,
/// wrong argument counts, or unknown method names.
pub(super) fn dispatch(
    list: Vec<RuntimeValue>,
    method: &str,
    args: &[RuntimeValue],
) -> Result<RuntimeValue, super::VmError> {
    match method {
        // ── Queries ───────────────────────────────────────────────────────────
        "len" => {
            require_args("len", args, 0)?;
            Ok(RuntimeValue::Int(list.len() as i64))
        }

        "get" => {
            require_args("get", args, 1)?;
            let idx = require_int(&args[0], "get", "index")?;
            let pos = resolve_index(idx, list.len())?;
            // SAFETY: `resolve_index` guarantees `pos < list.len()`.
            list.into_iter().nth(pos).ok_or_else(|| {
                super::VmError::TypeError(
                    "list.get(): index out of bounds (resolve_index guarantee violated)"
                        .to_string(),
                )
            })
        }

        "first" => {
            require_args("first", args, 0)?;
            list.into_iter().next().ok_or_else(|| {
                super::VmError::TypeError("list.first() called on empty list".to_string())
            })
        }

        "last" => {
            require_args("last", args, 0)?;
            list.into_iter().last().ok_or_else(|| {
                super::VmError::TypeError("list.last() called on empty list".to_string())
            })
        }

        "contains" => {
            require_args("contains", args, 1)?;
            let found = list.iter().any(|el| el == &args[0]);
            Ok(RuntimeValue::Bool(found))
        }

        // ── Transformers (all pure — return a new list) ───────────────────────
        "append" => {
            require_args("append", args, 1)?;
            let mut out = list;
            out.push(args[0].clone());
            Ok(RuntimeValue::List(out))
        }

        "prepend" => {
            require_args("prepend", args, 1)?;
            let mut out = Vec::with_capacity(list.len() + 1);
            out.push(args[0].clone());
            out.extend(list);
            Ok(RuntimeValue::List(out))
        }

        "pop" => {
            require_args("pop", args, 0)?;
            if list.is_empty() {
                return Err(super::VmError::TypeError(
                    "list.pop() called on empty list".to_string(),
                ));
            }
            let mut out = list;
            out.pop();
            Ok(RuntimeValue::List(out))
        }

        "concat" => {
            require_args("concat", args, 1)?;
            match &args[0] {
                RuntimeValue::List(other) => {
                    let mut out = list;
                    out.extend(other.iter().cloned());
                    Ok(RuntimeValue::List(out))
                }
                other => Err(super::VmError::TypeError(format!(
                    "list.concat() expected a List argument, got {:?}",
                    other
                ))),
            }
        }

        "reversed" => {
            require_args("reversed", args, 0)?;
            let mut out = list;
            out.reverse();
            Ok(RuntimeValue::List(out))
        }

        "with" => {
            require_args("with", args, 2)?;
            let idx = require_int(&args[0], "with", "index")?;
            let pos = resolve_index(idx, list.len())?;
            let mut out = list;
            out[pos] = args[1].clone();
            Ok(RuntimeValue::List(out))
        }

        "slice" => {
            require_args_range("slice", args, 1, 2)?;
            let len = list.len();

            // Resolve a possibly-negative slice bound, clamping to [0, len].
            let resolve_bound = |raw: i64| -> usize {
                let resolved = if raw < 0 {
                    (len as i64 + raw).max(0)
                } else {
                    raw.min(len as i64)
                };
                resolved as usize
            };

            let start = resolve_bound(require_int(&args[0], "slice", "start")?);
            let end = if args.len() == 2 {
                resolve_bound(require_int(&args[1], "slice", "end")?)
            } else {
                len
            };

            // Both bounds are already clamped; guard against inverted range.
            let (start, end) = (start.min(len), end.min(len));
            let range = if start <= end {
                start..end
            } else {
                start..start
            };

            Ok(RuntimeValue::List(list[range].to_vec()))
        }

        // ── String conversion ─────────────────────────────────────────────────
        "join" => {
            require_args_range("join", args, 0, 1)?;
            let sep = if args.is_empty() {
                String::new()
            } else {
                match &args[0] {
                    RuntimeValue::Str(ps) => ps.to_string(),
                    other => {
                        return Err(super::VmError::TypeError(format!(
                            "list.join() separator must be a Str, got {:?}",
                            other
                        )));
                    }
                }
            };

            let joined = list
                .iter()
                .map(format_for_join)
                .collect::<Vec<_>>()
                .join(&sep);

            Ok(RuntimeValue::Str(ParsedString::new_plain(&joined)))
        }

        // ── Stubs ─────────────────────────────────────────────────────────────
        "map" => {
            require_args("map", args, 1)?;
            let (params, body) = match &args[0] {
                RuntimeValue::Function { params, body } => (params.clone(), body.as_ref()),
                other => {
                    return Err(super::VmError::TypeError(format!(
                        "list.map() expects a function argument, got {:?}",
                        other
                    )));
                }
            };
            if params.len() != 1 {
                return Err(super::VmError::TypeError(format!(
                    "list.map() function must take exactly 1 parameter, got {}",
                    params.len()
                )));
            }
            let mut result = Vec::with_capacity(list.len());
            for item in list {
                let mapped = super::eval::exec_fn_body(body, &params, &[item])?;
                result.push(mapped);
            }
            Ok(RuntimeValue::List(result))
        }

        // ── Unknown ───────────────────────────────────────────────────────────
        other => Err(super::VmError::TypeError(format!(
            "unknown list method '{other}'"
        ))),
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

/// Resolve a possibly-negative index into a concrete `usize`, or return
/// [`super::VmError::IndexOutOfBounds`].
///
/// - Non-negative `idx`: used as-is; must be in `[0, len)`.
/// - Negative `idx`: treated as `len + idx`; must still land in `[0, len)`.
fn resolve_index(idx: i64, len: usize) -> Result<usize, super::VmError> {
    let resolved: i64 = if idx < 0 { len as i64 + idx } else { idx };

    if resolved < 0 || resolved >= len as i64 {
        return Err(super::VmError::IndexOutOfBounds { index: idx, len });
    }

    Ok(resolved as usize)
}

/// Assert that exactly `expected` arguments were supplied, returning a
/// descriptive [`super::VmError::TypeError`] if not.
fn require_args(
    method: &str,
    args: &[RuntimeValue],
    expected: usize,
) -> Result<(), super::VmError> {
    if args.len() != expected {
        return Err(super::VmError::TypeError(format!(
            "list.{method}() takes {expected} argument(s), got {}",
            args.len()
        )));
    }
    Ok(())
}

/// Assert that the argument count is within `[min, max]` inclusive, returning
/// a descriptive [`super::VmError::TypeError`] if not.
fn require_args_range(
    method: &str,
    args: &[RuntimeValue],
    min: usize,
    max: usize,
) -> Result<(), super::VmError> {
    if args.len() < min || args.len() > max {
        return Err(super::VmError::TypeError(format!(
            "list.{method}() takes {min}–{max} argument(s), got {}",
            args.len()
        )));
    }
    Ok(())
}

/// Coerce a [`RuntimeValue`] to `i64`, returning a descriptive
/// [`super::VmError::TypeError`] if the value is not a `RuntimeValue::Int`.
fn require_int(val: &RuntimeValue, method: &str, param: &str) -> Result<i64, super::VmError> {
    match val {
        RuntimeValue::Int(i) => Ok(*i),
        other => Err(super::VmError::TypeError(format!(
            "list.{method}() expected an Int for '{param}', got {:?}",
            other
        ))),
    }
}

/// Format a [`RuntimeValue`] as a plain string for use in `join()`.
///
/// This is a simple display conversion — no format-spec support — that mirrors
/// what you would expect from a basic `to_string()` without invoking the full
/// evaluator.  Composite or opaque values (maps, decorators) are rendered as
/// `"<opaque>"`.
fn format_for_join(val: &RuntimeValue) -> String {
    match val {
        RuntimeValue::Null => "null".to_string(),
        RuntimeValue::Bool(b) => b.to_string(),
        RuntimeValue::Int(i) => i.to_string(),
        RuntimeValue::Float(f) => f.to_string(),
        RuntimeValue::Str(ps) => ps.to_string(),
        RuntimeValue::Dice(c, s) => format!("{}d{}", c, s),
        RuntimeValue::Label { name, .. } => name.clone(),
        RuntimeValue::List(items) => format!(
            "[{}]",
            items
                .iter()
                .map(format_for_join)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        _ => "<opaque>".to_string(),
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Convenience constructors ──────────────────────────────────────────────

    fn int(n: i64) -> RuntimeValue {
        RuntimeValue::Int(n)
    }

    fn str_val(s: &str) -> RuntimeValue {
        RuntimeValue::Str(ParsedString::new_plain(s))
    }

    fn list_of(items: impl IntoIterator<Item = RuntimeValue>) -> RuntimeValue {
        RuntimeValue::List(items.into_iter().collect())
    }

    /// Thin wrapper so tests don't have to spell out `&args[..]` every time.
    fn call(
        items: Vec<RuntimeValue>,
        method: &str,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, super::super::VmError> {
        dispatch(items, method, &args)
    }

    // ── len ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_len() {
        assert_eq!(call(vec![], "len", vec![]).unwrap(), int(0));
        assert_eq!(
            call(vec![int(1), int(2), int(3)], "len", vec![]).unwrap(),
            int(3)
        );
    }

    // ── get ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_get_positive() {
        let result = call(vec![int(10), int(20), int(30)], "get", vec![int(1)]).unwrap();
        assert_eq!(result, int(20));
    }

    #[test]
    fn test_get_negative() {
        let result = call(vec![int(10), int(20), int(30)], "get", vec![int(-1)]).unwrap();
        assert_eq!(result, int(30));
    }

    #[test]
    fn test_get_out_of_bounds() {
        let err = call(vec![int(1), int(2)], "get", vec![int(5)]).unwrap_err();
        assert!(
            matches!(
                err,
                super::super::VmError::IndexOutOfBounds { index: 5, len: 2 }
            ),
            "expected IndexOutOfBounds, got: {err:?}"
        );
    }

    // ── first / last ─────────────────────────────────────────────────────────

    #[test]
    fn test_first_last() {
        let items = vec![int(1), int(2), int(3)];

        assert_eq!(call(items.clone(), "first", vec![]).unwrap(), int(1));
        assert_eq!(call(items.clone(), "last", vec![]).unwrap(), int(3));

        // Empty list must error
        assert!(matches!(
            call(vec![], "first", vec![]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
        assert!(matches!(
            call(vec![], "last", vec![]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── contains ─────────────────────────────────────────────────────────────

    #[test]
    fn test_contains() {
        let items = vec![int(1), int(2), int(3)];
        assert_eq!(
            call(items.clone(), "contains", vec![int(2)]).unwrap(),
            RuntimeValue::Bool(true)
        );
        assert_eq!(
            call(items, "contains", vec![int(99)]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    // ── append ───────────────────────────────────────────────────────────────

    #[test]
    fn test_append() {
        let original = vec![int(1), int(2)];
        let result = call(original, "append", vec![int(3)]).unwrap();

        // New list contains the appended element.
        assert_eq!(result, list_of([int(1), int(2), int(3)]));

        // The original two-element value is unchanged (the new list differs).
        assert_ne!(result, list_of([int(1), int(2)]));
    }

    // ── prepend ──────────────────────────────────────────────────────────────

    #[test]
    fn test_prepend() {
        let result = call(vec![int(2), int(3)], "prepend", vec![int(1)]).unwrap();
        assert_eq!(result, list_of([int(1), int(2), int(3)]));
    }

    // ── pop ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_pop() {
        let result = call(vec![int(1), int(2), int(3)], "pop", vec![]).unwrap();
        assert_eq!(result, list_of([int(1), int(2)]));

        assert!(matches!(
            call(vec![], "pop", vec![]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── concat ───────────────────────────────────────────────────────────────

    #[test]
    fn test_concat() {
        let a = vec![int(1), int(2)];
        let b = list_of([int(3), int(4)]);
        let result = call(a, "concat", vec![b]).unwrap();
        assert_eq!(result, list_of([int(1), int(2), int(3), int(4)]));
    }

    // ── reversed ─────────────────────────────────────────────────────────────

    #[test]
    fn test_reversed() {
        let result = call(vec![int(1), int(2), int(3)], "reversed", vec![]).unwrap();
        assert_eq!(result, list_of([int(3), int(2), int(1)]));

        // Empty list stays empty
        assert_eq!(call(vec![], "reversed", vec![]).unwrap(), list_of([]));
    }

    // ── with ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_with() {
        // Positive index
        let result = call(vec![int(1), int(2), int(3)], "with", vec![int(1), int(99)]).unwrap();
        assert_eq!(result, list_of([int(1), int(99), int(3)]));

        // Negative index
        let result = call(vec![int(1), int(2), int(3)], "with", vec![int(-1), int(77)]).unwrap();
        assert_eq!(result, list_of([int(1), int(2), int(77)]));
    }

    // ── slice ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_slice_one_arg() {
        // [1,2,3,4,5].slice(2) → [3,4,5]
        let result = call(
            vec![int(1), int(2), int(3), int(4), int(5)],
            "slice",
            vec![int(2)],
        )
        .unwrap();
        assert_eq!(result, list_of([int(3), int(4), int(5)]));
    }

    #[test]
    fn test_slice_two_args() {
        // [1,2,3,4,5].slice(1,3) → [2,3]
        let result = call(
            vec![int(1), int(2), int(3), int(4), int(5)],
            "slice",
            vec![int(1), int(3)],
        )
        .unwrap();
        assert_eq!(result, list_of([int(2), int(3)]));
    }

    #[test]
    fn test_slice_negative() {
        // [1,2,3].slice(-2) → [2,3]
        let result = call(vec![int(1), int(2), int(3)], "slice", vec![int(-2)]).unwrap();
        assert_eq!(result, list_of([int(2), int(3)]));
    }

    // ── join ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_join_no_sep() {
        // [1, 2, 3].join() → "123"
        let result = call(vec![int(1), int(2), int(3)], "join", vec![]).unwrap();
        assert_eq!(result, str_val("123"));
    }

    #[test]
    fn test_join_with_sep() {
        // [1, 2, 3].join(", ") → "1, 2, 3"
        let result = call(vec![int(1), int(2), int(3)], "join", vec![str_val(", ")]).unwrap();
        assert_eq!(result, str_val("1, 2, 3"));
    }

    // ── map ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_map_doubles_elements() {
        use crate::parser::ast::{Ast, Operator};

        // Build fn(x) { x * 2 } as a RuntimeValue::Function
        let body = Box::new(Ast::block(vec![Ast::binop(
            Operator::Multiply,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::Int(2)),
        )]));
        let func = RuntimeValue::Function {
            params: vec!["x".to_string()],
            body,
        };

        let list = vec![
            RuntimeValue::Int(1),
            RuntimeValue::Int(2),
            RuntimeValue::Int(3),
        ];
        let result = dispatch(list, "map", &[func]).unwrap();
        assert_eq!(
            result,
            RuntimeValue::List(vec![
                RuntimeValue::Int(2),
                RuntimeValue::Int(4),
                RuntimeValue::Int(6),
            ])
        );
    }

    #[test]
    fn test_map_wrong_arg_type() {
        let list = vec![RuntimeValue::Int(1)];
        let err = dispatch(list, "map", &[RuntimeValue::Int(42)]).unwrap_err();
        assert!(matches!(err, super::super::VmError::TypeError(_)));
    }

    #[test]
    fn test_map_wrong_function_arity() {
        use crate::parser::ast::Ast;
        // Function that takes 2 params — should error for map
        let body = Box::new(Ast::block(vec![]));
        let func = RuntimeValue::Function {
            params: vec!["x".to_string(), "y".to_string()],
            body,
        };
        let list = vec![RuntimeValue::Int(1)];
        let err = dispatch(list, "map", &[func]).unwrap_err();
        assert!(matches!(err, super::super::VmError::TypeError(_)));
    }

    // ── unknown method ───────────────────────────────────────────────────────

    #[test]
    fn test_unknown_method() {
        let err = call(vec![], "frobnicate", vec![]).unwrap_err();
        assert!(
            matches!(err, super::super::VmError::TypeError(_)),
            "expected TypeError, got: {err:?}"
        );
        if let super::super::VmError::TypeError(msg) = err {
            assert!(
                msg.contains("frobnicate"),
                "error message should name the unknown method, got: {msg}"
            );
        }
    }
}
