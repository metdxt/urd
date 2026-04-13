//! # Map Methods
//!
//! This module implements all built-in methods callable on `RuntimeValue::Map`
//! via method-call syntax (e.g. `m.keys()`, `m.get("k")`, `m.set("k", v)`).
//!
//! ## Pure-functional semantics
//!
//! All "mutating" methods — `set`, `remove`, `merge` — return a **new**
//! `RuntimeValue::Map` and leave the original map untouched.  To update a
//! variable the caller must reassign:
//!
//! ```urd
//! m = m.set("score", 10)
//! m = m.remove("tmp")
//! ```
//!
//! This is consistent with Urd's immutable-value model: a map variable holds
//! a value; methods produce new values; assignment is how you "update" state.
//!
//! The single public entry point is [`dispatch`], called by the evaluator
//! whenever it encounters a method call whose receiver is a
//! `RuntimeValue::Map`.

use std::collections::HashMap;

use crate::lexer::strings::ParsedString;
use crate::runtime::value::RuntimeValue;

// ─── Public entry point ───────────────────────────────────────────────────────

/// Dispatch a method call on a `RuntimeValue::Map`.
///
/// # Parameters
///
/// - `map` — the owned inner `HashMap<String, Box<RuntimeValue>>` extracted
///   from the receiver
/// - `method` — the method name (e.g. `"get"`, `"keys"`, `"set"`)
/// - `args` — already-evaluated argument values
///
/// # Errors
///
/// Returns a [`super::VmError`] on type mismatches, wrong argument counts, or
/// unknown method names.
pub(super) fn dispatch(
    map_ref: crate::runtime::value::Shared<HashMap<String, Box<RuntimeValue>>>,
    method: &str,
    args: &[RuntimeValue],
) -> Result<RuntimeValue, super::VmError> {
    let map = map_ref.borrow().clone();
    match method {
        // ── Queries ───────────────────────────────────────────────────────────
        "get" => {
            require_args("get", args, 1)?;
            let key = require_str(&args[0], "get", "key")?;
            Ok(map
                .get(&key)
                .map(|v| *v.clone())
                .unwrap_or(RuntimeValue::Null))
        }

        "has" => {
            require_args("has", args, 1)?;
            let key = require_str(&args[0], "has", "key")?;
            Ok(RuntimeValue::Bool(map.contains_key(&key)))
        }

        "keys" => {
            require_args("keys", args, 0)?;
            // Sort keys for deterministic output — HashMap iteration order is
            // undefined, and callers generally expect a stable sequence.
            let mut ks: Vec<&String> = map.keys().collect::<Vec<_>>();
            ks.sort();
            let list = ks
                .into_iter()
                .map(|k| RuntimeValue::Str(ParsedString::new_plain(k)))
                .collect::<Vec<_>>();
            Ok(RuntimeValue::List(crate::runtime::value::shared(list)))
        }

        "values" => {
            require_args("values", args, 0)?;
            // Iterate in sorted key order so the result is deterministic.
            let mut pairs: Vec<(&String, &Box<RuntimeValue>)> = map.iter().collect::<Vec<_>>();
            pairs.sort_by_key(|(k, _)| k.as_str());
            let list = pairs.into_iter().map(|(_, v)| *v.clone()).collect::<Vec<_>>();
            Ok(RuntimeValue::List(crate::runtime::value::shared(list)))
        }

        "len" => {
            require_args("len", args, 0)?;
            Ok(RuntimeValue::Int(map.len() as i64))
        }

        "is_empty" => {
            require_args("is_empty", args, 0)?;
            Ok(RuntimeValue::Bool(map.is_empty()))
        }

        // ── Transformers (pure — all return a new map) ────────────────────────
        "set" => {
            require_args("set", args, 2)?;
            let key = require_str(&args[0], "set", "key")?;
            let mut new_map = map;
            new_map.insert(key, Box::new(args[1].clone()));
            Ok(RuntimeValue::Map(crate::runtime::value::shared(new_map)))
        }

        "remove" => {
            require_args("remove", args, 1)?;
            let key = require_str(&args[0], "remove", "key")?;
            let mut new_map = map;
            new_map.remove(&key);
            Ok(RuntimeValue::Map(crate::runtime::value::shared(new_map)))
        }

        "merge" => {
            require_args("merge", args, 1)?;
            match &args[0] {
                RuntimeValue::Map(other) => {
                    let mut new_map = map;
                    // `other` wins on key conflicts — its entries overwrite ours.
                    for (k, v) in other.borrow().iter() {
                        new_map.insert(k.clone(), v.clone());
                    }
                    Ok(RuntimeValue::Map(crate::runtime::value::shared(new_map)))
                }
                other => Err(super::VmError::TypeError(format!(
                    "map.merge() expected a Map argument, got {:?}",
                    other
                ))),
            }
        }

        // ── Unknown ───────────────────────────────────────────────────────────
        other => Err(super::VmError::UnknownMethod(format!(
            "'{}' on type Map",
            other
        ))),
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

/// Assert that exactly `expected` arguments were supplied, returning a
/// descriptive [`super::VmError::TypeError`] if not.
fn require_args(
    method: &str,
    args: &[RuntimeValue],
    expected: usize,
) -> Result<(), super::VmError> {
    if args.len() != expected {
        return Err(super::VmError::TypeError(format!(
            "map.{method}() takes {expected} argument(s), got {}",
            args.len()
        )));
    }
    Ok(())
}

/// Coerce a [`RuntimeValue`] to a `String`, returning a descriptive
/// [`super::VmError::TypeError`] if the value is not a `RuntimeValue::Str`.
fn require_str(val: &RuntimeValue, method: &str, param: &str) -> Result<String, super::VmError> {
    match val {
        RuntimeValue::Str(ps) => Ok(ps.to_string()),
        other => Err(super::VmError::TypeError(format!(
            "map.{method}() expected a Str for '{param}', got {:?}",
            other
        ))),
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Convenience constructors ──────────────────────────────────────────────

    fn str_val(s: &str) -> RuntimeValue {
        RuntimeValue::Str(ParsedString::new_plain(s))
    }

    fn int(n: i64) -> RuntimeValue {
        RuntimeValue::Int(n)
    }

    /// Build a small `HashMap<String, Box<RuntimeValue>>` from key-value pairs.
    fn make_map(pairs: &[(&str, RuntimeValue)]) -> HashMap<String, Box<RuntimeValue>> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), Box::new(v.clone())))
            .collect()
    }

    fn call(
        map: HashMap<String, Box<RuntimeValue>>,
        method: &str,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, super::super::VmError> {
        dispatch(crate::runtime::value::shared(map), method, args)
    }

    // ── get ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_get_existing_key() {
        let m = make_map(&[("hp", int(100))]);
        assert_eq!(call(m, "get", &[str_val("hp")]).unwrap(), int(100));
    }

    #[test]
    fn test_get_missing_key_returns_null() {
        let m = make_map(&[("hp", int(100))]);
        assert_eq!(
            call(m, "get", &[str_val("mp")]).unwrap(),
            RuntimeValue::Null
        );
    }

    #[test]
    fn test_get_wrong_arg_type() {
        let m = make_map(&[("hp", int(1))]);
        assert!(matches!(
            call(m, "get", &[int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_get_wrong_arg_count() {
        let m = make_map(&[]);
        assert!(matches!(
            call(m, "get", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── has ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_has_present() {
        let m = make_map(&[("x", int(1))]);
        assert_eq!(
            call(m, "has", &[str_val("x")]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_has_absent() {
        let m = make_map(&[("x", int(1))]);
        assert_eq!(
            call(m, "has", &[str_val("y")]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    // ── keys ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_keys_sorted() {
        let m = make_map(&[("z", int(3)), ("a", int(1)), ("m", int(2))]);
        assert_eq!(
            call(m, "keys", &[]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![str_val("a"), str_val("m"), str_val("z")]))
        );
    }

    #[test]
    fn test_keys_empty_map() {
        let m = make_map(&[]);
        assert_eq!(call(m, "keys", &[]).unwrap(), RuntimeValue::List(crate::runtime::value::shared(vec![])));
    }

    // ── values ────────────────────────────────────────────────────────────────

    #[test]
    fn test_values_sorted_by_key() {
        // values() must return values in sorted-by-key order for determinism.
        let m = make_map(&[("b", int(2)), ("a", int(1))]);
        assert_eq!(
            call(m, "values", &[]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![int(1), int(2)]))
        );
    }

    // ── len / is_empty ────────────────────────────────────────────────────────

    #[test]
    fn test_len() {
        let m = make_map(&[("a", int(1)), ("b", int(2))]);
        assert_eq!(call(m, "len", &[]).unwrap(), int(2));
    }

    #[test]
    fn test_len_empty() {
        assert_eq!(call(make_map(&[]), "len", &[]).unwrap(), int(0));
    }

    #[test]
    fn test_is_empty_true() {
        assert_eq!(
            call(make_map(&[]), "is_empty", &[]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_is_empty_false() {
        let m = make_map(&[("k", int(1))]);
        assert_eq!(call(m, "is_empty", &[]).unwrap(), RuntimeValue::Bool(false));
    }

    // ── set ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_set_new_key() {
        let m = make_map(&[("a", int(1))]);
        let result = call(m, "set", &[str_val("b"), int(2)]).unwrap();
        // The returned map must contain both "a" and the new "b".
        match result {
            RuntimeValue::Map(out) => {
                assert_eq!(*out.borrow()["a"], int(1));
                assert_eq!(*out.borrow()["b"], int(2));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_set_overwrites_existing_key() {
        let m = make_map(&[("hp", int(100))]);
        let result = call(m, "set", &[str_val("hp"), int(50)]).unwrap();
        match result {
            RuntimeValue::Map(out) => {
                assert_eq!(*out.borrow()["hp"], int(50));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_set_wrong_arg_count() {
        let m = make_map(&[]);
        assert!(matches!(
            call(m, "set", &[str_val("k")]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── remove ────────────────────────────────────────────────────────────────

    #[test]
    fn test_remove_existing_key() {
        let m = make_map(&[("a", int(1)), ("b", int(2))]);
        let result = call(m, "remove", &[str_val("a")]).unwrap();
        match result {
            RuntimeValue::Map(out) => {
                assert!(!out.borrow().contains_key("a"));
                assert!(out.borrow().contains_key("b"));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_remove_missing_key_is_noop() {
        let m = make_map(&[("a", int(1))]);
        let result = call(m, "remove", &[str_val("z")]).unwrap();
        match result {
            RuntimeValue::Map(out) => {
                assert!(out.borrow().contains_key("a"));
                assert_eq!(out.borrow().len(), 1);
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    // ── merge ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_merge_disjoint_maps() {
        let base = make_map(&[("a", int(1))]);
        let other_map = make_map(&[("b", int(2))]);
        let other_rv = RuntimeValue::Map(crate::runtime::value::shared(other_map));

        let result = call(base, "merge", &[other_rv]).unwrap();
        match result {
            RuntimeValue::Map(out) => {
                assert_eq!(*out.borrow()["a"], int(1));
                assert_eq!(*out.borrow()["b"], int(2));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_merge_other_wins_on_conflict() {
        let base = make_map(&[("hp", int(100)), ("mp", int(50))]);
        let other_map = make_map(&[("hp", int(999))]);
        let other_rv = RuntimeValue::Map(crate::runtime::value::shared(other_map));

        let result = call(base, "merge", &[other_rv]).unwrap();
        match result {
            RuntimeValue::Map(out) => {
                // "hp" was in both; `other` wins.
                assert_eq!(*out.borrow()["hp"], int(999));
                // "mp" was only in base; preserved.
                assert_eq!(*out.borrow()["mp"], int(50));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_merge_wrong_arg_type() {
        let m = make_map(&[]);
        assert!(matches!(
            call(m, "merge", &[int(42)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── unknown method ────────────────────────────────────────────────────────

    #[test]
    fn test_unknown_method() {
        let m = make_map(&[]);
        assert!(matches!(
            call(m, "frobnicate", &[]).unwrap_err(),
            super::super::VmError::UnknownMethod(_)
        ));
    }
}
