//! # Int Methods
//!
//! This module implements all built-in methods callable on [`RuntimeValue::Int`]
//! via method-call syntax (e.g. `n.abs()`, `n.pow(3)`, `n.clamp(0, 100)`).
//!
//! ## Pure-functional semantics
//!
//! All methods are pure — they compute and return new values without mutating
//! the receiver.  To update a variable the caller must reassign:
//!
//! ```urd
//! x = x.abs()
//! x = x.clamp(0, 10)
//! ```
//!
//! The single public entry point is [`dispatch`], called by the evaluator
//! whenever it encounters a method call whose receiver is a
//! `RuntimeValue::Int`.

use crate::lexer::strings::ParsedString;
use crate::runtime::value::RuntimeValue;

// ─── Public entry point ───────────────────────────────────────────────────────

/// Dispatch a method call on a `RuntimeValue::Int`.
///
/// # Parameters
///
/// - `n`      — the `i64` value extracted from the receiver
/// - `method` — the method name (e.g. `"abs"`, `"pow"`)
/// - `args`   — already-evaluated argument values
///
/// # Errors
///
/// Returns a [`super::VmError`] on type mismatches, wrong argument counts,
/// out-of-range exponents, or unknown method names.
pub(super) fn dispatch(
    n: i64,
    method: &str,
    args: &[RuntimeValue],
) -> Result<RuntimeValue, super::VmError> {
    match method {
        // ── Conversions ───────────────────────────────────────────────────────
        "to_string" => {
            require_args("to_string", args, 0)?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(&n.to_string())))
        }

        "to_float" => {
            require_args("to_float", args, 0)?;
            Ok(RuntimeValue::Float(n as f64))
        }

        // ── Arithmetic ────────────────────────────────────────────────────────
        "abs" => {
            require_args("abs", args, 0)?;
            n.checked_abs()
                .map(RuntimeValue::Int)
                .ok_or_else(|| super::VmError::TypeError("int.abs(): overflow on i64::MIN".into()))
        }

        "min" => {
            require_args("min", args, 1)?;
            let other = require_int(&args[0], "min", "other")?;
            Ok(RuntimeValue::Int(n.min(other)))
        }

        "max" => {
            require_args("max", args, 1)?;
            let other = require_int(&args[0], "max", "other")?;
            Ok(RuntimeValue::Int(n.max(other)))
        }

        "clamp" => {
            require_args("clamp", args, 2)?;
            let lo = require_int(&args[0], "clamp", "min")?;
            let hi = require_int(&args[1], "clamp", "max")?;
            if lo > hi {
                return Err(super::VmError::TypeError(format!(
                    "int.clamp(): min ({lo}) must not exceed max ({hi})"
                )));
            }
            Ok(RuntimeValue::Int(n.clamp(lo, hi)))
        }

        "pow" => {
            require_args("pow", args, 1)?;
            let exp = require_int(&args[0], "pow", "exp")?;
            if exp < 0 {
                return Err(super::VmError::TypeError(format!(
                    "int.pow(): exponent must be non-negative, got {exp}"
                )));
            }
            // exp fits in u32: i64::MAX < 2^63, so any useful exponent is < 2^32.
            if exp > u32::MAX as i64 {
                return Err(super::VmError::TypeError(format!(
                    "int.pow(): exponent {exp} exceeds maximum allowed value ({})",
                    u32::MAX
                )));
            }
            let exp_u32 = exp as u32;
            let mut result: i64 = 1;
            for _ in 0..exp_u32 {
                result = result.checked_mul(n).ok_or_else(|| {
                    super::VmError::TypeError(format!(
                        "int.pow(): result overflows i64 (base={n}, exp={exp})"
                    ))
                })?;
            }
            Ok(RuntimeValue::Int(result))
        }

        "signum" => {
            require_args("signum", args, 0)?;
            Ok(RuntimeValue::Int(n.signum()))
        }

        // ── Unknown ───────────────────────────────────────────────────────────
        other => Err(super::VmError::UnknownMethod(format!(
            "'{other}' on type Int"
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
            "int.{method}() takes {expected} argument(s), got {}",
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
            "int.{method}() expected an Int for '{param}', got {other:?}"
        ))),
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

    fn float(f: f64) -> RuntimeValue {
        RuntimeValue::Float(f)
    }

    fn str_val(s: &str) -> RuntimeValue {
        RuntimeValue::Str(ParsedString::new_plain(s))
    }

    fn call(
        n: i64,
        method: &str,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, super::super::VmError> {
        dispatch(n, method, args)
    }

    // ── to_string ─────────────────────────────────────────────────────────────

    #[test]
    fn test_to_string_positive() {
        assert_eq!(call(42, "to_string", &[]).unwrap(), str_val("42"));
    }

    #[test]
    fn test_to_string_negative() {
        assert_eq!(call(-7, "to_string", &[]).unwrap(), str_val("-7"));
    }

    #[test]
    fn test_to_string_zero() {
        assert_eq!(call(0, "to_string", &[]).unwrap(), str_val("0"));
    }

    #[test]
    fn test_to_string_wrong_arg_count() {
        assert!(matches!(
            call(1, "to_string", &[int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── to_float ──────────────────────────────────────────────────────────────

    #[test]
    fn test_to_float() {
        assert_eq!(call(5, "to_float", &[]).unwrap(), float(5.0));
    }

    #[test]
    fn test_to_float_negative() {
        assert_eq!(call(-3, "to_float", &[]).unwrap(), float(-3.0));
    }

    #[test]
    fn test_to_float_wrong_arg_count() {
        assert!(matches!(
            call(1, "to_float", &[int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── abs ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_abs_positive() {
        assert_eq!(call(5, "abs", &[]).unwrap(), int(5));
    }

    #[test]
    fn test_abs_negative() {
        assert_eq!(call(-5, "abs", &[]).unwrap(), int(5));
    }

    #[test]
    fn test_abs_zero() {
        assert_eq!(call(0, "abs", &[]).unwrap(), int(0));
    }

    #[test]
    fn test_abs_wrong_arg_count() {
        assert!(matches!(
            call(1, "abs", &[int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_abs_min_overflows() {
        // i64::MIN has no positive counterpart; must return a TypeError, not panic.
        assert!(matches!(
            call(i64::MIN, "abs", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── min / max ─────────────────────────────────────────────────────────────

    #[test]
    fn test_min_picks_smaller() {
        assert_eq!(call(3, "min", &[int(7)]).unwrap(), int(3));
        assert_eq!(call(7, "min", &[int(3)]).unwrap(), int(3));
    }

    #[test]
    fn test_min_equal() {
        assert_eq!(call(5, "min", &[int(5)]).unwrap(), int(5));
    }

    #[test]
    fn test_max_picks_larger() {
        assert_eq!(call(3, "max", &[int(7)]).unwrap(), int(7));
        assert_eq!(call(7, "max", &[int(3)]).unwrap(), int(7));
    }

    #[test]
    fn test_max_equal() {
        assert_eq!(call(5, "max", &[int(5)]).unwrap(), int(5));
    }

    #[test]
    fn test_min_wrong_arg_type() {
        assert!(matches!(
            call(1, "min", &[float(1.0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_max_wrong_arg_type() {
        assert!(matches!(
            call(1, "max", &[str_val("hi")]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_min_no_args() {
        assert!(matches!(
            call(1, "min", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── clamp ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_clamp_within_range() {
        assert_eq!(call(5, "clamp", &[int(0), int(10)]).unwrap(), int(5));
    }

    #[test]
    fn test_clamp_below_min() {
        assert_eq!(call(-5, "clamp", &[int(0), int(10)]).unwrap(), int(0));
    }

    #[test]
    fn test_clamp_above_max() {
        assert_eq!(call(15, "clamp", &[int(0), int(10)]).unwrap(), int(10));
    }

    #[test]
    fn test_clamp_at_boundaries() {
        assert_eq!(call(0, "clamp", &[int(0), int(10)]).unwrap(), int(0));
        assert_eq!(call(10, "clamp", &[int(0), int(10)]).unwrap(), int(10));
    }

    #[test]
    fn test_clamp_equal_bounds() {
        assert_eq!(call(5, "clamp", &[int(5), int(5)]).unwrap(), int(5));
    }

    #[test]
    fn test_clamp_inverted_bounds_errors() {
        assert!(matches!(
            call(5, "clamp", &[int(10), int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_clamp_wrong_arg_count() {
        assert!(matches!(
            call(5, "clamp", &[int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_clamp_wrong_arg_type() {
        assert!(matches!(
            call(5, "clamp", &[float(0.0), int(10)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── pow ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_pow_basic() {
        assert_eq!(call(2, "pow", &[int(10)]).unwrap(), int(1024));
    }

    #[test]
    fn test_pow_zero_exponent() {
        assert_eq!(call(99, "pow", &[int(0)]).unwrap(), int(1));
    }

    #[test]
    fn test_pow_one_exponent() {
        assert_eq!(call(42, "pow", &[int(1)]).unwrap(), int(42));
    }

    #[test]
    fn test_pow_zero_base() {
        assert_eq!(call(0, "pow", &[int(5)]).unwrap(), int(0));
    }

    #[test]
    fn test_pow_negative_exponent_errors() {
        assert!(matches!(
            call(2, "pow", &[int(-1)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_pow_wrong_arg_type() {
        assert!(matches!(
            call(2, "pow", &[float(2.0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_pow_no_args() {
        assert!(matches!(
            call(2, "pow", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_pow_overflow_errors() {
        // 2^63 overflows i64; must return a TypeError, not panic or wrap.
        assert!(matches!(
            call(2, "pow", &[int(63)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_pow_large_base_overflow_errors() {
        // i64::MAX ^ 2 obviously overflows.
        assert!(matches!(
            call(i64::MAX, "pow", &[int(2)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_pow_negative_base_overflow_errors() {
        // (-2)^63 = i64::MIN, which is exactly representable.
        // (-2)^64 = 2^64, which overflows i64 — the loop must catch it.
        assert!(matches!(
            call(-2, "pow", &[int(64)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── signum ────────────────────────────────────────────────────────────────

    #[test]
    fn test_signum_positive() {
        assert_eq!(call(42, "signum", &[]).unwrap(), int(1));
    }

    #[test]
    fn test_signum_negative() {
        assert_eq!(call(-7, "signum", &[]).unwrap(), int(-1));
    }

    #[test]
    fn test_signum_zero() {
        assert_eq!(call(0, "signum", &[]).unwrap(), int(0));
    }

    #[test]
    fn test_signum_wrong_arg_count() {
        assert!(matches!(
            call(1, "signum", &[int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── unknown method ────────────────────────────────────────────────────────

    #[test]
    fn test_unknown_method() {
        assert!(matches!(
            call(1, "frobnicate", &[]).unwrap_err(),
            super::super::VmError::UnknownMethod(_)
        ));
    }

    #[test]
    fn test_unknown_method_message_contains_name() {
        let err = call(1, "explode", &[]).unwrap_err();
        match err {
            super::super::VmError::UnknownMethod(msg) => {
                assert!(
                    msg.contains("explode"),
                    "message should mention method name, got: {msg}"
                );
                assert!(
                    msg.contains("Int"),
                    "message should mention type, got: {msg}"
                );
            }
            other => panic!("expected UnknownMethod, got {other:?}"),
        }
    }
}
