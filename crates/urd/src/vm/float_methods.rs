//! # Float Methods
//!
//! This module implements all built-in methods callable on `RuntimeValue::Float`
//! via method-call syntax (e.g. `f.abs()`, `f.sqrt()`, `f.clamp(0.0, 1.0)`).
//!
//! ## Pure-functional semantics
//!
//! All methods are pure — they return new values and never mutate the receiver.
//! The single public entry point is [`dispatch`], which is called by the
//! evaluator whenever it encounters a method call whose receiver is a
//! `RuntimeValue::Float`.

use crate::lexer::strings::ParsedString;
use crate::runtime::value::RuntimeValue;

// ─── Public entry point ───────────────────────────────────────────────────────

/// Dispatch a method call on a `RuntimeValue::Float`.
///
/// # Parameters
///
/// - `n`      — the `f64` extracted from the receiver
/// - `method` — the method name (e.g. `"abs"`, `"sqrt"`)
/// - `args`   — already-evaluated argument values
///
/// # Errors
///
/// Returns a [`super::VmError`] on type mismatches, wrong argument counts,
/// invalid operand values (e.g. NaN bounds for `clamp`), or unknown method
/// names.
pub(super) fn dispatch(
    n: f64,
    method: &str,
    args: &[RuntimeValue],
) -> Result<RuntimeValue, super::VmError> {
    match method {
        // ── Conversions ───────────────────────────────────────────────────────
        // Returns the `Float` formatted as a `Str`.
        "to_string" => {
            require_args("to_string", args, 0)?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(&n.to_string())))
        }

        // Truncates toward zero and returns an `Int`.
        "to_int" => {
            require_args("to_int", args, 0)?;
            require_finite(n, "to_int")?;
            Ok(RuntimeValue::Int(checked_f64_to_i64(n, "to_int")?))
        }

        // ── Arithmetic ────────────────────────────────────────────────────────
        // Returns the absolute value as a `Float`.
        "abs" => {
            require_args("abs", args, 0)?;
            Ok(RuntimeValue::Float(n.abs()))
        }

        // Rounds down to the nearest integer, returned as `Int`.
        "floor" => {
            require_args("floor", args, 0)?;
            require_finite(n, "floor")?;
            Ok(RuntimeValue::Int(checked_f64_to_i64(n.floor(), "floor")?))
        }

        // Rounds up to the nearest integer, returned as `Int`.
        "ceil" => {
            require_args("ceil", args, 0)?;
            require_finite(n, "ceil")?;
            Ok(RuntimeValue::Int(checked_f64_to_i64(n.ceil(), "ceil")?))
        }

        // Rounds to the nearest integer (half-away-from-zero), returned as `Int`.
        "round" => {
            require_args("round", args, 0)?;
            require_finite(n, "round")?;
            Ok(RuntimeValue::Int(checked_f64_to_i64(n.round(), "round")?))
        }

        // Returns the square root as a `Float`.
        "sqrt" => {
            require_args("sqrt", args, 0)?;
            if n < 0.0 {
                return Err(super::VmError::TypeError(
                    "float.sqrt(): cannot take square root of a negative number".into(),
                ));
            }
            Ok(RuntimeValue::Float(n.sqrt()))
        }

        // Returns the smaller of `self` and `other`.
        "min" => {
            require_args("min", args, 1)?;
            let other = require_float(&args[0], "min", "other")?;
            Ok(RuntimeValue::Float(n.min(other)))
        }

        // Returns the larger of `self` and `other`.
        "max" => {
            require_args("max", args, 1)?;
            let other = require_float(&args[0], "max", "other")?;
            Ok(RuntimeValue::Float(n.max(other)))
        }

        // Clamps `self` into `[min, max]`.
        // Returns `TypeError` if `min > max` or if either bound is NaN.
        "clamp" => {
            require_args("clamp", args, 2)?;
            let lo = require_float(&args[0], "clamp", "min")?;
            let hi = require_float(&args[1], "clamp", "max")?;
            if lo.is_nan() || hi.is_nan() {
                return Err(super::VmError::TypeError(
                    "float.clamp(): bounds must not be NaN".to_string(),
                ));
            }
            if lo > hi {
                return Err(super::VmError::TypeError(format!(
                    "float.clamp(): min ({lo}) must not exceed max ({hi})"
                )));
            }
            Ok(RuntimeValue::Float(n.clamp(lo, hi)))
        }

        // Raises `self` to the power `exp`, returning a `Float`.
        "pow" => {
            require_args("pow", args, 1)?;
            let exp = require_float(&args[0], "pow", "exp")?;
            Ok(RuntimeValue::Float(n.powf(exp)))
        }

        // ── Predicates ────────────────────────────────────────────────────────
        // Returns `true` if `self` is NaN.
        "is_nan" => {
            require_args("is_nan", args, 0)?;
            Ok(RuntimeValue::Bool(n.is_nan()))
        }

        // Returns `true` if `self` is finite (not NaN and not infinite).
        "is_finite" => {
            require_args("is_finite", args, 0)?;
            Ok(RuntimeValue::Bool(n.is_finite()))
        }

        // ── Sign ──────────────────────────────────────────────────────────────
        // Returns `-1.0` or `1.0` according to the IEEE 754 sign of `self`.
        // Note: +0.0 → 1.0, -0.0 → -1.0 (IEEE 754 signed-zero semantics).
        // Returns NaN unchanged (propagates NaN signum semantics from Rust).
        "signum" => {
            require_args("signum", args, 0)?;
            Ok(RuntimeValue::Float(n.signum()))
        }

        // ── Unknown ───────────────────────────────────────────────────────────
        other => Err(super::VmError::UnknownMethod(format!(
            "'{}' on type Float",
            other
        ))),
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

/// Checked conversion from `f64` to `i64`.
///
/// `i64::MIN as f64` is exact (−2^63) and `i64::MAX as f64` rounds up to 2^63.
/// Any finite `f64` in the closed interval `[i64::MIN as f64, i64::MAX as f64]`
/// truncates to a value representable as `i64`, so we reject values outside
/// that interval.
fn checked_f64_to_i64(n: f64, method: &str) -> Result<i64, super::VmError> {
    if n < (i64::MIN as f64) || n > (i64::MAX as f64) {
        return Err(super::VmError::TypeError(format!(
            "float.{method}(): float value {n} is out of integer range"
        )));
    }
    Ok(n as i64)
}

/// Guard that rejects NaN and ±Infinity before a float-to-integer cast.
fn require_finite(n: f64, method: &str) -> Result<(), super::VmError> {
    if !n.is_finite() {
        return Err(super::VmError::TypeError(format!(
            "float.{method}(): cannot convert {} to integer",
            if n.is_nan() { "NaN" } else { "Infinity" }
        )));
    }
    Ok(())
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
            "float.{method}() takes {expected} argument(s), got {}",
            args.len()
        )));
    }
    Ok(())
}

/// Coerce a [`RuntimeValue`] to `f64`.
///
/// Accepts both `Float` and `Int` (auto-widening), returning a descriptive
/// [`super::VmError::TypeError`] for any other variant.
fn require_float(val: &RuntimeValue, method: &str, param: &str) -> Result<f64, super::VmError> {
    match val {
        RuntimeValue::Float(f) => Ok(*f),
        RuntimeValue::Int(i) => Ok(*i as f64),
        other => Err(super::VmError::TypeError(format!(
            "float.{method}() expected a Float for '{param}', got {:?}",
            other
        ))),
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Convenience helpers ───────────────────────────────────────────────────

    fn float(f: f64) -> RuntimeValue {
        RuntimeValue::Float(f)
    }

    fn int(i: i64) -> RuntimeValue {
        RuntimeValue::Int(i)
    }

    fn call(
        n: f64,
        method: &str,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, super::super::VmError> {
        dispatch(n, method, args)
    }

    // ── to_string ─────────────────────────────────────────────────────────────

    #[test]
    fn test_to_string() {
        assert_eq!(
            call(1.23, "to_string", &[]).unwrap(),
            RuntimeValue::Str(ParsedString::new_plain("1.23"))
        );
    }

    #[test]
    fn test_to_string_integer_float() {
        assert_eq!(
            call(2.0, "to_string", &[]).unwrap(),
            RuntimeValue::Str(ParsedString::new_plain("2"))
        );
    }

    // ── to_int ────────────────────────────────────────────────────────────────

    #[test]
    fn test_to_int_truncates_toward_zero() {
        assert_eq!(call(3.9, "to_int", &[]).unwrap(), int(3));
        assert_eq!(call(-3.9, "to_int", &[]).unwrap(), int(-3));
        assert_eq!(call(0.0, "to_int", &[]).unwrap(), int(0));
    }

    // ── abs ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_abs_positive() {
        assert_eq!(call(4.5, "abs", &[]).unwrap(), float(4.5));
    }

    #[test]
    fn test_abs_negative() {
        assert_eq!(call(-4.5, "abs", &[]).unwrap(), float(4.5));
    }

    #[test]
    fn test_abs_zero() {
        assert_eq!(call(0.0, "abs", &[]).unwrap(), float(0.0));
    }

    // ── floor ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_floor() {
        assert_eq!(call(2.9, "floor", &[]).unwrap(), int(2));
        assert_eq!(call(-2.1, "floor", &[]).unwrap(), int(-3));
        assert_eq!(call(3.0, "floor", &[]).unwrap(), int(3));
    }

    // ── ceil ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_ceil() {
        assert_eq!(call(2.1, "ceil", &[]).unwrap(), int(3));
        assert_eq!(call(-2.9, "ceil", &[]).unwrap(), int(-2));
        assert_eq!(call(3.0, "ceil", &[]).unwrap(), int(3));
    }

    // ── round ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_round() {
        assert_eq!(call(2.4, "round", &[]).unwrap(), int(2));
        assert_eq!(call(2.5, "round", &[]).unwrap(), int(3));
        assert_eq!(call(-2.5, "round", &[]).unwrap(), int(-3));
    }

    // ── sqrt ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_sqrt() {
        assert_eq!(call(4.0, "sqrt", &[]).unwrap(), float(2.0));
        assert_eq!(call(9.0, "sqrt", &[]).unwrap(), float(3.0));
    }

    #[test]
    fn test_sqrt_negative_errors() {
        assert!(matches!(
            call(-1.0, "sqrt", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── min / max ─────────────────────────────────────────────────────────────

    #[test]
    fn test_min() {
        assert_eq!(call(3.0, "min", &[float(5.0)]).unwrap(), float(3.0));
        assert_eq!(call(5.0, "min", &[float(3.0)]).unwrap(), float(3.0));
    }

    #[test]
    fn test_max() {
        assert_eq!(call(3.0, "max", &[float(5.0)]).unwrap(), float(5.0));
        assert_eq!(call(5.0, "max", &[float(3.0)]).unwrap(), float(5.0));
    }

    #[test]
    fn test_min_accepts_int_arg() {
        // Int is auto-widened to Float.
        assert_eq!(call(2.5, "min", &[int(5)]).unwrap(), float(2.5));
    }

    #[test]
    fn test_max_accepts_int_arg() {
        assert_eq!(call(2.5, "max", &[int(5)]).unwrap(), float(5.0));
    }

    // ── clamp ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_clamp_in_range() {
        assert_eq!(
            call(0.5, "clamp", &[float(0.0), float(1.0)]).unwrap(),
            float(0.5)
        );
    }

    #[test]
    fn test_clamp_below_min() {
        assert_eq!(
            call(-1.0, "clamp", &[float(0.0), float(1.0)]).unwrap(),
            float(0.0)
        );
    }

    #[test]
    fn test_clamp_above_max() {
        assert_eq!(
            call(2.0, "clamp", &[float(0.0), float(1.0)]).unwrap(),
            float(1.0)
        );
    }

    #[test]
    fn test_clamp_inverted_bounds_errors() {
        assert!(matches!(
            call(0.5, "clamp", &[float(1.0), float(0.0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_clamp_nan_bound_errors() {
        assert!(matches!(
            call(0.5, "clamp", &[float(f64::NAN), float(1.0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── pow ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_pow() {
        assert_eq!(call(2.0, "pow", &[float(10.0)]).unwrap(), float(1024.0));
        assert_eq!(call(9.0, "pow", &[float(0.5)]).unwrap(), float(3.0));
    }

    #[test]
    fn test_pow_accepts_int_exp() {
        assert_eq!(call(3.0, "pow", &[int(3)]).unwrap(), float(27.0));
    }

    // ── is_nan ────────────────────────────────────────────────────────────────

    #[test]
    fn test_is_nan_true() {
        assert_eq!(
            call(f64::NAN, "is_nan", &[]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_is_nan_false() {
        assert_eq!(call(1.0, "is_nan", &[]).unwrap(), RuntimeValue::Bool(false));
    }

    // ── is_finite ─────────────────────────────────────────────────────────────

    #[test]
    fn test_is_finite_true() {
        assert_eq!(
            call(1.23, "is_finite", &[]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_is_finite_false_for_infinity() {
        assert_eq!(
            call(f64::INFINITY, "is_finite", &[]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    #[test]
    fn test_is_finite_false_for_nan() {
        assert_eq!(
            call(f64::NAN, "is_finite", &[]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    // ── signum ────────────────────────────────────────────────────────────────

    #[test]
    fn test_signum_positive() {
        assert_eq!(call(42.0, "signum", &[]).unwrap(), float(1.0));
    }

    #[test]
    fn test_signum_negative() {
        assert_eq!(call(-42.0, "signum", &[]).unwrap(), float(-1.0));
    }

    #[test]
    fn test_signum_zero() {
        // IEEE 754: +0.0 has positive sign → signum returns 1.0, not 0.0.
        assert_eq!(call(0.0, "signum", &[]).unwrap(), float(1.0));
        // -0.0 has negative sign → signum returns -1.0.
        assert_eq!(call(-0.0_f64, "signum", &[]).unwrap(), float(-1.0));
    }

    // ── Error cases ───────────────────────────────────────────────────────────

    #[test]
    fn test_unknown_method() {
        assert!(matches!(
            call(1.0, "frobnicate", &[]).unwrap_err(),
            super::super::VmError::UnknownMethod(_)
        ));
    }

    #[test]
    fn test_wrong_arg_count() {
        assert!(matches!(
            call(1.0, "abs", &[float(2.0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_wrong_arg_type() {
        assert!(matches!(
            call(1.0, "min", &[RuntimeValue::Bool(true)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_min_no_args_errors() {
        assert!(matches!(
            call(1.0, "min", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_clamp_too_few_args_errors() {
        assert!(matches!(
            call(1.0, "clamp", &[float(0.0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── NaN / ±Infinity rejection ─────────────────────────────────────────────

    #[test]
    fn test_to_int_nan_errors() {
        assert!(matches!(
            call(f64::NAN, "to_int", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_to_int_infinity_errors() {
        assert!(matches!(
            call(f64::INFINITY, "to_int", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
        assert!(matches!(
            call(f64::NEG_INFINITY, "to_int", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_floor_nan_errors() {
        assert!(matches!(
            call(f64::NAN, "floor", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_floor_infinity_errors() {
        assert!(matches!(
            call(f64::INFINITY, "floor", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
        assert!(matches!(
            call(f64::NEG_INFINITY, "floor", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_ceil_nan_errors() {
        assert!(matches!(
            call(f64::NAN, "ceil", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_ceil_infinity_errors() {
        assert!(matches!(
            call(f64::INFINITY, "ceil", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
        assert!(matches!(
            call(f64::NEG_INFINITY, "ceil", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_round_nan_errors() {
        assert!(matches!(
            call(f64::NAN, "round", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_round_infinity_errors() {
        assert!(matches!(
            call(f64::INFINITY, "round", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
        assert!(matches!(
            call(f64::NEG_INFINITY, "round", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── Out-of-i64-range tests ────────────────────────────────────────────

    #[test]
    fn test_to_int_large_positive_errors() {
        assert!(matches!(
            call(1e100, "to_int", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_to_int_large_negative_errors() {
        assert!(matches!(
            call(-1e100, "to_int", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_floor_large_positive_errors() {
        assert!(matches!(
            call(1e100, "floor", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_ceil_large_negative_errors() {
        assert!(matches!(
            call(-1e100, "ceil", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_round_large_positive_errors() {
        assert!(matches!(
            call(1e100, "round", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_to_int_normal_values_still_work() {
        assert_eq!(call(42.7, "to_int", &[]).unwrap(), int(42));
        assert_eq!(call(-42.7, "to_int", &[]).unwrap(), int(-42));
        assert_eq!(call(0.0, "to_int", &[]).unwrap(), int(0));
    }

    #[test]
    fn test_floor_normal_values_still_work() {
        assert_eq!(call(42.7, "floor", &[]).unwrap(), int(42));
        assert_eq!(call(-42.7, "floor", &[]).unwrap(), int(-43));
    }

    #[test]
    fn test_ceil_normal_values_still_work() {
        assert_eq!(call(42.3, "ceil", &[]).unwrap(), int(43));
        assert_eq!(call(-42.3, "ceil", &[]).unwrap(), int(-42));
    }

    #[test]
    fn test_round_normal_values_still_work() {
        assert_eq!(call(42.5, "round", &[]).unwrap(), int(43));
        assert_eq!(call(-42.5, "round", &[]).unwrap(), int(-43));
    }
}
