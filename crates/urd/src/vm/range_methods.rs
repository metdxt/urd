//! # Range Methods
//!
//! Built-in methods callable on [`crate::runtime::value::RuntimeValue::Range`]
//! via method-call syntax (e.g. `range.len()`, `range.contains(n)`).
//!
//! ## Semantics
//!
//! A range `start..end` (exclusive) contains integers `n` where `start <= n < end`.
//! A range `start..=end` (inclusive) contains integers `n` where `start <= n <= end`.
//!
//! An empty/inverted range (`start >= end` for exclusive, `start > end` for inclusive)
//! has length 0 and contains no values.

use crate::runtime::value::RuntimeValue;

/// Dispatch a method call on a `RuntimeValue::Range`.
///
/// # Parameters
///
/// - `start`     — the lower bound of the range
/// - `end`       — the upper bound of the range
/// - `inclusive` — `true` for `..=`, `false` for `..`
/// - `method`    — the method name (e.g. `"len"`, `"contains"`)
/// - `args`      — already-evaluated argument values
///
/// # Errors
///
/// Returns a [`super::VmError`] on type mismatches, wrong argument counts,
/// or unknown method names.
pub(super) fn dispatch(
    start: i64,
    end: i64,
    inclusive: bool,
    method: &str,
    args: &[RuntimeValue],
) -> Result<RuntimeValue, super::VmError> {
    match method {
        "len" => {
            require_args("len", args, 0)?;
            let len = range_len(start, end, inclusive)?;
            Ok(RuntimeValue::Int(len))
        }

        "contains" => {
            require_args("contains", args, 1)?;
            match &args[0] {
                RuntimeValue::Int(n) => Ok(RuntimeValue::Bool(range_contains(
                    start, end, inclusive, *n,
                ))),
                other => Err(super::VmError::TypeError(format!(
                    "range.contains() requires an Int argument, got {:?}",
                    other
                ))),
            }
        }

        _ => Err(super::VmError::UnknownMethod(format!(
            "method '{method}' is not defined on range"
        ))),
    }
}

/// Computes the number of integers in the range.
///
/// Returns 0 for empty/inverted ranges.
///
/// # Errors
///
/// Returns [`super::VmError::TypeError`] if the range length exceeds `i64::MAX`.
fn range_len(start: i64, end: i64, inclusive: bool) -> Result<i64, super::VmError> {
    let raw = (end as i128) - (start as i128) + if inclusive { 1 } else { 0 };
    if raw < 0 {
        Ok(0)
    } else if raw > i64::MAX as i128 {
        Err(super::VmError::TypeError(
            "range length exceeds i64::MAX".into(),
        ))
    } else {
        Ok(raw as i64)
    }
}

/// Returns `true` if `n` lies within the range.
fn range_contains(start: i64, end: i64, inclusive: bool, n: i64) -> bool {
    if inclusive {
        n >= start && n <= end
    } else {
        n >= start && n < end
    }
}

fn require_args(
    method: &str,
    args: &[RuntimeValue],
    expected: usize,
) -> Result<(), super::VmError> {
    if args.len() != expected {
        Err(super::VmError::TypeError(format!(
            "range.{method}() takes {expected} argument(s), got {}",
            args.len()
        )))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn call(
        start: i64,
        end: i64,
        inclusive: bool,
        method: &str,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, super::super::VmError> {
        dispatch(start, end, inclusive, method, &args)
    }

    // ── len ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_len_exclusive_basic() {
        // 0..5 has 5 elements: 0, 1, 2, 3, 4
        assert_eq!(
            call(0, 5, false, "len", vec![]).unwrap(),
            RuntimeValue::Int(5)
        );
    }

    #[test]
    fn test_len_inclusive_basic() {
        // 0..=5 has 6 elements: 0, 1, 2, 3, 4, 5
        assert_eq!(
            call(0, 5, true, "len", vec![]).unwrap(),
            RuntimeValue::Int(6)
        );
    }

    #[test]
    fn test_len_exclusive_empty_same_bounds() {
        // 5..5 is empty
        assert_eq!(
            call(5, 5, false, "len", vec![]).unwrap(),
            RuntimeValue::Int(0)
        );
    }

    #[test]
    fn test_len_inclusive_single() {
        // 5..=5 has exactly one element
        assert_eq!(
            call(5, 5, true, "len", vec![]).unwrap(),
            RuntimeValue::Int(1)
        );
    }

    #[test]
    fn test_len_inverted_exclusive() {
        // 10..5 is empty — len clamps to 0
        assert_eq!(
            call(10, 5, false, "len", vec![]).unwrap(),
            RuntimeValue::Int(0)
        );
    }

    #[test]
    fn test_len_inverted_inclusive() {
        // 10..=5 is empty — len clamps to 0
        assert_eq!(
            call(10, 5, true, "len", vec![]).unwrap(),
            RuntimeValue::Int(0)
        );
    }

    #[test]
    fn test_len_too_many_args() {
        assert!(call(0, 5, false, "len", vec![RuntimeValue::Int(1)]).is_err());
    }

    // ── contains ─────────────────────────────────────────────────────────────

    #[test]
    fn test_contains_exclusive_inside() {
        assert_eq!(
            call(0, 5, false, "contains", vec![RuntimeValue::Int(3)]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_contains_exclusive_at_start() {
        // 0 is in 0..5
        assert_eq!(
            call(0, 5, false, "contains", vec![RuntimeValue::Int(0)]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_contains_exclusive_at_end_is_false() {
        // 5 is NOT in 0..5 (exclusive upper bound)
        assert_eq!(
            call(0, 5, false, "contains", vec![RuntimeValue::Int(5)]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    #[test]
    fn test_contains_inclusive_at_end_is_true() {
        // 5 IS in 0..=5 (inclusive upper bound)
        assert_eq!(
            call(0, 5, true, "contains", vec![RuntimeValue::Int(5)]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_contains_below_start_is_false() {
        assert_eq!(
            call(0, 5, false, "contains", vec![RuntimeValue::Int(-1)]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    #[test]
    fn test_contains_wrong_type_errors() {
        assert!(call(0, 5, false, "contains", vec![RuntimeValue::Bool(true)]).is_err());
    }

    #[test]
    fn test_contains_no_args_errors() {
        assert!(call(0, 5, false, "contains", vec![]).is_err());
    }

    #[test]
    fn test_unknown_method_errors() {
        assert!(call(0, 5, false, "foo", vec![]).is_err());
    }
}
