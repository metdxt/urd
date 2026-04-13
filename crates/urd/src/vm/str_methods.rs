//! # String Methods
//!
//! This module implements all built-in methods callable on [`crate::runtime::value::RuntimeValue::Str`]
//! via method-call syntax (e.g. `s.len()`, `s.to_upper()`, `s.contains("x")`).
//!
//! ## Pure-functional semantics
//!
//! All methods are pure — they return new values and never mutate the receiver.
//! String operations are always **character-safe**: `slice`, `len`, and `chars`
//! all operate on Unicode scalar values, not raw bytes.
//!
//! The single public entry point is [`dispatch`], called by the evaluator
//! whenever it encounters a method call whose receiver is a
//! [`crate::runtime::value::RuntimeValue::Str`].

use crate::lexer::strings::ParsedString;
use crate::runtime::value::RuntimeValue;

// ─── Public entry point ───────────────────────────────────────────────────────

/// Dispatch a method call on a `RuntimeValue::Str`.
///
/// # Parameters
///
/// - `s`      — the owned [`ParsedString`] extracted from the receiver
/// - `method` — the method name (e.g. `"len"`, `"to_upper"`)
/// - `args`   — already-evaluated argument values
///
/// # Errors
///
/// Returns a [`super::VmError`] on type mismatches, wrong argument counts,
/// parse failures, or unknown method names.  Specifically:
///
/// - [`super::VmError::TypeError`] — wrong number of arguments, wrong argument
///   type, negative repeat count, or failed parse in `to_int`/`to_float`.
/// - [`super::VmError::UnknownMethod`] — unrecognised method name.
pub(super) fn dispatch(
    s: ParsedString,
    method: &str,
    args: &[RuntimeValue],
) -> Result<RuntimeValue, super::VmError> {
    // Work with the plain string representation for all operations.
    // By the time a value reaches method dispatch the evaluator has already
    // resolved any interpolation placeholders, so `to_string()` gives us the
    // fully-materialised content.
    let raw: String = s.to_string();

    match method {
        // ── Queries ───────────────────────────────────────────────────────────
        "len" => {
            require_args("len", args, 0)?;
            // Character count, not byte count.
            Ok(RuntimeValue::Int(raw.chars().count() as i64))
        }

        "is_empty" => {
            require_args("is_empty", args, 0)?;
            Ok(RuntimeValue::Bool(raw.is_empty()))
        }

        // ── Case conversion ───────────────────────────────────────────────────
        "to_upper" => {
            require_args("to_upper", args, 0)?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(
                &raw.to_uppercase(),
            )))
        }

        "to_lower" => {
            require_args("to_lower", args, 0)?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(
                &raw.to_lowercase(),
            )))
        }

        // ── Whitespace trimming ───────────────────────────────────────────────
        "trim" => {
            require_args("trim", args, 0)?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(raw.trim())))
        }

        "trim_start" => {
            require_args("trim_start", args, 0)?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(raw.trim_start())))
        }

        "trim_end" => {
            require_args("trim_end", args, 0)?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(raw.trim_end())))
        }

        // ── Predicates ────────────────────────────────────────────────────────
        "contains" => {
            require_args("contains", args, 1)?;
            let needle = require_str(&args[0], "contains", "s")?;
            Ok(RuntimeValue::Bool(raw.contains(needle.as_str())))
        }

        "starts_with" => {
            require_args("starts_with", args, 1)?;
            let prefix = require_str(&args[0], "starts_with", "s")?;
            Ok(RuntimeValue::Bool(raw.starts_with(prefix.as_str())))
        }

        "ends_with" => {
            require_args("ends_with", args, 1)?;
            let suffix = require_str(&args[0], "ends_with", "s")?;
            Ok(RuntimeValue::Bool(raw.ends_with(suffix.as_str())))
        }

        // ── Split / replace ───────────────────────────────────────────────────
        "split" => {
            require_args("split", args, 1)?;
            let sep = require_str(&args[0], "split", "sep")?;
            let parts: Vec<RuntimeValue> = raw
                .split(sep.as_str())
                .map(|part| RuntimeValue::Str(ParsedString::new_plain(part)))
                .collect();
            Ok(RuntimeValue::List(crate::runtime::value::shared(parts)))
        }

        "replace" => {
            require_args("replace", args, 2)?;
            let from = require_str(&args[0], "replace", "from")?;
            let to = require_str(&args[1], "replace", "to")?;
            Ok(RuntimeValue::Str(ParsedString::new_plain(
                &raw.replace(from.as_str(), to.as_str()),
            )))
        }

        // ── Slicing ───────────────────────────────────────────────────────────

        // `slice(start)` or `slice(start, end)`.
        //
        // Both bounds are character-based (Unicode scalar values), not bytes.
        // Negative indices count from the end of the string, clamped to [0, len].
        // An inverted range (start > end after resolution) yields an empty string.
        "slice" => {
            require_args_range("slice", args, 1, 2)?;
            let char_count = raw.chars().count();

            let resolve_bound = |raw_idx: i64| -> usize {
                if raw_idx < 0 {
                    // Negative: count from end, clamping at 0.
                    ((char_count as i64 + raw_idx).max(0)) as usize
                } else {
                    // Positive: clamp at char_count.
                    (raw_idx as usize).min(char_count)
                }
            };

            let start = resolve_bound(require_int(&args[0], "slice", "start")?);
            let end = if args.len() == 2 {
                resolve_bound(require_int(&args[1], "slice", "end")?)
            } else {
                char_count
            };

            let result: String = if start < end {
                raw.chars().skip(start).take(end - start).collect()
            } else {
                String::new()
            };
            Ok(RuntimeValue::Str(ParsedString::new_plain(&result)))
        }

        // ── Type coercions ────────────────────────────────────────────────────
        "to_int" => {
            require_args("to_int", args, 0)?;
            raw.trim()
                .parse::<i64>()
                .map(RuntimeValue::Int)
                .map_err(|_| {
                    super::VmError::TypeError(format!(
                        "str.to_int(): cannot parse {:?} as Int",
                        raw
                    ))
                })
        }

        "to_float" => {
            require_args("to_float", args, 0)?;
            raw.trim()
                .parse::<f64>()
                .map(RuntimeValue::Float)
                .map_err(|_| {
                    super::VmError::TypeError(format!(
                        "str.to_float(): cannot parse {:?} as Float",
                        raw
                    ))
                })
        }

        // ── Character decomposition ───────────────────────────────────────────
        // Returns a `List[Str]` where every element is a single-character string.
        "chars" => {
            require_args("chars", args, 0)?;
            let chars: Vec<RuntimeValue> = raw
                .chars()
                .map(|c| {
                    let mut buf = [0u8; 4];
                    let encoded = c.encode_utf8(&mut buf);
                    RuntimeValue::Str(ParsedString::new_plain(encoded))
                })
                .collect();
            Ok(RuntimeValue::List(crate::runtime::value::shared(chars)))
        }

        "repeat" => {
            require_args("repeat", args, 1)?;
            let n = require_int(&args[0], "repeat", "n")?;
            if n < 0 {
                return Err(super::VmError::TypeError(format!(
                    "str.repeat(): count must be non-negative, got {}",
                    n
                )));
            }
            const MAX_REPEAT_BYTES: usize = 10 * 1024 * 1024; // 10 MiB
            let n_usize = n as usize;
            let total = raw.len().checked_mul(n_usize).ok_or_else(|| {
                super::VmError::TypeError("str.repeat(): result size overflows usize".into())
            })?;
            if total > MAX_REPEAT_BYTES {
                return Err(super::VmError::TypeError(format!(
                    "str.repeat(): result would be {total} bytes (limit: {MAX_REPEAT_BYTES})"
                )));
            }
            Ok(RuntimeValue::Str(ParsedString::new_plain(
                &raw.repeat(n_usize),
            )))
        }

        "lines" => {
            require_args("lines", args, 0)?;
            let lines: Vec<RuntimeValue> = raw
                .lines()
                .map(|line| RuntimeValue::Str(ParsedString::new_plain(line)))
                .collect();
            Ok(RuntimeValue::List(crate::runtime::value::shared(lines)))
        }

        // ── Unknown ───────────────────────────────────────────────────────────
        other => Err(super::VmError::UnknownMethod(format!(
            "'{}' on type Str",
            other
        ))),
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

/// Assert that exactly `expected` arguments were supplied.
fn require_args(
    method: &str,
    args: &[RuntimeValue],
    expected: usize,
) -> Result<(), super::VmError> {
    if args.len() != expected {
        return Err(super::VmError::TypeError(format!(
            "str.{method}() takes {expected} argument(s), got {}",
            args.len()
        )));
    }
    Ok(())
}

/// Assert that the argument count is within `[min, max]` inclusive.
fn require_args_range(
    method: &str,
    args: &[RuntimeValue],
    min: usize,
    max: usize,
) -> Result<(), super::VmError> {
    if args.len() < min || args.len() > max {
        return Err(super::VmError::TypeError(format!(
            "str.{method}() takes {min}–{max} argument(s), got {}",
            args.len()
        )));
    }
    Ok(())
}

/// Coerce a [`RuntimeValue`] to a plain `String`, failing with a descriptive
/// [`super::VmError::TypeError`] if the value is not `RuntimeValue::Str`.
fn require_str(val: &RuntimeValue, method: &str, param: &str) -> Result<String, super::VmError> {
    match val {
        RuntimeValue::Str(ps) => Ok(ps.to_string()),
        other => Err(super::VmError::TypeError(format!(
            "str.{method}() expected a Str for '{param}', got {:?}",
            other
        ))),
    }
}

/// Coerce a [`RuntimeValue`] to `i64`, failing with a descriptive
/// [`super::VmError::TypeError`] if the value is not `RuntimeValue::Int`.
fn require_int(val: &RuntimeValue, method: &str, param: &str) -> Result<i64, super::VmError> {
    match val {
        RuntimeValue::Int(i) => Ok(*i),
        other => Err(super::VmError::TypeError(format!(
            "str.{method}() expected an Int for '{param}', got {:?}",
            other
        ))),
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Convenience constructors ──────────────────────────────────────────────

    fn s(text: &str) -> ParsedString {
        ParsedString::new_plain(text)
    }

    fn str_val(text: &str) -> RuntimeValue {
        RuntimeValue::Str(s(text))
    }

    fn call(
        ps: ParsedString,
        method: &str,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, super::super::VmError> {
        dispatch(ps, method, args)
    }

    // ── len ───────────────────────────────────────────────────────────────────

    #[test]
    fn test_len_ascii() {
        assert_eq!(call(s("hello"), "len", &[]).unwrap(), RuntimeValue::Int(5));
    }

    #[test]
    fn test_len_empty() {
        assert_eq!(call(s(""), "len", &[]).unwrap(), RuntimeValue::Int(0));
    }

    #[test]
    fn test_len_unicode() {
        // "héllo" has 5 characters, not 6 bytes.
        assert_eq!(call(s("héllo"), "len", &[]).unwrap(), RuntimeValue::Int(5));
    }

    #[test]
    fn test_len_wrong_arity() {
        assert!(matches!(
            call(s("x"), "len", &[RuntimeValue::Int(0)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── is_empty ──────────────────────────────────────────────────────────────

    #[test]
    fn test_is_empty_true() {
        assert_eq!(
            call(s(""), "is_empty", &[]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_is_empty_false() {
        assert_eq!(
            call(s("a"), "is_empty", &[]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    // ── to_upper / to_lower ───────────────────────────────────────────────────

    #[test]
    fn test_to_upper() {
        assert_eq!(
            call(s("Hello World"), "to_upper", &[]).unwrap(),
            str_val("HELLO WORLD")
        );
    }

    #[test]
    fn test_to_lower() {
        assert_eq!(
            call(s("Hello World"), "to_lower", &[]).unwrap(),
            str_val("hello world")
        );
    }

    // ── trim / trim_start / trim_end ──────────────────────────────────────────

    #[test]
    fn test_trim() {
        assert_eq!(call(s("  hi  "), "trim", &[]).unwrap(), str_val("hi"));
    }

    #[test]
    fn test_trim_start() {
        assert_eq!(
            call(s("  hi  "), "trim_start", &[]).unwrap(),
            str_val("hi  ")
        );
    }

    #[test]
    fn test_trim_end() {
        assert_eq!(call(s("  hi  "), "trim_end", &[]).unwrap(), str_val("  hi"));
    }

    #[test]
    fn test_trim_noop() {
        assert_eq!(call(s("clean"), "trim", &[]).unwrap(), str_val("clean"));
    }

    // ── contains ─────────────────────────────────────────────────────────────

    #[test]
    fn test_contains_present() {
        assert_eq!(
            call(s("foobar"), "contains", &[str_val("bar")]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_contains_absent() {
        assert_eq!(
            call(s("foobar"), "contains", &[str_val("baz")]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    #[test]
    fn test_contains_wrong_type() {
        assert!(matches!(
            call(s("hi"), "contains", &[RuntimeValue::Int(1)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── starts_with / ends_with ───────────────────────────────────────────────

    #[test]
    fn test_starts_with_true() {
        assert_eq!(
            call(s("hello"), "starts_with", &[str_val("hel")]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_starts_with_false() {
        assert_eq!(
            call(s("hello"), "starts_with", &[str_val("ell")]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    #[test]
    fn test_ends_with_true() {
        assert_eq!(
            call(s("hello"), "ends_with", &[str_val("llo")]).unwrap(),
            RuntimeValue::Bool(true)
        );
    }

    #[test]
    fn test_ends_with_false() {
        assert_eq!(
            call(s("hello"), "ends_with", &[str_val("xyz")]).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    // ── split ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_split_comma() {
        assert_eq!(
            call(s("a,b,c"), "split", &[str_val(",")]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![
                str_val("a"),
                str_val("b"),
                str_val("c")
            ]))
        );
    }

    #[test]
    fn test_split_empty_sep() {
        // Splitting by "" in Rust yields empty strings around every character.
        let result = call(s("ab"), "split", &[str_val("")]).unwrap();
        assert!(matches!(result, RuntimeValue::List(_)));
    }

    #[test]
    fn test_split_no_sep_present() {
        assert_eq!(
            call(s("hello"), "split", &[str_val(",")]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![str_val("hello")]))
        );
    }

    // ── replace ───────────────────────────────────────────────────────────────

    #[test]
    fn test_replace_basic() {
        assert_eq!(
            call(
                s("hello world"),
                "replace",
                &[str_val("world"), str_val("Rust")]
            )
            .unwrap(),
            str_val("hello Rust")
        );
    }

    #[test]
    fn test_replace_multiple_occurrences() {
        assert_eq!(
            call(s("aaa"), "replace", &[str_val("a"), str_val("b")]).unwrap(),
            str_val("bbb")
        );
    }

    #[test]
    fn test_replace_not_found() {
        assert_eq!(
            call(s("hello"), "replace", &[str_val("xyz"), str_val("abc")]).unwrap(),
            str_val("hello")
        );
    }

    // ── slice ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_slice_start_only() {
        // "hello".slice(2) → "llo"
        assert_eq!(
            call(s("hello"), "slice", &[RuntimeValue::Int(2)]).unwrap(),
            str_val("llo")
        );
    }

    #[test]
    fn test_slice_start_and_end() {
        // "hello".slice(1, 4) → "ell"
        assert_eq!(
            call(
                s("hello"),
                "slice",
                &[RuntimeValue::Int(1), RuntimeValue::Int(4)]
            )
            .unwrap(),
            str_val("ell")
        );
    }

    #[test]
    fn test_slice_unicode_safe() {
        // "héllo" — 'é' is a single char; slice(0, 3) must give "hél", not
        // a byte-split mess.
        assert_eq!(
            call(
                s("héllo"),
                "slice",
                &[RuntimeValue::Int(0), RuntimeValue::Int(3)]
            )
            .unwrap(),
            str_val("hél")
        );
    }

    #[test]
    fn test_slice_inverted_range_returns_empty() {
        assert_eq!(
            call(
                s("hello"),
                "slice",
                &[RuntimeValue::Int(4), RuntimeValue::Int(2)]
            )
            .unwrap(),
            str_val("")
        );
    }

    #[test]
    fn test_slice_out_of_bounds_clamped() {
        // End beyond string length is clamped.
        assert_eq!(
            call(
                s("hi"),
                "slice",
                &[RuntimeValue::Int(0), RuntimeValue::Int(100)]
            )
            .unwrap(),
            str_val("hi")
        );
    }

    #[test]
    fn test_slice_negative_start() {
        // "hello".slice(-3) → "llo"
        assert_eq!(
            call(s("hello"), "slice", &[RuntimeValue::Int(-3)]).unwrap(),
            str_val("llo")
        );
    }

    // ── to_int ────────────────────────────────────────────────────────────────

    #[test]
    fn test_to_int_valid() {
        assert_eq!(call(s("42"), "to_int", &[]).unwrap(), RuntimeValue::Int(42));
    }

    #[test]
    fn test_to_int_negative() {
        assert_eq!(call(s("-7"), "to_int", &[]).unwrap(), RuntimeValue::Int(-7));
    }

    #[test]
    fn test_to_int_trims_whitespace() {
        assert_eq!(
            call(s(" 10 "), "to_int", &[]).unwrap(),
            RuntimeValue::Int(10)
        );
    }

    #[test]
    fn test_to_int_invalid() {
        assert!(matches!(
            call(s("abc"), "to_int", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── to_float ──────────────────────────────────────────────────────────────

    #[test]
    fn test_to_float_valid() {
        assert_eq!(
            call(s("1.23"), "to_float", &[]).unwrap(),
            RuntimeValue::Float(1.23)
        );
    }

    #[test]
    fn test_to_float_integer_string() {
        assert_eq!(
            call(s("2"), "to_float", &[]).unwrap(),
            RuntimeValue::Float(2.0)
        );
    }

    #[test]
    fn test_to_float_invalid() {
        assert!(matches!(
            call(s("xyz"), "to_float", &[]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── chars ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_chars_ascii() {
        assert_eq!(
            call(s("abc"), "chars", &[]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![
                str_val("a"),
                str_val("b"),
                str_val("c")
            ]))
        );
    }

    #[test]
    fn test_chars_unicode() {
        // Each element must be a single Unicode scalar value.
        let result = call(s("hé"), "chars", &[]).unwrap();
        match result {
            RuntimeValue::List(items) => {
                assert_eq!(items.borrow().len(), 2);
                assert_eq!(items.borrow()[0], str_val("h"));
                assert_eq!(items.borrow()[1], str_val("é"));
            }
            other => panic!("expected List, got {:?}", other),
        }
    }

    #[test]
    fn test_chars_empty() {
        assert_eq!(
            call(s(""), "chars", &[]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![]))
        );
    }

    // ── repeat ────────────────────────────────────────────────────────────────

    #[test]
    fn test_repeat_basic() {
        assert_eq!(
            call(s("ab"), "repeat", &[RuntimeValue::Int(3)]).unwrap(),
            str_val("ababab")
        );
    }

    #[test]
    fn test_repeat_zero() {
        assert_eq!(
            call(s("x"), "repeat", &[RuntimeValue::Int(0)]).unwrap(),
            str_val("")
        );
    }

    #[test]
    fn test_repeat_negative_errors() {
        assert!(matches!(
            call(s("x"), "repeat", &[RuntimeValue::Int(-1)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    // ── lines ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_lines_basic() {
        assert_eq!(
            call(s("a\nb\nc"), "lines", &[]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![
                str_val("a"),
                str_val("b"),
                str_val("c")
            ]))
        );
    }

    #[test]
    fn test_lines_crlf() {
        // Rust's `str::lines` strips both `\n` and `\r\n`.
        assert_eq!(
            call(s("x\r\ny"), "lines", &[]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![
                str_val("x"),
                str_val("y")
            ]))
        );
    }

    #[test]
    fn test_lines_single_line() {
        assert_eq!(
            call(s("hello"), "lines", &[]).unwrap(),
            RuntimeValue::List(crate::runtime::value::shared(vec![str_val("hello")]))
        );
    }

    // ── error cases ───────────────────────────────────────────────────────────

    #[test]
    fn test_unknown_method() {
        assert!(matches!(
            call(s("hello"), "frobnicate", &[]).unwrap_err(),
            super::super::VmError::UnknownMethod(_)
        ));
    }

    #[test]
    fn test_unknown_method_message_contains_name() {
        let err = call(s("hello"), "frobnicate", &[]).unwrap_err();
        match err {
            super::super::VmError::UnknownMethod(msg) => {
                assert!(msg.contains("frobnicate"), "message was: {msg}");
            }
            other => panic!("expected UnknownMethod, got {:?}", other),
        }
    }

    #[test]
    fn test_wrong_arity_returns_type_error() {
        // `trim` takes 0 args; supplying one should fail.
        assert!(matches!(
            call(s("hello"), "trim", &[str_val("x")]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }

    #[test]
    fn test_wrong_arg_type_contains() {
        assert!(matches!(
            call(s("hello"), "contains", &[RuntimeValue::Int(5)]).unwrap_err(),
            super::super::VmError::TypeError(_)
        ));
    }
}
