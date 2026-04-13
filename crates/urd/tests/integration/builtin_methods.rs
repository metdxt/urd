//! # Built-in Method Integration Tests
//!
//! These tests exercise **every** built-in method through the full
//! parse → compile → VM pipeline, ensuring that the methods are not only
//! correct in isolation (unit tests in `*_methods.rs`) but also reachable
//! and correctly wired when invoked from real Urd scripts.
//!
//! Each test embeds a small Urd script, runs it to completion, and asserts
//! on the dialogue text emitted via string interpolation.
//!
//! ## Key constraints
//!
//! - String interpolation only supports simple paths (`{x}`, `{x.y}`), **not**
//!   method calls.  Every method result must be stored in a `let` binding first.
//! - Method chaining (`x.foo().bar()`) is **not** supported by the parser.
//!   Intermediate variables must be used for each call.

#![allow(missing_docs)]

use urd::{Event, RuntimeValue, VmError, VmStep};

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion (or the first error/termination),
/// returning every [`VmStep`] observed.  Capped at 1024 steps to prevent infinite
/// loops in broken scripts.
fn run_script(src: &str) -> Vec<VmStep> {
    super::fixtures::run_script(src, 1024)
}

/// Collect every dialogue line (as a plain `String`) from a step sequence.
fn dialogue_texts(steps: &[VmStep]) -> Vec<String> {
    let mut out = Vec::new();
    for step in steps {
        if let VmStep::Event(Event::Dialogue { lines, .. }) = step {
            for val in lines {
                if let RuntimeValue::Str(ps) = val {
                    out.push(ps.to_string());
                }
            }
        }
    }
    out
}

/// Return the first [`VmError`] in the step sequence, if any.
fn first_error(steps: &[VmStep]) -> Option<&VmError> {
    steps.iter().find_map(|s| {
        if let VmStep::Error(e) = s {
            Some(e)
        } else {
            None
        }
    })
}

/// Run a script and return the collected dialogue texts.  Panics if a VM
/// error is encountered.
#[allow(clippy::expect_used)]
fn run_and_collect(src: &str) -> Vec<String> {
    let steps = run_script(src);
    assert!(
        first_error(&steps).is_none(),
        "unexpected VM error: {:?}",
        first_error(&steps)
    );
    dialogue_texts(&steps)
}

// ═══════════════════════════════════════════════════════════════════════════════
// String methods
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn test_str_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello"
    let r = x.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_str_len_unicode() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "héllo"
    let r = x.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    // len() counts Unicode scalar values, not bytes.
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_str_is_empty_true() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = ""
    let r = x.is_empty()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_str_is_empty_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hi"
    let r = x.is_empty()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_str_to_upper() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello World"
    let r = x.to_upper()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["HELLO WORLD"]);
}

#[test]
fn test_str_to_lower() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "Hello WORLD"
    let r = x.to_lower()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["hello world"]);
}

#[test]
fn test_str_trim() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "  hello  "
    let r = x.trim()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["hello"]);
}

#[test]
fn test_str_trim_start() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "  hello  "
    let r = x.trim_start()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["hello  "]);
}

#[test]
fn test_str_trim_end() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "  hello  "
    let r = x.trim_end()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["  hello"]);
}

#[test]
fn test_str_contains_true() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello world"
    let r = x.contains("world")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_str_contains_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello world"
    let r = x.contains("xyz")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_str_starts_with() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello world"
    let a = x.starts_with("hello")
    let b = x.starts_with("world")
    Narrator: "{a}"
    Narrator: "{b}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true", "false"]);
}

#[test]
fn test_str_ends_with() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello world"
    let a = x.ends_with("world")
    let b = x.ends_with("hello")
    Narrator: "{a}"
    Narrator: "{b}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true", "false"]);
}

#[test]
fn test_str_split() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "a,b,c"
    let r = x.split(",")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[a, b, c]"]);
}

#[test]
fn test_str_replace() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello world"
    let r = x.replace("world", "urd")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["hello urd"]);
}

#[test]
fn test_str_replace_multiple_occurrences() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "aaa"
    let r = x.replace("a", "bb")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["bbbbbb"]);
}

#[test]
fn test_str_slice_one_arg() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello"
    let r = x.slice(2)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["llo"]);
}

#[test]
fn test_str_slice_two_args() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello"
    let r = x.slice(1, 4)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["ell"]);
}

#[test]
fn test_str_slice_negative_start() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello"
    let r = x.slice(-3)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["llo"]);
}

#[test]
fn test_str_to_int() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "42"
    let r = x.to_int()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["42"]);
}

#[test]
fn test_str_to_int_negative() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "-7"
    let r = x.to_int()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["-7"]);
}

#[test]
fn test_str_to_float() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "3.14"
    let r = x.to_float()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["3.14"]);
}

#[test]
fn test_str_chars() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "abc"
    let r = x.chars()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[a, b, c]"]);
}

#[test]
fn test_str_repeat() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "ab"
    let r = x.repeat(3)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["ababab"]);
}

#[test]
fn test_str_repeat_zero() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "ab"
    let r = x.repeat(0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec![""]);
}

#[test]
fn test_str_lines() {
    // Using \n escape sequences inside a regular Urd string.
    let texts = run_and_collect(
        "
@entry
label start {
    let x = \"line1\\nline2\\nline3\"
    let r = x.lines()
    Narrator: \"{r}\"
    end!()
}
",
    );
    assert_eq!(texts, vec!["[line1, line2, line3]"]);
}

// ── String method chaining (via intermediate variables) ───────────────────────

#[test]
fn test_str_chain_to_upper_then_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "hello"
    let upper = x.to_upper()
    let r = upper.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_str_chain_trim_then_to_upper() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = "  hi  "
    let trimmed = x.trim()
    let r = trimmed.to_upper()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["HI"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Int methods
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn test_int_to_string() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 42
    let r = x.to_string()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["42"]);
}

#[test]
fn test_int_to_float() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 7
    let r = x.to_float()
    Narrator: "{r}"
    end!()
}
"#,
    );
    // 7 as f64 displays as "7" via f64::to_string()
    assert_eq!(texts, vec!["7"]);
}

#[test]
fn test_int_abs_positive() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 5
    let r = x.abs()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_int_abs_negative() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = -5
    let r = x.abs()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_int_abs_zero() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 0
    let r = x.abs()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["0"]);
}

#[test]
fn test_int_min() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 10
    let r = x.min(3)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["3"]);
}

#[test]
fn test_int_max() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 10
    let r = x.max(30)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["30"]);
}

#[test]
fn test_int_clamp_in_range() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 5
    let r = x.clamp(0, 10)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_int_clamp_below_min() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = -5
    let r = x.clamp(0, 10)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["0"]);
}

#[test]
fn test_int_clamp_above_max() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 50
    let r = x.clamp(0, 10)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10"]);
}

#[test]
fn test_int_pow() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 2
    let r = x.pow(10)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["1024"]);
}

#[test]
fn test_int_pow_zero_exponent() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 99
    let r = x.pow(0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["1"]);
}

#[test]
fn test_int_signum_positive() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 42
    let r = x.signum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["1"]);
}

#[test]
fn test_int_signum_negative() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = -42
    let r = x.signum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["-1"]);
}

#[test]
fn test_int_signum_zero() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 0
    let r = x.signum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["0"]);
}

// ── Int method chaining (via intermediate variables) ──────────────────────────

#[test]
fn test_int_to_string_then_str_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let n = 12345
    let s = n.to_string()
    let r = s.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Float methods
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn test_float_to_string() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 3.14
    let r = x.to_string()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["3.14"]);
}

#[test]
fn test_float_to_int() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 3.99
    let r = x.to_int()
    Narrator: "{r}"
    end!()
}
"#,
    );
    // Truncates toward zero
    assert_eq!(texts, vec!["3"]);
}

#[test]
fn test_float_to_int_negative() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = -3.99
    let r = x.to_int()
    Narrator: "{r}"
    end!()
}
"#,
    );
    // Truncates toward zero: -3.99 → -3
    assert_eq!(texts, vec!["-3"]);
}

#[test]
fn test_float_abs() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = -2.5
    let r = x.abs()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["2.5"]);
}

#[test]
fn test_float_floor() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 3.7
    let r = x.floor()
    Narrator: "{r}"
    end!()
}
"#,
    );
    // floor() returns Int
    assert_eq!(texts, vec!["3"]);
}

#[test]
fn test_float_floor_negative() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = -3.2
    let r = x.floor()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["-4"]);
}

#[test]
fn test_float_ceil() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 3.2
    let r = x.ceil()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["4"]);
}

#[test]
fn test_float_round() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let a = 3.4
    let ra = a.round()
    Narrator: "{ra}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["3"]);
}

#[test]
fn test_float_round_half_up() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let b = 3.5
    let rb = b.round()
    Narrator: "{rb}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["4"]);
}

#[test]
fn test_float_sqrt() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 16.0
    let r = x.sqrt()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["4"]);
}

#[test]
fn test_float_min() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 5.5
    let r = x.min(2.0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["2"]);
}

#[test]
fn test_float_max() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 5.5
    let r = x.max(10.0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10"]);
}

#[test]
fn test_float_clamp() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 15.0
    let r = x.clamp(0.0, 10.0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10"]);
}

#[test]
fn test_float_pow() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 2.0
    let r = x.pow(3.0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["8"]);
}

#[test]
fn test_float_is_finite() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 1.0
    let r = x.is_finite()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_float_is_nan_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 1.0
    let r = x.is_nan()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_float_signum_positive() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = 3.14
    let r = x.signum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["1"]);
}

#[test]
fn test_float_signum_negative() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let x = -3.14
    let r = x.signum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["-1"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// List methods
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn test_list_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let r = xs.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_list_len_empty() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = []
    let r = xs.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["0"]);
}

#[test]
fn test_list_get_positive() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [10, 20, 30]
    let a = xs.get(0)
    let b = xs.get(2)
    Narrator: "{a}"
    Narrator: "{b}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10", "30"]);
}

#[test]
fn test_list_get_negative_index() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [10, 20, 30]
    let r = xs.get(-1)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["30"]);
}

#[test]
fn test_list_first() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [10, 20, 30]
    let r = xs.first()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10"]);
}

#[test]
fn test_list_last() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [10, 20, 30]
    let r = xs.last()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["30"]);
}

#[test]
fn test_list_contains_true() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.contains(2)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_list_contains_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.contains(99)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_list_append() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2]
    let r = xs.append(3)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[1, 2, 3]"]);
}

#[test]
fn test_list_prepend() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [2, 3]
    let r = xs.prepend(1)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[1, 2, 3]"]);
}

#[test]
fn test_list_pop() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.pop()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[1, 2]"]);
}

#[test]
fn test_list_concat() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let a = [1, 2]
    let b = [3, 4]
    let r = a.concat(b)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[1, 2, 3, 4]"]);
}

#[test]
fn test_list_reversed() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.reversed()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[3, 2, 1]"]);
}

#[test]
fn test_list_with() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [10, 20, 30]
    let r = xs.with(1, 99)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[10, 99, 30]"]);
}

#[test]
fn test_list_slice_one_arg() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let r = xs.slice(2)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[3, 4, 5]"]);
}

#[test]
fn test_list_slice_two_args() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let r = xs.slice(1, 3)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[2, 3]"]);
}

#[test]
fn test_list_join_no_sep() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.join()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["123"]);
}

#[test]
fn test_list_join_with_sep() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.join(", ")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["1, 2, 3"]);
}

#[test]
fn test_list_min() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [5, 3, 8, 1, 4]
    let r = xs.min()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["1"]);
}

#[test]
fn test_list_max() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [5, 3, 8, 1, 4]
    let r = xs.max()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["8"]);
}

#[test]
fn test_list_sum() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let r = xs.sum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["15"]);
}

#[test]
fn test_list_sum_empty() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = []
    let r = xs.sum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["0"]);
}

// ── List higher-order methods ─────────────────────────────────────────────────

#[test]
fn test_list_map_with_anonymous_fn() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let double = fn(x) { return x * 2 }
    let r = xs.map(double)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[2, 4, 6]"]);
}

#[test]
fn test_list_filter_with_anonymous_fn() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5, 6]
    let pred = fn(x) { return x > 3 }
    let r = xs.filter(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[4, 5, 6]"]);
}

#[test]
fn test_list_filter_empty_result() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let pred = fn(x) { return x > 100 }
    let r = xs.filter(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[]"]);
}

#[test]
fn test_list_reduce_sum() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4]
    let add = fn(acc, x) { return acc + x }
    let r = xs.reduce(0, add)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10"]);
}

#[test]
fn test_list_fold_alias() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4]
    let add = fn(acc, x) { return acc + x }
    let r = xs.fold(0, add)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10"]);
}

#[test]
fn test_list_reduce_product() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let mul = fn(acc, x) { return acc * x }
    let r = xs.reduce(1, mul)
    Narrator: "{r}"
    end!()
}
"#,
    );
    // 1 * (1 * 2 * 3 * 4 * 5) = 120
    assert_eq!(texts, vec!["120"]);
}

#[test]
fn test_list_reduce_empty_returns_init() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = []
    let add = fn(acc, x) { return acc + x }
    let r = xs.reduce(42, add)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["42"]);
}

#[test]
fn test_list_reduce_max_manual() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [3, 7, 2, 9, 4]
    let pick_max = fn(acc, x) { if acc > x { return acc } else { return x } }
    let r = xs.reduce(0, pick_max)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["9"]);
}

#[test]
fn test_list_find_returns_first_match() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let pred = fn(x) { return x > 3 }
    let r = xs.find(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["4"]);
}

#[test]
fn test_list_find_returns_null_when_no_match() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let pred = fn(x) { return x > 100 }
    let r = xs.find(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["null"]);
}

#[test]
fn test_list_any_true() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let pred = fn(x) { return x == 2 }
    let r = xs.any(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_list_any_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let pred = fn(x) { return x > 100 }
    let r = xs.any(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_list_any_empty_is_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = []
    let pred = fn(x) { return x > 0 }
    let r = xs.any(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_list_all_true() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [2, 4, 6]
    let pred = fn(x) { return x > 0 }
    let r = xs.all(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_list_all_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [2, 4, -1]
    let pred = fn(x) { return x > 0 }
    let r = xs.all(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_list_all_empty_is_true() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = []
    let pred = fn(x) { return x > 0 }
    let r = xs.all(pred)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_list_sort_by_ascending() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [3, 1, 4, 1, 5]
    let cmp = fn(a, b) { return a - b }
    let r = xs.sort_by(cmp)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[1, 1, 3, 4, 5]"]);
}

#[test]
fn test_list_sort_by_descending() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [3, 1, 4, 1, 5]
    let cmp = fn(a, b) { return b - a }
    let r = xs.sort_by(cmp)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[5, 4, 3, 1, 1]"]);
}

#[test]
fn test_list_zip_equal_length() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let a = [1, 2, 3]
    let b = ["a", "b", "c"]
    let r = a.zip(b)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[[1, a], [2, b], [3, c]]"]);
}

#[test]
fn test_list_zip_different_lengths() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let a = [1, 2, 3, 4]
    let b = ["x", "y"]
    let r = a.zip(b)
    Narrator: "{r}"
    end!()
}
"#,
    );
    // Truncates to shorter length
    assert_eq!(texts, vec!["[[1, x], [2, y]]"]);
}

// ── List method chaining (via intermediate variables) ─────────────────────────

#[test]
fn test_list_map_then_filter() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let double = fn(x) { return x * 2 }
    let doubled = xs.map(double)
    let gt5 = fn(x) { return x > 5 }
    let r = doubled.filter(gt5)
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[6, 8, 10]"]);
}

#[test]
fn test_list_filter_then_sum() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5, 6]
    let pred = fn(x) { return x > 3 }
    let filtered = xs.filter(pred)
    let r = filtered.sum()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["15"]);
}

#[test]
fn test_list_reversed_then_first() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [10, 20, 30]
    let rev = xs.reversed()
    let r = rev.first()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["30"]);
}

#[test]
fn test_list_map_then_join() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let times10 = fn(x) { return x * 10 }
    let tens = xs.map(times10)
    let r = tens.join("-")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["10-20-30"]);
}

#[test]
fn test_str_split_then_list_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let s = "one,two,three"
    let parts = s.split(",")
    let r = parts.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["3"]);
}

#[test]
fn test_str_chars_then_reversed_then_join() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let s = "abcd"
    let chars = s.chars()
    let rev = chars.reversed()
    let r = rev.join("")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["dcba"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Map methods
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn test_map_get_existing() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{hp: 100, mp: 50}
    let r = m.get("hp")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["100"]);
}

#[test]
fn test_map_get_missing_returns_null() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{hp: 100}
    let r = m.get("stamina")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["null"]);
}

#[test]
fn test_map_has_present() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{hp: 100, mp: 50}
    let r = m.has("hp")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_map_has_absent() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{hp: 100, mp: 50}
    let r = m.has("stamina")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_map_keys() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{b: 2, a: 1}
    let r = m.keys()
    Narrator: "{r}"
    end!()
}
"#,
    );
    // keys() returns sorted keys
    assert_eq!(texts, vec!["[a, b]"]);
}

#[test]
fn test_map_values_sorted_by_key() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{b: 2, a: 1}
    let r = m.values()
    Narrator: "{r}"
    end!()
}
"#,
    );
    // values() iterates in sorted key order: a=1, b=2
    assert_eq!(texts, vec!["[1, 2]"]);
}

#[test]
fn test_map_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{x: 1, y: 2, z: 3}
    let r = m.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["3"]);
}

#[test]
fn test_map_is_empty_false() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{x: 1}
    let r = m.is_empty()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_map_set() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{hp: 100}
    let m2 = m.set("mp", 50)
    let hp = m2.get("hp")
    let mp = m2.get("mp")
    Narrator: "{hp}"
    Narrator: "{mp}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["100", "50"]);
}

#[test]
fn test_map_remove() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{hp: 100, mp: 50}
    let m2 = m.remove("hp")
    let has_hp = m2.has("hp")
    let has_mp = m2.has("mp")
    Narrator: "{has_hp}"
    Narrator: "{has_mp}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false", "true"]);
}

#[test]
fn test_map_merge() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let a = :{hp: 100, mp: 50}
    let b = :{hp: 999, sp: 25}
    let r = a.merge(b)
    let rhp = r.get("hp")
    let rmp = r.get("mp")
    let rsp = r.get("sp")
    Narrator: "{rhp}"
    Narrator: "{rmp}"
    Narrator: "{rsp}"
    end!()
}
"#,
    );
    // b wins on "hp" conflict
    assert_eq!(texts, vec!["999", "50", "25"]);
}

// ── Map method chaining (via intermediate variables) ──────────────────────────

#[test]
fn test_map_set_then_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let m = :{a: 1}
    let m2 = m.set("b", 2)
    let r = m2.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["2"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Range methods
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn test_range_len_exclusive() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let lo = 0
    let hi = 5
    let r = lo..hi
    let n = r.len()
    Narrator: "{n}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["5"]);
}

#[test]
fn test_range_len_inclusive() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let lo = 0
    let hi = 5
    let r = lo..=hi
    let n = r.len()
    Narrator: "{n}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["6"]);
}

#[test]
fn test_range_contains_true() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let lo = 0
    let hi = 10
    let r = lo..hi
    let c = r.contains(5)
    Narrator: "{c}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_range_contains_false_at_exclusive_end() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let lo = 0
    let hi = 10
    let r = lo..hi
    let c = r.contains(10)
    Narrator: "{c}"
    end!()
}
"#,
    );
    // 10 is NOT in 0..10 (exclusive upper bound)
    assert_eq!(texts, vec!["false"]);
}

#[test]
fn test_range_contains_true_at_inclusive_end() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let lo = 0
    let hi = 10
    let r = lo..=hi
    let c = r.contains(10)
    Narrator: "{c}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["true"]);
}

#[test]
fn test_range_contains_below_start() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let lo = 5
    let hi = 10
    let r = lo..hi
    let c = r.contains(3)
    Narrator: "{c}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["false"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Cross-type chaining and complex scenarios (via intermediate variables)
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn test_list_join_int_elements() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.join(", ")
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["1, 2, 3"]);
}

#[test]
fn test_map_merge_then_keys() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let a = :{x: 1}
    let b = :{y: 2}
    let merged = a.merge(b)
    let r = merged.keys()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["[x, y]"]);
}

#[test]
fn test_list_filter_then_len() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5, 6, 7, 8]
    let pred = fn(x) { return x > 4 }
    let big = xs.filter(pred)
    let r = big.len()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["4"]);
}

#[test]
fn test_list_map_then_reduce() {
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let double = fn(x) { return x * 2 }
    let doubled = xs.map(double)
    let add = fn(acc, x) { return acc + x }
    let r = doubled.reduce(0, add)
    Narrator: "{r}"
    end!()
}
"#,
    );
    // [2, 4, 6] → sum = 12
    assert_eq!(texts, vec!["12"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Error-path / edge-case tests
// ═══════════════════════════════════════════════════════════════════════════════
//
// Every test above is a happy path.  The tests below deliberately feed invalid
// or boundary inputs to built-in methods and assert that the VM surfaces the
// expected error (or correct fallback value) instead of panicking or silently
// producing garbage.

// ── String error paths ────────────────────────────────────────────────────────

#[test]
fn test_str_to_int_invalid() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = "abc"
    let r = x.to_int()
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for to_int on non-numeric string"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("to_int") && msg.contains("abc"),
        "error should mention method name and offending value, got: {msg}"
    );
}

#[test]
fn test_str_to_float_invalid() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = "not_a_number"
    let r = x.to_float()
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for to_float on non-numeric string"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("to_float"),
        "error should mention 'to_float', got: {msg}"
    );
}

#[test]
fn test_str_repeat_negative() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = "hi"
    let r = x.repeat(-1)
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for repeat with negative count"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("non-negative"),
        "error should mention 'non-negative', got: {msg}"
    );
}

#[test]
fn test_str_to_int_empty_string() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = ""
    let r = x.to_int()
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(err.is_some(), "expected VmError for to_int on empty string");
}

// ── List error paths ──────────────────────────────────────────────────────────

#[test]
fn test_list_first_empty() {
    // list.first() on an empty list returns Err(TypeError).
    let steps = run_script(
        r#"
@entry
label start {
    let xs = []
    let r = xs.first()
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(err.is_some(), "expected VmError for first() on empty list");
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("empty list"),
        "error should mention 'empty list', got: {msg}"
    );
}

#[test]
fn test_list_last_empty() {
    // list.last() on an empty list returns Err(TypeError).
    let steps = run_script(
        r#"
@entry
label start {
    let xs = []
    let r = xs.last()
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(err.is_some(), "expected VmError for last() on empty list");
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("empty list"),
        "error should mention 'empty list', got: {msg}"
    );
}

#[test]
fn test_list_pop_empty() {
    // list.pop() on an empty list returns Err(TypeError).
    let steps = run_script(
        r#"
@entry
label start {
    let xs = []
    let r = xs.pop()
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(err.is_some(), "expected VmError for pop() on empty list");
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("empty list"),
        "error should mention 'empty list', got: {msg}"
    );
}

#[test]
fn test_list_get_out_of_bounds() {
    // list.get(99) on a 3-element list returns Err(IndexOutOfBounds).
    let steps = run_script(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.get(99)
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for get() with out-of-bounds index"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("out of bounds"),
        "error should mention 'out of bounds', got: {msg}"
    );
}

#[test]
fn test_list_get_negative_out_of_bounds() {
    // list.get(-100) on a 3-element list; resolves to negative, OOB.
    let steps = run_script(
        r#"
@entry
label start {
    let xs = [1, 2, 3]
    let r = xs.get(-100)
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for get() with large negative index"
    );
}

#[test]
fn test_list_min_empty_returns_null() {
    // list.min() on an empty list returns Null (not an error).
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = []
    let r = xs.min()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["null"]);
}

#[test]
fn test_list_max_empty_returns_null() {
    // list.max() on an empty list returns Null (not an error).
    let texts = run_and_collect(
        r#"
@entry
label start {
    let xs = []
    let r = xs.max()
    Narrator: "{r}"
    end!()
}
"#,
    );
    assert_eq!(texts, vec!["null"]);
}

// ── Float error paths ─────────────────────────────────────────────────────────

#[test]
fn test_float_sqrt_negative() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = -1.0
    let r = x.sqrt()
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(err.is_some(), "expected VmError for sqrt of negative float");
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("negative"),
        "error should mention 'negative', got: {msg}"
    );
}

#[test]
fn test_float_clamp_inverted_bounds() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = 5.0
    let r = x.clamp(10.0, 0.0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for float.clamp with inverted bounds"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("must not exceed"),
        "error should mention 'must not exceed', got: {msg}"
    );
}

#[test]
fn test_float_division_by_zero() {
    // 0.0 / 0.0 is guarded by the VM — produces a TypeError, not NaN.
    // This means NaN is unreachable through normal script operations,
    // so is_nan will always be false in a well-behaved script.
    let steps = run_script(
        r#"
@entry
label start {
    let x = 0.0
    let r = x / 0.0
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(err.is_some(), "expected VmError for float division by zero");
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("division by zero"),
        "error should mention 'division by zero', got: {msg}"
    );
}

// ── Int error paths ───────────────────────────────────────────────────────────

#[test]
fn test_int_clamp_inverted_bounds() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = 5
    let r = x.clamp(10, 0)
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for int.clamp with inverted bounds"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("must not exceed"),
        "error should mention 'must not exceed', got: {msg}"
    );
}

#[test]
fn test_int_pow_negative_exponent() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = 2
    let r = x.pow(-1)
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for int.pow with negative exponent"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("non-negative"),
        "error should mention 'non-negative', got: {msg}"
    );
}

#[test]
fn test_int_pow_overflow() {
    // 2^63 overflows i64; the implementation checks for this.
    let steps = run_script(
        r#"
@entry
label start {
    let x = 2
    let r = x.pow(63)
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(err.is_some(), "expected VmError for int.pow overflow");
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("overflow"),
        "error should mention 'overflow', got: {msg}"
    );
}

#[test]
fn test_int_division_by_zero() {
    let steps = run_script(
        r#"
@entry
label start {
    let x = 42
    let r = x / 0
    Narrator: "{r}"
    end!()
}
"#,
    );
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected VmError for integer division by zero"
    );
    let msg = err.unwrap().to_string();
    assert!(
        msg.contains("division by zero"),
        "error should mention 'division by zero', got: {msg}"
    );
}
