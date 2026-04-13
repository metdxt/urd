//! # Adversarial Tests — Round 2
//!
//! This module targets attack vectors NOT covered by `adversarial.rs` or
//! `adversarial_new.rs`.  Every test asserts the **correct** expected
//! behaviour; tests that expose un-patched bugs will **FAIL**.
//!
//! ## Attack surface covered
//!
//! | Cat | Attack vector |
//! |-----|---------------|
//! | I   | `list.sum()` unchecked integer overflow (panic in debug, wrap in release) |
//! | J   | `str.repeat()` capacity overflow → panic inside `String::repeat` |
//! | K   | `int.pow()` O(n) CPU denial-of-service with base ∈ {-1, 0, 1} |
//! | L   | `range.len()` integer overflow on extreme bounds |
//! | M   | `float.to_int()` / `floor()` / `ceil()` / `round()` silent garbage for NaN / ±∞ |
//! | N   | Pure-function infinite recursion → native stack overflow (no depth guard) |
//! | O   | Float modulo by zero (not tested in adversarial_new) |
//! | P   | `i64::MIN / -1` and `i64::MIN % -1` edge cases (hardware fault without guard) |
//! | Q   | `coerce_to_i64` silent NaN / ±∞ truncation in list aggregates |
//! | R   | Shift operations: large shift amounts, negative base edge cases |
//! | S   | Integer negation overflow: `-(i64::MIN)` |
//! | T   | `list.sum()` on floats coerced to int — precision loss |
//! | U   | Unterminated string literal silently accepted by lexer |
//! | V   | Deeply nested parentheses → parser stack overflow |
//! | W   | `int.pow(0)` boundary and `0.pow(0)` semantics |
//! | X   | Mixed-type division: `Int / Float(0.0)` and `Float / Int(0)` |
//! | Y   | `str.repeat(0)` and `str.repeat(1)` boundary behaviour |
//! | Z   | Empty list aggregate methods: `[].min()`, `[].max()` |

#![allow(missing_docs)]

use std::panic::{AssertUnwindSafe, catch_unwind};

use urd::{
    Event, RuntimeValue, VmError, VmStep,
    compiler::{Compiler, loader::parse_source},
    vm::{Vm, registry::DecoratorRegistry},
};

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion or the first terminal step.
/// Capped at 1024 steps to prevent infinite loops.
fn run_script(src: &str) -> Vec<VmStep> {
    super::fixtures::run_script(src, 1024)
}

/// Collect every dialogue text line from a step sequence.
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

/// Return the first `VmError` observed, if any.
fn first_error(steps: &[VmStep]) -> Option<&VmError> {
    steps.iter().find_map(|s| {
        if let VmStep::Error(e) = s {
            Some(e)
        } else {
            None
        }
    })
}

/// Try to parse+compile+run, catching panics.  Returns Err(msg) on panic.
fn run_script_catching_panic(src: &str) -> Result<Vec<VmStep>, String> {
    catch_unwind(AssertUnwindSafe(|| run_script(src))).map_err(|payload| {
        if let Some(s) = payload.downcast_ref::<&str>() {
            s.to_string()
        } else if let Some(s) = payload.downcast_ref::<String>() {
            s.clone()
        } else {
            "unknown panic".to_string()
        }
    })
}

/// Run a potentially-crashing test in a **subprocess** to isolate stack
/// overflows and SIGABRTs that `catch_unwind` cannot intercept.
///
/// Returns `Some(output)` to the **parent** process (which should inspect the
/// result and `return` early).  Returns `None` inside the **subprocess**
/// (which should execute the dangerous code directly — it is already isolated).
#[allow(clippy::expect_used)]
fn subprocess_guard(test_name: &str) -> Option<std::process::Output> {
    if std::env::var("__URD_SUBPROCESS").is_ok() {
        return None; // We are the subprocess — run the test body.
    }

    let exe = std::env::current_exe().expect("current_exe");
    let output = std::process::Command::new(&exe)
        .env("__URD_SUBPROCESS", "1")
        .arg("--exact")
        .arg(format!("adversarial_round2::{test_name}"))
        .arg("--test-threads=1")
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .expect("failed to spawn subprocess");

    Some(output)
}

/// Like [`subprocess_guard`], but kills the subprocess if it exceeds the given
/// timeout.  Returns `(success, timed_out, stderr)` to the parent.
/// Returns `None` inside the subprocess.
#[allow(clippy::expect_used)]
fn subprocess_guard_timed(test_name: &str, timeout_secs: u64) -> Option<(bool, bool, String)> {
    if std::env::var("__URD_SUBPROCESS").is_ok() {
        return None; // We are the subprocess — run the test body.
    }

    let exe = std::env::current_exe().expect("current_exe");
    let mut child = std::process::Command::new(&exe)
        .env("__URD_SUBPROCESS", "1")
        .arg("--exact")
        .arg(format!("adversarial_round2::{test_name}"))
        .arg("--test-threads=1")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn subprocess");

    let start = std::time::Instant::now();
    let timeout = std::time::Duration::from_secs(timeout_secs);

    loop {
        match child.try_wait().expect("try_wait") {
            Some(_status) => {
                let mut stderr = String::new();
                if let Some(mut err) = child.stderr.take() {
                    use std::io::Read;
                    let _ = err.read_to_string(&mut stderr);
                }
                return Some((_status.success(), false, stderr));
            }
            None => {
                if start.elapsed() > timeout {
                    let _ = child.kill();
                    let _ = child.wait();
                    return Some((false, true, String::new()));
                }
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category I — `list.sum()` unchecked integer overflow
// ════════════════════════════════════════════════════════════════════════════════

/// `[i64::MAX, 1].sum()` must return a graceful error, not panic or wrap.
///
/// Root cause: `list_methods.rs` uses `total += coerce_to_i64(el)` with plain
/// `+=` instead of `checked_add`.  In debug builds this panics; in release it
/// silently wraps to `i64::MIN`.
#[test]
fn test_list_sum_overflow_must_not_panic() {
    if let Some(output) = subprocess_guard("test_list_sum_overflow_must_not_panic") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "list.sum() panicked or aborted (unchecked `+=` overflow in \
             list_methods.rs) instead of returning a VmError.\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let src = &format!(
        r#"
const narrator = :{{ name: "N", name_color: "white" }}
let xs = [{}, 1]
let total = xs.sum()
narrator: "{{total}}"
"#,
        i64::MAX
    );

    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "list.sum([i64::MAX, 1]) should return an overflow error, got: {:?}",
        dialogue_texts(&steps)
    );
}

/// `[i64::MIN, -1].sum()` — underflow direction.
#[test]
fn test_list_sum_underflow_must_not_panic() {
    if let Some(output) = subprocess_guard("test_list_sum_underflow_must_not_panic") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "list.sum() panicked or aborted on underflow (unchecked `+=` in \
             list_methods.rs).\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let min_val = -9223372036854775807 - 1
let xs = [min_val, -1]
let total = xs.sum()
narrator: "{total}"
"#;

    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "list.sum([i64::MIN, -1]) should return an underflow error, got: {:?}",
        dialogue_texts(&steps)
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Category J — `str.repeat()` capacity overflow → panic
// ════════════════════════════════════════════════════════════════════════════════

/// `"a".repeat(i64::MAX)` must not panic.  Rust's `String::repeat` internally
/// calls `with_capacity` which panics on capacity overflow.
///
/// Runs in a subprocess because the OOM abort (SIGABRT) kills the test runner.
#[test]
fn test_str_repeat_huge_count_must_not_panic() {
    if let Some(output) = subprocess_guard("test_str_repeat_huge_count_must_not_panic") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "str.repeat(i64::MAX) panicked or OOM-aborted instead of returning a VmError.\n\
             stderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let src = &format!(
        r#"
const narrator = :{{ name: "N", name_color: "white" }}
let s = "a"
let result = s.repeat({})
narrator: "{{result}}"
"#,
        i64::MAX
    );

    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "str.repeat(i64::MAX) should return an error, not succeed"
    );
}

/// `"abc".repeat(i64::MAX / 2)` — a repeat count that won't overflow `usize`
/// on 64-bit but will still try to allocate terabytes.  Must error gracefully.
///
/// Runs in a subprocess because the OOM abort (SIGABRT) kills the test runner.
#[test]
fn test_str_repeat_large_count_must_not_panic() {
    if let Some(output) = subprocess_guard("test_str_repeat_large_count_must_not_panic") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "str.repeat(1_000_000_000_000) panicked or OOM-aborted.\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let s = "abc"
let result = s.repeat(1000000000000)
narrator: "{result}"
"#;

    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "str.repeat(1_000_000_000_000) should return an error, not try to allocate"
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Category K — `int.pow()` CPU denial-of-service
// ════════════════════════════════════════════════════════════════════════════════

/// `1.pow(2147483647)` — base=1 means the multiplication never overflows,
/// so the O(n) loop runs ~2 billion times.  This should either use
/// exponentiation by squaring or cap the exponent.
///
/// Runs in a subprocess with a timeout — an O(n) loop would hang the runner.
#[test]
fn test_int_pow_base_one_huge_exponent_must_not_hang() {
    if let Some((success, timed_out, stderr)) =
        subprocess_guard_timed("test_int_pow_base_one_huge_exponent_must_not_hang", 5)
    {
        if timed_out {
            panic!(
                "int.pow(2_000_000_000) with base=1 did not complete in 5s — \
                 O(n) loop CPU DoS confirmed"
            );
        }
        assert!(success, "int.pow() crashed or failed.\nstderr: {stderr}");
        return;
    }

    // Subprocess body
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let base = 1
let result = base.pow(2000000000)
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    // 1^anything == 1
    assert!(
        texts.contains(&"1".to_string()) || first_error(&steps).is_some(),
        "expected either '1' or a graceful error, got: {texts:?}"
    );
}

/// `(-1).pow(2147483647)` — same O(n) issue since `(-1)*(-1)` never overflows.
///
/// Runs in a subprocess with a timeout.
#[test]
fn test_int_pow_base_neg_one_huge_exponent_must_not_hang() {
    if let Some((success, timed_out, stderr)) =
        subprocess_guard_timed("test_int_pow_base_neg_one_huge_exponent_must_not_hang", 5)
    {
        if timed_out {
            panic!(
                "int.pow(2_000_000_000) with base=-1 did not complete in 5s — \
                 O(n) loop CPU DoS confirmed"
            );
        }
        assert!(success, "int.pow() crashed or failed.\nstderr: {stderr}");
        return;
    }

    // Subprocess body
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let base = -1
let result = base.pow(2000000000)
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    // (-1)^(even) = 1, (-1)^(odd) = -1.  2_000_000_000 is even.
    assert!(
        texts.contains(&"1".to_string())
            || texts.contains(&"-1".to_string())
            || first_error(&steps).is_some(),
        "expected ±1 or a graceful error, got: {texts:?}"
    );
}

/// `0.pow(2147483647)` — same O(n) issue, though mathematically 0^n = 0 for
/// n > 0.  Should fast-path.
///
/// Runs in a subprocess with a timeout.
#[test]
fn test_int_pow_base_zero_huge_exponent_must_not_hang() {
    if let Some((success, timed_out, stderr)) =
        subprocess_guard_timed("test_int_pow_base_zero_huge_exponent_must_not_hang", 5)
    {
        if timed_out {
            panic!(
                "int.pow(2_000_000_000) with base=0 did not complete in 5s — \
                 O(n) loop CPU DoS confirmed"
            );
        }
        assert!(success, "int.pow() crashed or failed.\nstderr: {stderr}");
        return;
    }

    // Subprocess body
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let base = 0
let result = base.pow(2000000000)
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    assert!(
        texts.contains(&"0".to_string()) || first_error(&steps).is_some(),
        "expected '0' or a graceful error, got: {texts:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Category L — `range.len()` integer overflow on extreme bounds
// ════════════════════════════════════════════════════════════════════════════════

/// `(i64::MIN..=i64::MAX).len()` — the mathematical length is 2^64 which
/// doesn't fit in `i64`.  `range_len` does `end - start + 1` which overflows.
///
/// Runs in a subprocess because the overflow panics (SIGABRT) in debug mode.
#[test]
fn test_range_len_extreme_bounds_must_not_panic() {
    if let Some(output) = subprocess_guard("test_range_len_extreme_bounds_must_not_panic") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "range.len() panicked on extreme bounds (unchecked `end - start + 1` \
             overflow in range_methods.rs).\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let src = &format!(
        r#"
const narrator = :{{ name: "N", name_color: "white" }}
let min_val = -9223372036854775807 - 1
let r = min_val..={}
let l = r.len()
narrator: "{{l}}"
"#,
        i64::MAX
    );

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    if let Some(text) = texts.first() {
        let val: i64 = text.parse().unwrap_or(0);
        assert!(
            val > 0 || first_error(&steps).is_some(),
            "range.len() returned {val} for a range spanning all of i64 — \
             expected positive value or overflow error"
        );
    }
}

/// `(0..=i64::MAX).len()` — length is `i64::MAX + 1` which overflows i64.
///
/// Runs in a subprocess because the overflow panics (SIGABRT) in debug mode.
#[test]
fn test_range_len_zero_to_max_must_not_panic() {
    if let Some(output) = subprocess_guard("test_range_len_zero_to_max_must_not_panic") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "range.len() panicked on 0..=i64::MAX (unchecked `end - start + 1` \
             overflow in range_methods.rs).\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let src = &format!(
        r#"
const narrator = :{{ name: "N", name_color: "white" }}
let r = 0..={}
let l = r.len()
narrator: "{{l}}"
"#,
        i64::MAX
    );

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    if let Some(text) = texts.first() {
        let val: i64 = text.parse().unwrap_or(0);
        assert!(
            val > 0 || first_error(&steps).is_some(),
            "range.len() returned {val} for 0..=i64::MAX — \
             expected positive value or overflow error"
        );
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category M — float.to_int() / floor() / ceil() / round() with NaN / ±∞
// ════════════════════════════════════════════════════════════════════════════════

/// `(0.0 / 0.0).to_int()` — NaN `as i64` gives 0 in Rust (saturating cast).
/// The scripting language should error, not silently produce 0.
///
/// NOTE: This test depends on float division by zero producing NaN or an error.
/// If B (float div by zero) is fixed to return an error, this test may need
/// adjustment — but that's actually the correct outcome.
#[test]
fn test_float_nan_to_int_must_error() {
    // We need to produce a NaN.  If `0.0 / 0.0` is already guarded, try sqrt(-1).
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let neg = -1.0
let nan = neg.sqrt()
let result = nan.to_int()
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // If we get "0" that's the bug — NaN was silently cast to 0.
    if texts.contains(&"0".to_string()) && first_error(&steps).is_none() {
        panic!("float.to_int() silently converted NaN to 0 — should return an error");
    }
    // Getting an error is the correct behaviour.
}

/// `(1.0 / 0.0).floor()` — Infinity.floor() `as i64` saturates to i64::MAX.
/// Should error instead of silently producing a wrong integer.
#[test]
fn test_float_infinity_floor_must_error() {
    // This depends on whether float div by zero is guarded.
    // If it is, the error at division is fine. If not, we get to .floor().
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }

fn make_huge() -> float {
    return 1.7976931348623157e308
}

let big = make_huge()
let bigger = big * 2.0
let result = bigger.floor()
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    let err = first_error(&steps);

    // `f64::MAX * 2.0` = Infinity in IEEE 754.
    // `.floor()` on Infinity then `as i64` → i64::MAX (saturating).
    // The correct behaviour is an error, not i64::MAX.
    if let Some(text) = texts.first() {
        let val: i64 = text.parse().unwrap_or(0);
        if val == i64::MAX && err.is_none() {
            panic!(
                "float.floor() silently saturated Infinity to i64::MAX — should return an error"
            );
        }
    }
}

/// `(-1.0).sqrt().ceil()` — NaN.ceil() should error.
#[test]
fn test_float_nan_ceil_must_error() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let neg = -1.0
let nan = neg.sqrt()
let result = nan.ceil()
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    if texts.contains(&"0".to_string()) && first_error(&steps).is_none() {
        panic!("float.ceil() silently converted NaN to 0 — should return an error");
    }
}

/// `(-1.0).sqrt().round()` — NaN.round() should error.
#[test]
fn test_float_nan_round_must_error() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let neg = -1.0
let nan = neg.sqrt()
let result = nan.round()
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    if texts.contains(&"0".to_string()) && first_error(&steps).is_none() {
        panic!("float.round() silently converted NaN to 0 — should return an error");
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category N — Pure-function infinite recursion → native stack overflow
// ════════════════════════════════════════════════════════════════════════════════

/// A top-level `fn` that tries to call itself.  Because `exec_fn_body` creates
/// a **fresh isolated environment**, the recursive call sees `UndefinedVariable`
/// rather than recursing.  This is a design property of pure functions (they
/// cannot capture outer scope), but it means the error message is misleading:
/// the user probably intended recursion, not "variable `boom` is not defined".
///
/// This test documents the current behaviour and checks that at least *some*
/// error is returned (not silent success).
#[test]
fn test_pure_fn_self_recursion_produces_error() {
    let src = r#"
fn boom(n: int) -> int {
    return boom(n + 1)
}

const narrator = :{ name: "N", name_color: "white" }
let result = boom(0)
narrator: "{result}"
"#;

    let steps = run_script(src);
    // The call to `boom` inside the body fails because the isolated env
    // doesn't contain `boom`.  We get an UndefinedVariable or similar error.
    assert!(
        first_error(&steps).is_some(),
        "Self-recursive fn should produce an error (pure fns can't see outer scope). Got: {:?}",
        dialogue_texts(&steps)
    );
}

/// Inner function recursion via a locally-defined helper.
/// The inner `fn inner` is stored in the fn env by `exec_fn_stmt`, but when
/// `inner` calls itself, `exec_fn_body` creates a **new** fresh env that
/// does NOT contain `inner`.  So this also fails with UndefinedVariable.
///
/// This documents a limitation: no form of recursion is possible in pure fns.
#[test]
fn test_pure_fn_inner_recursion_produces_error() {
    let src = r#"
fn outer(n: int) -> int {
    fn inner(x: int) -> int {
        if x <= 0 {
            return 0
        }
        return inner(x - 1)
    }
    return inner(n)
}

const narrator = :{ name: "N", name_color: "white" }
let result = outer(5)
narrator: "{result}"
"#;

    let steps = run_script(src);
    // inner(5) → exec_fn_body creates fresh env → inner(4) fails because
    // the new env doesn't have `inner`.
    assert!(
        first_error(&steps).is_some(),
        "Inner fn recursion should produce an error (isolated env). Got: {:?}",
        dialogue_texts(&steps)
    );
}

/// Compiler-level deep nesting: the compiler's `compile_node` and `scan_labels`
/// recurse into AST children with no depth bound.  A 5000-deep if-chain could
/// blow the compiler's stack.
///
/// Runs in a subprocess because stack overflow is SIGABRT.
#[test]
#[allow(clippy::expect_used)]
fn test_compiler_deep_nesting_must_not_crash() {
    if let Some(output) = subprocess_guard("test_compiler_deep_nesting_must_not_crash") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "Compiler blew the stack on 5000-deep if nesting.\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let depth = 5000;
    let mut src = String::new();
    src.push_str("const narrator = :{ name: \"N\", name_color: \"white\" }\n");
    for _ in 0..depth {
        src.push_str("if true {\n");
    }
    src.push_str("narrator: \"deep\"\n");
    for _ in 0..depth {
        src.push_str("}\n");
    }

    if let Ok(ast) = parse_source(&src)
        && let Ok(graph) = Compiler::compile(&ast)
    {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
        let _step = vm.next(None);
        // Any non-crash result is fine.
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category O — Float modulo by zero
// ════════════════════════════════════════════════════════════════════════════════

/// `1.0 % 0.0` — float modulo by zero.  IEEE 754 says this is NaN.
/// The language should return a TypeError, consistent with int modulo by zero.
///
/// Note: adversarial_new.rs tests INT modulo by zero but NOT float modulo.
#[test]
fn test_float_modulo_by_zero_must_error() {
    // Float modulo may or may not be supported.  If it's not, a type error
    // is also acceptable.
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let result = 1.0 % 0.0
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("Float modulo by zero panicked: {panic_msg}");
        }
        Ok(steps) => {
            // If we get a NaN value rendered, that's a bug.
            let texts = dialogue_texts(&steps);
            if let Some(text) = texts.first()
                && (text == "NaN" || text == "nan")
            {
                panic!("float modulo by zero silently produced NaN — should return an error");
            }
            // An error (TypeError) is the correct behaviour.
        }
    }
}

/// `0.0 % 0.0` — also NaN in IEEE 754.
#[test]
fn test_zero_float_modulo_by_zero_must_error() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let result = 0.0 % 0.0
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("0.0 %% 0.0 panicked: {panic_msg}");
        }
        Ok(steps) => {
            let texts = dialogue_texts(&steps);
            if let Some(text) = texts.first()
                && (text == "NaN" || text == "nan")
            {
                panic!("0.0 %% 0.0 silently produced NaN — should return an error");
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category P — `i64::MIN / -1` and `i64::MIN % -1` edge cases
// ════════════════════════════════════════════════════════════════════════════════

/// `i64::MIN / -1` is not representable in two's complement.
/// In hardware, this is a SIGFPE on x86.  Must return a graceful error.
#[test]
fn test_int_min_div_neg_one_must_not_panic() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let min_val = -9223372036854775807 - 1
let result = min_val / -1
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("i64::MIN / -1 panicked (likely hardware SIGFPE): {panic_msg}");
        }
        Ok(steps) => {
            // Should be a graceful overflow error.
            assert!(
                first_error(&steps).is_some(),
                "i64::MIN / -1 should return an overflow error, got: {:?}",
                dialogue_texts(&steps)
            );
        }
    }
}

/// `i64::MIN % -1` is also a hardware fault on some architectures.
/// The existing adversarial_new.rs tests `1 % 0` but not this edge case.
#[test]
fn test_int_min_mod_neg_one_must_not_panic() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let min_val = -9223372036854775807 - 1
let result = min_val % -1
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("i64::MIN %% -1 panicked: {panic_msg}");
        }
        Ok(steps) => {
            // Should either be 0 (mathematically correct) or a graceful error.
            // Anything but a panic is acceptable as long as the contract is clear.
            assert!(
                first_error(&steps).is_some() || dialogue_texts(&steps).contains(&"0".to_string()),
                "i64::MIN %% -1 should return 0 or an overflow error, got: {:?}",
                dialogue_texts(&steps)
            );
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category Q — `coerce_to_i64` silent NaN / ±∞ truncation in list aggregates
// ════════════════════════════════════════════════════════════════════════════════

/// `[NaN].sum()` — coerce_to_i64 converts NaN to 0 silently.
/// Should error since NaN is not a valid integer.
#[test]
fn test_list_sum_with_nan_element_must_error() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let neg = -1.0
let nan = neg.sqrt()
let xs = [nan]
let total = xs.sum()
narrator: "{total}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    if texts.contains(&"0".to_string()) && first_error(&steps).is_none() {
        panic!("list.sum() silently coerced NaN to 0 — should return an error");
    }
}

/// `[1.7976931348623157e308, 1.7976931348623157e308].sum()` — the float values
/// are each close to f64::MAX.  `coerce_to_i64` casts them to i64::MAX
/// (saturating), then sums two i64::MAX values which overflows.
#[test]
fn test_list_sum_with_huge_floats_must_not_silently_wrap() {
    if let Some(output) = subprocess_guard("test_list_sum_with_huge_floats_must_not_silently_wrap")
    {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "list.sum() with huge floats panicked or aborted (coerce_to_i64 \
             saturates to i64::MAX, then unchecked `+=` overflows).\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }

fn huge() -> float {
    return 1.7976931348623157e308
}

let xs = [huge(), huge()]
let total = xs.sum()
narrator: "{total}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    if let Some(text) = texts.first() {
        let val: i64 = text.parse().unwrap_or(0);
        assert!(
            val >= 0 || first_error(&steps).is_some(),
            "list.sum() silently wrapped huge float sum to {val}"
        );
    }
    assert!(
        first_error(&steps).is_some(),
        "list.sum([f64::MAX, f64::MAX]) should return an overflow error, got: {:?}",
        texts
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Category R — Shift operations edge cases
// ════════════════════════════════════════════════════════════════════════════════

/// `i64::MIN << 1` — left-shifting the most negative number.  This discards
/// the sign bit and produces 0 in Rust.  Whether this is a bug depends on
/// intent, but it's inconsistent with checked arithmetic elsewhere.
#[test]
fn test_left_shift_min_value_is_consistent() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let min_val = -9223372036854775807 - 1
let result = min_val << 1
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("Left shift panicked: {panic_msg}");
        }
        Ok(steps) => {
            let texts = dialogue_texts(&steps);
            // Document whatever the current behaviour is.
            // If it's "0", that's the silent bit-loss bug.
            if let Some(text) = texts.first()
                && text == "0"
                && first_error(&steps).is_none()
            {
                // This is technically correct Rust behaviour but
                // unexpected in a scripting language. Document it.
                eprintln!(
                    "NOTE: i64::MIN << 1 silently produces 0 (sign bit lost). \
                     Consider making this an overflow error for consistency."
                );
            }
            // Not asserting failure here — this is a documentation/consistency issue.
        }
    }
}

/// `1 << 63` — produces i64::MIN (the sign bit is set).  This might surprise
/// users expecting unsigned semantics.
#[test]
fn test_left_shift_into_sign_bit() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let result = 1 << 63
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("Left shift panicked: {panic_msg}");
        }
        Ok(steps) => {
            let texts = dialogue_texts(&steps);
            if let Some(text) = texts.first() {
                let val: i64 = text.parse().unwrap_or(0);
                if val == i64::MIN && first_error(&steps).is_none() {
                    eprintln!(
                        "NOTE: 1 << 63 produces i64::MIN ({}). \
                         This is Rust's wrapping left-shift semantics leaking through.",
                        i64::MIN
                    );
                }
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category S — Integer negation overflow: `-(i64::MIN)`
// ════════════════════════════════════════════════════════════════════════════════

/// `-(i64::MIN)` is not representable as i64.  Must not panic.
#[test]
fn test_negate_int_min_must_not_panic() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let x = -9223372036854775807 - 1
let result = -x
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("Negation of i64::MIN panicked: {panic_msg}");
        }
        Ok(steps) => {
            // Either a graceful error or a documented behaviour.
            // A positive i64::MIN (which is impossible) means something went very wrong.
            let texts = dialogue_texts(&steps);
            if first_error(&steps).is_some() {
                // Graceful error — good.
            } else if let Some(text) = texts.first() {
                let val: i64 = text.parse().unwrap_or(0);
                if val == i64::MIN {
                    // -i64::MIN wrapped back to i64::MIN.  This is the bug.
                    panic!(
                        "-(i64::MIN) silently wrapped to i64::MIN — \
                         should return an overflow error"
                    );
                }
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category T — `list.sum()` on floats coerced to int — precision loss
// ════════════════════════════════════════════════════════════════════════════════

/// `[3.7, 2.9].sum()` truncates each element to int before summing.
/// 3.7 → 3, 2.9 → 2, sum = 5.  Mathematically the sum is 6.6.
/// This truncation is at minimum surprising and should be documented.
#[test]
fn test_list_sum_float_truncation_is_surprising() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let xs = [3.7, 2.9]
let total = xs.sum()
narrator: "{total}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // If the result is "5", truncation is happening.
    // If the result is "6" or "6.6", the implementation handles floats natively.
    if texts.contains(&"5".to_string()) {
        eprintln!(
            "NOTE: [3.7, 2.9].sum() returns 5 due to float→int truncation. \
             Consider returning a Float sum when any element is Float."
        );
    }
    // Not a hard failure — but worth documenting.
}

// ════════════════════════════════════════════════════════════════════════════════
// Category U — Unterminated string literal silently accepted by lexer
// ════════════════════════════════════════════════════════════════════════════════

/// `"hello` (no closing quote) should produce a lexer/parser error.
/// Currently the lexer's string callback falls through without error when
/// `ExitString` is never hit, silently accepting the unterminated literal.
#[test]
fn test_unterminated_string_must_be_rejected() {
    let src = "const narrator = :{ name: \"N\", name_color: \"white\" }\nnarrator: \"hello";

    let parse_result = parse_source(src);

    match parse_result {
        Ok(ast) => {
            // If it parsed, try compiling and running — should not succeed silently.
            match Compiler::compile(&ast) {
                Ok(graph) => {
                    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
                    let step = vm.next(None);
                    if let VmStep::Event(Event::Dialogue { lines, .. }) = &step {
                        // If the unterminated string was silently accepted AND produced
                        // dialogue, that's the bug.
                        if !lines.is_empty() {
                            panic!(
                                "Unterminated string literal was silently accepted and produced \
                                 dialogue: {:?}",
                                lines
                            );
                        }
                    }
                }
                Err(_) => {
                    // Compilation error is acceptable — caught downstream.
                }
            }
        }
        Err(_) => {
            // Parse error is the correct behaviour.
        }
    }
}

/// `"hello\"` — escaped quote at end, no real closing quote.
#[test]
#[allow(clippy::expect_used)]
fn test_escaped_quote_at_eof_must_be_rejected() {
    // Use a raw string so Rust doesn't interpret the escapes
    let src = r#"const narrator = :{ name: "N", name_color: "white" }
narrator: "hello\""#;

    // This is trickier: the `\"` is an escaped quote inside the string,
    // but there's no closing `"`.  The lexer should detect EOF without ExitString.
    let parse_result = parse_source(src);

    // Getting a parse error is correct.
    // Getting Ok but the string eating the rest of the file is the bug.
    if let Ok(ast) = parse_result
        && let Ok(graph) = Compiler::compile(&ast)
    {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
        let step = vm.next(None);
        if let VmStep::Event(Event::Dialogue { lines, .. }) = &step {
            for line in lines {
                if let RuntimeValue::Str(ps) = line {
                    let s = ps.to_string();
                    // If the string contains everything after the opening quote
                    // (including what should be separate code), it ate the file.
                    if s.len() > 20 {
                        panic!(
                            "Escaped quote at EOF caused the string to eat the rest of \
                             the file: {s:?}"
                        );
                    }
                }
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category V — Deeply nested parentheses → parser stack overflow
// ════════════════════════════════════════════════════════════════════════════════

/// 5000 nested parentheses: `(((((...1...)))))`.
/// Should either parse successfully or return a graceful error.
/// Must NOT segfault due to parser stack overflow.
///
/// Runs in a subprocess because stack overflow is SIGABRT, not a catchable panic.
#[test]
#[allow(clippy::expect_used)]
fn test_deeply_nested_parens_must_not_crash() {
    if let Some(output) = subprocess_guard("test_deeply_nested_parens_must_not_crash") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "Deeply nested parentheses (5000 levels) caused a crash — \
             likely parser stack overflow.\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let depth = 5000;
    let mut src = String::new();
    src.push_str("const narrator = :{ name: \"N\", name_color: \"white\" }\n");
    src.push_str("let x = ");
    for _ in 0..depth {
        src.push('(');
    }
    src.push('1');
    for _ in 0..depth {
        src.push(')');
    }
    src.push_str("\nnarrator: \"{x}\"\n");

    // If we crash here the parent detects it.
    if let Ok(ast) = parse_source(&src)
        && let Ok(graph) = Compiler::compile(&ast)
    {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
        let step = vm.next(None);
        if let VmStep::Event(Event::Dialogue { lines, .. }) = &step
            && let Some(RuntimeValue::Str(ps)) = lines.first()
        {
            assert_eq!(
                ps.to_string(),
                "1",
                "Deeply nested parens should evaluate to 1"
            );
        }
    }
}

/// 5000 nested braces: `{ { { ... 1 ... } } }`.
///
/// Runs in a subprocess because stack overflow is SIGABRT, not a catchable panic.
#[test]
#[allow(clippy::expect_used)]
fn test_deeply_nested_braces_must_not_crash() {
    if let Some(output) = subprocess_guard("test_deeply_nested_braces_must_not_crash") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success(),
            "Deeply nested braces (5000 levels) caused a crash — \
             likely parser/compiler stack overflow.\nstderr: {stderr}"
        );
        return;
    }

    // Subprocess body
    let depth = 5000;
    let mut src = String::new();
    src.push_str("const narrator = :{ name: \"N\", name_color: \"white\" }\n");
    for _ in 0..depth {
        src.push_str("if true {\n");
    }
    src.push_str("narrator: \"deep\"\n");
    for _ in 0..depth {
        src.push_str("}\n");
    }

    // If we crash here the parent detects it.
    if let Ok(ast) = parse_source(&src)
        && let Ok(graph) = Compiler::compile(&ast)
    {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
        for _ in 0..2 {
            let _ = vm.next(None);
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category W — `int.pow(0)` boundary and `0.pow(0)` semantics
// ════════════════════════════════════════════════════════════════════════════════

/// `x.pow(0)` should return 1 for any non-zero x (mathematical convention).
#[test]
fn test_int_pow_zero_exponent() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let a = 42
let result_a = a.pow(0)
let b = -7
let result_b = b.pow(0)
narrator: "{result_a}"
narrator: "{result_b}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    assert!(
        texts.contains(&"1".to_string()),
        "x.pow(0) should return 1, got: {texts:?}"
    );
    assert_eq!(
        texts.iter().filter(|t| t.as_str() == "1").count(),
        2,
        "Both 42.pow(0) and (-7).pow(0) should return 1, got: {texts:?}"
    );
}

/// `0.pow(0)` — mathematically controversial, but most languages return 1.
#[test]
fn test_zero_pow_zero() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let base = 0
let result = base.pow(0)
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // Most languages (Python, JS, C) define 0^0 = 1.
    // An error is also acceptable but should be documented.
    assert!(
        texts.contains(&"1".to_string()) || first_error(&steps).is_some(),
        "0.pow(0) should return 1 or a documented error, got: {texts:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Category X — Mixed-type division edge cases
// ════════════════════════════════════════════════════════════════════════════════

/// `1 / 0.0` — Int divided by Float(0.0).  The `to_float` coercion path
/// should still guard against division by zero.
#[test]
fn test_int_div_float_zero_must_error() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let result = 1 / 0.0
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("Int / Float(0.0) panicked: {panic_msg}");
        }
        Ok(steps) => {
            let texts = dialogue_texts(&steps);
            // "Infinity" or "inf" means the zero guard is missing in the coercion path.
            if let Some(text) = texts.first() {
                let lower = text.to_lowercase();
                if lower.contains("inf") {
                    panic!("1 / 0.0 silently produced {text} — should return an error");
                }
            }
        }
    }
}

/// `1.0 / 0` — Float divided by Int(0).  Same coercion path, opposite direction.
#[test]
fn test_float_div_int_zero_must_error() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let result = 1.0 / 0
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("Float / Int(0) panicked: {panic_msg}");
        }
        Ok(steps) => {
            let texts = dialogue_texts(&steps);
            if let Some(text) = texts.first() {
                let lower = text.to_lowercase();
                if lower.contains("inf") {
                    panic!("1.0 / 0 silently produced {text} — should return an error");
                }
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Category Y — `str.repeat()` boundary behaviour
// ════════════════════════════════════════════════════════════════════════════════

/// `"abc".repeat(0)` should return an empty string.
#[test]
fn test_str_repeat_zero() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let s = "abc"
let result = s.repeat(0)
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    assert!(
        texts.contains(&String::new()),
        "str.repeat(0) should return empty string, got: {texts:?}"
    );
}

/// `"abc".repeat(1)` should return the same string.
#[test]
fn test_str_repeat_one() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let s = "abc"
let result = s.repeat(1)
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    assert!(
        texts.contains(&"abc".to_string()),
        "str.repeat(1) should return 'abc', got: {texts:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Category Z — Empty list aggregate methods
// ════════════════════════════════════════════════════════════════════════════════

/// `[].min()` — empty list min.  Should return null or an error, not panic.
#[test]
fn test_empty_list_min() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let xs = []
let result = xs.min()
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("[].min() panicked: {panic_msg}");
        }
        Ok(steps) => {
            // null or error are both acceptable for empty list min.
            let texts = dialogue_texts(&steps);
            assert!(
                texts.contains(&"null".to_string())
                    || first_error(&steps).is_some()
                    // Some implementations use "Null" with a capital N
                    || texts.iter().any(|t| t.to_lowercase() == "null"),
                "[].min() should return null or an error, got: {texts:?}"
            );
        }
    }
}

/// `[].max()` — same as min but for max.
#[test]
fn test_empty_list_max() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let xs = []
let result = xs.max()
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("[].max() panicked: {panic_msg}");
        }
        Ok(steps) => {
            let texts = dialogue_texts(&steps);
            assert!(
                texts.contains(&"null".to_string())
                    || first_error(&steps).is_some()
                    || texts.iter().any(|t| t.to_lowercase() == "null"),
                "[].max() should return null or an error, got: {texts:?}"
            );
        }
    }
}

/// `[].sum()` — empty sum should return 0 (identity element).
#[test]
fn test_empty_list_sum() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let xs = []
let result = xs.sum()
narrator: "{result}"
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    assert!(
        texts.contains(&"0".to_string()),
        "[].sum() should return 0, got: {texts:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Bonus: NaN propagation through arithmetic chains
// ════════════════════════════════════════════════════════════════════════════════

/// Once NaN enters the system (via sqrt(-1)), it should not silently propagate
/// through arithmetic without ever being caught.  The user should be told
/// something is wrong, not get silent garbage.
#[test]
fn test_nan_propagation_through_arithmetic_chain() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let neg = -1.0
let nan = neg.sqrt()
let a = nan + 1.0
let b = a * 2.0
let c = b - 3.0

if c == c {
    narrator: "equal"
} else {
    narrator: "not equal"
}
"#;

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // NaN != NaN (IEEE 754), so `c == c` is false.
    // The fact that `c` is NaN silently propagated through 3 arithmetic ops
    // and only manifests as `c != c` is the bug.
    if texts.contains(&"not equal".to_string()) && first_error(&steps).is_none() {
        eprintln!(
            "NOTE: NaN silently propagated through 3 arithmetic operations \
             (sqrt → add → mul → sub) without any error.  Consider trapping NaN \
             production at the source (sqrt of negative, 0/0, etc.)."
        );
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Bonus: Integer floor-division overflow: i64::MIN // -1
// ════════════════════════════════════════════════════════════════════════════════

/// `i64::MIN // -1` — floor division overflow, same root cause as `i64::MIN / -1`.
#[test]
fn test_int_min_floordiv_neg_one_must_not_panic() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
let min_val = -9223372036854775807 - 1
let result = min_val // -1
narrator: "{result}"
"#;

    let result = run_script_catching_panic(src);

    match result {
        Err(panic_msg) => {
            panic!("i64::MIN // -1 panicked: {panic_msg}");
        }
        Ok(steps) => {
            assert!(
                first_error(&steps).is_some(),
                "i64::MIN // -1 should return an overflow error, got: {:?}",
                dialogue_texts(&steps)
            );
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Bonus: Excessively long identifier / string token (DoS via lexer allocation)
// ════════════════════════════════════════════════════════════════════════════════

/// A 100,000-character identifier should either parse (slowly) or error
/// gracefully.  Must not OOM-crash the process.
#[test]
fn test_very_long_identifier_does_not_crash() {
    let long_ident: String = std::iter::once('x')
        .chain(std::iter::repeat_n('a', 99_999))
        .collect();

    let src = format!(
        "const narrator = :{{ name: \"N\", name_color: \"white\" }}\nlet {} = 42\nnarrator: \"ok\"\n",
        long_ident
    );

    let result = catch_unwind(AssertUnwindSafe(|| match parse_source(&src) {
        Ok(ast) => match Compiler::compile(&ast) {
            Ok(graph) => {
                let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
                let step = vm.next(None);
                Some(step)
            }
            Err(_) => None,
        },
        Err(_) => None,
    }));

    assert!(result.is_ok(), "100,000-char identifier caused a crash");
}

/// A 100,000-character string literal should parse without crashing.
#[test]
fn test_very_long_string_literal_does_not_crash() {
    let long_str: String = std::iter::repeat_n('a', 100_000).collect();

    let src = format!(
        "const narrator = :{{ name: \"N\", name_color: \"white\" }}\nnarrator: \"{}\"\n",
        long_str
    );

    let result = catch_unwind(AssertUnwindSafe(|| match parse_source(&src) {
        Ok(ast) => match Compiler::compile(&ast) {
            Ok(graph) => {
                let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
                let step = vm.next(None);
                Some(step)
            }
            Err(_) => None,
        },
        Err(_) => None,
    }));

    assert!(result.is_ok(), "100,000-char string literal caused a crash");
}

// ════════════════════════════════════════════════════════════════════════════════
// Bonus: BOM (Byte Order Mark) at start of file
// ════════════════════════════════════════════════════════════════════════════════

/// A UTF-8 BOM (U+FEFF) at the start of a file is common on Windows.
/// Should be silently stripped or produce a clear error, not a cryptic
/// lexer failure.
#[test]
fn test_bom_at_file_start_is_handled() {
    let src =
        "\u{FEFF}const narrator = :{ name: \"N\", name_color: \"white\" }\nnarrator: \"hello\"\n";

    let result = catch_unwind(AssertUnwindSafe(|| parse_source(src)));

    match result {
        Err(_) => {
            panic!("BOM at file start caused a panic in the lexer");
        }
        Ok(Ok(_ast)) => {
            // Successfully parsed — BOM was stripped.  Good.
        }
        Ok(Err(errors)) => {
            // Check if the error is helpful.
            let msg = format!("{errors:?}");
            if msg.contains("unexpected") || msg.contains("unknown") {
                eprintln!(
                    "NOTE: BOM at file start produces a cryptic error: {msg}\n\
                     Consider stripping U+FEFF from the beginning of source input."
                );
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Bonus: Multiple assignment to const in same scope
// ════════════════════════════════════════════════════════════════════════════════

/// Assigning to a const should produce a clear error at the assignment point.
#[test]
fn test_const_reassignment_produces_error() {
    let src = r#"
const narrator = :{ name: "N", name_color: "white" }
const x = 10
x = 20
narrator: "{x}"
"#;

    let steps = run_script(src);

    // Should get an error about const reassignment.
    assert!(
        first_error(&steps).is_some(),
        "Reassigning to a const should produce an error, got: {:?}",
        dialogue_texts(&steps)
    );
}

// ════════════════════════════════════════════════════════════════════════════════
// Bonus: Integer comparison near i64 boundaries
// ════════════════════════════════════════════════════════════════════════════════

/// `i64::MAX > i64::MIN` — should be true.
#[test]
fn test_extreme_int_comparison() {
    let src = &format!(
        r#"
const narrator = :{{ name: "N", name_color: "white" }}
let min_val = -9223372036854775807 - 1
if {} > min_val {{
    narrator: "correct"
}} else {{
    narrator: "wrong"
}}
"#,
        i64::MAX
    );

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    assert!(
        texts.contains(&"correct".to_string()),
        "i64::MAX > i64::MIN should be true, got: {texts:?}"
    );
}

/// `i64::MAX == i64::MAX` — reflexivity at the boundary.
#[test]
fn test_int_max_equality_reflexive() {
    let src = &format!(
        r#"
const narrator = :{{ name: "N", name_color: "white" }}
let x = {}
if x == x {{
    narrator: "equal"
}} else {{
    narrator: "not equal"
}}
"#,
        i64::MAX
    );

    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    assert!(
        texts.contains(&"equal".to_string()),
        "i64::MAX == i64::MAX should be true, got: {texts:?}"
    );
}
