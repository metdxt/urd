//! # New Adversarial Tests — Un-patched Bugs
//!
//! This module documents **newly discovered defects** that are NOT covered by
//! the existing `adversarial.rs` suite (whose bugs have since been fixed and
//! are now regression tests).
//!
//! Each test asserts the **correct** expected behaviour.  Where the current
//! implementation is wrong the test **FAILS**.  Where the implementation is
//! right but untested, the test is a canary against regressions.
//!
//! ## Confirmed Un-patched Bugs
//!
//! | # | Bug | Root cause |
//! |---|-----|------------|
//! | A | `1 % 0` panics (Rust abort in debug) instead of returning `VmError::TypeError` | `numeric_int_binop` has no zero-divisor guard for `%` |
//! | B | `1.0 / 0.0` silently produces `Float(Infinity)` | Float path in `numeric_div` has no zero-divisor guard |
//! | C | `0.0 / 0.0` silently produces `Float(NaN)` whose `x == x` is `false` | Same root as B; violates equality reflexivity |
//! | D | `1.0 // 0.0` and `0.0 // 0.0` silently produce Infinity / NaN | Float path in `numeric_floordiv` has no zero guard |
//! | E | `i64::MAX + 1`, `i64::MAX * 2`, `i64::MIN - 2` panic in debug mode | `numeric_binop` uses plain arithmetic with no overflow check |
//! | F | `match x {}` (0 arms) and non-exhaustive match without wildcard silently terminate the VM | Switch node with no matching arm sets `cursor = None` |
//! | G | `Float(NaN)` is coerced to integer `0` in range-match patterns | Range-arm scalar extraction uses unchecked `NaN as i64 = 0` |
//! | H | `match` inside a `fn` body fails at runtime with a misleading `InvalidExpression` error | `exec_fn_stmt` falls to `eval_expr` for `Match` nodes |

#![allow(missing_docs)]

use std::panic::{AssertUnwindSafe, catch_unwind};

use urd::{
    Event, RuntimeValue, VmError, VmStep,
    compiler::{Compiler, CompilerError, loader::parse_source},
    vm::{Vm, registry::DecoratorRegistry},
};

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion or the first terminal step.
/// Capped at 256 steps to prevent infinite loops.
fn run_script(src: &str) -> Vec<VmStep> {
    let ast = parse_source(src).expect("script should parse");
    let graph = Compiler::compile(&ast).expect("script should compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm should initialise");
    let mut steps = Vec::new();
    for _ in 0..256 {
        let step = vm.next(None);
        let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
        steps.push(step);
        if terminal {
            break;
        }
    }
    steps
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

// ════════════════════════════════════════════════════════════════════════════
// BUG A — `Int % 0` panics instead of returning `VmError::TypeError`
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG A: `1 % 0` panics at the Rust level instead of returning a `VmError`.
///
/// `numeric_int_binop` applies the closure `|a, b| a % b` directly without
/// checking whether `b == 0`.  `1i64 % 0i64` triggers Rust's arithmetic panic
/// ("attempt to calculate the remainder with a divisor of zero") in debug
/// builds and is undefined behaviour in release builds.
///
/// By contrast, `numeric_div` (the `/` operator) *does* guard against zero:
///
/// ```text
/// if *b == 0 { return Err(VmError::TypeError("integer division by zero")) }
/// ```
///
/// The `%` operator has no equivalent guard — a direct inconsistency.
///
/// ### Correct expected behaviour
/// `1 % 0` must return `VmStep::Error(VmError::TypeError(...))`.
///
/// ### Why this test FAILS
/// `vm.next()` panics.  `catch_unwind` catches the panic and the test
/// re-panics with a descriptive BUG message.
#[test]
fn test_int_modulo_by_zero_panics_instead_of_erroring() {
    let src = r#"
@entry
label start {
    let x = 1 % 0
    Narrator: "unreachable"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");

    let result = catch_unwind(AssertUnwindSafe(move || {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm");
        let mut steps = Vec::new();
        for _ in 0..32 {
            let step = vm.next(None);
            let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
            steps.push(step);
            if terminal {
                break;
            }
        }
        steps
    }));

    match result {
        Err(_panic_payload) => {
            panic!(
                "BUG A: `1 % 0` caused a Rust panic instead of returning \
                 VmStep::Error(VmError::TypeError). \
                 Fix: add `if *b == 0 {{ return Err(VmError::TypeError(...)) }}` \
                 in numeric_int_binop when the operation is Percent, \
                 mirroring the guard already present in numeric_div."
            );
        }
        Ok(steps) => {
            assert!(
                matches!(first_error(&steps), Some(VmError::TypeError(_))),
                "expected VmStep::Error(VmError::TypeError) for `1 % 0`, got: {steps:?}"
            );
        }
    }
}

/// ## BUG A (zero dividend): `0 % 0` must also be caught.
///
/// Every non-zero or zero value `% 0` panics; this variant confirms the guard
/// must trigger for any divisor of zero regardless of the dividend.
#[test]
fn test_zero_modulo_zero_panics_instead_of_erroring() {
    let src = r#"
@entry
label start {
    let x = 0 % 0
    Narrator: "unreachable"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");

    let result = catch_unwind(AssertUnwindSafe(move || {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm");
        let mut steps = Vec::new();
        for _ in 0..32 {
            let step = vm.next(None);
            let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
            steps.push(step);
            if terminal {
                break;
            }
        }
        steps
    }));

    match result {
        Err(_) => {
            panic!("BUG A: `0 % 0` caused a Rust panic. Should return VmError::TypeError.");
        }
        Ok(steps) => {
            assert!(
                matches!(first_error(&steps), Some(VmError::TypeError(_))),
                "expected VmError::TypeError for `0 % 0`, got: {steps:?}"
            );
        }
    }
}

/// ## Boundary: negative modulo by -1 must return `0`, not error.
///
/// `i64::MIN % -1` would trigger an x86 hardware fault (the CPU computes
/// quotient and remainder together, and the quotient overflows), so we use
/// `-100 % -1 = 0` as the representative case.  Any zero-divisor guard added
/// for BUG A must NOT fire for `b == -1`; only `b == 0` is forbidden.
#[test]
fn test_int_modulo_by_neg_one_is_zero_not_error() {
    // -100 % -1 = 0: safe on all platforms, no overflow, divisor ≠ 0.
    let src = r#"
@entry
label start {
    let result = -100 % -1
    if result == 0 {
        Narrator: "zero"
    } else {
        Narrator: "nonzero"
    }
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");

    // This is a regular run — no panic expected.
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    assert_eq!(
        texts,
        vec!["zero"],
        "-100 % -1 must equal 0 (any zero-divisor guard must only fire for b == 0); \
         got {texts:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════
// BUG B — `Float / 0.0` silently produces IEEE Infinity
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG B: `1.0 / 0.0` silently produces `Float(Infinity)`, not an error.
///
/// `numeric_div` for the `(Float, Float)` arm computes `a / b` with no guard:
///
/// ```text
/// (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Ok(RuntimeValue::Float(a / b)),
/// ```
///
/// `1.0f64 / 0.0f64` is `f64::INFINITY`.  The VM continues with Infinity in
/// scope, silently corrupting all subsequent arithmetic and comparisons.
///
/// The `Int / Int` arm **does** guard against zero division.  The `Float / Float`
/// arm has no equivalent protection — a direct inconsistency.
///
/// ### Correct expected behaviour
/// `1.0 / 0.0` must return `VmStep::Error(VmError::TypeError(...))`.
///
/// ### Why this test FAILS
/// No error is emitted; `x > 9999999.0` evaluates as `Infinity > 9999999.0 = true`
/// and `"got infinity"` is emitted with no preceding error step.
#[test]
fn test_float_div_by_zero_silently_produces_infinity() {
    let src = r#"
@entry
label start {
    let x = 1.0 / 0.0
    if x > 9999999.0 {
        Narrator: "got infinity"
    } else {
        Narrator: "got finite or errored"
    }
    end!()
}
"#;
    let steps = run_script(src);

    assert!(
        first_error(&steps).is_some(),
        "BUG B: `1.0 / 0.0` must produce VmStep::Error, not silently continue. \
         Fix: add a `b == 0.0` guard in the Float arm of numeric_div. \
         Got: {steps:?}"
    );
    assert!(
        matches!(first_error(&steps), Some(VmError::TypeError(_))),
        "expected VmError::TypeError for float division by zero, got: {:?}",
        first_error(&steps)
    );
}

/// ## BUG B (mixed types): `1 / 0.0` (Int dividend, Float zero divisor) also
/// silently produces Infinity.
///
/// When one operand is Int and the other is Float, `numeric_div` falls to the
/// catch-all arm that converts both via `to_float` and then divides:
/// `1.0f64 / 0.0f64 = Infinity`.  The zero guard is again absent.
#[test]
fn test_int_divided_by_float_zero_silently_produces_infinity() {
    let src = r#"
@entry
label start {
    let zero_f: float = 0.0
    let result = 1 / zero_f
    if result > 9999999.0 {
        Narrator: "got infinity from int/float"
    } else {
        Narrator: "got finite or errored"
    }
    end!()
}
"#;
    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "BUG B (mixed): `1 / 0.0` (Int/Float) must produce VmStep::Error, not Infinity. \
         Got: {steps:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════
// BUG C — `0.0 / 0.0` silently produces NaN; NaN != NaN violates reflexivity
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG C: `0.0 / 0.0` silently produces `Float(NaN)`, breaking `x == x`.
///
/// `0.0f64 / 0.0f64 = f64::NAN`.  `values_equal` for `(Float, Float)` uses
/// `x == y` — IEEE 754 semantics — so `NaN == NaN` is `false`.
///
/// The expression `nan_val == nan_val` therefore evaluates to `false`, violating
/// the mathematical axiom that every value is equal to itself.  Scripts that
/// assume `x == x` will silently misbehave.
///
/// ### Correct expected behaviour
/// Option A: `0.0 / 0.0` returns `VmStep::Error`.
/// Option B: `nan_val == nan_val` is `true` (identity-equality special case).
///
/// ### Why this test FAILS
/// The VM produces NaN without error and `nan_val == nan_val` is `false`,
/// so `"NaN is not self-equal"` is emitted.
#[test]
fn test_nan_from_zero_div_zero_violates_equality_reflexivity() {
    let src = r#"
@entry
label start {
    let nan_val = 0.0 / 0.0
    if nan_val == nan_val {
        Narrator: "self-equal (correct)"
    } else {
        Narrator: "NaN is not self-equal (reflexivity violated)"
    }
    end!()
}
"#;
    let steps = run_script(src);

    let errored = first_error(&steps).is_some();
    let reflexivity_holds = dialogue_texts(&steps) == vec!["self-equal (correct)"];

    assert!(
        errored || reflexivity_holds,
        "BUG C: `0.0 / 0.0` must either error or preserve equality reflexivity. \
         Got: {steps:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════
// BUG D — `Float // 0.0` silently produces Infinity or NaN
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG D: `1.0 // 0.0` silently produces `Float(Infinity)`.
///
/// `numeric_floordiv` guards the `Int // Int` case:
///
/// ```text
/// if *b == 0 { return Err(VmError::TypeError("floor division by zero")) }
/// ```
///
/// The fallback `Float` path — reached for any mixed or float operands —
/// computes `(a / b).floor()` with no zero check:
/// `(1.0f64 / 0.0f64).floor() = Infinity.floor() = Infinity`.
///
/// ### Correct expected behaviour
/// `1.0 // 0.0` must return `VmStep::Error(VmError::TypeError(...))`.
///
/// ### Why this test FAILS
/// Infinity propagates; the `if` condition is true and `"got infinity"` is emitted.
#[test]
fn test_float_floordiv_by_zero_silently_produces_infinity() {
    let src = r#"
@entry
label start {
    let x = 1.0 // 0.0
    if x > 9999999.0 {
        Narrator: "got infinity from floordiv"
    } else {
        Narrator: "got finite or errored"
    }
    end!()
}
"#;
    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "BUG D: `1.0 // 0.0` must produce VmStep::Error, not Infinity. \
         Got: {steps:?}"
    );
    assert!(
        matches!(first_error(&steps), Some(VmError::TypeError(_))),
        "expected VmError::TypeError for float floor-division by zero, got: {:?}",
        first_error(&steps)
    );
}

/// ## BUG D (0 / 0 variant): `0.0 // 0.0` silently produces `Float(NaN)`.
///
/// `(0.0f64 / 0.0f64).floor() = NaN.floor() = NaN`.  Like BUG C, this then
/// violates equality reflexivity and is silently falsy in conditions.
#[test]
fn test_zero_floordiv_zero_silently_produces_nan() {
    let src = r#"
@entry
label start {
    let x = 0.0 // 0.0
    if x == x {
        Narrator: "self-equal"
    } else {
        Narrator: "NaN from 0.0 // 0.0 — not self-equal"
    }
    end!()
}
"#;
    let steps = run_script(src);

    let errored = first_error(&steps).is_some();
    let self_equal = dialogue_texts(&steps) == vec!["self-equal"];

    assert!(
        errored || self_equal,
        "BUG D: `0.0 // 0.0` must either error or the result must satisfy `x == x`. \
         Got: {steps:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════
// BUG E — Integer arithmetic overflow panics in debug mode
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG E: `i64::MAX + 1` panics with Rust arithmetic overflow.
///
/// `numeric_binop` for the `(Int, Int)` case applies the closure
/// `int_op(*a, *b)` = `a + b` using the plain Rust `+` operator.  In debug
/// mode Rust inserts overflow checks, so `i64::MAX + 1` triggers
/// "attempt to add with overflow".  In release mode it silently wraps to
/// `i64::MIN`.
///
/// `eval_unary` was already fixed to use `checked_neg()`.  The same protection
/// must be applied to binary arithmetic via `checked_add`, `checked_sub`,
/// and `checked_mul`.
///
/// ### Correct expected behaviour
/// `i64::MAX + 1` must return `VmStep::Error(VmError::TypeError(...))`.
///
/// ### Why this test FAILS (debug build)
/// The closure panics.  `catch_unwind` catches the panic and this test
/// re-panics with a descriptive BUG message.
#[test]
fn test_integer_addition_overflow_panics_instead_of_erroring() {
    // 9223372036854775807 = i64::MAX
    let src = r#"
@entry
label start {
    let big = 9223372036854775807
    let boom = big + 1
    Narrator: "unreachable"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");

    let result = catch_unwind(AssertUnwindSafe(move || {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm");
        let mut steps = Vec::new();
        for _ in 0..32 {
            let step = vm.next(None);
            let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
            steps.push(step);
            if terminal {
                break;
            }
        }
        steps
    }));

    match result {
        Err(_) => {
            panic!(
                "BUG E: `i64::MAX + 1` caused a Rust panic (arithmetic overflow) instead of \
                 returning VmStep::Error(VmError::TypeError). \
                 Fix: replace `int_op(*a, *b)` in numeric_binop's Int arm with \
                 checked_add / checked_sub / checked_mul, mapping None to VmError::TypeError, \
                 mirroring the existing checked_neg fix in eval_unary."
            );
        }
        Ok(steps) => {
            assert!(
                matches!(first_error(&steps), Some(VmError::TypeError(_))),
                "expected VmError::TypeError for i64::MAX + 1, got: {steps:?}"
            );
        }
    }
}

/// ## BUG E (multiplication): `i64::MAX * 2` panics in debug mode.
#[test]
fn test_integer_multiplication_overflow_panics_instead_of_erroring() {
    let src = r#"
@entry
label start {
    let big = 9223372036854775807
    let boom = big * 2
    Narrator: "unreachable"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");

    let result = catch_unwind(AssertUnwindSafe(move || {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm");
        let mut steps = Vec::new();
        for _ in 0..32 {
            let step = vm.next(None);
            let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
            steps.push(step);
            if terminal {
                break;
            }
        }
        steps
    }));

    match result {
        Err(_) => {
            panic!(
                "BUG E: `i64::MAX * 2` caused a Rust panic (multiplication overflow) instead of \
                 returning VmStep::Error(VmError::TypeError). \
                 Fix: use checked_mul in numeric_binop's Int arm."
            );
        }
        Ok(steps) => {
            assert!(
                matches!(first_error(&steps), Some(VmError::TypeError(_))),
                "expected VmError::TypeError for i64::MAX * 2, got: {steps:?}"
            );
        }
    }
}

/// ## BUG E (subtraction underflow): `-(i64::MAX) - 2` underflows in debug.
///
/// `-9223372036854775807` is `-(i64::MAX)`, computed safely via `checked_neg`.
/// Subtracting 2 from that yields `i64::MIN - 1`, which is not representable
/// in `i64` and panics.
#[test]
fn test_integer_subtraction_underflow_panics_instead_of_erroring() {
    let src = r#"
@entry
label start {
    let near_min = -9223372036854775807
    let boom = near_min - 2
    Narrator: "unreachable"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");

    let result = catch_unwind(AssertUnwindSafe(move || {
        let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm");
        let mut steps = Vec::new();
        for _ in 0..32 {
            let step = vm.next(None);
            let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
            steps.push(step);
            if terminal {
                break;
            }
        }
        steps
    }));

    match result {
        Err(_) => {
            panic!(
                "BUG E: integer subtraction underflow caused a Rust panic instead of \
                 returning VmStep::Error(VmError::TypeError). \
                 Fix: use checked_sub in numeric_binop's Int arm."
            );
        }
        Ok(steps) => {
            assert!(
                matches!(first_error(&steps), Some(VmError::TypeError(_))),
                "expected VmError::TypeError for integer underflow, got: {steps:?}"
            );
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════
// BUG F — Match with no matching arm silently terminates the VM
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG F1: `match x {}` (zero arms) silently terminates the VM without error.
///
/// The parser accepts an empty arms list.  The compiler emits a `Switch` node
/// with no outgoing `Arm(i)` edges and no `Default` (wildcard) edge.
///
/// At runtime the VM evaluates the Switch, finds no matching arm, searches for
/// a `Default` edge (none exists), and sets `state.cursor = None`.  All
/// subsequent instructions — including `Narrator: "after match"` and `end!()` —
/// are **silently skipped**.  The VM returns `VmStep::Ended` without emitting
/// them, making it appear as though the script ended successfully when it
/// actually fell off an internal edge.
///
/// ### Correct expected behaviour
/// Either:
/// (a) `VmStep::Error` — non-exhaustive match, OR
/// (b) Fall-through to the next statement.
///
/// ### Why this test FAILS
/// The VM ends before `Narrator: "after match"` runs; no error is emitted.
#[test]
fn test_match_zero_arms_silently_terminates_vm() {
    let src = r#"
@entry
label start {
    let x = 42
    match x {}
    Narrator: "after match"
    end!()
}
"#;
    let steps = run_script(src);

    let errored = first_error(&steps).is_some();
    let reached = dialogue_texts(&steps).contains(&"after match".to_string());

    assert!(
        errored || reached,
        "BUG F1: `match x {{}}` (0 arms) silently terminated the VM. \
         Expected VmStep::Error OR `Narrator: \"after match\"` to run. \
         Got: {steps:?}"
    );
}

/// ## BUG F2: Non-exhaustive value arms without a wildcard silently terminate.
///
/// When the scrutinee (`x = 99`) does not match any listed value arm (`1`, `2`,
/// `3`) and there is no wildcard, the VM applies the same silent-termination
/// path as BUG F1.
///
/// This is particularly insidious for runtime values: a script author who adds
/// a new enum variant and forgets the corresponding arm gets silent VM
/// truncation rather than a recoverable error.
///
/// ### Why this test FAILS
/// x = 99 matches none of the arms; `Narrator: "after match"` is never reached.
#[test]
fn test_match_non_exhaustive_value_arms_silently_terminate_vm() {
    let src = r#"
@entry
label start {
    let x = 99
    match x {
        1 { Narrator: "one" }
        2 { Narrator: "two" }
        3 { Narrator: "three" }
    }
    Narrator: "after match"
    end!()
}
"#;
    let steps = run_script(src);

    let errored = first_error(&steps).is_some();
    let reached = dialogue_texts(&steps).contains(&"after match".to_string());

    assert!(
        errored || reached,
        "BUG F2: non-exhaustive match (x=99, no wildcard) silently terminated the VM. \
         Expected VmStep::Error OR fall-through to \"after match\". \
         Got: {steps:?}"
    );
}

/// ## BUG F2 (range variant): Non-exhaustive range arms without wildcard.
///
/// `score = 50` falls outside both covered ranges (1–10, 90–100) and there is
/// no wildcard.  Same silent-termination path as the value variant above.
#[test]
fn test_match_non_exhaustive_range_arms_silently_terminate_vm() {
    let src = r#"
@entry
label start {
    let score = 50
    match score {
        1..=10   { Narrator: "low" }
        90..=100 { Narrator: "high" }
    }
    Narrator: "after match"
    end!()
}
"#;
    let steps = run_script(src);

    let errored = first_error(&steps).is_some();
    let reached = dialogue_texts(&steps).contains(&"after match".to_string());

    assert!(
        errored || reached,
        "BUG F2 (range): score=50 fell outside all range arms with no wildcard; \
         VM silently terminated. Expected VmStep::Error OR fall-through. \
         Got: {steps:?}"
    );
}

// ════════════════════════════════════════════════════════════════════════════
// BUG G — NaN coerced to integer 0 in range-match patterns
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG G: `Float(NaN)` is silently treated as integer `0` in range-match patterns.
///
/// In the VM's `Switch` handler, the range-pattern arm extracts a scalar from
/// the scrutinee with:
///
/// ```text
/// RuntimeValue::Float(f) => Some(*f as i64),
/// ```
///
/// Rust's float-to-integer cast uses **saturating semantics** since Rust 1.45:
/// `f64::NAN as i64 = 0`.  A `Float(NaN)` scrutinee is therefore treated as
/// `Int(0)` for range matching, causing `Float(NaN)` to match `0..=0`.
///
/// Compare with Value-pattern matching: `values_equal(Float(NaN), Int(0))` is
/// `false` (because `NaN.fract() = NaN` triggers the early-false guard in
/// `values_equal`).  So NaN does NOT match the value pattern `0`, but it DOES
/// match the range pattern `0..=0` — an internal inconsistency.
///
/// ### Correct expected behaviour
/// `Float(NaN)` must not match any integer range; the wildcard arm fires.
///
/// ### Why this test FAILS
/// `NaN as i64 = 0`; `0 in 0..=0 = true`; the range arm fires and
/// `"nan wrongly matched zero range"` is emitted.
#[test]
fn test_nan_matches_zero_in_range_pattern_but_should_not() {
    let src = r#"
@entry
label start {
    let nan_val = 0.0 / 0.0
    match nan_val {
        0..=0 { Narrator: "nan wrongly matched zero range" }
        _ { Narrator: "correctly did not match any integer range" }
    }
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    let errored = first_error(&steps).is_some();

    // Two acceptable outcomes:
    // 1. `0.0 / 0.0` is now a TypeError (Phase 3a), so the script errors before
    //    the match is reached — NaN still never matched 0..=0. Correct.
    // 2. If NaN somehow reaches the match (future language paths), the wildcard
    //    arm fires instead of the 0..=0 arm — also correct (Phase 3b NaN guard).
    assert!(
        errored || texts == vec!["correctly did not match any integer range"],
        "BUG G: Float(NaN) was coerced to 0 via `NaN as i64` and matched `0..=0`. \
         Fix: add `if f.is_nan() {{ return (false, None); }}` before the float-to-int \
         cast in the Switch handler's Range arm. Got: texts={texts:?}, errored={errored}"
    );
}

/// ## BUG G (asymmetry): NaN does NOT match the value pattern `0` but DOES
/// match the range `0..=0` — a direct internal inconsistency.
///
/// Both scripts use the same NaN scrutinee but use different pattern kinds.
/// They must produce the same result; today they diverge.
#[test]
fn test_nan_value_pattern_and_range_pattern_must_agree() {
    let value_src = r#"
@entry
label start {
    let nan_val = 0.0 / 0.0
    match nan_val {
        0 { Narrator: "matched 0" }
        _ { Narrator: "no match" }
    }
    end!()
}
"#;

    let range_src = r#"
@entry
label start {
    let nan_val = 0.0 / 0.0
    match nan_val {
        0..=0 { Narrator: "matched 0" }
        _ { Narrator: "no match" }
    }
    end!()
}
"#;

    let value_steps = run_script(value_src);
    let range_steps = run_script(range_src);

    let value_texts = dialogue_texts(&value_steps);
    let range_texts = dialogue_texts(&range_steps);

    // Both must agree.  If float-div-by-zero is fixed (BUG B), both produce
    // empty texts (errored) — still consistent.
    assert_eq!(
        value_texts, range_texts,
        "BUG G (asymmetry): value-pattern match on NaN gave {value_texts:?} but \
         range-pattern match gave {range_texts:?}. \
         Both must treat NaN identically — one is lying."
    );
}

// ════════════════════════════════════════════════════════════════════════════
// BUG H — `match` inside `fn` bodies is unsupported; gives misleading error
// ════════════════════════════════════════════════════════════════════════════

/// ## BUG H: `match` in a `fn` body fails at runtime with a misleading error.
///
/// `exec_fn_stmt` handles `return`, `if`, `let`/`const`, assignment, and
/// inner `fn` definitions.  `match` is NOT handled — it falls through to:
///
/// ```text
/// _ => eval_expr(stmt, env).map(FnExecResult::Normal),
/// ```
///
/// `eval_expr` explicitly rejects `Match` nodes:
///
/// ```text
/// AstContent::Match { .. } => Err(VmError::InvalidExpression(
///     "Match cannot appear in expression context".to_string(),
/// )),
/// ```
///
/// The message "expression context" is **misleading**: the match IS in a
/// statement context (inside a fn body); the real problem is that
/// `exec_fn_stmt` simply omitted the `Match` arm.
///
/// Static analysis does NOT catch this: `check_node` for `FnDef` only checks
/// variable references, not statement-type validity.  The script compiles
/// successfully; the error surfaces only when the fn is called at runtime.
///
/// ### Correct expected behaviour
/// `classify(1)` returns `"one"` and the VM emits `Narrator: "one"`.
///
/// ### Why this test FAILS
/// When `classify(1)` is called, `exec_fn_stmt` hits the `_ =>` arm for the
/// Match node, `eval_expr` returns `InvalidExpression`, and the VM emits
/// `VmStep::Error` instead of the dialogue.
#[test]
fn test_match_in_fn_body_fails_at_runtime_with_misleading_error() {
    // No `-> int` annotation to avoid the `-> str` parse gap; the fn still
    // returns an Int at runtime when the match works correctly.
    let src = r#"
fn classify(n: int) {
    match n {
        1 { return 10 }
        2 { return 20 }
        _ { return 0 }
    }
}

@entry
label start {
    let result = classify(1)
    if result == 10 {
        Narrator: "match worked"
    } else {
        Narrator: "match failed or errored"
    }
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    assert_eq!(
        texts,
        vec!["match worked"],
        "BUG H: `match` inside a `fn` body should work but failed at runtime. \
         Expected [\"match worked\"]; error (if any): {:?}; steps: {steps:?}. \
         Fix: add an `AstContent::Match {{ .. }}` arm to `exec_fn_stmt` that \
         mirrors the Switch-node handler in the top-level VM loop.",
        first_error(&steps)
    );
}

/// ## BUG H (baseline): `if` inside `fn` body works; `match` does not.
///
/// `exec_fn_stmt` handles `AstContent::If` correctly.  This test documents
/// the working baseline so the `match` omission stands out clearly.
/// It should **pass** today; a future regression in `if` handling would also
/// surface here.
#[test]
fn test_if_in_fn_body_works_baseline() {
    // No `-> int` annotation (avoids the `-> str` parse issue) — the fn still
    // returns an Int when the if/else logic works correctly.
    let src = r#"
fn double_if(n: int) {
    if n == 5 {
        return 10
    } else {
        return 0
    }
}

@entry
label start {
    let r = double_if(5)
    if r == 10 {
        Narrator: "if worked"
    } else {
        Narrator: "if failed"
    }
    end!()
}
"#;
    let steps = run_script(src);
    // This PASSES — if-in-fn-body is supported.  Its passage makes the
    // match omission (BUG H) harder to justify.
    assert_eq!(
        dialogue_texts(&steps),
        vec!["if worked"],
        "baseline: `if` in fn body must work; got {:?}",
        dialogue_texts(&steps)
    );
}

// ════════════════════════════════════════════════════════════════════════════
// CANARY — Correct behaviours that are currently untested
// ════════════════════════════════════════════════════════════════════════════

/// An empty script must compile and produce `VmStep::Ended` immediately.
#[test]
fn test_empty_script_compiles_and_ends_immediately() {
    let ast = parse_source("").expect("empty string must parse");
    let graph = Compiler::compile(&ast).expect("empty script must compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
    assert!(
        matches!(vm.next(None), VmStep::Ended),
        "empty script must end on the first next() call"
    );
}

/// A whitespace-only script must compile and end immediately.
#[test]
fn test_whitespace_only_script_compiles_and_ends_immediately() {
    let ast = parse_source("   \n\n\t\n  ").expect("whitespace-only must parse");
    let graph = Compiler::compile(&ast).expect("whitespace-only must compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
    assert!(
        matches!(vm.next(None), VmStep::Ended),
        "whitespace-only script must end on the first next() call"
    );
}

/// `\uD800` is a Unicode surrogate code point (range U+D800–U+DFFF).
/// Surrogates are not valid Unicode scalar values; `char::from_u32(0xD800)`
/// returns `None`.  The lexer must reject this escape.
///
/// ### Known defect in the error message
/// The current error text uses the **decimal** representation of the code point
/// (`"55296"`) rather than the hex form (`"\\uD800"`), which is confusing.
#[test]
fn test_unicode_surrogate_escape_is_a_lex_error() {
    // Raw string to avoid Rust-level escape interpretation.
    let src = "let s = \"hello \\uD800 world\"";
    assert!(
        parse_source(src).is_err(),
        "\\uD800 (a surrogate code point) must be a lex error"
    );
}

/// `\u{110000}` exceeds the maximum valid Unicode code point (U+10FFFF = 1 114 111).
/// `char::from_u32(0x110000)` returns `None`; the lexer must reject it.
///
/// ### Known defect in the error message
/// The current error text uses decimal ("1114112") rather than hex ("110000").
#[test]
fn test_overlong_unicode_brace_escape_is_a_lex_error() {
    let src = "let s = \"too big: \\u{110000}\"";
    assert!(
        parse_source(src).is_err(),
        "\\u{{110000}} (code point > U+10FFFF) must be a lex error"
    );
}

/// The maximum valid code point `\u{10FFFF}` must parse successfully.
/// This is a boundary canary complementing the overlong escape test above.
#[test]
fn test_max_valid_unicode_brace_escape_parses_ok() {
    let src = "@entry\nlabel start {\n    let s = \"\\u{10FFFF}\"\n    end!()\n}";
    assert!(
        parse_source(src).is_ok(),
        "\\u{{10FFFF}} (maximum valid code point) must parse successfully"
    );
}

/// `\x00` (null byte) is a valid two-hex-digit escape and must produce a
/// null character (`'\0'`) in the string, not an error.
#[test]
fn test_null_byte_escape_is_valid() {
    let src = "@entry\nlabel start {\n    let s = \"\\x00\"\n    end!()\n}";
    assert!(
        parse_source(src).is_ok(),
        "\\x00 (null byte escape) must be a valid string literal"
    );
}

/// ## QUIRK — Empty string `""` is truthy (all `Str` values are).
///
/// `is_truthy` for `Str` falls to the `_ => true` catch-all, so every string —
/// including `""` — is truthy.  This differs from Python, JavaScript, and most
/// scripting languages where `""` is falsy.
///
/// This test **documents** the current behaviour.  If the design changes,
/// update the assertion and the `is_truthy` doc comment together.
#[test]
fn test_empty_string_is_truthy_current_behaviour() {
    let src = r#"
@entry
label start {
    let empty = ""
    if empty {
        Narrator: "truthy"
    } else {
        Narrator: "falsy"
    }
    end!()
}
"#;
    let steps = run_script(src);
    assert_eq!(
        dialogue_texts(&steps),
        vec!["truthy"],
        "current behaviour: empty string is truthy (all Str fall to `_ => true` in \
         is_truthy). Update this assertion if the design intentionally changes."
    );
}

/// ## CANARY — Duplicate label names are rejected by the compiler itself.
///
/// `scan_label_nops` checks `labels.contains_key(label)` and returns
/// `CompilerError::DuplicateLabel`.  This guards against the second label
/// silently overwriting the first in the pre-allocated Nop map.
#[test]
fn test_compiler_rejects_duplicate_label_names() {
    let src = r#"
label start {
    end!()
}

label start {
    Narrator: "I am a duplicate"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let result = Compiler::compile(&ast);

    assert!(
        matches!(result, Err(CompilerError::DuplicateLabel(ref n)) if n == "start"),
        "expected CompilerError::DuplicateLabel(\"start\"); got: {result:?}"
    );
}

/// ## CANARY — Importing the same file under two different aliases works.
///
/// The BFS loader deduplicates modules by path: `lib.urd` is compiled once.
/// `build_global_labels` maps both `a::lib_label` and `b::lib_label` to the
/// same `NodeIndex`.  Jumping to each alias should reach the same IR node.
///
/// This test verifies the compiler and VM handle the diamond-import case
/// without duplicate-emission errors or label collisions.
#[test]
fn test_same_file_imported_under_two_aliases_resolves_correctly() {
    use urd::{
        compiler::Compiler,
        vm::{Vm, loader::MemLoader},
    };

    let mut loader = MemLoader::new();
    loader.add(
        "lib.urd",
        r#"
label shared {
    Sys: "from lib"
}
"#,
    );

    let main_src = r#"
import "lib.urd" as a
import "lib.urd" as b

@entry
label main {
    jump a.shared
}
"#;

    use urd::{parse_test, parser::block::script, runtime::value::RuntimeValue};

    let ast = parse_test!(script(), main_src).expect("main script must parse");

    let graph = Compiler::compile_with_loader(&ast, &loader)
        .expect("compile_with_loader must not error for diamond import");

    // Both aliases must be registered in the label map.
    assert!(
        graph.labels.contains_key("a::shared"),
        "a::shared must be in graph.labels; got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    assert!(
        graph.labels.contains_key("b::shared"),
        "b::shared must be in graph.labels; got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );

    // Both aliases must point to the same IR node (deduplication).
    assert_eq!(
        graph.labels["a::shared"], graph.labels["b::shared"],
        "a::shared and b::shared must reference the same IR node"
    );

    // Execute via alias `a` and verify the dialogue fires.
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");
    let mut lines: Vec<String> = Vec::new();
    loop {
        match vm.next(None) {
            VmStep::Ended => break,
            VmStep::Event(Event::Dialogue { lines: ls, .. }) => {
                for v in &ls {
                    if let RuntimeValue::Str(ps) = v {
                        lines.push(ps.to_string());
                    }
                }
            }
            VmStep::Error(e) => panic!("VM error: {e}"),
            VmStep::Event(_) => {}
        }
    }
    assert_eq!(
        lines,
        vec!["from lib"],
        "expected dialogue from lib.urd via alias a; got: {lines:?}"
    );
}
