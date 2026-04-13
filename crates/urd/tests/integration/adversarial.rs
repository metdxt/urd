//! # Adversarial tests for the Urd VM
//!
//! These tests are **designed to expose known bugs** in the implementation.
//! Each test asserts the *correct* expected behavior, and will FAIL against
//! the current implementation to demonstrate a real defect.
//!
//! Bugs documented here:
//! 1. `//` (floor-div) uses `div_euclid` for Int operands but `.floor()` for
//!    Float — inconsistent semantics for the same operator.
//! 2. Cross-type `Int == Float` comparison casts `i64` to `f64`, losing bits
//!    for integers > 2^53.
//! 3. `Map` values are never equal under `==`, even with identical content —
//!    value-type semantics violated, undocumented.
//! 4. `fn` bodies run in a completely isolated `Environment::new()` — cannot
//!    read `global` variables from the outer script; silently errors at runtime.
//! 5. 2-segment `IdentPath` resolution (`a.b`) falls back to returning `a`'s
//!    value when `b` doesn't exist on `a` — field-access typos are silent.
//! 6. String interpolation `{a.b}` falls back to the bare name `b` — an
//!    entirely unrelated variable of the right name is silently used.

#![allow(missing_docs)]

use urd::{
    Event, RuntimeValue, VmError, VmStep,
    analysis::AnalysisError,
    compiler::loader::parse_source,
};

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion (or the first error/termination),
/// returning every [`VmStep`] observed.  Capped at 128 steps to prevent infinite
/// loops in broken scripts.
fn run_script(src: &str) -> Vec<VmStep> {
    super::fixtures::run_script(src, 128)
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
    for step in steps {
        if let VmStep::Error(e) = step {
            return Some(e);
        }
    }
    None
}

// ── Tests ─────────────────────────────────────────────────────────────────────

/// ## BUG: `//` for `Int // Int` uses Euclidean division, not floor division.
///
/// `(-7) // (-2)` with integer operands calls [`i64::div_euclid`], which
/// returns `4` because `-7 = (-2) * 4 + 1` satisfies the Euclidean remainder
/// constraint `0 ≤ r < |divisor|`.
///
/// Python (and most languages that have a floor-division operator) define `//`
/// as `floor(a / b)`, which for `(-7) / (-2) = 3.5` gives `3`.
///
/// The Float path in `numeric_floordiv` correctly uses `(a / b).floor()` and
/// therefore returns `3.0` for `(-7.0) // (-2.0)`.  The two code paths
/// implement **different mathematical operations** behind the same operator.
///
/// ### Correct expected result
/// `(-7) // (-2)` should be `3` (floor), matching Python semantics and the
/// Float implementation of the same operator.
///
/// ### Why this test FAILS
/// The VM currently evaluates the expression to `4` (Euclidean), so the `if`
/// takes the `"euclidean"` branch and the dialogue emitted is `["euclidean"]`,
/// not `["floor"]`.
#[test]
fn test_floordiv_int_int_uses_euclid_not_floor() {
    let src = r#"
@entry
label start {
    let result = -7 // -2
    if result == 3 {
        Narrator: "floor"
    } elif result == 4 {
        Narrator: "euclidean"
    } else {
        Narrator: "unexpected"
    }
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // Correct floor-division semantics: floor(-7 / -2) = floor(3.5) = 3.
    // This assertion will FAIL: the VM emits ["euclidean"] because it calls
    // div_euclid instead of floor division.
    assert_eq!(
        texts,
        vec!["floor"],
        "(-7) // (-2) should yield 3 under floor semantics; \
         got {texts:?} — the Int path uses div_euclid instead of floor"
    );
}

/// ## BUG: `//` produces inconsistent results for identical Int vs Float operands.
///
/// Both `(-7) // (-2)` and `(-7.0) // (-2.0)` represent the same mathematical
/// floor-division expression.  They should produce the same numeric result.
/// Instead:
///
/// - `Int // Int` calls [`i64::div_euclid`]  → `Int(4)`
/// - `Float // Float` calls `(a / b).floor()` → `Float(3.0)`
///
/// When these values are compared with `==`, the cross-type arm of
/// `values_equal` casts the integer to `f64`: `(4i64 as f64) == 3.0` → `false`.
/// The observable effect is that `int_result == float_result` evaluates to
/// `false` even though both operands encoded the same real-valued division.
///
/// ### Correct expected result
/// A floor-division operator with a consistent definition should produce `3`
/// for both pairs of operands; `int_result == float_result` should be `true`.
///
/// ### Why this test FAILS
/// The VM emits `["inconsistent floor_div"]` because `Int(4) != Float(3.0)`.
#[test]
fn test_floordiv_int_int_and_float_float_give_different_results() {
    let src = r#"
@entry
label start {
    let int_result = -7 // -2
    let float_result = -7.0 // -2.0
    if int_result == float_result {
        Narrator: "consistent"
    } else {
        Narrator: "inconsistent floor_div"
    }
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // Both expressions should yield the same value under any consistent
    // definition of floor-division.  This assertion FAILS: the VM returns
    // Int(4) for the integer case and Float(3.0) for the float case.
    assert_eq!(
        texts,
        vec!["consistent"],
        "(-7) // (-2) and (-7.0) // (-2.0) should agree; \
         got {texts:?} — Int uses div_euclid (→4), Float uses floor (→3.0)"
    );
}

/// ## BUG: `Int == Float` comparison loses bits for integers larger than 2^53.
///
/// `values_equal` handles the cross-type case with:
///
/// ```urd/crates/urd/src/vm/eval.rs#L781-782
/// (RuntimeValue::Int(x), RuntimeValue::Float(y)) => (*x as f64) == *y,
/// ```
///
/// Casting an `i64` to `f64` is a lossy operation for values whose magnitude
/// exceeds 2^53 (the f64 mantissa width).  In particular,
/// `9_007_199_254_740_993i64` (= 2^53 + 1) rounds to `9_007_199_254_740_992.0`
/// when stored as `f64`, making it indistinguishable from `9_007_199_254_740_992i64`.
///
/// The script therefore tests a comparison that **should** be `false` —
/// `9007199254740993 != 9007199254740992.0` — but the cast makes both sides
/// equal at the `f64` level.
///
/// ### Correct expected result
/// The two values are mathematically distinct integers; `==` should return
/// `false` and the script should emit `"correctly not equal"`.
///
/// ### Why this test FAILS
/// The VM casts the `Int` to `f64` before comparing, producing `true`, and
/// the dialogue emitted is `["falsely equal"]`.
#[test]
fn test_int_float_equality_precision_loss() {
    let src = r#"
@entry
label start {
    let big: int = 9007199254740993
    let small_f: float = 9007199254740992.0
    if big == small_f {
        Narrator: "falsely equal"
    } else {
        Narrator: "correctly not equal"
    }
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // 9007199254740993 (2^53 + 1) != 9007199254740992.0 (2^53) — they differ
    // by exactly 1.  This assertion FAILS: the VM casts the Int to f64 first,
    // losing the low-order bit, and the comparison returns true.
    assert_eq!(
        texts,
        vec!["correctly not equal"],
        "9007199254740993 and 9007199254740992.0 are distinct values; \
         got {texts:?} — i64-to-f64 cast drops the +1 bit, making them appear equal"
    );
}

/// ## BUG: Two `Map` values with identical content never compare equal under `==`.
///
/// `values_equal` exhaustively matches known value pairs, but `Map` is not
/// listed.  The trailing `_ => false` catch-all handles it instead, so any
/// two maps — including structurally identical ones — always compare as
/// **not equal**.
///
/// This violates value-type semantics: if maps are compared by content (as
/// integers, strings, and lists are), identical maps must be equal.  If maps
/// are intended to be compared by identity, that constraint should be
/// documented and the equality operator should not be usable on them at all
/// (ideally a type error).  Silently returning `false` is the worst of both
/// worlds.
///
/// ### Correct expected result
/// `:{score: 42} == :{score: 42}` should evaluate to `true` under value
/// semantics.  The script should emit `"equal"`.
///
/// ### Why this test FAILS
/// The `_ => false` arm in `values_equal` swallows the Map case.  The script
/// emits `["not equal"]`.
#[test]
fn test_map_equality_always_false_for_identical_maps() {
    let src = r#"
@entry
label start {
    let a = :{ score: 42 }
    let b = :{ score: 42 }
    if a == b {
        Narrator: "equal"
    } else {
        Narrator: "not equal"
    }
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);

    // Two maps with identical structure and values should be equal under value
    // semantics.  This assertion FAILS: values_equal has no Map arm and the
    // _ => false catch-all always returns false for maps.
    assert_eq!(
        texts,
        vec!["equal"],
        "two maps with identical content should compare equal; \
         got {texts:?} — the _ => false catch-all in values_equal swallows Map comparisons"
    );
}

/// ## FIX-5: `fn` bodies are intentionally pure — globals must not be visible
/// inside a `fn` body, and the `undefined_var` analysis pass must catch this
/// statically rather than letting the VM crash at runtime.
///
/// Before the fix, `undefined_var` included all top-level names (globals,
/// consts, externs) as "in scope everywhere" and never modelled `fn` body
/// isolation.  A `fn` that read a global produced no analysis error; the
/// compiler emitted IR; the VM crashed with `VmError::UndefinedVariable`.
///
/// After the fix, `check_node` handles `AstContent::FnDef` by building an
/// isolated scope containing only the declared parameters, fn-body locals,
/// builtins, and type/variant names.  Globals, consts, and externs are
/// deliberately excluded.  The analysis pass therefore flags `score` as
/// undefined inside the fn body before any IR is generated.
///
/// ### Verified behaviour
/// `urd::analysis::analyze` returns an `AnalysisError::UndefinedVariable`
/// whose `name` field is `"score"`.
#[test]
fn test_fn_body_cannot_read_global_variable() {
    let src = r#"
global score: int = 100

fn get_score() -> int {
    return score
}

@entry
label start {
    let result = get_score()
    Narrator: "score retrieved"
    end!()
}
"#;
    let ast = parse_source(src).expect("script should parse");

    // The undefined_var analysis pass must flag `score` as undefined inside
    // the fn body (fn bodies are isolated — they cannot see globals).
    let errors = urd::analysis::analyze(&ast);
    let has_score_error = errors
        .iter()
        .any(|e| matches!(e, AnalysisError::UndefinedVariable { name, .. } if name == "score"));
    assert!(
        has_score_error,
        "analysis should flag `score` as undefined inside the fn body \
         (fn bodies are pure and cannot access outer globals); \
         got errors: {errors:?}"
    );
}

/// ## BUG: Accessing a nonexistent field via a 2-segment `IdentPath` silently
/// returns the **first segment's value** instead of an error.
///
/// When the VM resolves `IdentPath(["counter", "nonexistent"])` it tries, in
/// order:
///
/// 1. Enum-variant lookup: `env.get_enum_variant("counter", "nonexistent")` — fails.
/// 2. Module-namespaced lookup: `env.get("counter::nonexistent")` — fails.
/// 3. Struct-field access: `env.get("counter")` returns `Int(42)`, which is
///    not a `Struct`, so the pattern-guard fails.
/// 4. **Fallback**: `env.get("counter")` is tried a second time (the legacy
///    first-segment lookup).  This succeeds, returning `Int(42)`.
///
/// The returned value is silently assigned to `x`.  No error is raised, no
/// warning is logged.  A typo in a field name (`counter.nonexstent`) goes
/// completely undetected and produces a value that has nothing to do with
/// the intended access.
///
/// ### Correct expected result
/// Attempting to read a field that does not exist on a non-struct value should
/// yield `VmStep::Error(VmError::UndefinedVariable("counter.nonexistent"))`.
///
/// ### Why this test FAILS
/// The fallback `env.get(&path[0])` succeeds and the expression quietly
/// evaluates to `42`.  The dialogue `"silently returned counter value"` is
/// emitted, no error step is present in the sequence.
#[test]
fn test_two_segment_path_fallback_silently_returns_first_segment() {
    let src = r#"
@entry
label start {
    let counter: int = 42
    let x = counter.nonexistent
    if x == 42 {
        Narrator: "silently returned counter value"
    } else {
        Narrator: "got expected error or different value"
    }
    end!()
}
"#;
    let steps = run_script(src);

    // Correct behaviour: accessing a nonexistent field on a non-struct value
    // should propagate VmError::UndefinedVariable("counter.nonexistent").
    // This assertion FAILS: the fallback path in eval_runtime_value returns
    // env.get("counter") = Int(42), so no error is ever produced.
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "reading `counter.nonexistent` on a non-struct Int should produce an error; \
         got none — the VM silently returned `counter`'s own value (42) instead; \
         steps: {steps:?}"
    );

    if let Some(VmError::UndefinedVariable(name)) = err {
        assert_eq!(
            name, "counter.nonexistent",
            "the error should name the full dotted path; got '{name}'"
        );
    } else if let Some(other) = err {
        panic!("expected VmError::UndefinedVariable(\"counter.nonexistent\"), got {other:?}");
    }
}

/// ## BUG: String interpolation `{inv.gold}` silently resolves to an entirely
/// unrelated variable whose name matches the **second** path segment.
///
/// `interpolate_string` resolves a 2-segment placeholder `{inv.gold}` through
/// three attempts:
///
/// 1. `env.get_enum_variant("inv", "gold")` — fails (no enum named `inv`).
/// 2. `env.get("inv::gold")` — fails (no module-namespaced variable).
/// 3. **Fallback**: `env.get("gold")` (the **second** segment in isolation).
///
/// If a variable named `gold` happens to exist in scope — even though it has
/// nothing semantically to do with `inv.gold` — its value is silently
/// substituted.  The author intended to read a field of `inv`; they get a
/// completely different variable with no indication that anything went wrong.
///
/// The comment in the source describes this as "merged globals from imported
/// modules live in the flat env under their original name", which is a valid
/// use-case for *imported* names, but the fallback is applied indiscriminately
/// and will misfire on any same-named local variable.
///
/// ### Correct expected result
/// When `inv` is not a module import and `inv.gold` cannot be resolved, the
/// interpolation should either:
/// (a) preserve the placeholder as-is: `"You have {inv.gold} gold"`, or
/// (b) emit `VmStep::Error` indicating the unresolvable path.
///
/// ### Why this test FAILS
/// `env.get("gold")` succeeds (returning `Int(999)`), so the text becomes
/// `"You have 999 gold"` — the wrong variable's value is silently used.
#[test]
fn test_string_interpolation_fallback_uses_unrelated_variable() {
    let src = r#"
@entry
label start {
    let gold: int = 999
    let inv: str = "wallet"
    Narrator: "You have {inv.gold} gold"
    end!()
}
"#;
    let steps = run_script(src);

    // Correct behaviour: {inv.gold} cannot be resolved to a struct field or
    // module-namespaced variable, so the placeholder should be preserved or
    // an error emitted.  This assertion FAILS: the fallback path tries
    // env.get("gold") (the second segment), finds the unrelated local variable
    // `gold = 999`, and substitutes it silently.
    let texts = dialogue_texts(&steps);

    // Either the placeholder is preserved intact …
    let placeholder_preserved = texts == vec!["You have {inv.gold} gold"];
    // … or execution was aborted by an error.
    let got_error = first_error(&steps).is_some();

    assert!(
        placeholder_preserved || got_error,
        "interpolation of an unresolvable path `{{inv.gold}}` should preserve \
         the placeholder or raise an error; instead the unrelated variable \
         `gold = 999` was silently substituted, producing: {texts:?}"
    );
}
