//! # Regression tests for the Urd VM
//!
//! These tests are **designed to prevent regressions of known bugs** in the implementation.
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

/// `//` (floor-div) uses true floor division for both Int and Float.
///
/// `(-7) // (-2)` with integer operands evaluates to `3`, matching Python
/// and `(-7.0) // (-2.0)`.
#[test]
fn test_floordiv_int_int_is_correct() {
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

    assert_eq!(
        texts,
        vec!["floor"],
        "(-7) // (-2) should yield 3 under floor semantics"
    );
}

/// `//` produces consistent results for identical Int vs Float operands.
#[test]
fn test_floordiv_int_int_and_float_float_give_consistent_results() {
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

    assert_eq!(
        texts,
        vec!["consistent"],
        "(-7) // (-2) and (-7.0) // (-2.0) should agree"
    );
}

/// `Int == Float` comparison correctly handles integers larger than 2^53.
#[test]
fn test_int_float_equality_precision_preservation() {
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

    assert_eq!(
        texts,
        vec!["correctly not equal"],
        "9007199254740993 and 9007199254740992.0 are distinct values"
    );
}

/// Two `Map` values with identical content compare equal under `==`.
#[test]
fn test_map_equality_for_identical_maps() {
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

    assert_eq!(
        texts,
        vec!["equal"],
        "two maps with identical content should compare equal"
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

/// ## FIXED: Accessing a nonexistent field via a 2-segment `IdentPath` silently
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
    // This assertion passes: the fallback path in eval_runtime_value returns
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

/// ## FIXED: String interpolation `{inv.gold}` silently resolves to an entirely
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
    // an error emitted.  This assertion passes: the fallback path tries
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

#[test]
fn test_cyclic_collections_do_not_stack_overflow() {
    let src = r#"
@entry
label start {
    let a = []
    a.push(a)
    let s = "{a}"
    if a == a {
        Narrator: "equal"
    }
    
    let m = {}
    m["self"] = m
    if m == m {
        Narrator: "m equal"
    }
    
    end!()
}
"#;
    let steps = run_script(src);
    // Should parse and format cleanly as `[...]` without blowing the stack.
    let end = steps.last().expect("expected steps");
    assert!(matches!(end, VmStep::Ended), "expected Ended, got {end:?}");
}
