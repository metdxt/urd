//! # Integration tests: `in` operator & pure functions
//!
//! End-to-end tests that exercise the `in` operator (range, list, map, string)
//! and pure function definitions through the full parse → compile → VM pipeline.
//! Each test drives the VM to completion and inspects dialogue output or errors.

#![allow(missing_docs)]

use urd::{
    Event, RuntimeValue, VmError, VmStep,
    compiler::loader::parse_source,
};

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion (or first terminal step).
/// Capped at 1024 steps to prevent infinite loops in broken scripts.
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

// ═══════════════════════════════════════════════════════════════════════════════
//  `in` operator — Range
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn in_range_exclusive_contains_interior() {
    let src = r#"
@entry
label start {
    let result = 3 in 0..5
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_range_exclusive_excludes_upper_bound() {
    let src = r#"
@entry
label start {
    let result = 5 in 0..5
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_range_exclusive_includes_lower_bound() {
    let src = r#"
@entry
label start {
    let result = 0 in 0..5
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_range_exclusive_rejects_below_start() {
    let src = r#"
@entry
label start {
    let result = -1 in 0..5
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_range_inclusive_includes_upper_bound() {
    let src = r#"
@entry
label start {
    let result = 5 in 0..=5
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_range_inclusive_rejects_above_upper_bound() {
    let src = r#"
@entry
label start {
    let result = 6 in 0..=5
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_range_empty_exclusive_range_never_contains() {
    // 5..5 is an empty range — nothing should be "in" it.
    let src = r#"
@entry
label start {
    let result = 5 in 5..5
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  `in` operator — List
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn in_list_found() {
    let src = r#"
@entry
label start {
    let result = 2 in [1, 2, 3]
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_list_not_found() {
    let src = r#"
@entry
label start {
    let result = 4 in [1, 2, 3]
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_list_empty_list_never_contains() {
    let src = r#"
@entry
label start {
    let result = 1 in []
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_list_string_element() {
    let src = r#"
@entry
label start {
    let result = "b" in ["a", "b", "c"]
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_list_bool_element() {
    let src = r#"
@entry
label start {
    let result = true in [false, true]
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  `in` operator — Map
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn in_map_key_found() {
    let src = r#"
@entry
label start {
    let result = "key" in :{key: 1, other: 2}
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_map_key_not_found() {
    let src = r#"
@entry
label start {
    let result = "missing" in :{key: 1}
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_map_empty_map_never_contains() {
    let src = r#"
@entry
label start {
    let result = "x" in :{}
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_map_int_lhs_errors() {
    // Maps require a Str key on the left-hand side.
    let src = r#"
@entry
label start {
    let result = 42 in :{key: 1}
    Narrator: "should not reach"
    end!()
}
"#;
    let steps = run_script(src);
    let err = first_error(&steps);
    assert!(
        matches!(err, Some(VmError::TypeError(_))),
        "expected VmError::TypeError when using Int as LHS for `in` on Map; got: {err:?}"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  `in` operator — String
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn in_string_substring_found() {
    let src = r#"
@entry
label start {
    let result = "ell" in "hello"
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_string_substring_not_found() {
    let src = r#"
@entry
label start {
    let result = "xyz" in "hello"
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_string_empty_needle_always_found() {
    // An empty string is a substring of every string.
    let src = r#"
@entry
label start {
    let result = "" in "hello"
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_string_empty_haystack_not_found() {
    let src = r#"
@entry
label start {
    let result = "a" in ""
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["not found"]);
}

#[test]
fn in_string_empty_both_found() {
    // "" in "" → true  (Rust's `"".contains("")` is true)
    let src = r#"
@entry
label start {
    let result = "" in ""
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_string_int_lhs_errors() {
    // Strings require a Str on the LHS.
    let src = r#"
@entry
label start {
    let result = 42 in "hello"
    Narrator: "should not reach"
    end!()
}
"#;
    let steps = run_script(src);
    let err = first_error(&steps);
    assert!(
        matches!(err, Some(VmError::TypeError(_))),
        "expected VmError::TypeError for Int `in` Str; got: {err:?}"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  `in` operator — wrong RHS type
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn in_rhs_int_errors() {
    let src = r#"
@entry
label start {
    let result = 3 in 5
    Narrator: "should not reach"
    end!()
}
"#;
    let steps = run_script(src);
    let err = first_error(&steps);
    assert!(
        matches!(err, Some(VmError::TypeError(_))),
        "expected VmError::TypeError when RHS of `in` is a bare Int; got: {err:?}"
    );
}

#[test]
fn in_rhs_bool_errors() {
    let src = r#"
@entry
label start {
    let result = true in false
    Narrator: "should not reach"
    end!()
}
"#;
    let steps = run_script(src);
    let err = first_error(&steps);
    assert!(
        matches!(err, Some(VmError::TypeError(_))),
        "expected VmError::TypeError when RHS of `in` is Bool; got: {err:?}"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  `in` operator — combined with `not`
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn not_in_list() {
    let src = r#"
@entry
label start {
    let result = not (3 in [1, 2])
    if result {
        Narrator: "absent"
    } else {
        Narrator: "present"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["absent"]);
}

#[test]
fn not_in_list_present() {
    let src = r#"
@entry
label start {
    let result = not (2 in [1, 2, 3])
    if result {
        Narrator: "absent"
    } else {
        Narrator: "present"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["present"]);
}

#[test]
fn in_range_wrong_lhs_type_errors() {
    // Range requires Int on the LHS.
    let src = r#"
@entry
label start {
    let result = "hello" in 0..5
    Narrator: "should not reach"
    end!()
}
"#;
    let steps = run_script(src);
    let err = first_error(&steps);
    assert!(
        matches!(err, Some(VmError::TypeError(_))),
        "expected VmError::TypeError for Str `in` Range; got: {err:?}"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Pure functions — basic definition and call
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_basic_add() {
    let src = r#"
fn add(a, b) {
    return a + b
}

@entry
label start {
    let x = add(1, 2)
    Narrator: "{x}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["3"]);
}

#[test]
fn fn_with_return_type_annotation() {
    let src = r#"
fn double(x) -> int {
    return x * 2
}

@entry
label start {
    let r = double(7)
    Narrator: "{r}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["14"]);
}

#[test]
fn fn_returns_string() {
    let src = r#"
fn greet() {
    return "hello world"
}

@entry
label start {
    let msg = greet()
    Narrator: "{msg}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["hello world"]);
}

#[test]
fn fn_returns_bool() {
    let src = r#"
fn is_positive(n) {
    return n > 0
}

@entry
label start {
    let r = is_positive(5)
    if r {
        Narrator: "positive"
    } else {
        Narrator: "non-positive"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["positive"]);
}

#[test]
fn fn_no_parameters() {
    let src = r#"
fn greeting() {
    return "hello"
}

@entry
label start {
    let msg = greeting()
    Narrator: "{msg}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["hello"]);
}

#[test]
fn fn_multiple_parameters() {
    let src = r#"
fn clamp(val, lo, hi) {
    if val < lo {
        return lo
    }
    if val > hi {
        return hi
    }
    return val
}

@entry
label start {
    let a = clamp(-5, 0, 10)
    let b = clamp(7, 0, 10)
    let c = clamp(20, 0, 10)
    Narrator: "{a}"
    Narrator: "{b}"
    Narrator: "{c}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["0", "7", "10"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Pure functions — calling other functions
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_calls_another_fn() {
    // NOTE: Pure fn bodies are isolated and cannot see other top-level
    // functions.  This should error at runtime because `square` is not
    // visible inside `sum_of_squares`.  If the implementation changes to
    // allow it, update this test accordingly.
    let src = r#"
fn square(x) {
    return x * x
}

fn sum_of_squares(a, b) {
    return square(a) + square(b)
}

@entry
label start {
    let r = sum_of_squares(3, 4)
    Narrator: "{r}"
    end!()
}
"#;
    let steps = run_script(src);
    // The VM currently does NOT support fn-to-fn calls because pure
    // functions run in isolated environments — `square` is not visible
    // inside `sum_of_squares`.  Assert that this produces an error.
    //
    // TODO: If/when fn-to-fn calls are supported in the future, this test
    // should be updated to assert success with `texts == vec!["25"]`.
    let err = first_error(&steps);
    assert!(
        err.is_some(),
        "expected an error because fn-to-fn calls are not supported \
         (functions run in isolated environments), but got none"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Pure functions — scope isolation
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_scope_isolation_cannot_read_outer_let() {
    // Variables declared in the label body must not leak into the fn body.
    // The analysis pass should flag `outer` as undefined.
    let src = r#"
fn read_outer() {
    return outer
}

@entry
label start {
    let outer = 42
    let r = read_outer()
    Narrator: "{r}"
    end!()
}
"#;
    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "expected error: fn body should not see outer variable `outer`"
    );
}

#[test]
fn fn_scope_isolation_cannot_read_global() {
    // Pure functions cannot access `global` declarations either.
    // The analysis pass flags this; confirm end-to-end.
    let src = r#"
global score: int = 100

fn get_score() -> int {
    return score
}

@entry
label start {
    let r = get_score()
    Narrator: "{r}"
    end!()
}
"#;
    // This should fail at analysis or runtime.
    let ast = parse_source(src).expect("should parse");
    let errors = urd::analysis::analyze(&ast);
    let has_undef = errors.iter().any(|e| {
        matches!(e, urd::analysis::AnalysisError::UndefinedVariable { name, .. } if name == "score")
    });
    assert!(
        has_undef,
        "analysis should flag `score` as undefined inside the fn body; got: {errors:?}"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Pure functions — wrong arity
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_wrong_arity_too_few_args() {
    let src = r#"
fn add(a, b) {
    return a + b
}

@entry
label start {
    let r = add(1)
    Narrator: "{r}"
    end!()
}
"#;
    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "expected error when calling a 2-param function with 1 argument"
    );
}

#[test]
fn fn_wrong_arity_too_many_args() {
    let src = r#"
fn add(a, b) {
    return a + b
}

@entry
label start {
    let r = add(1, 2, 3)
    Narrator: "{r}"
    end!()
}
"#;
    let steps = run_script(src);
    assert!(
        first_error(&steps).is_some(),
        "expected error when calling a 2-param function with 3 arguments"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Pure functions — implicit return (last expression)
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_bare_expression_does_not_parse_as_statement() {
    // Bare binary expressions like `x * x` without `return` are not valid
    // statements in function bodies — the parser requires explicit `return`.
    let src = r#"
fn square(x) {
    x * x
}

@entry
label start {
    let r = square(6)
    Narrator: "{r}"
    end!()
}
"#;
    assert!(
        parse_source(src).is_err(),
        "bare binary expression without `return` should fail to parse"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Pure functions — if/else inside function body
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_if_else_in_body() {
    let src = r#"
fn abs(x) {
    if x < 0 {
        return -x
    } else {
        return x
    }
}

@entry
label start {
    let a = abs(-7)
    let b = abs(3)
    Narrator: "{a}"
    Narrator: "{b}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["7", "3"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Pure functions — local variable inside function
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_local_variable() {
    let src = r#"
fn hypotenuse_sq(a, b) {
    let a2 = a * a
    let b2 = b * b
    return a2 + b2
}

@entry
label start {
    let r = hypotenuse_sq(3, 4)
    Narrator: "{r}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["25"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Anonymous functions (closures) — list.map / list.filter
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn anon_fn_list_map() {
    let src = r#"
@entry
label start {
    let xs = [1, 2, 3]
    let f = fn(x) { return x * 2 }
    let doubled = xs.map(f)
    let r = doubled.len()
    Narrator: "{r}"
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    // list.map should produce a 3-element list.
    assert_eq!(texts, vec!["3"], "mapped list should have 3 elements");
}

#[test]
fn anon_fn_list_filter() {
    let src = r#"
@entry
label start {
    let xs = [1, 2, 3, 4, 5]
    let pred = fn(x) { return x % 2 == 0 }
    let evens = xs.filter(pred)
    let r = evens.len()
    Narrator: "{r}"
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["2"], "filter should keep 2 and 4");
}

#[test]
fn anon_fn_assigned_to_variable() {
    let src = r#"
@entry
label start {
    let double = fn(x) { return x * 2 }
    let xs = [10, 20]
    let ys = xs.map(double)
    let first = ys.get(0)
    let second = ys.get(1)
    Narrator: "{first}"
    Narrator: "{second}"
    end!()
}
"#;
    let steps = run_script(src);
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["20", "40"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Anonymous functions — list.reduce
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn anon_fn_list_reduce_sum() {
    let src = r#"
@entry
label start {
    let xs = [1, 2, 3, 4]
    let f = fn(acc, x) { return acc + x }
    let total = xs.reduce(0, f)
    Narrator: "{total}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["10"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Combined: `in` + functions
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_uses_in_operator_internally() {
    let src = r#"
fn contains_three(xs) {
    return 3 in xs
}

@entry
label start {
    let a = contains_three([1, 2, 3])
    let b = contains_three([4, 5, 6])
    if a {
        Narrator: "a:yes"
    } else {
        Narrator: "a:no"
    }
    if b {
        Narrator: "b:yes"
    } else {
        Narrator: "b:no"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["a:yes", "b:no"]);
}

#[test]
fn in_with_function_result_as_needle() {
    let src = r#"
fn pick() {
    return 2
}

@entry
label start {
    let r = pick() in [1, 2, 3]
    if r {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Edge case: `in` with computed expressions
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn in_range_with_computed_bounds() {
    let src = r#"
@entry
label start {
    let lo = 1 + 1
    let hi = 3 + 3
    let result = 4 in lo..hi
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

#[test]
fn in_list_with_computed_elements() {
    let src = r#"
@entry
label start {
    let a = 10
    let result = 10 in [a, a + 1, a + 2]
    if result {
        Narrator: "found"
    } else {
        Narrator: "not found"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["found"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Functions — return from nested if without else
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_early_return_from_if() {
    let src = r#"
fn classify(n) {
    if n > 0 {
        return "positive"
    }
    if n < 0 {
        return "negative"
    }
    return "zero"
}

@entry
label start {
    let a = classify(5)
    let b = classify(-3)
    let c = classify(0)
    Narrator: "{a}"
    Narrator: "{b}"
    Narrator: "{c}"
    end!()
}
"#;
    assert_eq!(
        dialogue_texts(&run_script(src)),
        vec!["positive", "negative", "zero"]
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Functions — nested fn definition inside fn body
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_nested_definition() {
    let src = r#"
fn outer(x) {
    fn inner(y) {
        return y + 1
    }
    return inner(x) * 2
}

@entry
label start {
    let r = outer(4)
    Narrator: "{r}"
    end!()
}
"#;
    // inner(4) = 5, 5 * 2 = 10
    assert_eq!(dialogue_texts(&run_script(src)), vec!["10"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Functions — string operations inside fn
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_string_return_parameter() {
    let src = r#"
fn echo(msg) {
    return msg
}

@entry
label start {
    let name = echo("Jane Doe")
    Narrator: "{name}"
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["Jane Doe"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Functions — arithmetic chains
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn fn_arithmetic_chain() {
    let src = r#"
fn compute(a, b, c) {
    let step1 = a + b
    let step2 = step1 * c
    return step2 - 1
}

@entry
label start {
    let r = compute(2, 3, 4)
    Narrator: "{r}"
    end!()
}
"#;
    // (2+3)*4 - 1 = 19
    assert_eq!(dialogue_texts(&run_script(src)), vec!["19"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
//  `in` operator — chained boolean with `and` / `or`
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn in_combined_with_and() {
    let src = r#"
@entry
label start {
    let r = (2 in [1, 2, 3]) and (5 in 0..10)
    if r {
        Narrator: "both"
    } else {
        Narrator: "not both"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["both"]);
}

#[test]
fn in_combined_with_or() {
    let src = r#"
@entry
label start {
    let r = (99 in [1, 2]) or ("x" in "fox")
    if r {
        Narrator: "at least one"
    } else {
        Narrator: "neither"
    }
    end!()
}
"#;
    assert_eq!(dialogue_texts(&run_script(src)), vec!["at least one"]);
}
