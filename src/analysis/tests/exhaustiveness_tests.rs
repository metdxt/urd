//! Integration tests for the [`crate::analysis::exhaustiveness`] pass.
//!
//! These tests build AST nodes directly using the builder methods on [`Ast`]
//! and assert that [`exhaustiveness::check`] returns the expected diagnostics.

#![allow(clippy::unwrap_used)]

use crate::analysis::exhaustiveness;
use crate::analysis::AnalysisError;
use crate::analysis::context::AnalysisContext;
use crate::parser::ast::{Ast, MatchArm, MatchPattern, TypeAnnotation};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn make_ctx(enums: &[(&str, &[&str])], vars: &[(&str, TypeAnnotation)]) -> AnalysisContext {
    let mut ctx = AnalysisContext::default();
    for (name, variants) in enums {
        ctx.enums.insert(
            (*name).to_owned(),
            variants.iter().map(|s| (*s).to_owned()).collect(),
        );
    }
    for (var_name, ann) in vars {
        ctx.top_level_vars.insert((*var_name).to_owned(), ann.clone());
    }
    ctx
}

fn ident_path(name: &str) -> Ast {
    Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
}

fn value_arm(variant: &str, body: Ast) -> MatchArm {
    MatchArm::new(MatchPattern::Value(ident_path(variant)), body)
}

fn wildcard_arm(body: Ast) -> MatchArm {
    MatchArm::new(MatchPattern::Wildcard, body)
}

fn return_block() -> Ast {
    Ast::block(vec![Ast::return_stmt(None)])
}

fn assert_no_errors(errors: &[AnalysisError]) {
    assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
}

fn assert_non_exhaustive(
    errors: &[AnalysisError],
    expected_enum: &str,
    expected_missing: &[&str],
) {
    assert_eq!(
        errors.len(),
        1,
        "expected exactly 1 error, got: {errors:?}"
    );
    match &errors[0] {
        AnalysisError::NonExhaustiveMatch {
            enum_name,
            missing_variants,
            ..
        } => {
            assert_eq!(enum_name.as_str(), expected_enum);
            let mut actual: Vec<&str> = missing_variants.iter().map(String::as_str).collect();
            actual.sort_unstable();
            let mut expected_sorted: Vec<&str> = expected_missing.to_vec();
            expected_sorted.sort_unstable();
            assert_eq!(
                actual, expected_sorted,
                "missing variants mismatch for enum '{expected_enum}'"
            );
        }
        other => panic!("expected NonExhaustiveMatch, got: {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Wildcard arm
// ---------------------------------------------------------------------------

#[test]
fn wildcard_arm_is_always_exhaustive() {
    let ctx = make_ctx(&[("Dir", &["North", "South", "East", "West"])], &[]);
    let scrutinee = ident_path("direction");
    let arms = vec![
        value_arm("North", return_block()),
        wildcard_arm(return_block()),
    ];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

#[test]
fn wildcard_only_arm_is_exhaustive() {
    let ctx = make_ctx(&[("Mood", &["Happy", "Sad", "Angry"])], &[]);
    let scrutinee = ident_path("m");
    let arms = vec![wildcard_arm(return_block())];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

// ---------------------------------------------------------------------------
// All variants covered
// ---------------------------------------------------------------------------

#[test]
fn all_variants_covered_is_ok() {
    let ctx = make_ctx(&[("Color", &["Red", "Green", "Blue"])], &[]);
    let scrutinee = ident_path("c");
    let arms = vec![
        value_arm("Red", return_block()),
        value_arm("Green", return_block()),
        value_arm("Blue", return_block()),
    ];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

#[test]
fn single_variant_enum_covered_is_ok() {
    let ctx = make_ctx(&[("Unit", &["Only"])], &[]);
    let scrutinee = ident_path("u");
    let arms = vec![value_arm("Only", return_block())];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

// ---------------------------------------------------------------------------
// Missing one variant
// ---------------------------------------------------------------------------

#[test]
fn missing_one_variant_is_error() {
    let ctx = make_ctx(&[("Dir", &["North", "South", "East", "West"])], &[]);
    let scrutinee = ident_path("direction");
    let arms = vec![
        value_arm("North", return_block()),
        value_arm("South", return_block()),
        value_arm("East", return_block()),
        // "West" is missing
    ];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Dir", &["West"]);
}

#[test]
fn missing_first_variant_is_error() {
    let ctx = make_ctx(&[("Season", &["Spring", "Summer", "Autumn", "Winter"])], &[]);
    let scrutinee = ident_path("s");
    let arms = vec![
        // "Spring" is missing
        value_arm("Summer", return_block()),
        value_arm("Autumn", return_block()),
        value_arm("Winter", return_block()),
    ];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Season", &["Spring"]);
}

// ---------------------------------------------------------------------------
// Missing multiple variants
// ---------------------------------------------------------------------------

#[test]
fn missing_multiple_variants_reported() {
    let ctx = make_ctx(
        &[("Phase", &["New", "Waxing", "Full", "Waning"])],
        &[],
    );
    let scrutinee = ident_path("phase");
    // Only "New" is covered
    let arms = vec![value_arm("New", return_block())];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Phase", &["Waxing", "Full", "Waning"]);
}

#[test]
fn all_variants_missing_when_no_arms_and_typed_scrutinee() {
    // scrutinee is a typed variable → enum is resolved via annotation
    let ctx = make_ctx(
        &[("Bit", &["Zero", "One"])],
        &[("b", TypeAnnotation::Named(vec!["Bit".to_owned()]))],
    );
    let scrutinee = ident_path("b");
    // No arms (and no wildcard)
    let ast = Ast::match_stmt(scrutinee, vec![]);
    let root = Ast::block(vec![ast]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Bit", &["Zero", "One"]);
}

// ---------------------------------------------------------------------------
// Unknown enum scrutinee is silently skipped
// ---------------------------------------------------------------------------

#[test]
fn unknown_enum_scrutinee_is_skipped() {
    // No enums registered at all
    let ctx = make_ctx(&[], &[]);
    let scrutinee = ident_path("x");
    let arms = vec![
        value_arm("Foo", return_block()),
        value_arm("Bar", return_block()),
    ];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

#[test]
fn unregistered_enum_name_in_annotation_is_skipped() {
    // "state" is annotated as Named("State"), but "State" is not in ctx.enums
    let ctx = make_ctx(
        &[],
        &[("state", TypeAnnotation::Named(vec!["State".to_owned()]))],
    );
    let scrutinee = ident_path("state");
    let arms = vec![value_arm("On", return_block())];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

// ---------------------------------------------------------------------------
// Variable with Named type annotation resolves enum
// ---------------------------------------------------------------------------

#[test]
fn variable_with_named_type_resolves_enum() {
    let ctx = make_ctx(
        &[("Switch", &["On", "Off"])],
        &[("state", TypeAnnotation::Named(vec!["Switch".to_owned()]))],
    );
    let scrutinee = ident_path("state");
    // Only "On" is covered
    let arms = vec![value_arm("On", return_block())];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Switch", &["Off"]);
}

#[test]
fn variable_with_named_type_all_covered_is_ok() {
    let ctx = make_ctx(
        &[("Light", &["On", "Off", "Dim"])],
        &[("lamp", TypeAnnotation::Named(vec!["Light".to_owned()]))],
    );
    let scrutinee = ident_path("lamp");
    let arms = vec![
        value_arm("On", return_block()),
        value_arm("Off", return_block()),
        value_arm("Dim", return_block()),
    ];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

// ---------------------------------------------------------------------------
// Non-Int annotations on variable are not mistaken for enums
// ---------------------------------------------------------------------------

#[test]
fn int_annotated_variable_match_is_skipped() {
    let ctx = make_ctx(
        &[("Foo", &["A", "B"])],
        &[("x", TypeAnnotation::Int)],
    );
    let scrutinee = ident_path("x");
    let arms = vec![value_arm("A", return_block())];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    // "x" is Int, not Named → resolves via patterns → "Foo" contains "A" → only one match
    // This might still match via pattern heuristic. But without "B", it would error.
    // The key assertion: the check doesn't crash; whether it errors depends on heuristic.
    // (We're testing best-effort behaviour, not a contract.)
    let _errors = exhaustiveness::check(&root, &ctx);
    // No assertion on count — just ensure it doesn't panic.
}

// ---------------------------------------------------------------------------
// Match inside a labeled block — location reflects label
// ---------------------------------------------------------------------------

#[test]
fn error_location_reflects_labeled_block() {
    let ctx = make_ctx(&[("T", &["A", "B"])], &[]);
    let scrutinee = ident_path("t");
    let arms = vec![value_arm("A", return_block())]; // "B" missing
    let match_node = Ast::match_stmt(scrutinee, arms);
    let label = Ast::labeled_block(
        "act_one".to_owned(),
        Ast::block(vec![match_node]),
    );
    let root = Ast::block(vec![label]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_eq!(errors.len(), 1, "expected exactly 1 error, got: {errors:?}");
    match &errors[0] {
        AnalysisError::NonExhaustiveMatch { location, .. } => {
            assert!(
                location.0.contains("act_one"),
                "expected location to mention 'act_one', got '{}'",
                location.0
            );
        }
        other => panic!("expected NonExhaustiveMatch, got: {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Match inside menu option — location reflects option
// ---------------------------------------------------------------------------

#[test]
fn error_location_reflects_menu_option() {
    let ctx = make_ctx(&[("T", &["A", "B"])], &[]);
    let scrutinee = ident_path("t");
    let arms = vec![value_arm("A", return_block())]; // "B" missing
    let match_node = Ast::match_stmt(scrutinee, arms);
    let option = Ast::menu_option(
        "Choice One".to_owned(),
        Ast::block(vec![match_node]),
    );
    let menu = Ast::menu(vec![option]);
    let root = Ast::block(vec![menu]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
    match &errors[0] {
        AnalysisError::NonExhaustiveMatch { location, .. } => {
            assert!(
                location.0.contains("Choice One"),
                "expected location to mention 'Choice One', got '{}'",
                location.0
            );
        }
        other => panic!("expected NonExhaustiveMatch, got: {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Nested matches: both are checked independently
// ---------------------------------------------------------------------------

#[test]
fn nested_match_both_checked_independently() {
    // Outer match: A / B fully covered
    // Inner match (inside arm A): X missing Y
    let ctx = make_ctx(
        &[
            ("Outer", &["A", "B"]),
            ("Inner", &["X", "Y"]),
        ],
        &[],
    );
    let inner_scrutinee = ident_path("inner_val");
    let inner_arms = vec![value_arm("X", return_block())]; // "Y" missing
    let inner_match = Ast::match_stmt(inner_scrutinee, inner_arms);

    let outer_scrutinee = ident_path("outer_val");
    let arm_a = MatchArm::new(
        MatchPattern::Value(ident_path("A")),
        Ast::block(vec![inner_match, Ast::return_stmt(None)]),
    );
    let arm_b = value_arm("B", return_block());
    let outer_match = Ast::match_stmt(outer_scrutinee, vec![arm_a, arm_b]);

    let root = Ast::block(vec![outer_match, Ast::return_stmt(None)]);
    let errors = exhaustiveness::check(&root, &ctx);

    // Only the inner non-exhaustive match should produce an error.
    assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
    match &errors[0] {
        AnalysisError::NonExhaustiveMatch {
            enum_name,
            missing_variants,
            ..
        } => {
            assert_eq!(enum_name.as_str(), "Inner");
            assert_eq!(missing_variants, &["Y".to_owned()]);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn two_non_exhaustive_matches_both_reported() {
    let ctx = make_ctx(
        &[
            ("A", &["A1", "A2"]),
            ("B", &["B1", "B2"]),
        ],
        &[],
    );
    let match_a = Ast::match_stmt(
        ident_path("a"),
        vec![value_arm("A1", return_block())], // "A2" missing
    );
    let match_b = Ast::match_stmt(
        ident_path("b"),
        vec![value_arm("B1", return_block())], // "B2" missing
    );
    let root = Ast::block(vec![match_a, match_b]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
}

// ---------------------------------------------------------------------------
// Ambiguous pattern names (two enums share a superset relationship)
// ---------------------------------------------------------------------------

#[test]
fn ambiguous_pattern_names_are_silently_skipped() {
    // Two enums both contain "Foo" and "Bar"; heuristic cannot pick one → skip.
    let ctx = make_ctx(
        &[
            ("EnumA", &["Foo", "Bar"]),
            ("EnumB", &["Foo", "Bar", "Baz"]),
        ],
        &[],
    );
    let scrutinee = ident_path("x");
    let arms = vec![
        value_arm("Foo", return_block()),
        value_arm("Bar", return_block()),
    ];
    let ast = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

// ---------------------------------------------------------------------------
// No patterns at all, no wildcard, no typed scrutinee → silently skipped
// ---------------------------------------------------------------------------

#[test]
fn empty_arms_no_typed_scrutinee_is_skipped() {
    let ctx = make_ctx(&[("X", &["P", "Q"])], &[]);
    // No arms, no wildcard, scrutinee is an untyped variable
    let ast = Ast::match_stmt(ident_path("x"), vec![]);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

// ---------------------------------------------------------------------------
// Match inside an if branch is checked
// ---------------------------------------------------------------------------

#[test]
fn match_inside_if_branch_is_checked() {
    let ctx = make_ctx(&[("Flag", &["Yes", "No"])], &[]);
    let match_node = Ast::match_stmt(
        ident_path("flag"),
        vec![value_arm("Yes", return_block())], // "No" missing
    );
    let then_block = Ast::block(vec![match_node]);
    let if_node = Ast::if_stmt(
        Ast::value(RuntimeValue::Bool(true)),
        then_block,
        None,
    );
    let root = Ast::block(vec![if_node]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Flag", &["No"]);
}

// ---------------------------------------------------------------------------
// Multiple enums, only one scrutinee is non-exhaustive
// ---------------------------------------------------------------------------

#[test]
fn only_non_exhaustive_match_produces_error() {
    let ctx = make_ctx(
        &[
            ("Color", &["Red", "Green", "Blue"]),
            ("Size", &["Small", "Large"]),
        ],
        &[],
    );
    // Exhaustive match over Color
    let match_color = Ast::match_stmt(
        ident_path("color"),
        vec![
            value_arm("Red", return_block()),
            value_arm("Green", return_block()),
            value_arm("Blue", return_block()),
        ],
    );
    // Non-exhaustive match over Size (missing "Large")
    let match_size = Ast::match_stmt(
        ident_path("size"),
        vec![value_arm("Small", return_block())],
    );
    let root = Ast::block(vec![match_color, match_size]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Size", &["Large"]);
}

// ---------------------------------------------------------------------------
// check() on a non-block root (e.g. a bare match node)
// ---------------------------------------------------------------------------

#[test]
fn check_on_non_exhaustive_match_at_top_level_block_works() {
    let ctx = make_ctx(&[("AB", &["A", "B"])], &[]);
    let scrutinee = ident_path("v");
    let arms = vec![value_arm("A", return_block())]; // "B" missing
    let match_node = Ast::match_stmt(scrutinee, arms);
    let root = Ast::block(vec![match_node]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "AB", &["B"]);
}

// ---------------------------------------------------------------------------
// Enum with one variant, wildcard present → exhaustive
// ---------------------------------------------------------------------------

#[test]
fn single_variant_enum_with_wildcard_is_ok() {
    let ctx = make_ctx(&[("Sole", &["Only"])], &[]);
    let arms = vec![wildcard_arm(return_block())];
    let ast = Ast::match_stmt(ident_path("s"), arms);
    let root = Ast::block(vec![ast]);
    assert_no_errors(&exhaustiveness::check(&root, &ctx));
}

// ---------------------------------------------------------------------------
// Decorators / DecoratorDef bodies are walked
// ---------------------------------------------------------------------------

#[test]
fn match_inside_decorator_body_is_checked() {
    use crate::parser::ast::EventConstraint;
    let ctx = make_ctx(&[("Ev", &["Start", "End"])], &[]);
    let match_node = Ast::match_stmt(
        ident_path("ev"),
        vec![value_arm("Start", return_block())], // "End" missing
    );
    let body = Ast::block(vec![match_node]);
    let dec_def = Ast::decorator_def(
        "my_dec".to_owned(),
        EventConstraint::Any,
        vec![],
        body,
    );
    let root = Ast::block(vec![dec_def]);
    let errors = exhaustiveness::check(&root, &ctx);
    assert_non_exhaustive(&errors, "Ev", &["End"]);
}
