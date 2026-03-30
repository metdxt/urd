//! Integration tests for the [`crate::analysis::types`] pass.
//!
//! These tests build AST nodes directly using the builder methods on [`Ast`]
//! and assert that [`types::check`] returns the expected diagnostics.

#![allow(clippy::unwrap_used)]

use crate::analysis::AnalysisError;
use crate::analysis::context::AnalysisContext;
use crate::analysis::types;
use crate::parser::ast::{Ast, DeclKind, TypeAnnotation};
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
        ctx.top_level_vars
            .insert((*var_name).to_owned(), ann.clone());
    }
    ctx
}

fn ident(name: &str) -> Ast {
    Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
}

fn int_lit(n: i64) -> Ast {
    Ast::value(RuntimeValue::Int(n))
}

fn float_lit(f: f64) -> Ast {
    Ast::value(RuntimeValue::Float(f))
}

fn bool_lit(b: bool) -> Ast {
    Ast::value(RuntimeValue::Bool(b))
}

fn null_lit() -> Ast {
    Ast::value(RuntimeValue::Null)
}

fn str_lit(s: &str) -> Ast {
    Ast::value(RuntimeValue::Str(
        crate::lexer::strings::ParsedString::new_plain(s),
    ))
}

fn dice_lit(count: u8, sides: u8) -> Ast {
    Ast::value(RuntimeValue::Dice(count, sides))
}

fn typed_decl(name: &str, ann: TypeAnnotation, val: Ast) -> Ast {
    Ast::typed_decl(DeclKind::Variable, ident(name), ann, val)
}

fn untyped_decl(name: &str, val: Ast) -> Ast {
    Ast::decl(DeclKind::Variable, ident(name), val)
}

fn assert_no_errors(errors: &[AnalysisError]) {
    assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
}

fn assert_type_mismatch(errors: &[AnalysisError], var: &str) {
    assert_eq!(errors.len(), 1, "expected exactly 1 error, got: {errors:?}");
    match &errors[0] {
        AnalysisError::TypeMismatch { variable, .. } => {
            assert_eq!(
                variable.as_str(),
                var,
                "mismatch variable name: expected '{var}', got '{variable}'"
            );
        }
        other => panic!("expected TypeMismatch for '{var}', got: {other:?}"),
    }
}

fn assert_mismatch_count(errors: &[AnalysisError], count: usize) {
    let mismatches: Vec<_> = errors
        .iter()
        .filter(|e| matches!(e, AnalysisError::TypeMismatch { .. }))
        .collect();
    assert_eq!(
        mismatches.len(),
        count,
        "expected {count} TypeMismatch error(s), got: {errors:?}"
    );
}

// ---------------------------------------------------------------------------
// Int annotation
// ---------------------------------------------------------------------------

#[test]
fn int_literal_compatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, int_lit(42))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn zero_int_compatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, int_lit(0))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn negative_int_compatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, int_lit(-99))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn float_not_compatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, float_lit(1.5))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

#[test]
fn bool_not_compatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, bool_lit(true))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

#[test]
fn str_literal_incompatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, str_lit("hello"))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

#[test]
fn null_not_compatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, null_lit())]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

// ---------------------------------------------------------------------------
// Float annotation (with Int widening)
// ---------------------------------------------------------------------------

#[test]
fn float_annotation_accepts_float_literal() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("f", TypeAnnotation::Float, float_lit(2.5))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn float_annotation_accepts_int_widening() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("f", TypeAnnotation::Float, int_lit(10))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn float_annotation_accepts_zero_int() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("f", TypeAnnotation::Float, int_lit(0))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn float_annotation_rejects_bool() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl(
        "f",
        TypeAnnotation::Float,
        bool_lit(false),
    )]);
    assert_type_mismatch(&types::check(&ast, &ctx), "f");
}

#[test]
fn float_annotation_rejects_str() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("f", TypeAnnotation::Float, str_lit("pi"))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "f");
}

// ---------------------------------------------------------------------------
// Bool annotation
// ---------------------------------------------------------------------------

#[test]
fn bool_literal_compatible_with_bool_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("b", TypeAnnotation::Bool, bool_lit(false))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn bool_true_compatible_with_bool_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("b", TypeAnnotation::Bool, bool_lit(true))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn int_not_compatible_with_bool_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("b", TypeAnnotation::Bool, int_lit(1))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "b");
}

#[test]
fn str_not_compatible_with_bool_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("b", TypeAnnotation::Bool, str_lit("true"))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "b");
}

// ---------------------------------------------------------------------------
// Str annotation
// ---------------------------------------------------------------------------

#[test]
fn str_literal_compatible_with_str_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl(
        "s",
        TypeAnnotation::Str,
        str_lit("hello world"),
    )]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn int_not_compatible_with_str_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("s", TypeAnnotation::Str, int_lit(0))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "s");
}

#[test]
fn bool_not_compatible_with_str_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("s", TypeAnnotation::Str, bool_lit(false))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "s");
}

// ---------------------------------------------------------------------------
// Null annotation
// ---------------------------------------------------------------------------

#[test]
fn null_compatible_with_null_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("n", TypeAnnotation::Null, null_lit())]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn bool_not_compatible_with_null_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("n", TypeAnnotation::Null, bool_lit(true))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "n");
}

#[test]
fn int_not_compatible_with_null_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("n", TypeAnnotation::Null, int_lit(0))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "n");
}

// ---------------------------------------------------------------------------
// Named (enum) annotation
// ---------------------------------------------------------------------------

#[test]
fn enum_variant_compatible_with_named_annotation() {
    let ctx = make_ctx(&[("Dir", &["North", "South"])], &[]);
    let val = Ast::value(RuntimeValue::IdentPath(vec!["North".to_owned()]));
    let ast = Ast::block(vec![typed_decl(
        "d",
        TypeAnnotation::Named(vec!["Dir".to_owned()]),
        val,
    )]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn all_enum_variants_compatible_with_named_annotation() {
    let ctx = make_ctx(
        &[("Season", &["Spring", "Summer", "Autumn", "Winter"])],
        &[],
    );
    for variant in &["Spring", "Summer", "Autumn", "Winter"] {
        let val = Ast::value(RuntimeValue::IdentPath(vec![(*variant).to_owned()]));
        let ast = Ast::block(vec![typed_decl(
            "s",
            TypeAnnotation::Named(vec!["Season".to_owned()]),
            val,
        )]);
        assert_no_errors(&types::check(&ast, &ctx));
    }
}

#[test]
fn invalid_enum_variant_reports_mismatch() {
    let ctx = make_ctx(&[("Dir", &["North", "South"])], &[]);
    let val = Ast::value(RuntimeValue::IdentPath(vec!["East".to_owned()]));
    let ast = Ast::block(vec![typed_decl(
        "d",
        TypeAnnotation::Named(vec!["Dir".to_owned()]),
        val,
    )]);
    assert_type_mismatch(&types::check(&ast, &ctx), "d");
}

#[test]
fn null_compatible_with_named_annotation() {
    // Named types accept null (nullable enum).
    let ctx = make_ctx(&[("Dir", &["North", "South"])], &[]);
    let ast = Ast::block(vec![typed_decl(
        "d",
        TypeAnnotation::Named(vec!["Dir".to_owned()]),
        null_lit(),
    )]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn unknown_named_enum_accepted_best_effort() {
    // If the enum is not registered in the context, we accept (best-effort).
    let ctx = make_ctx(&[], &[]);
    let val = Ast::value(RuntimeValue::IdentPath(vec!["Maybe".to_owned()]));
    let ast = Ast::block(vec![typed_decl(
        "x",
        TypeAnnotation::Named(vec!["UnknownEnum".to_owned()]),
        val,
    )]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn int_not_compatible_with_named_annotation() {
    let ctx = make_ctx(&[("Dir", &["North", "South"])], &[]);
    let ast = Ast::block(vec![typed_decl(
        "d",
        TypeAnnotation::Named(vec!["Dir".to_owned()]),
        int_lit(0),
    )]);
    assert_type_mismatch(&types::check(&ast, &ctx), "d");
}

// ---------------------------------------------------------------------------
// Dice annotation
// ---------------------------------------------------------------------------

#[test]
fn dice_value_compatible_with_dice_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl(
        "roll",
        TypeAnnotation::Dice,
        dice_lit(2, 6),
    )]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn int_not_compatible_with_dice_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("roll", TypeAnnotation::Dice, int_lit(6))]);
    assert_type_mismatch(&types::check(&ast, &ctx), "roll");
}

#[test]
fn str_not_compatible_with_dice_annotation() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl(
        "roll",
        TypeAnnotation::Dice,
        str_lit("2d6"),
    )]);
    assert_type_mismatch(&types::check(&ast, &ctx), "roll");
}

// ---------------------------------------------------------------------------
// List annotation
// ---------------------------------------------------------------------------

#[test]
fn list_literal_compatible_with_list_annotation() {
    let ctx = make_ctx(&[], &[]);
    let list = Ast::list(vec![int_lit(1), int_lit(2), int_lit(3)]);
    let ast = Ast::block(vec![typed_decl("lst", TypeAnnotation::List, list)]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn empty_list_compatible_with_list_annotation() {
    let ctx = make_ctx(&[], &[]);
    let list = Ast::list(vec![]);
    let ast = Ast::block(vec![typed_decl("lst", TypeAnnotation::List, list)]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn list_literal_incompatible_with_int_annotation() {
    let ctx = make_ctx(&[], &[]);
    let list = Ast::list(vec![int_lit(1)]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, list)]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

#[test]
fn list_literal_incompatible_with_bool_annotation() {
    let ctx = make_ctx(&[], &[]);
    let list = Ast::list(vec![bool_lit(true)]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Bool, list)]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

// ---------------------------------------------------------------------------
// Map annotation
// ---------------------------------------------------------------------------

#[test]
fn map_literal_compatible_with_map_annotation() {
    let ctx = make_ctx(&[], &[]);
    let map = Ast::map(vec![(ident("key"), int_lit(1))]);
    let ast = Ast::block(vec![typed_decl("m", TypeAnnotation::Map, map)]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn empty_map_compatible_with_map_annotation() {
    let ctx = make_ctx(&[], &[]);
    let map = Ast::map(vec![]);
    let ast = Ast::block(vec![typed_decl("m", TypeAnnotation::Map, map)]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn map_literal_incompatible_with_bool_annotation() {
    let ctx = make_ctx(&[], &[]);
    let map = Ast::map(vec![]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Bool, map)]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

#[test]
fn map_literal_incompatible_with_str_annotation() {
    let ctx = make_ctx(&[], &[]);
    let map = Ast::map(vec![]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Str, map)]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

// ---------------------------------------------------------------------------
// Reassignment via BinOp Assign
// ---------------------------------------------------------------------------

#[test]
fn reassignment_checks_type() {
    // "score" is declared as Int in the context; assigning a bool is a mismatch.
    let ctx = make_ctx(&[], &[("score", TypeAnnotation::Int)]);
    let assign = Ast::assign_op(ident("score"), bool_lit(true));
    let ast = Ast::block(vec![assign]);
    assert_type_mismatch(&types::check(&ast, &ctx), "score");
}

#[test]
fn reassignment_compatible_type_ok() {
    let ctx = make_ctx(&[], &[("score", TypeAnnotation::Int)]);
    let assign = Ast::assign_op(ident("score"), int_lit(100));
    let ast = Ast::block(vec![assign]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn reassignment_float_widening_ok() {
    let ctx = make_ctx(&[], &[("ratio", TypeAnnotation::Float)]);
    let assign = Ast::assign_op(ident("ratio"), int_lit(1));
    let ast = Ast::block(vec![assign]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn reassignment_to_unknown_variable_is_skipped() {
    // "unknown" has no annotation — silently skip.
    let ctx = make_ctx(&[], &[]);
    let assign = Ast::assign_op(ident("unknown"), bool_lit(false));
    let ast = Ast::block(vec![assign]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn reassignment_str_to_int_variable_is_error() {
    let ctx = make_ctx(&[], &[("name", TypeAnnotation::Str)]);
    let assign = Ast::assign_op(ident("name"), int_lit(42));
    let ast = Ast::block(vec![assign]);
    assert_type_mismatch(&types::check(&ast, &ctx), "name");
}

// ---------------------------------------------------------------------------
// Declaration then reassignment in same block
// ---------------------------------------------------------------------------

#[test]
fn local_declaration_then_bad_reassignment_reports_error() {
    let ctx = make_ctx(&[], &[]);
    let decl = typed_decl("x", TypeAnnotation::Int, int_lit(0));
    let assign = Ast::assign_op(ident("x"), bool_lit(true)); // wrong type
    let ast = Ast::block(vec![decl, assign]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

#[test]
fn local_declaration_then_good_reassignment_ok() {
    let ctx = make_ctx(&[], &[]);
    let decl = typed_decl("x", TypeAnnotation::Int, int_lit(0));
    let assign = Ast::assign_op(ident("x"), int_lit(99));
    let ast = Ast::block(vec![decl, assign]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn untyped_declaration_followed_by_reassignment_not_checked() {
    let ctx = make_ctx(&[], &[]);
    let decl = untyped_decl("x", int_lit(0));
    // Assigning a bool to an untyped variable — no annotation, no check.
    let assign = Ast::assign_op(ident("x"), bool_lit(true));
    let ast = Ast::block(vec![decl, assign]);
    assert_no_errors(&types::check(&ast, &ctx));
}

// ---------------------------------------------------------------------------
// Non-literal initialisers are silently accepted
// ---------------------------------------------------------------------------

#[test]
fn expression_initialiser_is_silently_accepted() {
    let ctx = make_ctx(&[], &[]);
    // x: Int = 1 + 2  (BinOp, not a literal — silently ok)
    let init = Ast::add_op(int_lit(1), int_lit(2));
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, init)]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn call_result_initialiser_is_silently_accepted() {
    let ctx = make_ctx(&[], &[]);
    let func = Ast::value(RuntimeValue::IdentPath(vec!["compute".to_owned()]));
    let call = Ast::call(func, Ast::expr_list(vec![]));
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, call)]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn variable_as_rhs_is_silently_accepted() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Bool, ident("other"))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

// ---------------------------------------------------------------------------
// Multiple mismatches all reported
// ---------------------------------------------------------------------------

#[test]
fn multiple_mismatches_all_reported() {
    let ctx = make_ctx(&[], &[]);
    let d1 = typed_decl("a", TypeAnnotation::Int, bool_lit(true)); // mismatch
    let d2 = typed_decl("b", TypeAnnotation::Bool, int_lit(0)); // mismatch
    let d3 = typed_decl("c", TypeAnnotation::Str, str_lit("ok")); // ok
    let d4 = typed_decl("d", TypeAnnotation::Float, str_lit("bad")); // mismatch
    let ast = Ast::block(vec![d1, d2, d3, d4]);
    let errors = types::check(&ast, &ctx);
    assert_mismatch_count(&errors, 3);
}

// ---------------------------------------------------------------------------
// Scope: inner block shadows outer variable
// ---------------------------------------------------------------------------

#[test]
fn inner_scope_shadowed_variable_checked_against_inner_type() {
    let ctx = make_ctx(&[], &[]);
    // Outer: x: Int = 0
    // Inner block: x: Bool = true  (shadows outer, compatible with Bool)
    // Then assign x = false inside inner block (still Bool — ok)
    let inner_decl = typed_decl("x", TypeAnnotation::Bool, bool_lit(true));
    let inner_assign = Ast::assign_op(ident("x"), bool_lit(false));
    let inner_block = Ast::block(vec![inner_decl, inner_assign]);
    let outer_decl = typed_decl("x", TypeAnnotation::Int, int_lit(0));
    let ast = Ast::block(vec![outer_decl, inner_block]);
    assert_no_errors(&types::check(&ast, &ctx));
}

#[test]
fn inner_scope_bad_assignment_uses_inner_type() {
    let ctx = make_ctx(&[], &[]);
    // Inner: x: Bool; then x = 42 (Int, mismatch with Bool)
    let inner_decl = typed_decl("x", TypeAnnotation::Bool, bool_lit(true));
    let inner_assign = Ast::assign_op(ident("x"), int_lit(42)); // mismatch
    let inner_block = Ast::block(vec![inner_decl, inner_assign]);
    let ast = Ast::block(vec![inner_block]);
    assert_type_mismatch(&types::check(&ast, &ctx), "x");
}

// ---------------------------------------------------------------------------
// Labeled block
// ---------------------------------------------------------------------------

#[test]
fn typed_decl_inside_labeled_block_is_checked() {
    let ctx = make_ctx(&[], &[]);
    let decl = typed_decl("health", TypeAnnotation::Int, str_lit("full")); // mismatch
    let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![decl]));
    let ast = Ast::block(vec![label]);
    assert_type_mismatch(&types::check(&ast, &ctx), "health");
}

#[test]
fn typed_decl_inside_labeled_block_ok() {
    let ctx = make_ctx(&[], &[]);
    let decl = typed_decl("health", TypeAnnotation::Int, int_lit(100));
    let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![decl]));
    let ast = Ast::block(vec![label]);
    assert_no_errors(&types::check(&ast, &ctx));
}

// ---------------------------------------------------------------------------
// If branches
// ---------------------------------------------------------------------------

#[test]
fn typed_decl_inside_if_then_branch_is_checked() {
    let ctx = make_ctx(&[], &[]);
    let bad_decl = typed_decl("v", TypeAnnotation::Int, bool_lit(false)); // mismatch
    let then_block = Ast::block(vec![bad_decl]);
    let if_node = Ast::if_stmt(bool_lit(true), then_block, None);
    let ast = Ast::block(vec![if_node]);
    assert_type_mismatch(&types::check(&ast, &ctx), "v");
}

#[test]
fn typed_decl_inside_if_else_branch_is_checked() {
    let ctx = make_ctx(&[], &[]);
    let bad_decl = typed_decl("w", TypeAnnotation::Str, int_lit(0)); // mismatch
    let else_block = Ast::block(vec![bad_decl]);
    let if_node = Ast::if_stmt(bool_lit(false), Ast::block(vec![]), Some(else_block));
    let ast = Ast::block(vec![if_node]);
    assert_type_mismatch(&types::check(&ast, &ctx), "w");
}

// ---------------------------------------------------------------------------
// Menu option scope
// ---------------------------------------------------------------------------

#[test]
fn typed_decl_inside_menu_option_is_checked() {
    let ctx = make_ctx(&[], &[]);
    let bad_decl = typed_decl("choice", TypeAnnotation::Bool, int_lit(1)); // mismatch
    let option = Ast::menu_option("Option A".to_owned(), Ast::block(vec![bad_decl]));
    let menu = Ast::menu(vec![option]);
    let ast = Ast::block(vec![menu]);
    assert_type_mismatch(&types::check(&ast, &ctx), "choice");
}

// ---------------------------------------------------------------------------
// Match arm scope
// ---------------------------------------------------------------------------

#[test]
fn typed_decl_inside_match_arm_is_checked() {
    use crate::parser::ast::{MatchArm, MatchPattern};

    let ctx = make_ctx(&[], &[]);
    let bad_decl = typed_decl("result", TypeAnnotation::Float, str_lit("nan")); // mismatch
    let arm = MatchArm::new(MatchPattern::Wildcard, Ast::block(vec![bad_decl]));
    let scrutinee = Ast::value(RuntimeValue::IdentPath(vec!["x".to_owned()]));
    let match_node = Ast::match_stmt(scrutinee, vec![arm]);
    let ast = Ast::block(vec![match_node]);
    assert_type_mismatch(&types::check(&ast, &ctx), "result");
}

// ---------------------------------------------------------------------------
// Return value (not checked against any annotation, just recursed into)
// ---------------------------------------------------------------------------

#[test]
fn typed_decl_inside_return_not_applicable_but_recursed() {
    // Return takes an expression; the expression itself may contain declarations.
    // Here we just confirm that nesting doesn't panic.
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![Ast::return_stmt(Some(int_lit(1)))]);
    assert_no_errors(&types::check(&ast, &ctx));
}

// ---------------------------------------------------------------------------
// Top-level context variable reassignment
// ---------------------------------------------------------------------------

#[test]
fn top_level_context_var_reassignment_in_nested_block_is_still_checked() {
    // "hp" is declared in context as Int; a nested block assigns a str — mismatch.
    let ctx = make_ctx(&[], &[("hp", TypeAnnotation::Int)]);
    let bad_assign = Ast::assign_op(ident("hp"), str_lit("full"));
    let nested = Ast::block(vec![bad_assign]);
    let ast = Ast::block(vec![nested]);
    assert_type_mismatch(&types::check(&ast, &ctx), "hp");
}

#[test]
fn top_level_context_var_good_reassignment_in_nested_block_ok() {
    let ctx = make_ctx(&[], &[("hp", TypeAnnotation::Int)]);
    let assign = Ast::assign_op(ident("hp"), int_lit(50));
    let nested = Ast::block(vec![assign]);
    let ast = Ast::block(vec![nested]);
    assert_no_errors(&types::check(&ast, &ctx));
}

// ---------------------------------------------------------------------------
// Named annotation with multi-segment path is not matched as IdentPath
// ---------------------------------------------------------------------------

#[test]
fn multi_segment_ident_path_not_compatible_with_named_annotation() {
    let ctx = make_ctx(&[("Dir", &["North"])], &[]);
    // "Dir.North" is a two-segment path, not a single-segment variant name.
    let val = Ast::value(RuntimeValue::IdentPath(vec![
        "Dir".to_owned(),
        "North".to_owned(),
    ]));
    let ast = Ast::block(vec![typed_decl(
        "d",
        TypeAnnotation::Named(vec!["Dir".to_owned()]),
        val,
    )]);
    assert_type_mismatch(&types::check(&ast, &ctx), "d");
}

// ---------------------------------------------------------------------------
// Empty block produces no errors
// ---------------------------------------------------------------------------

#[test]
fn empty_block_produces_no_errors() {
    let ctx = make_ctx(&[], &[]);
    let ast = Ast::block(vec![]);
    assert_no_errors(&types::check(&ast, &ctx));
}

// ---------------------------------------------------------------------------
// Untyped declarations are ignored
// ---------------------------------------------------------------------------

#[test]
fn untyped_declaration_any_value_is_ignored() {
    let ctx = make_ctx(&[], &[]);
    // Assigning a bool to an untyped int variable — no annotation, no check.
    let ast = Ast::block(vec![untyped_decl("anything", bool_lit(true))]);
    assert_no_errors(&types::check(&ast, &ctx));
}
