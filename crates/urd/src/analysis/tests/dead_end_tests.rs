//! Integration tests for the [`crate::analysis::dead_end`] pass.
//!
//! These tests build AST nodes directly using the builder methods on [`Ast`]
//! and assert that [`dead_end::check`] returns the expected diagnostics.

#![allow(clippy::unwrap_used)]

use crate::analysis::AnalysisError;
use crate::analysis::dead_end;
use crate::parser::ast::{Ast, MatchArm, MatchPattern};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn end_call() -> Ast {
    let func = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_owned()]));
    Ast::call(func, Ast::expr_list(vec![]))
}

fn todo_call() -> Ast {
    let func = Ast::value(RuntimeValue::IdentPath(vec!["todo!".to_owned()]));
    Ast::call(func, Ast::expr_list(vec![]))
}

fn return_node() -> Ast {
    Ast::return_stmt(None)
}

fn jump_one_way(label: &str) -> Ast {
    Ast::jump_stmt(label.to_owned(), false)
}

fn jump_and_return(label: &str) -> Ast {
    Ast::jump_stmt(label.to_owned(), true)
}

fn dialogue() -> Ast {
    let speaker = Ast::value(RuntimeValue::IdentPath(vec!["Narrator".to_owned()]));
    let content = Ast::value(RuntimeValue::Str(
        crate::lexer::strings::ParsedString::new_plain("Hello."),
    ));
    Ast::dialogue(speaker, content)
}

fn has_dead_end_at(errors: &[AnalysisError], fragment: &str) -> bool {
    errors.iter().any(|e| match e {
        AnalysisError::DeadEnd { description, .. } => description.0.contains(fragment),
        _ => false,
    })
}

fn assert_no_errors(errors: &[AnalysisError]) {
    assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
}

fn assert_single_dead_end(errors: &[AnalysisError], fragment: &str) {
    assert_eq!(errors.len(), 1, "expected exactly 1 error, got: {errors:?}");
    assert!(
        has_dead_end_at(errors, fragment),
        "expected dead-end error containing '{fragment}', got: {errors:?}"
    );
}

// ---------------------------------------------------------------------------
// Empty block
// ---------------------------------------------------------------------------

#[test]
fn empty_block_is_not_dead_end() {
    // A root Block is a definitions container — never a dead end, even when empty.
    let ast = Ast::block(vec![]);
    let errors = dead_end::check(&ast);
    assert_no_errors(&errors);
}

// ---------------------------------------------------------------------------
// Return
// ---------------------------------------------------------------------------

#[test]
fn block_ending_with_return_terminates() {
    let ast = Ast::block(vec![dialogue(), return_node()]);
    assert_no_errors(&dead_end::check(&ast));
}

#[test]
fn bare_return_with_no_preceding_statements_terminates() {
    let ast = Ast::block(vec![return_node()]);
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// Jump
// ---------------------------------------------------------------------------

#[test]
fn block_ending_with_jump_terminates() {
    let ast = Ast::block(vec![dialogue(), jump_one_way("next_scene")]);
    assert_no_errors(&dead_end::check(&ast));
}

#[test]
fn jump_and_return_does_not_terminate() {
    // expects_return: true means it is a call-and-return, NOT a one-way jump.
    // Flow lives inside a LabeledBlock, not directly in the root Block.
    let label = Ast::labeled_block(
        "scene".to_owned(),
        Ast::block(vec![jump_and_return("sub_scene")]),
    );
    let ast = Ast::block(vec![label]);
    let errors = dead_end::check(&ast);
    assert_single_dead_end(&errors, "scene");
}

// ---------------------------------------------------------------------------
// end! and todo! calls
// ---------------------------------------------------------------------------

#[test]
fn end_call_terminates() {
    let ast = Ast::block(vec![dialogue(), end_call()]);
    assert_no_errors(&dead_end::check(&ast));
}

#[test]
fn todo_call_terminates() {
    let ast = Ast::block(vec![dialogue(), todo_call()]);
    assert_no_errors(&dead_end::check(&ast));
}

#[test]
fn generic_call_does_not_terminate() {
    // Flow lives inside a LabeledBlock, not directly in the root Block.
    let func = Ast::value(RuntimeValue::IdentPath(vec!["say_hello".to_owned()]));
    let call = Ast::call(func, Ast::expr_list(vec![]));
    let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![call]));
    let ast = Ast::block(vec![label]);
    let errors = dead_end::check(&ast);
    assert_single_dead_end(&errors, "scene");
}

// ---------------------------------------------------------------------------
// LetCall
// ---------------------------------------------------------------------------

#[test]
fn let_call_alone_does_not_terminate() {
    // Flow lives inside a LabeledBlock, not directly in the root Block.
    let label = Ast::labeled_block(
        "scene".to_owned(),
        Ast::block(vec![Ast::let_call(
            "result".to_owned(),
            "compute".to_owned(),
        )]),
    );
    let ast = Ast::block(vec![label]);
    let errors = dead_end::check(&ast);
    assert_single_dead_end(&errors, "scene");
}

#[test]
fn let_call_followed_by_return_terminates() {
    let ast = Ast::block(vec![
        Ast::let_call("result".to_owned(), "compute".to_owned()),
        return_node(),
    ]);
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// If with both branches terminating
// ---------------------------------------------------------------------------

#[test]
fn if_with_both_branches_terminating_terminates() {
    let cond = Ast::value(RuntimeValue::Bool(true));
    let then_b = Ast::block(vec![return_node()]);
    let else_b = Ast::block(vec![end_call()]);
    let ast = Ast::block(vec![Ast::if_stmt(cond, then_b, Some(else_b))]);
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// If without else
// ---------------------------------------------------------------------------

#[test]
fn if_without_else_is_dead_end() {
    // The if without else makes the enclosing LabeledBlock MayTerminate → dead end.
    // Flow lives inside a LabeledBlock, not directly in the root Block.
    let cond = Ast::value(RuntimeValue::Bool(true));
    let then_b = Ast::block(vec![return_node()]);
    let label = Ast::labeled_block(
        "scene".to_owned(),
        Ast::block(vec![Ast::if_stmt(cond, then_b, None)]),
    );
    let ast = Ast::block(vec![label]);
    let errors = dead_end::check(&ast);
    assert_single_dead_end(&errors, "scene");
}

#[test]
fn if_without_else_followed_by_return_terminates() {
    // A very common pattern: early branch + guaranteed fallthrough return.
    let cond = Ast::value(RuntimeValue::Bool(false));
    let then_b = Ast::block(vec![dialogue()]);
    let if_node = Ast::if_stmt(cond, then_b, None);
    let ast = Ast::block(vec![if_node, return_node()]);
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// If with only then branch terminating (else open)
// ---------------------------------------------------------------------------

#[test]
fn if_with_only_then_terminating_is_dead_end() {
    // Flow lives inside a LabeledBlock, not directly in the root Block.
    // The open else branch gets its own error, plus the enclosing label.
    let cond = Ast::value(RuntimeValue::Bool(true));
    let then_b = Ast::block(vec![return_node()]);
    let else_b = Ast::block(vec![dialogue()]); // open
    let label = Ast::labeled_block(
        "scene".to_owned(),
        Ast::block(vec![Ast::if_stmt(cond, then_b, Some(else_b))]),
    );
    let ast = Ast::block(vec![label]);
    let errors = dead_end::check(&ast);
    // The open else branch gets its own error, plus the labeled block itself.
    assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
    assert!(
        has_dead_end_at(&errors, "else branch"),
        "expected else-branch error, got: {errors:?}"
    );
    assert!(
        has_dead_end_at(&errors, "scene"),
        "expected dead-end error for 'scene', got: {errors:?}"
    );
}

// ---------------------------------------------------------------------------
// Menu — all options terminate
// ---------------------------------------------------------------------------

#[test]
fn menu_all_options_terminate_is_ok() {
    let opt_a = Ast::menu_option("Yes".to_owned(), Ast::block(vec![return_node()]));
    let opt_b = Ast::menu_option("No".to_owned(), Ast::block(vec![end_call()]));
    let opt_c = Ast::menu_option(
        "Maybe".to_owned(),
        Ast::block(vec![jump_one_way("elsewhere")]),
    );
    let ast = Ast::block(vec![Ast::menu(vec![opt_a, opt_b, opt_c])]);
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// Menu — one option is a dead end
// ---------------------------------------------------------------------------

#[test]
fn menu_one_option_open_is_dead_end() {
    let opt_a = Ast::menu_option("Agree".to_owned(), Ast::block(vec![return_node()]));
    let opt_b = Ast::menu_option("Disagree".to_owned(), Ast::block(vec![dialogue()])); // open
    // Wrap in a LabeledBlock so dead_end::check treats it as a flow-bearing
    // root and emit_terminal_menu_errors fires for the open option.
    let ast = Ast::labeled_block(
        "start".to_owned(),
        Ast::block(vec![Ast::menu(vec![opt_a, opt_b])]),
    );
    let errors = dead_end::check(&ast);
    assert!(
        !errors.is_empty(),
        "expected at least 1 error, got: {errors:?}"
    );
    assert!(
        has_dead_end_at(&errors, "Disagree"),
        "expected dead-end mentioning 'Disagree', got: {errors:?}"
    );
}

// ---------------------------------------------------------------------------
// Labeled block without terminator
// ---------------------------------------------------------------------------

#[test]
fn labeled_block_without_terminator_is_dead_end() {
    // Labeled block nested inside a top-level block that otherwise ends with end!
    let inner = Ast::block(vec![dialogue()]); // no terminator
    let label = Ast::labeled_block("intro".to_owned(), inner);
    let ast = Ast::block(vec![label, end_call()]);
    let errors = dead_end::check(&ast);
    assert_single_dead_end(&errors, "intro");
}

#[test]
fn labeled_block_with_return_inside_is_ok() {
    let inner = Ast::block(vec![dialogue(), return_node()]);
    let label = Ast::labeled_block("scene_a".to_owned(), inner);
    let ast = Ast::block(vec![label, end_call()]);
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// Nested labeled block — reports its own error
// ---------------------------------------------------------------------------

#[test]
fn nested_labeled_block_reports_its_own_error() {
    // A labeled block nested inside a top-level block.
    // The nested label has no terminator; the outer block ends with end!.
    let inner_label_block = Ast::block(vec![dialogue()]); // no terminator
    let nested_label = Ast::labeled_block("sub_scene".to_owned(), inner_label_block);
    let ast = Ast::block(vec![nested_label, end_call()]);
    let errors = dead_end::check(&ast);
    assert_eq!(errors.len(), 1, "expected exactly 1 error, got: {errors:?}");
    assert!(
        has_dead_end_at(&errors, "sub_scene"),
        "expected error for 'sub_scene', got: {errors:?}"
    );
}

#[test]
fn multiple_open_labeled_blocks_each_report_error() {
    // Per the block-walking algorithm, a statement returning `Terminates` stops
    // further analysis of subsequent statements (they are unreachable).  To
    // observe errors from *both* open labeled blocks we must ensure nothing
    // in between them causes early termination — so we omit any terminating
    // labeled block and let end_call() close the outer block after both.
    let la = Ast::labeled_block(
        "alpha".to_owned(),
        Ast::block(vec![dialogue()]), // open → pushes error
    );
    let lc = Ast::labeled_block(
        "gamma".to_owned(),
        Ast::block(vec![dialogue()]), // open → pushes error
    );
    // end_call() terminates the outer block so check() doesn't add a
    // top-level dead-end on top of the two label errors.
    let ast = Ast::block(vec![la, lc, end_call()]);
    let errors = dead_end::check(&ast);
    assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
    assert!(has_dead_end_at(&errors, "alpha"));
    assert!(has_dead_end_at(&errors, "gamma"));
}

// ---------------------------------------------------------------------------
// Match
// ---------------------------------------------------------------------------

#[test]
fn match_all_arms_terminate_is_ok() {
    let scrutinee = Ast::value(RuntimeValue::IdentPath(vec!["state".to_owned()]));
    let arm_a = MatchArm::new(
        MatchPattern::Value(Ast::value(RuntimeValue::IdentPath(vec!["On".to_owned()]))),
        Ast::block(vec![return_node()]),
    );
    let arm_b = MatchArm::new(MatchPattern::Wildcard, Ast::block(vec![end_call()]));
    let match_node = Ast::match_stmt(scrutinee, vec![arm_a, arm_b]);
    let ast = Ast::block(vec![match_node]);
    assert_no_errors(&dead_end::check(&ast));
}

#[test]
fn match_with_open_arm_makes_block_open() {
    // The match itself does not push an error; the enclosing LabeledBlock handles it.
    // Flow lives inside a LabeledBlock, not directly in the root Block.
    let scrutinee = Ast::value(RuntimeValue::IdentPath(vec!["x".to_owned()]));
    let arm_a = MatchArm::new(MatchPattern::Wildcard, Ast::block(vec![dialogue()])); // open
    let match_node = Ast::match_stmt(scrutinee, vec![arm_a]);
    let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![match_node]));
    let ast = Ast::block(vec![label]);
    let errors = dead_end::check(&ast);
    assert_single_dead_end(&errors, "scene");
}

// ---------------------------------------------------------------------------
// Dialogue / Declaration are transparent (not terminators)
// ---------------------------------------------------------------------------

#[test]
fn block_with_only_declarations_is_not_dead_end() {
    // A root Block is a definitions container — declarations carry no flow,
    // so the block must never be flagged as a dead end.
    use crate::parser::ast::DeclKind;
    let decl = Ast::decl(
        DeclKind::Variable,
        Ast::value(RuntimeValue::IdentPath(vec!["x".to_owned()])),
        Ast::value(RuntimeValue::Int(0)),
    );
    let ast = Ast::block(vec![decl]);
    let errors = dead_end::check(&ast);
    assert_no_errors(&errors);
}

// ---------------------------------------------------------------------------
// Non-block root is silently ignored
// ---------------------------------------------------------------------------

#[test]
fn non_block_root_is_skipped() {
    // A bare return node as root — not a block, so check() is a no-op.
    let ast = return_node();
    assert_no_errors(&dead_end::check(&ast));
}

#[test]
fn non_block_root_value_is_skipped() {
    let ast = Ast::value(RuntimeValue::Int(42));
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// Terminator stops early — unreachable code after terminator is not an error
// ---------------------------------------------------------------------------

#[test]
fn terminator_early_in_block_stops_analysis() {
    // return, then more dialogue — the block terminates; no error expected.
    let ast = Ast::block(vec![return_node(), dialogue(), dialogue()]);
    assert_no_errors(&dead_end::check(&ast));
}

// ---------------------------------------------------------------------------
// Deeply nested blocks
// ---------------------------------------------------------------------------

#[test]
fn deeply_nested_block_without_terminator_surfaces_error() {
    // top → label → if → then_block → if → then_block (no terminator, no else)
    // Flow lives inside a LabeledBlock, not directly in the root Block.
    let deepest = Ast::block(vec![dialogue()]);
    let inner_if = Ast::if_stmt(Ast::value(RuntimeValue::Bool(true)), deepest, None);
    let outer_if = Ast::if_stmt(
        Ast::value(RuntimeValue::Bool(false)),
        Ast::block(vec![inner_if]),
        None,
    );
    let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![outer_if]));
    let ast = Ast::block(vec![label]);
    // The labeled block ends up MayTerminate → dead end for "scene".
    let errors = dead_end::check(&ast);
    assert!(!errors.is_empty(), "expected at least 1 error");
    assert!(
        has_dead_end_at(&errors, "scene"),
        "expected dead-end error mentioning 'scene', got: {errors:?}"
    );
}
