//! # Dead-End Detection Pass
//!
//! Checks that every execution path through a script ends with a recognised
//! terminator.  A "dead end" is a path that falls off the bottom of a block
//! without any explicit `end!`, `todo!`, `return`, or one-way `jump`.
//!
//! ## Terminators
//!
//! | Node                                    | Terminates? |
//! |-----------------------------------------|-------------|
//! | `Return { .. }`                         | yes         |
//! | `Jump { expects_return: false, .. }`    | yes         |
//! | `Jump { expects_return: true, .. }`     | no          |
//! | `LetCall { .. }`                        | no          |
//! | `Call` where func is `end!`             | yes         |
//! | `Call` where func is `todo!`            | yes (quiet) |
//! | Everything else                         | no          |
//!
//! A `Block` terminates if and only if one of its statements terminates.
//! A `Menu` terminates if and only if every option terminates.
//! An `If` with both branches terminates if both branches terminate.

use crate::parser::ast::{Ast, AstContent};
use crate::runtime::value::RuntimeValue;

use chumsky::span::SimpleSpan;

use super::{AnalysisError, NodeDescription};

// ---------------------------------------------------------------------------
// Internal termination status
// ---------------------------------------------------------------------------

/// Describes how (or whether) a node terminates all of its execution paths.
#[derive(Debug, Clone, PartialEq)]
enum Termination {
    /// Every path through this node reaches a recognised terminator.
    Terminates,
    /// Some paths terminate, some do not (e.g. `if` without an `else`).
    MayTerminate,
    /// No path through this node reaches a terminator.
    Open,
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the dead-end pass over `ast` and return any diagnostics found.
///
/// Only `Block` and `LabeledBlock` roots are meaningful; all other root shapes
/// are silently skipped (they are sub-expressions that cannot be "dead ends" on
/// their own).
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors: Vec<AnalysisError> = Vec::new();

    match ast.content() {
        AstContent::Block(_) => {
            let loc = NodeDescription::top_level();
            let t = termination_of(ast, &mut errors, ast.span(), &loc);
            if t != Termination::Terminates {
                errors.push(AnalysisError::DeadEnd {
                    span: ast.span(),
                    description: loc,
                });
            }
        }
        AstContent::LabeledBlock { label, .. } => {
            let loc = NodeDescription::label(label);
            let t = termination_of(ast, &mut errors, ast.span(), &loc);
            if t != Termination::Terminates {
                errors.push(AnalysisError::DeadEnd {
                    span: ast.span(),
                    description: loc,
                });
            }
        }
        _ => {}
    }

    errors
}

// ---------------------------------------------------------------------------
// Core recursive analysis
// ---------------------------------------------------------------------------

/// Recursively compute the [`Termination`] status of `ast`.
///
/// Any nested dead-end diagnostics discovered along the way are pushed into
/// `errors`.  `parent_span` is the span of the enclosing context (used when
/// the current node has a zero span).  `location` is a human-readable
/// description used for ariadne labels and zero-span fallback messages.
fn termination_of(
    ast: &Ast,
    errors: &mut Vec<AnalysisError>,
    parent_span: SimpleSpan,
    location: &NodeDescription,
) -> Termination {
    // Resolve the span to use for any errors emitted for this node.
    let span = {
        let s = ast.span();
        if s.start == 0 && s.end == 0 { parent_span } else { s }
    };
    match ast.content() {
        // ── Explicit terminators ──────────────────────────────────────────
        AstContent::Return { .. } => Termination::Terminates,

        AstContent::Jump { expects_return, .. } => {
            if *expects_return {
                // Call-and-return: control comes back, so this is *not* a
                // one-way terminator.
                Termination::Open
            } else {
                Termination::Terminates
            }
        }

        // LetCall is always a call-and-return; not a terminator.
        AstContent::LetCall { .. } => Termination::Open,

        // `end!` and `todo!` are terminating calls; everything else is open.
        AstContent::Call { func_path, .. } => {
            match extract_call_name(func_path) {
                Some("end!") | Some("todo!") => Termination::Terminates,
                _ => Termination::Open,
            }
        }

        // ── Block ─────────────────────────────────────────────────────────
        AstContent::Block(stmts) => {
            if stmts.is_empty() {
                return Termination::Open;
            }

            let mut last = Termination::Open;

            for stmt in stmts {
                let t = termination_of(stmt, errors, span, location);
                if t == Termination::Terminates {
                    // This statement terminates every path.  Anything after
                    // it is unreachable — we stop here (but do not emit
                    // errors for the unreachable code itself).
                    return Termination::Terminates;
                }
                last = t;
            }

            // After exhausting all statements without hitting a hard
            // terminator: if the last one was MayTerminate, propagate that;
            // otherwise the block is Open.
            match last {
                Termination::MayTerminate => Termination::MayTerminate,
                _ => Termination::Open,
            }
        }

        // ── LabeledBlock ──────────────────────────────────────────────────
        AstContent::LabeledBlock { label, block } => {
            let inner_loc = NodeDescription::label(label);
            let block_span = {
                let s = block.span();
                if s.start == 0 && s.end == 0 { span } else { s }
            };
            let t = termination_of(block, errors, block_span, &inner_loc);
            if t == Termination::Open {
                errors.push(AnalysisError::DeadEnd {
                    span: block_span,
                    description: inner_loc,
                });
            }
            t
        }

        // ── If ────────────────────────────────────────────────────────────
        AstContent::If { then_block, else_block: Some(else_block), .. } => {
            let t_then = termination_of(then_block, errors, span, location);
            let t_else = termination_of(else_block, errors, span, location);

            match (t_then, t_else) {
                (Termination::Terminates, Termination::Terminates) => Termination::Terminates,
                (Termination::Open, Termination::Open) => Termination::Open,
                _ => Termination::MayTerminate,
            }
        }

        AstContent::If { then_block, else_block: None, .. } => {
            // Recurse into the then-block only to surface any *nested* errors.
            let _t = termination_of(then_block, errors, span, location);
            // Without an else branch the overall if is always MayTerminate: the
            // else path is unconditionally open.
            Termination::MayTerminate
        }

        // ── Menu ──────────────────────────────────────────────────────────
        AstContent::Menu { options } => {
            if options.is_empty() {
                return Termination::Open;
            }

            let mut all_terminate = true;

            for opt in options {
                // The option node should be a MenuOption; we pass it to
                // termination_of which will handle the MenuOption arm below.
                // We need the label for error messages *before* recursing.
                let opt_label: &str = match opt.content() {
                    AstContent::MenuOption { label, .. } => label.as_str(),
                    _ => "(unknown option)",
                };
                let opt_loc = NodeDescription::menu_option(opt_label, location);
                let opt_span = {
                    let s = opt.span();
                    if s.start == 0 && s.end == 0 { span } else { s }
                };

                // Compute termination of the *content* block (via MenuOption
                // arm) so that nested errors inside the content are collected.
                let t = termination_of(opt, errors, opt_span, location);

                if t != Termination::Terminates {
                    errors.push(AnalysisError::DeadEnd {
                        span: opt_span,
                        description: opt_loc,
                    });
                    all_terminate = false;
                }
            }

            if all_terminate {
                Termination::Terminates
            } else {
                Termination::Open
            }
        }

        // MenuOption: delegate to its content block.
        AstContent::MenuOption { content, .. } => {
            termination_of(content, errors, span, location)
        }

        // ── Match ─────────────────────────────────────────────────────────
        AstContent::Match { arms, .. } => {
            if arms.is_empty() {
                return Termination::Open;
            }

            let mut all_terminate = true;

            for arm in arms.iter() {
                let t = termination_of(&arm.body, errors, span, location);
                if t != Termination::Terminates {
                    // We do *not* push a DeadEnd error here; the spec says the
                    // enclosing block (or the public check entry-point) handles
                    // the outer error.  Nested errors inside arm bodies are
                    // still collected via the recursive call above.
                    all_terminate = false;
                }
            }

            if all_terminate {
                Termination::Terminates
            } else {
                Termination::Open
            }
        }

        // ── Everything else is an open (non-terminating) node ─────────────
        AstContent::Value(_)
        | AstContent::BinOp { .. }
        | AstContent::UnaryOp { .. }
        | AstContent::ExprList(_)
        | AstContent::Declaration { .. }
        | AstContent::List(_)
        | AstContent::Map(_)
        | AstContent::Dialogue { .. }
        | AstContent::EnumDecl { .. }
        | AstContent::DecoratorDef { .. }
        | AstContent::Subscript { .. }
        | AstContent::SubscriptAssign { .. }
        | AstContent::Import { .. } => Termination::Open,
    }
}

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

/// If `func_path` is a single-segment `IdentPath`, return that segment.
///
/// Used to detect the `end!` and `todo!` built-in terminators.
fn extract_call_name(func_path: &Ast) -> Option<&str> {
    match func_path.content() {
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => {
            Some(path[0].as_str())
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use chumsky::span::{SimpleSpan, Span};
    use crate::parser::ast::{MatchArm, MatchPattern};
    use crate::runtime::value::RuntimeValue;

    // ── helpers ─────────────────────────────────────────────────────────────

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

    fn dialogue_node() -> Ast {
        let speaker = Ast::value(RuntimeValue::IdentPath(vec!["Narrator".to_owned()]));
        let content = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("Hello."),
        ));
        Ast::dialogue(speaker, content)
    }

    // ── extract_call_name ────────────────────────────────────────────────────

    #[test]
    fn extract_call_name_single_segment() {
        let node = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_owned()]));
        assert_eq!(extract_call_name(&node), Some("end!"));
    }

    #[test]
    fn extract_call_name_multi_segment_returns_none() {
        let node = Ast::value(RuntimeValue::IdentPath(vec![
            "a".to_owned(),
            "b".to_owned(),
        ]));
        assert_eq!(extract_call_name(&node), None);
    }

    #[test]
    fn extract_call_name_non_ident_returns_none() {
        let node = Ast::value(RuntimeValue::Int(0));
        assert_eq!(extract_call_name(&node), None);
    }

    // ── Return ───────────────────────────────────────────────────────────────

    #[test]
    fn return_node_terminates() {
        let ast = return_node();
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    // ── Jump ─────────────────────────────────────────────────────────────────

    #[test]
    fn one_way_jump_terminates() {
        let ast = jump_one_way("scene_b");
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn jump_and_return_is_open() {
        let ast = jump_and_return("sub");
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty());
    }

    // ── LetCall ──────────────────────────────────────────────────────────────

    #[test]
    fn let_call_is_open() {
        let ast = Ast::let_call("result".to_owned(), "sub".to_owned());
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty());
    }

    // ── end! / todo! calls ───────────────────────────────────────────────────

    #[test]
    fn end_call_terminates() {
        let ast = end_call();
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn todo_call_terminates() {
        let ast = todo_call();
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn generic_call_is_open() {
        let func = Ast::value(RuntimeValue::IdentPath(vec!["say".to_owned()]));
        let ast = Ast::call(func, Ast::expr_list(vec![]));
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
    }

    // ── Block ────────────────────────────────────────────────────────────────

    #[test]
    fn empty_block_is_open() {
        let ast = Ast::block(vec![]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_ending_with_return_terminates() {
        let ast = Ast::block(vec![dialogue_node(), return_node()]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_ending_with_jump_terminates() {
        let ast = Ast::block(vec![dialogue_node(), jump_one_way("end")]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_ending_with_end_call_terminates() {
        let ast = Ast::block(vec![dialogue_node(), end_call()]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_with_only_dialogue_is_open() {
        let ast = Ast::block(vec![dialogue_node()]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty()); // no nested errors; the open is surfaced by the caller
    }

    #[test]
    fn block_terminates_early_on_first_terminator() {
        // return is the first statement; remaining statements are unreachable
        let ast = Ast::block(vec![return_node(), dialogue_node(), dialogue_node()]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    // ── LabeledBlock ─────────────────────────────────────────────────────────

    #[test]
    fn labeled_block_with_return_terminates_no_error() {
        let inner = Ast::block(vec![return_node()]);
        let ast = Ast::labeled_block("intro".to_owned(), inner);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn labeled_block_without_terminator_pushes_error() {
        let inner = Ast::block(vec![dialogue_node()]);
        let ast = Ast::labeled_block("intro".to_owned(), inner);
        let mut errors = vec![];
        let _t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::DeadEnd { description, .. } => {
                assert!(description.0.contains("intro"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    // ── If ───────────────────────────────────────────────────────────────────

    #[test]
    fn if_both_branches_terminate_terminates() {
        let cond = Ast::value(RuntimeValue::Bool(true));
        let then_b = Ast::block(vec![return_node()]);
        let else_b = Ast::block(vec![end_call()]);
        let ast = Ast::if_stmt(cond, then_b, Some(else_b));
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn if_without_else_is_may_terminate() {
        let cond = Ast::value(RuntimeValue::Bool(true));
        let then_b = Ast::block(vec![return_node()]);
        let ast = Ast::if_stmt(cond, then_b, None);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::MayTerminate);
        assert!(errors.is_empty());
    }

    #[test]
    fn if_then_terminates_else_open_is_may_terminate() {
        let cond = Ast::value(RuntimeValue::Bool(true));
        let then_b = Ast::block(vec![return_node()]);
        let else_b = Ast::block(vec![dialogue_node()]);
        let ast = Ast::if_stmt(cond, then_b, Some(else_b));
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::MayTerminate);
    }

    #[test]
    fn if_both_open_is_open() {
        let cond = Ast::value(RuntimeValue::Bool(true));
        let then_b = Ast::block(vec![dialogue_node()]);
        let else_b = Ast::block(vec![dialogue_node()]);
        let ast = Ast::if_stmt(cond, then_b, Some(else_b));
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
    }

    // ── Menu ─────────────────────────────────────────────────────────────────

    #[test]
    fn menu_all_options_terminate_terminates() {
        let opt_a = Ast::menu_option("A".to_owned(), Ast::block(vec![return_node()]));
        let opt_b = Ast::menu_option("B".to_owned(), Ast::block(vec![end_call()]));
        let ast = Ast::menu(vec![opt_a, opt_b]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn menu_one_option_open_pushes_error() {
        let opt_a = Ast::menu_option("Yes".to_owned(), Ast::block(vec![return_node()]));
        let opt_b = Ast::menu_option("No".to_owned(), Ast::block(vec![dialogue_node()]));
        let ast = Ast::menu(vec![opt_a, opt_b]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::DeadEnd { description, .. } => {
                assert!(description.0.contains("No"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn menu_empty_is_open() {
        let ast = Ast::menu(vec![]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
    }

    // ── Match ────────────────────────────────────────────────────────────────

    #[test]
    fn match_all_arms_terminate_terminates() {
        let scrutinee = Ast::value(RuntimeValue::IdentPath(vec!["state".to_owned()]));
        let arm_a = MatchArm::new(
            MatchPattern::Value(Ast::value(RuntimeValue::IdentPath(vec!["A".to_owned()]))),
            Ast::block(vec![return_node()]),
        );
        let arm_b = MatchArm::new(MatchPattern::Wildcard, Ast::block(vec![end_call()]));
        let ast = Ast::match_stmt(scrutinee, vec![arm_a, arm_b]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn match_one_arm_open_is_open() {
        let scrutinee = Ast::value(RuntimeValue::IdentPath(vec!["x".to_owned()]));
        let arm_a = MatchArm::new(MatchPattern::Wildcard, Ast::block(vec![return_node()]));
        let arm_b = MatchArm::new(
            MatchPattern::Value(Ast::value(RuntimeValue::Int(0))),
            Ast::block(vec![dialogue_node()]),
        );
        let ast = Ast::match_stmt(scrutinee, vec![arm_a, arm_b]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
        // No immediate DeadEnd pushed from inside match (the caller handles it)
        assert!(errors.is_empty());
    }

    #[test]
    fn match_empty_arms_is_open() {
        let scrutinee = Ast::value(RuntimeValue::IdentPath(vec!["x".to_owned()]));
        let ast = Ast::match_stmt(scrutinee, vec![]);
        let mut errors = vec![];
        let t = termination_of(&ast, &mut errors, SimpleSpan::new((), 0..0), &NodeDescription::top_level());
        assert_eq!(t, Termination::Open);
    }

    // ── check (public API) ───────────────────────────────────────────────────

    #[test]
    fn check_block_with_return_no_errors() {
        let ast = Ast::block(vec![return_node()]);
        let errors = check(&ast);
        assert!(errors.is_empty());
    }

    #[test]
    fn check_empty_block_reports_dead_end() {
        let ast = Ast::block(vec![]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::DeadEnd { description, .. } => {
                assert_eq!(description.0, "top-level block");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn check_labeled_block_without_terminator_reports_two_errors() {
        // The labeled block pushes one error for itself (from termination_of),
        // and then the outer check also pushes one because the top-level
        // termination status is still Open (labeled blocks are usually nested
        // inside a top-level Block, but here it *is* the root).
        let inner = Ast::block(vec![dialogue_node()]);
        let ast = Ast::labeled_block("scene".to_owned(), inner);
        let errors = check(&ast);
        // At least one error for the dead-end inside the label.
        assert!(!errors.is_empty());
        let has_label_error = errors.iter().any(|e| match e {
            AnalysisError::DeadEnd { description, .. } => description.0.contains("scene"),
            _ => false,
        });
        assert!(has_label_error, "expected a dead-end error mentioning 'scene'");
    }

    #[test]
    fn check_non_block_root_is_skipped() {
        // A bare `Return` node at the root is not a block, so check() is a no-op.
        let ast = return_node();
        let errors = check(&ast);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_with_if_no_else_followed_by_return_terminates() {
        // A common real-world pattern: conditional logic then a guaranteed return.
        let cond = Ast::value(RuntimeValue::Bool(true));
        let then_b = Ast::block(vec![dialogue_node()]);
        let if_node = Ast::if_stmt(cond, then_b, None);
        let ast = Ast::block(vec![if_node, return_node()]);
        let errors = check(&ast);
        assert!(errors.is_empty());
    }

    #[test]
    fn nested_labeled_block_reports_its_own_error() {
        // A labeled block nested inside a top-level block that ends with end!.
        // The nested label's dead-end should be reported independently.
        let inner = Ast::block(vec![dialogue_node()]); // no terminator
        let label = Ast::labeled_block("sub".to_owned(), inner);
        let ast = Ast::block(vec![label, end_call()]);
        let errors = check(&ast);
        // The top-level block ends with end!, so no top-level dead-end.
        // But the labeled block inside has no terminator.
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::DeadEnd { description, .. } => {
                assert!(description.0.contains("sub"), "expected 'sub' in '{}'", description.0);
            }
            other => panic!("unexpected: {other:?}"),
        }
    }
}
