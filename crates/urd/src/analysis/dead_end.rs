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
//! When a terminator is found, any subsequent `LabeledBlock` statements are
//! still visited — they are independent jump targets that can be reached from
//! anywhere in the script regardless of their source position.
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
/// A root [`AstContent::Block`] is a **definitions container** (structs, enums,
/// consts, imports, and labeled blocks).  It carries no flow of its own, so the
/// block itself is never a dead end.  We still recurse into it so that any
/// nested [`AstContent::LabeledBlock`] children are checked individually.
///
/// A root [`AstContent::LabeledBlock`] *does* carry flow and is checked
/// normally.  All other root shapes are silently skipped.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors: Vec<AnalysisError> = Vec::new();

    match ast.content() {
        AstContent::Block(_) => {
            // The top-level block is purely a definitions container; `top_level::check`
            // already enforces that no flow statements appear here.  Dead-end
            // detection therefore does NOT apply to the block itself — there is
            // nothing to "fall off" from.  We still recurse so that LabeledBlock
            // children (which DO carry flow) are checked individually.
            let loc = NodeDescription::top_level();
            termination_of(ast, &mut errors, ast.span(), &loc);
            // Intentionally do NOT push a DeadEnd for the top-level block.
        }
        AstContent::LabeledBlock { label, .. } => {
            // `termination_of` already emits the label-level dead-end (or
            // terminal menu option dead-ends) for labeled blocks. Keep it as
            // the single authoritative emitter to avoid duplicate diagnostics
            // when the labeled block itself is the analysis root.
            let loc = NodeDescription::label(label);
            let _ = termination_of(ast, &mut errors, ast.span(), &loc);
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
/// If `block` ends with a `Menu` node (the last non-`LabeledBlock` statement),
/// return a reference to that `Menu` node.  Used by `LabeledBlock` to emit
/// per-option diagnostics only when the menu is known to be in a terminal
/// position (i.e. nothing after it provides a terminator).
fn last_menu_in_block(block: &Ast) -> Option<&Ast> {
    let stmts = match block.content() {
        AstContent::Block(s) => s,
        _ => return None,
    };
    // Walk forward, skipping trailing LabeledBlock nodes (they are jump
    // targets, not inline flow), and return the last inline statement if it
    // is a Menu.
    stmts
        .iter()
        .rev()
        .find(|s| !matches!(s.content(), AstContent::LabeledBlock { .. }))
        .and_then(|s| {
            if matches!(s.content(), AstContent::Menu { .. }) {
                Some(s)
            } else {
                None
            }
        })
}

/// Emit one [`AnalysisError::DeadEnd`] for each option in `menu_node` whose
/// content does not terminate.  Only called when the menu is the final
/// statement in a labeled block that is itself open — i.e. we know there are
/// no statements after the menu that could provide a terminator.
///
/// This gives precise per-option diagnostics without false positives.
fn emit_terminal_menu_errors(
    menu_node: &Ast,
    errors: &mut Vec<AnalysisError>,
    parent_span: SimpleSpan,
    location: &NodeDescription,
) {
    let options = match menu_node.content() {
        AstContent::Menu { options } => options,
        _ => return,
    };
    for opt in options {
        let opt_label: &str = match opt.content() {
            AstContent::MenuOption { label, .. } => label.as_str(),
            _ => "(unknown option)",
        };
        let opt_loc = NodeDescription::menu_option(opt_label, location);
        let opt_span = {
            let s = opt.span();
            if s.start == 0 && s.end == 0 {
                parent_span
            } else {
                s
            }
        };
        // Re-evaluate without pushing new nested errors (they were already
        // collected during the main termination_of pass).
        let t = termination_of(opt, &mut Vec::new(), opt_span, location);
        if t != Termination::Terminates {
            errors.push(AnalysisError::DeadEnd {
                span: opt_span,
                description: opt_loc,
            });
        }
    }
}

fn termination_of(
    ast: &Ast,
    errors: &mut Vec<AnalysisError>,
    parent_span: SimpleSpan,
    location: &NodeDescription,
) -> Termination {
    // Resolve the span to use for any errors emitted for this node.
    let span = {
        let s = ast.span();
        if s.start == 0 && s.end == 0 {
            parent_span
        } else {
            s
        }
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
        AstContent::Call { func_path, .. } => match extract_call_name(func_path) {
            Some("end!") | Some("todo!") => Termination::Terminates,
            _ => Termination::Open,
        },

        // ── Block ─────────────────────────────────────────────────────────
        AstContent::Block(stmts) => {
            if stmts.is_empty() {
                return Termination::Open;
            }

            let mut last = Termination::Open;
            let mut terminated = false;

            for stmt in stmts {
                if terminated {
                    // A terminator was already found — remaining inline code
                    // is unreachable and not checked.  However, LabeledBlocks
                    // are jump targets reachable from anywhere in the script,
                    // so we must still visit them even when they appear after
                    // a `jump` or `return`.
                    if matches!(stmt.content(), AstContent::LabeledBlock { .. }) {
                        termination_of(stmt, errors, span, location);
                    }
                    continue;
                }

                let t = termination_of(stmt, errors, span, location);
                if t == Termination::Terminates {
                    terminated = true;
                }
                last = t;
            }

            if terminated {
                Termination::Terminates
            } else {
                // After exhausting all statements without hitting a hard
                // terminator: if the last one was MayTerminate, propagate
                // that; otherwise the block is Open.
                match last {
                    Termination::MayTerminate => Termination::MayTerminate,
                    _ => Termination::Open,
                }
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
            if t != Termination::Terminates {
                // If the block ends with a menu, emit per-option errors rather
                // than (or in addition to) a coarse label-level error.  This
                // only happens here — after we know no statement following the
                // menu provides a terminator — so there are no false positives
                // from menus that are followed by a `jump` or `return`.
                if let Some(menu) = last_menu_in_block(block) {
                    emit_terminal_menu_errors(menu, errors, block_span, &inner_loc);
                } else {
                    errors.push(AnalysisError::DeadEnd {
                        span: block_span,
                        description: inner_loc,
                    });
                }
            }
            t
        }

        // ── If ────────────────────────────────────────────────────────────
        AstContent::If {
            then_block,
            else_block: Some(else_block),
            ..
        } => {
            let t_then = termination_of(then_block, errors, span, location);
            let t_else = termination_of(else_block, errors, span, location);

            match (t_then, t_else) {
                (Termination::Terminates, Termination::Terminates) => Termination::Terminates,
                (Termination::Terminates, _) => {
                    // then-branch terminates but else-branch does not — flag it.
                    let else_span = {
                        let s = else_block.span();
                        if s.start == 0 && s.end == 0 { span } else { s }
                    };
                    errors.push(AnalysisError::DeadEnd {
                        span: else_span,
                        description: NodeDescription(format!("else branch in {location}")),
                    });
                    Termination::MayTerminate
                }
                (_, Termination::Terminates) => {
                    // else-branch terminates but then-branch does not — flag it.
                    let then_span = {
                        let s = then_block.span();
                        if s.start == 0 && s.end == 0 { span } else { s }
                    };
                    errors.push(AnalysisError::DeadEnd {
                        span: then_span,
                        description: NodeDescription(format!("if branch in {location}")),
                    });
                    Termination::MayTerminate
                }
                (Termination::Open, Termination::Open) => Termination::Open,
                _ => Termination::MayTerminate,
            }
        }

        AstContent::If {
            then_block,
            else_block: None,
            ..
        } => {
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

            // Recurse into every option to surface errors nested *inside*
            // option bodies (e.g. a nested if-without-else that itself dead-
            // ends).  Per-option dead-end errors for the options themselves are
            // NOT emitted here — we don't yet know whether statements after
            // this menu in the enclosing block will provide a terminator.
            // `emit_terminal_menu_errors` (called from `LabeledBlock` when
            // appropriate) handles that responsibility.
            let mut all_terminate = true;
            for opt in options {
                let opt_span = {
                    let s = opt.span();
                    if s.start == 0 && s.end == 0 { span } else { s }
                };
                let t = termination_of(opt, errors, opt_span, location);
                if t != Termination::Terminates {
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
        AstContent::MenuOption { content, .. } => termination_of(content, errors, span, location),

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
        | AstContent::StructDecl { .. }
        | AstContent::DecoratorDef { .. }
        | AstContent::FnDef { .. }
        | AstContent::Subscript { .. }
        | AstContent::SubscriptAssign { .. }
        | AstContent::Import { .. }
        | AstContent::ExternDeclaration { .. } => Termination::Open,
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
mod tests {
    use super::*;
    use crate::parser::ast::{MatchArm, MatchPattern};
    use crate::runtime::value::RuntimeValue;
    use chumsky::span::{SimpleSpan, Span};

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
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    // ── Jump ─────────────────────────────────────────────────────────────────

    #[test]
    fn one_way_jump_terminates() {
        let ast = jump_one_way("scene_b");
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn jump_and_return_is_open() {
        let ast = jump_and_return("sub");
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty());
    }

    // ── LetCall ──────────────────────────────────────────────────────────────

    #[test]
    fn let_call_is_open() {
        let ast = Ast::let_call("result".to_owned(), "sub".to_owned());
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty());
    }

    // ── end! / todo! calls ───────────────────────────────────────────────────

    #[test]
    fn end_call_terminates() {
        let ast = end_call();
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn todo_call_terminates() {
        let ast = todo_call();
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn generic_call_is_open() {
        let func = Ast::value(RuntimeValue::IdentPath(vec!["say".to_owned()]));
        let ast = Ast::call(func, Ast::expr_list(vec![]));
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
    }

    // ── Block ────────────────────────────────────────────────────────────────

    #[test]
    fn empty_block_is_open() {
        let ast = Ast::block(vec![]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_ending_with_return_terminates() {
        let ast = Ast::block(vec![dialogue_node(), return_node()]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_ending_with_jump_terminates() {
        let ast = Ast::block(vec![dialogue_node(), jump_one_way("end")]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_ending_with_end_call_terminates() {
        let ast = Ast::block(vec![dialogue_node(), end_call()]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn block_with_only_dialogue_is_open() {
        let ast = Ast::block(vec![dialogue_node()]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
        assert!(errors.is_empty()); // no nested errors; the open is surfaced by the caller
    }

    #[test]
    fn block_terminates_early_on_first_terminator() {
        // return is the first statement; remaining statements are unreachable
        let ast = Ast::block(vec![return_node(), dialogue_node(), dialogue_node()]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    // ── LabeledBlock ─────────────────────────────────────────────────────────

    #[test]
    fn labeled_block_with_return_terminates_no_error() {
        let inner = Ast::block(vec![return_node()]);
        let ast = Ast::labeled_block("intro".to_owned(), inner);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn labeled_block_without_terminator_pushes_error() {
        let inner = Ast::block(vec![dialogue_node()]);
        let ast = Ast::labeled_block("intro".to_owned(), inner);
        let mut errors = vec![];
        let _t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
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
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn if_without_else_is_may_terminate() {
        let cond = Ast::value(RuntimeValue::Bool(true));
        let then_b = Ast::block(vec![return_node()]);
        let ast = Ast::if_stmt(cond, then_b, None);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
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
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::MayTerminate);
    }

    #[test]
    fn if_both_open_is_open() {
        let cond = Ast::value(RuntimeValue::Bool(true));
        let then_b = Ast::block(vec![dialogue_node()]);
        let else_b = Ast::block(vec![dialogue_node()]);
        let ast = Ast::if_stmt(cond, then_b, Some(else_b));
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
    }

    // ── Menu ─────────────────────────────────────────────────────────────────

    #[test]
    fn menu_all_options_terminate_terminates() {
        let opt_a = Ast::menu_option("A".to_owned(), Ast::block(vec![return_node()]));
        let opt_b = Ast::menu_option("B".to_owned(), Ast::block(vec![end_call()]));
        let ast = Ast::menu(vec![opt_a, opt_b]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Terminates);
        assert!(errors.is_empty());
    }

    #[test]
    fn menu_one_option_open_is_open_without_errors_from_menu_itself() {
        // termination_of(Menu) never emits per-option errors — that is the
        // responsibility of emit_terminal_menu_errors, called only from
        // LabeledBlock when the menu is known to be in a terminal position.
        let opt_a = Ast::menu_option("Yes".to_owned(), Ast::block(vec![return_node()]));
        let opt_b = Ast::menu_option("No".to_owned(), Ast::block(vec![dialogue_node()]));
        let ast = Ast::menu(vec![opt_a, opt_b]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
        assert!(
            errors.is_empty(),
            "termination_of(Menu) must not emit per-option errors: {errors:?}"
        );
    }

    #[test]
    fn menu_all_options_open_is_open_without_errors() {
        // Uniformly-open menu (all options show dialogue and fall through).
        // No errors from the menu node itself.
        let opt_a = Ast::menu_option("Browse".to_owned(), Ast::block(vec![dialogue_node()]));
        let opt_b = Ast::menu_option("Leave".to_owned(), Ast::block(vec![dialogue_node()]));
        let ast = Ast::menu(vec![opt_a, opt_b]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
        assert!(
            errors.is_empty(),
            "expected no per-option errors for uniformly-open menu, got: {errors:?}"
        );
    }

    // -- emit_terminal_menu_errors / LabeledBlock integration ----------------

    /// Build a LabeledBlock whose body is exactly the given statements.
    fn labeled_block_with_stmts(label: &str, stmts: Vec<Ast>) -> Ast {
        Ast::labeled_block(label.to_owned(), Ast::block(stmts))
    }

    #[test]
    fn terminal_menu_asymmetric_emits_per_option_errors() {
        // Label block whose only statement is an asymmetric menu (one option
        // terminates, one does not).  Because nothing follows the menu,
        // emit_terminal_menu_errors should fire for the open option.
        let opt_yes = Ast::menu_option("Yes".to_owned(), Ast::block(vec![return_node()]));
        let opt_no = Ast::menu_option("No".to_owned(), Ast::block(vec![dialogue_node()]));
        let ast = labeled_block_with_stmts("start", vec![Ast::menu(vec![opt_yes, opt_no])]);
        let errors = super::super::dead_end::check(&ast);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::DeadEnd { description, .. } if description.0.contains("No")
            )),
            "expected DeadEnd for 'No' option, got: {errors:?}"
        );
    }

    #[test]
    fn terminal_menu_uniformly_open_emits_per_option_errors() {
        // All options are open and the menu is the last statement — every
        // option should be flagged.
        let opt_a = Ast::menu_option("A".to_owned(), Ast::block(vec![dialogue_node()]));
        let opt_b = Ast::menu_option("B".to_owned(), Ast::block(vec![dialogue_node()]));
        let ast = labeled_block_with_stmts("start", vec![Ast::menu(vec![opt_a, opt_b])]);
        let errors = super::super::dead_end::check(&ast);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::DeadEnd { description, .. } if description.0.contains('A')
            )),
            "expected DeadEnd for option A, got: {errors:?}"
        );
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::DeadEnd { description, .. } if description.0.contains('B')
            )),
            "expected DeadEnd for option B, got: {errors:?}"
        );
    }

    #[test]
    fn menu_followed_by_jump_no_per_option_errors() {
        // Menu (asymmetric: opt A terminates, opts B/C open) followed by
        // `jump next` in the same label.  The jump covers the open options
        // so there must be zero errors.
        let opt_a = Ast::menu_option("A".to_owned(), Ast::block(vec![return_node()]));
        let opt_b = Ast::menu_option("B".to_owned(), Ast::block(vec![dialogue_node()]));
        let opt_c = Ast::menu_option("C".to_owned(), Ast::block(vec![dialogue_node()]));
        let ast = labeled_block_with_stmts(
            "market",
            vec![
                Ast::menu(vec![opt_a, opt_b, opt_c]),
                Ast::jump_stmt("next".to_owned(), false),
            ],
        );
        let errors = super::super::dead_end::check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors when menu is followed by a jump, got: {errors:?}"
        );
    }

    #[test]
    fn menu_followed_by_jump_uniformly_open_no_errors() {
        // All options open, but a jump follows the menu — no errors.
        let opt_a = Ast::menu_option("Browse".to_owned(), Ast::block(vec![dialogue_node()]));
        let opt_b = Ast::menu_option("Leave".to_owned(), Ast::block(vec![dialogue_node()]));
        let ast = labeled_block_with_stmts(
            "market",
            vec![
                Ast::menu(vec![opt_a, opt_b]),
                Ast::jump_stmt("village_square".to_owned(), false),
            ],
        );
        let errors = super::super::dead_end::check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors when all-open menu is followed by a jump, got: {errors:?}"
        );
    }

    #[test]
    fn menu_empty_is_open() {
        let ast = Ast::menu(vec![]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
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
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
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
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
        assert_eq!(t, Termination::Open);
        // No immediate DeadEnd pushed from inside match (the caller handles it)
        assert!(errors.is_empty());
    }

    #[test]
    fn match_empty_arms_is_open() {
        let scrutinee = Ast::value(RuntimeValue::IdentPath(vec!["x".to_owned()]));
        let ast = Ast::match_stmt(scrutinee, vec![]);
        let mut errors = vec![];
        let t = termination_of(
            &ast,
            &mut errors,
            SimpleSpan::new((), 0..0),
            &NodeDescription::top_level(),
        );
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
    fn check_empty_block_no_error() {
        // A top-level block is a definitions container, not a flow block.
        // An empty one (or one with only definitions) must never be flagged.
        let ast = Ast::block(vec![]);
        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors for empty top-level block, got: {errors:?}"
        );
    }

    #[test]
    fn check_definitions_only_block_no_error() {
        // Structs, enums, and consts have no flow — the top-level block must
        // never be reported as a dead end regardless of how many definitions it
        // contains.
        let ast = Ast::block(vec![
            Ast::struct_decl("Character".to_owned(), vec![]),
            Ast::enum_decl(
                "Faction".to_owned(),
                vec![
                    ("Guild".to_owned(), chumsky::span::SimpleSpan::new((), 0..0)),
                    (
                        "Empire".to_owned(),
                        chumsky::span::SimpleSpan::new((), 0..0),
                    ),
                ],
            ),
            Ast::decl(
                crate::parser::ast::DeclKind::Constant,
                Ast::value(RuntimeValue::IdentPath(vec!["narrator".to_owned()])),
                Ast::value(RuntimeValue::Str(
                    crate::lexer::strings::ParsedString::new_plain("Narrator"),
                )),
            ),
        ]);
        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors for definitions-only block, got: {errors:?}"
        );
    }

    #[test]
    fn check_labeled_block_without_terminator_reports_one_error() {
        // Root labeled blocks should be diagnosed exactly once by
        // `termination_of`, which is the authoritative emitter.
        let inner = Ast::block(vec![dialogue_node()]);
        let ast = Ast::labeled_block("scene".to_owned(), inner);
        let errors = check(&ast);

        assert_eq!(
            errors.len(),
            1,
            "expected exactly one dead-end diagnostic, got: {errors:?}"
        );
        match &errors[0] {
            AnalysisError::DeadEnd { description, .. } => {
                assert!(
                    description.0.contains("scene"),
                    "expected a dead-end error mentioning 'scene', got: {description:?}"
                );
            }
            other => panic!("expected DeadEnd, got: {other:?}"),
        }
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
    fn labeled_block_after_top_level_jump_is_flagged() {
        // This is the cave.urd pattern: `jump start` appears early, putting
        // all subsequent labeled blocks outside the reach of the sequential
        // block walker.  They must still be checked independently.
        let ast = Ast::block(vec![
            jump_one_way("start"),
            Ast::labeled_block("start".to_string(), Ast::block(vec![return_node()])),
            Ast::labeled_block("_end".to_string(), Ast::block(vec![])), // empty — no terminator
        ]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected exactly 1 error; got: {errors:?}");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, AnalysisError::DeadEnd { description, .. }
                if description.0.contains("_end"))),
            "expected a dead-end error for label '_end'; got: {errors:?}"
        );
    }

    #[test]
    fn labeled_block_after_top_level_jump_with_terminator_is_clean() {
        // A properly-terminated label appearing after a top-level jump
        // must NOT produce an error.
        let ast = Ast::block(vec![
            jump_one_way("start"),
            Ast::labeled_block("start".to_string(), Ast::block(vec![return_node()])),
        ]);
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn multiple_labels_after_jump_all_checked() {
        // All labels after a jump are visited; only the open ones are reported.
        let ast = Ast::block(vec![
            jump_one_way("a"),
            Ast::labeled_block("a".to_string(), Ast::block(vec![return_node()])), // ok
            Ast::labeled_block("b".to_string(), Ast::block(vec![end_call()])),    // ok
            Ast::labeled_block("dead".to_string(), Ast::block(vec![])),           // error
        ]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected exactly 1 error; got: {errors:?}");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, AnalysisError::DeadEnd { description, .. }
                if description.0.contains("dead"))),
            "expected dead-end for label 'dead'; got: {errors:?}"
        );
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
                assert!(
                    description.0.contains("sub"),
                    "expected 'sub' in '{}'",
                    description.0
                );
            }
            other => panic!("unexpected: {other:?}"),
        }
    }
}
