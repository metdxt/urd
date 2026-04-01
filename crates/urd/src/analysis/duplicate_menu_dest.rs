//! # Duplicate Menu Destination Analysis Pass
//!
//! Checks that no two options within the same `menu` block have structurally
//! identical bodies.
//!
//! [`AnalysisError::DuplicateMenuDestination`] (warning) is emitted for every
//! pair `(A, B)` where option `B` appears after option `A` in the same menu
//! and `A.content == B.content` (by structural AST equality, ignoring spans).
//!
//! ## Equality semantics
//!
//! [`Ast`] implements [`PartialEq`] by comparing `content` fields only (not
//! spans or decorators), so `==` on two `Ast` nodes is a structural comparison.
//! This means two options that produce the exact same code path — even if written
//! in different positions — will be flagged.
//!
//! ## Walk strategy
//!
//! The pass uses [`walk_ast`] to visit every node.  When a [`AstContent::Menu`]
//! is encountered its options are inspected pairwise.  Nested menus (options
//! that themselves contain menus) are also visited because `walk_ast` recurses
//! into all descendants.

use crate::analysis::AnalysisError;
use crate::parser::ast::{Ast, AstContent, walk_ast};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the duplicate-menu-destination check over `ast` and return any
/// diagnostics found.
///
/// For each [`AstContent::Menu`] node the options are compared pairwise.
/// When two options have `content` fields that are structurally equal
/// (via [`Ast`]'s `PartialEq` impl, which ignores spans), a
/// [`AnalysisError::DuplicateMenuDestination`] is emitted for the *second*
/// (later) option, referencing the label text of both options.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors = Vec::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::Menu { options } = node.content() {
            check_menu_options(options, &mut errors);
        }
    });

    errors
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Emit diagnostics for every (earlier, later) pair of options in `options`
/// that share structurally identical bodies.
///
/// The outer loop index `i` selects the "reference" option; the inner loop
/// index `j > i` looks for duplicates.  Only the first occurrence is used as
/// the `first_option` reference; if there are three identical options (A, B, C)
/// the pass emits `(A, B)` and `(A, C)` — i.e. B and C are each compared
/// against the earliest identical predecessor.
fn check_menu_options(options: &[Ast], errors: &mut Vec<AnalysisError>) {
    // Track which indices have already been flagged as a duplicate of an
    // earlier option.  A flagged option must not itself become the "anchor"
    // (first_option) for a later comparison — otherwise three identical
    // options (A, B, C) would produce three pairs instead of two.
    let mut already_flagged: std::collections::HashSet<usize> = std::collections::HashSet::new();

    for i in 0..options.len() {
        // Skip options that are themselves duplicates of an earlier entry.
        if already_flagged.contains(&i) {
            continue;
        }

        let a = &options[i];
        let (a_label, a_content) = match a.content() {
            AstContent::MenuOption { label, content } => (label.as_str(), content.as_ref()),
            // Guard against malformed ASTs where a non-MenuOption ends up in
            // the options list (should never happen from the parser).
            _ => continue,
        };

        for (j, b) in options.iter().enumerate().skip(i + 1) {
            let (b_label, b_content) = match b.content() {
                AstContent::MenuOption { label, content } => (label.as_str(), content.as_ref()),
                _ => continue,
            };

            // Ast's PartialEq compares `content` only (span-insensitive).
            if a_content == b_content {
                already_flagged.insert(j);
                errors.push(AnalysisError::DuplicateMenuDestination {
                    first_option: a_label.to_owned(),
                    second_option: b_label.to_owned(),
                    span: b.span(),
                });
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;
    use crate::lexer::strings::ParsedString;
    use crate::parser::ast::Ast;
    use crate::runtime::value::RuntimeValue;

    // ── AST builder helpers ──────────────────────────────────────────────────

    fn make_end_call() -> Ast {
        let func = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_owned()]));
        Ast::call(func, Ast::expr_list(vec![]))
    }

    fn make_jump(label: &str) -> Ast {
        Ast::jump_stmt(label.to_owned(), false)
    }

    fn make_option_end(label: &str) -> Ast {
        Ast::menu_option(label.to_owned(), Ast::block(vec![make_end_call()]))
    }

    fn make_option_jump(label: &str, target: &str) -> Ast {
        Ast::menu_option(label.to_owned(), Ast::block(vec![make_jump(target)]))
    }

    fn make_option_dialogue(label: &str, text: &str) -> Ast {
        let speaker = Ast::value(RuntimeValue::IdentPath(vec!["Narrator".to_owned()]));
        let content = Ast::value(RuntimeValue::Str(ParsedString::new_plain(text)));
        let dialogue = Ast::dialogue(speaker, content);
        Ast::menu_option(
            label.to_owned(),
            Ast::block(vec![dialogue, make_end_call()]),
        )
    }

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    // ── No-error cases ───────────────────────────────────────────────────────

    #[test]
    fn distinct_options_are_clean() {
        let ast = parse(
            r#"
label start {
    menu {
        "Yes" { end!() }
        "No"  { jump start }
    }
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn three_distinct_options_are_clean() {
        let ast = parse(
            r#"
label start {
    menu {
        "A" { end!() }
        "B" { jump start }
        "C" { todo!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn empty_menu_no_errors() {
        let ast = Ast::menu(vec![]);
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn single_option_menu_no_errors() {
        let opt = make_option_end("Only");
        let menu = Ast::menu(vec![opt]);
        let errors = check(&menu);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn options_with_same_label_but_different_bodies_are_clean() {
        // Same display label text, but different bodies → not a duplicate body.
        let a = Ast::menu_option("Same label".to_owned(), Ast::block(vec![make_end_call()]));
        let b = Ast::menu_option(
            "Same label".to_owned(),
            Ast::block(vec![make_jump("other")]),
        );
        let menu = Ast::menu(vec![a, b]);
        let errors = check(&menu);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── DuplicateMenuDestination — basic detection ───────────────────────────

    #[test]
    fn two_options_with_identical_bodies_emits_error() {
        let ast = parse(
            r#"
label start {
    menu {
        "Choice A" { end!() }
        "Choice B" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::DuplicateMenuDestination {
                first_option,
                second_option,
                ..
            } => {
                assert_eq!(first_option, "Choice A");
                assert_eq!(second_option, "Choice B");
            }
            other => panic!("expected DuplicateMenuDestination, got: {other:?}"),
        }
    }

    #[test]
    fn duplicate_jump_targets_emit_error() {
        let ast = parse(
            r#"
label start {
    menu {
        "Go north" { jump forest }
        "Go south" { jump forest }
    }
}
label forest { end!() }
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        assert!(
            matches!(&errors[0], AnalysisError::DuplicateMenuDestination { .. }),
            "expected DuplicateMenuDestination, got: {:?}",
            errors[0]
        );
    }

    #[test]
    fn direct_ast_identical_bodies() {
        let a = make_option_end("First");
        let b = make_option_end("Second");
        let menu = Ast::menu(vec![a, b]);
        let errors = check(&menu);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::DuplicateMenuDestination {
                first_option,
                second_option,
                ..
            } => {
                assert_eq!(first_option, "First");
                assert_eq!(second_option, "Second");
            }
            other => panic!("expected DuplicateMenuDestination, got: {other:?}"),
        }
    }

    // ── Three-way duplicates ─────────────────────────────────────────────────

    #[test]
    fn three_identical_options_emit_two_errors() {
        // (A, B) and (A, C) should both be reported.
        let a = make_option_end("A");
        let b = make_option_end("B");
        let c = make_option_end("C");
        let menu = Ast::menu(vec![a, b, c]);
        let errors = check(&menu);
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");

        let pairs: Vec<(&str, &str)> = errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::DuplicateMenuDestination {
                    first_option,
                    second_option,
                    ..
                } => Some((first_option.as_str(), second_option.as_str())),
                _ => None,
            })
            .collect();

        assert!(
            pairs.contains(&("A", "B")),
            "expected (A, B) pair, got: {pairs:?}"
        );
        assert!(
            pairs.contains(&("A", "C")),
            "expected (A, C) pair, got: {pairs:?}"
        );
    }

    #[test]
    fn three_options_two_duplicates_one_unique() {
        // A == C, but B is different.
        let a = make_option_end("A");
        let b = make_option_jump("B", "other_label");
        let c = make_option_end("C");
        let menu = Ast::menu(vec![a, b, c]);
        let errors = check(&menu);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::DuplicateMenuDestination {
                first_option,
                second_option,
                ..
            } => {
                assert_eq!(first_option, "A");
                assert_eq!(second_option, "C");
            }
            other => panic!("expected DuplicateMenuDestination, got: {other:?}"),
        }
    }

    // ── Nested menus ─────────────────────────────────────────────────────────

    #[test]
    fn inner_nested_menu_duplicates_are_reported() {
        // Outer menu: two distinct options.
        // Inner menu (inside option A): two identical options.
        let ast = parse(
            r#"
label start {
    menu {
        "Outer A" {
            menu {
                "Inner X" { end!() }
                "Inner Y" { end!() }
            }
        }
        "Outer B" { jump start }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(
            errors.len(),
            1,
            "expected 1 error (inner menu), got: {errors:?}"
        );
        assert!(
            matches!(&errors[0], AnalysisError::DuplicateMenuDestination { .. }),
            "expected DuplicateMenuDestination, got: {:?}",
            errors[0]
        );
    }

    #[test]
    fn outer_menu_duplicates_and_inner_menu_distinct_reports_outer_only() {
        let a = make_option_end("A");
        let b = make_option_end("B"); // duplicate of A

        // C has an inner menu with two distinct options.
        let inner_opt1 = make_option_end("In1");
        let inner_opt2 = make_option_jump("In2", "somewhere");
        let inner_menu = Ast::menu(vec![inner_opt1, inner_opt2]);
        let c = Ast::menu_option(
            "C".to_owned(),
            Ast::block(vec![inner_menu, make_end_call()]),
        );

        let outer = Ast::menu(vec![a, b, c]);
        let errors = check(&outer);
        // Only (A, B) is a duplicate; inner menu is fine.
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::DuplicateMenuDestination {
                first_option,
                second_option,
                ..
            } => {
                assert_eq!(first_option, "A");
                assert_eq!(second_option, "B");
            }
            other => panic!("expected DuplicateMenuDestination, got: {other:?}"),
        }
    }

    #[test]
    fn outer_and_inner_menus_each_with_duplicates_both_reported() {
        let ast = parse(
            r#"
label start {
    menu {
        "A" {
            menu {
                "Inner P" { end!() }
                "Inner Q" { end!() }
            }
        }
        "B" {
            menu {
                "Inner P" { end!() }
                "Inner Q" { end!() }
            }
        }
    }
}
"#,
        );
        let errors = check(&ast);
        // Each of the two inner menus contributes 1 error.
        // The outer menu: options A and B have identical bodies (same inner menu) →
        // 1 more error. Total: 3.
        let dup_count = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::DuplicateMenuDestination { .. }))
            .count();
        assert!(
            dup_count >= 2,
            "expected at least 2 DuplicateMenuDestination errors, got: {errors:?}"
        );
    }

    // ── Dialogue content inside options ──────────────────────────────────────

    #[test]
    fn options_with_same_dialogue_body_emit_error() {
        let a = make_option_dialogue("Left door", "You enter the left room.");
        let b = make_option_dialogue("Right door", "You enter the left room.");
        let menu = Ast::menu(vec![a, b]);
        let errors = check(&menu);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            AnalysisError::DuplicateMenuDestination { .. }
        ));
    }

    #[test]
    fn options_with_different_dialogue_text_are_clean() {
        let a = make_option_dialogue("Left door", "You enter the left room.");
        let b = make_option_dialogue("Right door", "You enter the right room.");
        let menu = Ast::menu(vec![a, b]);
        let errors = check(&menu);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── Span is set to second (later) option's span ──────────────────────────

    #[test]
    fn error_span_is_nonzero_for_parsed_source() {
        let ast = parse(
            r#"
label start {
    menu {
        "First"  { end!() }
        "Second" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        if let AnalysisError::DuplicateMenuDestination { span, .. } = &errors[0] {
            assert!(
                span.start != 0 || span.end != 0,
                "expected a real source span, got zero span"
            );
        }
    }

    #[test]
    fn error_span_is_zero_for_ast_constructed_node() {
        let a = make_option_end("A");
        let b = make_option_end("B");
        let menu = Ast::menu(vec![a, b]);
        let errors = check(&menu);
        assert_eq!(errors.len(), 1);
        if let AnalysisError::DuplicateMenuDestination { span, .. } = &errors[0] {
            assert_eq!(span.start, 0, "expected zero span for AST-built node");
            assert_eq!(span.end, 0, "expected zero span for AST-built node");
        }
    }

    // ── Display / message smoke tests ────────────────────────────────────────

    #[test]
    fn error_message_contains_both_option_labels() {
        use chumsky::span::{SimpleSpan, Span};
        let err = AnalysisError::DuplicateMenuDestination {
            first_option: "Alpha".to_owned(),
            second_option: "Beta".to_owned(),
            span: SimpleSpan::new((), 0..0),
        };
        let msg = err.to_string();
        assert!(
            msg.contains("Alpha"),
            "message should contain first option name, got: {msg}"
        );
        assert!(
            msg.contains("Beta"),
            "message should contain second option name, got: {msg}"
        );
    }

    #[test]
    fn error_message_indicates_duplicate() {
        use chumsky::span::{SimpleSpan, Span};
        let err = AnalysisError::DuplicateMenuDestination {
            first_option: "X".to_owned(),
            second_option: "Y".to_owned(),
            span: SimpleSpan::new((), 0..0),
        };
        let msg = err.to_string();
        assert!(
            msg.contains("duplicate") || msg.contains("identical") || msg.contains("same"),
            "message should indicate duplication, got: {msg}"
        );
    }

    // ── No cross-menu contamination ──────────────────────────────────────────

    #[test]
    fn identical_options_in_different_menus_do_not_cross_contaminate() {
        // Two separate menus each having distinct options — both clean.
        // Shared bodies across menus must NOT be flagged (only same-menu pairs matter).
        let ast = parse(
            r#"
label start {
    menu {
        "A" { end!() }
        "B" { jump start }
    }
    menu {
        "C" { end!() }
        "D" { jump start }
    }
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn duplicate_in_first_menu_does_not_affect_second_menu_reporting() {
        // First menu: A and B are identical → 1 error.
        // Second menu: C and D are distinct → clean.
        let ast = parse(
            r#"
label start {
    menu {
        "A" { end!() }
        "B" { end!() }
    }
    menu {
        "C" { end!() }
        "D" { jump start }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected exactly 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::DuplicateMenuDestination {
                first_option,
                second_option,
                ..
            } => {
                assert_eq!(first_option, "A");
                assert_eq!(second_option, "B");
            }
            other => panic!("expected DuplicateMenuDestination, got: {other:?}"),
        }
    }
}
