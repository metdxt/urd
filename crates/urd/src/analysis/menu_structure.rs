//! # Menu Structure Analysis Pass
//!
//! Checks that every `menu` block has a reasonable number of options:
//!
//! - [`AnalysisError::EmptyMenu`] (error) — the `menu` has zero *real*
//!   (non-default) options.  The player is presented with a choice screen they
//!   can never interact with.
//!
//! - [`AnalysisError::SingleOptionMenu`] (warning) — the `menu` has exactly
//!   one *real* option **and** no default/wildcard fallback.  The player has no
//!   real choice; a simple dialogue line would be semantically equivalent.
//!
//! - [`AnalysisError::MultipleMenuDefaults`] (error) — the `menu` has more
//!   than one `_ { … }` wildcard option.  At most one default is allowed.

use crate::analysis::AnalysisError;
use crate::parser::ast::{Ast, AstContent, walk_ast};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the menu-structure check over `ast` and return any diagnostics found.
///
/// Walks the entire AST and emits:
/// - [`AnalysisError::EmptyMenu`] for every [`AstContent::Menu`] with zero
///   non-default options.
/// - [`AnalysisError::SingleOptionMenu`] for every [`AstContent::Menu`] with
///   exactly one non-default option **and** no default fallback.
/// - [`AnalysisError::MultipleMenuDefaults`] for every [`AstContent::Menu`]
///   with more than one `_` wildcard option.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors = Vec::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::Menu { options } = node.content() {
            let default_count = options
                .iter()
                .filter(|opt| {
                    matches!(
                        opt.content(),
                        AstContent::MenuOption {
                            is_default: true,
                            ..
                        }
                    )
                })
                .count();

            let real_options = options
                .iter()
                .filter(|opt| {
                    !matches!(
                        opt.content(),
                        AstContent::MenuOption {
                            is_default: true,
                            ..
                        }
                    )
                })
                .count();

            // A menu may have at most one wildcard/default option.
            if default_count > 1 {
                errors.push(AnalysisError::MultipleMenuDefaults { span: node.span() });
            }

            match real_options {
                // No real options at all — even if a default exists, the
                // player never sees a meaningful choice.
                0 => errors.push(AnalysisError::EmptyMenu { span: node.span() }),
                // Exactly one real option with no default fallback — the
                // player has no real choice.  If a default *is* present the
                // single concrete option is still meaningful (it becomes the
                // only *named* path), so we do not warn.
                1 if default_count == 0 => {
                    errors.push(AnalysisError::SingleOptionMenu { span: node.span() })
                }
                _ => {}
            }
        }
    });

    errors
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
    use chumsky::span::{SimpleSpan, Span};

    fn zero_span() -> SimpleSpan {
        SimpleSpan::new((), 0..0)
    }

    // ── AST builder helpers ──────────────────────────────────────────────────

    fn make_end_call() -> Ast {
        let func = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_owned()]));
        Ast::call(func, Ast::expr_list(vec![]))
    }

    fn make_option(label: &str) -> Ast {
        Ast::menu_option(label.to_owned(), Ast::block(vec![make_end_call()]), false)
    }

    fn make_default_option() -> Ast {
        Ast::menu_default_option(Ast::block(vec![make_end_call()]))
    }

    fn make_dialogue(text: &str) -> Ast {
        let speaker = Ast::value(RuntimeValue::IdentPath(vec!["Narrator".to_owned()]));
        let content = Ast::value(RuntimeValue::Str(ParsedString::new_plain(text)));
        Ast::dialogue(speaker, content)
    }

    // ── parse helpers ────────────────────────────────────────────────────────

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    // ── No-error cases ───────────────────────────────────────────────────────

    #[test]
    fn two_option_menu_is_clean() {
        let ast = parse(
            r#"
label start {
    menu {
        "Yes" { end!() }
        "No"  { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn three_option_menu_is_clean() {
        let ast = parse(
            r#"
label start {
    menu {
        "A" { end!() }
        "B" { end!() }
        "C" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn no_menu_at_all_is_clean() {
        let ast = parse(
            r#"
label start {
    Narrator: "Hello."
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── EmptyMenu ────────────────────────────────────────────────────────────

    #[test]
    fn empty_menu_emits_error() {
        let ast = parse(
            r#"
label start {
    menu {}
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected exactly 1 error, got: {errors:?}");
        assert!(
            matches!(&errors[0], AnalysisError::EmptyMenu { .. }),
            "expected EmptyMenu, got: {:?}",
            errors[0]
        );
    }

    #[test]
    fn empty_menu_ast_direct() {
        // Confirm via direct AST construction (no span).
        let menu = Ast::menu(vec![]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyMenu { .. }));
    }

    #[test]
    fn multiple_empty_menus_all_reported() {
        let ast = parse(
            r#"
label start {
    menu {}
    menu {}
}
"#,
        );
        let errors = check(&ast);
        let empty_count = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::EmptyMenu { .. }))
            .count();
        assert_eq!(
            empty_count, 2,
            "expected 2 EmptyMenu errors, got: {errors:?}"
        );
    }

    // ── SingleOptionMenu ─────────────────────────────────────────────────────

    #[test]
    fn single_option_menu_emits_warning() {
        let ast = parse(
            r#"
label start {
    menu {
        "Only choice" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(
            errors.len(),
            1,
            "expected exactly 1 warning, got: {errors:?}"
        );
        assert!(
            matches!(&errors[0], AnalysisError::SingleOptionMenu { .. }),
            "expected SingleOptionMenu, got: {:?}",
            errors[0]
        );
    }

    #[test]
    fn single_option_menu_ast_direct() {
        let opt = make_option("Proceed");
        let menu = Ast::menu(vec![opt]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::SingleOptionMenu { .. }));
    }

    #[test]
    fn multiple_single_option_menus_all_reported() {
        let ast = parse(
            r#"
label start {
    menu {
        "Go" { jump next }
    }
    menu {
        "Continue" { end!() }
    }
}
label next { end!() }
"#,
        );
        let errors = check(&ast);
        let single_count = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::SingleOptionMenu { .. }))
            .count();
        assert_eq!(
            single_count, 2,
            "expected 2 SingleOptionMenu warnings, got: {errors:?}"
        );
    }

    // ── Default / wildcard option interactions ───────────────────────────────

    #[test]
    fn single_real_option_with_default_is_clean() {
        // 1 real option + 1 default → no SingleOptionMenu warning because the
        // default provides a meaningful fallback path.
        let opt = make_option("Accept");
        let def = make_default_option();
        let menu = Ast::menu(vec![opt, def]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert!(
            errors.is_empty(),
            "expected no errors for 1-option + default menu, got: {errors:?}"
        );
    }

    #[test]
    fn two_real_options_with_default_is_clean() {
        let opt1 = make_option("A");
        let opt2 = make_option("B");
        let def = make_default_option();
        let menu = Ast::menu(vec![opt1, opt2, def]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert!(
            errors.is_empty(),
            "expected no errors for 2-option + default menu, got: {errors:?}"
        );
    }

    #[test]
    fn only_default_option_emits_empty_menu() {
        // A menu with only a `_` default and no real options — the player
        // never sees a real choice.
        let def = make_default_option();
        let menu = Ast::menu(vec![def]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        assert!(
            matches!(&errors[0], AnalysisError::EmptyMenu { .. }),
            "expected EmptyMenu for default-only menu, got: {:?}",
            errors[0]
        );
    }

    #[test]
    fn multiple_defaults_emits_error() {
        let opt = make_option("Real");
        let def1 = make_default_option();
        let def2 = make_default_option();
        let menu = Ast::menu(vec![opt, def1, def2]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, AnalysisError::MultipleMenuDefaults { .. })),
            "expected MultipleMenuDefaults error, got: {errors:?}"
        );
    }

    #[test]
    fn multiple_defaults_no_real_options_emits_both_errors() {
        // 0 real options + 2 defaults → EmptyMenu AND MultipleMenuDefaults.
        let def1 = make_default_option();
        let def2 = make_default_option();
        let menu = Ast::menu(vec![def1, def2]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, AnalysisError::MultipleMenuDefaults { .. })),
            "expected MultipleMenuDefaults, got: {errors:?}"
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, AnalysisError::EmptyMenu { .. })),
            "expected EmptyMenu, got: {errors:?}"
        );
    }

    #[test]
    fn exactly_one_default_is_not_flagged_as_multiple() {
        let opt1 = make_option("X");
        let opt2 = make_option("Y");
        let def = make_default_option();
        let menu = Ast::menu(vec![opt1, opt2, def]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        assert!(
            !errors
                .iter()
                .any(|e| matches!(e, AnalysisError::MultipleMenuDefaults { .. })),
            "did not expect MultipleMenuDefaults, got: {errors:?}"
        );
    }

    #[test]
    fn three_defaults_still_emits_multiple_defaults() {
        let def1 = make_default_option();
        let def2 = make_default_option();
        let def3 = make_default_option();
        let menu = Ast::menu(vec![def1, def2, def3]);
        let root = Ast::block(vec![menu]);
        let errors = check(&root);
        let multi_count = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::MultipleMenuDefaults { .. }))
            .count();
        assert_eq!(
            multi_count, 1,
            "expected exactly 1 MultipleMenuDefaults error, got: {errors:?}"
        );
    }

    // ── Mixed errors in one pass ─────────────────────────────────────────────

    #[test]
    fn empty_and_single_option_both_reported() {
        let ast = parse(
            r#"
label start {
    menu {}
    menu {
        "Solo" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, AnalysisError::EmptyMenu { .. }))
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, AnalysisError::SingleOptionMenu { .. }))
        );
    }

    // ── Nested menus ─────────────────────────────────────────────────────────

    #[test]
    fn nested_empty_menu_inside_option_is_reported() {
        // The outer menu is fine (2 options); the inner one inside an option body is empty.
        let ast = parse(
            r#"
label start {
    menu {
        "First" {
            menu {}
        }
        "Second" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyMenu { .. }));
    }

    #[test]
    fn nested_single_option_menu_inside_option_is_reported() {
        let ast = parse(
            r#"
label start {
    menu {
        "Outer A" {
            menu {
                "Inner only" { end!() }
            }
        }
        "Outer B" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::SingleOptionMenu { .. }));
    }

    #[test]
    fn nested_good_menu_inside_single_option_menu_reports_outer_only() {
        // Outer menu: single option → SingleOptionMenu
        // Inner menu: two options → clean
        let ast = parse(
            r#"
label start {
    menu {
        "Sole option" {
            menu {
                "Yes" { end!() }
                "No"  { end!() }
            }
        }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::SingleOptionMenu { .. }));
    }

    // ── Span is preserved for parse-based nodes ──────────────────────────────

    #[test]
    fn empty_menu_span_is_nonzero_for_parsed_source() {
        let ast = parse(
            r#"
label start {
    menu {}
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        if let AnalysisError::EmptyMenu { span } = &errors[0] {
            assert!(
                span.start != 0 || span.end != 0,
                "expected a real source span, got zero span"
            );
        }
    }

    #[test]
    fn single_option_menu_span_is_nonzero_for_parsed_source() {
        let ast = parse(
            r#"
label start {
    menu {
        "Only" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        if let AnalysisError::SingleOptionMenu { span } = &errors[0] {
            assert!(
                span.start != 0 || span.end != 0,
                "expected a real source span, got zero span"
            );
        }
    }

    // ── Menu inside a labeled block nested in another label ──────────────────

    #[test]
    fn empty_menu_inside_deeply_nested_structure_is_found() {
        let ast = parse(
            r#"
label outer {
    if true {
        menu {}
    } else {
        end!()
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyMenu { .. }));
    }

    // ── Direct AST construction with span ────────────────────────────────────

    #[test]
    fn empty_menu_zero_span_in_ast_constructed_node() {
        let menu = Ast::menu(vec![]);
        let errors = check(&menu);
        assert_eq!(errors.len(), 1);
        if let AnalysisError::EmptyMenu { span } = &errors[0] {
            assert_eq!(span.start, 0);
            assert_eq!(span.end, 0);
        }
    }

    // ── Two-option menu after fixing: no regressions ─────────────────────────

    #[test]
    fn adding_second_option_clears_single_option_warning() {
        // Previously single-option; now two — must be clean.
        let opt1 = make_option("A");
        let opt2 = make_option("B");
        let menu = Ast::menu(vec![opt1, opt2]);
        let errors = check(&menu);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── Non-menu nodes are not touched ───────────────────────────────────────

    #[test]
    fn dialogue_node_is_not_flagged() {
        let diag = make_dialogue("Hello world");
        let root = Ast::block(vec![diag]);
        let errors = check(&root);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── Display / message smoke test ─────────────────────────────────────────

    #[test]
    fn empty_menu_error_has_useful_display() {
        let err = AnalysisError::EmptyMenu { span: zero_span() };
        let msg = err.to_string();
        assert!(
            msg.contains("menu") || msg.contains("choice"),
            "message should mention menu/choice, got: {msg}"
        );
    }

    #[test]
    fn single_option_menu_error_has_useful_display() {
        let err = AnalysisError::SingleOptionMenu { span: zero_span() };
        let msg = err.to_string();
        assert!(
            msg.contains("menu") || msg.contains("option") || msg.contains("choice"),
            "message should mention menu/option/choice, got: {msg}"
        );
    }

    #[test]
    fn multiple_defaults_error_has_useful_display() {
        let err = AnalysisError::MultipleMenuDefaults { span: zero_span() };
        let msg = err.to_string();
        assert!(
            msg.contains("default") || msg.contains("wildcard"),
            "message should mention default/wildcard, got: {msg}"
        );
    }

    // ── Nested default interactions ──────────────────────────────────────────

    #[test]
    fn nested_menu_with_multiple_defaults_reported_independently() {
        // Outer menu: 2 real options → clean.
        // Inner menu: 1 real option + 2 defaults → MultipleMenuDefaults.
        let inner_opt = make_option("Inner");
        let inner_def1 = make_default_option();
        let inner_def2 = make_default_option();
        let inner_menu = Ast::menu(vec![inner_opt, inner_def1, inner_def2]);

        let outer_opt1 =
            Ast::menu_option("Outer A".to_owned(), Ast::block(vec![inner_menu]), false);
        let outer_opt2 = make_option("Outer B");
        let outer_menu = Ast::menu(vec![outer_opt1, outer_opt2]);
        let root = Ast::block(vec![outer_menu]);

        let errors = check(&root);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        assert!(
            matches!(&errors[0], AnalysisError::MultipleMenuDefaults { .. }),
            "expected MultipleMenuDefaults, got: {:?}",
            errors[0]
        );
    }
}
