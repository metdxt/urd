//! # Label Resolution Analysis
//!
//! Checks that every `jump` statement (and `let … = jump … and return`) references
//! a label that is actually defined somewhere in the script.
//!
//! When an undefined label is found, the check also attempts to compute a
//! suggestion (Levenshtein ≤ 2 via [`possible_typo::closest_match`], then a
//! semantic fallback) and embeds it directly in the
//! [`AnalysisError::UndefinedLabel`] variant, producing a single unified
//! diagnostic instead of separate `UndefinedLabel` + `PossibleTypo` warnings.

use crate::analysis::AnalysisError;
use crate::analysis::context::AnalysisContext;
use crate::analysis::possible_typo::closest_match;
use crate::analysis::semantic_suggest::SemanticSuggest;
use crate::parser::ast::{Ast, AstContent, walk_ast};

/// Run the label-resolution check over `ast`.
///
/// Returns one [`AnalysisError::UndefinedLabel`] for every `jump` (or
/// `let-call`) whose target label does not appear in
/// [`AnalysisContext::labels`].
///
/// When `semantic` is `Some`, it is consulted as a fallback suggestion source
/// whenever Levenshtein edit distance produces no close match (distance > 2).
/// When `None`, only Levenshtein is used.
pub fn check(
    ast: &Ast,
    ctx: &AnalysisContext,
    semantic: Option<&dyn SemanticSuggest>,
) -> Vec<AnalysisError> {
    let defined: Vec<String> = ctx.labels.iter().cloned().collect();
    let mut errors = Vec::new();
    check_node(ast, ctx, &defined, semantic, &mut errors);
    errors
}

fn check_node(
    node: &Ast,
    ctx: &AnalysisContext,
    defined: &[String],
    semantic: Option<&dyn SemanticSuggest>,
    errors: &mut Vec<AnalysisError>,
) {
    walk_ast(node, &mut |n| {
        let (label_str, span) = match n.content() {
            // ── Direct jump ──────────────────────────────────────────
            // Qualified labels (e.g. `inv.show_inventory`) are cross-module
            // references validated at compile time — skip them here since the
            // single-file analyser has no access to the imported label set.
            AstContent::Jump { label, .. }
                if !label.contains('.') && !ctx.labels.contains(label) =>
            {
                (label.clone(), n.span())
            }

            // ── let name = jump target and return ────────────────────
            // Same cross-module exemption as for Jump above.
            AstContent::LetCall { target, .. }
                if !target.contains('.') && !ctx.labels.contains(target) =>
            {
                (target.clone(), n.span())
            }

            _ => return,
        };

        // Only attempt suggestions for names long enough to have meaningful
        // edit-distance comparisons; single/double-char names would produce too
        // many false positives.
        let suggestion = if label_str.len() > 2 {
            closest_match(&label_str, defined.iter())
                .and_then(|(dist, s)| if dist <= 2 { Some(s) } else { None })
                .or_else(|| semantic.and_then(|m| m.find_synonym(&label_str, defined)))
        } else {
            None
        };

        errors.push(AnalysisError::UndefinedLabel {
            label: label_str,
            suggestion,
            span,
        });
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::context::AnalysisContext;
    use crate::compiler::loader::parse_source;

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    #[test]
    fn jump_to_existing_label_is_ok() {
        let ast = parse("label start {\n  end!()\n}\nlabel next {\n  jump start\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn jump_to_missing_label_reports_error() {
        let ast = parse("label start {\n  jump nowhere\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::UndefinedLabel { label, .. } => {
                assert_eq!(label, "nowhere");
            }
            other => panic!("expected UndefinedLabel, got: {other:?}"),
        }
    }

    #[test]
    fn let_call_to_missing_label_reports_error() {
        let ast = parse("label start {\n  let x = jump ghost and return\n  end!()\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel { label, .. } if label == "ghost"
            )),
            "expected UndefinedLabel for 'ghost', got: {errors:?}"
        );
    }

    #[test]
    fn jump_inside_if_checked() {
        let src = "label start {\n  if true {\n    jump nope\n  } else {\n    end!()\n  }\n}\n";
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel { label, .. } if label == "nope"
            )),
            "expected UndefinedLabel for 'nope', got: {errors:?}"
        );
    }

    #[test]
    fn jump_inside_menu_checked() {
        let src = "label start {\n  menu {\n    \"Go\" {\n      jump missing\n    }\n  }\n}\n";
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel { label, .. } if label == "missing"
            )),
            "expected UndefinedLabel for 'missing', got: {errors:?}"
        );
    }

    #[test]
    fn multiple_missing_labels() {
        let src = "label a {\n  jump x\n}\nlabel b {\n  jump y\n}\n";
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
        let labels: Vec<&str> = errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::UndefinedLabel { label, .. } => Some(label.as_str()),
                _ => None,
            })
            .collect();
        assert!(labels.contains(&"x"));
        assert!(labels.contains(&"y"));
    }

    #[test]
    fn directly_imported_label_is_not_undefined() {
        // Simulates: `import (show_inventory) from "items.urd"`
        // The label `show_inventory` is NOT defined in this file — it lives in
        // the imported module — but it has been injected into the context via
        // `build_with_imports` as an imported label.  The check must not emit
        // `UndefinedLabel` for `jump show_inventory`.
        let ast = parse("label village_farewell {\n  jump show_inventory\n}\n");

        let mut imported_labels = std::collections::HashSet::new();
        imported_labels.insert("show_inventory".to_owned());

        let ctx = AnalysisContext::build_with_imports(
            &ast,
            std::collections::HashMap::new(),
            std::collections::HashMap::new(),
            imported_labels,
        );

        let errors = check(&ast, &ctx, None);
        assert!(
            errors.is_empty(),
            "expected no errors for directly-imported label, got: {errors:?}"
        );
    }

    #[test]
    fn unimported_label_still_reports_error_after_build_with_imports() {
        // Ensure that `build_with_imports` doesn't accidentally suppress errors
        // for labels that were NOT imported.
        let ast = parse("label start {\n  jump ghost\n}\n");

        let mut imported_labels = std::collections::HashSet::new();
        imported_labels.insert("other_label".to_owned()); // something else, not "ghost"

        let ctx = AnalysisContext::build_with_imports(
            &ast,
            std::collections::HashMap::new(),
            std::collections::HashMap::new(),
            imported_labels,
        );

        let errors = check(&ast, &ctx, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel { label, .. } if label == "ghost"
            )),
            "expected UndefinedLabel for 'ghost', got: {errors:?}"
        );
    }

    #[test]
    fn suggestion_present_when_close_match_exists() {
        // "staart" has Levenshtein distance 1 from "start" — suggestion must be populated.
        let ast = parse("label start {\n  end!()\n}\nlabel other {\n  jump staart\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel {
                    label,
                    suggestion: Some(s),
                    ..
                } if label == "staart" && s == "start"
            )),
            "expected UndefinedLabel with suggestion 'start' for 'staart', got: {errors:?}"
        );
    }

    #[test]
    fn suggestion_absent_when_no_close_match() {
        // "completelymissing" has no label within edit distance 2 — suggestion is None.
        let ast = parse("label start {\n  end!()\n}\nlabel other {\n  jump completelymissing\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel {
                    label,
                    suggestion: None,
                    ..
                } if label == "completelymissing"
            )),
            "expected UndefinedLabel with no suggestion for 'completelymissing', got: {errors:?}"
        );
    }

    #[test]
    fn semantic_suggestion_used_when_levenshtein_finds_nothing() {
        use crate::analysis::semantic_suggest::SemanticSuggest;

        struct MockSemantic;
        impl SemanticSuggest for MockSemantic {
            fn find_synonym(&self, query: &str, candidates: &[String]) -> Option<String> {
                if query == "go_to_woods" && candidates.iter().any(|c| c == "go_to_forest") {
                    Some("go_to_forest".to_owned())
                } else {
                    None
                }
            }
        }

        // Edit distance between "go_to_woods" and "go_to_forest" is 4 — beyond
        // the Levenshtein threshold — so only the semantic backend can provide
        // the suggestion.
        let ast =
            parse("label go_to_forest {\n  end!()\n}\nlabel start {\n  jump go_to_woods\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, Some(&MockSemantic));
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel {
                    label,
                    suggestion: Some(s),
                    ..
                } if label == "go_to_woods" && s == "go_to_forest"
            )),
            "expected UndefinedLabel with semantic suggestion 'go_to_forest' for 'go_to_woods', \
             got: {errors:?}"
        );
    }

    #[test]
    fn no_suggestion_when_semantic_is_none_and_no_close_match() {
        // Without a semantic backend and no Levenshtein match, suggestion is None.
        let ast =
            parse("label go_to_forest {\n  end!()\n}\nlabel start {\n  jump go_to_woods\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedLabel {
                    label,
                    suggestion: None,
                    ..
                } if label == "go_to_woods"
            )),
            "expected UndefinedLabel with no suggestion for 'go_to_woods' when semantic=None, \
             got: {errors:?}"
        );
    }
}
