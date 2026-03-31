//! # Label Resolution Analysis
//!
//! Checks that every `jump` statement (and `let … = jump … and return`) references
//! a label that is actually defined somewhere in the script.

use crate::analysis::AnalysisError;
use crate::analysis::context::AnalysisContext;
use crate::parser::ast::{Ast, AstContent, walk_ast};

/// Run the label-resolution check over `ast`.
///
/// Returns one [`AnalysisError::UndefinedLabel`] for every `jump` (or `let-call`)
/// whose target label does not appear in [`AnalysisContext::labels`].
pub fn check(ast: &Ast, ctx: &AnalysisContext) -> Vec<AnalysisError> {
    let mut errors = Vec::new();
    check_node(ast, ctx, &mut errors);
    errors
}

fn check_node(node: &Ast, ctx: &AnalysisContext, errors: &mut Vec<AnalysisError>) {
    walk_ast(node, &mut |n| {
        match n.content() {
            // ── Direct jump ──────────────────────────────────────────
            // Qualified labels (e.g. `inv.show_inventory`) are cross-module
            // references validated at compile time — skip them here since the
            // single-file analyser has no access to the imported label set.
            AstContent::Jump { label, .. }
                if !label.contains('.') && !ctx.labels.contains(label) =>
            {
                errors.push(AnalysisError::UndefinedLabel {
                    label: label.clone(),
                    span: n.span(),
                });
            }

            // ── let name = jump target and return ────────────────────
            // Same cross-module exemption as for Jump above.
            AstContent::LetCall { target, .. }
                if !target.contains('.') && !ctx.labels.contains(target) =>
            {
                errors.push(AnalysisError::UndefinedLabel {
                    label: target.clone(),
                    span: n.span(),
                });
            }

            _ => {}
        }
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
        let errors = check(&ast, &ctx);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn jump_to_missing_label_reports_error() {
        let ast = parse("label start {\n  jump nowhere\n}\n");
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx);
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
        let errors = check(&ast, &ctx);
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
        let errors = check(&ast, &ctx);
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
        let errors = check(&ast, &ctx);
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
        let errors = check(&ast, &ctx);
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
}
