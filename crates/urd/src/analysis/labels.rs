//! # Label Resolution Analysis
//!
//! Checks that every `jump` statement (and `let … = jump … and return`) references
//! a label that is actually defined somewhere in the script.

use crate::analysis::AnalysisError;
use crate::analysis::context::AnalysisContext;
use crate::parser::ast::{Ast, AstContent, MatchPattern};

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
    match node.content() {
        // ── Direct jump ──────────────────────────────────────────────
        AstContent::Jump { label, .. } => {
            if !ctx.labels.contains(label) {
                errors.push(AnalysisError::UndefinedLabel {
                    label: label.clone(),
                    span: node.span(),
                });
            }
        }

        // ── let name = jump target and return ────────────────────────
        AstContent::LetCall { target, .. } => {
            if !ctx.labels.contains(target) {
                errors.push(AnalysisError::UndefinedLabel {
                    label: target.clone(),
                    span: node.span(),
                });
            }
        }

        // ── Containers: recurse into children ────────────────────────
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for stmt in stmts {
                check_node(stmt, ctx, errors);
            }
        }

        AstContent::LabeledBlock { block, .. } => {
            check_node(block, ctx, errors);
        }

        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            check_node(condition, ctx, errors);
            check_node(then_block, ctx, errors);
            if let Some(eb) = else_block {
                check_node(eb, ctx, errors);
            }
        }

        AstContent::Match { scrutinee, arms } => {
            check_node(scrutinee, ctx, errors);
            for arm in arms {
                if let MatchPattern::Value(v) = &arm.pattern {
                    check_node(v, ctx, errors);
                }
                check_node(&arm.body, ctx, errors);
            }
        }

        AstContent::Menu { options } => {
            for opt in options {
                check_node(opt, ctx, errors);
            }
        }

        AstContent::MenuOption { content, .. } => {
            check_node(content, ctx, errors);
        }

        AstContent::Declaration {
            decl_name,
            decl_defs,
            ..
        } => {
            check_node(decl_name, ctx, errors);
            check_node(decl_defs, ctx, errors);
        }

        AstContent::BinOp { left, right, .. } => {
            check_node(left, ctx, errors);
            check_node(right, ctx, errors);
        }

        AstContent::UnaryOp { expr, .. } => {
            check_node(expr, ctx, errors);
        }

        AstContent::Call { func_path, params } => {
            check_node(func_path, ctx, errors);
            check_node(params, ctx, errors);
        }

        AstContent::Dialogue { speakers, content } => {
            check_node(speakers, ctx, errors);
            check_node(content, ctx, errors);
        }

        AstContent::DecoratorDef { body, .. } => {
            check_node(body, ctx, errors);
        }

        AstContent::Return { value: Some(v) } => {
            check_node(v, ctx, errors);
        }

        AstContent::Subscript { object, key } => {
            check_node(object, ctx, errors);
            check_node(key, ctx, errors);
        }

        AstContent::SubscriptAssign { object, key, value } => {
            check_node(object, ctx, errors);
            check_node(key, ctx, errors);
            check_node(value, ctx, errors);
        }

        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                check_node(k, ctx, errors);
                check_node(v, ctx, errors);
            }
        }

        // Leaf nodes that cannot contain a jump.
        AstContent::Value(_)
        | AstContent::Return { value: None }
        | AstContent::EnumDecl { .. }
        | AstContent::StructDecl { .. }
        | AstContent::Import { .. } => {}
    }
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
