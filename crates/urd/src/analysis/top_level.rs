//! # Top-Level Statement Validation
//!
//! This pass ensures that only *definitions* appear at the top level of an Urd
//! script.  Flow-control statements (`if`, `match`, `jump`, …), dialogue,
//! assignments, and bare expressions are rejected with a [`AnalysisError::TopLevelFlow`]
//! diagnostic.
//!
//! Additionally, the pass checks that at most one `LabeledBlock` carries the
//! `@entry` decorator.  A second (or subsequent) `@entry` label produces a
//! [`AnalysisError::DuplicateEntry`] error.

use crate::parser::ast::{Ast, AstContent, Operator};
use crate::runtime::value::RuntimeValue;

use super::AnalysisError;

/// Run the top-level validation pass over `ast`.
///
/// If the root node is a [`AstContent::Block`], each direct child is inspected.
/// Children whose content is not one of the allowed definition kinds produce a
/// [`AnalysisError::TopLevelFlow`] error.  Any duplicate `@entry` decorators
/// produce a [`AnalysisError::DuplicateEntry`] error.
///
/// If the root node is *not* a `Block`, the function returns an empty `Vec`
/// (nothing to check).
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let children = match ast.content() {
        AstContent::Block(children) => children,
        _ => return Vec::new(),
    };

    let mut errors = Vec::new();

    // ------------------------------------------------------------------
    // 1. Reject disallowed top-level statements
    // ------------------------------------------------------------------
    for child in children {
        if let Some(description) = disallowed_description(child) {
            errors.push(AnalysisError::TopLevelFlow {
                description,
                span: child.span(),
            });
        }
    }

    // ------------------------------------------------------------------
    // 2. Enforce at most one @entry decorator
    // ------------------------------------------------------------------
    let mut first_entry_seen = false;

    for child in children {
        if let AstContent::LabeledBlock { label, .. } = child.content() {
            let has_entry = child.decorators().iter().any(|d| d.name() == "entry");

            if has_entry {
                if first_entry_seen {
                    errors.push(AnalysisError::DuplicateEntry {
                        label: label.clone(),
                        span: child.span(),
                    });
                } else {
                    first_entry_seen = true;
                }
            }
        }
    }

    errors
}

/// If `node` is **not** an allowed top-level definition, return a short
/// human-readable description of the offending construct.  Otherwise return
/// `None`.
///
/// Allowed top-level forms:
/// - `Declaration` (let / const / global bindings)
/// - `ExternDeclaration` (extern const / extern global provided by the runtime)
/// - `EnumDecl`
/// - `StructDecl`
/// - `DecoratorDef`
/// - `Import`
/// - `LabeledBlock`
fn disallowed_description(node: &Ast) -> Option<String> {
    match node.content() {
        // ---- allowed ----
        AstContent::Declaration { .. }
        | AstContent::ExternDeclaration { .. }
        | AstContent::EnumDecl { .. }
        | AstContent::StructDecl { .. }
        | AstContent::DecoratorDef { .. }
        | AstContent::Import { .. }
        | AstContent::LabeledBlock { .. } => None,

        // ---- disallowed — each gets a descriptive label ----
        AstContent::Jump { .. } => Some("jump statement".into()),
        AstContent::Return { .. } => Some("return statement".into()),
        AstContent::LetCall { .. } => Some("let-call statement".into()),
        AstContent::If { .. } => Some("if statement".into()),
        AstContent::Match { .. } => Some("match statement".into()),
        AstContent::Menu { .. } => Some("menu".into()),
        AstContent::MenuOption { .. } => Some("menu option".into()),
        AstContent::Dialogue { .. } => Some("dialogue".into()),

        AstContent::Call { func_path, .. } => {
            // Try to give a nicer name for well-known builtins.
            let label = match func_path.content() {
                AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => {
                    match path[0].as_str() {
                        "end!" => "end! call".to_owned(),
                        "todo!" => "todo! call".to_owned(),
                        other => format!("call to `{other}`"),
                    }
                }
                _ => "function call".into(),
            };
            Some(label)
        }

        AstContent::BinOp { op, .. } => {
            if matches!(op, Operator::Assign) {
                Some("assignment".into())
            } else {
                Some("binary expression".into())
            }
        }

        AstContent::UnaryOp { .. } => Some("unary expression".into()),
        AstContent::Value(_) => Some("bare value".into()),
        AstContent::List(_) => Some("list literal".into()),
        AstContent::Map(_) => Some("map literal".into()),
        AstContent::Subscript { .. } => Some("subscript expression".into()),
        AstContent::SubscriptAssign { .. } => Some("subscript assignment".into()),
        AstContent::ExprList(_) => Some("expression list".into()),
        AstContent::Block(_) => Some("anonymous block".into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::strings::ParsedString;
    use crate::parser::ast::{Ast, AstContent, DeclKind, Decorator};
    use crate::runtime::value::RuntimeValue;
    use chumsky::span::{SimpleSpan, Span};

    /// Helper: create an identifier AST node.
    fn ident(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
    }

    /// Helper: wrap children in a root `Block` with a nonzero span so errors
    /// are distinguishable.
    fn root_block(children: Vec<Ast>) -> Ast {
        Ast::new(AstContent::Block(children)).with_span(SimpleSpan::new((), 0..100))
    }

    /// Helper: build a child node with a recognisable span.
    fn spanned(ast: Ast, start: usize, end: usize) -> Ast {
        ast.with_span(SimpleSpan::new((), start..end))
    }

    // ---------------------------------------------------------------
    // Allowed top-level nodes produce no errors
    // ---------------------------------------------------------------

    #[test]
    fn allowed_nodes_produce_no_errors() {
        let children = vec![
            // Declaration
            spanned(
                Ast::decl(
                    DeclKind::Variable,
                    ident("x"),
                    Ast::value(RuntimeValue::Int(1)),
                ),
                0,
                10,
            ),
            // EnumDecl
            spanned(
                Ast::enum_decl("Color".into(), vec!["Red".into(), "Blue".into()]),
                11,
                20,
            ),
            // StructDecl
            spanned(Ast::struct_decl("Point".into(), vec![]), 21, 30),
            // Import
            spanned(
                Ast::import_module("other.urd".into(), "other".into()),
                31,
                40,
            ),
            // LabeledBlock
            spanned(
                Ast::labeled_block("main".into(), Ast::block(vec![])),
                41,
                50,
            ),
        ];

        let ast = root_block(children);
        let errors = check(&ast);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    }

    // ---------------------------------------------------------------
    // ExternDeclaration is allowed at top level
    // ---------------------------------------------------------------

    #[test]
    fn extern_declaration_is_allowed() {
        use crate::parser::ast::DeclKind;
        let node = spanned(
            Ast::extern_decl(DeclKind::Constant, ident("narrator"), None),
            0,
            20,
        );
        let ast = root_block(vec![node]);
        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "extern declaration should be allowed at top level, got: {errors:?}"
        );
    }

    // ---------------------------------------------------------------
    // Disallowed nodes produce TopLevelFlow errors
    // ---------------------------------------------------------------

    #[test]
    fn jump_is_rejected() {
        let ast = root_block(vec![spanned(
            Ast::jump_stmt("somewhere".into(), false),
            0,
            10,
        )]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TopLevelFlow { description, .. } => {
                assert_eq!(description, "jump statement");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn return_is_rejected() {
        let ast = root_block(vec![spanned(Ast::return_stmt(None), 0, 5)]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TopLevelFlow { description, .. } => {
                assert_eq!(description, "return statement");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn if_statement_is_rejected() {
        let ast = root_block(vec![spanned(
            Ast::if_stmt(
                Ast::value(RuntimeValue::Bool(true)),
                Ast::block(vec![]),
                None,
            ),
            0,
            20,
        )]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TopLevelFlow { description, .. } => {
                assert_eq!(description, "if statement");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn assignment_is_rejected() {
        let ast = root_block(vec![spanned(
            Ast::assign_op(ident("x"), Ast::value(RuntimeValue::Int(5))),
            0,
            10,
        )]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TopLevelFlow { description, .. } => {
                assert_eq!(description, "assignment");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn bare_value_is_rejected() {
        let ast = root_block(vec![spanned(Ast::value(RuntimeValue::Int(42)), 0, 5)]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TopLevelFlow { description, .. } => {
                assert_eq!(description, "bare value");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn dialogue_is_rejected() {
        let ast = root_block(vec![spanned(
            Ast::dialogue(
                ident("narrator"),
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("hello"))),
            ),
            0,
            20,
        )]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TopLevelFlow { description, .. } => {
                assert_eq!(description, "dialogue");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn call_is_rejected() {
        let ast = root_block(vec![spanned(
            Ast::call(ident("end!"), Ast::expr_list(vec![])),
            0,
            10,
        )]);
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TopLevelFlow { description, .. } => {
                assert_eq!(description, "end! call");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn multiple_disallowed_nodes_all_reported() {
        let ast = root_block(vec![
            spanned(Ast::jump_stmt("a".into(), false), 0, 5),
            spanned(Ast::return_stmt(None), 6, 10),
            spanned(Ast::value(RuntimeValue::Int(1)), 11, 15),
        ]);
        let errors = check(&ast);
        let top_level_count = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::TopLevelFlow { .. }))
            .count();
        assert_eq!(top_level_count, 3);
    }

    // ---------------------------------------------------------------
    // Non-block root returns empty
    // ---------------------------------------------------------------

    #[test]
    fn non_block_root_returns_empty() {
        let ast = Ast::value(RuntimeValue::Int(1));
        let errors = check(&ast);
        assert!(errors.is_empty());
    }

    // ---------------------------------------------------------------
    // @entry duplicate detection
    // ---------------------------------------------------------------

    #[test]
    fn single_entry_is_fine() {
        let label = Ast::labeled_block("main".into(), Ast::block(vec![]))
            .with_decorators(vec![Decorator::bare("entry".into())])
            .with_span(SimpleSpan::new((), 0..20));

        let ast = root_block(vec![label]);
        let errors = check(&ast);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    }

    #[test]
    fn duplicate_entry_is_reported() {
        let label1 = Ast::labeled_block("first".into(), Ast::block(vec![]))
            .with_decorators(vec![Decorator::bare("entry".into())])
            .with_span(SimpleSpan::new((), 0..20));

        let label2 = Ast::labeled_block("second".into(), Ast::block(vec![]))
            .with_decorators(vec![Decorator::bare("entry".into())])
            .with_span(SimpleSpan::new((), 21..40));

        let ast = root_block(vec![label1, label2]);
        let errors = check(&ast);

        let dup_errors: Vec<_> = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::DuplicateEntry { .. }))
            .collect();
        assert_eq!(dup_errors.len(), 1);

        match &dup_errors[0] {
            AnalysisError::DuplicateEntry { label, .. } => {
                assert_eq!(label, "second");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn three_entries_produce_two_errors() {
        let make_entry = |name: &str, start: usize, end: usize| {
            Ast::labeled_block(name.into(), Ast::block(vec![]))
                .with_decorators(vec![Decorator::bare("entry".into())])
                .with_span(SimpleSpan::new((), start..end))
        };

        let ast = root_block(vec![
            make_entry("a", 0, 10),
            make_entry("b", 11, 20),
            make_entry("c", 21, 30),
        ]);
        let errors = check(&ast);

        let dup_errors: Vec<_> = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::DuplicateEntry { .. }))
            .collect();
        assert_eq!(dup_errors.len(), 2);
    }

    #[test]
    fn non_entry_decorators_are_ignored() {
        let label1 = Ast::labeled_block("first".into(), Ast::block(vec![]))
            .with_decorators(vec![Decorator::bare("entry".into())])
            .with_span(SimpleSpan::new((), 0..20));

        let label2 = Ast::labeled_block("second".into(), Ast::block(vec![]))
            .with_decorators(vec![Decorator::bare("other".into())])
            .with_span(SimpleSpan::new((), 21..40));

        let ast = root_block(vec![label1, label2]);
        let errors = check(&ast);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    }
}
