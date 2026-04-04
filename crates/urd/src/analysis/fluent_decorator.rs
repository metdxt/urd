//! Analysis pass for the `@fluent` decorator.
//!
//! Validates that `@fluent` / `@fluent("alias")` decorators are used correctly:
//! only on variable declarations, with at most one occurrence, and with a
//! plain string argument if an alias is provided.
//!
//! ## Rules enforced
//!
//! | Condition | Severity | Diagnostic |
//! |-----------|----------|------------|
//! | More than one `@fluent` on a single node | Error | [`AnalysisError::InvalidFluentDecorator`] |
//! | `@fluent` has more than 1 argument | Error | [`AnalysisError::InvalidFluentDecorator`] |
//! | `@fluent` argument is not a plain string literal | Error | [`AnalysisError::InvalidFluentDecorator`] |
//! | `@fluent` argument contains string interpolation | Error | [`AnalysisError::InvalidFluentDecorator`] |
//! | `@fluent` on a node type that is not a variable declaration | Warning | [`AnalysisError::FluentOnUnsupportedNode`] |
//!
//! ## Supported node type
//!
//! `@fluent` is meaningful only on [`AstContent::Declaration`].  Any other
//! node type triggers [`AnalysisError::FluentOnUnsupportedNode`] (warning —
//! the decorator has no effect but is not necessarily wrong).

use crate::analysis::AnalysisError;
use crate::lexer::strings::StringPart;
use crate::parser::ast::{Ast, AstContent, Decorator, walk_ast};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the `@fluent` decorator validation pass over `ast` and return all
/// diagnostics.
///
/// Every node in the tree is inspected; any ill-formed `@fluent` decorator
/// emits an [`AnalysisError::InvalidFluentDecorator`] or, when placed on an
/// unsupported node kind, an [`AnalysisError::FluentOnUnsupportedNode`].
///
/// All checks always run to completion; a problem found on one node does not
/// suppress checks on sibling nodes.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors = Vec::new();
    walk_ast(ast, &mut |node| {
        validate_fluent_decorators(node, &mut errors);
    });
    errors
}

// ---------------------------------------------------------------------------
// Per-node validation
// ---------------------------------------------------------------------------

/// Inspect a single AST node for `@fluent` decorator problems.
///
/// Rules applied in order (early-return after the first argument error so that
/// the same `@fluent` does not produce multiple diagnostics for the same
/// mistake):
///
/// 1. **Multiplicity** — more than one `@fluent` on the same node →
///    [`AnalysisError::InvalidFluentDecorator`].
/// 2. **Argument count** — the `ExprList` that forms the argument list must
///    contain 0 or 1 expressions.  0 is valid (bare `@fluent`); 1 is valid
///    when it is a plain string.  More than 1 →
///    [`AnalysisError::InvalidFluentDecorator`].
/// 3. **Argument type / interpolation** — if 1 argument is present it must be
///    a [`RuntimeValue::Str`] whose parts contain no
///    [`StringPart::Interpolation`] → [`AnalysisError::InvalidFluentDecorator`].
/// 4. **Supported node type** — if the node content is not
///    [`AstContent::Declaration`], [`AnalysisError::FluentOnUnsupportedNode`]
///    is emitted (warning, not error).
fn validate_fluent_decorators(node: &Ast, errors: &mut Vec<AnalysisError>) {
    let fluent_decorators: Vec<&Decorator> = node
        .decorators()
        .iter()
        .filter(|d| d.name() == "fluent")
        .collect();

    // Rule 1: at most one @fluent per node.
    if fluent_decorators.len() > 1 {
        errors.push(AnalysisError::InvalidFluentDecorator {
            reason: format!(
                "only one @fluent decorator is allowed per node, found {}",
                fluent_decorators.len()
            ),
            span: node.span(),
        });
        return;
    }

    // No @fluent on this node — nothing to validate.
    if fluent_decorators.is_empty() {
        return;
    }

    let decorator = fluent_decorators[0];

    // Rule 2 & 3: validate argument count and type.
    let items = match decorator.args().content() {
        AstContent::ExprList(items) => items,
        _ => {
            // Structurally impossible (the parser always produces ExprList for
            // decorator args), but guard defensively.
            errors.push(AnalysisError::InvalidFluentDecorator {
                reason: "@fluent arguments must be an expression list".to_string(),
                span: decorator.span(),
            });
            return;
        }
    };

    // Rule 2: 0 args is valid (bare @fluent — variable name is used as key).
    // 1 arg is valid if it is a plain string. More than 1 is always invalid.
    if items.len() > 1 {
        errors.push(AnalysisError::InvalidFluentDecorator {
            reason: format!("@fluent expects at most 1 argument, got {}", items.len()),
            span: decorator.span(),
        });
        return;
    }

    // Rule 3: if exactly 1 arg, it must be a plain (non-interpolated) string.
    if items.len() == 1 {
        let arg = &items[0];
        match arg.content() {
            AstContent::Value(RuntimeValue::Str(ps)) => {
                let has_interpolation = ps
                    .parts()
                    .iter()
                    .any(|p| matches!(p, StringPart::Interpolation(_)));

                if has_interpolation {
                    errors.push(AnalysisError::InvalidFluentDecorator {
                        reason:
                            "@fluent alias must be a plain string literal with no interpolation"
                                .to_string(),
                        span: decorator.span(),
                    });
                    return;
                }
            }
            _ => {
                errors.push(AnalysisError::InvalidFluentDecorator {
                    reason: "@fluent alias argument must be a plain string literal".to_string(),
                    span: decorator.span(),
                });
                return;
            }
        }
    }

    // Rule 4: @fluent is only meaningful on Declaration nodes.
    let is_supported = matches!(node.content(), AstContent::Declaration { .. });

    if !is_supported {
        errors.push(AnalysisError::FluentOnUnsupportedNode {
            node_kind: node_kind_name(node.content()),
            span: node.span(),
        });
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Returns a short, human-readable name for an [`AstContent`] variant.
///
/// Used as the `node_kind` field of [`AnalysisError::FluentOnUnsupportedNode`].
fn node_kind_name(content: &AstContent) -> String {
    match content {
        AstContent::Declaration { .. } => "Declaration".to_string(),
        AstContent::ExternDeclaration { .. } => "ExternDeclaration".to_string(),
        AstContent::FnDef { .. } => "FnDef".to_string(),
        AstContent::DecoratorDef { .. } => "DecoratorDef".to_string(),
        AstContent::EnumDecl { .. } => "EnumDecl".to_string(),
        AstContent::StructDecl { .. } => "StructDecl".to_string(),
        AstContent::Import { .. } => "Import".to_string(),
        AstContent::Jump { .. } => "Jump".to_string(),
        AstContent::Return { .. } => "Return".to_string(),
        AstContent::LetCall { .. } => "LetCall".to_string(),
        AstContent::Call { .. } => "Call".to_string(),
        AstContent::Block(_) => "Block".to_string(),
        AstContent::List(_) => "List".to_string(),
        AstContent::Map(_) => "Map".to_string(),
        AstContent::BinOp { .. } => "BinOp".to_string(),
        AstContent::UnaryOp { .. } => "UnaryOp".to_string(),
        AstContent::Value(_) => "Value".to_string(),
        AstContent::ExprList(_) => "ExprList".to_string(),
        AstContent::Subscript { .. } => "Subscript".to_string(),
        AstContent::SubscriptAssign { .. } => "SubscriptAssign".to_string(),
        AstContent::Dialogue { .. } => "Dialogue".to_string(),
        AstContent::Menu { .. } => "Menu".to_string(),
        AstContent::MenuOption { .. } => "MenuOption".to_string(),
        AstContent::LabeledBlock { .. } => "LabeledBlock".to_string(),
        AstContent::If { .. } => "If".to_string(),
        AstContent::Match { .. } => "Match".to_string(),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::AnalysisError;
    use crate::lexer::strings::{Interpolation, ParsedString, StringPart};
    use crate::parser::ast::{Ast, DeclKind, Decorator};
    use crate::runtime::value::RuntimeValue;

    // ── builder helpers ───────────────────────────────────────────────────────

    /// Plain string value node.
    fn str_val(s: &str) -> Ast {
        Ast::value(RuntimeValue::Str(ParsedString::new_plain(s)))
    }

    /// Interpolated string value node (e.g. `"{name}"`).
    fn interp_str_val(path: &str) -> Ast {
        let parts = vec![StringPart::Interpolation(Interpolation {
            path: path.to_owned(),
            format: None,
        })];
        Ast::value(RuntimeValue::Str(ParsedString::new_from_parts(parts)))
    }

    /// Variable-reference value node.
    fn ident_val(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
    }

    /// Build a bare `@fluent` decorator (no arguments).
    fn fluent_bare() -> Decorator {
        Decorator::bare("fluent".to_string())
    }

    /// Build a `@fluent("alias")` decorator with a plain string alias.
    fn fluent_with_alias(alias: &str) -> Decorator {
        Decorator::new("fluent".to_string(), Ast::expr_list(vec![str_val(alias)]))
    }

    /// Build a `@fluent` decorator with a custom (possibly invalid) argument.
    fn fluent_with_arg(arg: Ast) -> Decorator {
        Decorator::new("fluent".to_string(), Ast::expr_list(vec![arg]))
    }

    /// Build a `@fluent` decorator with two arguments.
    fn fluent_two_args(a: &str, b: &str) -> Decorator {
        Decorator::new(
            "fluent".to_string(),
            Ast::expr_list(vec![str_val(a), str_val(b)]),
        )
    }

    /// Build a variable declaration node with optional decorators.
    fn decl_node(name: &str, decorators: Vec<Decorator>) -> Ast {
        Ast::decl(
            DeclKind::Variable,
            ident_val(name),
            Ast::value(RuntimeValue::Int(0)),
        )
        .with_decorators(decorators)
    }

    /// Wrap a list of statements inside a `LabeledBlock`, then inside a root
    /// `Block`.  The resulting tree is the standard input shape for these tests.
    fn in_label(name: &str, stmts: Vec<Ast>) -> Ast {
        let block = Ast::block(stmts);
        let label = Ast::labeled_block(name.to_string(), block);
        Ast::block(vec![label])
    }

    // ── Clean cases ───────────────────────────────────────────────────────────

    /// A node with no `@fluent` decorator produces no diagnostics.
    #[test]
    fn no_fluent_is_clean() {
        let node = decl_node("count", vec![]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    /// A bare `@fluent` on a Declaration is fully valid.
    #[test]
    fn bare_fluent_on_declaration_is_clean() {
        let node = decl_node("score", vec![fluent_bare()]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    /// `@fluent("alias")` with a plain string alias on a Declaration is valid.
    #[test]
    fn fluent_with_alias_on_declaration_is_clean() {
        let node = decl_node("score", vec![fluent_with_alias("player-score")]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    /// An empty AST produces no diagnostics.
    #[test]
    fn empty_ast_is_clean() {
        let ast = Ast::block(vec![]);
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── FluentOnUnsupportedNode warnings ─────────────────────────────────────

    /// `@fluent` on a `Dialogue` node emits a `FluentOnUnsupportedNode` warning.
    #[test]
    fn fluent_on_dialogue_emits_warning() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"))
            .with_decorators(vec![fluent_bare()]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 diagnostic, got: {errors:?}");
        match &errors[0] {
            AnalysisError::FluentOnUnsupportedNode { node_kind, .. } => {
                assert_eq!(node_kind, "Dialogue");
            }
            other => panic!("expected FluentOnUnsupportedNode, got: {other:?}"),
        }
        assert!(
            errors[0].is_warning(),
            "FluentOnUnsupportedNode must be a warning"
        );
    }

    /// `@fluent` on a `LabeledBlock` emits a `FluentOnUnsupportedNode` warning.
    #[test]
    fn fluent_on_labeled_block_emits_warning() {
        let inner_label = Ast::labeled_block("inner".to_string(), Ast::block(vec![]))
            .with_decorators(vec![fluent_bare()]);
        let ast = Ast::block(vec![inner_label]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 diagnostic, got: {errors:?}");
        match &errors[0] {
            AnalysisError::FluentOnUnsupportedNode { node_kind, .. } => {
                assert_eq!(node_kind, "LabeledBlock");
            }
            other => panic!("expected FluentOnUnsupportedNode, got: {other:?}"),
        }
        assert!(
            errors[0].is_warning(),
            "FluentOnUnsupportedNode must be a warning"
        );
    }

    // ── InvalidFluentDecorator errors ─────────────────────────────────────────

    /// Two `@fluent` decorators on the same node is an error.
    #[test]
    fn two_fluent_on_same_node_is_error() {
        let node = decl_node(
            "count",
            vec![fluent_bare(), fluent_with_alias("player-count")],
        );
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidFluentDecorator { reason, .. } => {
                assert!(
                    reason.contains("only one") || reason.contains('2'),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidFluentDecorator, got: {other:?}"),
        }
        assert!(
            !errors[0].is_warning(),
            "InvalidFluentDecorator must be a hard error"
        );
    }

    /// `@fluent` with two string arguments is an error.
    #[test]
    fn fluent_with_two_args_is_error() {
        let node = decl_node("score", vec![fluent_two_args("alias-a", "alias-b")]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidFluentDecorator { reason, .. } => {
                assert!(
                    reason.contains("at most 1 argument") || reason.contains('2'),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidFluentDecorator, got: {other:?}"),
        }
        assert!(!errors[0].is_warning());
    }

    /// `@fluent` with an interpolated string argument is an error.
    #[test]
    fn fluent_with_interpolated_string_arg_is_error() {
        let node = decl_node("score", vec![fluent_with_arg(interp_str_val("name"))]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidFluentDecorator { reason, .. } => {
                assert!(
                    reason.contains("interpolation") || reason.contains("plain"),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidFluentDecorator, got: {other:?}"),
        }
        assert!(!errors[0].is_warning());
    }

    /// `@fluent` with a non-string argument (variable reference) is an error.
    #[test]
    fn fluent_with_non_string_arg_is_error() {
        let node = decl_node("score", vec![fluent_with_arg(ident_val("some_var"))]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidFluentDecorator { reason, .. } => {
                assert!(
                    reason.contains("string literal"),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidFluentDecorator, got: {other:?}"),
        }
        assert!(!errors[0].is_warning());
    }

    /// `@fluent` with an integer argument is an error.
    #[test]
    fn fluent_with_integer_arg_is_error() {
        let int_arg = Ast::value(RuntimeValue::Int(42));
        let node = decl_node("score", vec![fluent_with_arg(int_arg)]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidFluentDecorator { reason, .. } => {
                assert!(
                    reason.contains("string literal"),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidFluentDecorator, got: {other:?}"),
        }
    }

    /// A `@fluent` with an escaped-char string (not interpolation) is valid.
    #[test]
    fn fluent_with_escaped_char_string_is_valid() {
        use crate::lexer::strings::StringPart;
        let parts = vec![StringPart::EscapedChar("n".to_string())];
        let ps_arg = Ast::value(RuntimeValue::Str(ParsedString::new_from_parts(parts)));
        let node = decl_node("score", vec![fluent_with_arg(ps_arg)]);
        let ast = in_label("scene", vec![node]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "escaped-char alias must be valid (no interpolation), got: {errors:?}"
        );
    }

    /// A `@fluent` on a `Menu` node produces a warning (unsupported node kind).
    #[test]
    fn fluent_on_menu_emits_warning() {
        let option = Ast::menu_option("go".to_string(), Ast::block(vec![]), false);
        let menu = Ast::menu(vec![option]).with_decorators(vec![fluent_bare()]);
        let ast = in_label("scene", vec![menu]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 diagnostic, got: {errors:?}");
        match &errors[0] {
            AnalysisError::FluentOnUnsupportedNode { node_kind, .. } => {
                assert_eq!(node_kind, "Menu");
            }
            other => panic!("expected FluentOnUnsupportedNode, got: {other:?}"),
        }
        assert!(errors[0].is_warning());
    }

    /// Multiple declarations, one with valid `@fluent` and one without, is clean.
    #[test]
    fn mixed_declarations_one_with_fluent_is_clean() {
        let d1 = decl_node("gold", vec![fluent_with_alias("player-gold")]);
        let d2 = decl_node("health", vec![]);
        let d3 = decl_node("mana", vec![fluent_bare()]);
        let ast = in_label("scene", vec![d1, d2, d3]);

        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }
}
