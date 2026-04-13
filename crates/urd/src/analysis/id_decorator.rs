//! # `@id` Decorator Analysis Pass
//!
//! Validates the usage of the `@id("custom-segment")` decorator before compilation.
//!
//! ## Rules enforced
//!
//! | Condition | Severity | Diagnostic |
//! |-----------|----------|------------|
//! | More than one `@id` on a single node | Error | [`AnalysisError::InvalidIdDecorator`] |
//! | `@id` has ≠ 1 argument | Error | [`AnalysisError::InvalidIdDecorator`] |
//! | `@id` argument is not a plain string literal | Error | [`AnalysisError::InvalidIdDecorator`] |
//! | `@id` argument contains string interpolation | Error | [`AnalysisError::InvalidIdDecorator`] |
//! | `@id` on a node type that does not support localisation IDs | Warning | [`AnalysisError::IdOnUnsupportedNode`] |
//! | Two direct children of the same [`AstContent::LabeledBlock`] share an `@id` value | Error | [`AnalysisError::DuplicateId`] |
//!
//! ## Supported node types
//!
//! `@id` is meaningful on: [`AstContent::LabeledBlock`], [`AstContent::Menu`],
//! [`AstContent::Match`], [`AstContent::If`], [`AstContent::Dialogue`], and
//! [`AstContent::MenuOption`].  Any other node type triggers
//! [`AnalysisError::IdOnUnsupportedNode`] (warning only — the decorator is
//! otherwise ignored by the compiler).

use std::collections::HashMap;

use chumsky::span::SimpleSpan;

use crate::analysis::AnalysisError;
use crate::lexer::strings::StringPart;
use crate::parser::ast::{Ast, AstContent, Decorator, walk_ast};
use crate::runtime::value::RuntimeValue;

use super::context::node_kind_name;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the `@id` decorator validation pass over `ast` and return all diagnostics.
///
/// Two independent checks are performed:
///
/// 1. **Per-node validation** — every node in the tree is inspected; any
///    ill-formed `@id` decorator emits an [`AnalysisError::InvalidIdDecorator`]
///    or [`AnalysisError::IdOnUnsupportedNode`].
/// 2. **Duplicate detection** — every [`AstContent::LabeledBlock`] body is
///    scanned for direct children that share the same (valid) `@id` value; each
///    collision emits an [`AnalysisError::DuplicateId`].
///
/// All checks always run to completion; a problem found in check 1 does not
/// suppress check 2.
#[must_use]
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors = Vec::new();
    walk_ast(ast, &mut |node| {
        validate_id_decorators(node, &mut errors);
    });
    check_duplicate_ids(ast, &mut errors);
    errors
}

// ---------------------------------------------------------------------------
// Per-node validation
// ---------------------------------------------------------------------------

/// Inspect a single AST node for `@id` decorator problems.
///
/// Rules applied in order (early-return after the first error so that the
/// same `@id` does not produce multiple diagnostics for the same mistake):
///
/// 1. **Multiplicity** — more than one `@id` on the same node →
///    [`AnalysisError::InvalidIdDecorator`].
/// 2. **Argument count** — the `ExprList` that forms the argument list must
///    contain exactly one expression → [`AnalysisError::InvalidIdDecorator`].
/// 3. **Argument type / interpolation** — the single argument must be a
///    [`RuntimeValue::Str`] whose parts contain no
///    [`StringPart::Interpolation`] → [`AnalysisError::InvalidIdDecorator`].
/// 4. **Supported node type** — if the node content is not one of the
///    recognised kinds, [`AnalysisError::IdOnUnsupportedNode`] is emitted
///    (warning, not error).
fn validate_id_decorators(node: &Ast, errors: &mut Vec<AnalysisError>) {
    let id_decorators: Vec<&Decorator> = node
        .decorators()
        .iter()
        .filter(|d| d.name() == "id")
        .collect();

    // Rule 1: at most one @id per node.
    if id_decorators.len() > 1 {
        errors.push(AnalysisError::InvalidIdDecorator {
            reason: format!(
                "only one @id decorator is allowed per node, found {}",
                id_decorators.len()
            ),
            span: node.span(),
        });
        return;
    }

    // No @id on this node — nothing to validate.
    if id_decorators.is_empty() {
        return;
    }

    let decorator = id_decorators[0];

    // Rule 2: argument count must be exactly 1.
    let items = match decorator.args().content() {
        AstContent::ExprList(items) => items,
        _ => {
            // Structurally impossible (the parser always produces ExprList for
            // decorator args), but guard defensively.
            errors.push(AnalysisError::InvalidIdDecorator {
                reason: "@id arguments must be an expression list".to_string(),
                span: decorator.span(),
            });
            return;
        }
    };

    if items.len() != 1 {
        errors.push(AnalysisError::InvalidIdDecorator {
            reason: format!("@id expects exactly 1 argument, got {}", items.len()),
            span: decorator.span(),
        });
        return;
    }

    // Rule 3: argument must be a plain (non-interpolated) string literal.
    let arg = &items[0];
    match arg.content() {
        AstContent::Value(RuntimeValue::Str(ps)) => {
            let has_interpolation = ps
                .parts()
                .iter()
                .any(|p| matches!(p, StringPart::Interpolation(_)));

            if has_interpolation {
                errors.push(AnalysisError::InvalidIdDecorator {
                    reason: "@id value must be a plain string literal with no interpolation"
                        .to_string(),
                    span: decorator.span(),
                });
                return;
            }
        }
        _ => {
            errors.push(AnalysisError::InvalidIdDecorator {
                reason: "@id argument must be a plain string literal".to_string(),
                span: decorator.span(),
            });
            return;
        }
    }

    // Rule 4: @id is only meaningful on certain node types.
    let is_supported = matches!(
        node.content(),
        AstContent::LabeledBlock { .. }
            | AstContent::Menu { .. }
            | AstContent::Match { .. }
            | AstContent::If { .. }
            | AstContent::Dialogue { .. }
            | AstContent::MenuOption { .. }
    );

    if !is_supported {
        errors.push(AnalysisError::IdOnUnsupportedNode {
            node_kind: node_kind_name(node.content()),
            span: node.span(),
        });
    }
}

// ---------------------------------------------------------------------------
// Duplicate-ID detection within LabeledBlock bodies
// ---------------------------------------------------------------------------

/// Walk every [`AstContent::LabeledBlock`] in `ast` and emit
/// [`AnalysisError::DuplicateId`] whenever two *direct children* of the same
/// block share the same valid `@id` value.
///
/// Only valid `@id` strings (as returned by [`crate::loc::extract_id_override`])
/// participate in the check; nodes with malformed `@id` decorators are skipped
/// silently — those problems are already reported by [`validate_id_decorators`].
fn check_duplicate_ids(ast: &Ast, errors: &mut Vec<AnalysisError>) {
    walk_ast(ast, &mut |node| {
        if let AstContent::LabeledBlock { block, .. } = node.content()
            && let AstContent::Block(stmts) = block.content()
        {
            let mut seen: HashMap<String, SimpleSpan> = HashMap::new();
            for stmt in stmts {
                if let Some(id_val) = extract_valid_id_override(stmt.decorators()) {
                    if let Some(&first_span) = seen.get(&id_val) {
                        errors.push(AnalysisError::DuplicateId {
                            id: id_val.clone(),
                            first_span,
                            second_span: stmt.span(),
                        });
                    } else {
                        seen.insert(id_val, stmt.span());
                    }
                }
            }
        }
    });
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extracts a valid (non-interpolated) `@id` string from `decorators`.
///
/// Returns `None` when there is no `@id` decorator, when it is structurally
/// malformed, or when the string contains interpolation.  The per-node
/// validation pass reports those problems separately, so callers here may
/// treat `None` as "no usable override".
fn extract_valid_id_override(decorators: &[Decorator]) -> Option<String> {
    crate::loc::extract_id_override(decorators)
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
    use chumsky::span::{SimpleSpan, Span};

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

    /// Build a valid `@id("value")` decorator.
    fn id_dec(value: &str) -> Decorator {
        Decorator::new("id".to_string(), Ast::expr_list(vec![str_val(value)]))
    }

    /// Build an `@id` decorator with no arguments.
    fn id_dec_no_args() -> Decorator {
        Decorator::new("id".to_string(), Ast::expr_list(vec![]))
    }

    /// Build an `@id` decorator with a custom (possibly invalid) argument.
    fn id_dec_with(arg: Ast) -> Decorator {
        Decorator::new("id".to_string(), Ast::expr_list(vec![arg]))
    }

    /// Build an `@id` decorator with two string arguments.
    fn id_dec_two_args(a: &str, b: &str) -> Decorator {
        Decorator::new(
            "id".to_string(),
            Ast::expr_list(vec![str_val(a), str_val(b)]),
        )
    }

    /// Wrap a list of statements inside a `LabeledBlock` body, then inside a
    /// root `Block`.  The resulting tree is the standard input shape for these
    /// tests.
    fn in_label(name: &str, stmts: Vec<Ast>) -> Ast {
        let block = Ast::block(stmts);
        let label = Ast::labeled_block(name.to_string(), block);
        Ast::block(vec![label])
    }

    fn nonzero_span(start: usize, end: usize) -> SimpleSpan {
        SimpleSpan::new((), start..end)
    }

    // ── InvalidIdDecorator: no arguments ─────────────────────────────────────

    #[test]
    fn id_with_no_args_is_invalid() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"))
            .with_decorators(vec![id_dec_no_args()]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidIdDecorator { reason, .. } => {
                assert!(
                    reason.contains("exactly 1 argument") || reason.contains('0'),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidIdDecorator, got: {other:?}"),
        }
        assert!(
            !errors[0].is_warning(),
            "InvalidIdDecorator must be a hard error"
        );
    }

    // ── InvalidIdDecorator: variable reference argument ───────────────────────

    #[test]
    fn id_with_variable_arg_is_invalid() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"))
            .with_decorators(vec![id_dec_with(ident_val("x"))]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidIdDecorator { reason, .. } => {
                assert!(
                    reason.contains("string literal"),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidIdDecorator, got: {other:?}"),
        }
    }

    // ── InvalidIdDecorator: interpolated string ───────────────────────────────

    #[test]
    fn id_with_interpolated_string_is_invalid() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"))
            .with_decorators(vec![id_dec_with(interp_str_val("name"))]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidIdDecorator { reason, .. } => {
                assert!(
                    reason.contains("interpolation") || reason.contains("plain"),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidIdDecorator, got: {other:?}"),
        }
    }

    // ── InvalidIdDecorator: two arguments ────────────────────────────────────

    #[test]
    fn id_with_two_args_is_invalid() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"))
            .with_decorators(vec![id_dec_two_args("a", "b")]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidIdDecorator { reason, .. } => {
                assert!(
                    reason.contains("exactly 1 argument") || reason.contains('2'),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidIdDecorator, got: {other:?}"),
        }
    }

    // ── InvalidIdDecorator: multiple @id decorators on one node ──────────────

    #[test]
    fn multiple_id_decorators_on_one_node_is_invalid() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"))
            .with_decorators(vec![id_dec("first"), id_dec("second")]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::InvalidIdDecorator { reason, .. } => {
                assert!(
                    reason.contains("only one") || reason.contains('2'),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected InvalidIdDecorator, got: {other:?}"),
        }
    }

    // ── Valid @id on supported node types ────────────────────────────────────

    #[test]
    fn valid_id_on_dialogue_is_clean() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"))
            .with_decorators(vec![id_dec("intro")]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn valid_id_on_menu_is_clean() {
        let option = Ast::menu_option("go".to_string(), Ast::block(vec![]), false);
        let menu = Ast::menu(vec![option]).with_decorators(vec![id_dec("my-menu")]);
        let ast = in_label("scene", vec![menu]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors on @id(Menu), got: {errors:?}"
        );
    }

    #[test]
    fn valid_id_on_menu_option_is_clean() {
        let option = Ast::menu_option("go".to_string(), Ast::block(vec![]), false)
            .with_decorators(vec![id_dec("go-opt")]);
        let menu = Ast::menu(vec![option]);
        let ast = in_label("scene", vec![menu]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors on @id(MenuOption), got: {errors:?}"
        );
    }

    #[test]
    fn valid_id_on_if_is_clean() {
        let if_node = Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![]),
            None,
        )
        .with_decorators(vec![id_dec("my-if")]);
        let ast = in_label("scene", vec![if_node]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors on @id(If), got: {errors:?}"
        );
    }

    #[test]
    fn valid_id_on_labeled_block_is_clean() {
        // An inner label nested inside an outer label.
        let inner = Ast::labeled_block("inner".to_string(), Ast::block(vec![]))
            .with_decorators(vec![id_dec("inner-id")]);
        let outer = Ast::labeled_block("outer".to_string(), Ast::block(vec![inner]));
        let ast = Ast::block(vec![outer]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "expected no errors on @id(LabeledBlock), got: {errors:?}"
        );
    }

    // ── IdOnUnsupportedNode: @id on a Declaration ─────────────────────────────

    #[test]
    fn id_on_declaration_emits_warning() {
        let decl = Ast::decl(
            DeclKind::Variable,
            ident_val("x"),
            Ast::value(RuntimeValue::Int(0)),
        )
        .with_decorators(vec![id_dec("my-var")]);
        let ast = in_label("scene", vec![decl]);

        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 diagnostic, got: {errors:?}");
        match &errors[0] {
            AnalysisError::IdOnUnsupportedNode { node_kind, .. } => {
                assert_eq!(node_kind, "Declaration");
            }
            other => panic!("expected IdOnUnsupportedNode, got: {other:?}"),
        }
        assert!(
            errors[0].is_warning(),
            "IdOnUnsupportedNode must be a warning"
        );
    }

    // ── DuplicateId: two dialogues with the same @id in one label ────────────

    #[test]
    fn duplicate_id_in_same_label_is_an_error() {
        let d1 = Ast::dialogue(ident_val("narrator"), str_val("first"))
            .with_decorators(vec![id_dec("same")])
            .with_span(nonzero_span(10, 30));
        let d2 = Ast::dialogue(ident_val("narrator"), str_val("second"))
            .with_decorators(vec![id_dec("same")])
            .with_span(nonzero_span(40, 60));

        let ast = in_label("scene", vec![d1, d2]);

        let errors = check(&ast);
        assert_eq!(
            errors.len(),
            1,
            "expected 1 DuplicateId error, got: {errors:?}"
        );
        match &errors[0] {
            AnalysisError::DuplicateId { id, .. } => {
                assert_eq!(id, "same");
            }
            other => panic!("expected DuplicateId, got: {other:?}"),
        }
        assert!(!errors[0].is_warning(), "DuplicateId must be a hard error");
    }

    // ── DuplicateId: third occurrence produces a second error ─────────────────

    #[test]
    fn three_same_ids_in_one_label_emits_two_errors() {
        let d1 = Ast::dialogue(ident_val("n"), str_val("a"))
            .with_decorators(vec![id_dec("dup")])
            .with_span(nonzero_span(0, 10));
        let d2 = Ast::dialogue(ident_val("n"), str_val("b"))
            .with_decorators(vec![id_dec("dup")])
            .with_span(nonzero_span(11, 20));
        let d3 = Ast::dialogue(ident_val("n"), str_val("c"))
            .with_decorators(vec![id_dec("dup")])
            .with_span(nonzero_span(21, 30));

        let ast = in_label("scene", vec![d1, d2, d3]);

        let dup_count = check(&ast)
            .into_iter()
            .filter(|e| matches!(e, AnalysisError::DuplicateId { .. }))
            .count();
        assert_eq!(dup_count, 2, "expected 2 DuplicateId errors");
    }

    // ── DuplicateId not triggered across separate labels ─────────────────────

    #[test]
    fn same_id_in_different_labels_is_clean() {
        let d1 =
            Ast::dialogue(ident_val("n"), str_val("first")).with_decorators(vec![id_dec("shared")]);
        let d2 = Ast::dialogue(ident_val("n"), str_val("second"))
            .with_decorators(vec![id_dec("shared")]);

        let label1 = Ast::labeled_block("a".to_string(), Ast::block(vec![d1]));
        let label2 = Ast::labeled_block("b".to_string(), Ast::block(vec![d2]));
        let ast = Ast::block(vec![label1, label2]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "same @id in separate labels must be clean, got: {errors:?}"
        );
    }

    // ── Distinct @id values in the same label body ────────────────────────────

    #[test]
    fn distinct_ids_in_same_label_is_clean() {
        let d1 =
            Ast::dialogue(ident_val("n"), str_val("first")).with_decorators(vec![id_dec("alpha")]);
        let d2 =
            Ast::dialogue(ident_val("n"), str_val("second")).with_decorators(vec![id_dec("beta")]);
        let d3 =
            Ast::dialogue(ident_val("n"), str_val("third")).with_decorators(vec![id_dec("gamma")]);

        let ast = in_label("scene", vec![d1, d2, d3]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "distinct @id values must not trigger DuplicateId, got: {errors:?}"
        );
    }

    // ── @id on label, menu, and dialogue with distinct values ─────────────────

    #[test]
    fn distinct_ids_on_label_menu_and_dialogue_is_clean() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("welcome"))
            .with_decorators(vec![id_dec("welcome-line")]);

        let option = Ast::menu_option("choice".to_string(), Ast::block(vec![]), false);
        let menu = Ast::menu(vec![option]).with_decorators(vec![id_dec("main-menu")]);

        let block = Ast::block(vec![dialogue, menu]);
        let label = Ast::labeled_block("intro".to_string(), block)
            .with_decorators(vec![id_dec("intro-label")]);
        let ast = Ast::block(vec![label]);

        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── EscapedChar string is accepted (not interpolation) ───────────────────

    #[test]
    fn id_with_escaped_char_string_is_valid() {
        let parts = vec![StringPart::EscapedChar("n".to_string())];
        let ps_arg = Ast::value(RuntimeValue::Str(ParsedString::new_from_parts(parts)));
        let dialogue =
            Ast::dialogue(ident_val("narrator"), str_val("hello")).with_decorators(vec![
                Decorator::new("id".to_string(), Ast::expr_list(vec![ps_arg])),
            ]);
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "escaped-char @id must be valid (no interpolation), got: {errors:?}"
        );
    }

    // ── Nodes without any @id produce no diagnostics ─────────────────────────

    #[test]
    fn node_without_id_is_clean() {
        let dialogue = Ast::dialogue(ident_val("narrator"), str_val("hello"));
        let ast = in_label("scene", vec![dialogue]);

        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "node without @id must be clean, got: {errors:?}"
        );
    }

    // ── Empty AST is clean ────────────────────────────────────────────────────

    #[test]
    fn empty_ast_is_clean() {
        let ast = Ast::block(vec![]);
        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "empty AST must be clean, got: {errors:?}"
        );
    }
}
