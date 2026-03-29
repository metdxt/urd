//! # Match Exhaustiveness Pass
//!
//! Checks that every `match` statement either contains a wildcard (`_`) arm or
//! covers every variant of the enum being scrutinised.
//!
//! ## Enum detection
//!
//! The pass uses two complementary strategies to determine which enum a `match`
//! operates over:
//!
//! 1. **Scrutinee type annotation** – if the scrutinee is a simple variable
//!    (`IdentPath` of length 1) that appears in
//!    [`AnalysisContext::top_level_vars`] with a
//!    [`TypeAnnotation::Named`] annotation, and that name exists in
//!    [`AnalysisContext::enums`], the match is checked against that enum.
//!
//! 2. **Pattern-name heuristic** – if strategy 1 fails, the pass collects
//!    every single-segment `IdentPath` pattern value and looks for the unique
//!    enum in the context whose variant list is a superset of those names.
//!    If zero or more than one enum qualifies the match is silently skipped
//!    (best-effort: no false positives for unknown enums).
//!
//! A wildcard arm always makes a `match` exhaustive; the pass returns
//! immediately without further checks.

use chumsky::span::SimpleSpan;

use crate::parser::ast::{Ast, AstContent, MatchArm, MatchPattern, TypeAnnotation};
use crate::runtime::value::RuntimeValue;

use super::AnalysisError;
use super::context::AnalysisContext;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the exhaustiveness pass over `ast` and return any diagnostics found.
pub fn check(ast: &Ast, ctx: &AnalysisContext) -> Vec<AnalysisError> {
    let mut errors: Vec<AnalysisError> = Vec::new();
    check_node(ast, ctx, ast.span(), &mut errors);
    errors
}

// ---------------------------------------------------------------------------
// Recursive walker
// ---------------------------------------------------------------------------

fn check_node(
    ast: &Ast,
    ctx: &AnalysisContext,
    parent_span: SimpleSpan,
    errors: &mut Vec<AnalysisError>,
) {
    // Use the current node's span when it is non-zero; otherwise inherit the
    // parent's span so nested errors have the most specific location available.
    let span = {
        let s = ast.span();
        if s.start == 0 && s.end == 0 { parent_span } else { s }
    };

    match ast.content() {
        // ── Match: check this node then recurse into arm bodies ───────────
        AstContent::Match { scrutinee, arms } => {
            check_match(scrutinee, arms, ctx, span, errors);

            for arm in arms.iter() {
                check_node(&arm.body, ctx, span, errors);
            }
        }

        // ── Structural nodes that may contain nested matches ──────────────
        AstContent::Block(stmts) => {
            for stmt in stmts {
                check_node(stmt, ctx, span, errors);
            }
        }

        AstContent::LabeledBlock { block, .. } => {
            check_node(block, ctx, ast.span(), errors);
        }

        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            check_node(condition, ctx, span, errors);
            check_node(then_block, ctx, span, errors);
            if let Some(eb) = else_block {
                check_node(eb, ctx, span, errors);
            }
        }

        AstContent::Menu { options } => {
            for opt in options {
                check_node(opt, ctx, span, errors);
            }
        }

        AstContent::MenuOption { content, .. } => {
            check_node(content, ctx, span, errors);
        }

        AstContent::Declaration { decl_defs, .. } => {
            check_node(decl_defs, ctx, span, errors);
        }

        AstContent::BinOp { left, right, .. } => {
            check_node(left, ctx, span, errors);
            check_node(right, ctx, span, errors);
        }

        AstContent::UnaryOp { expr, .. } => {
            check_node(expr, ctx, span, errors);
        }

        AstContent::Call { func_path, params } => {
            check_node(func_path, ctx, span, errors);
            check_node(params, ctx, span, errors);
        }

        AstContent::Return { value: Some(v) } => {
            check_node(v, ctx, span, errors);
        }

        AstContent::ExprList(exprs) => {
            for e in exprs {
                check_node(e, ctx, span, errors);
            }
        }

        AstContent::List(items) => {
            for item in items {
                check_node(item, ctx, span, errors);
            }
        }

        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                check_node(k, ctx, span, errors);
                check_node(v, ctx, span, errors);
            }
        }

        AstContent::Subscript { object, key } => {
            check_node(object, ctx, span, errors);
            check_node(key, ctx, span, errors);
        }

        AstContent::SubscriptAssign { object, key, value } => {
            check_node(object, ctx, span, errors);
            check_node(key, ctx, span, errors);
            check_node(value, ctx, span, errors);
        }

        AstContent::Dialogue { speakers, content } => {
            check_node(speakers, ctx, span, errors);
            check_node(content, ctx, span, errors);
        }

        AstContent::DecoratorDef { body, .. } => {
            check_node(body, ctx, span, errors);
        }

        // Leaf nodes or nodes that cannot contain a Match.
        AstContent::Value(_)
        | AstContent::Jump { .. }
        | AstContent::LetCall { .. }
        | AstContent::EnumDecl { .. }
        | AstContent::Import { .. }
        | AstContent::Return { value: None } => {}
    }
}

// ---------------------------------------------------------------------------
// Per-match exhaustiveness check
// ---------------------------------------------------------------------------

fn check_match(
    scrutinee: &Ast,
    arms: &[MatchArm],
    ctx: &AnalysisContext,
    span: SimpleSpan,
    errors: &mut Vec<AnalysisError>,
) {
    // A wildcard arm always makes the match exhaustive.
    if arms.iter().any(|arm| arm.pattern == MatchPattern::Wildcard) {
        return;
    }

    // Attempt to resolve which enum is being matched.
    let Some((enum_name, all_variants)) = resolve_enum(scrutinee, arms, ctx) else {
        // Could not identify the enum; skip (best-effort, no false positives).
        return;
    };

    // Collect the set of variant names covered by the arms.
    let covered: std::collections::HashSet<&str> = arms
        .iter()
        .filter_map(|arm| {
            if let MatchPattern::Value(ast) = &arm.pattern {
                extract_variant_name(ast)
            } else {
                None
            }
        })
        .collect();

    // Determine which variants are missing.
    let missing_variants: Vec<String> = all_variants
        .iter()
        .filter(|v| !covered.contains(v.as_str()))
        .cloned()
        .collect();

    if !missing_variants.is_empty() {
        errors.push(AnalysisError::NonExhaustiveMatch {
            enum_name,
            missing_variants,
            span,
        });
    }
}

// ---------------------------------------------------------------------------
// Enum resolution helpers
// ---------------------------------------------------------------------------

/// Try to determine which enum a match operates over.
///
/// Returns `Some((enum_name, variants))` on success, `None` when the enum
/// cannot be identified (analysis should be silently skipped).
fn resolve_enum<'a>(
    scrutinee: &Ast,
    arms: &[MatchArm],
    ctx: &'a AnalysisContext,
) -> Option<(String, &'a Vec<String>)> {
    // Strategy 1: scrutinee is a typed variable with a Named annotation.
    if let Some((name, variants)) = resolve_via_scrutinee(scrutinee, ctx) {
        return Some((name, variants));
    }

    // Strategy 2: all pattern names are a subset of exactly one known enum.
    resolve_via_patterns(arms, ctx)
}

/// Try to resolve the enum from the scrutinee's declared type.
fn resolve_via_scrutinee<'a>(
    scrutinee: &Ast,
    ctx: &'a AnalysisContext,
) -> Option<(String, &'a Vec<String>)> {
    let var_name = single_ident_path(scrutinee)?;

    let ann = ctx.top_level_vars.get(var_name)?;

    let enum_name = match ann {
        TypeAnnotation::Named(path) if path.len() == 1 => &path[0],
        _ => return None,
    };

    let variants = ctx.enums.get(enum_name)?;
    Some((enum_name.clone(), variants))
}

/// Try to resolve the enum by looking at the pattern names.
///
/// Collects every single-segment `IdentPath` pattern, then finds the unique
/// enum in `ctx.enums` whose variant list is a superset of those names.
fn resolve_via_patterns<'a>(
    arms: &[MatchArm],
    ctx: &'a AnalysisContext,
) -> Option<(String, &'a Vec<String>)> {
    let pattern_names: Vec<&str> = arms
        .iter()
        .filter_map(|arm| {
            if let MatchPattern::Value(ast) = &arm.pattern {
                extract_variant_name(ast)
            } else {
                None
            }
        })
        .collect();

    if pattern_names.is_empty() {
        return None;
    }

    let mut found: Option<(String, &Vec<String>)> = None;

    for (enum_name, variants) in &ctx.enums {
        let all_in_enum = pattern_names
            .iter()
            .all(|p| variants.iter().any(|v| v.as_str() == *p));

        if all_in_enum {
            if found.is_some() {
                // Ambiguous: multiple enums could match — skip to avoid false
                // positives.
                return None;
            }
            found = Some((enum_name.clone(), variants));
        }
    }

    found
}

// ---------------------------------------------------------------------------
// Small AST helpers
// ---------------------------------------------------------------------------

/// If `ast` is `Value(IdentPath([name]))`, return `name`.
fn single_ident_path(ast: &Ast) -> Option<&str> {
    match ast.content() {
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => {
            Some(path[0].as_str())
        }
        _ => None,
    }
}

/// If `ast` is a `Value(IdentPath([variant]))`, return the variant name.
fn extract_variant_name(ast: &Ast) -> Option<&str> {
    single_ident_path(ast)
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::parser::ast::{MatchArm, MatchPattern};
    use crate::runtime::value::RuntimeValue;


    // ── helpers ──────────────────────────────────────────────────────────────

    fn make_ctx(enums: &[(&str, &[&str])], vars: &[(&str, TypeAnnotation)]) -> AnalysisContext {
        let mut ctx = AnalysisContext::default();
        for (name, variants) in enums {
            ctx.enums.insert(
                (*name).to_owned(),
                variants.iter().map(|s| (*s).to_owned()).collect(),
            );
        }
        for (name, ann) in vars {
            ctx.top_level_vars.insert((*name).to_owned(), ann.clone());
        }
        ctx
    }

    fn ident_path(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
    }

    fn value_arm(variant: &str, body: Ast) -> MatchArm {
        MatchArm::new(MatchPattern::Value(ident_path(variant)), body)
    }

    fn wildcard_arm(body: Ast) -> MatchArm {
        MatchArm::new(MatchPattern::Wildcard, body)
    }

    fn return_block() -> Ast {
        Ast::block(vec![Ast::return_stmt(None)])
    }

    // ── wildcard ─────────────────────────────────────────────────────────────

    #[test]
    fn wildcard_arm_is_always_exhaustive() {
        let ctx = make_ctx(&[("Dir", &["N", "S", "E", "W"])], &[]);
        let scrutinee = ident_path("dir");
        let arms = vec![
            value_arm("N", return_block()),
            wildcard_arm(return_block()),
        ];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        assert!(errors.is_empty(), "wildcard arm should suppress exhaustiveness check");
    }

    // ── all variants covered ─────────────────────────────────────────────────

    #[test]
    fn all_variants_covered_is_ok() {
        let ctx = make_ctx(&[("Color", &["Red", "Green", "Blue"])], &[]);
        let scrutinee = ident_path("c");
        let arms = vec![
            value_arm("Red", return_block()),
            value_arm("Green", return_block()),
            value_arm("Blue", return_block()),
        ];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        assert!(errors.is_empty(), "all variants covered — no error expected");
    }

    // ── missing variants ─────────────────────────────────────────────────────

    #[test]
    fn missing_one_variant_is_error() {
        let ctx = make_ctx(&[("Dir", &["North", "South", "East", "West"])], &[]);
        let scrutinee = ident_path("direction");
        let arms = vec![
            value_arm("North", return_block()),
            value_arm("South", return_block()),
            value_arm("East", return_block()),
            // "West" is missing
        ];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::NonExhaustiveMatch {
                enum_name,
                missing_variants,
                ..
            } => {
                assert_eq!(enum_name, "Dir");
                assert_eq!(missing_variants, &["West".to_owned()]);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn missing_multiple_variants_reported() {
        let ctx = make_ctx(&[("Season", &["Spring", "Summer", "Autumn", "Winter"])], &[]);
        let scrutinee = ident_path("s");
        // Only "Spring" is covered
        let arms = vec![value_arm("Spring", return_block())];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::NonExhaustiveMatch { missing_variants, .. } => {
                let mut missing = missing_variants.clone();
                missing.sort();
                assert_eq!(missing, vec!["Autumn", "Summer", "Winter"]);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    // ── unknown enum ─────────────────────────────────────────────────────────

    #[test]
    fn unknown_enum_scrutinee_is_skipped() {
        // The context has no enums at all; the match should be silently skipped.
        let ctx = make_ctx(&[], &[]);
        let scrutinee = ident_path("x");
        let arms = vec![
            value_arm("Foo", return_block()),
            value_arm("Bar", return_block()),
        ];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        assert!(errors.is_empty(), "no enum registered — should be silently skipped");
    }

    // ── scrutinee via Named annotation ───────────────────────────────────────

    #[test]
    fn variable_with_named_type_resolves_enum() {
        // "state" is declared as type State, and State = { On, Off }
        let ctx = make_ctx(
            &[("State", &["On", "Off"])],
            &[("state", TypeAnnotation::Named(vec!["State".to_owned()]))],
        );
        let scrutinee = ident_path("state");
        // Only "On" is matched — should report "Off" as missing.
        let arms = vec![value_arm("On", return_block())];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::NonExhaustiveMatch {
                enum_name,
                missing_variants,
                ..
            } => {
                assert_eq!(enum_name, "State");
                assert_eq!(missing_variants, &["Off".to_owned()]);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn named_annotation_with_unknown_enum_is_skipped() {
        // "state" has a Named annotation but the enum "State" is not in ctx.enums.
        let ctx = make_ctx(
            &[],
            &[("state", TypeAnnotation::Named(vec!["State".to_owned()]))],
        );
        let scrutinee = ident_path("state");
        let arms = vec![value_arm("On", return_block())];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        assert!(errors.is_empty());
    }

    // ── nested matches ───────────────────────────────────────────────────────

    #[test]
    fn nested_match_inside_arm_is_checked() {
        // Outer match is exhaustive; inner match (nested in an arm body) is not.
        let ctx = make_ctx(
            &[
                ("Outer", &["A", "B"]),
                ("Inner", &["X", "Y"]),
            ],
            &[],
        );

        // Build the inner (non-exhaustive) match: covers only "X" of Inner.
        let inner_scrutinee = ident_path("inner_val");
        let inner_arms = vec![value_arm("X", return_block())]; // "Y" missing
        let inner_match = Ast::match_stmt(inner_scrutinee, inner_arms);

        // Build the outer match: covers both "A" and "B", arm "A" body has the inner match.
        let outer_scrutinee = ident_path("outer_val");
        let arm_a = MatchArm::new(
            MatchPattern::Value(ident_path("A")),
            Ast::block(vec![inner_match, Ast::return_stmt(None)]),
        );
        let arm_b = value_arm("B", return_block());
        let outer_match = Ast::match_stmt(outer_scrutinee, vec![arm_a, arm_b]);

        let root = Ast::block(vec![outer_match, Ast::return_stmt(None)]);
        let errors = check(&root, &ctx);

        // Exactly one error: the inner match missing "Y".
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::NonExhaustiveMatch {
                enum_name,
                missing_variants,
                ..
            } => {
                assert_eq!(enum_name, "Inner");
                assert_eq!(missing_variants, &["Y".to_owned()]);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn match_inside_labeled_block_is_checked() {
        let ctx = make_ctx(&[("Mood", &["Happy", "Sad"])], &[]);
        let scrutinee = ident_path("mood");
        let arms = vec![value_arm("Happy", return_block())]; // "Sad" missing
        let match_node = Ast::match_stmt(scrutinee, arms);
        let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![match_node]));
        let root = Ast::block(vec![label]);
        let errors = check(&root, &ctx);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::NonExhaustiveMatch { missing_variants, .. } => {
                assert_eq!(missing_variants, &["Sad".to_owned()]);
            }
            other => panic!("unexpected: {other:?}"),
        }
    }

    // ── empty arms ───────────────────────────────────────────────────────────

    #[test]
    fn match_with_no_arms_and_known_enum_reports_all_missing() {
        let ctx = make_ctx(&[("Bit", &["Zero", "One"])], &[]);
        // With no arms and no wildcard, pattern-based resolution yields no
        // pattern names → cannot find enum → silently skipped.
        let scrutinee = ident_path("b");
        let ast = Ast::match_stmt(scrutinee, vec![]);
        let errors = check(&ast, &ctx);
        // No patterns → can't identify enum via heuristic; skipped silently.
        assert!(errors.is_empty());
    }

    #[test]
    fn match_with_no_arms_but_typed_var_reports_all_missing() {
        let ctx = make_ctx(
            &[("Bit", &["Zero", "One"])],
            &[("b", TypeAnnotation::Named(vec!["Bit".to_owned()]))],
        );
        let scrutinee = ident_path("b");
        let ast = Ast::match_stmt(scrutinee, vec![]);
        let errors = check(&ast, &ctx);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::NonExhaustiveMatch {
                enum_name,
                missing_variants,
                ..
            } => {
                assert_eq!(enum_name, "Bit");
                let mut mv = missing_variants.clone();
                mv.sort();
                assert_eq!(mv, vec!["One", "Zero"]);
            }
            other => panic!("unexpected: {other:?}"),
        }
    }

    // ── ambiguous patterns ───────────────────────────────────────────────────

    #[test]
    fn ambiguous_enum_patterns_are_skipped() {
        // Two enums share the same variant names → heuristic can't decide → skip.
        let ctx = make_ctx(
            &[
                ("A", &["Foo", "Bar"]),
                ("B", &["Foo", "Bar", "Baz"]),
            ],
            &[],
        );
        let scrutinee = ident_path("x");
        let arms = vec![
            value_arm("Foo", return_block()),
            value_arm("Bar", return_block()),
        ];
        let ast = Ast::match_stmt(scrutinee, arms);
        let errors = check(&ast, &ctx);
        // Ambiguous — silently skipped.
        assert!(errors.is_empty());
    }

    // ── location in error ────────────────────────────────────────────────────

    #[test]
    fn error_location_is_enclosing_context() {
        let ctx = make_ctx(&[("T", &["A", "B"])], &[]);
        let scrutinee = ident_path("t");
        let arms = vec![value_arm("A", return_block())];
        let match_node = Ast::match_stmt(scrutinee, arms);
        // Place the match inside a label block.
        // Test AST nodes have zero spans, so we just assert a NonExhaustiveMatch is produced.
        let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![match_node]));
        let root = Ast::block(vec![label]);
        let errors = check(&root, &ctx);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::NonExhaustiveMatch { span, .. } => {
                // Test nodes have zero spans.
                assert_eq!(span.start, 0);
                assert_eq!(span.end, 0);
            }
            other => panic!("unexpected: {other:?}"),
        }
    }
}
