//! # Type-Annotation Compatibility Pass
//!
//! Walks the AST and checks that every value assigned to a variable with an
//! explicit type annotation is compatible with that annotation.
//!
//! ## Checked constructs
//!
//! 1. **`Declaration`** – when a variable/constant/global is declared with an
//!    explicit `: TypeAnnotation` and its initialiser is a literal
//!    [`AstContent::Value`], the literal's type is checked against the
//!    annotation.  The variable is then entered into the [`ScopeStack`] so
//!    that subsequent assignment checks can find its type.
//!
//! 2. **`BinOp { op: Assign, .. }`** – when the left-hand side is a simple
//!    [`IdentPath`] that resolves to a typed variable in the current scope,
//!    the right-hand side (if it is a literal [`AstContent::Value`]) is
//!    checked against the recorded type.
//!
//! ## Compatibility rules
//!
//! | Annotation      | Compatible value types                                     |
//! |-----------------|------------------------------------------------------------|
//! | `Int`           | `Int`                                                      |
//! | `Float`         | `Float`, `Int` (widening)                                  |
//! | `Bool`          | `Bool`                                                     |
//! | `Str`           | `Str`                                                      |
//! | `Null`          | `Null`                                                     |
//! | `List`          | (list literal – checked structurally, always ok for now)  |
//! | `Map`           | (map literal – checked structurally, always ok for now)   |
//! | `Dice`          | `Dice`                                                     |
//! | `Label`         | `Label { .. }`                                             |
//! | `Named(_)`      | single-segment `IdentPath` (enum variant), `Null`          |
//!
//! Any combination not listed above is considered a [`AnalysisError::TypeMismatch`].
//!
//! The pass is best-effort: non-literal right-hand sides (e.g. arithmetic
//! expressions, function call results) are silently accepted because their
//! types cannot be determined statically without full type inference.

use crate::parser::ast::{Ast, AstContent, Operator, TypeAnnotation};
use crate::runtime::value::RuntimeValue;

use super::context::{AnalysisContext, ScopeStack, extract_decl_name};
use super::{AnalysisError, NodeDescription};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the type-compatibility pass over `ast` and return any diagnostics found.
pub fn check(ast: &Ast, ctx: &AnalysisContext) -> Vec<AnalysisError> {
    let mut errors: Vec<AnalysisError> = Vec::new();
    let mut scope = ScopeStack::new(&ctx.top_level_vars);
    check_node(ast, ctx, &NodeDescription::top_level(), &mut scope, &mut errors);
    errors
}

// ---------------------------------------------------------------------------
// Recursive walker
// ---------------------------------------------------------------------------

fn check_node(
    ast: &Ast,
    ctx: &AnalysisContext,
    location: &NodeDescription,
    scope: &mut ScopeStack,
    errors: &mut Vec<AnalysisError>,
) {
    match ast.content() {
        // ── Declaration with type annotation ─────────────────────────────
        AstContent::Declaration {
            decl_name,
            type_annotation: Some(ann),
            decl_defs,
            ..
        } => {
            // Check that the initialiser is compatible with the annotation.
            if let Some(var_name) = extract_decl_name(decl_name) {
                check_value_compat(
                    decl_defs,
                    ann,
                    &var_name,
                    location,
                    ctx,
                    errors,
                );
                // Register the variable so later assignments can be checked.
                scope.declare(var_name, ann.clone());
            }
            // Recurse into the initialiser for nested declarations/assigns.
            check_node(decl_defs, ctx, location, scope, errors);
        }

        // ── Declaration without type annotation (still recurse) ───────────
        AstContent::Declaration {
            type_annotation: None,
            decl_defs,
            ..
        } => {
            check_node(decl_defs, ctx, location, scope, errors);
        }

        // ── Assignment: `left = right` ────────────────────────────────────
        AstContent::BinOp {
            op: Operator::Assign,
            left,
            right,
        } => {
            // Resolve the variable name on the LHS.
            if let AstContent::Value(RuntimeValue::IdentPath(path)) = left.content()
                && path.len() == 1
            {
                let var_name = &path[0];
                if let Some(ann) = scope.lookup(var_name).cloned() {
                    check_value_compat(right, &ann, var_name, location, ctx, errors);
                }
            }
            check_node(left, ctx, location, scope, errors);
            check_node(right, ctx, location, scope, errors);
        }

        // ── Other BinOp: recurse into both sides ─────────────────────────
        AstContent::BinOp { left, right, .. } => {
            check_node(left, ctx, location, scope, errors);
            check_node(right, ctx, location, scope, errors);
        }

        // ── Block: push a new scope, walk statements, pop ────────────────
        AstContent::Block(stmts) => {
            scope.push();
            for stmt in stmts {
                check_node(stmt, ctx, location, scope, errors);
            }
            scope.pop();
        }

        // ── LabeledBlock ─────────────────────────────────────────────────
        AstContent::LabeledBlock { label, block } => {
            let inner_loc = NodeDescription::label(label);
            scope.push();
            check_node(block, ctx, &inner_loc, scope, errors);
            scope.pop();
        }

        // ── If ────────────────────────────────────────────────────────────
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            check_node(condition, ctx, location, scope, errors);
            scope.push();
            check_node(then_block, ctx, location, scope, errors);
            scope.pop();
            if let Some(eb) = else_block {
                scope.push();
                check_node(eb, ctx, location, scope, errors);
                scope.pop();
            }
        }

        // ── Menu ──────────────────────────────────────────────────────────
        AstContent::Menu { options } => {
            for opt in options {
                check_node(opt, ctx, location, scope, errors);
            }
        }

        // ── MenuOption ────────────────────────────────────────────────────
        AstContent::MenuOption { label, content } => {
            let opt_loc = NodeDescription::menu_option(label, location);
            scope.push();
            check_node(content, ctx, &opt_loc, scope, errors);
            scope.pop();
        }

        // ── Match ─────────────────────────────────────────────────────────
        AstContent::Match { scrutinee, arms } => {
            check_node(scrutinee, ctx, location, scope, errors);
            for (i, arm) in arms.iter().enumerate() {
                let arm_loc = NodeDescription::match_arm(i, location);
                scope.push();
                check_node(&arm.body, ctx, &arm_loc, scope, errors);
                scope.pop();
            }
        }

        // ── Call ──────────────────────────────────────────────────────────
        AstContent::Call { func_path, params } => {
            check_node(func_path, ctx, location, scope, errors);
            check_node(params, ctx, location, scope, errors);
        }

        // ── Return ────────────────────────────────────────────────────────
        AstContent::Return { value: Some(v) } => {
            check_node(v, ctx, location, scope, errors);
        }

        // ── Unary ─────────────────────────────────────────────────────────
        AstContent::UnaryOp { expr, .. } => {
            check_node(expr, ctx, location, scope, errors);
        }

        // ── ExprList ──────────────────────────────────────────────────────
        AstContent::ExprList(exprs) => {
            for e in exprs {
                check_node(e, ctx, location, scope, errors);
            }
        }

        // ── List ──────────────────────────────────────────────────────────
        AstContent::List(items) => {
            for item in items {
                check_node(item, ctx, location, scope, errors);
            }
        }

        // ── Map ───────────────────────────────────────────────────────────
        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                check_node(k, ctx, location, scope, errors);
                check_node(v, ctx, location, scope, errors);
            }
        }

        // ── Subscript ─────────────────────────────────────────────────────
        AstContent::Subscript { object, key } => {
            check_node(object, ctx, location, scope, errors);
            check_node(key, ctx, location, scope, errors);
        }

        // ── SubscriptAssign ───────────────────────────────────────────────
        AstContent::SubscriptAssign { object, key, value } => {
            check_node(object, ctx, location, scope, errors);
            check_node(key, ctx, location, scope, errors);
            check_node(value, ctx, location, scope, errors);
        }

        // ── Dialogue ──────────────────────────────────────────────────────
        AstContent::Dialogue { speakers, content } => {
            check_node(speakers, ctx, location, scope, errors);
            check_node(content, ctx, location, scope, errors);
        }

        // ── DecoratorDef ──────────────────────────────────────────────────
        AstContent::DecoratorDef { body, .. } => {
            check_node(body, ctx, location, scope, errors);
        }

        // ── Leaf / no-op nodes ────────────────────────────────────────────
        AstContent::Value(_)
        | AstContent::Return { value: None }
        | AstContent::Jump { .. }
        | AstContent::LetCall { .. }
        | AstContent::EnumDecl { .. }
        | AstContent::Import { .. } => {}
    }
}

// ---------------------------------------------------------------------------
// Type compatibility check
// ---------------------------------------------------------------------------

/// If `value_ast` is a literal [`AstContent::Value`] node, check that the
/// runtime value is compatible with `expected`.  Pushes a
/// [`AnalysisError::TypeMismatch`] into `errors` on mismatch.
///
/// Non-literal nodes (expressions, call results, etc.) are silently accepted
/// because we cannot determine their type without full type inference.
fn check_value_compat(
    value_ast: &Ast,
    expected: &TypeAnnotation,
    variable: &str,
    location: &NodeDescription,
    ctx: &AnalysisContext,
    errors: &mut Vec<AnalysisError>,
) {
    let rv = match value_ast.content() {
        AstContent::Value(rv) => rv,
        // Non-literal: cannot check statically — silently accept.
        AstContent::List(_) => {
            // A list literal is compatible with a List annotation, or we
            // just skip the check for other annotations (best-effort).
            if !matches!(expected, TypeAnnotation::List) {
                errors.push(AnalysisError::TypeMismatch {
                    variable: variable.to_owned(),
                    expected: expected.clone(),
                    got: "list".to_owned(),
                    location: location.clone(),
                });
            }
            return;
        }
        AstContent::Map(_) => {
            if !matches!(expected, TypeAnnotation::Map) {
                errors.push(AnalysisError::TypeMismatch {
                    variable: variable.to_owned(),
                    expected: expected.clone(),
                    got: "map".to_owned(),
                    location: location.clone(),
                });
            }
            return;
        }
        _ => return,
    };

    if is_compatible(rv, expected, ctx) {
        return;
    }

    errors.push(AnalysisError::TypeMismatch {
        variable: variable.to_owned(),
        expected: expected.clone(),
        got: runtime_value_type_name(rv),
        location: location.clone(),
    });
}

/// Returns `true` if `value` is compatible with `annotation`.
fn is_compatible(value: &RuntimeValue, annotation: &TypeAnnotation, ctx: &AnalysisContext) -> bool {
    // An IdentPath with more than one segment is always a qualified path
    // (e.g. `module.thing`) that we cannot resolve statically.  A single-segment
    // IdentPath *might* be an enum variant (handled by the Named arm below) or a
    // plain variable reference whose type we don't know.  For non-Named
    // annotations we treat any IdentPath as a variable reference and accept it
    // silently — the runtime will catch real type errors.
    if let RuntimeValue::IdentPath(_) = value
        && !matches!(annotation, TypeAnnotation::Named(_))
    {
        return true;
    }

    match annotation {
        TypeAnnotation::Int => matches!(value, RuntimeValue::Int(_)),

        // Float accepts both float literals and int literals (widening).
        TypeAnnotation::Float => {
            matches!(value, RuntimeValue::Float(_) | RuntimeValue::Int(_))
        }

        TypeAnnotation::Bool => matches!(value, RuntimeValue::Bool(_)),

        TypeAnnotation::Str => matches!(value, RuntimeValue::Str(_)),

        TypeAnnotation::Null => matches!(value, RuntimeValue::Null),

        TypeAnnotation::List => {
            // A List(RuntimeValue) from the Value variant is not possible for
            // list literals (those are AstContent::List), so this arm handles
            // the case where someone stores a list value from an expression.
            // Best-effort: always compatible when annotation is List.
            true
        }

        TypeAnnotation::Map => {
            // Same reasoning as List above.
            true
        }

        TypeAnnotation::Dice => matches!(value, RuntimeValue::Dice(_, _)),

        TypeAnnotation::Label => matches!(value, RuntimeValue::Label { .. }),

        TypeAnnotation::Named(path) => {
            match value {
                // `null` is always compatible with a Named type (nullable enum).
                RuntimeValue::Null => true,
                // A single-segment IdentPath is treated as an enum variant.
                RuntimeValue::IdentPath(ident_path) if ident_path.len() == 1 => {
                    // If we can find the enum, verify the variant exists.
                    let enum_name = path.first().map(String::as_str).unwrap_or("");
                    if let Some(variants) = ctx.enums.get(enum_name) {
                        variants.iter().any(|v| v.as_str() == ident_path[0].as_str())
                    } else {
                        // Enum not in context — best-effort accept.
                        true
                    }
                }
                _ => false,
            }
        }
    }
}

/// Returns a short human-readable name for the type of `value`.
fn runtime_value_type_name(value: &RuntimeValue) -> String {
    match value {
        RuntimeValue::Null => "null".to_owned(),
        RuntimeValue::Bool(_) => "bool".to_owned(),
        RuntimeValue::Int(_) => "int".to_owned(),
        RuntimeValue::Float(_) => "float".to_owned(),
        RuntimeValue::Str(_) => "str".to_owned(),
        RuntimeValue::Dice(_, _) => "dice".to_owned(),
        RuntimeValue::IdentPath(path) => {
            if path.len() == 1 {
                format!("identifier '{}'", path[0])
            } else {
                format!("path '{}'", path.join("."))
            }
        }
        RuntimeValue::Label { name, .. } => format!("label '{name}'"),
        RuntimeValue::Map(_) => "map".to_owned(),
        RuntimeValue::ScriptDecorator { .. } => "decorator".to_owned(),
    }
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::parser::ast::{DeclKind, TypeAnnotation};
    use crate::runtime::value::RuntimeValue;


    // ── helpers ───────────────────────────────────────────────────────────────

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

    fn ident(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
    }

    fn int_lit(n: i64) -> Ast {
        Ast::value(RuntimeValue::Int(n))
    }

    fn float_lit(f: f64) -> Ast {
        Ast::value(RuntimeValue::Float(f))
    }

    fn bool_lit(b: bool) -> Ast {
        Ast::value(RuntimeValue::Bool(b))
    }

    fn null_lit() -> Ast {
        Ast::value(RuntimeValue::Null)
    }

    fn str_lit(s: &str) -> Ast {
        Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain(s),
        ))
    }

    fn typed_decl(name: &str, ann: TypeAnnotation, val: Ast) -> Ast {
        Ast::typed_decl(DeclKind::Variable, ident(name), ann, val)
    }

    fn assert_no_errors(errors: &[AnalysisError]) {
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    fn assert_type_mismatch(errors: &[AnalysisError], var: &str) {
        assert_eq!(errors.len(), 1, "expected exactly 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::TypeMismatch { variable, .. } => {
                assert_eq!(variable.as_str(), var);
            }
            other => panic!("expected TypeMismatch, got: {other:?}"),
        }
    }

    // ── Int ───────────────────────────────────────────────────────────────────

    #[test]
    fn int_literal_compatible_with_int_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, int_lit(42))]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn float_not_compatible_with_int_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl(
            "x",
            TypeAnnotation::Int,
            float_lit(1.5),
        )]);
        assert_type_mismatch(&check(&ast, &ctx), "x");
    }

    #[test]
    fn str_literal_incompatible_with_int_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl(
            "x",
            TypeAnnotation::Int,
            str_lit("hello"),
        )]);
        assert_type_mismatch(&check(&ast, &ctx), "x");
    }

    // ── Float ─────────────────────────────────────────────────────────────────

    #[test]
    fn float_literal_compatible_with_float_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("f", TypeAnnotation::Float, float_lit(3.14))]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn int_literal_widened_to_float() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("f", TypeAnnotation::Float, int_lit(0))]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn bool_not_compatible_with_float_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("f", TypeAnnotation::Float, bool_lit(true))]);
        assert_type_mismatch(&check(&ast, &ctx), "f");
    }

    // ── Bool ─────────────────────────────────────────────────────────────────

    #[test]
    fn bool_literal_compatible_with_bool_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("b", TypeAnnotation::Bool, bool_lit(false))]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn int_not_compatible_with_bool_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("b", TypeAnnotation::Bool, int_lit(1))]);
        assert_type_mismatch(&check(&ast, &ctx), "b");
    }

    // ── Str ───────────────────────────────────────────────────────────────────

    #[test]
    fn str_literal_compatible_with_str_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("s", TypeAnnotation::Str, str_lit("hi"))]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn int_not_compatible_with_str_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("s", TypeAnnotation::Str, int_lit(0))]);
        assert_type_mismatch(&check(&ast, &ctx), "s");
    }

    // ── Null ─────────────────────────────────────────────────────────────────

    #[test]
    fn null_compatible_with_null_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("n", TypeAnnotation::Null, null_lit())]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn bool_not_compatible_with_null_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("n", TypeAnnotation::Null, bool_lit(true))]);
        assert_type_mismatch(&check(&ast, &ctx), "n");
    }

    // ── Named (enum) ──────────────────────────────────────────────────────────

    #[test]
    fn enum_variant_compatible_with_named_annotation() {
        let ctx = make_ctx(&[("Dir", &["North", "South"])], &[]);
        let val = Ast::value(RuntimeValue::IdentPath(vec!["North".to_owned()]));
        let ast = Ast::block(vec![typed_decl(
            "d",
            TypeAnnotation::Named(vec!["Dir".to_owned()]),
            val,
        )]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn invalid_enum_variant_reports_mismatch() {
        let ctx = make_ctx(&[("Dir", &["North", "South"])], &[]);
        let val = Ast::value(RuntimeValue::IdentPath(vec!["East".to_owned()]));
        let ast = Ast::block(vec![typed_decl(
            "d",
            TypeAnnotation::Named(vec!["Dir".to_owned()]),
            val,
        )]);
        assert_type_mismatch(&check(&ast, &ctx), "d");
    }

    #[test]
    fn null_compatible_with_named_annotation() {
        // Named types accept null (nullable enum).
        let ctx = make_ctx(&[("Dir", &["North", "South"])], &[]);
        let ast = Ast::block(vec![typed_decl(
            "d",
            TypeAnnotation::Named(vec!["Dir".to_owned()]),
            null_lit(),
        )]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn unknown_named_enum_accepted_best_effort() {
        // If the enum is not in the context we cannot verify, so we accept.
        let ctx = make_ctx(&[], &[]);
        let val = Ast::value(RuntimeValue::IdentPath(vec!["Maybe".to_owned()]));
        let ast = Ast::block(vec![typed_decl(
            "x",
            TypeAnnotation::Named(vec!["Unknown".to_owned()]),
            val,
        )]);
        assert_no_errors(&check(&ast, &ctx));
    }

    // ── Dice ─────────────────────────────────────────────────────────────────

    #[test]
    fn dice_value_compatible_with_dice_annotation() {
        let ctx = make_ctx(&[], &[]);
        let val = Ast::value(RuntimeValue::Dice(2, 6));
        let ast = Ast::block(vec![typed_decl("roll", TypeAnnotation::Dice, val)]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn int_not_compatible_with_dice_annotation() {
        let ctx = make_ctx(&[], &[]);
        let ast = Ast::block(vec![typed_decl("roll", TypeAnnotation::Dice, int_lit(6))]);
        assert_type_mismatch(&check(&ast, &ctx), "roll");
    }

    // ── List / Map (structural) ───────────────────────────────────────────────

    #[test]
    fn list_literal_compatible_with_list_annotation() {
        let ctx = make_ctx(&[], &[]);
        let list = Ast::list(vec![int_lit(1), int_lit(2)]);
        let ast = Ast::block(vec![typed_decl("lst", TypeAnnotation::List, list)]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn map_literal_compatible_with_map_annotation() {
        let ctx = make_ctx(&[], &[]);
        let map = Ast::map(vec![(ident("key"), int_lit(1))]);
        let ast = Ast::block(vec![typed_decl("m", TypeAnnotation::Map, map)]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn list_literal_incompatible_with_int_annotation() {
        let ctx = make_ctx(&[], &[]);
        let list = Ast::list(vec![int_lit(1)]);
        let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, list)]);
        assert_type_mismatch(&check(&ast, &ctx), "x");
    }

    #[test]
    fn map_literal_incompatible_with_bool_annotation() {
        let ctx = make_ctx(&[], &[]);
        let map = Ast::map(vec![]);
        let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Bool, map)]);
        assert_type_mismatch(&check(&ast, &ctx), "x");
    }

    // ── Reassignment (BinOp Assign) ───────────────────────────────────────────

    #[test]
    fn reassignment_compatible_type_ok() {
        let ctx = make_ctx(
            &[],
            &[("score", TypeAnnotation::Int)],
        );
        // score = 100
        let assign = Ast::assign_op(ident("score"), int_lit(100));
        let ast = Ast::block(vec![assign]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn reassignment_incompatible_type_reports_error() {
        let ctx = make_ctx(
            &[],
            &[("score", TypeAnnotation::Int)],
        );
        // score = true  (wrong type)
        let assign = Ast::assign_op(ident("score"), bool_lit(true));
        let ast = Ast::block(vec![assign]);
        assert_type_mismatch(&check(&ast, &ctx), "score");
    }

    #[test]
    fn reassignment_float_widen_from_int_ok() {
        let ctx = make_ctx(&[], &[("ratio", TypeAnnotation::Float)]);
        let assign = Ast::assign_op(ident("ratio"), int_lit(1));
        let ast = Ast::block(vec![assign]);
        assert_no_errors(&check(&ast, &ctx));
    }

    #[test]
    fn reassignment_unknown_variable_is_skipped() {
        // "unknown" has no type annotation — assignment check is silently skipped.
        let ctx = make_ctx(&[], &[]);
        let assign = Ast::assign_op(ident("unknown"), bool_lit(false));
        let ast = Ast::block(vec![assign]);
        assert_no_errors(&check(&ast, &ctx));
    }

    // ── Declaration with subsequent reassignment ───────────────────────────────

    #[test]
    fn local_declaration_then_bad_reassignment_reports_error() {
        // Declare x: Int = 0, then x = true (mismatch).
        let ctx = make_ctx(&[], &[]);
        let decl = typed_decl("x", TypeAnnotation::Int, int_lit(0));
        let assign = Ast::assign_op(ident("x"), bool_lit(true));
        let ast = Ast::block(vec![decl, assign]);
        let errors = check(&ast, &ctx);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::TypeMismatch { variable, .. } => {
                assert_eq!(variable.as_str(), "x");
            }
            other => panic!("expected TypeMismatch, got: {other:?}"),
        }
    }

    #[test]
    fn local_declaration_then_good_reassignment_ok() {
        let ctx = make_ctx(&[], &[]);
        let decl = typed_decl("x", TypeAnnotation::Int, int_lit(0));
        let assign = Ast::assign_op(ident("x"), int_lit(99));
        let ast = Ast::block(vec![decl, assign]);
        assert_no_errors(&check(&ast, &ctx));
    }

    // ── Non-literal initialisers are silently accepted ─────────────────────────

    #[test]
    fn expression_initialiser_is_silently_accepted() {
        let ctx = make_ctx(&[], &[]);
        // x: Int = 1 + 2  (BinOp, not a literal Value — silently ok)
        let init = Ast::add_op(int_lit(1), int_lit(2));
        let ast = Ast::block(vec![typed_decl("x", TypeAnnotation::Int, init)]);
        assert_no_errors(&check(&ast, &ctx));
    }

    // ── Multiple errors collected ──────────────────────────────────────────────

    #[test]
    fn multiple_mismatches_all_reported() {
        let ctx = make_ctx(&[], &[]);
        let d1 = typed_decl("a", TypeAnnotation::Int, bool_lit(true)); // mismatch
        let d2 = typed_decl("b", TypeAnnotation::Bool, int_lit(0));   // mismatch
        let d3 = typed_decl("c", TypeAnnotation::Str, str_lit("ok")); // ok
        let ast = Ast::block(vec![d1, d2, d3]);
        let errors = check(&ast, &ctx);
        assert_eq!(errors.len(), 2);
    }

    // ── Scope: inner block shadows outer ──────────────────────────────────────

    #[test]
    fn inner_scope_shadowed_variable_checked_against_inner_type() {
        let ctx = make_ctx(&[], &[]);
        // Outer: x: Int = 0
        // Inner block: x: Bool = true  (shadows outer; compatible with Bool)
        let inner_decl = typed_decl("x", TypeAnnotation::Bool, bool_lit(true));
        let inner_assign = Ast::assign_op(ident("x"), bool_lit(false));
        let inner_block = Ast::block(vec![inner_decl, inner_assign]);
        let outer_decl = typed_decl("x", TypeAnnotation::Int, int_lit(0));
        let ast = Ast::block(vec![outer_decl, inner_block]);
        assert_no_errors(&check(&ast, &ctx));
    }
}
