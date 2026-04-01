//! # Always-Dead Branch Pass
//!
//! Warns when an `if` condition is composed entirely of compile-time-known
//! constant values, making one of its branches permanently unreachable.
//!
//! ## Algorithm
//!
//! 1. Build a `const_vals: HashMap<String, bool>` from every `const` declaration
//!    at the **top-level block** (direct children of the root `Block`, *not*
//!    inside labels) whose initialiser is `Value(Bool(b))`.
//! 2. Walk the entire AST looking for `If { condition, then_block, else_block }` nodes.
//! 3. For each `if`, attempt to *const-fold* the condition:
//!    - `Value(Bool(b))`                            → `Some(b)`
//!    - `Value(IdentPath([name]))` in `const_vals`  → `Some(const_vals[name])`
//!    - `BinOp { And, left, right }`                → fold both; `Some(l && r)` if both `Some`
//!    - `BinOp { Or,  left, right }`                → fold both; `Some(l || r)` if both `Some`
//!    - `UnaryOp { Not, expr }`                     → fold expr; `Some(!b)` if `Some(b)`
//!    - anything else                               → `None`
//! 4. If the fold succeeds:
//!    - `Some(true)`  → else branch is dead (`dead_branch_is_then: false`),
//!                      only emitted when an else branch is actually present.
//!    - `Some(false)` → then branch is dead (`dead_branch_is_then: true`).
//! 5. The diagnostic span is the span of the `if` node itself.

use std::collections::HashMap;

use crate::parser::ast::{Ast, AstContent, DeclKind, Operator, UnaryOperator};
use crate::runtime::value::RuntimeValue;

use super::AnalysisError;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the always-dead-branch pass over `ast` and return any diagnostics.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let const_vals = collect_top_level_bool_consts(ast);
    let mut errors = Vec::new();
    walk_for_dead_branches(ast, &const_vals, &mut errors);
    errors
}

// ---------------------------------------------------------------------------
// Step 1: collect top-level bool consts
// ---------------------------------------------------------------------------

/// Collect every `const name = Value(Bool(b))` declaration that is a *direct*
/// child of the root `Block` (i.e. at the top level, not inside a label).
///
/// Returns a map `name → bool`.
fn collect_top_level_bool_consts(root: &Ast) -> HashMap<String, bool> {
    let mut map = HashMap::new();

    let stmts = match root.content() {
        AstContent::Block(stmts) => stmts.as_slice(),
        // If the root is a single labeled block (common in tests), there are
        // no top-level const declarations.
        _ => return map,
    };

    for stmt in stmts {
        if let AstContent::Declaration {
            kind: DeclKind::Constant,
            decl_name,
            decl_defs,
            ..
        } = stmt.content()
        {
            if let Some(name) = extract_simple_ident(decl_name) {
                if let AstContent::Value(RuntimeValue::Bool(b)) = decl_defs.content() {
                    map.insert(name, *b);
                }
            }
        }
    }

    map
}

// ---------------------------------------------------------------------------
// Step 2: walk the AST for `If` nodes
// ---------------------------------------------------------------------------

/// Recursively walk `node`, emitting diagnostics for any `if` whose condition
/// const-folds to a known boolean.
fn walk_for_dead_branches(
    node: &Ast,
    const_vals: &HashMap<String, bool>,
    errors: &mut Vec<AnalysisError>,
) {
    match node.content() {
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            // Attempt to fold the condition.
            if let Some(value) = fold_condition(condition, const_vals) {
                let span = node.span();
                if value {
                    // Condition is always true → else branch is dead.
                    // Only warn when there actually is an else branch.
                    if else_block.is_some() {
                        errors.push(AnalysisError::AlwaysDeadBranch {
                            dead_branch_is_then: false,
                            span,
                        });
                    }
                } else {
                    // Condition is always false → then branch is dead.
                    errors.push(AnalysisError::AlwaysDeadBranch {
                        dead_branch_is_then: true,
                        span,
                    });
                }
            }

            // Always recurse into both branches regardless of whether we emitted
            // a diagnostic — there may be further nested `if` nodes inside.
            walk_for_dead_branches(condition, const_vals, errors);
            walk_for_dead_branches(then_block, const_vals, errors);
            if let Some(eb) = else_block {
                walk_for_dead_branches(eb, const_vals, errors);
            }
        }

        // For all other nodes, recurse into children.
        _ => {
            for child in node.children() {
                walk_for_dead_branches(child, const_vals, errors);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Step 3: const-fold a condition expression
// ---------------------------------------------------------------------------

/// Attempt to evaluate `node` as a constant `bool`.
///
/// Returns `Some(b)` if the entire expression resolves to a known boolean,
/// or `None` if any sub-expression cannot be determined statically.
fn fold_condition(node: &Ast, const_vals: &HashMap<String, bool>) -> Option<bool> {
    match node.content() {
        // Literal boolean.
        AstContent::Value(RuntimeValue::Bool(b)) => Some(*b),

        // Identifier: look up in the const map.
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => {
            const_vals.get(&path[0]).copied()
        }

        // Logical AND.
        AstContent::BinOp {
            op: Operator::And,
            left,
            right,
        } => {
            let l = fold_condition(left, const_vals)?;
            let r = fold_condition(right, const_vals)?;
            Some(l && r)
        }

        // Logical OR.
        AstContent::BinOp {
            op: Operator::Or,
            left,
            right,
        } => {
            let l = fold_condition(left, const_vals)?;
            let r = fold_condition(right, const_vals)?;
            Some(l || r)
        }

        // Logical NOT.
        AstContent::UnaryOp {
            op: UnaryOperator::Not,
            expr,
        } => {
            let v = fold_condition(expr, const_vals)?;
            Some(!v)
        }

        // Anything else (arithmetic, function calls, runtime variables, …) →
        // cannot fold.
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

/// Return `Some(name)` if `node` is `Value(IdentPath([name]))`, else `None`.
fn extract_simple_ident(node: &Ast) -> Option<String> {
    match node.content() {
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => {
            Some(path[0].clone())
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]

    use crate::analysis::AnalysisError;
    use crate::compiler::loader::parse_source;

    use super::check;

    // ── Helpers ──────────────────────────────────────────────────────────────

    fn dead_branch_errors(errors: &[AnalysisError]) -> Vec<&AnalysisError> {
        errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::AlwaysDeadBranch { .. }))
            .collect()
    }

    fn parse(src: &str) -> crate::parser::ast::Ast {
        parse_source(src).unwrap()
    }

    // ── No-error cases ───────────────────────────────────────────────────────

    #[test]
    fn no_error_for_runtime_variable_condition() {
        // The condition is a runtime variable — cannot be const-folded.
        let src = r#"
label scene {
    let flag = true
    if flag {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            dead_branch_errors(&errors).is_empty(),
            "runtime variable condition must not be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn no_error_when_non_const_declaration() {
        // `let` (not `const`) is not collected into the const map.
        let src = r#"
let always_true = true
label scene {
    if always_true {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            dead_branch_errors(&errors).is_empty(),
            "let variable (not const) must not be const-folded, got: {errors:?}"
        );
    }

    #[test]
    fn no_error_for_always_true_without_else() {
        // Condition is always true but there is no else branch — nothing dead.
        let src = r#"
const active = true
label scene {
    if active {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            dead_branch_errors(&errors).is_empty(),
            "always-true without else branch should not produce a diagnostic, got: {errors:?}"
        );
    }

    #[test]
    fn no_error_for_arithmetic_condition() {
        // Only bool consts are folded; numeric expressions yield None.
        let src = r#"
const threshold = 5
label scene {
    let x = 10
    if x {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            dead_branch_errors(&errors).is_empty(),
            "non-bool condition must not be folded, got: {errors:?}"
        );
    }

    // ── Positive cases ───────────────────────────────────────────────────────

    #[test]
    fn detects_literal_false_condition() {
        // `if false { … }` — the then branch is always dead.
        let src = r#"
label scene {
    if false {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert_eq!(
            db.len(),
            1,
            "expected one dead-branch diagnostic, got: {errors:?}"
        );
        match db[0] {
            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => assert!(
                dead_branch_is_then,
                "then-branch should be dead for `if false`, got: {db:?}"
            ),
            _ => panic!("expected AlwaysDeadBranch"),
        }
    }

    #[test]
    fn detects_literal_true_condition_with_else() {
        // `if true { … } else { … }` — the else branch is always dead.
        let src = r#"
label scene {
    if true {
        end!
    } else {
        end!
    }
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert_eq!(
            db.len(),
            1,
            "expected one dead-branch diagnostic, got: {errors:?}"
        );
        match db[0] {
            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => assert!(
                !dead_branch_is_then,
                "else-branch should be dead for `if true`, got: {db:?}"
            ),
            _ => panic!("expected AlwaysDeadBranch"),
        }
    }

    #[test]
    fn detects_const_bool_false_condition() {
        // `const debug = false` at top-level; `if debug { … }` is always dead.
        let src = r#"
const debug = false
label scene {
    if debug {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert_eq!(
            db.len(),
            1,
            "expected one dead-branch diagnostic, got: {errors:?}"
        );
        match db[0] {
            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => assert!(
                dead_branch_is_then,
                "then-branch should be dead when const is false, got: {db:?}"
            ),
            _ => panic!("expected AlwaysDeadBranch"),
        }
    }

    #[test]
    fn detects_const_bool_true_condition_with_else() {
        let src = r#"
const release = true
label scene {
    if release {
        end!
    } else {
        end!
    }
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert_eq!(
            db.len(),
            1,
            "expected one dead-branch diagnostic, got: {errors:?}"
        );
        match db[0] {
            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => assert!(
                !dead_branch_is_then,
                "else-branch should be dead when const is true, got: {db:?}"
            ),
            _ => panic!("expected AlwaysDeadBranch"),
        }
    }

    // ── Compound boolean expressions ──────────────────────────────────────────

    #[test]
    fn detects_and_of_two_true_consts_with_else() {
        let src = r#"
const a = true
const b = true
label scene {
    if a && b {
        end!
    } else {
        end!
    }
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert_eq!(
            db.len(),
            1,
            "expected one dead-branch diagnostic, got: {errors:?}"
        );
        match db[0] {
            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => assert!(!dead_branch_is_then, "else should be dead for true && true"),
            _ => panic!("expected AlwaysDeadBranch"),
        }
    }

    #[test]
    fn detects_not_true_literal_condition() {
        // `if not true { … }` → condition folds to false → then-branch is dead.
        // Note: `!` in Urd is `BitwiseNot`; `not` is the logical NOT keyword.
        let src = r#"
label scene {
    if not true {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert_eq!(
            db.len(),
            1,
            "expected one dead-branch diagnostic, got: {errors:?}"
        );
        match db[0] {
            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => assert!(
                dead_branch_is_then,
                "then-branch should be dead for `if !true`, got: {db:?}"
            ),
            _ => panic!("expected AlwaysDeadBranch"),
        }
    }

    #[test]
    fn no_fold_when_and_operand_is_runtime() {
        // `a` is const true, `x` is a runtime variable — cannot fold the AND.
        let src = r#"
const a = true
label scene {
    let x = false
    if a && x {
        end!
    } else {
        end!
    }
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            dead_branch_errors(&errors).is_empty(),
            "AND with a runtime variable must not be folded, got: {errors:?}"
        );
    }

    #[test]
    fn detects_or_of_false_and_false_consts() {
        // `false || false` → false → then-branch dead.
        let src = r#"
const p = false
const q = false
label scene {
    if p || q {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert_eq!(
            db.len(),
            1,
            "expected one dead-branch diagnostic, got: {errors:?}"
        );
        match db[0] {
            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => assert!(
                dead_branch_is_then,
                "then-branch should be dead for false || false, got: {db:?}"
            ),
            _ => panic!("expected AlwaysDeadBranch"),
        }
    }

    // ── Span is non-zero for parsed source ────────────────────────────────────

    #[test]
    fn diagnostic_has_nonzero_span_for_parsed_source() {
        let src = r#"
label scene {
    if false {
        end!
    }
    end!
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let db = dead_branch_errors(&errors);
        assert!(!db.is_empty(), "expected a dead-branch diagnostic");
        let span = db[0].span();
        assert!(
            span.end > 0,
            "expected a non-zero span for a parsed `if`, got {span:?}"
        );
    }

    // ── Non-bool const is not folded ──────────────────────────────────────────

    #[test]
    fn non_bool_const_not_folded() {
        // `const n = 42` is an integer const, not a bool — the if condition
        // uses `n` which cannot be bool-folded.
        let src = r#"
const n = 42
label scene {
    if n {
        end!
    } else {
        end!
    }
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            dead_branch_errors(&errors).is_empty(),
            "non-bool const must not trigger AlwaysDeadBranch, got: {errors:?}"
        );
    }
}
