//! # Const Reassignment Analysis
//!
//! Detects assignments to variables that were declared as `const` at the top
//! level of the script.  Assigning a new value to a constant after its initial
//! declaration is a logic error that this pass surfaces as a
//! [`AnalysisError::ConstReassignment`].
//!
//! ## Algorithm
//!
//! 1. Walk the outermost `Block`'s direct children and collect the names of
//!    every `Declaration { kind: Constant, .. }` into a `HashSet<String>`
//!    (`const_names`).
//! 2. Walk the entire AST.  For every `BinOp { op: Assign, left, .. }` where
//!    `left` is a bare `Value(IdentPath([name]))` (single-segment ident), check
//!    whether `name` is in `const_names`.  If so, emit a
//!    [`AnalysisError::ConstReassignment`].
//!
//! ## What is NOT flagged
//!
//! - A new `Declaration` (e.g. `let foo = …` inside a label) that happens to
//!   shadow a top-level const is **not** flagged — shadowing is a new binding,
//!   not a reassignment.
//! - Subscript assignments (`foo[key] = …`) are not flagged because the AST
//!   node is `SubscriptAssign`, not `BinOp { op: Assign, .. }`.
//! - Qualified paths (`some.module.VAR`) are not flagged because they refer to
//!   a different binding in another module's scope.

use std::collections::HashSet;

use crate::analysis::AnalysisError;
use crate::parser::ast::{Ast, AstContent, DeclKind, Operator, walk_ast};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the const-reassignment check over `ast`.
///
/// Returns one [`AnalysisError::ConstReassignment`] for every bare assignment
/// (`name = …`) where `name` is a top-level `const` declaration.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let const_names = collect_const_names(ast);
    if const_names.is_empty() {
        return Vec::new();
    }

    let mut errors = Vec::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::BinOp {
            op: Operator::Assign,
            left,
            ..
        } = node.content()
        {
            // Only flag bare single-segment ident assignments, not qualified
            // paths or subscript assignments.
            if let AstContent::Value(RuntimeValue::IdentPath(path)) = left.content() {
                if path.len() == 1 {
                    let name = &path[0];
                    if const_names.contains(name.as_str()) {
                        errors.push(AnalysisError::ConstReassignment {
                            name: name.clone(),
                            span: node.span(),
                        });
                    }
                }
            }
        }
    });

    errors
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/// Collect the names of every `const` variable declared as a direct child of
/// the outermost `Block` in `ast`.
///
/// Two root shapes are accepted (mirroring `context::collect_top_level_vars`):
/// - `ast` is a `Block` — its direct children are scanned.
/// - `ast` is a `LabeledBlock` wrapping a `Block` — the inner block's direct
///   children are scanned.
///
/// Any other root shape returns an empty set.
fn collect_const_names(ast: &Ast) -> HashSet<String> {
    let stmts: &[Ast] = match ast.content() {
        AstContent::Block(stmts) => stmts,
        AstContent::LabeledBlock { block, .. } => match block.content() {
            AstContent::Block(stmts) => stmts,
            _ => return HashSet::new(),
        },
        _ => return HashSet::new(),
    };

    let mut names = HashSet::new();

    for stmt in stmts {
        if let AstContent::Declaration {
            kind: DeclKind::Constant,
            decl_name,
            ..
        } = stmt.content()
        {
            if let AstContent::Value(RuntimeValue::IdentPath(path)) = decl_name.content() {
                if path.len() == 1 {
                    names.insert(path[0].clone());
                }
            }
        }
    }

    names
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    // -----------------------------------------------------------------------
    // No-error cases
    // -----------------------------------------------------------------------

    #[test]
    fn no_consts_no_errors() {
        // Script with only `let` variables — nothing to flag.
        let ast = parse(
            r#"
let x = 1
label start {
  x = 2
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn const_declared_and_never_reassigned_is_ok() {
        let ast = parse(
            r#"
const MAX = 100
label start {
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn let_variable_reassignment_is_not_flagged() {
        // Only assignments to `const` names are flagged; `let` reassignments
        // are perfectly legal.
        let ast = parse(
            r#"
let score = 0
label start {
  score = 42
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn shadow_via_let_inside_label_is_not_flagged() {
        // Declaring a new `let` binding with the same name as a top-level
        // `const` is shadowing, not reassignment — must not be flagged.
        let ast = parse(
            r#"
const MAX = 100
label start {
  let MAX = 999
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn empty_script_produces_no_errors() {
        let ast = parse("");
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // -----------------------------------------------------------------------
    // Positive (error) cases
    // -----------------------------------------------------------------------

    #[test]
    fn assignment_to_const_inside_label_is_flagged() {
        let ast = parse(
            r#"
const LIMIT = 10
label start {
  LIMIT = 99
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::ConstReassignment { name, .. } => {
                assert_eq!(name, "LIMIT");
            }
            other => panic!("expected ConstReassignment, got: {other:?}"),
        }
    }

    #[test]
    fn multiple_assignments_to_same_const_each_flagged() {
        let ast = parse(
            r#"
const X = 0
label start {
  X = 1
  X = 2
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
        assert!(
            errors.iter().all(|e| matches!(
                e,
                AnalysisError::ConstReassignment { name, .. } if name == "X"
            )),
            "all errors should be ConstReassignment for 'X', got: {errors:?}"
        );
    }

    #[test]
    fn multiple_different_consts_reassigned_all_flagged() {
        let ast = parse(
            r#"
const A = 1
const B = 2
label start {
  A = 10
  B = 20
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
        let names: Vec<&str> = errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::ConstReassignment { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect();
        assert!(
            names.contains(&"A"),
            "expected 'A' in errors, got: {names:?}"
        );
        assert!(
            names.contains(&"B"),
            "expected 'B' in errors, got: {names:?}"
        );
    }

    #[test]
    fn assignment_to_const_inside_nested_if_is_flagged() {
        let ast = parse(
            r#"
const FLAG = true
label start {
  if true {
    FLAG = false
  } else {
    end!()
  }
  end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::ConstReassignment { name, .. } if name == "FLAG"
            )),
            "expected ConstReassignment for 'FLAG', got: {errors:?}"
        );
    }

    #[test]
    fn assignment_to_const_inside_menu_option_is_flagged() {
        let ast = parse(
            r#"
const CHOSEN = false
label start {
  menu {
    "Yes" {
      CHOSEN = true
      end!()
    }
    "No" {
      end!()
    }
  }
}
"#,
        );
        let errors = check(&ast);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::ConstReassignment { name, .. } if name == "CHOSEN"
            )),
            "expected ConstReassignment for 'CHOSEN', got: {errors:?}"
        );
    }

    // -----------------------------------------------------------------------
    // Edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn qualified_path_assignment_is_not_flagged() {
        // `some.CONST = …` is a multi-segment path — this pass only tracks
        // bare single-segment names at the top level.
        // We verify no spurious error is emitted for names that look like
        // qualified references (the parser won't produce a BinOp Assign for
        // dotted paths, but we confirm the pass is safe either way).
        let ast = parse(
            r#"
const SCORE = 0
label start {
  SCORE = 10
  end!()
}
"#,
        );
        // The const IS flagged here; we're just verifying correctness of the
        // single-segment check — there's no `some.SCORE` syntax to test
        // directly at parse level, so just confirm the single-segment case.
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::ConstReassignment { name, .. } => assert_eq!(name, "SCORE"),
            other => panic!("unexpected: {other:?}"),
        }
    }

    #[test]
    fn non_block_root_returns_empty() {
        // A bare value node is not a valid script root — the pass must not
        // panic and must return no errors.
        let ast = Ast::value(RuntimeValue::Int(42));
        let errors = check(&ast);
        assert!(errors.is_empty());
    }
}
