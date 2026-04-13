//! # Unused Variable Pass
//!
//! Warns when a `let` or `const` variable is declared inside a labeled block
//! but never subsequently read anywhere within that same label scope.
//!
//! ## Scope
//!
//! The pass operates per labeled block and does **not** cross into nested
//! labeled blocks, which are independent jump targets with their own scope.
//!
//! ## Algorithm
//!
//! Within each [`AstContent::LabeledBlock`], the pass:
//!
//! 1. Walks statements in order.
//! 2. On `Declaration { kind: Variable | Constant, decl_name: IdentPath([name]) }`:
//!    adds `name` to the "declared, not yet read" set with its span.
//! 3. Whenever `Value(IdentPath([name]))` appears in a *non-LHS* expression
//!    position, marks `name` as read (removes from the set).
//! 4. String interpolations (`{name}` inside string literals) count as reads.
//! 5. At the end of the block, emits [`AnalysisError::UnusedVariable`] for
//!    every name still in the "not yet read" set.
//!
//! ## Exclusions
//!
//! - `global` declarations (may be read cross-file).
//! - `@fluent`-decorated variables (consumed by the localizer at runtime, not by urd code).
//! - `LetCall` targets (`let x = jump label and return`) — the jump is the effect.
use std::collections::HashMap;

use chumsky::span::SimpleSpan;

use crate::lexer::strings::StringPart;
use crate::parser::ast::{Ast, AstContent, DeclKind, Operator};
use crate::runtime::value::RuntimeValue;

use super::AnalysisError;
use super::context::extract_decl_name;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the unused-variable pass over `ast` and return any diagnostics.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors = Vec::new();
    find_labeled_blocks(ast, &mut errors);
    errors
}

// ---------------------------------------------------------------------------
// Labeled-block discovery
// ---------------------------------------------------------------------------

/// Recursively find every `LabeledBlock` in the tree and analyse each one.
///
/// We recurse through the top-level `Block` and into nested structures, but
/// we do NOT recurse into a `LabeledBlock`'s *contents* from within another
/// `LabeledBlock` — each label scope is analysed independently when it is
/// discovered at this level.
fn find_labeled_blocks(node: &Ast, errors: &mut Vec<AnalysisError>) {
    match node.content() {
        AstContent::LabeledBlock { block, .. } => {
            // Analyse this block.
            check_block(block, errors);
            // Then descend into the block to find any *nested* LabeledBlocks.
            find_nested_labels(block, errors);
        }

        AstContent::Block(stmts) => {
            for stmt in stmts {
                find_labeled_blocks(stmt, errors);
            }
        }

        _ => {
            for child in node.children() {
                find_labeled_blocks(child, errors);
            }
        }
    }
}

/// Descend into a block's statements to discover nested `LabeledBlock` nodes,
/// without re-analysing the current block's own declarations.
fn find_nested_labels(node: &Ast, errors: &mut Vec<AnalysisError>) {
    match node.content() {
        AstContent::LabeledBlock { block, .. } => {
            // This is a nested label — analyse it as a fresh scope.
            check_block(block, errors);
            find_nested_labels(block, errors);
        }

        AstContent::Block(stmts) => {
            for stmt in stmts {
                find_nested_labels(stmt, errors);
            }
        }

        _ => {
            for child in node.children() {
                find_nested_labels(child, errors);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Per-block analysis
// ---------------------------------------------------------------------------

/// Analyse a single block (the *contents* of a labeled block) for unused variables.
///
/// `block` should be an `AstContent::Block` node.
fn check_block(block: &Ast, errors: &mut Vec<AnalysisError>) {
    // Map: variable name → declaration span.
    let mut unread: HashMap<String, SimpleSpan> = HashMap::new();

    collect_in_block(block, &mut unread, errors);

    // Emit diagnostics for everything still unread.
    let mut unread_vec: Vec<(String, SimpleSpan)> = unread.into_iter().collect();
    // Sort for deterministic output (by span start, then name).
    unread_vec.sort_by(|a, b| a.1.start.cmp(&b.1.start).then(a.0.cmp(&b.0)));

    for (name, span) in unread_vec {
        errors.push(AnalysisError::UnusedVariable { name, span });
    }
}

// ---------------------------------------------------------------------------
// Traversal within a single labeled-block scope
// ---------------------------------------------------------------------------

/// Walk `node`, updating the `unread` map as declarations and reads are found.
///
/// Does NOT recurse into nested `LabeledBlock` nodes — those are separate scopes.
fn collect_in_block(node: &Ast, unread: &mut HashMap<String, SimpleSpan>, errors: &mut Vec<AnalysisError>) {
    match node.content() {
        // ── Nested labeled blocks are independent — skip ─────────────────
        AstContent::LabeledBlock { .. } => {}

        // ── Block: process statements in order ───────────────────────────
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_in_block(stmt, unread, errors);
            }
        }

        // ── LetCall: `let x = jump label and return` — exempt ────────────
        AstContent::LetCall { .. } => {}

        // ── Extern declaration: provided by runtime, not tracked ─────────────────
        AstContent::ExternDeclaration { .. } => {}

        // ── Declaration ──────────────────────────────────────────────────
        AstContent::Declaration {
            kind,
            decl_name,
            decl_defs,
            ..
        } => {
            match kind {
                // Global variables may be read cross-file; skip.
                DeclKind::Global => {
                    // Still scan the initialiser for reads.
                    scan_reads(decl_defs, unread);
                }

                DeclKind::Variable | DeclKind::Constant | DeclKind::Assignment => {
                    // Scan the initialiser for reads first (in case it reads a
                    // previously declared variable).
                    scan_reads(decl_defs, unread);

                    // @fluent-decorated variables are consumed by the localizer
                    // at runtime — they should not be flagged as unused.
                    let is_fluent = node.decorators().iter().any(|d| d.name() == "fluent");

                    // Then register the declaration.
                    if !is_fluent && let Some(name) = extract_decl_name(decl_name) {
                        // If the same name was already in `unread` (re-declaration
                        // / shadowing), the old entry was never read — emit an
                        // UnusedVariable warning for the previous declaration
                        // before replacing it.
                        if let Some(old_span) = unread.get(&name) {
                            errors.push(AnalysisError::UnusedVariable {
                                name: name.to_string(),
                                span: *old_span,
                            });
                        }
                        unread.insert(name, node.span());
                    }
                }
            }
        }

        // ── Assignment BinOp: the LHS is a write, not a read ────────────
        AstContent::BinOp {
            op: Operator::Assign,
            left,
            right,
        } => {
            // Only scan the RHS for reads; LHS is a write target.
            // (For subscript-assign style `a[i] = v` the full node is
            // SubscriptAssign which is handled below.)
            scan_reads(right, unread);
            // If LHS is not a simple ident (e.g. complex expression), scan it too.
            if extract_decl_name(left).is_none() {
                scan_reads(left, unread);
            }
        }

        // ── SubscriptAssign: obj[key] = value ────────────────────────────
        AstContent::SubscriptAssign { object, key, value } => {
            scan_reads(object, unread);
            scan_reads(key, unread);
            scan_reads(value, unread);
        }

        // ── If / else ─────────────────────────────────────────────────────
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            scan_reads(condition, unread);
            collect_in_block(then_block, unread, errors);
            if let Some(eb) = else_block {
                collect_in_block(eb, unread, errors);
            }
        }

        // ── Match ─────────────────────────────────────────────────────────
        AstContent::Match { scrutinee, arms } => {
            scan_reads(scrutinee, unread);
            for arm in arms {
                // Pattern values can be reads (enum variants are idents).
                if let crate::parser::ast::MatchPattern::Value(pv) = &arm.pattern {
                    scan_reads(pv, unread);
                }
                collect_in_block(&arm.body, unread, errors);
            }
        }

        // ── Menu ──────────────────────────────────────────────────────────
        AstContent::Menu { options } => {
            for opt in options {
                collect_in_block(opt, unread, errors);
            }
        }

        AstContent::MenuOption { content, .. } => {
            collect_in_block(content, unread, errors);
        }

        // ── All other nodes: scan for reads ───────────────────────────────
        _ => {
            scan_reads(node, unread);
        }
    }
}

// ---------------------------------------------------------------------------
// Read-scanning helper
// ---------------------------------------------------------------------------

/// Walk `node` and remove from `unread` every variable that is *read*.
///
/// A "read" is any `Value(IdentPath([name]))` appearing in an expression
/// position, plus variable names referenced inside string interpolations.
///
/// Does NOT recurse into nested `LabeledBlock` nodes.
/// Does NOT treat the LHS of an `Assign` BinOp as a read (simple ident case).
fn scan_reads(node: &Ast, unread: &mut HashMap<String, SimpleSpan>) {
    match node.content() {
        // Plain identifier read.
        AstContent::Value(RuntimeValue::IdentPath(path)) if !path.is_empty() => {
            // The root segment is the local variable.
            unread.remove(&path[0]);
        }

        // String literal — check interpolations.
        AstContent::Value(RuntimeValue::Str(parsed_str)) => {
            for part in parsed_str.parts() {
                if let StringPart::Interpolation(interp) = part {
                    let root = interp.path.split('.').next().unwrap_or("");
                    if !root.is_empty() {
                        unread.remove(root);
                    }
                }
            }
        }

        // Other value types carry no variable reads.
        AstContent::Value(_) => {}

        // Nested labeled blocks are independent — don't look inside.
        AstContent::LabeledBlock { .. } => {}

        // LetCall carries no reads of local variables.
        AstContent::LetCall { .. } => {}

        // For an assignment BinOp, only the RHS contains reads; the LHS is a
        // write target. (The non-simple-ident LHS case is handled in
        // collect_in_block above.)
        AstContent::BinOp {
            op: Operator::Assign,
            left,
            right,
        } => {
            if extract_decl_name(left).is_none() {
                scan_reads(left, unread);
            }
            scan_reads(right, unread);
        }

        // For all other nodes, recurse into children.
        _ => {
            for child in node.children() {
                scan_reads(child, unread);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::analysis::AnalysisError;
    use crate::compiler::loader::parse_source;

    use super::check;

    // ── Helpers ──────────────────────────────────────────────────────────────

    fn unused_names(errors: &[AnalysisError]) -> Vec<&str> {
        errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::UnusedVariable { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect()
    }

    fn parse(src: &str) -> crate::parser::ast::Ast {
        parse_source(src).unwrap()
    }

    // ── No-error cases ───────────────────────────────────────────────────────

    #[test]
    fn no_error_when_variable_is_read() {
        let src = r#"
label scene {
    let hp = 100
    if hp {
        end!()
    }
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "expected no unused-variable errors, got: {errors:?}"
        );
    }

    #[test]
    fn no_error_for_global_variable() {
        let src = r#"
global score = 0
label scene {
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "global variables should not be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn no_error_when_variable_used_in_string_interpolation() {
        let src = r#"
label scene {
    let name = "Alice"
    narrator: "Hello, {name}!"
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "string interpolation should count as a read, got: {errors:?}"
        );
    }

    #[test]
    fn no_error_when_const_is_read_in_condition() {
        let src = r#"
label scene {
    const debug = false
    if debug {
        end!()
    }
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "const read in condition should not be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn no_error_when_variable_used_in_assignment_rhs() {
        let src = r#"
label scene {
    let x = 5
    let y = x + 1
    if y {
        end!()
    }
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "x used in rhs should not be flagged, got: {errors:?}"
        );
    }

    // ── Positive cases ───────────────────────────────────────────────────────

    #[test]
    fn detects_unused_let_variable() {
        let src = r#"
label scene {
    let hp = 100
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let names = unused_names(&errors);
        assert_eq!(
            names,
            vec!["hp"],
            "expected 'hp' to be unused, got: {errors:?}"
        );
    }

    #[test]
    fn detects_unused_const_variable() {
        let src = r#"
label scene {
    const max_hp = 200
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let names = unused_names(&errors);
        assert_eq!(
            names,
            vec!["max_hp"],
            "expected 'max_hp' to be unused, got: {errors:?}"
        );
    }

    #[test]
    fn detects_multiple_unused_variables() {
        let src = r#"
label scene {
    let a = 1
    let b = 2
    let c = 3
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let mut names = unused_names(&errors);
        names.sort_unstable();
        assert_eq!(
            names,
            vec!["a", "b", "c"],
            "expected a, b, c unused, got: {errors:?}"
        );
    }

    #[test]
    fn detects_variable_unused_after_assignment() {
        // `x` is assigned but never read.
        let src = r#"
label scene {
    let x = 0
    x = 5
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let names = unused_names(&errors);
        assert!(
            names.contains(&"x"),
            "x assigned but never read should be flagged, got: {errors:?}"
        );
    }

    // ── LetCall exclusion ─────────────────────────────────────────────────────

    #[test]
    fn let_call_target_not_flagged() {
        let src = r#"
label scene {
    let result = jump helper and return
    end!()
}
label helper {
    return
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "LetCall target should not be flagged as unused, got: {errors:?}"
        );
    }

    // ── Read in nested branch counts ─────────────────────────────────────────

    #[test]
    fn variable_read_inside_else_branch_not_flagged() {
        let src = r#"
label scene {
    let flag = true
    if flag {
        end!()
    } else {
        end!()
    }
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "flag read in if condition should clear it, got: {errors:?}"
        );
    }

    #[test]
    fn variable_read_only_in_one_match_arm_not_flagged() {
        let src = r#"
enum Dir { North, South }
label scene {
    let d = Dir.North
    let msg = "going north"
    match d {
        Dir.North {
            narrator: "{msg}"
            end!()
        }
        Dir.South { end!() }
    }
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        // `d` is the scrutinee (read), `msg` is read in the North arm.
        let names = unused_names(&errors);
        assert!(
            !names.contains(&"d"),
            "d is the match scrutinee — should not be unused, got: {errors:?}"
        );
        assert!(
            !names.contains(&"msg"),
            "msg is read in one arm — should not be unused, got: {errors:?}"
        );
    }

    // ── Nested labeled blocks are independent scopes ──────────────────────────

    #[test]
    fn inner_label_unused_variable_reported_independently() {
        let src = r#"
label outer {
    let x = 1
    if x {
        end!()
    }
    end!()
}
label inner {
    let y = 99
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let names = unused_names(&errors);
        assert!(
            !names.contains(&"x"),
            "x is used in outer — should not be flagged, got: {errors:?}"
        );
        assert!(
            names.contains(&"y"),
            "y in inner is unused — should be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn fluent_decorated_variable_not_flagged() {
        let src = r#"
label scene {
    @fluent("item")
    let item = "health_potion"
    narrator: "Hello"
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "@fluent variable should not be flagged as unused, got: {errors:?}"
        );
    }

    #[test]
    fn fluent_bare_decorated_variable_not_flagged() {
        let src = r#"
label scene {
    @fluent
    let gold = 50
    narrator: "Hello"
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        assert!(
            unused_names(&errors).is_empty(),
            "@fluent (bare) variable should not be flagged as unused, got: {errors:?}"
        );
    }

    #[test]
    fn non_fluent_unused_variable_still_flagged() {
        let src = r#"
label scene {
    @fluent("item")
    let item = "health_potion"
    let unused = 42
    narrator: "Hello"
}
"#;
        let errors = check(&parse(src));
        let names = unused_names(&errors);
        assert_eq!(
            names,
            vec!["unused"],
            "only the non-@fluent variable should be flagged"
        );
    }

    #[test]
    fn outer_variable_not_flagged_by_inner_read() {
        // `x` declared in outer, only referenced inside a nested labeled block.
        // Since nested labeled blocks are independent scopes, `x` from outer
        // is NOT marked as read by the inner block's reference — and should
        // still be flagged as unused in the outer scope.
        let src = r#"
label outer {
    let x = 42
    label inner {
        if x {
            end!()
        }
        end!()
    }
    end!()
}
"#;
        let ast = parse(src);
        let errors = check(&ast);
        let names = unused_names(&errors);
        // The inner block's read of `x` does NOT count toward outer's tracking.
        assert!(
            names.contains(&"x"),
            "x in outer is never read within outer — should be flagged, got: {errors:?}"
        );
    }
}
