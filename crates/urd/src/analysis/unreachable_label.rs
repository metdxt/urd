//! # Unreachable Label Analysis
//!
//! Detects labeled blocks that can never be reached from any `jump` statement,
//! `let-call` (`let name = jump target and return`), or `@entry` decorator.
//!
//! A label that no reachable execution path can ever enter is dead code and is
//! flagged as a [`AnalysisError::UnreachableLabel`] warning.
//!
//! ## Algorithm
//!
//! 1. Walk the entire AST, recording every `LabeledBlock { label, .. }` and its
//!    source span into `defined: HashMap<String, SimpleSpan>`.
//! 2. Walk the entire AST again (in a single `walk_ast` pass) to build
//!    `reachable: HashSet<String>`:
//!    - `Jump { label, .. }` → add `label` to `reachable`.
//!    - `LetCall { target, .. }` → add `target` to `reachable`.
//!    - `LabeledBlock { label, .. }` that carries a decorator named `"entry"`
//!      → add `label` to `reachable` (it is a program entry point).
//! 3. Merge in the caller-supplied `externally_reachable` set — these are
//!    labels that other modules jump to (populated by the import resolver).
//! 4. Any label in `defined` that is **not** in the merged reachable set emits
//!    [`AnalysisError::UnreachableLabel`].
//!
//! ## Exemptions
//!
//! - Qualified names containing `'.'` (cross-module references such as
//!   `inv.show_inventory`) are skipped in every step: they cannot be local
//!   label definitions and must not count as making a local label reachable.
//! - A label decorated with `@entry` is always reachable by definition.
//! - Labels listed in the `externally_reachable` set (typically provided by
//!   an import resolver that knows which labels other modules jump into) are
//!   never flagged.

use std::collections::{HashMap, HashSet};

use chumsky::span::SimpleSpan;

use crate::analysis::AnalysisError;
use crate::parser::ast::{Ast, AstContent, walk_ast};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the unreachable-label check over `ast`.
///
/// Returns one [`AnalysisError::UnreachableLabel`] for every labeled block
/// that no `jump`, `let-call`, or `@entry` can reach.
///
/// Results are ordered by source position (ascending) for deterministic output.
///
/// # Parameters
///
/// * `ast` — the parsed AST of the module under analysis.
/// * `externally_reachable` — a set of label names that are known to be
///   reachable from *other* modules (e.g. via `import`).  Labels present in
///   this set will **not** be flagged as unreachable even if no local `jump`
///   or `@entry` targets them.  Pass an empty set when cross-module
///   information is unavailable.
pub fn check(ast: &Ast, externally_reachable: &HashSet<String>) -> Vec<AnalysisError> {
    // ── Step 1: collect all locally-defined labels and their spans ──────────
    let defined = collect_defined(ast);
    if defined.is_empty() {
        return Vec::new();
    }

    // ── Step 2: collect all labels that can be reached ──────────────────────
    let reachable = collect_reachable(ast);

    // ── Step 3: emit an error for every defined label not in the reachable set
    //            and not in the externally-reachable set ──────────────────────
    let mut errors: Vec<AnalysisError> = defined
        .into_iter()
        .filter(|(name, _span)| {
            !reachable.contains(name.as_str()) && !externally_reachable.contains(name.as_str())
        })
        .map(|(label, span)| AnalysisError::UnreachableLabel { label, span })
        .collect();

    // Sort by source position so that diagnostics are deterministic regardless
    // of HashMap iteration order.
    errors.sort_by_key(|e| e.span().start);

    errors
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/// Walk `ast` and return every locally-defined label name mapped to the source
/// span of its `LabeledBlock` node.
///
/// Qualified names (containing `'.'`) are excluded — they are cross-module
/// references and cannot be defined in the current file.
fn collect_defined(ast: &Ast) -> HashMap<String, SimpleSpan> {
    let mut defined: HashMap<String, SimpleSpan> = HashMap::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::LabeledBlock { label, .. } = node.content() {
            // Skip cross-module qualified labels; they are never locally defined.
            if !label.contains('.') {
                // Use `entry` to preserve the span of the *first* occurrence if
                // the same name somehow appears twice (duplicate-label detection
                // is a separate pass).
                defined.entry(label.clone()).or_insert_with(|| node.span());
            }
        }
    });

    defined
}

/// Walk `ast` and return the set of label names that are reachable via at least
/// one `jump`, `let-call`, or `@entry` decorator.
///
/// Qualified names (containing `'.'`) are **not** added to the reachable set
/// because they refer to labels in other modules, not local ones.
fn collect_reachable(ast: &Ast) -> HashSet<String> {
    let mut reachable: HashSet<String> = HashSet::new();

    walk_ast(ast, &mut |node| {
        match node.content() {
            // A `jump target` makes `target` reachable.
            AstContent::Jump { label, .. } if !label.contains('.') => {
                reachable.insert(label.clone());
            }

            // `let name = jump target and return` makes `target` reachable.
            AstContent::LetCall { target, .. } if !target.contains('.') => {
                reachable.insert(target.clone());
            }

            // A labeled block decorated with `@entry` is a root entry point
            // and is therefore reachable by definition.
            AstContent::LabeledBlock { label, .. }
                if !label.contains('.')
                    && node.decorators().iter().any(|d| d.name() == "entry") =>
            {
                reachable.insert(label.clone());
            }

            _ => {}
        }
    });

    reachable
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    /// Helper: run `check` with an empty externally-reachable set (the common
    /// case for single-module analysis).
    fn check_no_external(ast: &Ast) -> Vec<AnalysisError> {
        check(ast, &HashSet::new())
    }

    // -----------------------------------------------------------------------
    // No-error cases
    // -----------------------------------------------------------------------

    #[test]
    fn empty_script_produces_no_errors() {
        let ast = parse("");
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn single_entry_label_is_not_unreachable() {
        // A label decorated with @entry is always reachable.
        let ast = parse(
            r#"
@entry
label main {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn all_labels_reachable_via_jumps_produces_no_errors() {
        let ast = parse(
            r#"
@entry
label start {
  jump middle
}
label middle {
  jump finish
}
label finish {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn label_reachable_via_let_call_produces_no_error() {
        let ast = parse(
            r#"
@entry
label main {
  let result = jump helper and return
  end!()
}
label helper {
  return
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn multiple_labels_all_reachable_via_jumps_no_error() {
        let ast = parse(
            r#"
@entry
label hub {
  menu {
    "Path A" {
      jump path_a
    }
    "Path B" {
      jump path_b
    }
  }
}
label path_a {
  end!()
}
label path_b {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn label_reachable_via_jump_inside_if_no_error() {
        let ast = parse(
            r#"
@entry
label start {
  if true {
    jump target
  } else {
    end!()
  }
  end!()
}
label target {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // -----------------------------------------------------------------------
    // Positive (error) cases
    // -----------------------------------------------------------------------

    #[test]
    fn single_label_with_no_jump_and_no_entry_is_unreachable() {
        // The script has one label, but no @entry and no jump points to it.
        let ast = parse(
            r#"
label orphan {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::UnreachableLabel { label, .. } => {
                assert_eq!(label, "orphan");
            }
            other => panic!("expected UnreachableLabel, got: {other:?}"),
        }
    }

    #[test]
    fn label_with_no_incoming_jump_is_unreachable() {
        // `start` is an @entry, but `dead` is never jumped to.
        let ast = parse(
            r#"
@entry
label start {
  end!()
}
label dead {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::UnreachableLabel { label, .. } => {
                assert_eq!(label, "dead");
            }
            other => panic!("expected UnreachableLabel, got: {other:?}"),
        }
    }

    #[test]
    fn multiple_unreachable_labels_all_reported() {
        let ast = parse(
            r#"
@entry
label start {
  end!()
}
label ghost_a {
  end!()
}
label ghost_b {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");

        let labels: Vec<&str> = errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::UnreachableLabel { label, .. } => Some(label.as_str()),
                _ => None,
            })
            .collect();
        assert!(
            labels.contains(&"ghost_a"),
            "expected 'ghost_a' in errors, got: {labels:?}"
        );
        assert!(
            labels.contains(&"ghost_b"),
            "expected 'ghost_b' in errors, got: {labels:?}"
        );
    }

    #[test]
    fn script_with_no_entry_and_no_jumps_flags_all_labels() {
        let ast = parse(
            r#"
label alpha {
  end!()
}
label beta {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");

        let labels: Vec<&str> = errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::UnreachableLabel { label, .. } => Some(label.as_str()),
                _ => None,
            })
            .collect();
        assert!(labels.contains(&"alpha"));
        assert!(labels.contains(&"beta"));
    }

    #[test]
    fn chain_with_one_missing_link_flags_the_tail() {
        // `start` is @entry, jumps to `middle`, but `tail` has no incoming jump.
        let ast = parse(
            r#"
@entry
label start {
  jump middle
}
label middle {
  end!()
}
label tail {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::UnreachableLabel { label, .. } => {
                assert_eq!(label, "tail");
            }
            other => panic!("expected UnreachableLabel for 'tail', got: {other:?}"),
        }
    }

    #[test]
    fn label_reachable_only_via_let_call_is_not_flagged() {
        // Ensures `LetCall` targets count as reachable.
        let ast = parse(
            r#"
@entry
label main {
  let r = jump sub and return
  end!()
}
label sub {
  return
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // -----------------------------------------------------------------------
    // Edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn cross_module_jump_does_not_make_local_label_reachable() {
        // `jump other.start` is a cross-module reference; it must NOT count as
        // making the local label named `start` reachable.
        let ast = parse(
            r#"
@entry
label main {
  jump other.start
}
label start {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        // `start` should still be flagged as unreachable — the qualified jump
        // does not satisfy it.
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::UnreachableLabel { label, .. } => {
                assert_eq!(label, "start");
            }
            other => panic!("expected UnreachableLabel for 'start', got: {other:?}"),
        }
    }

    #[test]
    fn cross_module_let_call_does_not_make_local_label_reachable() {
        // Same cross-module exemption but for `let-call`.
        let ast = parse(
            r#"
@entry
label main {
  let r = jump other.worker and return
  end!()
}
label worker {
  return
}
"#,
        );
        let errors = check_no_external(&ast);
        // `worker` should be unreachable — `jump other.worker` is cross-module.
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::UnreachableLabel { label, .. } => {
                assert_eq!(label, "worker");
            }
            other => panic!("expected UnreachableLabel for 'worker', got: {other:?}"),
        }
    }

    #[test]
    fn entry_label_itself_is_never_reported_as_unreachable() {
        // Even when nothing jumps to the @entry label, it must not be reported.
        let ast = parse(
            r#"
@entry
label main {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(
            errors.iter().all(|e| !matches!(
                e,
                AnalysisError::UnreachableLabel { label, .. } if label == "main"
            )),
            "the @entry label should never be flagged as unreachable, got: {errors:?}"
        );
    }

    #[test]
    fn two_entry_labels_neither_flagged() {
        // Even with duplicate @entry (a separate error), neither label should
        // be flagged as unreachable by this pass.
        let ast = parse(
            r#"
@entry
label first {
  end!()
}
@entry
label second {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        // This pass emits UnreachableLabel only; neither label is unreachable.
        assert!(
            errors.is_empty(),
            "expected no UnreachableLabel errors, got: {errors:?}"
        );
    }

    #[test]
    fn jump_inside_menu_option_counts_as_reachable() {
        let ast = parse(
            r#"
@entry
label start {
  menu {
    "Option A" {
      jump destination
    }
    "Option B" {
      end!()
    }
  }
}
label destination {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn results_ordered_by_source_position() {
        // Verify that errors come out in source order, not HashMap order.
        let ast = parse(
            r#"
label first_dead {
  end!()
}
label second_dead {
  end!()
}
label third_dead {
  end!()
}
"#,
        );
        let errors = check_no_external(&ast);
        assert_eq!(errors.len(), 3);

        let labels: Vec<&str> = errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::UnreachableLabel { label, .. } => Some(label.as_str()),
                _ => None,
            })
            .collect();

        assert_eq!(
            labels,
            vec!["first_dead", "second_dead", "third_dead"],
            "errors should be in source order"
        );
    }

    // -----------------------------------------------------------------------
    // Externally-reachable labels
    // -----------------------------------------------------------------------

    #[test]
    fn externally_reachable_label_is_not_flagged() {
        // `helper` has no local jump or @entry targeting it, but it is listed
        // in the externally-reachable set (simulating another module that
        // imports and jumps to it).
        let ast = parse(
            r#"
@entry
label main {
  end!()
}
label helper {
  end!()
}
"#,
        );
        let mut ext = HashSet::new();
        ext.insert("helper".to_owned());

        let errors = check(&ast, &ext);
        assert!(
            errors.is_empty(),
            "expected no errors when 'helper' is externally reachable, got: {errors:?}"
        );
    }

    #[test]
    fn externally_reachable_does_not_suppress_other_unreachable_labels() {
        // `helper` is externally reachable, but `orphan` is not — it should
        // still be flagged.
        let ast = parse(
            r#"
@entry
label main {
  end!()
}
label helper {
  end!()
}
label orphan {
  end!()
}
"#,
        );
        let mut ext = HashSet::new();
        ext.insert("helper".to_owned());

        let errors = check(&ast, &ext);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::UnreachableLabel { label, .. } => {
                assert_eq!(label, "orphan");
            }
            other => panic!("expected UnreachableLabel for 'orphan', got: {other:?}"),
        }
    }

    #[test]
    fn externally_reachable_label_not_defined_locally_is_harmless() {
        // The externally-reachable set mentions a label that doesn't even
        // exist in this file.  That must not cause any crash or spurious error.
        let ast = parse(
            r#"
@entry
label main {
  end!()
}
"#,
        );
        let mut ext = HashSet::new();
        ext.insert("nonexistent".to_owned());

        let errors = check(&ast, &ext);
        assert!(
            errors.is_empty(),
            "expected no errors, got: {errors:?}"
        );
    }

    #[test]
    fn all_labels_externally_reachable_produces_no_errors() {
        // Every defined label is in the externally-reachable set — none should
        // be flagged.
        let ast = parse(
            r#"
label alpha {
  end!()
}
label beta {
  end!()
}
"#,
        );
        let mut ext = HashSet::new();
        ext.insert("alpha".to_owned());
        ext.insert("beta".to_owned());

        let errors = check(&ast, &ext);
        assert!(
            errors.is_empty(),
            "expected no errors when all labels are externally reachable, got: {errors:?}"
        );
    }

    #[test]
    fn externally_reachable_combined_with_local_jump() {
        // `helper` is externally reachable, `target` is locally jumped to,
        // `dead` is neither — only `dead` should be flagged.
        let ast = parse(
            r#"
@entry
label main {
  jump target
}
label target {
  end!()
}
label helper {
  end!()
}
label dead {
  end!()
}
"#,
        );
        let mut ext = HashSet::new();
        ext.insert("helper".to_owned());

        let errors = check(&ast, &ext);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        match &errors[0] {
            AnalysisError::UnreachableLabel { label, .. } => {
                assert_eq!(label, "dead");
            }
            other => panic!("expected UnreachableLabel for 'dead', got: {other:?}"),
        }
    }
}
