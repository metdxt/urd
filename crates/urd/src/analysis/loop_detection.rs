//! # Infinite Dialogue Loop Detection (Hybrid AST + IR)
//!
//! This pass is **opt-in** and emits [`AnalysisError::InfiniteDialogueLoop`]
//! only for labels decorated with `@lint(check_loops)`.
//!
//! Detection is hybrid:
//!
//! 1. **AST phase**: collect opt-in labels + spans from decorators.
//! 2. **IR phase**: compile to [`crate::ir::IrGraph`], project label-to-label
//!    one-way jump graph, run SCC using petgraph, then perform escape analysis
//!    on IR control flow for each cyclic SCC.
//!
//! A diagnostic is emitted for each opted-in label that belongs to a cyclic SCC
//! where no path can escape to:
//! - a terminator (`return`, `end!`, `todo!`),
//! - or a label outside that SCC.

use std::collections::{HashMap, HashSet, VecDeque};

use chumsky::span::SimpleSpan;
use petgraph::algo::kosaraju_scc;
use petgraph::stable_graph::{NodeIndex, StableDiGraph};
use petgraph::visit::EdgeRef;

use crate::compiler::Compiler;
use crate::ir::analysis::{compute_clusters, follow_nops, reachable_nodes};
use crate::ir::{IrEdge, IrNodeKind};
use crate::parser::ast::{Ast, AstContent, Decorator};
use crate::runtime::value::RuntimeValue;

use super::AnalysisError;

/// Run loop detection.
///
/// This pass is intentionally best-effort:
/// - If IR compilation fails, it returns no diagnostics (other passes will
///   already report the underlying issues).
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let opt_in = collect_opt_in_labels(ast);
    if opt_in.is_empty() {
        return Vec::new();
    }

    let ir = match Compiler::compile(ast) {
        Ok(g) => g,
        Err(_) => return Vec::new(),
    };

    let reachable = reachable_nodes(&ir);
    let clusters = compute_clusters(&ir, &reachable);
    if clusters.is_empty() {
        return Vec::new();
    }

    let entry_to_label = build_entry_to_label(&ir);
    let node_to_label = build_node_to_label(&clusters);

    let jump_graph = build_label_jump_graph(&ir, &clusters, &entry_to_label);
    let sccs = kosaraju_scc(&jump_graph);

    let mut out = Vec::new();

    for comp in sccs {
        if !is_cyclic_component(&jump_graph, &comp) {
            continue;
        }

        let comp_labels: Vec<String> = comp
            .iter()
            .filter_map(|idx| jump_graph.node_weight(*idx).cloned())
            .collect();

        if comp_labels.is_empty() {
            continue;
        }

        let has_opt_in = comp_labels.iter().any(|l| opt_in.contains_key(l));
        if !has_opt_in {
            continue;
        }

        let has_escape = component_has_escape_path(
            &ir,
            &comp_labels,
            &clusters,
            &node_to_label,
            &entry_to_label,
        );
        if has_escape {
            continue;
        }

        for label in &comp_labels {
            if let Some(&span) = opt_in.get(label) {
                out.push(AnalysisError::InfiniteDialogueLoop {
                    label: label.clone(),
                    span,
                });
            }
        }
    }

    out.sort_by(|a, b| match (a, b) {
        (
            AnalysisError::InfiniteDialogueLoop { label: la, .. },
            AnalysisError::InfiniteDialogueLoop { label: lb, .. },
        ) => la.cmp(lb),
        _ => std::cmp::Ordering::Equal,
    });

    out
}

// ─────────────────────────────────────────────────────────────────────────────
// AST opt-in collection
// ─────────────────────────────────────────────────────────────────────────────

fn collect_opt_in_labels(root: &Ast) -> HashMap<String, SimpleSpan> {
    let mut out = HashMap::new();
    collect_opt_in_recursive(root, &mut out);
    out
}

fn collect_opt_in_recursive(node: &Ast, out: &mut HashMap<String, SimpleSpan>) {
    match node.content() {
        AstContent::LabeledBlock { label, block } => {
            if has_check_loops_lint(node) {
                out.entry(label.clone()).or_insert(node.span());
            }
            collect_opt_in_recursive(block, out);
        }
        _ => {
            for child in node.children() {
                collect_opt_in_recursive(child, out);
            }
        }
    }
}

fn has_check_loops_lint(label_ast: &Ast) -> bool {
    label_ast.decorators().iter().any(is_check_loops_decorator)
}

fn is_check_loops_decorator(d: &Decorator) -> bool {
    if d.name() != "lint" {
        return false;
    }

    match d.args().content() {
        AstContent::ExprList(items) => items.iter().any(is_check_loops_arg),
        _ => false,
    }
}

fn is_check_loops_arg(arg: &Ast) -> bool {
    match arg.content() {
        AstContent::Value(RuntimeValue::IdentPath(parts)) => {
            parts.len() == 1 && parts[0] == "check_loops"
        }
        AstContent::Value(RuntimeValue::Str(s)) => s.to_string() == "check_loops",
        _ => false,
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Label graph projection from IR
// ─────────────────────────────────────────────────────────────────────────────

fn build_entry_to_label(ir: &crate::ir::IrGraph) -> HashMap<NodeIndex, String> {
    // A NodeIndex may have multiple aliases in merged graphs; choose deterministic
    // canonical name (shorter first, then lexicographic).
    let mut grouped: HashMap<NodeIndex, Vec<String>> = HashMap::new();
    for (name, &idx) in &ir.labels {
        grouped.entry(idx).or_default().push(name.clone());
    }

    let mut out = HashMap::new();
    for (idx, mut names) in grouped {
        names.sort_by(|a, b| a.len().cmp(&b.len()).then(a.cmp(b)));
        if let Some(name) = names.into_iter().next() {
            out.insert(idx, name);
        }
    }
    out
}

fn build_node_to_label(
    clusters: &HashMap<String, HashSet<NodeIndex>>,
) -> HashMap<NodeIndex, String> {
    let mut out = HashMap::new();
    for (label, nodes) in clusters {
        for &n in nodes {
            out.insert(n, label.clone());
        }
    }
    out
}

fn build_label_jump_graph(
    ir: &crate::ir::IrGraph,
    clusters: &HashMap<String, HashSet<NodeIndex>>,
    entry_to_label: &HashMap<NodeIndex, String>,
) -> StableDiGraph<String, ()> {
    let mut g: StableDiGraph<String, ()> = StableDiGraph::new();
    let mut label_idx: HashMap<String, NodeIndex> = HashMap::new();

    let mut labels: Vec<String> = clusters.keys().cloned().collect();
    labels.sort();

    for label in labels {
        let idx = g.add_node(label.clone());
        label_idx.insert(label, idx);
    }

    let mut edges_seen: HashSet<(NodeIndex, NodeIndex)> = HashSet::new();

    for (src_label, nodes) in clusters {
        let Some(&src_idx) = label_idx.get(src_label) else {
            continue;
        };

        for &node in nodes {
            let Some(IrNodeKind::Jump) = ir.graph.node_weight(node) else {
                continue;
            };

            for e in ir.graph.edges(node) {
                if !matches!(e.weight(), IrEdge::Jump) {
                    continue;
                }

                let target_raw = e.target();
                let Some(target) = follow_nops(ir, target_raw) else {
                    continue;
                };
                let Some(dst_label) = entry_to_label.get(&target) else {
                    continue;
                };
                let Some(&dst_idx) = label_idx.get(dst_label) else {
                    continue;
                };

                if edges_seen.insert((src_idx, dst_idx)) {
                    g.add_edge(src_idx, dst_idx, ());
                }
            }
        }
    }

    g
}

fn is_cyclic_component(g: &StableDiGraph<String, ()>, comp: &[NodeIndex]) -> bool {
    if comp.len() > 1 {
        return true;
    }

    let Some(&only) = comp.first() else {
        return false;
    };

    g.edges(only).any(|e| e.target() == only)
}

// ─────────────────────────────────────────────────────────────────────────────
// Escape analysis on IR CFG
// ─────────────────────────────────────────────────────────────────────────────

fn component_has_escape_path(
    ir: &crate::ir::IrGraph,
    comp_labels: &[String],
    clusters: &HashMap<String, HashSet<NodeIndex>>,
    node_to_label: &HashMap<NodeIndex, String>,
    entry_to_label: &HashMap<NodeIndex, String>,
) -> bool {
    let label_set: HashSet<&str> = comp_labels.iter().map(String::as_str).collect();

    // Seed traversal from each SCC label entry.
    let mut queue: VecDeque<NodeIndex> = VecDeque::new();
    let mut seen: HashSet<NodeIndex> = HashSet::new();

    for (entry, label) in entry_to_label {
        if label_set.contains(label.as_str()) {
            queue.push_back(*entry);
        }
    }

    while let Some(cur) = queue.pop_front() {
        if !seen.insert(cur) {
            continue;
        }

        let Some(kind) = ir.graph.node_weight(cur) else {
            return true;
        };

        // Direct terminator: escape path exists.
        if matches!(
            kind,
            IrNodeKind::Return { .. } | IrNodeKind::End | IrNodeKind::Todo
        ) {
            return true;
        }

        // Conservative false-positive prevention:
        // subroutine call-and-return may escape via callee behavior.
        if matches!(kind, IrNodeKind::LetCall { .. }) {
            return true;
        }

        if matches!(kind, IrNodeKind::Jump) {
            // One-way jump: follow only Jump edge.
            let mut had_jump_edge = false;
            for e in ir.graph.edges(cur) {
                if !matches!(e.weight(), IrEdge::Jump) {
                    continue;
                }
                had_jump_edge = true;

                let Some(next) = follow_nops(ir, e.target()) else {
                    return true;
                };

                if let Some(lbl) = node_to_label.get(&next) {
                    if label_set.contains(lbl.as_str()) {
                        queue.push_back(next);
                    } else {
                        return true;
                    }
                } else {
                    return true;
                }
            }

            if !had_jump_edge {
                return true;
            }

            continue;
        }

        // Generic flow: follow all outgoing edges.
        for e in ir.graph.edges(cur) {
            let Some(next) = follow_nops(ir, e.target()) else {
                return true;
            };

            if let Some(lbl) = node_to_label.get(&next) {
                if label_set.contains(lbl.as_str()) {
                    queue.push_back(next);
                } else {
                    // We reached a node owned by a label outside this SCC.
                    return true;
                }
            } else {
                // Target is outside any label cluster => escape path exists.
                return true;
            }
        }
    }

    // No escaping path discovered.
    let _ = clusters; // kept in signature for future refinement/debugging context.
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;

    fn parse(src: &str) -> Ast {
        match parse_source(src) {
            Ok(ast) => ast,
            Err(err) => panic!("parse failed: {err:?}"),
        }
    }

    fn loop_labels(errors: &[AnalysisError]) -> Vec<String> {
        let mut out: Vec<String> = errors
            .iter()
            .filter_map(|e| match e {
                AnalysisError::InfiniteDialogueLoop { label, .. } => Some(label.clone()),
                _ => None,
            })
            .collect();
        out.sort();
        out
    }

    #[test]
    fn empty_script_has_no_diagnostics() {
        let ast = Ast::block(vec![]);
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn self_loop_with_opt_in_is_reported() {
        let ast = parse(
            r#"
@lint(check_loops)
label start {
  jump start
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert_eq!(labels, vec!["start".to_owned()]);
    }

    #[test]
    fn self_loop_without_opt_in_is_not_reported() {
        let ast = parse(
            r#"
label start {
  jump start
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert!(labels.is_empty(), "expected no labels, got: {labels:?}");
    }

    #[test]
    fn two_label_cycle_reports_only_opted_in_labels() {
        let ast = parse(
            r#"
@lint(check_loops)
label a {
  jump b
}

label b {
  jump a
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert_eq!(labels, vec!["a".to_owned()]);
    }

    #[test]
    fn cycle_with_return_escape_is_not_reported() {
        let ast = parse(
            r#"
@lint(check_loops)
label a {
  if true {
    jump b
  } else {
    return
  }
}

label b {
  jump a
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert!(labels.is_empty(), "expected no labels, got: {labels:?}");
    }

    #[test]
    fn cycle_with_jump_to_outside_label_is_not_reported() {
        let ast = parse(
            r#"
@lint(check_loops)
label a {
  menu {
    "loop" { jump b }
    "escape" { jump exit }
  }
}

label b {
  jump a
}

label exit {
  end!()
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert!(labels.is_empty(), "expected no labels, got: {labels:?}");
    }

    #[test]
    fn call_and_return_is_treated_conservatively_as_escape() {
        let ast = parse(
            r#"
@lint(check_loops)
label a {
  let x = jump worker and return
  jump a
}

label worker {
  return 1
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert!(labels.is_empty(), "expected no labels, got: {labels:?}");
    }

    #[test]
    fn trapped_cycle_with_two_opted_in_labels_reports_both() {
        let ast = parse(
            r#"
@lint(check_loops)
label a {
  jump b
}

@lint(check_loops)
label b {
  jump a
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert_eq!(labels, vec!["a".to_owned(), "b".to_owned()]);
    }
}
