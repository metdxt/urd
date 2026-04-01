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
//! A diagnostic is emitted for each opted-in label that:
//! - belongs to a cyclic SCC where no path can escape to a terminator
//!   (`return`, `end!`, `todo!`) or a label outside that SCC, **or**
//! - is not itself part of a cycle but all paths from it lead exclusively
//!   into such trapped SCCs (i.e. the label is a "feeder" into a black hole).

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

    // ── Pass 1: find all trapped SCCs (cyclic, no escape path). ─────────────
    // Collect the set of label names that are members of a trapped SCC so
    // that Pass 2 can reason about whether a feeder label flows only into them.
    let mut trapped_labels: HashSet<String> = HashSet::new();

    for comp in &sccs {
        if !is_cyclic_component(&jump_graph, comp) {
            continue;
        }

        let comp_labels: Vec<String> = comp
            .iter()
            .filter_map(|idx| jump_graph.node_weight(*idx).cloned())
            .collect();

        if comp_labels.is_empty() {
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

        // This SCC is trapped.  Record all its member labels.
        for label in &comp_labels {
            trapped_labels.insert(label.clone());
        }

        // Report any opted-in members immediately.
        for label in &comp_labels {
            if let Some(&span) = opt_in.get(label) {
                out.push(AnalysisError::InfiniteDialogueLoop {
                    label: label.clone(),
                    span,
                });
            }
        }
    }

    // ── Pass 2: feeder labels ────────────────────────────────────────────────
    // An opt-in label that is NOT itself part of a trapped SCC may still be
    // inescapable if every path it can take leads only into trapped labels.
    // Walk the label jump-graph from each such label; if every reachable
    // non-trapped label is a dead-end (no outgoing edges) or all successors
    // are trapped, the label is a feeder and must be reported.
    let already_reported: HashSet<String> = out
        .iter()
        .filter_map(|e| match e {
            AnalysisError::InfiniteDialogueLoop { label, .. } => Some(label.clone()),
            _ => None,
        })
        .collect();

    // Build a quick label→NodeIndex lookup for the jump graph.
    let label_to_jump_node: HashMap<String, NodeIndex> = jump_graph
        .node_indices()
        .filter_map(|idx| jump_graph.node_weight(idx).map(|l| (l.clone(), idx)))
        .collect();

    for (label, &span) in &opt_in {
        if already_reported.contains(label) {
            continue;
        }

        let Some(&start_idx) = label_to_jump_node.get(label) else {
            continue;
        };

        if label_inevitably_enters_trap(start_idx, &jump_graph, &trapped_labels) {
            out.push(AnalysisError::InfiniteDialogueLoop {
                label: label.clone(),
                span,
            });
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

/// Return `true` if every path reachable from `start` in the label jump-graph
/// inevitably enters a trapped SCC with no way out.
///
/// Strategy: collect all non-trapped nodes reachable from `start`.  Within
/// that subgraph, a node is an "escape" if it has no outgoing edges (it
/// terminates without looping) or if it belongs to a non-trivial cycle among
/// non-trapped nodes (meaning those labels interact among themselves, which
/// means their IR bodies contain real control flow including possible returns —
/// the existing pass already verified they are not trapped).  If ANY such
/// escape node exists the start label is not inevitably trapped.
///
/// In practice the check is: within the non-trapped reachable subgraph,
/// does every node have at least one successor?  If any non-trapped reachable
/// node has zero successors (a terminal label) the path escapes.  If the
/// subgraph contains a cycle entirely within non-trapped nodes, those labels
/// were already cleared by Pass 1's escape analysis, so we treat that cycle
/// as an escape too.
fn label_inevitably_enters_trap(
    start: NodeIndex,
    g: &StableDiGraph<String, ()>,
    trapped_labels: &HashSet<String>,
) -> bool {
    // If the start node itself has no successors, it cannot loop.
    if g.edges(start).count() == 0 {
        return false;
    }

    // Collect all non-trapped nodes reachable from start (including start).
    let mut reachable_non_trapped: HashSet<NodeIndex> = HashSet::new();
    {
        let mut stack = vec![start];
        while let Some(cur) = stack.pop() {
            if !reachable_non_trapped.insert(cur) {
                continue;
            }
            for e in g.edges(cur) {
                let succ = e.target();
                let succ_label = g.node_weight(succ).map(String::as_str).unwrap_or("");
                // Don't expand trapped nodes — they are black holes.
                if !trapped_labels.contains(succ_label) && !reachable_non_trapped.contains(&succ) {
                    stack.push(succ);
                }
            }
        }
    }

    // Within the non-trapped subgraph, check for any escape node.
    for &node in &reachable_non_trapped {
        let label = g.node_weight(node).map(String::as_str).unwrap_or("");
        if trapped_labels.contains(label) {
            continue;
        }

        // Count successors that are also non-trapped.
        let non_trapped_succs: Vec<NodeIndex> = g
            .edges(node)
            .map(|e| e.target())
            .filter(|succ| {
                let sl = g.node_weight(*succ).map(String::as_str).unwrap_or("");
                !trapped_labels.contains(sl)
            })
            .collect();

        let all_succs: Vec<NodeIndex> = g.edges(node).map(|e| e.target()).collect();

        if all_succs.is_empty() {
            // Terminal label — this is an escape path (falls through / returns).
            return false;
        }

        // If this non-trapped node has a successor that is also non-trapped
        // AND that successor is not start itself forming a trivial self-trap,
        // AND the non-trapped subgraph contains a cycle (node reachable from
        // its own successor) — those nodes were already cleared by Pass 1,
        // meaning their IR bodies have real escape paths.  Treat as escape.
        for &succ in &non_trapped_succs {
            // If the successor can reach back to a non-trapped node (i.e.
            // there's a cycle within non-trapped nodes), that cycle was vetted
            // by Pass 1 and is not a true trap — it's an escape.
            if can_reach_within(succ, node, g, trapped_labels) {
                return false;
            }
        }
    }

    // No escape node found — every path leads into a trapped SCC.
    true
}

/// Return `true` if `target` is reachable from `from` following only
/// non-trapped nodes in `g`.
fn can_reach_within(
    from: NodeIndex,
    target: NodeIndex,
    g: &StableDiGraph<String, ()>,
    trapped_labels: &HashSet<String>,
) -> bool {
    let mut visited: HashSet<NodeIndex> = HashSet::new();
    let mut queue: VecDeque<NodeIndex> = VecDeque::new();
    queue.push_back(from);

    while let Some(cur) = queue.pop_front() {
        if cur == target {
            return true;
        }
        if !visited.insert(cur) {
            continue;
        }
        let label = g.node_weight(cur).map(String::as_str).unwrap_or("");
        if trapped_labels.contains(label) {
            continue;
        }
        for e in g.edges(cur) {
            queue.push_back(e.target());
        }
    }
    false
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

    // ── Feeder-label tests ────────────────────────────────────────────────────

    #[test]
    fn feeder_label_into_trapped_cycle_is_reported() {
        // `start` jumps to `a` which cycles with `b` — inescapable.
        // Only `start` has @lint(check_loops).
        let ast = parse(
            r#"
@lint(check_loops)
label start {
  jump a
}

label a {
  jump b
}

label b {
  jump a
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert_eq!(labels, vec!["start".to_owned()]);
    }

    #[test]
    fn feeder_label_with_escape_is_not_reported() {
        // `start` can jump to `a` (trapped) or `exit` (escapes).
        let ast = parse(
            r#"
@lint(check_loops)
label start {
  menu {
    "loop path" { jump a }
    "escape"    { jump exit }
  }
}

label a {
  jump b
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
        assert!(labels.is_empty(), "expected no errors, got: {labels:?}");
    }

    #[test]
    fn feeder_chain_all_opt_in_reported() {
        // start -> mid -> (a <-> b).  start and mid both have @lint.
        let ast = parse(
            r#"
@lint(check_loops)
label start {
  jump mid
}

@lint(check_loops)
label mid {
  jump a
}

label a {
  jump b
}

label b {
  jump a
}
"#,
        );

        let labels = loop_labels(&check(&ast));
        assert_eq!(labels, vec!["mid".to_owned(), "start".to_owned()]);
    }

    #[test]
    fn feeder_into_non_trapped_cycle_not_reported() {
        // `a <-> b` has an escape path (jump exit inside a), so it is not a
        // trapped SCC.  `start` feeding into it should therefore not be flagged.
        let ast = parse(
            r#"
@lint(check_loops)
label start {
  jump a
}

label a {
  menu {
    "loop" { jump b }
    "exit" { jump exit }
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
        assert!(labels.is_empty(), "expected no errors, got: {labels:?}");
    }
}
