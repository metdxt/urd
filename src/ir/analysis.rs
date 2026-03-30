//! Shared graph-analysis helpers for IR renderers.
//!
//! Both the [`super::dot`] and [`super::mermaid`] renderers need the same
//! structural analysis of an [`super::IrGraph`] before they can emit their
//! respective output formats.  Centralising these passes here ensures that
//! both renderers see exactly the same set of reachable nodes, cluster
//! assignments, and return-edge routing — so their diagrams are always
//! consistent with each other.
//!
//! # Functions
//!
//! | Function | Purpose |
//! |---|---|
//! | [`reachable_nodes`] | BFS from `graph.entry`; excludes dead nodes |
//! | [`compute_clusters`] | Maps each label name → its member [`NodeId`]s |
//! | [`follow_nops`] | Collapses compiler merge-point (`Nop`) chains |
//! | [`entry_cluster_name`] | Finds the first label reached from `graph.entry` |
//! | [`callee_to_rets`] | Maps callee entry → caller ret continuations |
//! | [`node_to_cluster`] | Inverts the cluster map: NodeId → owning label |

use std::collections::{HashMap, HashSet, VecDeque};

use super::{IrGraph, IrNodeKind, NODE_END, NodeId};

// ─── Reachability ─────────────────────────────────────────────────────────────

/// Returns the set of all [`NodeId`]s reachable from [`IrGraph::entry`].
///
/// Performs a BFS following every edge kind — including [`IrNodeKind::Jump`]
/// targets and both `target` and `next` of [`IrNodeKind::LetCall`] — so that
/// only genuinely unreachable nodes (e.g. dead `ExitScope` nodes left over
/// when all paths use `jump`/`return`) are excluded from the result.
///
/// [`NODE_END`] and out-of-bounds indices are silently ignored.
pub fn reachable_nodes(graph: &IrGraph) -> HashSet<NodeId> {
    let mut reachable: HashSet<NodeId> = HashSet::new();
    let mut queue: VecDeque<NodeId> = VecDeque::new();

    if graph.entry != NODE_END {
        queue.push_back(graph.entry);
    }

    while let Some(id) = queue.pop_front() {
        if id == NODE_END || id.0 as usize >= graph.nodes.len() {
            continue;
        }
        if !reachable.insert(id) {
            continue; // already visited
        }

        match &graph.nodes[id.0 as usize].kind {
            // True terminals — no successors to enqueue.
            IrNodeKind::End | IrNodeKind::Todo | IrNodeKind::Return { .. } => {}

            IrNodeKind::Jump { target } => {
                queue.push_back(*target);
            }

            IrNodeKind::LetCall { target, next, .. } => {
                queue.push_back(*target);
                queue.push_back(*next);
            }

            IrNodeKind::Assign { next, .. }
            | IrNodeKind::Eval { next, .. }
            | IrNodeKind::EnterScope { next, .. }
            | IrNodeKind::ExitScope { next, .. }
            | IrNodeKind::DefineEnum { next, .. }
            | IrNodeKind::DefineScriptDecorator { next, .. }
            | IrNodeKind::Nop { next }
            | IrNodeKind::Dialogue { next, .. } => {
                queue.push_back(*next);
            }

            IrNodeKind::Branch {
                then_node,
                else_node,
                ..
            } => {
                queue.push_back(*then_node);
                queue.push_back(*else_node);
            }

            IrNodeKind::Switch { arms, default, .. } => {
                for arm in arms {
                    queue.push_back(arm.target);
                }
                if let Some(d) = default {
                    queue.push_back(*d);
                }
            }

            IrNodeKind::Choice { options, .. } => {
                for opt in options {
                    queue.push_back(opt.entry);
                }
            }
        }
    }

    reachable
}

// ─── Nop collapsing ───────────────────────────────────────────────────────────

/// Follows a chain of consecutive [`IrNodeKind::Nop`] nodes and returns the
/// first non-`Nop` [`NodeId`] (or [`NODE_END`] if the chain terminates there).
///
/// Used by both renderers to collapse compiler-generated merge points so they
/// never appear as rendered nodes or intermediate edge targets.
///
/// Includes a cycle-guard (iterates at most `graph.nodes.len() + 1` times) to
/// avoid an infinite loop on a pathological graph.
pub fn follow_nops(graph: &IrGraph, mut id: NodeId) -> NodeId {
    for _ in 0..graph.nodes.len() + 1 {
        if id == NODE_END || id.0 as usize >= graph.nodes.len() {
            break;
        }
        match &graph.nodes[id.0 as usize].kind {
            IrNodeKind::Nop { next } => id = *next,
            _ => break,
        }
    }
    id
}

// ─── Cluster membership ───────────────────────────────────────────────────────

/// Computes which [`NodeId`]s belong to each named label scope.
///
/// For each label, performs a BFS from its [`IrNodeKind::EnterScope`] node
/// while respecting these boundaries:
///
/// - **Dead nodes** (not in `reachable`) are excluded.
/// - **[`IrNodeKind::Nop`] nodes** are followed transparently but *never*
///   added to the member set (they are collapsed at render time).
/// - **Other labels' entry nodes** are not entered — they belong to their own
///   cluster.
/// - **[`IrNodeKind::Jump`]** targets are not followed: a Jump *belongs* to
///   the current cluster, but its destination belongs to another.
/// - **[`IrNodeKind::LetCall`]**: the callee (`target`) lives in another
///   cluster (guarded by `all_entries`), but the continuation (`next`) stays
///   in this cluster and *is* followed.
/// - **[`IrNodeKind::ExitScope`]** for this label stops recursion: the exit
///   marker is included, but its `next` continuation falls outside the cluster.
/// - **[`IrNodeKind::Return`]** and **[`IrNodeKind::End`]** are true
///   terminals — no successors within the cluster.
pub fn compute_clusters(
    graph: &IrGraph,
    reachable: &HashSet<NodeId>,
) -> HashMap<String, HashSet<NodeId>> {
    let all_entries: HashSet<NodeId> = graph.labels.values().copied().collect();
    let mut clusters: HashMap<String, HashSet<NodeId>> = HashMap::new();

    for (label_name, &entry_id) in &graph.labels {
        let mut members: HashSet<NodeId> = HashSet::new();
        let mut queue: VecDeque<NodeId> = VecDeque::new();
        queue.push_back(entry_id);

        while let Some(node_id) = queue.pop_front() {
            if node_id == NODE_END || node_id.0 as usize >= graph.nodes.len() {
                continue;
            }
            if members.contains(&node_id) {
                continue;
            }
            if !reachable.contains(&node_id) {
                continue; // skip dead nodes
            }
            // Do not enter a different label's cluster.
            if all_entries.contains(&node_id) && node_id != entry_id {
                continue;
            }

            let node = &graph.nodes[node_id.0 as usize];

            // Nop nodes are visited (to reach their successors) but not added.
            if !matches!(&node.kind, IrNodeKind::Nop { .. }) {
                members.insert(node_id);
            }

            match &node.kind {
                // True terminals — no successors within this cluster.
                IrNodeKind::Jump { .. }
                | IrNodeKind::Return { .. }
                | IrNodeKind::End
                | IrNodeKind::Todo => {}

                // LetCall: the callee belongs to another cluster (guarded by
                // all_entries above), but the continuation (next) stays in
                // this cluster.
                IrNodeKind::LetCall { next, .. } => {
                    queue.push_back(*next);
                }

                // Our own exit scope: include it, but do not recurse beyond.
                IrNodeKind::ExitScope { label, .. } if label == label_name => {}

                IrNodeKind::Assign { next, .. }
                | IrNodeKind::Eval { next, .. }
                | IrNodeKind::EnterScope { next, .. }
                | IrNodeKind::ExitScope { next, .. }
                | IrNodeKind::DefineEnum { next, .. }
                | IrNodeKind::DefineScriptDecorator { next, .. }
                | IrNodeKind::Nop { next }
                | IrNodeKind::Dialogue { next, .. } => {
                    queue.push_back(*next);
                }

                IrNodeKind::Branch {
                    then_node,
                    else_node,
                    ..
                } => {
                    queue.push_back(*then_node);
                    queue.push_back(*else_node);
                }

                IrNodeKind::Switch { arms, default, .. } => {
                    for arm in arms {
                        queue.push_back(arm.target);
                    }
                    if let Some(d) = default {
                        queue.push_back(*d);
                    }
                }

                IrNodeKind::Choice { options, .. } => {
                    for opt in options {
                        queue.push_back(opt.entry);
                    }
                }
            }
        }

        clusters.insert(label_name.clone(), members);
    }

    clusters
}

// ─── Entry cluster ────────────────────────────────────────────────────────────

/// Finds the name of the first label cluster encountered when following the
/// linear prologue (assignments, enums, nops) from [`IrGraph::entry`].
///
/// The "entry cluster" is the label that the script's preamble (global
/// declarations, enum definitions, etc.) falls through to first.  Renderers
/// use this to give the entry cluster a visually distinct border.
///
/// Returns `None` if `graph.entry` never reaches a label node (e.g. the
/// script has no `label` blocks at all).
pub fn entry_cluster_name(graph: &IrGraph) -> Option<String> {
    let label_by_entry: HashMap<NodeId, &str> = graph
        .labels
        .iter()
        .map(|(name, &id)| (id, name.as_str()))
        .collect();

    let mut current = graph.entry;
    let mut visited: HashSet<NodeId> = HashSet::new();

    loop {
        if current == NODE_END || current.0 as usize >= graph.nodes.len() {
            return None;
        }
        if !visited.insert(current) {
            return None; // cycle guard
        }
        if let Some(name) = label_by_entry.get(&current) {
            return Some((*name).to_string());
        }
        match &graph.nodes[current.0 as usize].kind {
            IrNodeKind::Assign { next, .. }
            | IrNodeKind::Eval { next, .. }
            | IrNodeKind::Nop { next }
            | IrNodeKind::DefineEnum { next, .. }
            | IrNodeKind::DefineScriptDecorator { next, .. } => {
                current = *next;
            }
            _ => return None,
        }
    }
}

// ─── Callee → ret continuations ──────────────────────────────────────────────

/// Builds a map from **callee entry [`NodeId`]** to the list of **ret
/// continuation [`NodeId`]**s for every [`IrNodeKind::LetCall`] in the graph.
///
/// A `LetCall { target, next, .. }` node means "call the subroutine whose
/// [`IrNodeKind::EnterScope`] is at `target` and resume at `next` when it
/// returns".  This map inverts that relationship so that a `Return` node inside
/// a subroutine can look up all caller resumption points and draw back-edges to
/// them rather than pointing at the misleading `__end__` sink.
///
/// `next` values are **not** Nop-followed here — callers should apply
/// [`follow_nops`] when consuming the returned `NodeId`s.
pub fn callee_to_rets(graph: &IrGraph) -> HashMap<NodeId, Vec<NodeId>> {
    let mut map: HashMap<NodeId, Vec<NodeId>> = HashMap::new();
    for node in &graph.nodes {
        if let IrNodeKind::LetCall { target, next, .. } = &node.kind {
            map.entry(*target).or_default().push(*next);
        }
    }
    map
}

// ─── Inverted cluster map ────────────────────────────────────────────────────

/// Inverts a cluster map, producing a lookup from [`NodeId`] to the name of
/// the label cluster that owns it.
///
/// The returned map borrows label names from `clusters` for zero-copy
/// efficiency.  If a node somehow appears in multiple clusters (which should
/// not happen in a well-formed graph) the last one wins.
pub fn node_to_cluster(clusters: &HashMap<String, HashSet<NodeId>>) -> HashMap<NodeId, &str> {
    clusters
        .iter()
        .flat_map(|(name, members)| members.iter().map(move |&id| (id, name.as_str())))
        .collect()
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)]
    #![allow(clippy::unwrap_used)]

    use std::collections::HashMap;

    use super::*;
    use crate::ir::{IrGraph, IrNode, IrNodeKind, NODE_END, NodeId};

    // ── follow_nops ──────────────────────────────────────────────────────────

    /// Build a small graph: N0(Nop→N1), N1(Nop→N2), N2(End).
    fn nop_chain_graph() -> IrGraph {
        IrGraph {
            nodes: vec![
                IrNode {
                    id: NodeId(0),
                    kind: IrNodeKind::Nop { next: NodeId(1) },
                },
                IrNode {
                    id: NodeId(1),
                    kind: IrNodeKind::Nop { next: NodeId(2) },
                },
                IrNode {
                    id: NodeId(2),
                    kind: IrNodeKind::End,
                },
            ],
            entry: NodeId(0),
            labels: HashMap::new(),
        }
    }

    #[test]
    fn follow_nops_skips_full_chain() {
        let g = nop_chain_graph();
        assert_eq!(follow_nops(&g, NodeId(0)), NodeId(2));
    }

    #[test]
    fn follow_nops_returns_non_nop_unchanged() {
        let g = nop_chain_graph();
        assert_eq!(follow_nops(&g, NodeId(2)), NodeId(2));
    }

    #[test]
    fn follow_nops_returns_node_end_unchanged() {
        let g = nop_chain_graph();
        assert_eq!(follow_nops(&g, NODE_END), NODE_END);
    }

    // ── reachable_nodes ──────────────────────────────────────────────────────

    /// N0(Assign→N1), N1(End).  N2(Assign→NODE_END) is unreachable.
    fn two_node_graph_with_dead() -> IrGraph {
        use crate::parser::ast::{Ast, DeclKind};
        use crate::runtime::value::RuntimeValue;
        let dummy_expr = Ast::value(RuntimeValue::Int(0));
        let dummy_var = Ast::value(RuntimeValue::IdentPath(vec!["x".into()]));
        IrGraph {
            nodes: vec![
                IrNode {
                    id: NodeId(0),
                    kind: IrNodeKind::Assign {
                        var: "x".into(),
                        scope: DeclKind::Variable,
                        expr: dummy_expr.clone(),
                        next: NodeId(1),
                    },
                },
                IrNode {
                    id: NodeId(1),
                    kind: IrNodeKind::End,
                },
                IrNode {
                    id: NodeId(2),
                    kind: IrNodeKind::Assign {
                        var: "y".into(),
                        scope: DeclKind::Variable,
                        expr: dummy_var,
                        next: NODE_END,
                    },
                },
            ],
            entry: NodeId(0),
            labels: HashMap::new(),
        }
    }

    #[test]
    fn reachable_includes_live_nodes() {
        let g = two_node_graph_with_dead();
        let r = reachable_nodes(&g);
        assert!(r.contains(&NodeId(0)));
        assert!(r.contains(&NodeId(1)));
    }

    #[test]
    fn reachable_excludes_dead_nodes() {
        let g = two_node_graph_with_dead();
        let r = reachable_nodes(&g);
        assert!(
            !r.contains(&NodeId(2)),
            "N2 is unreachable and must be excluded"
        );
    }

    // ── compute_clusters ────────────────────────────────────────────────────

    fn compile(src: &str) -> IrGraph {
        use crate::{compiler::Compiler, parse_test, parser::block::script};
        let ast = parse_test!(script(), src).expect("parse failed");
        Compiler::compile(&ast).expect("compile failed")
    }

    #[test]
    fn compute_clusters_basic_label() {
        let g = compile(
            r#"
            label foo {
                let x = 1
            }
            "#,
        );
        let r = reachable_nodes(&g);
        let clusters = compute_clusters(&g, &r);
        assert!(clusters.contains_key("foo"), "must have cluster for 'foo'");
        assert!(!clusters["foo"].is_empty(), "cluster must be non-empty");
    }

    #[test]
    fn compute_clusters_letcall_next_stays_in_cluster() {
        // After the LetCall, the continuation (next) should remain in the caller's cluster.
        let g = compile(
            r#"
            label start {
                let result = jump helper and return
                let x = 1
            }
            label helper {
                return "ok"
            }
            "#,
        );
        let r = reachable_nodes(&g);
        let clusters = compute_clusters(&g, &r);

        // Find the LetCall node
        let letcall_node = g
            .nodes
            .iter()
            .find(|n| matches!(&n.kind, IrNodeKind::LetCall { .. }));
        assert!(letcall_node.is_some(), "must have a LetCall node");

        let letcall_id = letcall_node.unwrap().id;

        // The LetCall itself must be in the "start" cluster
        let start_cluster = clusters.get("start").expect("start cluster must exist");
        assert!(
            start_cluster.contains(&letcall_id),
            "LetCall node must be in the start cluster"
        );

        // Find the Assign(x=1) node — it comes after the LetCall
        let assign_x = g
            .nodes
            .iter()
            .find(|n| matches!(&n.kind, IrNodeKind::Assign { var, .. } if var == "x"));
        if let Some(ax) = assign_x {
            assert!(
                start_cluster.contains(&ax.id),
                "node after LetCall (let x=1) must be in the same cluster as the LetCall (start)"
            );
        }
    }

    // ── entry_cluster_name ───────────────────────────────────────────────────

    #[test]
    fn entry_cluster_name_finds_first_label() {
        let g = compile(
            r#"
            label intro {
                let y = 0
            }
            "#,
        );
        let name = entry_cluster_name(&g);
        assert_eq!(name.as_deref(), Some("intro"));
    }

    #[test]
    fn entry_cluster_name_skips_preamble_assigns() {
        let g = compile(
            r#"
            global rep = 0
            label main_scene {
                let x = 1
            }
            "#,
        );
        let name = entry_cluster_name(&g);
        assert_eq!(name.as_deref(), Some("main_scene"));
    }

    #[test]
    fn entry_cluster_name_none_when_no_labels() {
        let g = compile(
            r#"
            let x = 1
            "#,
        );
        let name = entry_cluster_name(&g);
        assert!(
            name.is_none(),
            "no labels → entry_cluster_name must be None"
        );
    }

    // ── callee_to_rets ───────────────────────────────────────────────────────

    #[test]
    fn callee_to_rets_maps_target_to_next() {
        let g = compile(
            r#"
            label start {
                let r = jump helper and return
            }
            label helper {
                return "ok"
            }
            "#,
        );
        let map = callee_to_rets(&g);
        let helper_entry = *g.labels.get("helper").expect("helper must be a label");
        assert!(
            map.contains_key(&helper_entry),
            "callee_to_rets must have an entry for the helper's entry NodeId"
        );
        let rets = &map[&helper_entry];
        assert_eq!(rets.len(), 1, "one call site → one ret continuation");
    }

    #[test]
    fn callee_to_rets_empty_when_no_letcall() {
        let g = compile(
            r#"
            let x = 1
            "#,
        );
        let map = callee_to_rets(&g);
        assert!(
            map.is_empty(),
            "no LetCall nodes → callee_to_rets must be empty"
        );
    }

    // ── node_to_cluster ──────────────────────────────────────────────────────

    #[test]
    fn node_to_cluster_inverts_correctly() {
        let g = compile(
            r#"
            label alpha {
                let a = 1
            }
            label beta {
                let b = 2
            }
            "#,
        );
        let r = reachable_nodes(&g);
        let clusters = compute_clusters(&g, &r);
        let inv = node_to_cluster(&clusters);

        // Every node that is in a cluster must resolve back to that cluster's name.
        for (name, members) in &clusters {
            for &id in members {
                assert_eq!(
                    inv.get(&id),
                    Some(&name.as_str()),
                    "node {id:?} must map back to its owning cluster '{name}'"
                );
            }
        }
    }

    #[test]
    fn node_to_cluster_does_not_contain_unclustered_nodes() {
        // A bare assign with no label produces a node outside any cluster.
        let g = compile(
            r#"
            let z = 99
            "#,
        );
        let r = reachable_nodes(&g);
        let clusters = compute_clusters(&g, &r);
        let inv = node_to_cluster(&clusters);
        // Since there are no labels, the inv map must be empty.
        assert!(inv.is_empty(), "no labels → node_to_cluster must be empty");
    }
}
