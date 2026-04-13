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
//! | [`reachable_nodes`] | DFS from `graph.entry`; excludes dead nodes |
//! | [`compute_clusters`] | Maps each label name → its member [`NodeIndex`]es |
//! | [`follow_nops`] | Collapses compiler merge-point (`Nop`) chains |
//! | [`entry_cluster_name`] | Finds the first label reached from `graph.entry` |
//! | [`callee_to_rets`] | Maps callee entry → caller ret continuations |
//! | [`node_to_cluster`] | Inverts the cluster map: NodeIndex → owning label |

use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::Direction;
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::{Dfs, EdgeRef};

use super::{IrEdge, IrGraph, IrNodeKind};

// ─── Reachability ─────────────────────────────────────────────────────────────

/// Returns the set of all [`NodeIndex`]es reachable from [`IrGraph::entry`].
///
/// Uses a petgraph [`Dfs`] traversal following every outgoing edge so that
/// only genuinely unreachable nodes (e.g. dead `ExitScope` nodes left over
/// when all paths use `jump`/`return`) are excluded from the result.
///
/// Returns an empty set when [`IrGraph::entry`] is `None`.
pub fn reachable_nodes(graph: &IrGraph) -> HashSet<NodeIndex> {
    let mut reachable: HashSet<NodeIndex> = HashSet::new();
    if let Some(entry) = graph.entry {
        let mut dfs = Dfs::new(&graph.graph, entry);
        while let Some(idx) = dfs.next(&graph.graph) {
            reachable.insert(idx);
        }
    }
    reachable
}

// ─── Nop collapsing ───────────────────────────────────────────────────────────

/// Follows a chain of consecutive [`IrNodeKind::Nop`] nodes via their single
/// [`IrEdge::Next`] outgoing edge and returns the first non-`Nop` [`NodeIndex`].
///
/// Returns `None` when:
/// - `idx` refers to a node that has been removed from the graph.
/// - The Nop chain terminates without a successor (no outgoing `Next` edge).
///
/// This is the petgraph equivalent of the old `NODE_END` sentinel: the absence
/// of a `Next` edge *is* the sentinel.
///
/// Includes a cycle-guard (iterates at most `graph.graph.node_count() + 1`
/// times) to avoid an infinite loop on a pathological graph.
pub fn follow_nops(graph: &IrGraph, mut idx: NodeIndex) -> Option<NodeIndex> {
    for _ in 0..graph.graph.node_count() + 1 {
        match graph.graph.node_weight(idx) {
            None => return None,
            Some(IrNodeKind::Nop) => {
                // Follow the single outgoing Next edge, if any.
                let next = graph
                    .graph
                    .edges_directed(idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Next))
                    .map(|e| e.target());
                match next {
                    Some(n) => idx = n,
                    None => return None,
                }
            }
            Some(_) => return Some(idx),
        }
    }
    // Cycle guard hit — return last known position.
    Some(idx)
}

// ─── Cluster membership ───────────────────────────────────────────────────────

/// Computes which [`NodeIndex`]es belong to each named label scope.
///
/// For each label, performs a BFS from its [`IrNodeKind::EnterScope`] node
/// while respecting these boundaries:
///
/// - **Dead nodes** (not in `reachable`) are excluded.
/// - **[`IrNodeKind::Nop`] nodes** are followed transparently but *never*
///   added to the member set (they are collapsed at render time).
/// - **Other labels' entry nodes** are not entered — they belong to their own
///   cluster.
/// - **[`IrNodeKind::Jump`]** belongs to the current cluster, but its
///   [`IrEdge::Jump`] destination is not followed.
/// - **[`IrNodeKind::LetCall`]**: only the [`IrEdge::Ret`] continuation stays
///   in this cluster; the [`IrEdge::Call`] callee is guarded by `all_entries`.
/// - **[`IrNodeKind::ExitScope`]** for this label stops recursion: the exit
///   marker is included, but no outgoing edges are followed.
/// - **[`IrNodeKind::Return`]**, **[`IrNodeKind::End`]**, and
///   **[`IrNodeKind::Todo`]** are true terminals — no successors within the
///   cluster.
pub fn compute_clusters(
    graph: &IrGraph,
    reachable: &HashSet<NodeIndex>,
) -> HashMap<String, HashSet<NodeIndex>> {
    // Build the authoritative (name, entry_NodeIndex) pairs.
    //
    // When `cluster_names` is populated (multi-file compilation) it holds
    // exactly one canonical name per unique NodeIndex — no aliases — so every
    // cluster BFS starts from a distinct entry.  This prevents the duplicate
    // empty clusters that arise when `labels` contains both `"hub"` and
    // `"main::hub"` pointing at the same node (the first BFS claims all
    // members; the second finds none).
    //
    // Single-file graphs leave `cluster_names` empty; we fall back to
    // iterating `labels` as before.
    let label_entries: Vec<(String, NodeIndex)> = if graph.cluster_names.is_empty() {
        graph.labels.iter().map(|(k, &v)| (k.clone(), v)).collect()
    } else {
        graph
            .cluster_names
            .iter()
            .map(|(&idx, name)| (name.clone(), idx))
            .collect()
    };

    // All label entry NodeIndexes — used as cluster-boundary guards in the BFS.
    let all_entries: HashSet<NodeIndex> = label_entries.iter().map(|(_, idx)| *idx).collect();
    let mut clusters: HashMap<String, HashSet<NodeIndex>> = HashMap::new();

    for (cluster_display_name, entry_id) in &label_entries {
        // The ExitScope node stores the *bare* label name (as written in source).
        // When the display name is namespaced ("tavern::leave_tavern"), strip
        // the prefix so the ExitScope guard below matches correctly.
        let bare_label: &str = cluster_display_name
            .rfind("::")
            .map(|i| &cluster_display_name[i + 2..])
            .unwrap_or(cluster_display_name);

        let mut members: HashSet<NodeIndex> = HashSet::new();
        let mut queue: VecDeque<NodeIndex> = VecDeque::new();
        queue.push_back(*entry_id);

        while let Some(node_idx) = queue.pop_front() {
            if !reachable.contains(&node_idx) {
                continue;
            }
            if members.contains(&node_idx) {
                continue;
            }
            // Do not enter a different label's cluster.
            if all_entries.contains(&node_idx) && node_idx != *entry_id {
                continue;
            }

            let kind = match graph.graph.node_weight(node_idx) {
                Some(k) => k,
                None => continue,
            };

            // Nop nodes are visited (to reach their successors) but never added
            // to the member set — they are collapsed at render time.
            if !matches!(kind, IrNodeKind::Nop) {
                members.insert(node_idx);
            }

            match kind {
                // True terminals within the cluster — no successors to enqueue.
                IrNodeKind::Jump
                | IrNodeKind::Return { .. }
                | IrNodeKind::End
                | IrNodeKind::Todo => {}

                // LetCall: the callee (Call edge) lives in another cluster and
                // is guarded by `all_entries`.  Only follow the Ret continuation
                // which stays in this cluster.
                IrNodeKind::LetCall { .. } => {
                    for edge in graph.graph.edges_directed(node_idx, Direction::Outgoing) {
                        if matches!(edge.weight(), IrEdge::Ret) {
                            queue.push_back(edge.target());
                        }
                    }
                }

                // Our own ExitScope: include the node but do not recurse beyond.
                IrNodeKind::ExitScope { label } if label.as_str() == bare_label => {}

                // All other nodes (Assign, Eval, EnterScope, ExitScope for a
                // different label, DefineEnum, DefineScriptDecorator, Nop,
                // Dialogue, Branch, Switch, Choice): follow all outgoing edges.
                // Cross-cluster destinations are blocked by the `all_entries`
                // guard at the top of the loop.
                _ => {
                    for neighbor in graph
                        .graph
                        .neighbors_directed(node_idx, Direction::Outgoing)
                    {
                        queue.push_back(neighbor);
                    }
                }
            }
        }

        clusters.insert(cluster_display_name.clone(), members);
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
/// Returns `None` if `graph.entry` is absent or never reaches a label node
/// (e.g. the script has no `label` blocks at all).
pub fn entry_cluster_name(graph: &IrGraph) -> Option<String> {
    // Build a NodeIndex → name map.  When `cluster_names` is populated
    // (multi-file compilation) it already holds exactly one canonical name
    // per unique NodeIndex — use it directly.  For single-file graphs we
    // fall back to `labels`, but when two labels alias the same NodeIndex
    // we deterministically prefer the shorter name (and among equal-length
    // names, the one without a `::` module prefix).
    let label_by_entry: HashMap<NodeIndex, &str> = if !graph.cluster_names.is_empty() {
        graph
            .cluster_names
            .iter()
            .map(|(&idx, name)| (idx, name.as_str()))
            .collect()
    } else {
        let mut map: HashMap<NodeIndex, &str> = HashMap::new();
        for (name, &idx) in &graph.labels {
            let replace = match map.get(&idx) {
                None => true,
                Some(existing) => {
                    let new_has_prefix = name.contains("::");
                    let old_has_prefix = existing.contains("::");
                    if new_has_prefix != old_has_prefix {
                        // Prefer the bare (non-prefixed) name.
                        !new_has_prefix
                    } else {
                        // Both prefixed or both bare — prefer shorter, then
                        // lexicographically smaller for full determinism.
                        (name.len(), name.as_str()) < (existing.len(), *existing)
                    }
                }
            };
            if replace {
                map.insert(idx, name.as_str());
            }
        }
        map
    };

    let mut current = graph.entry?;
    let mut visited: HashSet<NodeIndex> = HashSet::new();

    loop {
        if !visited.insert(current) {
            return None; // cycle guard
        }

        if let Some(name) = label_by_entry.get(&current) {
            return Some((*name).to_string());
        }

        match graph.graph.node_weight(current) {
            Some(
                IrNodeKind::Assign { .. }
                | IrNodeKind::Eval { .. }
                | IrNodeKind::Nop
                | IrNodeKind::DefineEnum { .. }
                | IrNodeKind::DefineScriptDecorator { .. }
                | IrNodeKind::DefineFunction { .. }
                | IrNodeKind::DefineStruct { .. }
                | IrNodeKind::ExternDecl { .. },
            ) => {
                // Preamble node: follow its single Next edge.
                let next = graph
                    .graph
                    .edges_directed(current, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Next))
                    .map(|e| e.target());
                match next {
                    Some(n) => current = n,
                    None => return None,
                }
            }
            _ => return None,
        }
    }
}

// ─── Callee → ret continuations ──────────────────────────────────────────────

/// Builds a map from **callee entry [`NodeIndex`]** to the list of **ret
/// continuation [`NodeIndex`]**s for every [`IrNodeKind::LetCall`] in the
/// graph.
///
/// A `LetCall` node has an [`IrEdge::Call`] arc to the callee's entry and an
/// [`IrEdge::Ret`] arc to the return continuation.  This map inverts that
/// relationship so that a `Return` node inside a subroutine can look up all
/// caller resumption points and draw back-edges to them.
///
/// `Ret` targets are **not** Nop-followed here — callers should apply
/// [`follow_nops`] when consuming the returned [`NodeIndex`] values if needed.
pub fn callee_to_rets(graph: &IrGraph) -> HashMap<NodeIndex, Vec<NodeIndex>> {
    let mut map: HashMap<NodeIndex, Vec<NodeIndex>> = HashMap::new();
    for idx in graph.graph.node_indices() {
        if !matches!(
            graph.graph.node_weight(idx),
            Some(IrNodeKind::LetCall { .. })
        ) {
            continue;
        }
        let callee = graph
            .graph
            .edges_directed(idx, Direction::Outgoing)
            .find(|e| matches!(e.weight(), IrEdge::Call))
            .map(|e| e.target());
        let ret = graph
            .graph
            .edges_directed(idx, Direction::Outgoing)
            .find(|e| matches!(e.weight(), IrEdge::Ret))
            .map(|e| e.target());
        if let (Some(callee_idx), Some(ret_idx)) = (callee, ret) {
            map.entry(callee_idx).or_default().push(ret_idx);
        }
    }
    map
}

// ─── Inverted cluster map ────────────────────────────────────────────────────

/// Inverts a cluster map, producing a lookup from [`NodeIndex`] to the name of
/// the label cluster that owns it.
///
/// The returned map borrows label names from `clusters` for zero-copy
/// efficiency.  If a node somehow appears in multiple clusters (which should
/// not happen in a well-formed graph) the last one wins.
pub fn node_to_cluster(clusters: &HashMap<String, HashSet<NodeIndex>>) -> HashMap<NodeIndex, &str> {
    clusters
        .iter()
        .flat_map(|(name, members)| members.iter().map(move |&idx| (idx, name.as_str())))
        .collect()
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use petgraph::Direction;

    use super::*;
    use crate::ir::{IrEdge, IrGraph, IrNodeKind};

    // ── graph-building helpers ────────────────────────────────────────────────

    /// Build a small graph:
    ///   N0(Nop) --Next--> N1(Nop) --Next--> N2(End)
    ///
    /// Returns `(graph, n0, n1, n2)`.
    fn nop_chain_graph() -> (IrGraph, NodeIndex, NodeIndex, NodeIndex) {
        let mut g = IrGraph::new();
        let n0 = g.push(IrNodeKind::Nop);
        let n1 = g.push(IrNodeKind::Nop);
        let n2 = g.push(IrNodeKind::End);
        g.add_edge(n0, n1, IrEdge::Next);
        g.add_edge(n1, n2, IrEdge::Next);
        g.entry = Some(n0);
        (g, n0, n1, n2)
    }

    /// Build a small graph:
    ///   N0(Assign x=0) --Next--> N1(End)
    ///   N2(Assign y=0)  [no incoming edges — unreachable]
    ///
    /// Returns `(graph, n0, n1, n2)`.
    fn two_node_graph_with_dead() -> (IrGraph, NodeIndex, NodeIndex, NodeIndex) {
        use crate::parser::ast::Ast;
        use crate::parser::ast::DeclKind;
        use crate::runtime::value::RuntimeValue;
        let dummy_expr = Ast::value(RuntimeValue::Int(0));
        let dummy_var = Ast::value(RuntimeValue::IdentPath(vec!["x".into()]));

        let mut g = IrGraph::new();
        let n0 = g.push(IrNodeKind::Assign {
            var: "x".into(),
            scope: DeclKind::Variable,
            expr: dummy_expr,
            fluent_alias: None,
        });
        let n1 = g.push(IrNodeKind::End);
        let n2 = g.push(IrNodeKind::Assign {
            var: "y".into(),
            scope: DeclKind::Variable,
            expr: dummy_var,
            fluent_alias: None,
        });
        g.add_edge(n0, n1, IrEdge::Next);
        // n2 has no incoming edges and is therefore unreachable from entry.
        g.entry = Some(n0);
        (g, n0, n1, n2)
    }

    // ── follow_nops ──────────────────────────────────────────────────────────

    #[test]
    fn follow_nops_skips_full_chain() {
        let (g, n0, _n1, n2) = nop_chain_graph();
        assert_eq!(
            follow_nops(&g, n0),
            Some(n2),
            "follow_nops from chain start must reach the terminal End node"
        );
    }

    #[test]
    fn follow_nops_returns_non_nop_unchanged() {
        let (g, _n0, _n1, n2) = nop_chain_graph();
        assert_eq!(
            follow_nops(&g, n2),
            Some(n2),
            "follow_nops on a non-Nop node must return that node unchanged"
        );
    }

    #[test]
    fn follow_nops_returns_none_when_chain_has_no_continuation() {
        // A standalone Nop with no outgoing edge — equivalent to the old
        // `follow_nops(&g, NODE_END) == NODE_END` test.  The absence of a Next
        // edge IS the sentinel in the new petgraph design.
        let mut g = IrGraph::new();
        let nop = g.push(IrNodeKind::Nop); // no Next edge added
        g.entry = Some(nop);
        assert_eq!(
            follow_nops(&g, nop),
            None,
            "Nop with no outgoing Next edge must return None"
        );
    }

    // ── reachable_nodes ──────────────────────────────────────────────────────

    #[test]
    fn reachable_includes_live_nodes() {
        let (g, n0, n1, _n2) = two_node_graph_with_dead();
        let r = reachable_nodes(&g);
        assert!(r.contains(&n0), "N0 is reachable from entry");
        assert!(r.contains(&n1), "N1 is reachable from N0 via Next edge");
    }

    #[test]
    fn reachable_excludes_dead_nodes() {
        let (g, _n0, _n1, n2) = two_node_graph_with_dead();
        let r = reachable_nodes(&g);
        assert!(
            !r.contains(&n2),
            "N2 has no path from entry and must be excluded"
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
        // After the LetCall, the continuation (Ret edge target) must remain in
        // the caller's cluster.
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

        // Find the LetCall node by iterating all node indices.
        let letcall_idx = g
            .graph
            .node_indices()
            .find(|&idx| matches!(g.graph.node_weight(idx), Some(IrNodeKind::LetCall { .. })));
        assert!(letcall_idx.is_some(), "must have a LetCall node");
        let letcall_idx = letcall_idx.unwrap();

        // The LetCall itself must be in the "start" cluster.
        let start_cluster = clusters.get("start").expect("start cluster must exist");
        assert!(
            start_cluster.contains(&letcall_idx),
            "LetCall node must be in the start cluster"
        );

        // The Ret continuation (let x = 1 assign) must also be in "start".
        let ret_target = g
            .graph
            .edges_directed(letcall_idx, Direction::Outgoing)
            .find(|e| matches!(e.weight(), IrEdge::Ret))
            .map(|e| e.target());
        assert!(ret_target.is_some(), "LetCall must have a Ret edge");
        let ret_idx = ret_target.unwrap();

        // Walk through any Nop mergepoints to the actual Assign node.
        let after_ret = follow_nops(&g, ret_idx).unwrap_or(ret_idx);

        // Verify the node after LetCall (possibly past Nops) is in "start".
        assert!(
            start_cluster.contains(&after_ret) || start_cluster.contains(&ret_idx),
            "node after LetCall must be in the same cluster as the LetCall (start)"
        );
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
            "callee_to_rets must have an entry for the helper's entry NodeIndex"
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

        // Every node that is in a cluster must resolve back to that cluster.
        for (name, members) in &clusters {
            for &idx in members {
                assert_eq!(
                    inv.get(&idx),
                    Some(&name.as_str()),
                    "node {idx:?} must map back to its owning cluster '{name}'"
                );
            }
        }
    }

    #[test]
    fn node_to_cluster_does_not_contain_unclustered_nodes() {
        // A bare assign with no label produces nodes outside any cluster.
        let g = compile(
            r#"
            let z = 99
            "#,
        );
        let r = reachable_nodes(&g);
        let clusters = compute_clusters(&g, &r);
        let inv = node_to_cluster(&clusters);
        // Since there are no labels, the inverted map must be empty.
        assert!(inv.is_empty(), "no labels → node_to_cluster must be empty");
    }
}
