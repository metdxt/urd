//! Mermaid flowchart renderer for [`super::IrGraph`].
//!
//! Produces a `flowchart LR` string renderable by any Mermaid-compatible
//! viewer (mermaid.live, GitHub/GitLab markdown, Obsidian, etc.) without
//! needing Graphviz installed.
//!
//! ## Visual language  (same semantics as the DOT renderer)
//! * Each `label` block → **subgraph cluster**
//! * Nop nodes collapsed, unreachable nodes excluded
//! * `return` inside a subroutine → dashed ↩ edge back to caller's continuation
//! * Colour legend: cyan=assign, yellow=branch, green=scope, salmon=dialogue,
//!   plum=choice, lavender=enum/decorator, rose=return, wheat=jump/letcall
//!
//! # Quick usage
//! ```no_run
//! # use urd::ir::IrGraph;
//! # fn example(graph: IrGraph) {
//! let mmd = graph.to_mermaid();
//! std::fs::write("script.mmd", mmd).unwrap();
//! // paste into https://mermaid.live or embed in markdown as ```mermaid
//! # }
//! ```

use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

use petgraph::Direction;
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;

use super::analysis::{self, follow_nops};
use super::render_common::{
    arm_pattern_label, ast_summary, decl_kw, decorator_line, extract_content_lines,
    extract_speakers, truncate,
};
use super::{IrEdge, IrGraph, IrNodeKind};

// ─── Public surface ───────────────────────────────────────────────────────────

impl IrGraph {
    /// Renders this graph as a Mermaid `flowchart LR` string.
    ///
    /// See the [module documentation][self] for usage and visual legend.
    pub fn to_mermaid(&self) -> String {
        render_mermaid(self)
    }
}

/// Renders `graph` as a Mermaid `flowchart LR` string.
///
/// Prefer calling [`IrGraph::to_mermaid`] instead of this free function.
pub fn render_mermaid(graph: &IrGraph) -> String {
    let mut out = String::new();
    let mut has_end_edge = false;

    // ── Analysis passes ──────────────────────────────────────────────────────
    let reachable = analysis::reachable_nodes(graph);
    let clusters = analysis::compute_clusters(graph, &reachable);
    let callee_to_rets = analysis::callee_to_rets(graph);
    let node_to_cluster = analysis::node_to_cluster(&clusters);
    let entry_cluster = analysis::entry_cluster_name(graph);

    // Sort cluster names by entry NodeIndex (ascending = compilation order).
    // When cluster_names is populated (multi-file) the canonical NodeIndex
    // lives there; fall back to the labels map for single-file graphs.
    let mut sorted_labels: Vec<&String> = clusters.keys().collect();
    sorted_labels.sort_by_key(|name| {
        let idx = graph
            .cluster_names
            .iter()
            .find(|(_, n)| n.as_str() == name.as_str())
            .map(|(&i, _)| i)
            .or_else(|| graph.labels.get(*name).copied());
        idx.map(|id| id.index()).unwrap_or(usize::MAX)
    });

    // Reverse map: label entry NodeIndex → label name.
    let label_by_entry: HashMap<NodeIndex, String> = graph
        .labels
        .iter()
        .map(|(name, &id)| (id, name.clone()))
        .collect();

    // ── Header ───────────────────────────────────────────────────────────────
    // The init directive locks the edge line colour to a visible dark grey so
    // arrows don't disappear against the pale subgraph backgrounds when the
    // viewer is in dark mode (mermaid.live, GitHub dark, Obsidian dark, …).
    writeln!(
        out,
        "%%{{init: {{'theme': 'base', \
         'themeVariables': {{'lineColor': '#ffffff', \
         'edgeLabelBackground': '#ffffff'}}}}}}%%"
    )
    .ok();
    writeln!(out, "flowchart LR").ok();
    writeln!(out).ok();

    // ── classDef declarations ────────────────────────────────────────────────
    writeln!(
        out,
        "    classDef assign fill:#E0FFFF,stroke:#888,color:#000"
    )
    .ok();
    writeln!(
        out,
        "    classDef dialogue fill:#FFA07A,stroke:#888,color:#000"
    )
    .ok();
    writeln!(
        out,
        "    classDef branch fill:#FFFFE0,stroke:#888,color:#000"
    )
    .ok();
    writeln!(
        out,
        "    classDef choice fill:#DDA0DD,stroke:#888,color:#000"
    )
    .ok();
    writeln!(out, "    classDef jump fill:#F5DEB3,stroke:#888,color:#000").ok();
    writeln!(
        out,
        "    classDef letcall fill:#F5DEB3,stroke:#888,color:#000"
    )
    .ok();
    writeln!(out, "    classDef ret fill:#FFE4E1,stroke:#888,color:#000").ok();
    writeln!(
        out,
        "    classDef scope fill:#90EE90,stroke:#228B22,color:#000"
    )
    .ok();
    writeln!(
        out,
        "    classDef enumDef fill:#E6E6FA,stroke:#888,color:#000"
    )
    .ok();
    writeln!(
        out,
        "    classDef decoratorDef fill:#E6E6FA,stroke:#888,color:#000"
    )
    .ok();
    writeln!(
        out,
        "    classDef endNode fill:#FF6347,stroke:#333,color:#fff"
    )
    .ok();
    writeln!(
        out,
        "    classDef startNode fill:#1b4332,stroke:#333,color:#fff"
    )
    .ok();
    writeln!(out).ok();

    // ── START node ───────────────────────────────────────────────────────────
    writeln!(out, "    __start__[/\"▶ START\"\\]:::startNode").ok();
    writeln!(out).ok();

    // ── Subgraph clusters (node definitions only) ────────────────────────────
    //
    // When the graph was compiled from multiple files, label clusters are
    // wrapped in outer file-boundary subgraphs — one per source path.
    // Single-file graphs leave label_sources empty; clusters are emitted flat.

    // Collect distinct source-file paths in sorted-label order.
    let file_order: Vec<String> = {
        let mut seen = std::collections::HashSet::new();
        let mut order = Vec::new();
        for label_name in &sorted_labels {
            if clusters[*label_name].is_empty() {
                continue;
            }
            let entry_idx = graph
                .cluster_names
                .iter()
                .find(|(_, n)| n.as_str() == label_name.as_str())
                .map(|(&i, _)| i)
                .or_else(|| graph.labels.get(*label_name).copied());
            let source = entry_idx
                .and_then(|i| graph.label_sources.get(&i))
                .cloned()
                .unwrap_or_default();
            if seen.insert(source.clone()) {
                order.push(source);
            }
        }
        order
    };

    // Resolve labels → source file for grouping.
    let labels_by_file: HashMap<String, Vec<&String>> = {
        let mut map: HashMap<String, Vec<&String>> = HashMap::new();
        for label_name in &sorted_labels {
            if clusters[*label_name].is_empty() {
                continue;
            }
            let entry_idx = graph
                .cluster_names
                .iter()
                .find(|(_, n)| n.as_str() == label_name.as_str())
                .map(|(&i, _)| i)
                .or_else(|| graph.labels.get(*label_name).copied());
            let source = entry_idx
                .and_then(|i| graph.label_sources.get(&i))
                .cloned()
                .unwrap_or_default();
            map.entry(source).or_default().push(label_name);
        }
        map
    };

    // Per-file border colours (root always green; imports cycle through a palette).
    let import_strokes = ["#2255aa", "#aa7700", "#882288", "#cc3322", "#117788"];
    let file_stroke = |path: &str| -> &'static str {
        if path.is_empty() {
            "#228B22"
        } else {
            // Position-weighted hash: multiply each byte by its 1-based index
            // so files whose bytes sum to the same value still hash differently
            // (e.g. "main.urd" and "tavern.urd" would collide with a plain sum).
            let h = path.bytes().enumerate().fold(0usize, |acc, (i, b)| {
                acc.wrapping_add((b as usize).wrapping_mul(i + 1))
            });
            import_strokes[h % import_strokes.len()]
        }
    };

    let multi_file = file_order.len() > 1;

    // Closure that emits the inner label subgraphs for one file's label list.
    let emit_label_subgraphs =
        |out: &mut String,
         labels_for_file: &[&String],
         indent: &str,
         graph: &IrGraph,
         clusters: &HashMap<String, HashSet<NodeIndex>>,
         label_by_entry: &HashMap<NodeIndex, String>| {
            for label_name in labels_for_file {
                let members = &clusters[*label_name];
                if members.is_empty() {
                    continue;
                }
                let safe = sanitize_id(label_name);
                let escaped_name = escape_mermaid(label_name);

                writeln!(out, "{indent}subgraph cluster_{safe}[\"{escaped_name}\"]").ok();
                writeln!(out, "{indent}    direction TB").ok();

                let mut sorted_members: Vec<NodeIndex> = members.iter().copied().collect();
                sorted_members.sort_by_key(|n| n.index());

                for node_idx in sorted_members {
                    let kind = match graph.graph.node_weight(node_idx) {
                        Some(k) => k,
                        None => continue,
                    };
                    if matches!(kind, IrNodeKind::Nop) {
                        continue;
                    }
                    let def = mermaid_node_def(node_idx, kind, graph, label_by_entry);
                    writeln!(out, "{indent}    {def}").ok();
                }

                writeln!(out, "{indent}end").ok();
                writeln!(out).ok();
            }
        };

    for source_path in &file_order {
        let labels_for_file = match labels_by_file.get(source_path) {
            Some(v) if !v.is_empty() => v.as_slice(),
            _ => continue,
        };

        if multi_file {
            // Outer file-boundary subgraph.
            let file_safe = sanitize_id(if source_path.is_empty() {
                "root"
            } else {
                source_path
            });
            let file_label = if source_path.is_empty() {
                "(root)".to_string()
            } else {
                source_path.clone()
            };
            writeln!(
                out,
                "    subgraph cluster_file_{file_safe}[\"{file_label}\"]"
            )
            .ok();
            writeln!(out, "        direction TB").ok();
            writeln!(out).ok();

            emit_label_subgraphs(
                &mut out,
                labels_for_file,
                "        ",
                graph,
                &clusters,
                &label_by_entry,
            );

            writeln!(out, "    end").ok();
            writeln!(out).ok();
        } else {
            emit_label_subgraphs(
                &mut out,
                labels_for_file,
                "    ",
                graph,
                &clusters,
                &label_by_entry,
            );
        }
    }

    // ── Subgraph style lines ─────────────────────────────────────────────────
    for label_name in &sorted_labels {
        let members = &clusters[*label_name];
        if members.is_empty() {
            continue;
        }
        let safe = sanitize_id(label_name);
        let is_entry = entry_cluster.as_deref() == Some(label_name.as_str());
        let sw = if is_entry { "4px" } else { "2px" };
        writeln!(
            out,
            "    style cluster_{safe} fill:transparent,stroke:#228B22,stroke-width:{sw}"
        )
        .ok();
    }
    // Style lines for outer file-boundary subgraphs.
    if multi_file {
        for source_path in &file_order {
            let file_safe = sanitize_id(if source_path.is_empty() {
                "root"
            } else {
                source_path
            });
            let stroke = file_stroke(source_path);
            writeln!(
                out,
                "    style cluster_file_{file_safe} fill:transparent,stroke:{stroke},stroke-width:3px,stroke-dasharray:6 3"
            )
            .ok();
        }
    }
    writeln!(out).ok();

    // ── Non-clustered reachable non-Nop node definitions (preamble) ─────────
    let clustered: HashSet<NodeIndex> = clusters.values().flatten().copied().collect();
    let mut preamble_lines: Vec<String> = Vec::new();
    let mut preamble_ids: Vec<NodeIndex> = Vec::new();

    let mut any_global = false;

    for node_idx in graph.graph.node_indices() {
        let kind = match graph.graph.node_weight(node_idx) {
            Some(k) => k,
            None => continue,
        };
        if !reachable.contains(&node_idx) {
            continue;
        }
        if matches!(kind, IrNodeKind::Nop) {
            continue;
        }
        if clustered.contains(&node_idx) {
            continue;
        }
        if is_preamble_kind(kind) {
            preamble_ids.push(node_idx);
            preamble_lines.push(preamble_summary(kind));
        } else {
            any_global = true;
            let def = mermaid_node_def(node_idx, kind, graph, &label_by_entry);
            writeln!(out, "    {def}").ok();
        }
    }
    if any_global {
        writeln!(out).ok();
    }

    if preamble_ids.len() < 2 {
        // Single (or zero) preamble node — render individually so its native
        // Mermaid class (:::enumDef, :::assign, …) is preserved.
        for &pid in &preamble_ids {
            let kind = match graph.graph.node_weight(pid) {
                Some(k) => k,
                None => continue,
            };
            let def = mermaid_node_def(pid, kind, graph, &label_by_entry);
            writeln!(out, "    {def}").ok();
        }
        if !preamble_ids.is_empty() {
            writeln!(out).ok();
        }
        // Treat them as normal nodes (don't skip their edges).
        preamble_ids.clear();
    } else {
        let body = preamble_lines
            .iter()
            .map(|l| escape_mermaid(l))
            .collect::<Vec<_>>()
            .join("<br/>");
        writeln!(out, "    __preamble__[\"{body}\"]:::assign").ok();
        writeln!(out).ok();
    }

    let preamble_id_set: HashSet<NodeIndex> = preamble_ids.iter().copied().collect();

    // ── START → entry edge ───────────────────────────────────────────────────
    if !preamble_ids.is_empty() {
        // START → preamble → entry label
        writeln!(out, "    __start__ --> __preamble__").ok();
        let preamble_target = preamble_chain_target(graph, graph.entry);
        match preamble_target {
            None => {
                has_end_edge = true;
                writeln!(out, "    __preamble__ --> __end__").ok();
            }
            Some(t) => {
                let dst = nid(t);
                writeln!(out, "    __preamble__ --> {dst}").ok();
            }
        }
    } else if let Some(entry_idx) = graph.entry {
        let entry_resolved = follow_nops(graph, entry_idx);
        match entry_resolved {
            None => {
                has_end_edge = true;
                writeln!(out, "    __start__ --> __end__").ok();
            }
            Some(t) => {
                let dst = nid(t);
                writeln!(out, "    __start__ --> {dst}").ok();
            }
        }
    }
    writeln!(out).ok();

    // ── All edges ────────────────────────────────────────────────────────────
    for node_idx in graph.graph.node_indices() {
        let kind = match graph.graph.node_weight(node_idx) {
            Some(k) => k,
            None => continue,
        };
        if !reachable.contains(&node_idx) {
            continue;
        }
        if matches!(kind, IrNodeKind::Nop) {
            continue;
        }
        if preamble_id_set.contains(&node_idx) {
            continue;
        }

        let n = nid(node_idx);

        match kind {
            // ── Single-successor nodes ─────────────────────────────────────
            IrNodeKind::Assign { .. }
            | IrNodeKind::Eval { .. }
            | IrNodeKind::EnterScope { .. }
            | IrNodeKind::ExitScope { .. }
            | IrNodeKind::DefineEnum { .. }
            | IrNodeKind::DefineScriptDecorator { .. }
            | IrNodeKind::Dialogue { .. } => {
                let next_raw = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Next))
                    .map(|e| e.target());
                let target = next_raw.and_then(|t| follow_nops(graph, t));
                write_mermaid_edge(&mut out, &n, target, None, false, &mut has_end_edge);
            }

            // ── Conditional branch ─────────────────────────────────────────
            IrNodeKind::Branch { .. } => {
                let mut then_raw: Option<NodeIndex> = None;
                let mut else_raw: Option<NodeIndex> = None;
                for e in graph.graph.edges_directed(node_idx, Direction::Outgoing) {
                    match e.weight() {
                        IrEdge::Then => then_raw = Some(e.target()),
                        IrEdge::Else => else_raw = Some(e.target()),
                        _ => {}
                    }
                }
                let then_t = then_raw.and_then(|t| follow_nops(graph, t));
                let else_t = else_raw.and_then(|t| follow_nops(graph, t));
                write_mermaid_edge(&mut out, &n, then_t, Some("then"), false, &mut has_end_edge);
                write_mermaid_edge(&mut out, &n, else_t, Some("else"), true, &mut has_end_edge);
            }

            // ── Multi-arm switch ───────────────────────────────────────────
            IrNodeKind::Switch { arms, .. } => {
                for (i, arm) in arms.iter().enumerate() {
                    let arm_raw = graph
                        .graph
                        .edges_directed(node_idx, Direction::Outgoing)
                        .find(|e| {
                            if let IrEdge::Arm(j) = e.weight() {
                                *j == i
                            } else {
                                false
                            }
                        })
                        .map(|e| e.target());
                    let t = arm_raw.and_then(|r| follow_nops(graph, r));
                    let lbl = arm_pattern_label(&arm.pattern);
                    write_mermaid_edge(&mut out, &n, t, Some(&lbl), false, &mut has_end_edge);
                }
                let default_raw = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Default))
                    .map(|e| e.target());
                if let Some(def_raw) = default_raw {
                    let t = follow_nops(graph, def_raw);
                    write_mermaid_edge(&mut out, &n, t, Some("default"), true, &mut has_end_edge);
                }
            }

            // ── Jump: dashed ───────────────────────────────────────────────
            IrNodeKind::Jump => {
                let target_raw = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Jump))
                    .map(|e| e.target());
                match target_raw {
                    None => {
                        has_end_edge = true;
                        writeln!(out, "    {n} -.-> __end__").ok();
                    }
                    Some(t) => {
                        let dst = nid(t);
                        writeln!(out, "    {n} -.-> {dst}").ok();
                    }
                }
            }

            // ── LetCall: dashed "call" edge + dashed "ret" edge ───────────
            IrNodeKind::LetCall { .. } => {
                let call_raw = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Call))
                    .map(|e| e.target());
                let ret_raw = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Ret))
                    .map(|e| e.target());

                // Dashed edge to the call target (subroutine entry).
                match call_raw {
                    None => {
                        has_end_edge = true;
                        writeln!(out, "    {n} -. \"call\" .-> __end__").ok();
                    }
                    Some(t) => {
                        let dst = nid(t);
                        writeln!(out, "    {n} -. \"call\" .-> {dst}").ok();
                    }
                }
                // Dashed edge to the return continuation.
                let ret_resolved = ret_raw.and_then(|r| follow_nops(graph, r));
                match ret_resolved {
                    None => {
                        has_end_edge = true;
                        writeln!(out, "    {n} -. \"ret\" .-> __end__").ok();
                    }
                    Some(t) => {
                        let ret_dst = nid(t);
                        writeln!(out, "    {n} -. \"ret\" .-> {ret_dst}").ok();
                    }
                }
            }

            // ── Choice fan-out ─────────────────────────────────────────────
            IrNodeKind::Choice { options, .. } => {
                for (i, opt) in options.iter().enumerate() {
                    let opt_raw = graph
                        .graph
                        .edges_directed(node_idx, Direction::Outgoing)
                        .find(|e| {
                            if let IrEdge::Option(j) = e.weight() {
                                *j == i
                            } else {
                                false
                            }
                        })
                        .map(|e| e.target());
                    let entry = opt_raw.and_then(|r| follow_nops(graph, r));
                    let lbl = format!("[{i}] {}", truncate(&opt.label, 24));
                    let escaped_lbl = escape_mermaid(&lbl);
                    let dst = match entry {
                        None => {
                            has_end_edge = true;
                            "__end__".to_string()
                        }
                        Some(t) => nid(t),
                    };
                    writeln!(out, "    {n} -->|\"{escaped_lbl}\"| {dst}").ok();
                }
            }

            // ── Return ─────────────────────────────────────────────────────
            // If this Return lives inside a subroutine label, draw dashed ↩
            // back-edges to each caller's ret continuation.  A bare top-level
            // return still goes to __end__.
            IrNodeKind::Return { .. } => {
                let caller_rets: Option<&Vec<NodeIndex>> = node_to_cluster
                    .get(&node_idx)
                    .and_then(|label_name| graph.labels.get(*label_name))
                    .and_then(|entry_id| callee_to_rets.get(entry_id));

                match caller_rets {
                    Some(ret_nodes) if !ret_nodes.is_empty() => {
                        for &ret in ret_nodes {
                            let resolved = follow_nops(graph, ret);
                            match resolved {
                                None => {
                                    has_end_edge = true;
                                    writeln!(out, "    {n} -. \"↩\" .-> __end__").ok();
                                }
                                Some(t) => {
                                    let dst = nid(t);
                                    writeln!(out, "    {n} -. \"↩\" .-> {dst}").ok();
                                }
                            }
                        }
                    }
                    _ => {
                        has_end_edge = true;
                        writeln!(out, "    {n} -.-> __end__").ok();
                    }
                }
            }

            // ── Terminals with no outgoing edges ───────────────────────────
            IrNodeKind::End | IrNodeKind::Todo => {}

            // Nop is skipped at the top of the loop.
            IrNodeKind::Nop => unreachable!("Nop should have been skipped"),
        }
    }

    // ── END sink ─────────────────────────────────────────────────────────────
    if has_end_edge {
        writeln!(out).ok();
        writeln!(out, "    __end__(((\"END\"))):::endNode").ok();
    }

    out
}

// ─── Node definition builder ──────────────────────────────────────────────────

/// Returns the full Mermaid node definition string for a single IR node.
///
/// The returned string includes the node ID, shape syntax, label text
/// (Mermaid-escaped), and `:::className` suffix.  It does **not** include
/// leading indentation.
fn mermaid_node_def(
    idx: NodeIndex,
    kind: &IrNodeKind,
    graph: &IrGraph,
    label_by_entry: &HashMap<NodeIndex, String>,
) -> String {
    let n = nid(idx);
    match kind {
        // ── Assign / Eval ──────────────────────────────────────────────────
        IrNodeKind::Assign { var, scope, .. } => {
            let text = escape_mermaid(&format!("{} {} = ⟨expr⟩", decl_kw(scope), var));
            format!("{n}[\"{text}\"]:::assign")
        }

        IrNodeKind::Eval { .. } => {
            format!("{n}[\"eval ⟨expr⟩\"]:::assign")
        }

        // ── Branch / Switch ────────────────────────────────────────────────
        IrNodeKind::Branch { condition, .. } => {
            let summary = truncate(&ast_summary(condition), 28);
            let text = escape_mermaid(&format!("branch<br/>{summary}"));
            format!("{n}{{\"{text}\"}}:::branch")
        }

        IrNodeKind::Switch { arms, .. } => {
            let has_default = graph
                .graph
                .edges_directed(idx, Direction::Outgoing)
                .any(|e| matches!(e.weight(), IrEdge::Default));
            let total = arms.len() + usize::from(has_default);
            let text = escape_mermaid(&format!("match ({total} arms)"));
            format!("{n}{{\"{text}\"}}:::branch")
        }

        // ── Jump ───────────────────────────────────────────────────────────
        IrNodeKind::Jump => {
            let target_idx = graph
                .graph
                .edges_directed(idx, Direction::Outgoing)
                .find(|e| matches!(e.weight(), IrEdge::Jump))
                .map(|e| e.target());
            let dest = target_idx
                .and_then(|t| label_by_entry.get(&t))
                .map(|name| format!("jump → {name}"))
                .unwrap_or_else(|| match target_idx {
                    Some(t) => format!("jump → {}", nid(t)),
                    None => "jump → ∅".to_string(),
                });
            let text = escape_mermaid(&dest);
            format!("{n}>\"{text}\"]:::jump")
        }

        // ── LetCall ────────────────────────────────────────────────────────
        IrNodeKind::LetCall { var } => {
            let callee_idx = graph
                .graph
                .edges_directed(idx, Direction::Outgoing)
                .find(|e| matches!(e.weight(), IrEdge::Call))
                .map(|e| e.target());
            let callee = callee_idx
                .and_then(|t| label_by_entry.get(&t))
                .cloned()
                .unwrap_or_else(|| match callee_idx {
                    Some(t) => nid(t),
                    None => "∅".to_string(),
                });
            let raw = if var.is_empty() {
                format!("⤑ {callee}")
            } else {
                format!("⤑ {callee}<br/>→ {var}")
            };
            let text = escape_mermaid(&raw);
            format!("{n}>\"{text}\"]:::letcall")
        }

        // ── Return ─────────────────────────────────────────────────────────
        IrNodeKind::Return { value } => {
            let label_text = if value.is_some() {
                "return ⟨expr⟩"
            } else {
                "return"
            };
            let text = escape_mermaid(label_text);
            format!("{n}([\"  {text}  \"]):::ret")
        }

        // ── EnterScope / ExitScope ─────────────────────────────────────────
        IrNodeKind::EnterScope { label } => {
            let text = escape_mermaid(&format!("▶ {label}"));
            format!("{n}{{\"{text}\"}}:::scope")
        }

        IrNodeKind::ExitScope { label } => {
            let text = escape_mermaid(&format!("◀ {label}"));
            format!("{n}{{\"{text}\"}}:::scope")
        }

        // ── DefineEnum ─────────────────────────────────────────────────────
        IrNodeKind::DefineEnum { name, variants, .. } => {
            let text = escape_mermaid(&format!("enum {name} ({} variants)", variants.len()));
            format!("{n}[\"{text}\"]:::enumDef")
        }

        // ── DefineScriptDecorator ──────────────────────────────────────────
        IrNodeKind::DefineScriptDecorator { name, params, .. } => {
            let raw = format!("def_decorator<br/>@{name} ({} params)", params.len());
            let text = escape_mermaid(&raw);
            format!("{n}[\"{text}\"]:::decoratorDef")
        }

        // ── Nop ────────────────────────────────────────────────────────────
        IrNodeKind::Nop => {
            // Should never be reached — Nop nodes are filtered before calling
            // this function.  Emit a visible placeholder so bugs are obvious.
            format!("{n}[\"● {}\"]", idx.index())
        }

        // ── End ────────────────────────────────────────────────────────────
        IrNodeKind::End => {
            format!("{n}(((\"end\"))):::endNode")
        }

        // ── Todo ───────────────────────────────────────────────────────────
        IrNodeKind::Todo => {
            format!("{n}(((\"todo!\"))  ):::endNode")
        }

        // ── Dialogue ──────────────────────────────────────────────────────
        IrNodeKind::Dialogue {
            speakers,
            lines,
            decorators,
            ..
        } => {
            let speaker_str = extract_speakers(speakers);
            let line_texts = extract_content_lines(lines);
            let decs = decorator_line(decorators);

            let mut parts: Vec<String> = Vec::new();
            if !speaker_str.is_empty() {
                parts.push(format!("{speaker_str}:"));
            }
            for line in line_texts.iter().take(2) {
                parts.push(format!("\"{}\"", truncate(line, 34)));
            }
            if line_texts.len() > 2 {
                parts.push(format!("(+{} more)", line_texts.len() - 2));
            }
            if !decs.is_empty() {
                parts.push(decs);
            }

            let raw = parts.join("<br/>");
            let text = escape_mermaid(&raw);
            format!("{n}[\"{text}\"]:::dialogue")
        }

        // ── Choice ─────────────────────────────────────────────────────────
        IrNodeKind::Choice {
            options,
            decorators,
            ..
        } => {
            let decs = decorator_line(decorators);
            let raw = if decs.is_empty() {
                format!("choice ({} opts)", options.len())
            } else {
                format!("choice ({} opts)<br/>{decs}", options.len())
            };
            let text = escape_mermaid(&raw);
            format!("{n}{{{{\"  {text}  \"}}}}:::choice")
        }
    }
}

// ─── Node ID formatting ──────────────────────────────────────────────────────

/// Formats a [`NodeIndex`] as a renderer node identifier string (e.g. `N42`).
fn nid(idx: NodeIndex) -> String {
    format!("N{}", idx.index())
}

// ─── Edge helper ─────────────────────────────────────────────────────────────

/// Emits a single Mermaid edge from `src` to `dst`.
///
/// `dashed` uses `-.->` syntax; otherwise uses `-->`.
/// If `dst` is `None` the edge targets `__end__` and sets `has_end_edge`.
fn write_mermaid_edge(
    out: &mut String,
    src: &str,
    dst: Option<NodeIndex>,
    label: Option<&str>,
    dashed: bool,
    has_end_edge: &mut bool,
) {
    let dst_str = match dst {
        None => {
            *has_end_edge = true;
            "__end__".to_string()
        }
        Some(idx) => nid(idx),
    };

    match (label, dashed) {
        (Some(lbl), false) => {
            let escaped = escape_mermaid(lbl);
            writeln!(out, "    {src} -->|\"{escaped}\"| {dst_str}").ok();
        }
        (Some(lbl), true) => {
            let escaped = escape_mermaid(lbl);
            writeln!(out, "    {src} -. \"{escaped}\" .-> {dst_str}").ok();
        }
        (None, false) => {
            writeln!(out, "    {src} --> {dst_str}").ok();
        }
        (None, true) => {
            writeln!(out, "    {src} -.-> {dst_str}").ok();
        }
    }
}

// ─── Preamble helpers ─────────────────────────────────────────────────────────

/// Returns `true` for node kinds that should be collapsed into the preamble
/// summary node (Assign, DefineEnum, DefineScriptDecorator, Eval).
fn is_preamble_kind(kind: &IrNodeKind) -> bool {
    matches!(
        kind,
        IrNodeKind::Assign { .. }
            | IrNodeKind::DefineEnum { .. }
            | IrNodeKind::DefineScriptDecorator { .. }
            | IrNodeKind::Eval { .. }
    )
}

/// Returns a short one-line summary for a preamble (non-clustered) IR node.
fn preamble_summary(kind: &IrNodeKind) -> String {
    match kind {
        IrNodeKind::Assign { var, scope, .. } => {
            format!("{} {var}", decl_kw(scope))
        }
        IrNodeKind::DefineEnum { name, .. } => {
            format!("enum {name}")
        }
        IrNodeKind::DefineScriptDecorator { name, .. } => {
            format!("decorator {name}")
        }
        IrNodeKind::Eval { .. } => "⟨eval⟩".to_string(),
        _ => "⟨init⟩".to_string(),
    }
}

/// Walks from `cursor` following preamble-style nodes (Assign, DefineEnum,
/// DefineScriptDecorator, Nop, Eval) via their Next edges, until hitting an
/// EnterScope, `None`, or any other non-preamble node.  Returns that target
/// `Option<NodeIndex>`.
fn preamble_chain_target(graph: &IrGraph, cursor: Option<NodeIndex>) -> Option<NodeIndex> {
    let mut current = cursor?;
    let mut visited: HashSet<NodeIndex> = HashSet::new();
    loop {
        if !visited.insert(current) {
            return Some(current);
        }
        let kind = match graph.graph.node_weight(current) {
            Some(k) => k,
            None => return None,
        };
        match kind {
            IrNodeKind::Assign { .. }
            | IrNodeKind::DefineEnum { .. }
            | IrNodeKind::DefineScriptDecorator { .. }
            | IrNodeKind::Nop
            | IrNodeKind::Eval { .. } => {
                // Follow the Next edge.
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
            _ => return Some(current),
        }
    }
}

// ─── Mermaid-specific helpers ─────────────────────────────────────────────────

/// Escapes text for use inside a Mermaid double-quoted node label.
///
/// Mermaid uses HTML entity escapes inside quoted labels.
/// Note: `<br/>` sequences that were intentionally embedded are preserved.
fn escape_mermaid(s: &str) -> String {
    // We need to handle <br/> specially — it's intentional Mermaid markup.
    // Strategy: split on <br/>, escape each piece, then rejoin with <br/>.
    let parts: Vec<String> = s
        .split("<br/>")
        .map(|piece| {
            piece
                .replace('&', "#amp;")
                .replace('"', "#quot;")
                .replace('<', "#lt;")
                .replace('>', "#gt;")
                .replace('\n', "<br/>")
                .replace('\r', "")
        })
        .collect();
    parts.join("<br/>")
}

/// Converts an arbitrary string into a valid Mermaid subgraph/node ID segment
/// by replacing `::`, `.`, ` `, `-` with `_` and keeping alphanumeric + `_`.
///
/// NOTE: The DOT renderer has its own `sanitize_id` that skips the initial
/// `::` / `.` / ` ` / `-` normalisation pass.
fn sanitize_id(s: &str) -> String {
    let s = s.replace("::", "_").replace(['.', ' ', '-'], "_");
    s.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)]
    #![allow(clippy::unwrap_used)]

    use super::*;
    use crate::compiler::Compiler;
    use crate::lexer::strings::ParsedString;
    use crate::parser::ast::{Ast, DeclKind};
    use crate::runtime::value::RuntimeValue;

    fn compile(ast: Ast) -> IrGraph {
        Compiler::compile(&ast).expect("compile failed in test")
    }

    // ── Header ───────────────────────────────────────────────────────────────

    #[test]
    fn test_mermaid_header() {
        let mmd = compile(Ast::block(vec![])).to_mermaid();
        assert!(
            mmd.contains("flowchart LR"),
            "must contain flowchart LR, got: {mmd}"
        );
        assert!(
            mmd.contains("%%{init:"),
            "must contain %%{{init:}} directive for edge colour, got: {mmd}"
        );
        assert!(
            mmd.contains("lineColor"),
            "init directive must set lineColor, got: {mmd}"
        );
    }

    #[test]
    fn test_mermaid_has_classdefs() {
        let mmd = compile(Ast::block(vec![])).to_mermaid();
        assert!(mmd.contains("classDef assign"), "must have assign classDef");
        assert!(
            mmd.contains("classDef dialogue"),
            "must have dialogue classDef"
        );
        assert!(mmd.contains("classDef branch"), "must have branch classDef");
        assert!(mmd.contains("classDef choice"), "must have choice classDef");
        assert!(mmd.contains("classDef ret"), "must have ret classDef");
    }

    #[test]
    fn test_mermaid_has_start_node() {
        let mmd = compile(Ast::block(vec![Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::Int(1)),
        )]))
        .to_mermaid();
        assert!(mmd.contains("__start__"), "must contain __start__ node");
        assert!(mmd.contains("▶ START"), "must contain ▶ START label");
    }

    // ── Clusters ─────────────────────────────────────────────────────────────

    #[test]
    fn test_mermaid_labeled_block_produces_subgraph() {
        let mmd = compile(Ast::block(vec![Ast::labeled_block(
            "intro".into(),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                Ast::value(RuntimeValue::Int(1)),
            )]),
        )]))
        .to_mermaid();
        assert!(
            mmd.contains("subgraph cluster_intro"),
            "label must produce subgraph"
        );
    }

    #[test]
    fn test_mermaid_enter_scope_marker_in_subgraph() {
        let mmd = compile(Ast::block(vec![Ast::labeled_block(
            "act1".into(),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                Ast::value(RuntimeValue::Int(1)),
            )]),
        )]))
        .to_mermaid();
        assert!(mmd.contains("▶ act1"), "must contain EnterScope marker");
    }

    // ── Node kinds ───────────────────────────────────────────────────────────

    #[test]
    fn test_mermaid_assign_node() {
        let mmd = compile(Ast::block(vec![Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["health".into()])),
            Ast::value(RuntimeValue::Int(100)),
        )]))
        .to_mermaid();
        assert!(mmd.contains("let health"), "assign node must show var name");
        assert!(
            mmd.contains(":::assign"),
            "assign node must have assign class"
        );
    }

    #[test]
    fn test_mermaid_dialogue_node() {
        let mmd = compile(Ast::block(vec![Ast::dialogue(
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "Elara".into(),
            ]))]),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain(
                "Welcome, traveller.",
            ))),
        )]))
        .to_mermaid();
        assert!(mmd.contains("Elara"), "dialogue must show speaker");
        assert!(
            mmd.contains("Welcome, traveller."),
            "dialogue must show text"
        );
        assert!(
            mmd.contains(":::dialogue"),
            "dialogue node must have dialogue class"
        );
    }

    #[test]
    fn test_mermaid_branch_node() {
        let mmd = compile(Ast::block(vec![Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["a".into()])),
                Ast::value(RuntimeValue::Int(1)),
            )]),
            None,
        )]))
        .to_mermaid();
        assert!(mmd.contains("branch"), "must have branch node");
        assert!(
            mmd.contains(":::branch"),
            "branch node must have branch class"
        );
        assert!(mmd.contains("then"), "must have then edge");
        assert!(mmd.contains("else"), "must have else edge");
    }

    #[test]
    fn test_mermaid_choice_node() {
        let mmd = compile(Ast::block(vec![Ast::menu(vec![
            Ast::menu_option(
                "Yes".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["a".into()])),
                    Ast::value(RuntimeValue::Int(1)),
                )]),
            ),
            Ast::menu_option(
                "No".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["b".into()])),
                    Ast::value(RuntimeValue::Int(2)),
                )]),
            ),
        ])]))
        .to_mermaid();
        assert!(mmd.contains("choice"), "must have choice node");
        assert!(
            mmd.contains(":::choice"),
            "choice node must have choice class"
        );
        assert!(mmd.contains("Yes"), "must label option Yes");
        assert!(mmd.contains("No"), "must label option No");
    }

    #[test]
    fn test_mermaid_return_node() {
        let mmd = compile(Ast::block(vec![Ast::labeled_block(
            "s".into(),
            Ast::block(vec![Ast::return_stmt(None)]),
        )]))
        .to_mermaid();
        assert!(mmd.contains("return"), "must have return node");
        assert!(mmd.contains(":::ret"), "return node must have ret class");
    }

    #[test]
    fn test_mermaid_jump_node() {
        let mmd = compile(Ast::block(vec![
            Ast::labeled_block(
                "target".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                    Ast::value(RuntimeValue::Int(0)),
                )]),
            ),
            Ast::jump_stmt("target".into(), false),
        ]))
        .to_mermaid();
        assert!(mmd.contains("jump"), "must have jump node");
        assert!(mmd.contains(":::jump"), "jump node must have jump class");
        assert!(mmd.contains("-.->"), "jump edge must be dashed");
    }

    #[test]
    fn test_mermaid_end_sink() {
        let mmd = compile(Ast::block(vec![Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::Int(1)),
        )]))
        .to_mermaid();
        assert!(mmd.contains("__end__"), "must contain __end__ sink");
        assert!(
            mmd.contains(":::endNode"),
            "__end__ must have endNode class"
        );
    }

    #[test]
    fn test_mermaid_define_enum_node() {
        let mmd = compile(Ast::block(vec![Ast::enum_decl(
            "Dir".into(),
            vec!["N".into(), "S".into()],
        )]))
        .to_mermaid();
        assert!(mmd.contains("enum"), "must contain DefineEnum node");
        assert!(
            mmd.contains(":::enumDef"),
            "enum node must have enumDef class"
        );
    }

    // ── Escape helpers ────────────────────────────────────────────────────────

    #[test]
    fn test_escape_mermaid_quotes() {
        let s = escape_mermaid(r#"say "hi""#);
        assert!(!s.contains('"'), "double quotes must be escaped");
        assert!(s.contains("#quot;"), "must use #quot; entity");
    }

    #[test]
    fn test_escape_mermaid_angle_brackets() {
        let s = escape_mermaid("a < b > c");
        assert!(!s.contains('<') || s.contains("#lt;"), "< must be escaped");
        assert!(!s.contains('>') || s.contains("#gt;"), "> must be escaped");
    }

    #[test]
    fn test_escape_mermaid_preserves_br_tags() {
        // <br/> sequences used for multi-line labels must be preserved.
        let raw = "line1<br/>line2";
        let escaped = escape_mermaid(raw);
        assert!(
            escaped.contains("<br/>"),
            "escape_mermaid must preserve intentional <br/> sequences"
        );
    }

    #[test]
    fn test_escape_mermaid_newline_becomes_br() {
        let s = escape_mermaid("line1\nline2");
        assert!(!s.contains('\n'), "newlines must be converted");
        assert!(s.contains("<br/>"), "newlines must become <br/>");
    }

    #[test]
    fn test_sanitize_id_replaces_specials() {
        assert_eq!(sanitize_id("my-label"), "my_label");
        assert_eq!(sanitize_id("scene.1"), "scene_1");
        assert_eq!(sanitize_id("a::b"), "a_b");
        assert_eq!(sanitize_id("valid_name"), "valid_name");
    }

    #[test]
    fn test_truncate_short_unchanged() {
        use super::super::render_common::truncate;
        assert_eq!(truncate("hello", 10), "hello");
    }

    #[test]
    fn test_truncate_long_appends_ellipsis() {
        use super::super::render_common::truncate;
        let r = truncate("abcdefghijklmnop", 5);
        assert!(r.ends_with('…'));
        assert!(r.chars().count() <= 6);
    }

    // ── Subroutine return routing ─────────────────────────────────────────────

    #[test]
    fn test_mermaid_subroutine_return_emits_back_edge() {
        use crate::{parse_test, parser::block::script};
        let src = r#"
            label start {
                let r = jump helper and return
            }
            label helper {
                return "ok"
            }
        "#;
        let ast = parse_test!(script(), src).expect("parse failed");
        let graph = Compiler::compile(&ast).expect("compile failed");
        let mmd = graph.to_mermaid();
        assert!(
            mmd.contains("↩"),
            "subroutine Return must emit a ↩ back-edge to caller continuation"
        );
    }

    #[test]
    fn test_mermaid_letcall_has_call_and_ret_edges() {
        use crate::{parse_test, parser::block::script};
        let src = r#"
            label start {
                let r = jump helper and return
            }
            label helper {
                return "ok"
            }
        "#;
        let ast = parse_test!(script(), src).expect("parse failed");
        let graph = Compiler::compile(&ast).expect("compile failed");
        let mmd = graph.to_mermaid();
        assert!(mmd.contains("\"call\""), "LetCall must emit a 'call' edge");
        assert!(mmd.contains("\"ret\""), "LetCall must emit a 'ret' edge");
    }

    // ── LetCall.next cluster fix ──────────────────────────────────────────────

    #[test]
    fn test_nodes_after_letcall_are_in_same_cluster() {
        use crate::{parse_test, parser::block::script};
        let src = r#"
            label start {
                let r = jump helper and return
                let x = 1
            }
            label helper {
                return "ok"
            }
        "#;
        let ast = parse_test!(script(), src).expect("parse failed");
        let graph = Compiler::compile(&ast).expect("compile failed");
        let mmd = graph.to_mermaid();

        // Find the start subgraph block (between "subgraph cluster_start" and the matching "end").
        let start_pos = mmd
            .find("subgraph cluster_start")
            .expect("cluster_start must exist");
        // Find the "end" that closes this subgraph (first "end" after the subgraph open).
        let end_pos = mmd[start_pos..]
            .find("\n    end")
            .expect("closing end of cluster_start must exist")
            + start_pos;
        let cluster_block = &mmd[start_pos..end_pos];

        // The LetCall node (:::letcall) must be inside cluster_start.
        assert!(
            cluster_block.contains(":::letcall"),
            "LetCall node must be inside cluster_start block"
        );
        // The node after the LetCall (let x = 1) must also be inside cluster_start.
        assert!(
            cluster_block.contains("let x"),
            "node after LetCall (let x=1) must be inside cluster_start block"
        );
    }
}
