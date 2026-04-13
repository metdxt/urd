//! DOT graph renderer for [`super::IrGraph`].
//!
//! Produces a Graphviz DOT string that can be piped straight to `dot -Tsvg`
//! or pasted into an online viewer such as
//! <https://dreampuf.github.io/GraphvizOnline/>.
//!
//! ## Visual language
//!
//! * Each `label` block is rendered as a **`subgraph cluster`** — a rounded,
//!   pale-green box containing all nodes compiled from that block.  The first
//!   label (by compilation order / `NodeIndex`) gets a thicker border to
//!   signal it is the script's entry scope.
//! * `Nop` (compiler merge-point) nodes are **collapsed**: every edge that
//!   would have pointed to a `Nop` is re-routed through the Nop chain to the
//!   first real successor.  `Nop` nodes are never rendered.
//! * **Unreachable** nodes (dead `ExitScope` nodes left over when all paths
//!   use `jump`/`return`) are silently dropped.
//! * `jump` edges carry `constraint=false` so back-edges do not distort the
//!   layout ranking, and point directly at the target `EnterScope` node.
//!   (`compound=true` / `lhead` are intentionally omitted — they conflict
//!   with `splines=ortho` and force Graphviz into bezier routing.)
//! * `return` inside a subroutine label (targeted by a `LetCall` node) emits
//!   dashed `↩` back-edges to each caller's ret continuation rather than the
//!   `__end__` sink, making the real control flow visible.  A bare top-level
//!   `return` (one whose owning label is never targeted by a `LetCall`) still
//!   goes to `__end__`.
//! * `dialogue` nodes display the actual speaker(s) and line text extracted
//!   from the AST at render time (best-effort; falls back to `⟨expr⟩`).
//! * Colour legend: cyan = assign/eval, yellow = branch/match,
//!   green = scope marker, slate = dialogue, violet = choice,
//!   lavender = enum, rose = return, wheat = jump, amber = branch/switch.
//!
//! # Quick usage
//!
//! ```no_run
//! # use urd::ir::IrGraph;
//! # fn example(graph: IrGraph) {
//! let dot = graph.to_dot();
//! std::fs::write("script.dot", dot).expect("could not write DOT file");
//! // then: dot -Tsvg script.dot -o script.svg
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
    extract_speakers, is_preamble_kind, preamble_chain_target, preamble_summary, truncate,
};
use super::{IrEdge, IrGraph, IrNodeKind};

// ─── Public surface ──────────────────────────────────────────────────────────

impl IrGraph {
    /// Renders this graph as a Graphviz DOT string.
    ///
    /// See the [module documentation][self] for usage and visual legend.
    pub fn to_dot(&self) -> String {
        render_dot(self)
    }
}

/// Renders `graph` as a Graphviz DOT string.
///
/// Prefer calling [`IrGraph::to_dot`] instead of this free function.
pub fn render_dot(graph: &IrGraph) -> String {
    let mut out = String::new();
    let mut has_end_edge = false;

    // ── Analysis passes ──────────────────────────────────────────────────────
    let reachable = analysis::reachable_nodes(graph);
    let clusters = analysis::compute_clusters(graph, &reachable);

    // Sort cluster names by entry NodeIndex (ascending = compilation order).
    // This encourages Graphviz to place the first-written label at the top.
    // When cluster_names is populated the canonical entry NodeIndex lives there;
    // otherwise fall back to the labels map (single-file graphs).
    let mut sorted_labels: Vec<&String> = clusters.keys().collect();
    sorted_labels.sort_by_key(|name| {
        // Prefer cluster_names (canonical, one entry per node); fall back to labels.
        let idx = graph
            .cluster_names
            .iter()
            .find(|(_, n)| n.as_str() == name.as_str())
            .map(|(&idx, _)| idx)
            .or_else(|| graph.labels.get(*name).copied());
        idx.map(|id| id.index()).unwrap_or(usize::MAX)
    });

    // The entry cluster is the one whose entry NodeIndex is reached first from
    // graph.entry along the linear prologue (assignments, enums, etc.).
    let entry_cluster = analysis::entry_cluster_name(graph);

    // Reverse map: label entry NodeIndex → label name, used to display
    // human-readable jump targets instead of raw node indices.
    let label_by_entry: HashMap<NodeIndex, String> = graph
        .labels
        .iter()
        .map(|(name, &id)| (id, name.clone()))
        .collect();

    // For every LetCall site, record: callee entry NodeIndex → list of ret
    // continuation NodeIndexes.  This lets Return nodes inside subroutines
    // draw back-edges to their actual caller continuations instead of the
    // misleading __end__ sink.
    let callee_to_rets = analysis::callee_to_rets(graph);

    // Inverted cluster map: NodeIndex → owning label name.  Used when routing
    // Return edges so we know which subroutine a given Return node belongs to.
    let node_to_cluster = analysis::node_to_cluster(&clusters);

    // ── DOT header ───────────────────────────────────────────────────────────
    writeln!(out, "digraph urd_script {{").ok();
    writeln!(out, "    rankdir=TB;").ok();
    writeln!(out, "    splines=ortho;").ok();
    writeln!(out, "    node [fontname=\"monospace\", fontsize=11];").ok();
    writeln!(out, "    edge [fontname=\"monospace\", fontsize=9];").ok();
    writeln!(out).ok();

    // ── START node ───────────────────────────────────────────────────────────
    writeln!(
        out,
        "    __start__ [label=\"▶ START\", shape=invhouse, \
         style=\"filled,bold\", fillcolor=\"#1b4332\", \
         fontcolor=white, fontsize=12, margin=\"0.15,0.1\"];"
    )
    .ok();

    writeln!(out).ok();

    // ── Cluster subgraphs (node definitions only, no edges) ──────────────────
    //
    // When the graph was compiled from multiple files, labels are grouped into
    // outer file-boundary subgraphs (one per source path).  Each file boundary
    // wraps the label clusters that belong to it with a dashed, coloured border
    // so the viewer can immediately see which file owns which scene.
    //
    // Single-file graphs leave label_sources empty; the outer grouping is
    // skipped and clusters are emitted flat, exactly as before.
    //
    // Nodes are *defined* inside their subgraph so Graphviz places them there.
    // Edges are emitted after all subgraphs (idiomatic DOT).

    // Collect the distinct source-file paths in a stable order.
    let file_order: Vec<String> = {
        let mut seen = std::collections::HashSet::new();
        let mut order = Vec::new();
        for label_name in &sorted_labels {
            let clusters_entry = &clusters[*label_name];
            if clusters_entry.is_empty() {
                continue;
            }
            // Resolve this cluster's canonical NodeIndex.
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

    // Assign a distinct fill colour per file.  The root ("") is always green;
    // each imported file gets a shade from a small fixed palette.
    let import_fills = ["#e8f4ff", "#fff8e8", "#f8e8ff", "#ffe8e8", "#e8f8ff"];
    let import_borders = ["#2255aa", "#aa7700", "#882288", "#cc3322", "#117788"];
    // Position-weighted hash: multiply each byte by its 1-based index so files
    // whose bytes sum to the same value still hash differently
    // (e.g. "main.urd" and "tavern.urd" would collide with a plain sum).
    let file_hash = |path: &str| -> usize {
        path.bytes().enumerate().fold(0usize, |acc, (i, b)| {
            acc.wrapping_add((b as usize).wrapping_mul(i + 1))
        })
    };
    let file_fill = |path: &str| -> &'static str {
        if path.is_empty() {
            "#f0fff4"
        } else {
            import_fills[file_hash(path) % import_fills.len()]
        }
    };
    let file_border = |path: &str| -> &'static str {
        if path.is_empty() {
            "darkgreen"
        } else {
            import_borders[file_hash(path) % import_borders.len()]
        }
    };

    let emit_clusters = |out: &mut String,
                         labels_for_file: &[&String],
                         indent: &str,
                         graph: &IrGraph,
                         clusters: &HashMap<String, HashSet<NodeIndex>>,
                         entry_cluster: &Option<String>,
                         label_by_entry: &HashMap<NodeIndex, String>| {
        for label_name in labels_for_file {
            let members = &clusters[*label_name];
            if members.is_empty() {
                continue;
            }
            let safe = sanitize_id(label_name);
            let is_entry = entry_cluster.as_deref() == Some(label_name.as_str());
            let has_todo = members
                .iter()
                .any(|&idx| matches!(graph.graph.node_weight(idx), Some(IrNodeKind::Todo)));
            let penwidth = if is_entry {
                "3.0"
            } else if has_todo {
                "2.5"
            } else {
                "1.5"
            };
            let cluster_fill = if has_todo { "#ffe0b2" } else { "#f0fff4" };
            let cluster_color = if has_todo { "darkorange" } else { "darkgreen" };

            writeln!(out, "{indent}subgraph cluster_{safe} {{").ok();
            writeln!(out, "{indent}    label={};", quoted(label_name)).ok();
            writeln!(out, "{indent}    style=\"rounded,filled\";").ok();
            writeln!(out, "{indent}    fillcolor={};", quoted(cluster_fill)).ok();
            writeln!(out, "{indent}    color={cluster_color};").ok();
            writeln!(out, "{indent}    penwidth={penwidth};").ok();
            writeln!(out).ok();

            let mut sorted_members: Vec<NodeIndex> = members.iter().copied().collect();
            sorted_members.sort_by_key(|n| n.index());

            for node_idx in sorted_members {
                let kind = match graph.graph.node_weight(node_idx) {
                    Some(k) => k,
                    None => continue,
                };
                let (shape, fill, label) = node_attrs(node_idx, kind, graph, label_by_entry);
                writeln!(
                    out,
                    "{indent}    {} [label={}, shape={shape}, style=filled, fillcolor={}];",
                    nid(node_idx),
                    quoted(&label),
                    quoted(fill),
                )
                .ok();
            }

            writeln!(out, "{indent}}}").ok();
            writeln!(out).ok();
        }
    };

    // Resolve which labels belong to each source file.
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

    let multi_file = file_order.len() > 1;

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
            let fill = file_fill(source_path);
            let border = file_border(source_path);
            writeln!(out, "    subgraph cluster_file_{file_safe} {{").ok();
            writeln!(out, "        label={};", quoted(&file_label)).ok();
            writeln!(out, "        style=\"dashed,filled\";").ok();
            writeln!(out, "        fillcolor={};", quoted(fill)).ok();
            writeln!(out, "        color={border};").ok();
            writeln!(out, "        penwidth=2.0;").ok();
            writeln!(out, "        fontsize=13;").ok();
            writeln!(out, "        fontname=\"monospace bold\";").ok();
            writeln!(out).ok();

            emit_clusters(
                &mut out,
                labels_for_file,
                "        ",
                graph,
                &clusters,
                &entry_cluster,
                &label_by_entry,
            );

            writeln!(out, "    }}").ok();
            writeln!(out).ok();
        } else {
            emit_clusters(
                &mut out,
                labels_for_file,
                "    ",
                graph,
                &clusters,
                &entry_cluster,
                &label_by_entry,
            );
        }
    }

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
            let (shape, fill, label) = node_attrs(node_idx, kind, graph, &label_by_entry);
            writeln!(
                out,
                "    {} [label={}, shape={shape}, style=filled, fillcolor={}];",
                nid(node_idx),
                quoted(&label),
                quoted(fill),
            )
            .ok();
        }
    }
    if any_global {
        writeln!(out).ok();
    }

    if !preamble_lines.is_empty() {
        let label_text = preamble_lines.join("\\n");
        writeln!(
            out,
            "    __preamble__ [label={}, shape=note, style=filled, fillcolor=\"lavender\"];",
            quoted(&label_text),
        )
        .ok();
        writeln!(out).ok();
    }

    // ── START edge ───────────────────────────────────────────────────────────
    if !preamble_ids.is_empty() {
        writeln!(out, "    __start__ -> __preamble__;").ok();
        let target = preamble_chain_target(graph, graph.entry);
        match target {
            None => {
                has_end_edge = true;
                writeln!(out, "    __preamble__ -> __end__;").ok();
            }
            Some(t) => {
                writeln!(out, "    __preamble__ -> {};", nid(t)).ok();
            }
        }
    } else if let Some(entry_idx) = graph.entry {
        writeln!(out, "    __start__ -> {};", nid(entry_idx)).ok();
    }
    writeln!(out).ok();

    // ── All edges ────────────────────────────────────────────────────────────
    //
    // Emitted outside all subgraphs so Graphviz draws cross-cluster arrows
    // correctly.  Every edge target is passed through `follow_nops` so that
    // Nop merge nodes disappear from the visual without losing connectivity.
    let preamble_id_set: HashSet<NodeIndex> = preamble_ids.iter().copied().collect();

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
            // ── Single-successor nodes ────────────────────────────────────
            IrNodeKind::Assign { .. }
            | IrNodeKind::Eval { .. }
            | IrNodeKind::EnterScope { .. }
            | IrNodeKind::ExitScope { .. }
            | IrNodeKind::PushScope
            | IrNodeKind::PopScope
            | IrNodeKind::DefineEnum { .. }
            | IrNodeKind::DefineStruct { .. }
            | IrNodeKind::DefineScriptDecorator { .. }
            | IrNodeKind::DefineFunction { .. }
            | IrNodeKind::ExternDecl { .. }
            | IrNodeKind::Dialogue { .. } => {
                let next_idx = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Next))
                    .map(|e| e.target());
                let target = next_idx.and_then(|t| follow_nops(graph, t));
                write_edge(&mut out, &n, target, None, None, &mut has_end_edge);
            }

            // ── Conditional branch ────────────────────────────────────────
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
                write_edge(&mut out, &n, then_t, Some("then"), None, &mut has_end_edge);
                write_edge(
                    &mut out,
                    &n,
                    else_t,
                    Some("else"),
                    Some("style=dashed"),
                    &mut has_end_edge,
                );
            }

            // ── Multi-arm switch ──────────────────────────────────────────
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
                    write_edge(&mut out, &n, t, Some(&lbl), None, &mut has_end_edge);
                }
                let default_raw = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Default))
                    .map(|e| e.target());
                if let Some(def_raw) = default_raw {
                    let t = follow_nops(graph, def_raw);
                    write_edge(
                        &mut out,
                        &n,
                        t,
                        Some("default"),
                        Some("style=dashed"),
                        &mut has_end_edge,
                    );
                }
            }

            // ── Jump: dashed, constraint=false ────────────────────────────
            IrNodeKind::Jump => {
                let target_raw = graph
                    .graph
                    .edges_directed(node_idx, Direction::Outgoing)
                    .find(|e| matches!(e.weight(), IrEdge::Jump))
                    .map(|e| e.target());
                match target_raw {
                    None => {
                        has_end_edge = true;
                        writeln!(
                            out,
                            "    {n} -> __end__ [style=dashed, color=gray40, constraint=false];"
                        )
                        .ok();
                    }
                    Some(t) => {
                        let dst = nid(t);
                        writeln!(
                            out,
                            "    {n} -> {dst} [style=dashed, color=gray40, constraint=false];"
                        )
                        .ok();
                    }
                }
            }

            // ── LetCall: dashed edge to callee, solid edge to return continuation ──
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

                // Dashed edge to the call target (the subroutine entry).
                match call_raw {
                    None => {
                        has_end_edge = true;
                        writeln!(
                            out,
                            "    {n} -> __end__ [style=dashed, color=gray40, constraint=false];"
                        )
                        .ok();
                    }
                    Some(t) => {
                        let dst = nid(t);
                        writeln!(
                            out,
                            "    {n} -> {dst} [style=dashed, color=gray40, constraint=false, label=\"call\"];"
                        )
                        .ok();
                    }
                }
                // Dashed edge to the return continuation.
                let ret_resolved = ret_raw.and_then(|r| follow_nops(graph, r));
                let ret_dst_str = match ret_resolved {
                    None => {
                        has_end_edge = true;
                        "__end__".to_string()
                    }
                    Some(t) => nid(t),
                };
                writeln!(
                    out,
                    "    {n} -> {ret_dst_str} \
                     [style=dashed, color=darkslateblue, label=\"ret\"];",
                )
                .ok();
            }

            // ── Choice fan-out ────────────────────────────────────────────
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
                    let lbl = if opt.is_default {
                        format!("[{i}] _ (default)")
                    } else {
                        format!("[{i}] {}", truncate(&opt.label, 24))
                    };
                    let style = if opt.is_default {
                        "color=purple, style=dashed"
                    } else {
                        "color=purple"
                    };
                    write_edge(
                        &mut out,
                        &n,
                        entry,
                        Some(&lbl),
                        Some(style),
                        &mut has_end_edge,
                    );
                }
                // Choice has no `next` — control flows only via chosen option.
            }

            // ── Return ────────────────────────────────────────────────────
            // If this Return lives inside a subroutine label (i.e. some LetCall
            // node targets that label), draw dashed ↩ back-edges to each
            // caller's ret continuation so the diagram shows the actual resume
            // point.  A bare top-level return (no LetCall caller) still goes to
            // __end__ as before.
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
                                    writeln!(
                                        out,
                                        "    {n} -> __end__ \
                                         [style=dashed, color=darkslateblue, \
                                         constraint=false, label=\"↩\"];",
                                    )
                                    .ok();
                                }
                                Some(t) => {
                                    let dst = nid(t);
                                    writeln!(
                                        out,
                                        "    {n} -> {dst} \
                                         [style=dashed, color=darkslateblue, \
                                         constraint=false, label=\"↩\"];",
                                    )
                                    .ok();
                                }
                            }
                        }
                    }
                    _ => {
                        has_end_edge = true;
                        writeln!(out, "    {n} -> __end__ [style=dashed, color=gray40];").ok();
                    }
                }
            }

            // ── Terminals with no outgoing edges ──────────────────────────
            IrNodeKind::End => {}

            // todo!() is a placeholder terminator — no outgoing edges.
            IrNodeKind::Todo => {}

            // Nop is handled at the top of the loop (skipped entirely).
            IrNodeKind::Nop => unreachable!("Nop should have been skipped"),
        }
    }

    // ── END sink ─────────────────────────────────────────────────────────────
    if has_end_edge {
        writeln!(out).ok();
        writeln!(
            out,
            "    __end__ [label=\"END\", shape=doublecircle, \
             style=filled, fillcolor=tomato, fontcolor=white];"
        )
        .ok();
    }

    writeln!(out).ok();
    writeln!(out, "}}").ok();
    out
}

// ─── Visual attributes ───────────────────────────────────────────────────────

/// Returns `(shape, fillcolor, label)` for a given IR node.
fn node_attrs(
    idx: NodeIndex,
    kind: &IrNodeKind,
    graph: &IrGraph,
    label_by_entry: &HashMap<NodeIndex, String>,
) -> (&'static str, &'static str, String) {
    match kind {
        IrNodeKind::Assign { var, scope, .. } => (
            "box",
            "#a8d8d4",
            format!("{} {} = ⟨expr⟩", decl_kw(scope), var),
        ),

        IrNodeKind::Eval { .. } => ("box", "#a8d8d4", "eval ⟨expr⟩".into()),

        IrNodeKind::Branch { condition, .. } => (
            "diamond",
            "#ffd54f",
            format!("branch\n{}", truncate(&ast_summary(condition), 28)),
        ),

        IrNodeKind::Switch { arms, .. } => {
            let has_default = graph
                .graph
                .edges_directed(idx, Direction::Outgoing)
                .any(|e| matches!(e.weight(), IrEdge::Default));
            let total = arms.len() + usize::from(has_default);
            ("diamond", "#ffd54f", format!("match ({total} arms)"))
        }

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
            ("rarrow", "wheat", dest)
        }

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
            let label = if var.is_empty() {
                format!("⤑ {callee}")
            } else {
                format!("⤑ {callee}\n→ {var}")
            };
            ("rarrow", "wheat", label)
        }

        IrNodeKind::Return { value } => (
            "box",
            "#e8c4c4",
            if value.is_some() {
                "return ⟨expr⟩".into()
            } else {
                "return".into()
            },
        ),

        // Inside a cluster the label name is shown in the cluster title,
        // so the entry/exit markers use a compact arrow prefix.
        IrNodeKind::EnterScope { label } => ("cds", "#66bb6a", format!("▶ {label}")),

        IrNodeKind::ExitScope { label } => ("cds", "#66bb6a", format!("◀ {label}")),

        IrNodeKind::PushScope => ("box", "#a5d6a7", "PushScope".into()),

        IrNodeKind::PopScope => ("box", "#a5d6a7", "PopScope".into()),

        IrNodeKind::DefineEnum { name, variants, .. } => (
            "box",
            "lavender",
            format!("enum {name} ({} variants)", variants.len()),
        ),

        IrNodeKind::DefineStruct { name, fields, .. } => (
            "box",
            "lavender",
            format!("struct {name} ({} fields)", fields.len()),
        ),

        IrNodeKind::DefineScriptDecorator { name, params, .. } => (
            "box",
            "#E6E6FA",
            format!("def_decorator\n@{name} ({} params)", params.len()),
        ),

        IrNodeKind::DefineFunction { name, params, .. } => (
            "box",
            "#E6F5E6",
            format!("fn {name} ({} params)", params.len()),
        ),

        IrNodeKind::ExternDecl { name } => ("box", "#c8e6c9", format!("extern {name}")),

        IrNodeKind::Nop => {
            // Nop nodes should never reach node_attrs — they are skipped before
            // this function is called.  Fall back to a visible placeholder so
            // bugs are immediately obvious rather than silent.
            ("ellipse", "gray90", format!("● {}", idx.index()))
        }

        IrNodeKind::End => ("doublecircle", "tomato", "end".into()),

        IrNodeKind::Todo => ("doublecircle", "orange", "todo!".into()),

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
            for line in line_texts.iter().take(3) {
                parts.push(format!("\"{}\"", truncate(line, 34)));
            }
            if line_texts.len() > 3 {
                parts.push(format!("(+{} more)", line_texts.len() - 3));
            }
            if !decs.is_empty() {
                parts.push(decs);
            }

            ("box", "#b8c0cc", parts.join("\n"))
        }

        IrNodeKind::Choice {
            options,
            decorators,
            ..
        } => {
            let decs = decorator_line(decorators);
            let label = if decs.is_empty() {
                format!("choice ({} opts)", options.len())
            } else {
                format!("choice ({} opts)\n{decs}", options.len())
            };
            ("hexagon", "#b07fe8", label)
        }
    }
}

// ─── Edge helpers ────────────────────────────────────────────────────────────

/// Emits a single DOT edge from `src` to `dst`.
///
/// If `dst` is `None`, the edge targets the synthetic `__end__` sink
/// instead, and `has_end_edge` is set to `true`.
fn write_edge(
    out: &mut String,
    src: &str,
    dst: Option<NodeIndex>,
    label: Option<&str>,
    extra_attrs: Option<&str>,
    has_end_edge: &mut bool,
) {
    let dst_str = match dst {
        None => {
            *has_end_edge = true;
            "__end__".to_string()
        }
        Some(idx) => nid(idx),
    };

    let mut attrs: Vec<String> = Vec::new();
    if let Some(lbl) = label {
        attrs.push(format!("label={}", quoted(lbl)));
    }
    if let Some(extra) = extra_attrs {
        attrs.push(extra.to_string());
    }

    if attrs.is_empty() {
        writeln!(out, "    {src} -> {dst_str};").ok();
    } else {
        writeln!(out, "    {src} -> {dst_str} [{}];", attrs.join(", ")).ok();
    }
}

// ─── Node ID formatting ──────────────────────────────────────────────────────

/// Formats a [`NodeIndex`] as a renderer node identifier string (e.g. `N42`).
fn nid(idx: NodeIndex) -> String {
    format!("N{}", idx.index())
}

// ─── DOT-specific helpers ────────────────────────────────────────────────────

/// Wraps `s` in DOT double-quotes, escaping inner special characters.
fn quoted(s: &str) -> String {
    format!("\"{}\"", escape_dot(s))
}

/// Escapes characters that have special meaning inside a DOT double-quoted string.
fn escape_dot(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "")
        .replace('<', "\\<")
        .replace('>', "\\>")
        .replace('{', "\\{")
        .replace('}', "\\}")
        .replace('|', "\\|")
}

/// Converts an arbitrary string into a valid DOT identifier segment by
/// replacing non-alphanumeric characters (except `_`) with `_`.
///
/// NOTE: The Mermaid renderer has its own `sanitize_id` that additionally
/// normalises `::`, `.`, ` ` and `-` before the alphanumeric filter.
fn sanitize_id(s: &str) -> String {
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
    use std::collections::{HashMap, HashSet};

    use super::*;
    use crate::compiler::Compiler;
    use crate::ir::analysis;
    use crate::lexer::strings::ParsedString;
    use crate::parser::ast::{Ast, DeclKind, TokSpan};
    use crate::runtime::value::RuntimeValue;

    fn compile(ast: Ast) -> IrGraph {
        Compiler::compile(&ast).expect("compile failed in test")
    }

    // ── Header / structure ──────────────────────────────────────────────────

    #[test]
    fn test_dot_header_and_footer() {
        let dot = compile(Ast::block(vec![])).to_dot();
        assert!(
            dot.starts_with("digraph urd_script {"),
            "must open with digraph header"
        );
        assert!(dot.ends_with("}\n"), "must close with }}");
    }

    #[test]
    fn test_dot_splines_ortho() {
        let dot = compile(Ast::block(vec![])).to_dot();
        assert!(
            dot.contains("splines=ortho"),
            "must set splines=ortho for right-angle edge routing"
        );
        assert!(
            !dot.contains("compound=true"),
            "compound=true must be absent — it conflicts with splines=ortho"
        );
    }

    #[test]
    fn test_dot_start_node_is_prominent() {
        let dot = compile(Ast::block(vec![Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::Int(1)),
        )]))
        .to_dot();
        assert!(dot.contains("__start__"), "must contain __start__ node");
        assert!(dot.contains("▶ START"), "START label must be present");
        assert!(
            dot.contains("invhouse"),
            "__start__ must use invhouse shape"
        );
    }

    // ── Cluster structure ───────────────────────────────────────────────────

    #[test]
    fn test_labeled_block_produces_cluster() {
        let dot = compile(Ast::block(vec![Ast::labeled_block(
            "intro".into(),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                Ast::value(RuntimeValue::Int(1)),
            )]),
        )]))
        .to_dot();
        assert!(
            dot.contains("subgraph cluster_intro"),
            "label must produce a cluster subgraph"
        );
        assert!(
            dot.contains("label=\"intro\""),
            "cluster must display the label name"
        );
        assert!(
            dot.contains("darkgreen"),
            "cluster must use darkgreen border"
        );
    }

    #[test]
    fn test_multiple_labels_produce_multiple_clusters() {
        let dot = compile(Ast::block(vec![
            Ast::labeled_block(
                "scene_a".into(),
                Ast::block(vec![Ast::jump_stmt("scene_b".into(), false)]),
            ),
            Ast::labeled_block(
                "scene_b".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["done".into()])),
                    Ast::value(RuntimeValue::Bool(true)),
                )]),
            ),
        ]))
        .to_dot();
        assert!(
            dot.contains("subgraph cluster_scene_a"),
            "cluster for scene_a"
        );
        assert!(
            dot.contains("subgraph cluster_scene_b"),
            "cluster for scene_b"
        );
    }

    #[test]
    fn test_cluster_sorted_by_entry_node_id() {
        // scene_a compiles first → lower NodeIndex → appears first in DOT output.
        let dot = compile(Ast::block(vec![
            Ast::labeled_block(
                "scene_a".into(),
                Ast::block(vec![Ast::jump_stmt("scene_b".into(), false)]),
            ),
            Ast::labeled_block(
                "scene_b".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                    Ast::value(RuntimeValue::Int(0)),
                )]),
            ),
        ]))
        .to_dot();
        let pos_a = dot
            .find("subgraph cluster_scene_a")
            .expect("cluster_scene_a missing");
        let pos_b = dot
            .find("subgraph cluster_scene_b")
            .expect("cluster_scene_b missing");
        assert!(
            pos_a < pos_b,
            "scene_a (compiled first) must appear before scene_b in DOT output"
        );
    }

    #[test]
    fn test_entry_cluster_has_thicker_border() {
        // The first label (entry cluster) should have penwidth=3.0.
        let dot = compile(Ast::block(vec![
            Ast::labeled_block(
                "first".into(),
                Ast::block(vec![Ast::jump_stmt("second".into(), false)]),
            ),
            Ast::labeled_block(
                "second".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                    Ast::value(RuntimeValue::Int(0)),
                )]),
            ),
        ]))
        .to_dot();
        // Find cluster_first subgraph block and check it has penwidth=3.0
        let cluster_first_pos = dot
            .find("subgraph cluster_first")
            .expect("cluster_first missing");
        let closing_brace = dot[cluster_first_pos..]
            .find("\n    }")
            .expect("cluster closing brace missing")
            + cluster_first_pos;
        let first_block = &dot[cluster_first_pos..closing_brace];
        assert!(
            first_block.contains("penwidth=3.0"),
            "entry cluster must have penwidth=3.0, got:\n{first_block}"
        );
    }

    #[test]
    fn test_todo_cluster_gets_orange_styling() {
        // A label that contains todo!() must have orange border + fill.
        // A clean label (end!()) must retain the default green styling.
        //
        // Both labels are made reachable: a `start` label jumps into
        // `unfinished`, which in turn jumps to `done` so neither is pruned
        // by the dead-code reachability pass.

        let todo_call = {
            let func = Ast::value(RuntimeValue::IdentPath(vec!["todo!".to_string()]));
            let params = Ast::expr_list(vec![]);
            Ast::call(func, params)
        };
        let end_call = {
            let func = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_string()]));
            let params = Ast::expr_list(vec![]);
            Ast::call(func, params)
        };

        let dot = compile(Ast::block(vec![
            // `start` has a menu so both `unfinished` and `done` are reachable.
            Ast::labeled_block(
                "start".into(),
                Ast::block(vec![Ast::menu(vec![
                    Ast::menu_option(
                        "a".into(),
                        Ast::block(vec![Ast::jump_stmt("unfinished".into(), false)]),
                        false,
                    ),
                    Ast::menu_option(
                        "b".into(),
                        Ast::block(vec![Ast::jump_stmt("done".into(), false)]),
                        false,
                    ),
                ])]),
            ),
            Ast::labeled_block("unfinished".into(), Ast::block(vec![todo_call])),
            Ast::labeled_block("done".into(), Ast::block(vec![end_call])),
        ]))
        .to_dot();

        // ── todo cluster ─────────────────────────────────────────────────────
        let todo_pos = dot
            .find("subgraph cluster_unfinished")
            .expect("cluster_unfinished must be in DOT output");
        let todo_end = dot[todo_pos..]
            .find("\n    }")
            .expect("cluster_unfinished must have a closing brace")
            + todo_pos;
        let todo_block = &dot[todo_pos..todo_end];

        assert!(
            todo_block.contains("darkorange"),
            "todo cluster must use darkorange border, got:\n{todo_block}"
        );
        assert!(
            todo_block.contains("#ffe0b2"),
            "todo cluster must use orange tint fill, got:\n{todo_block}"
        );

        // ── clean cluster ─────────────────────────────────────────────────────
        let done_pos = dot
            .find("subgraph cluster_done")
            .expect("cluster_done must be in DOT output");
        let done_end = dot[done_pos..]
            .find("\n    }")
            .expect("cluster_done must have a closing brace")
            + done_pos;
        let done_block = &dot[done_pos..done_end];

        assert!(
            done_block.contains("darkgreen"),
            "clean cluster must keep darkgreen border, got:\n{done_block}"
        );
        assert!(
            done_block.contains("#f0fff4"),
            "clean cluster must keep green fill, got:\n{done_block}"
        );
    }

    #[test]
    fn test_nodes_inside_label_appear_in_cluster_block() {
        let dot = compile(Ast::block(vec![Ast::labeled_block(
            "act1".into(),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["score".into()])),
                Ast::value(RuntimeValue::Int(0)),
            )]),
        )]))
        .to_dot();
        let cluster_pos = dot.find("subgraph cluster_act1").expect("cluster missing");
        let score_pos = dot.find("let score").expect("assign node missing");
        assert!(
            score_pos > cluster_pos,
            "assign node must appear after the cluster subgraph opens"
        );
    }

    // ── Nop collapsing ──────────────────────────────────────────────────────

    #[test]
    fn test_nop_nodes_are_collapsed() {
        // if/elif/else produces Nop merge nodes.  They must not appear.
        let dot = compile(Ast::block(vec![Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["a".into()])),
                Ast::value(RuntimeValue::Int(1)),
            )]),
            Some(Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["b".into()])),
                Ast::value(RuntimeValue::Int(2)),
            )])),
        )]))
        .to_dot();
        // "●" is the prefix used for Nop node labels.
        assert!(
            !dot.contains('●'),
            "Nop merge nodes (● prefix) must be collapsed and not rendered"
        );
    }

    // ── Dead node filtering ─────────────────────────────────────────────────

    #[test]
    fn test_dead_exit_scope_not_rendered() {
        // A label block where all paths use `jump` leaves ExitScope unreachable.
        let dot = compile(Ast::block(vec![Ast::labeled_block(
            "loop_lbl".into(),
            Ast::block(vec![Ast::jump_stmt("loop_lbl".into(), false)]),
        )]))
        .to_dot();
        // EnterScope IS reachable (from entry and from jump).
        assert!(
            dot.contains("▶ loop_lbl"),
            "reachable EnterScope must be rendered"
        );
        // ExitScope is dead (jump never falls through to it).
        assert!(
            !dot.contains("◀ loop_lbl"),
            "unreachable ExitScope must NOT be rendered"
        );
    }

    #[test]
    fn test_reachable_exit_scope_is_rendered() {
        // A label that simply assigns and falls through → ExitScope IS reachable.
        let dot = compile(Ast::block(vec![Ast::labeled_block(
            "pass_thru".into(),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                Ast::value(RuntimeValue::Int(7)),
            )]),
        )]))
        .to_dot();
        assert!(dot.contains("▶ pass_thru"), "EnterScope must be rendered");
        assert!(
            dot.contains("◀ pass_thru"),
            "reachable ExitScope must be rendered"
        );
        assert!(
            dot.contains("#66bb6a"),
            "scope markers must use new green fill"
        );
        assert!(dot.contains("cds"), "scope markers must use cds shape");
    }

    // ── Jump routing ────────────────────────────────────────────────────────

    #[test]
    fn test_jump_edge_points_directly_at_enter_scope() {
        let dot = compile(Ast::block(vec![Ast::labeled_block(
            "loop_top".into(),
            Ast::block(vec![Ast::jump_stmt("loop_top".into(), false)]),
        )]))
        .to_dot();
        // compound=true / lhead are disabled so splines=ortho works everywhere.
        // Jump edges point directly at the EnterScope node inside the cluster.
        assert!(
            !dot.contains("lhead="),
            "jump edges must NOT use lhead (compound=true is disabled for splines=ortho)"
        );
        assert!(
            dot.contains("constraint=false"),
            "jump edge must still carry constraint=false to avoid rank distortion"
        );
    }

    #[test]
    fn test_jump_edge_is_dashed_and_no_constraint() {
        let dot = compile(Ast::block(vec![
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
        .to_dot();
        assert!(dot.contains("style=dashed"), "jump edge must be dashed");
        assert!(dot.contains("color=gray40"), "jump edge must be gray");
        assert!(
            dot.contains("constraint=false"),
            "jump edge must carry constraint=false to avoid distorting layout rank"
        );
    }

    // ── Return → __end__ ────────────────────────────────────────────────────

    #[test]
    fn test_return_emits_end_edge() {
        let dot = compile(Ast::block(vec![Ast::labeled_block(
            "s".into(),
            Ast::block(vec![Ast::return_stmt(None)]),
        )]))
        .to_dot();
        assert!(
            dot.contains("__end__"),
            "Return node must emit an edge to the __end__ sink"
        );
        assert!(dot.contains("return"), "Return node must be rendered");
        assert!(
            dot.contains("#e8c4c4"),
            "Return node must use muted rose fill"
        );
    }

    // ── Node-kind rendering ─────────────────────────────────────────────────

    #[test]
    fn test_assign_label() {
        let dot = compile(Ast::block(vec![Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["health".into()])),
            Ast::value(RuntimeValue::Int(100)),
        )]))
        .to_dot();
        assert!(dot.contains("let health"), "assign node must show var name");
    }

    #[test]
    fn test_dialogue_shows_speaker_and_text() {
        let dot = compile(Ast::block(vec![Ast::dialogue(
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "Elara".into(),
            ]))]),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain(
                "Welcome, traveller.",
            ))),
        )]))
        .to_dot();
        assert!(
            dot.contains("Elara"),
            "speaker name must appear in dialogue node label"
        );
        assert!(
            dot.contains("Welcome, traveller."),
            "line text must appear in dialogue node label"
        );
    }

    #[test]
    fn test_dialogue_multiline_content() {
        let dot = compile(Ast::block(vec![Ast::dialogue(
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "Host".into(),
            ]))]),
            Ast::expr_list(vec![
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("Line one."))),
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("Line two."))),
            ]),
        )]))
        .to_dot();
        assert!(dot.contains("Host"), "speaker must appear");
        assert!(dot.contains("Line one."), "first line must appear");
        assert!(dot.contains("Line two."), "second line must appear");
    }

    #[test]
    fn test_dialogue_node_styling() {
        let dot = compile(Ast::block(vec![Ast::dialogue(
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "Alice".into(),
            ]))]),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("Hello!"))),
        )]))
        .to_dot();
        assert!(
            dot.contains("#b8c0cc"),
            "dialogue must have muted slate fill"
        );
    }

    #[test]
    fn test_decorator_annotation_on_dialogue() {
        use crate::parser::ast::Decorator;
        let ast = Ast::dialogue(
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "Bob".into(),
            ]))]),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("Hey"))),
        )
        .with_decorators(vec![Decorator::bare("voiced".into())]);
        let dot = compile(Ast::block(vec![ast])).to_dot();
        assert!(
            dot.contains("@voiced"),
            "decorator name must appear in DOT output"
        );
    }

    #[test]
    fn test_branch_node_and_edges() {
        let dot = compile(Ast::block(vec![Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
                Ast::value(RuntimeValue::Int(1)),
            )]),
            None,
        )]))
        .to_dot();
        assert!(dot.contains("branch"), "must have branch node");
        assert!(dot.contains("then"), "must have 'then' edge");
        assert!(dot.contains("else"), "must have 'else' edge");
        assert!(dot.contains("diamond"), "branch must use diamond shape");
    }

    #[test]
    fn test_choice_node_and_edges() {
        let dot = compile(Ast::block(vec![Ast::menu(vec![
            Ast::menu_option(
                "Yes".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["a".into()])),
                    Ast::value(RuntimeValue::Int(1)),
                )]),
                false,
            ),
            Ast::menu_option(
                "No".into(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["b".into()])),
                    Ast::value(RuntimeValue::Int(2)),
                )]),
                false,
            ),
        ])]))
        .to_dot();
        assert!(dot.contains("choice"), "must have choice node");
        assert!(dot.contains("Yes"), "must label option 0");
        assert!(dot.contains("No"), "must label option 1");
        assert!(dot.contains("hexagon"), "choice must use hexagon shape");
        assert!(dot.contains("color=purple"), "choice edges must be purple");
    }

    #[test]
    fn test_define_enum_node() {
        let dot = compile(Ast::block(vec![Ast::enum_decl(
            "Dir".into(),
            vec![
                ("N".into(), TokSpan::default()),
                ("S".into(), TokSpan::default()),
            ],
        )]))
        .to_dot();
        assert!(dot.contains("enum"), "must contain DefineEnum node");
        assert!(dot.contains("lavender"), "enum node must be lavender");
    }

    #[test]
    fn test_end_sentinel_from_assign() {
        // A bare assignment with no Next successor must produce the __end__ sink.
        let dot = compile(Ast::block(vec![Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::Int(1)),
        )]))
        .to_dot();
        assert!(
            dot.contains("__end__"),
            "missing-next edge must produce __end__ sink"
        );
        assert!(dot.contains("tomato"), "__end__ must use tomato fill");
    }

    // ── Escape / truncation helpers ─────────────────────────────────────────

    #[test]
    fn test_quoted_escapes_inner_double_quotes() {
        let q = quoted(r#"say "hi""#);
        let inner = &q[1..q.len() - 1];
        assert!(inner.contains("\\\""), "escaped quote must appear");
        for (i, ch) in inner.char_indices() {
            if ch == '"' {
                assert!(
                    i > 0 && inner.as_bytes()[i - 1] == b'\\',
                    "unescaped quote at position {i}"
                );
            }
        }
    }

    #[test]
    fn test_escape_dot_newline() {
        let s = escape_dot("line1\nline2");
        assert!(!s.contains('\n'), "newlines must be escaped");
        assert!(s.contains("\\n"));
    }

    #[test]
    fn test_sanitize_id_replaces_special_chars() {
        assert_eq!(sanitize_id("my-label"), "my_label");
        assert_eq!(sanitize_id("scene.1"), "scene_1");
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

    #[test]
    fn test_follow_nops_skips_chain() {
        use crate::ir::{IrEdge, IrGraph, IrNodeKind};
        use petgraph::stable_graph::StableGraph;

        // Build a mini petgraph: N0(Nop) --Next--> N1(Nop) --Next--> N2(End).
        let mut g: StableGraph<IrNodeKind, IrEdge> = StableGraph::new();
        let n0 = g.add_node(IrNodeKind::Nop);
        let n1 = g.add_node(IrNodeKind::Nop);
        let n2 = g.add_node(IrNodeKind::End);
        g.add_edge(n0, n1, IrEdge::Next);
        g.add_edge(n1, n2, IrEdge::Next);

        let graph = IrGraph {
            graph: g,
            entry: Some(n0),
            labels: HashMap::new(),
            entry_labels: HashSet::new(),
            cluster_names: HashMap::new(),
            label_sources: HashMap::new(),
        };

        assert_eq!(
            analysis::follow_nops(&graph, n0),
            Some(n2),
            "follow_nops must skip both Nop nodes"
        );
        assert_eq!(
            analysis::follow_nops(&graph, n2),
            Some(n2),
            "follow_nops must return End unchanged"
        );
    }
}
