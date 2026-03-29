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
//!   label (by compilation order / `NodeId`) gets a thicker border to
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
//! * `return` always emits an edge to the `__end__` sink.
//! * `dialogue` nodes display the actual speaker(s) and line text extracted
//!   from the AST at render time (best-effort; falls back to `⟨expr⟩`).
//! * Colour legend: cyan = assign/eval, yellow = branch/match,
//!   green = scope marker, salmon = dialogue, plum = choice,
//!   lavender = enum, rose = return, wheat = jump.
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

use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Write as _;

use crate::parser::ast::{AstContent, DeclKind, MatchPattern};
use crate::runtime::value::RuntimeValue;

use super::{IrGraph, IrNodeKind, NODE_END, NodeId};

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
    let reachable = reachable_nodes(graph);
    let clusters = compute_clusters(graph, &reachable);

    // Sort cluster names by entry NodeId (ascending = compilation order).
    // This encourages Graphviz to place the first-written label at the top.
    let mut sorted_labels: Vec<&String> = clusters.keys().collect();
    sorted_labels.sort_by_key(|name| graph.labels.get(*name).map(|id| id.0).unwrap_or(u32::MAX));

    // The entry cluster is the one whose entry NodeId is reached first from
    // graph.entry along the linear prologue (assignments, enums, etc.).
    let entry_cluster = entry_cluster_name(graph);

    // Reverse map: label entry NodeId → label name, used to display human-readable
    // jump targets instead of raw NodeId numbers.
    let label_by_entry: HashMap<NodeId, String> = graph
        .labels
        .iter()
        .map(|(name, &id)| (id, name.clone()))
        .collect();

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

    if graph.entry != NODE_END {
        writeln!(out, "    __start__ -> {};", nid(graph.entry)).ok();
    }
    writeln!(out).ok();

    // ── Cluster subgraphs (node definitions only, no edges) ──────────────────
    //
    // Nodes *defined* inside a subgraph cluster belong to it visually.
    // Edges are emitted after all subgraphs (idiomatic DOT).
    for label_name in &sorted_labels {
        let members = &clusters[*label_name];
        let safe = sanitize_id(label_name);
        let is_entry = entry_cluster.as_deref() == Some(label_name.as_str());
        let penwidth = if is_entry { "3.0" } else { "1.5" };

        writeln!(out, "    subgraph cluster_{safe} {{").ok();
        writeln!(out, "        label={};", quoted(label_name)).ok();
        writeln!(out, "        style=\"rounded,filled\";").ok();
        writeln!(out, "        fillcolor=\"#f0fff4\";").ok();
        writeln!(out, "        color=darkgreen;").ok();
        writeln!(out, "        penwidth={penwidth};").ok();
        writeln!(out).ok();

        let mut sorted_members: Vec<NodeId> = members.iter().copied().collect();
        sorted_members.sort_by_key(|n| n.0);

        for node_id in sorted_members {
            if node_id.0 as usize >= graph.nodes.len() {
                continue;
            }
            let node = &graph.nodes[node_id.0 as usize];
            let (shape, fill, label) = node_attrs(node.id, &node.kind, &label_by_entry);
            writeln!(
                out,
                "        {} [label={}, shape={shape}, style=filled, fillcolor={}];",
                nid(node.id),
                quoted(&label),
                quoted(fill),
            )
            .ok();
        }

        writeln!(out, "    }}").ok();
        writeln!(out).ok();
    }

    // ── Non-clustered reachable non-Nop node definitions ────────────────────
    let clustered: HashSet<NodeId> = clusters.values().flatten().copied().collect();
    let mut any_global = false;

    for node in &graph.nodes {
        if !reachable.contains(&node.id) {
            continue;
        }
        if matches!(&node.kind, IrNodeKind::Nop { .. }) {
            continue;
        }
        if clustered.contains(&node.id) {
            continue;
        }
        any_global = true;
        let (shape, fill, label) = node_attrs(node.id, &node.kind, &label_by_entry);
        writeln!(
            out,
            "    {} [label={}, shape={shape}, style=filled, fillcolor={}];",
            nid(node.id),
            quoted(&label),
            quoted(fill),
        )
        .ok();
    }
    if any_global {
        writeln!(out).ok();
    }

    // ── All edges ────────────────────────────────────────────────────────────
    //
    // Emitted outside all subgraphs so Graphviz draws cross-cluster arrows
    // correctly.  Every edge target is passed through `follow_nops` so that
    // Nop merge nodes disappear from the visual without losing connectivity.
    for node in &graph.nodes {
        if !reachable.contains(&node.id) {
            continue;
        }
        if matches!(&node.kind, IrNodeKind::Nop { .. }) {
            continue;
        }

        let n = nid(node.id);

        match &node.kind {
            // ── Single-successor nodes ────────────────────────────────────
            IrNodeKind::Assign { next, .. }
            | IrNodeKind::Eval { next, .. }
            | IrNodeKind::EnterScope { next, .. }
            | IrNodeKind::ExitScope { next, .. }
            | IrNodeKind::DefineEnum { next, .. }
            | IrNodeKind::DefineScriptDecorator { next, .. }
            | IrNodeKind::Dialogue { next, .. } => {
                let target = follow_nops(graph, *next);
                write_edge(&mut out, &n, target, None, None, &mut has_end_edge);
            }

            // ── Conditional branch ────────────────────────────────────────
            IrNodeKind::Branch {
                then_node,
                else_node,
                ..
            } => {
                let then_t = follow_nops(graph, *then_node);
                let else_t = follow_nops(graph, *else_node);
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
            IrNodeKind::Switch { arms, default, .. } => {
                for arm in arms {
                    let t = follow_nops(graph, arm.target);
                    let lbl = arm_pattern_label(&arm.pattern);
                    write_edge(&mut out, &n, t, Some(&lbl), None, &mut has_end_edge);
                }
                if let Some(def) = default {
                    let t = follow_nops(graph, *def);
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
            IrNodeKind::Jump { target } => {
                if *target == NODE_END {
                    has_end_edge = true;
                    writeln!(
                        out,
                        "    {n} -> __end__ [style=dashed, color=gray40, constraint=false];"
                    )
                    .ok();
                } else {
                    let dst = nid(*target);
                    writeln!(
                        out,
                        "    {n} -> {dst} [style=dashed, color=gray40, constraint=false];"
                    )
                    .ok();
                }
            }

            // ── Choice fan-out ────────────────────────────────────────────
            IrNodeKind::Choice { options, .. } => {
                for (i, opt) in options.iter().enumerate() {
                    let entry = follow_nops(graph, opt.entry);
                    let lbl = format!("[{i}] {}", truncate(&opt.label, 24));
                    write_edge(
                        &mut out,
                        &n,
                        entry,
                        Some(&lbl),
                        Some("color=purple"),
                        &mut has_end_edge,
                    );
                }
                // Choice has no `next` — control flows only via chosen option.
            }

            // ── Return: always terminates to __end__ ──────────────────────
            IrNodeKind::Return { .. } => {
                has_end_edge = true;
                writeln!(out, "    {n} -> __end__ [style=dashed, color=gray40];").ok();
            }

            // ── Terminals with no outgoing edges ──────────────────────────
            IrNodeKind::End => {}

            // Nop is handled at the top of the loop (skipped entirely).
            IrNodeKind::Nop { .. } => unreachable!("Nop should have been skipped"),
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

// ─── Analysis helpers ────────────────────────────────────────────────────────

/// Returns the set of all [`NodeId`]s reachable from [`IrGraph::entry`].
///
/// Follows every edge kind — including [`IrNodeKind::Jump`] targets — so that
/// only genuinely unreachable nodes (e.g. dead `ExitScope` left over when all
/// paths use `jump`/`return`) are excluded.
fn reachable_nodes(graph: &IrGraph) -> HashSet<NodeId> {
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
            IrNodeKind::End | IrNodeKind::Return { .. } => {}

            IrNodeKind::Jump { target } => {
                queue.push_back(*target);
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

/// Follows a chain of consecutive [`IrNodeKind::Nop`] nodes and returns the
/// first non-`Nop` [`NodeId`] (or [`NODE_END`] if the chain ends there).
///
/// Used to collapse compiler-generated merge points so they never appear
/// in the rendered graph.
fn follow_nops(graph: &IrGraph, mut id: NodeId) -> NodeId {
    // Guard against pathological cycles (should never happen, but be safe).
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

/// Computes which [`NodeId`]s belong to each named label scope.
///
/// For each label, performs a BFS from its [`IrNodeKind::EnterScope`] node
/// while respecting these boundaries:
///
/// - **Dead nodes** (not in `reachable`) are excluded.
/// - **`Nop` nodes** are followed transparently but never added to the set
///   (they are collapsed at render time).
/// - **Other labels' entry nodes** are not entered.
/// - **`Jump`** targets are not followed (a `Jump` belongs to the current
///   cluster, but its destination belongs to another).
/// - **`ExitScope`** for this label stops recursion (exit marker is included,
///   but its continuation is outside the cluster).
fn compute_clusters(
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
                // Terminals — no successors.
                IrNodeKind::Jump { .. } | IrNodeKind::Return { .. } | IrNodeKind::End => {}

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

/// Finds the name of the first label cluster encountered when following the
/// linear prologue (assignments, enums, nops) from [`IrGraph::entry`].
///
/// This is used to give the "main" entry cluster a thicker border.
fn entry_cluster_name(graph: &IrGraph) -> Option<String> {
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
            return None;
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

// ─── Visual attributes ───────────────────────────────────────────────────────

/// Returns `(shape, fillcolor, label)` for a given IR node.
fn node_attrs(
    id: NodeId,
    kind: &IrNodeKind,
    label_by_entry: &HashMap<NodeId, String>,
) -> (&'static str, &'static str, String) {
    match kind {
        IrNodeKind::Assign { var, scope, .. } => (
            "box",
            "lightcyan",
            format!("{} {} = ⟨expr⟩", decl_kw(scope), var),
        ),

        IrNodeKind::Eval { .. } => ("box", "lightcyan", "eval ⟨expr⟩".into()),

        IrNodeKind::Branch { condition, .. } => (
            "diamond",
            "lightyellow",
            format!("branch\n{}", truncate(&ast_summary(condition), 28)),
        ),

        IrNodeKind::Switch { arms, default, .. } => {
            let total = arms.len() + usize::from(default.is_some());
            ("diamond", "lightyellow", format!("match ({total} arms)"))
        }

        IrNodeKind::Jump { target } => {
            let dest = label_by_entry
                .get(target)
                .map(|name| format!("jump → {name}"))
                .unwrap_or_else(|| format!("jump → {}", nid(*target)));
            ("rarrow", "wheat", dest)
        }

        IrNodeKind::Return { value } => (
            "box",
            "mistyrose",
            if value.is_some() {
                "return ⟨expr⟩".into()
            } else {
                "return".into()
            },
        ),

        // Inside a cluster the label name is shown in the cluster title,
        // so the entry/exit markers use a compact arrow prefix.
        IrNodeKind::EnterScope { label, .. } => ("cds", "palegreen", format!("▶ {label}")),

        IrNodeKind::ExitScope { label, .. } => ("cds", "palegreen", format!("◀ {label}")),

        IrNodeKind::DefineEnum { name, variants, .. } => (
            "box",
            "lavender",
            format!("enum {name} ({} variants)", variants.len()),
        ),

        IrNodeKind::DefineScriptDecorator { name, params, .. } => (
            "box",
            "#E6E6FA",
            format!("def_decorator\n@{name} ({} params)", params.len()),
        ),

        IrNodeKind::Nop { .. } => {
            // Nop nodes should never reach node_attrs — they are skipped before
            // this function is called.  Fall back to a visible placeholder so
            // bugs are immediately obvious rather than silent.
            ("ellipse", "gray90", format!("● {}", id.0))
        }

        IrNodeKind::End => ("doublecircle", "tomato", "end".into()),

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

            ("box", "lightsalmon", parts.join("\n"))
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
            ("hexagon", "plum", label)
        }
    }
}

// ─── Edge helpers ────────────────────────────────────────────────────────────

/// Emits a single DOT edge from `src` to `dst`.
///
/// If `dst` is [`NODE_END`], the edge targets the synthetic `__end__` sink
/// instead, and `has_end_edge` is set to `true`.
fn write_edge(
    out: &mut String,
    src: &str,
    dst: NodeId,
    label: Option<&str>,
    extra_attrs: Option<&str>,
    has_end_edge: &mut bool,
) {
    let dst_str = if dst == NODE_END {
        *has_end_edge = true;
        "__end__".to_string()
    } else {
        nid(dst)
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

// ─── Dialogue content extraction ─────────────────────────────────────────────

/// Extracts a human-readable comma-separated speaker string from the speakers
/// [`crate::parser::ast::Ast`] node of a dialogue statement.
fn extract_speakers(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::ExprList(items) => items
            .iter()
            .map(extract_simple_val)
            .collect::<Vec<_>>()
            .join(", "),
        _ => extract_simple_val(ast),
    }
}

/// Returns each dialogue line as a plain string (best-effort extraction).
fn extract_content_lines(ast: &crate::parser::ast::Ast) -> Vec<String> {
    match ast.content() {
        AstContent::ExprList(items) => items.iter().map(extract_line_text).collect(),
        _ => vec![extract_line_text(ast)],
    }
}

/// Extracts the display text of a single dialogue line node.
fn extract_line_text(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::Value(RuntimeValue::Str(ps)) => format!("{ps}"),
        _ => extract_simple_val(ast),
    }
}

/// Returns a compact display string for a simple value or identifier AST node.
fn extract_simple_val(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
        AstContent::Value(RuntimeValue::Str(ps)) => format!("{ps}"),
        AstContent::Value(RuntimeValue::Int(i)) => i.to_string(),
        AstContent::Value(RuntimeValue::Float(f)) => format!("{f:.2}"),
        AstContent::Value(RuntimeValue::Bool(b)) => b.to_string(),
        AstContent::Value(RuntimeValue::Null) => "null".into(),
        AstContent::Value(RuntimeValue::Dice(c, s)) => format!("{c}d{s}"),
        _ => "⟨?⟩".into(),
    }
}

// ─── Label / summary helpers ──────────────────────────────────────────────────

/// Formats a [`NodeId`] as a DOT node identifier string (e.g. `N42`).
fn nid(id: NodeId) -> String {
    format!("N{}", id.0)
}

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

/// Truncates `s` to at most `max` chars, appending `…` when truncated.
fn truncate(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let cut: String = s.chars().take(max).collect();
        format!("{cut}…")
    }
}

/// Returns a short human-readable summary of an AST expression node.
fn ast_summary(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::Value(rv) => rv_summary(rv),
        AstContent::BinOp { op, left, right } => format!(
            "{} {:?} {}",
            truncate(&ast_summary(left), 8),
            op,
            truncate(&ast_summary(right), 8)
        ),
        AstContent::UnaryOp { op, expr } => {
            format!("{:?} {}", op, truncate(&ast_summary(expr), 12))
        }
        AstContent::Call { func_path, .. } => {
            format!("{}(…)", truncate(&ast_summary(func_path), 14))
        }
        AstContent::ExprList(items) => {
            let parts: Vec<_> = items.iter().map(ast_summary).collect();
            format!("[{}]", truncate(&parts.join(", "), 20))
        }
        AstContent::List(_) => "⟨list⟩".into(),
        AstContent::Map(_) => "⟨map⟩".into(),
        _ => "⟨expr⟩".into(),
    }
}

/// Returns a short display string for a [`RuntimeValue`].
fn rv_summary(rv: &RuntimeValue) -> String {
    match rv {
        RuntimeValue::Null => "null".into(),
        RuntimeValue::Bool(b) => b.to_string(),
        RuntimeValue::Int(i) => i.to_string(),
        RuntimeValue::Float(f) => format!("{f:.3}"),
        RuntimeValue::Str(_) => "\"…\"".into(),
        RuntimeValue::Dice(c, s) => format!("{c}d{s}"),
        RuntimeValue::IdentPath(p) => p.join("."),
        RuntimeValue::Label(name) => format!(":{name}"),
        RuntimeValue::Map(m) => format!("⟨map({})⟩", m.len()),
        RuntimeValue::ScriptDecorator { .. } => "⟨decorator⟩".into(),
    }
}

/// Returns the keyword string for a [`DeclKind`].
fn decl_kw(kind: &DeclKind) -> &'static str {
    match kind {
        DeclKind::Global => "global",
        DeclKind::Constant => "const",
        DeclKind::Variable => "let",
    }
}

/// Produces a one-line decorator annotation string, e.g. `@voiced @id(…)`.
fn decorator_line(decorators: &[crate::parser::ast::Decorator]) -> String {
    decorators
        .iter()
        .map(|d| format!("@{}", d.name()))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Returns a compact label for a [`MatchPattern`] arm.
fn arm_pattern_label(pattern: &MatchPattern) -> String {
    match pattern {
        MatchPattern::Wildcard => "_".into(),
        MatchPattern::Value(inner) => truncate(&ast_summary(inner), 20),
    }
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
                Ast::block(vec![Ast::jump_stmt("scene_b".into())]),
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
        // scene_a compiles first → lower NodeId → appears first in DOT output.
        let dot = compile(Ast::block(vec![
            Ast::labeled_block(
                "scene_a".into(),
                Ast::block(vec![Ast::jump_stmt("scene_b".into())]),
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
                Ast::block(vec![Ast::jump_stmt("second".into())]),
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
            Ast::block(vec![Ast::jump_stmt("loop_lbl".into())]),
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
        assert!(dot.contains("palegreen"), "scope markers must be palegreen");
        assert!(dot.contains("cds"), "scope markers must use cds shape");
    }

    // ── Jump routing ────────────────────────────────────────────────────────

    #[test]
    fn test_jump_edge_points_directly_at_enter_scope() {
        let dot = compile(Ast::block(vec![Ast::labeled_block(
            "loop_top".into(),
            Ast::block(vec![Ast::jump_stmt("loop_top".into())]),
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
            Ast::jump_stmt("target".into()),
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
            dot.contains("mistyrose"),
            "Return node must use mistyrose fill"
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
            dot.contains("lightsalmon"),
            "dialogue must have salmon fill"
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
            vec!["N".into(), "S".into()],
        )]))
        .to_dot();
        assert!(dot.contains("enum"), "must contain DefineEnum node");
        assert!(dot.contains("lavender"), "enum node must be lavender");
    }

    #[test]
    fn test_end_sentinel_from_assign() {
        // A bare assignment with next=NODE_END must produce the __end__ sink.
        let dot = compile(Ast::block(vec![Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::Int(1)),
        )]))
        .to_dot();
        assert!(
            dot.contains("__end__"),
            "NODE_END edge must produce __end__ sink"
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
        assert_eq!(truncate("hello", 10), "hello");
    }

    #[test]
    fn test_truncate_long_appends_ellipsis() {
        let r = truncate("abcdefghijklmnop", 5);
        assert!(r.ends_with('…'));
        assert!(r.chars().count() <= 6);
    }

    #[test]
    fn test_follow_nops_skips_chain() {
        // Build a mini graph: N0(Nop→N1), N1(Nop→N2), N2(End).
        use super::super::{IrNode, IrNodeKind, NodeId};
        let graph = IrGraph {
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
        };
        assert_eq!(
            follow_nops(&graph, NodeId(0)),
            NodeId(2),
            "follow_nops must skip both Nop nodes"
        );
        assert_eq!(
            follow_nops(&graph, NodeId(2)),
            NodeId(2),
            "follow_nops must return End unchanged"
        );
        assert_eq!(
            follow_nops(&graph, NODE_END),
            NODE_END,
            "follow_nops must return NODE_END unchanged"
        );
    }
}
