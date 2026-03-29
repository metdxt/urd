//! Mermaid flowchart renderer for [`super::IrGraph`].
//!
//! Produces a `flowchart TD` string renderable by any Mermaid-compatible
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

use crate::parser::ast::{AstContent, DeclKind, MatchPattern};
use crate::runtime::value::RuntimeValue;

use super::analysis::{self, follow_nops};
use super::{IrGraph, IrNodeKind, NODE_END, NodeId};

// ─── Public surface ───────────────────────────────────────────────────────────

impl IrGraph {
    /// Renders this graph as a Mermaid `flowchart TD` string.
    ///
    /// See the [module documentation][self] for usage and visual legend.
    pub fn to_mermaid(&self) -> String {
        render_mermaid(self)
    }
}

/// Renders `graph` as a Mermaid `flowchart TD` string.
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

    // Sort cluster names by entry NodeId (ascending = compilation order).
    let mut sorted_labels: Vec<&String> = clusters.keys().collect();
    sorted_labels.sort_by_key(|name| graph.labels.get(*name).map(|id| id.0).unwrap_or(u32::MAX));

    // Reverse map: label entry NodeId → label name.
    let label_by_entry: HashMap<NodeId, String> = graph
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
    writeln!(out, "flowchart TD").ok();
    writeln!(out).ok();

    // ── classDef declarations ────────────────────────────────────────────────
    writeln!(out, "    classDef assign fill:#E0FFFF,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef dialogue fill:#FFA07A,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef branch fill:#FFFFE0,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef choice fill:#DDA0DD,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef jump fill:#F5DEB3,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef letcall fill:#F5DEB3,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef ret fill:#FFE4E1,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef scope fill:#90EE90,stroke:#228B22,color:#000").ok();
    writeln!(out, "    classDef enumDef fill:#E6E6FA,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef decoratorDef fill:#E6E6FA,stroke:#888,color:#000").ok();
    writeln!(out, "    classDef endNode fill:#FF6347,stroke:#333,color:#fff").ok();
    writeln!(out, "    classDef startNode fill:#1b4332,stroke:#333,color:#fff").ok();
    writeln!(out).ok();

    // ── START node ───────────────────────────────────────────────────────────
    writeln!(
        out,
        "    __start__[/\"▶ START\"\\]:::startNode"
    )
    .ok();
    writeln!(out).ok();

    // ── Subgraph clusters (node definitions only) ────────────────────────────
    for label_name in &sorted_labels {
        let members = &clusters[*label_name];
        let safe = sanitize_id(label_name);
        let escaped_name = escape_mermaid(label_name);

        writeln!(out, "    subgraph cluster_{safe}[\"{escaped_name}\"]").ok();
        writeln!(out, "        direction TB").ok();

        let mut sorted_members: Vec<NodeId> = members.iter().copied().collect();
        sorted_members.sort_by_key(|n| n.0);

        for node_id in sorted_members {
            if node_id.0 as usize >= graph.nodes.len() {
                continue;
            }
            let node = &graph.nodes[node_id.0 as usize];
            // Skip Nop nodes — they're collapsed.
            if matches!(&node.kind, IrNodeKind::Nop { .. }) {
                continue;
            }
            let def = mermaid_node_def(node.id, &node.kind, &label_by_entry);
            writeln!(out, "        {def}").ok();
        }

        writeln!(out, "    end").ok();
        writeln!(out).ok();
    }

    // ── Subgraph style lines ─────────────────────────────────────────────────
    for label_name in &sorted_labels {
        let safe = sanitize_id(label_name);
        let is_entry = entry_cluster.as_deref() == Some(label_name.as_str());
        let sw = if is_entry { "4px" } else { "2px" };
        writeln!(
            out,
            "    style cluster_{safe} fill:transparent,stroke:#228B22,stroke-width:{sw}"
        )
        .ok();
    }
    writeln!(out).ok();

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
        let def = mermaid_node_def(node.id, &node.kind, &label_by_entry);
        writeln!(out, "    {def}").ok();
    }
    if any_global {
        writeln!(out).ok();
    }

    // ── START → entry edge ───────────────────────────────────────────────────
    if graph.entry != NODE_END {
        let entry_resolved = follow_nops(graph, graph.entry);
        let dst = nid(entry_resolved);
        writeln!(out, "    __start__ --> {dst}").ok();
    }
    writeln!(out).ok();

    // ── All edges ────────────────────────────────────────────────────────────
    for node in &graph.nodes {
        if !reachable.contains(&node.id) {
            continue;
        }
        if matches!(&node.kind, IrNodeKind::Nop { .. }) {
            continue;
        }

        let n = nid(node.id);

        match &node.kind {
            // ── Single-successor nodes ─────────────────────────────────────
            IrNodeKind::Assign { next, .. }
            | IrNodeKind::Eval { next, .. }
            | IrNodeKind::EnterScope { next, .. }
            | IrNodeKind::ExitScope { next, .. }
            | IrNodeKind::DefineEnum { next, .. }
            | IrNodeKind::DefineScriptDecorator { next, .. }
            | IrNodeKind::Dialogue { next, .. } => {
                let target = follow_nops(graph, *next);
                write_mermaid_edge(&mut out, &n, target, None, false, &mut has_end_edge);
            }

            // ── Conditional branch ─────────────────────────────────────────
            IrNodeKind::Branch {
                then_node,
                else_node,
                ..
            } => {
                let then_t = follow_nops(graph, *then_node);
                let else_t = follow_nops(graph, *else_node);
                write_mermaid_edge(
                    &mut out,
                    &n,
                    then_t,
                    Some("then"),
                    false,
                    &mut has_end_edge,
                );
                write_mermaid_edge(
                    &mut out,
                    &n,
                    else_t,
                    Some("else"),
                    true,
                    &mut has_end_edge,
                );
            }

            // ── Multi-arm switch ───────────────────────────────────────────
            IrNodeKind::Switch { arms, default, .. } => {
                for arm in arms {
                    let t = follow_nops(graph, arm.target);
                    let lbl = arm_pattern_label(&arm.pattern);
                    write_mermaid_edge(&mut out, &n, t, Some(&lbl), false, &mut has_end_edge);
                }
                if let Some(def) = default {
                    let t = follow_nops(graph, *def);
                    write_mermaid_edge(
                        &mut out,
                        &n,
                        t,
                        Some("default"),
                        true,
                        &mut has_end_edge,
                    );
                }
            }

            // ── Jump: dashed ───────────────────────────────────────────────
            IrNodeKind::Jump { target } => {
                if *target == NODE_END {
                    has_end_edge = true;
                    writeln!(out, "    {n} -.-> __end__").ok();
                } else {
                    let dst = nid(*target);
                    writeln!(out, "    {n} -.-> {dst}").ok();
                }
            }

            // ── LetCall: dashed "call" edge + dashed "ret" edge ───────────
            IrNodeKind::LetCall { target, next, .. } => {
                // Dashed edge to the call target (subroutine entry).
                if *target == NODE_END {
                    has_end_edge = true;
                    writeln!(out, "    {n} -. \"call\" .-> __end__").ok();
                } else {
                    let dst = nid(*target);
                    writeln!(out, "    {n} -. \"call\" .-> {dst}").ok();
                }
                // Dashed edge to the return continuation.
                let ret = follow_nops(graph, *next);
                if ret == NODE_END {
                    has_end_edge = true;
                    writeln!(out, "    {n} -. \"ret\" .-> __end__").ok();
                } else {
                    let ret_dst = nid(ret);
                    writeln!(out, "    {n} -. \"ret\" .-> {ret_dst}").ok();
                }
            }

            // ── Choice fan-out ─────────────────────────────────────────────
            IrNodeKind::Choice { options, .. } => {
                for (i, opt) in options.iter().enumerate() {
                    let entry = follow_nops(graph, opt.entry);
                    let lbl = format!("[{i}] {}", truncate(&opt.label, 24));
                    let escaped_lbl = escape_mermaid(&lbl);
                    let dst = if entry == NODE_END {
                        has_end_edge = true;
                        "__end__".to_string()
                    } else {
                        nid(entry)
                    };
                    writeln!(out, "    {n} -->|\"{escaped_lbl}\"| {dst}").ok();
                }
            }

            // ── Return ─────────────────────────────────────────────────────
            // If this Return lives inside a subroutine label, draw dashed ↩
            // back-edges to each caller's ret continuation.  A bare top-level
            // return still goes to __end__.
            IrNodeKind::Return { .. } => {
                let caller_rets: Option<&Vec<NodeId>> = node_to_cluster
                    .get(&node.id)
                    .and_then(|label_name| graph.labels.get(*label_name))
                    .and_then(|entry_id| callee_to_rets.get(entry_id));

                match caller_rets {
                    Some(ret_nodes) if !ret_nodes.is_empty() => {
                        for &ret in ret_nodes {
                            let resolved = follow_nops(graph, ret);
                            if resolved == NODE_END {
                                has_end_edge = true;
                                writeln!(out, "    {n} -. \"↩\" .-> __end__").ok();
                            } else {
                                let dst = nid(resolved);
                                writeln!(out, "    {n} -. \"↩\" .-> {dst}").ok();
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
            IrNodeKind::End => {}

            // Nop is skipped at the top of the loop.
            IrNodeKind::Nop { .. } => unreachable!("Nop should have been skipped"),
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
    id: NodeId,
    kind: &IrNodeKind,
    label_by_entry: &HashMap<NodeId, String>,
) -> String {
    let n = nid(id);
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

        IrNodeKind::Switch { arms, default, .. } => {
            let total = arms.len() + usize::from(default.is_some());
            let text = escape_mermaid(&format!("match ({total} arms)"));
            format!("{n}{{\"{text}\"}}:::branch")
        }

        // ── Jump ───────────────────────────────────────────────────────────
        IrNodeKind::Jump { target } => {
            let dest = label_by_entry
                .get(target)
                .map(|name| format!("jump → {name}"))
                .unwrap_or_else(|| format!("jump → {}", nid(*target)));
            let text = escape_mermaid(&dest);
            format!("{n}>\"{text}\"]:::jump")
        }

        // ── LetCall ────────────────────────────────────────────────────────
        IrNodeKind::LetCall { var, target, .. } => {
            let callee = label_by_entry
                .get(target)
                .cloned()
                .unwrap_or_else(|| nid(*target));
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
        IrNodeKind::EnterScope { label, .. } => {
            let text = escape_mermaid(&format!("▶ {label}"));
            format!("{n}{{\"{text}\"}}:::scope")
        }

        IrNodeKind::ExitScope { label, .. } => {
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
        IrNodeKind::Nop { .. } => {
            // Should never be reached — Nop nodes are filtered before calling
            // this function.  Emit a visible placeholder so bugs are obvious.
            format!("{n}[\"● {}\"]", id.0)
        }

        // ── End ────────────────────────────────────────────────────────────
        IrNodeKind::End => {
            format!("{n}(((\"end\"))):::endNode")
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

// ─── Edge helper ─────────────────────────────────────────────────────────────

/// Emits a single Mermaid edge from `src` to `dst`.
///
/// `dashed` uses `-.->` syntax; otherwise uses `-->`.
/// If `dst` is [`NODE_END`] the edge targets `__end__` and sets `has_end_edge`.
fn write_mermaid_edge(
    out: &mut String,
    src: &str,
    dst: NodeId,
    label: Option<&str>,
    dashed: bool,
    has_end_edge: &mut bool,
) {
    let dst_str = if dst == NODE_END {
        *has_end_edge = true;
        "__end__".to_string()
    } else {
        nid(dst)
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

// ─── Dialogue content extraction ─────────────────────────────────────────────

/// Extracts a human-readable comma-separated speaker string.
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

/// Formats a [`NodeId`] as a Mermaid node identifier string (e.g. `N42`).
fn nid(id: NodeId) -> String {
    format!("N{}", id.0)
}

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
fn sanitize_id(s: &str) -> String {
    let s = s.replace("::", "_").replace(['.', ' ', '-'], "_");
    s.chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
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
        RuntimeValue::Label { name, .. } => format!(":{name}"),
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

    // ── Header ───────────────────────────────────────────────────────────────

    #[test]
    fn test_mermaid_header() {
        let mmd = compile(Ast::block(vec![])).to_mermaid();
        assert!(
            mmd.contains("flowchart TD"),
            "must contain flowchart TD, got: {mmd}"
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
        assert!(mmd.contains(":::assign"), "assign node must have assign class");
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
        assert_eq!(truncate("hello", 10), "hello");
    }

    #[test]
    fn test_truncate_long_appends_ellipsis() {
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
