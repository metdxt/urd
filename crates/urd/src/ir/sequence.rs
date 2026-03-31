//! Mermaid sequence diagram renderer for [`super::IrGraph`].
//!
//! Produces a `sequenceDiagram` showing how labeled blocks call and interact
//! with each other — modelled after UML protocol sequence diagrams.
//!
//! ## Visual language
//!
//! * Each `label` block appears as a **participant lifeline**.  Participants
//!   are listed in compilation order (smallest entry [`NodeId`] first).
//! * `LetCall` nodes produce a solid call arrow (`->>`) to the callee lifeline
//!   and a dashed return arrow (`-->>`) back to the caller.  The callee's body
//!   is recursively expanded inline, surrounded by Mermaid `activate`/
//!   `deactivate` markers that create the activation box on the lifeline.
//! * `Jump` nodes produce a solid arrow (`->>`) labelled `jump` and are
//!   terminal (no return expected).
//! * `Dialogue` nodes are rendered as `Note over` annotations showing the
//!   speaker and the first line of dialogue (truncated to 45 chars).
//! * `Branch` nodes emit a `Note over` with the condition text and then
//!   **follow the `else_node` branch** so that any code after the if/elif/else
//!   block (where all branches converge to a merge `Nop`) is also emitted.
//! * `Choice` nodes produce an `alt / else / end` block; each option body is
//!   walked recursively (options typically end with a `Jump` or `Return` and
//!   so terminate naturally).
//! * `autonumber` is always emitted so every arrow gets a step number.
//!
//! ## Quick usage
//!
//! ```no_run
//! # use urd::ir::IrGraph;
//! # fn example(graph: IrGraph) {
//! let diagram = graph.to_sequence_mermaid();
//! std::fs::write("script.mmd", diagram).expect("could not write Mermaid file");
//! // Paste the contents into <https://mermaid.live> to view the diagram.
//! # }
//! ```

use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

use crate::parser::ast::{AstContent, Operator};
use crate::runtime::value::RuntimeValue;

use super::render_common::truncate;
use super::{IrGraph, IrNodeKind, NODE_END, NodeId};

// ─── Public surface ───────────────────────────────────────────────────────────

impl IrGraph {
    /// Renders this graph as a Mermaid `sequenceDiagram` string.
    ///
    /// See the [module documentation][self] for usage and visual legend.
    pub fn to_sequence_mermaid(&self) -> String {
        render_sequence(self)
    }
}

/// Renders `graph` as a Mermaid sequence diagram string.
///
/// Prefer calling [`IrGraph::to_sequence_mermaid`] instead of this free
/// function.
pub fn render_sequence(graph: &IrGraph) -> String {
    let mut out = String::new();

    writeln!(out, "sequenceDiagram").ok();
    writeln!(out, "    autonumber").ok();
    writeln!(out).ok();

    // Build reverse map: entry NodeId → label name (for Jump / LetCall targets).
    let label_by_entry: HashMap<NodeId, String> = graph
        .labels
        .iter()
        .map(|(name, &id)| (id, name.clone()))
        .collect();

    // Sort labels by their entry NodeId (ascending = compilation order).
    let mut sorted_labels: Vec<(&String, NodeId)> =
        graph.labels.iter().map(|(name, &id)| (name, id)).collect();
    sorted_labels.sort_by_key(|(_, id)| id.0);

    // Emit participant declarations.
    for (name, _) in &sorted_labels {
        writeln!(out, "    participant {}", pid(name)).ok();
    }
    writeln!(out).ok();

    // Determine the entry label to start walking from.
    let entry_label = find_entry_label(graph);

    // Emit the entry label, recursively expanding calls.
    let mut visited: HashSet<String> = HashSet::new();
    if emit_label(&entry_label, graph, &label_by_entry, &mut visited, &mut out) {
        writeln!(out, "    deactivate {}", pid(&entry_label)).ok();
    }

    out
}

// ─── Decoded action enum ──────────────────────────────────────────────────────

/// A decoded, owned version of a single IR node's "action" for the sequence
/// diagram walk.  All data is cloned out of the graph before this value is
/// constructed so that the borrow checker allows mutable borrows of other
/// state (e.g. `visited`) to coexist.
#[derive(Debug)]
enum Action {
    /// Transparent node — just advance the cursor to `next`.
    Skip(NodeId),
    /// Terminal node — stop walking.
    Done,
    /// Emit a `Note over` annotation with the given text.
    Note { text: String, next: NodeId },
    /// Subroutine call via `LetCall`.
    Call {
        var: String,
        callee: String,
        next: NodeId,
    },
    /// Unconditional fire-and-forget jump to a named label.
    Jump(String),
    /// Conditional branch; we only need the else branch for sequence walking.
    Branch { cond: String, else_id: NodeId },
    /// Menu choice with labelled options.
    Choice(Vec<(String, NodeId)>),
}

// ─── Core walk ───────────────────────────────────────────────────────────────

/// Emits the full body of one label (participant lifeline).
///
/// Marks the label as visited before recursing so that mutual/self-recursive
/// calls are detected and cut off rather than looping forever.
/// Emits `activate`, walks the label body, and returns `true` if the label
/// was actually expanded (i.e. not already visited).
///
/// The caller is responsible for emitting `deactivate` at the appropriate
/// point — for a subroutine call that means *after* the `-->>` return arrow
/// so the activation box closes on the correct message.
fn emit_label(
    label_name: &str,
    graph: &IrGraph,
    label_by_entry: &HashMap<NodeId, String>,
    visited: &mut HashSet<String>,
    out: &mut String,
) -> bool {
    if !visited.insert(label_name.to_string()) {
        // Already being rendered (recursive call) — skip.
        return false;
    }

    let entry_id = match graph.labels.get(label_name) {
        Some(&id) => id,
        None => return false,
    };

    writeln!(out, "    activate {}", pid(label_name)).ok();
    walk(entry_id, label_name, graph, label_by_entry, visited, out);
    true
}

/// Walks the graph from `cursor`, emitting sequence diagram lines for each
/// node until a terminal is reached.
///
/// `current_label` is the name of the participant currently on the "stack"
/// (whose lifeline arrows are drawn from/to).
fn walk(
    mut cursor: NodeId,
    current_label: &str,
    graph: &IrGraph,
    label_by_entry: &HashMap<NodeId, String>,
    visited: &mut HashSet<String>,
    out: &mut String,
) {
    let lid = pid(current_label);
    // Guard against cycles in the node graph (should not happen in well-formed
    // IR, but be defensive).
    let mut seen: HashSet<NodeId> = HashSet::new();

    loop {
        if cursor == NODE_END || cursor.0 as usize >= graph.nodes.len() {
            break;
        }
        if !seen.insert(cursor) {
            // Cycle detected — bail out.
            break;
        }

        // Decode the current node into an Action, cloning all owned data out
        // so we release the shared borrow on `graph` before any mutable
        // borrows of `visited` / `out` occur.
        let action = decode_node(graph, cursor, label_by_entry);

        match action {
            Action::Skip(next) => {
                cursor = next;
                continue;
            }

            Action::Done => break,

            Action::Note { text, next } => {
                writeln!(out, "    Note over {lid}: {text}").ok();
                cursor = next;
                continue;
            }

            Action::Call { var, callee, next } => {
                let callee_lid = pid(&callee);
                let call_label = if var.is_empty() {
                    "⤑".to_string()
                } else {
                    format!("let {var} = ⤑")
                };
                let ret_label = if var.is_empty() {
                    "↩".to_string()
                } else {
                    format!("↩ {var}")
                };

                writeln!(out, "    {lid}->>{callee_lid}: {call_label}").ok();
                let expanded = emit_label(&callee, graph, label_by_entry, visited, out);
                writeln!(out, "    {callee_lid}-->>{lid}: {ret_label}").ok();
                if expanded {
                    writeln!(out, "    deactivate {callee_lid}").ok();
                }

                cursor = next;
                continue;
            }

            Action::Jump(target_label) => {
                let target_lid = pid(&target_label);
                writeln!(out, "    {lid}->>{target_lid}: jump").ok();
                // Jump is fire-and-forget; no return.
                break;
            }

            Action::Branch { cond, else_id } => {
                writeln!(out, "    Note over {lid}: if {cond}").ok();
                // Follow the else_id path — this eventually reaches the merge
                // Nop that points to the post-branch code.
                cursor = else_id;
                continue;
            }

            Action::Choice(options) => {
                for (i, (opt_label, opt_entry)) in options.iter().enumerate() {
                    if i == 0 {
                        writeln!(out, "    alt {opt_label}").ok();
                    } else {
                        writeln!(out, "    else {opt_label}").ok();
                    }
                    // Walk the option body — it normally terminates via Jump
                    // or Return, so this recursive call is bounded.
                    walk(
                        *opt_entry,
                        current_label,
                        graph,
                        label_by_entry,
                        visited,
                        out,
                    );
                }
                writeln!(out, "    end").ok();
                // Choice is terminal for the sequence walk.
                break;
            }
        }
    }
}

/// Decodes a single IR node into an [`Action`], cloning all data out of the
/// graph so callers can release the shared borrow before mutating other state.
fn decode_node(
    graph: &IrGraph,
    cursor: NodeId,
    label_by_entry: &HashMap<NodeId, String>,
) -> Action {
    let kind = &graph.nodes[cursor.0 as usize].kind;

    match kind {
        // ── Transparent / skip ────────────────────────────────────────────
        IrNodeKind::Nop { next }
        | IrNodeKind::Assign { next, .. }
        | IrNodeKind::Eval { next, .. }
        | IrNodeKind::DefineEnum { next, .. }
        | IrNodeKind::DefineScriptDecorator { next, .. }
        | IrNodeKind::EnterScope { next, .. }
        | IrNodeKind::ExitScope { next, .. } => Action::Skip(*next),

        // ── Terminal ──────────────────────────────────────────────────────
        IrNodeKind::End => Action::Done,

        IrNodeKind::Todo => Action::Done,

        IrNodeKind::Return { value } => {
            let text = match value {
                Some(ast) => format!("return {}", truncate(&ast_short(ast), 48)),
                None => "return".to_string(),
            };
            Action::Note {
                text,
                next: NODE_END,
            }
        }

        // ── Dialogue ──────────────────────────────────────────────────────
        IrNodeKind::Dialogue {
            speakers,
            lines,
            next,
            ..
        } => {
            let speaker = first_str(speakers);
            let line = first_str_from_list(lines);
            let text = if speaker.is_empty() {
                format!("\"{}\"", truncate(&line, 45))
            } else {
                format!("{speaker}: \"{}\"", truncate(&line, 45))
            };
            Action::Note { text, next: *next }
        }

        // ── LetCall ───────────────────────────────────────────────────────
        IrNodeKind::LetCall { var, target, next } => {
            let callee = label_by_entry
                .get(target)
                .cloned()
                .unwrap_or_else(|| format!("N{}", target.0));
            Action::Call {
                var: var.clone(),
                callee,
                next: *next,
            }
        }

        // ── Jump ──────────────────────────────────────────────────────────
        IrNodeKind::Jump { target } => {
            let target_label = label_by_entry
                .get(target)
                .cloned()
                .unwrap_or_else(|| format!("N{}", target.0));
            Action::Jump(target_label)
        }

        // ── Branch ────────────────────────────────────────────────────────
        IrNodeKind::Branch {
            condition,
            else_node,
            ..
        } => {
            let cond = truncate(&ast_short(condition), 48);
            Action::Branch {
                cond,
                else_id: *else_node,
            }
        }

        // ── Choice ────────────────────────────────────────────────────────
        IrNodeKind::Choice { options, .. } => {
            let opts: Vec<(String, NodeId)> = options
                .iter()
                .map(|o| (truncate(&o.label, 48), o.entry))
                .collect();
            Action::Choice(opts)
        }

        // ── Switch (rare in sequences — treat as a note) ──────────────────
        IrNodeKind::Switch { default, .. } => {
            let next = default.unwrap_or(NODE_END);
            Action::Note {
                text: "match ⟨expr⟩".to_string(),
                next,
            }
        }
    }
}

// ─── Entry-label discovery ───────────────────────────────────────────────────

/// Walks the graph from `graph.entry`, following transparent nodes, until it
/// finds an [`IrNodeKind::EnterScope`] and returns its label name.
///
/// Falls back to the lexicographically-first label if no `EnterScope` is
/// found in the opening chain.
fn find_entry_label(graph: &IrGraph) -> String {
    let mut cursor = graph.entry;
    for _ in 0..graph.nodes.len() + 1 {
        if cursor == NODE_END || cursor.0 as usize >= graph.nodes.len() {
            break;
        }
        match &graph.nodes[cursor.0 as usize].kind {
            IrNodeKind::EnterScope { label, .. } => return label.clone(),
            IrNodeKind::Nop { next }
            | IrNodeKind::Assign { next, .. }
            | IrNodeKind::Eval { next, .. }
            | IrNodeKind::DefineEnum { next, .. }
            | IrNodeKind::DefineScriptDecorator { next, .. } => {
                cursor = *next;
            }
            _ => break,
        }
    }

    // Fallback: pick whichever label has the smallest entry NodeId.
    graph
        .labels
        .iter()
        .min_by_key(|&(_, id)| id.0)
        .map(|(name, _)| name.clone())
        .unwrap_or_default()
}

// ─── String helpers ───────────────────────────────────────────────────────────

/// Sanitises a label name for use as a Mermaid participant identifier.
///
/// Replaces `"::"` → `"_"`, `"."` → `"_"`, `" "` → `"_"`.
fn pid(s: &str) -> String {
    s.replace("::", "_").replace(['.', ' '], "_")
}

/// Extracts the first plain-string content from an [`Ast`] node.
///
/// Handles:
/// - `Value(Str(_))` — the string itself
/// - `Value(IdentPath(_))` — dot-joined path
/// - `ExprList(_)` — takes the first element and recurses
///
/// Falls back to an empty string if no plain value is found.
fn first_str(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::Value(RuntimeValue::Str(ps)) => ps.to_string(),
        AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
        AstContent::ExprList(items) => items.first().map(first_str).unwrap_or_default(),
        _ => String::new(),
    }
}

/// Like [`first_str`] but treats the AST as a list of dialogue lines, picking
/// the first line's text.
fn first_str_from_list(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::ExprList(items) => items.first().map(scalar_str).unwrap_or_default(),
        _ => scalar_str(ast),
    }
}

/// Extracts a scalar display string from a single-value AST node.
fn scalar_str(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::Value(RuntimeValue::Str(ps)) => ps.to_string(),
        AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
        AstContent::Value(RuntimeValue::Int(i)) => i.to_string(),
        AstContent::Value(RuntimeValue::Float(f)) => format!("{f:.2}"),
        AstContent::Value(RuntimeValue::Bool(b)) => b.to_string(),
        AstContent::Value(RuntimeValue::Null) => "null".into(),
        _ => "⟨expr⟩".into(),
    }
}

/// Returns a short human-readable representation of an AST expression, used
/// for branch condition labels.
fn ast_short(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::Value(_) => scalar_str(ast),
        AstContent::BinOp { op, left, right } => {
            let op_sym = match op {
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Multiply => "*",
                Operator::Divide => "/",
                Operator::DoubleSlash => "//",
                Operator::Percent => "%",
                Operator::Equals => "==",
                Operator::NotEquals => "!=",
                Operator::GreaterThan => ">",
                Operator::LessThan => "<",
                Operator::GreaterThanOrEquals => ">=",
                Operator::LessThanOrEquals => "<=",
                Operator::BitwiseAnd => "&",
                Operator::BitwiseOr => "|",
                Operator::BitwiseXor => "^",
                Operator::LeftShift => "<<",
                Operator::RightShift => ">>",
                Operator::And => "and",
                Operator::Or => "or",
                Operator::Assign => "=",
            };
            format!(
                "{} {op_sym} {}",
                truncate(&ast_short(left), 16),
                truncate(&ast_short(right), 16)
            )
        }
        AstContent::UnaryOp { op, expr } => {
            format!("{op:?} {}", truncate(&ast_short(expr), 20))
        }
        AstContent::Call { func_path, .. } => {
            format!("{}(…)", truncate(&ast_short(func_path), 18))
        }
        AstContent::ExprList(items) => {
            let parts: Vec<_> = items.iter().map(ast_short).collect();
            format!("[{}]", truncate(&parts.join(", "), 24))
        }
        _ => "⟨expr⟩".into(),
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)]
    #![allow(clippy::unwrap_used)]

    use super::*;
    use crate::compiler::Compiler;
    use crate::parser::ast::Ast;

    fn compile(ast: Ast) -> IrGraph {
        Compiler::compile(&ast).expect("compile failed in test")
    }

    fn compile_script(src: &str) -> IrGraph {
        use crate::parse_test;
        use crate::parser::block::script;
        let ast = parse_test!(script(), src).expect("parse failed in test");
        Compiler::compile(&ast).expect("compile failed in test")
    }

    // ── Header / structure ──────────────────────────────────────────────────

    #[test]
    fn sequence_has_header() {
        let out = compile(Ast::block(vec![])).to_sequence_mermaid();
        assert!(
            out.starts_with("sequenceDiagram"),
            "must open with sequenceDiagram"
        );
    }

    #[test]
    fn sequence_has_autonumber() {
        let out = compile(Ast::block(vec![])).to_sequence_mermaid();
        assert!(out.contains("autonumber"), "must contain autonumber");
    }

    // ── Participants ────────────────────────────────────────────────────────

    #[test]
    fn sequence_has_participants_for_all_labels() {
        let script = r#"
label caller {
    jump callee
}
label callee {
    return
}
"#;
        let out = compile_script(script).to_sequence_mermaid();
        assert!(
            out.contains("participant caller"),
            "caller participant missing"
        );
        assert!(
            out.contains("participant callee"),
            "callee participant missing"
        );
    }

    // ── Call pairs ──────────────────────────────────────────────────────────

    #[test]
    fn sequence_shows_let_call_pair() {
        let script = r#"
label caller {
    let result = jump callee and return
}
label callee {
    return "done"
}
"#;
        let out = compile_script(script).to_sequence_mermaid();
        assert!(
            out.contains("caller->>callee: let result = ⤑"),
            "solid call arrow with binding missing;\ngot:\n{out}"
        );
        assert!(
            out.contains("callee-->>caller: ↩ result"),
            "dashed return arrow with binding missing;\ngot:\n{out}"
        );
    }

    #[test]
    fn sequence_shows_discard_call() {
        let script = r#"
label caller {
    jump callee and return
}
label callee {
    return
}
"#;
        let out = compile_script(script).to_sequence_mermaid();
        assert!(
            out.contains("caller->>callee: ⤑"),
            "solid call arrow (discard) missing;\ngot:\n{out}"
        );
        assert!(
            out.contains("callee-->>caller: ↩"),
            "dashed return arrow (discard) missing;\ngot:\n{out}"
        );
    }

    // ── Jump ────────────────────────────────────────────────────────────────

    #[test]
    fn sequence_shows_jump_arrow() {
        let script = r#"
label start {
    jump target
}
label target {
    return
}
"#;
        let out = compile_script(script).to_sequence_mermaid();
        assert!(
            out.contains("start->>target: jump"),
            "jump arrow missing;\ngot:\n{out}"
        );
    }

    // ── Dialogue note ───────────────────────────────────────────────────────

    #[test]
    fn sequence_shows_dialogue_as_note() {
        let script = r#"
label greet {
    <Alice>: "Hello world"
    return
}
"#;
        let out = compile_script(script).to_sequence_mermaid();
        assert!(
            out.contains("Note over greet:"),
            "Note over missing;\ngot:\n{out}"
        );
        assert!(
            out.contains("Alice"),
            "speaker Alice missing from note;\ngot:\n{out}"
        );
        assert!(
            out.contains("Hello world"),
            "dialogue text missing from note;\ngot:\n{out}"
        );
    }

    // ── Activation boxes ────────────────────────────────────────────────────

    #[test]
    fn sequence_activates_entry_label() {
        let script = r#"
label start {
    return
}
"#;
        let out = compile_script(script).to_sequence_mermaid();
        assert!(
            out.contains("activate start"),
            "activate start missing;\ngot:\n{out}"
        );
    }

    #[test]
    fn sequence_deactivates_entry_label() {
        let script = r#"
label start {
    return
}
"#;
        let out = compile_script(script).to_sequence_mermaid();
        assert!(
            out.contains("deactivate start"),
            "deactivate start missing;\ngot:\n{out}"
        );
    }
}
