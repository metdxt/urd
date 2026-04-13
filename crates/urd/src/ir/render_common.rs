//! Shared helper functions for IR graph renderers (DOT, Mermaid).
//!
//! These utilities were extracted from [`super::dot`] and [`super::mermaid`]
//! to eliminate duplication.  All items use `pub(super)` visibility so they
//! are accessible to sibling renderer modules but not to the wider crate.

use std::collections::HashSet;

use petgraph::Direction;
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::parser::ast::{AstContent, DeclKind, MatchPattern};
use crate::runtime::value::RuntimeValue;

use super::{IrEdge, IrGraph, IrNodeKind};

// ─── Dialogue content extraction ─────────────────────────────────────────────

/// Extracts a human-readable comma-separated speaker string from the speakers
/// AST node.
pub(super) fn extract_speakers(ast: &crate::parser::ast::Ast) -> String {
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
pub(super) fn extract_content_lines(ast: &crate::parser::ast::Ast) -> Vec<String> {
    match ast.content() {
        AstContent::ExprList(items) => items.iter().map(extract_line_text).collect(),
        _ => vec![extract_line_text(ast)],
    }
}

/// Extracts the display text of a single dialogue line node.
pub(super) fn extract_line_text(ast: &crate::parser::ast::Ast) -> String {
    match ast.content() {
        AstContent::Value(RuntimeValue::Str(ps)) => format!("{ps}"),
        _ => extract_simple_val(ast),
    }
}

/// Returns a compact display string for a simple value or identifier AST node.
pub(super) fn extract_simple_val(ast: &crate::parser::ast::Ast) -> String {
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

// ─── String helpers ──────────────────────────────────────────────────────────

/// Truncates `s` to at most `max` chars, appending `…` when truncated.
pub(super) fn truncate(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let cut: String = s.chars().take(max).collect();
        format!("{cut}…")
    }
}

// ─── AST / value summary helpers ─────────────────────────────────────────────

/// Returns a short human-readable summary of an AST expression node.
pub(super) fn ast_summary(ast: &crate::parser::ast::Ast) -> String {
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
pub(super) fn rv_summary(rv: &RuntimeValue) -> String {
    match rv {
        RuntimeValue::Null => "null".into(),
        RuntimeValue::Bool(b) => b.to_string(),
        RuntimeValue::Int(i) => i.to_string(),
        RuntimeValue::Float(f) => format!("{f:.3}"),
        RuntimeValue::Str(_) => "\"…\"".into(),
        RuntimeValue::Dice(c, s) => format!("{c}d{s}"),
        RuntimeValue::IdentPath(p) => p.join("."),

        RuntimeValue::Map(m) => format!("⟨map({})⟩", m.borrow().len()),
        RuntimeValue::List(items) => format!("⟨list({})⟩", items.borrow().len()),
        RuntimeValue::Roll(rolls) => format!("⟨roll({})⟩", rolls.len()),
        RuntimeValue::Function { params, .. } => format!("⟨fn({})⟩", params.len()),
        RuntimeValue::ScriptDecorator { .. } => "⟨decorator⟩".into(),
        RuntimeValue::Range {
            start,
            end,
            inclusive,
        } => {
            if *inclusive {
                format!("{start}..={end}")
            } else {
                format!("{start}..{end}")
            }
        }
        RuntimeValue::Struct { name, fields } => format!("⟨{}({})⟩", name, fields.borrow().len()),
        RuntimeValue::Extern(handle) => {
            let type_name = handle.type_name().unwrap_or_else(|_| "extern".into());
            format!("⟨{type_name}⟩")
        }
    }
}

// ─── Declaration / decorator helpers ─────────────────────────────────────────

/// Returns the keyword string for a [`DeclKind`].
pub(super) fn decl_kw(kind: &DeclKind) -> &'static str {
    match kind {
        DeclKind::Global => "global",
        DeclKind::Constant => "const",
        DeclKind::Variable => "let",
        DeclKind::Assignment => "",
    }
}

/// Produces a one-line decorator annotation string, e.g. `@voiced @id(…)`.
pub(super) fn decorator_line(decorators: &[crate::parser::ast::Decorator]) -> String {
    decorators
        .iter()
        .map(|d| format!("@{}", d.name()))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Returns a compact label for a [`MatchPattern`] arm.
pub(super) fn arm_pattern_label(pattern: &MatchPattern) -> String {
    match pattern {
        MatchPattern::Wildcard => "_".into(),
        MatchPattern::Value(inner) => truncate(&ast_summary(inner), 20),
        MatchPattern::Range { .. } | MatchPattern::Array(_) => truncate(&pattern.to_string(), 20),
    }
}

// ─── Preamble helpers ────────────────────────────────────────────────────────

/// Returns `true` for IR node kinds that belong to the script preamble
/// (top-level declarations / definitions that run before the first label).
pub(super) fn is_preamble_kind(kind: &IrNodeKind) -> bool {
    matches!(
        kind,
        IrNodeKind::Assign { .. }
            | IrNodeKind::DefineEnum { .. }
            | IrNodeKind::DefineStruct { .. }
            | IrNodeKind::DefineScriptDecorator { .. }
            | IrNodeKind::DefineFunction { .. }
            | IrNodeKind::ExternDecl { .. }
            | IrNodeKind::Eval { .. }
    )
}

/// Returns a short human-readable summary for a preamble node (assignment,
/// enum definition, etc.).  Used to build the collapsed `__preamble__` node.
///
/// # Panics
///
/// Panics if `kind` is not a preamble node (i.e. `is_preamble_kind` returns
/// `false`).
pub(super) fn preamble_summary(kind: &IrNodeKind) -> String {
    match kind {
        IrNodeKind::Assign { var, scope, .. } => {
            format!("{} {var}", decl_kw(scope))
        }
        IrNodeKind::DefineEnum { name, .. } => format!("enum {name}"),
        IrNodeKind::DefineStruct { name, .. } => format!("struct {name}"),
        IrNodeKind::DefineScriptDecorator { name, .. } => format!("decorator {name}"),
        IrNodeKind::DefineFunction { name, .. } => format!("fn {name}"),
        IrNodeKind::ExternDecl { name } => format!("extern {name}"),
        IrNodeKind::Eval { .. } => "⟨eval⟩".into(),
        _ => unreachable!("preamble_summary called on non-preamble node"),
    }
}

/// Walks the prologue chain from `cursor`, following preamble-style nodes
/// (Assign, DefineEnum, DefineStruct, DefineScriptDecorator, DefineFunction,
/// ExternDecl, Nop, Eval) via their [`IrEdge::Next`] edges, until the first
/// non-preamble node (typically `EnterScope`) or `None` is reached.
pub(super) fn preamble_chain_target(
    graph: &IrGraph,
    cursor: Option<NodeIndex>,
) -> Option<NodeIndex> {
    let mut current = cursor?;
    let mut visited: HashSet<NodeIndex> = HashSet::new();
    loop {
        if !visited.insert(current) {
            return Some(current);
        }
        let kind = graph.graph.node_weight(current)?;
        match kind {
            IrNodeKind::Assign { .. }
            | IrNodeKind::DefineEnum { .. }
            | IrNodeKind::DefineStruct { .. }
            | IrNodeKind::DefineScriptDecorator { .. }
            | IrNodeKind::DefineFunction { .. }
            | IrNodeKind::ExternDecl { .. }
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
