//! Shared helper functions for IR graph renderers (DOT, Mermaid).
//!
//! These utilities were extracted from [`super::dot`] and [`super::mermaid`]
//! to eliminate duplication.  All items use `pub(super)` visibility so they
//! are accessible to sibling renderer modules but not to the wider crate.

use crate::parser::ast::{AstContent, DeclKind, MatchPattern};
use crate::runtime::value::RuntimeValue;

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

        RuntimeValue::Map(m) => format!("⟨map({})⟩", m.len()),
        RuntimeValue::List(items) => format!("⟨list({})⟩", items.len()),
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
        RuntimeValue::Struct { name, fields } => format!("⟨{}({})⟩", name, fields.len()),
    }
}

// ─── Declaration / decorator helpers ─────────────────────────────────────────

/// Returns the keyword string for a [`DeclKind`].
pub(super) fn decl_kw(kind: &DeclKind) -> &'static str {
    match kind {
        DeclKind::Global => "global",
        DeclKind::Constant => "const",
        DeclKind::Variable => "let",
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
