use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::stable_graph::NodeIndex;

use crate::{
    loc::{EventKind, IdContext, extract_id_override},
    parser::ast::{Ast, AstContent},
    runtime::value::RuntimeValue,
};

use super::CompilerError;

// ─── Forward pre-pass ────────────────────────────────────────────────────────

/// Walk `ast` in **source order** (left-to-right), assigning every
/// localisation ID that `compile_node` would normally assign during its
/// right-to-left pass, and collect them into a [`VecDeque`].
///
/// The queue is built so that **popping from the front** during the
/// right-to-left compilation pass yields each node's correctly
/// source-ordered ID.  The key trick is that for a [`AstContent::Block`] we
/// collect per-statement sub-queues left-to-right, then concatenate them in
/// **reversed** order — this makes front-pops during right-to-left
/// compilation consume IDs in the same order the statements appear in source.
///
/// Non-block containers (`Menu`, `If`, `Match`, `LabeledBlock`) are handled
/// symmetrically in both the pre-pass and the compilation pass, so their
/// sub-queues are appended without reversal.
pub(super) fn preassign_subtree(ast: &Ast, id_ctx: &mut IdContext) -> VecDeque<Option<String>> {
    let override_id = extract_id_override(ast.decorators());
    match ast.content() {
        AstContent::Block(stmts) => {
            // Collect per-statement queues LEFT-TO-RIGHT so counters advance
            // in source order.
            let stmt_queues: Vec<VecDeque<_>> =
                stmts.iter().map(|s| preassign_subtree(s, id_ctx)).collect();
            // Concatenate REVERSED so that pop_front during right-to-left
            // compilation gives each node its source-ordered ID.
            let mut q = VecDeque::new();
            for sub in stmt_queues.into_iter().rev() {
                q.extend(sub);
            }
            q
        }

        AstContent::Dialogue { .. } => {
            let id = id_ctx.next_dialogue_id(override_id);
            let mut q = VecDeque::new();
            q.push_back(id);
            q
        }

        AstContent::Menu { options } => {
            let mut q = VecDeque::new();
            id_ctx.push_container(EventKind::Menu, override_id);
            q.push_back(id_ctx.current_full_path());
            for opt_ast in options {
                if let AstContent::MenuOption {
                    label,
                    content,
                    is_default,
                } = opt_ast.content()
                {
                    if *is_default {
                        // Default/wildcard options never get a localization ID.
                        q.push_back(None);
                    } else {
                        let opt_override = extract_id_override(opt_ast.decorators());
                        q.push_back(id_ctx.next_option_id(label, opt_override));
                    }
                    q.extend(preassign_subtree(content, id_ctx));
                }
            }
            id_ctx.pop_container();
            q
        }

        AstContent::If {
            then_block,
            else_block,
            ..
        } => {
            id_ctx.push_container(EventKind::If, override_id);
            let mut q = preassign_subtree(then_block, id_ctx);
            if let Some(eb) = else_block {
                q.extend(preassign_subtree(eb, id_ctx));
            }
            id_ctx.pop_container();
            q
        }

        AstContent::Match { arms, .. } => {
            id_ctx.push_container(EventKind::Match, override_id);
            let mut q = VecDeque::new();
            for arm in arms {
                q.extend(preassign_subtree(&arm.body, id_ctx));
            }
            id_ctx.pop_container();
            q
        }

        AstContent::LabeledBlock { label, block, .. } => {
            id_ctx.push_label(label, override_id);
            let q = preassign_subtree(block, id_ctx);
            id_ctx.pop_label();
            q
        }

        // All other node kinds produce no localisation IDs.
        _ => VecDeque::new(),
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

/// Extracts the Fluent variable alias from a `@fluent` / `@fluent("alias")` decorator.
///
/// Returns:
/// - `None` if no `@fluent` decorator is present.
/// - `Some(var_name.to_string())` if `@fluent` is bare (no arguments).
/// - `Some(alias)` if `@fluent("alias")` provides an explicit alias.
///
/// The analysis pass has already validated that the argument (if present) is a
/// plain string literal, so this function does not need to re-validate.
pub(super) fn extract_fluent_alias(
    decorators: &[crate::parser::ast::Decorator],
    var_name: &str,
) -> Option<String> {
    let dec = decorators.iter().find(|d| d.name() == "fluent")?;

    let items = match dec.args().content() {
        AstContent::ExprList(items) => items,
        _ => return Some(var_name.to_string()),
    };

    if items.is_empty() {
        return Some(var_name.to_string());
    }

    match items[0].content() {
        AstContent::Value(RuntimeValue::Str(ps)) => Some(ps.to_string()),
        _ => Some(var_name.to_string()),
    }
}

/// Extract a plain variable name string from a name-bearing AST node.
///
/// Accepts:
/// - `Value(IdentPath([name]))` — a simple single-segment identifier
/// - `Value(IdentPath([alias, var]))` — a 2-segment cross-module path; encoded
///   as `"alias::var"` for storage in the globals map.
///
/// # Errors
/// Returns [`CompilerError::InvalidStatement`] for any other node shape.
pub(super) fn extract_name(ast: &Ast) -> Result<String, CompilerError> {
    match ast.content() {
        AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(path))
            if path.len() == 1 =>
        {
            Ok(path[0].clone())
        }
        // Cross-module assignment: `alias.var = value` → store as "alias::var"
        AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(path))
            if path.len() == 2 =>
        {
            Ok(crate::ir::namespace(&path[0], &path[1]))
        }
        other => Err(CompilerError::InvalidStatement(format!(
            "expected an identifier for variable name, got {:?}",
            other
        ))),
    }
}

/// Resolve a label string to a [`NodeIndex`], handling both local and cross-module
/// (dot-notation `alias.label_name`) references.
///
/// Local labels are looked up in `label_placeholders` (populated by the label scan pass).
/// Cross-module labels (`alias.name`) are looked up in the graph's `labels` map, which
/// is pre-populated by the import pass.
pub(super) fn resolve_label(
    label: &str,
    label_placeholders: &HashMap<String, NodeIndex>,
    graph_labels: &HashMap<String, NodeIndex>,
    exported_labels: &Option<HashSet<String>>,
) -> Result<NodeIndex, CompilerError> {
    if let Some(dot_pos) = label.find('.') {
        let alias = &label[..dot_pos];
        let label_name = &label[dot_pos + 1..];
        let namespaced = crate::ir::namespace(alias, label_name);
        match graph_labels.get(&namespaced) {
            Some(&idx) => {
                // If exported_labels is Some (multi-file), enforce the restriction.
                if let Some(exported) = exported_labels
                    && !exported.contains(&namespaced)
                {
                    return Err(CompilerError::PrivateLabel(label.to_owned()));
                }
                Ok(idx)
            }
            None => Err(CompilerError::UnknownLabel(label.to_owned())),
        }
    } else {
        // Try locally-defined labels first, then directly-imported labels.
        label_placeholders
            .get(label)
            .or_else(|| graph_labels.get(label))
            .copied()
            .ok_or_else(|| CompilerError::UnknownLabel(label.to_owned()))
    }
}
