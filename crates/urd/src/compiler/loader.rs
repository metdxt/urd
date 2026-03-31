//! Multi-file compilation: import resolution, recursive compilation, and source parsing.

use std::collections::{HashMap, HashSet};

use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::{
    ir::{IrEdge, IrGraph, IrNodeKind},
    parser::ast::{Ast, AstContent},
};

use super::{CompilerError, CompilerState};

// ─── Multi-file compilation helpers ──────────────────────────────────────────

/// Compile `ast` recursively, resolving `import` statements via `loader`.
///
/// A fresh [`CompilerState`] is created for each module. Pass 0 merges all
/// imported modules before the label-scan and emit passes so that cross-module
/// labels are visible throughout.
pub(super) fn compile_recursive(
    ast: &Ast,
    loader: &dyn crate::vm::loader::FileLoader,
    in_progress: &mut HashSet<String>,
    completed: &mut HashSet<String>,
) -> Result<IrGraph, CompilerError> {
    let mut state = CompilerState::new();

    // Pass 0: collect all imports and pre-merge them into the graph BEFORE
    // the label scan pass. This ensures cross-module labels are in the graph's
    // labels map before pass 2 tries to resolve jumps to them.
    collect_imports(ast, &mut state, loader, in_progress, completed)?;

    // Pass 1: scan local labels (imports are already merged).
    state.scan_labels(ast);

    // Pass 2: emit IR nodes for the current module (main entry).
    // Uses top-level partitioning so that @entry-decorated labels are
    // recognised and preamble definitions chain into the entry label.
    let main_entry = state.compile_top_level(ast)?;

    // Chain imported module prologues before the main entry so that
    // DefineEnum / global-assignment nodes from imported modules are executed
    // before any code in the importing module runs. Without this, a reference
    // like `chars.Faction.Rebel` would fail at runtime because `DefineEnum`
    // for `chars::Faction` had never been executed.
    let entry = chain_prologues(&mut state.graph, &state.import_prologues, main_entry);
    state.graph.entry = entry;

    Ok(state.graph)
}

/// Pass 0: walk the AST looking for `Import` nodes. For each one, load, parse,
/// and compile the imported module, then merge it into `state.graph`.
pub(super) fn collect_imports(
    ast: &Ast,
    state: &mut CompilerState,
    loader: &dyn crate::vm::loader::FileLoader,
    in_progress: &mut HashSet<String>,
    completed: &mut HashSet<String>,
) -> Result<(), CompilerError> {
    match ast.content() {
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_imports(stmt, state, loader, in_progress, completed)?;
            }
        }
        AstContent::Import { path, symbols } => {
            // Determine whether this is a whole-module import (`import "p" as alias`,
            // where the single symbol entry has `original: None`) or a symbol import
            // (`import sym from "p"` / `import (a, b) from "p"`).
            let is_whole_module = symbols.first().map_or(false, |s| s.original.is_none());

            if is_whole_module {
                // ── Whole-module import ───────────────────────────────────────
                // The alias is the namespace prefix: every label in the module
                // becomes "alias::label_name" in the merged graph.
                let alias = symbols[0].alias.as_str();

                // True cycle: this module is already on the current call stack.
                if in_progress.contains(path.as_str()) {
                    return Err(CompilerError::CircularImport(path.clone()));
                }
                // Diamond / repeated import: already compiled successfully.
                // Re-merge under the new alias without re-running the prologue.
                if completed.contains(path.as_str()) {
                    // Re-load and re-merge so the alias (label namespace) mapping
                    // is registered even for the second importer, but don't push
                    // to import_prologues — the first compilation already queued
                    // this module's prologue. Re-running it would redeclare
                    // top-level `const` bindings and cause a TypeError.
                    let src = loader
                        .load(path)
                        .map_err(|msg| CompilerError::ModuleLoadError {
                            path: path.clone(),
                            message: msg,
                        })?;
                    let module_ast =
                        parse_source(&src).map_err(|msg| CompilerError::ModuleLoadError {
                            path: path.clone(),
                            message: msg,
                        })?;
                    let mut inner = CompilerState::new();
                    inner.scan_labels(&module_ast);
                    let entry = inner.compile_top_level(&module_ast)?;
                    inner.graph.entry = entry;
                    state.graph.merge(inner.graph, alias);
                    // Intentionally NOT pushing to state.import_prologues here.
                    return Ok(());
                }

                // Mark as in-progress before descending.
                in_progress.insert(path.clone());

                // Load and parse the source text.
                let src = loader
                    .load(path)
                    .map_err(|msg| CompilerError::ModuleLoadError {
                        path: path.clone(),
                        message: msg,
                    })?;
                let module_ast =
                    parse_source(&src).map_err(|msg| CompilerError::ModuleLoadError {
                        path: path.clone(),
                        message: msg,
                    })?;

                // Compile the module recursively (handles transitive imports).
                let module_graph = compile_recursive(&module_ast, loader, in_progress, completed)?;

                in_progress.remove(path.as_str());
                completed.insert(path.clone());

                // Merge into our graph, namespacing all labels as "alias::label".
                // `merge` returns an old→new NodeIndex map. We use it to translate
                // the module's entry point into a NodeIndex valid in the merged graph.
                // We skip the prologue entry when the module's entry node is an
                // EnterScope (meaning the module contains only labels and no
                // top-level statements). In that case there is nothing to run as
                // a prologue, and jumping to the entry would execute the first
                // label body unconditionally.
                let module_entry = module_graph.entry;
                let index_map: HashMap<NodeIndex, NodeIndex> =
                    state.graph.merge(module_graph, alias);
                if let Some(old_entry) = module_entry {
                    if let Some(&entry_idx) = index_map.get(&old_entry) {
                        let entry_is_label = matches!(
                            state.graph.graph.node_weight(entry_idx),
                            Some(IrNodeKind::EnterScope { .. })
                        );
                        if !entry_is_label {
                            state.import_prologues.push(entry_idx);
                        }
                    }
                }
            } else {
                // ── Symbol import ─────────────────────────────────────────────
                // Forms:
                //   import sym as alias from "path"
                //   import (sym1 as a1, sym2) from "path"
                //
                // Strategy:
                //   1. Compile the module (once, recursively).
                //   2. Merge its graph into ours with an empty namespace so its
                //      labels are stored as "::label_name" internally (users
                //      never address those directly).
                //   3. For each requested symbol, look up "::original_name" in
                //      the merged graph's labels map and insert the user alias.

                // True cycle guard.
                if in_progress.contains(path.as_str()) {
                    return Err(CompilerError::CircularImport(path.clone()));
                }

                // Helper: after merging with empty namespace, labels from `other`
                // live as "::label_name" in `state.graph.labels`. For each
                // requested symbol, look up "::original_name" and register the
                // user-specified alias directly in graph.labels.
                let apply_aliases = |graph: &mut IrGraph,
                                     symbols: &[crate::parser::ast::ImportSymbol],
                                     path: &str| {
                    for sym in symbols {
                        if let Some(orig) = &sym.original {
                            let namespaced = format!("::{orig}");
                            if let Some(&merged_id) = graph.labels.get(&namespaced) {
                                graph.labels.insert(sym.alias.clone(), merged_id);
                            } else {
                                log::warn!(
                                    "symbol import: label '{}' not found in module '{}'",
                                    orig,
                                    path
                                );
                            }
                        }
                    }
                };

                if completed.contains(path.as_str()) {
                    // Already compiled — re-compile without recursion to obtain
                    // the label map, merge nodes, then apply aliases.
                    // Do NOT push to import_prologues (prologue already queued).
                    let src = loader
                        .load(path)
                        .map_err(|msg| CompilerError::ModuleLoadError {
                            path: path.clone(),
                            message: msg,
                        })?;
                    let module_ast =
                        parse_source(&src).map_err(|msg| CompilerError::ModuleLoadError {
                            path: path.clone(),
                            message: msg,
                        })?;
                    let mut inner = CompilerState::new();
                    inner.scan_labels(&module_ast);
                    let entry = inner.compile_top_level(&module_ast)?;
                    inner.graph.entry = entry;
                    // Merge with empty namespace; labels land as "::label_name".
                    state.graph.merge(inner.graph, "");
                    apply_aliases(&mut state.graph, symbols, path);
                    return Ok(());
                }

                // First encounter: full recursive compile.
                in_progress.insert(path.clone());
                let src = loader
                    .load(path)
                    .map_err(|msg| CompilerError::ModuleLoadError {
                        path: path.clone(),
                        message: msg,
                    })?;
                let module_ast =
                    parse_source(&src).map_err(|msg| CompilerError::ModuleLoadError {
                        path: path.clone(),
                        message: msg,
                    })?;
                let module_graph = compile_recursive(&module_ast, loader, in_progress, completed)?;

                in_progress.remove(path.as_str());
                completed.insert(path.clone());

                // Merge with empty namespace; labels land as "::label_name".
                let module_entry = module_graph.entry;
                let index_map: HashMap<NodeIndex, NodeIndex> = state.graph.merge(module_graph, "");

                // Chain prologue if the module has top-level executable nodes.
                if let Some(old_entry) = module_entry {
                    if let Some(&entry_idx) = index_map.get(&old_entry) {
                        let entry_is_label = matches!(
                            state.graph.graph.node_weight(entry_idx),
                            Some(IrNodeKind::EnterScope { .. })
                        );
                        if !entry_is_label {
                            state.import_prologues.push(entry_idx);
                        }
                    }
                }

                apply_aliases(&mut state.graph, symbols, path);
            }
        }
        // Recurse into other block-like constructs so that nested imports are found.
        AstContent::If {
            then_block,
            else_block,
            ..
        } => {
            collect_imports(then_block, state, loader, in_progress, completed)?;
            if let Some(eb) = else_block {
                collect_imports(eb, state, loader, in_progress, completed)?;
            }
        }
        AstContent::Menu { options } => {
            for opt in options {
                if let AstContent::MenuOption { content, .. } = opt.content() {
                    collect_imports(content, state, loader, in_progress, completed)?;
                }
            }
        }
        AstContent::Match { arms, .. } => {
            for arm in arms {
                collect_imports(&arm.body, state, loader, in_progress, completed)?;
            }
        }
        _ => {}
    }
    Ok(())
}

/// Chain a sequence of prologue entry points before `main_entry` so that each
/// imported module's top-level nodes (DefineEnum, global assignments, etc.) run
/// first.
///
/// For each prologue the function walks its linear chain following
/// [`IrEdge::Next`] edges until it reaches the tail, then redirects that tail
/// to continue at the next link in the chain. The first prologue's entry
/// becomes the new overall entry point.
///
/// Non-linear nodes (Branch, Switch, Choice, etc.) terminate the walk early;
/// they will not appear in a well-formed module prologue.
fn chain_prologues(
    graph: &mut IrGraph,
    prologues: &[NodeIndex],
    main_entry: Option<NodeIndex>,
) -> Option<NodeIndex> {
    if prologues.is_empty() {
        return main_entry;
    }

    // Build the chain right-to-left: the last prologue's tail → main_entry,
    // then each earlier prologue's tail → the next prologue's entry.
    let mut next_start = main_entry;
    for &prologue in prologues.iter().rev() {
        if let Some(new_next) = next_start {
            patch_prologue_tail(graph, prologue, new_next);
        }
        next_start = Some(prologue);
    }
    next_start
}

/// Walk the linear chain starting at `from`, following [`IrEdge::Next`] edges,
/// and redirect the chain so that instead of falling into a label body or
/// terminating, execution continues at `new_next`.
///
/// Two stopping conditions are handled:
///
/// 1. A node with **no outgoing `Next` edge** — add a `Next` edge to `new_next`.
/// 2. A node whose `Next` edge points at an `EnterScope` (a label entry point)
///    or any other non-linear node — remove that edge and redirect to `new_next`,
///    bypassing the label body entirely.
///
/// This ensures that only top-level prologue nodes (DefineEnum, global
/// assignments, etc.) run as part of the imported module's prologue, and
/// label bodies remain reachable only via explicit `jump` instructions.
fn patch_prologue_tail(graph: &mut IrGraph, from: NodeIndex, new_next: NodeIndex) {
    let mut current = from;
    let mut visited = HashSet::new();

    loop {
        if !visited.insert(current) {
            // Cycle detected — give up.
            break;
        }

        // Only walk nodes that are valid linear prologue nodes.
        let is_walkable = matches!(
            graph.graph.node_weight(current),
            Some(
                IrNodeKind::Assign { .. }
                    | IrNodeKind::DefineEnum { .. }
                    | IrNodeKind::DefineScriptDecorator { .. }
                    | IrNodeKind::Nop
                    | IrNodeKind::Eval { .. }
            )
        );
        if !is_walkable {
            break;
        }

        // Find the outgoing Next edge from `current`, if any.
        // Collect into an owned value to avoid holding a borrow on `graph`.
        let next_edge_info: Option<(petgraph::stable_graph::EdgeIndex, NodeIndex)> = graph
            .graph
            .edges(current)
            .find(|e| matches!(e.weight(), IrEdge::Next))
            .map(|e| (e.id(), e.target()));

        match next_edge_info {
            None => {
                // No outgoing Next edge — this is the tail. Extend the chain.
                graph.add_edge(current, new_next, IrEdge::Next);
                break;
            }
            Some((edge_id, next_target)) => {
                // Check whether the successor is a "stop" node that should not
                // be included in the prologue walk (labels, non-linear nodes,
                // or terminals).
                let should_patch = matches!(
                    graph.graph.node_weight(next_target),
                    Some(
                        IrNodeKind::EnterScope { .. }
                            | IrNodeKind::ExitScope { .. }
                            | IrNodeKind::Dialogue { .. }
                            | IrNodeKind::Choice { .. }
                            | IrNodeKind::Branch { .. }
                            | IrNodeKind::Switch { .. }
                            | IrNodeKind::Jump
                            | IrNodeKind::End
                    )
                );

                if should_patch {
                    // Redirect: remove old Next edge and add one to new_next.
                    graph.graph.remove_edge(edge_id);
                    graph.add_edge(current, new_next, IrEdge::Next);
                    break;
                }

                // Next target is itself a prologue-compatible node — keep walking.
                current = next_target;
            }
        }
    }
}

/// Parse a raw Urd source string into an [`Ast`]. Returns `Err(message)` on
/// parse failure, collecting all parse errors into a single semicolon-separated
/// string.
pub fn parse_source(src: &str) -> Result<Ast, String> {
    parse_source_spanned(src).map_err(|errs| {
        errs.iter()
            .map(|(msg, _)| msg.as_str())
            .collect::<Vec<_>>()
            .join("; ")
    })
}

/// Like [`parse_source`] but returns each parse error together with its
/// byte-offset span so callers (e.g. the LSP) can place diagnostics at
/// the correct source location.
pub fn parse_source_spanned(src: &str) -> Result<Ast, Vec<(String, chumsky::span::SimpleSpan)>> {
    use chumsky::input::Stream;
    use chumsky::prelude::*;
    use chumsky::span::SimpleSpan;

    use crate::lexer::{Token, lex_src};
    use crate::parser::block::script;

    let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(e) => (Token::Error(e), span.into()),
    });

    let stream =
        Stream::from_iter(lexer).map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

    script().parse(stream).into_result().map_err(|errs| {
        errs.iter()
            .map(|e| (e.to_string(), *e.span()))
            .collect::<Vec<_>>()
    })
}
