//! Multi-file compilation: import resolution, recursive compilation, and source parsing.

use std::collections::HashSet;

use crate::{
    ir::{IrGraph, IrNodeKind, NODE_END, NodeId},
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
    let main_entry = state.compile_node(ast, NODE_END)?;

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
        AstContent::Import { path, alias } => {
            // True cycle: this module is already on the current call stack.
            if in_progress.contains(path.as_str()) {
                return Err(CompilerError::CircularImport(path.clone()));
            }
            // Diamond / repeated import: already compiled successfully, skip.
            // The caller can still merge it under a different alias if needed,
            // but we don't re-compile it.  For now we treat a repeated import
            // under the same or a different alias as a no-op on the second
            // encounter (the labels were already merged the first time under
            // the first alias; a second merge under the same alias is fine too,
            // but the most common case is simply "both A and B import C").
            if completed.contains(path.as_str()) {
                // Re-load and re-merge so the alias (label namespace) mapping is
                // registered even for the second importer, but don't recurse and
                // don't push to import_prologues — the first compilation already
                // queued this module's prologue for execution. Re-running it would
                // redeclare top-level `const` bindings and cause a TypeError.
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
                // Compile without recursion (no imports re-processed).
                let mut inner = CompilerState::new();
                inner.scan_labels(&module_ast);
                let entry = inner.compile_node(&module_ast, NODE_END)?;
                inner.graph.entry = entry;
                state.graph.merge(inner.graph, alias);
                // Intentionally NOT pushing to state.import_prologues here.
                return Ok(());
            }

            // Mark as in-progress before descending.
            in_progress.insert(path.clone());

            // Load the source text.
            let src = loader
                .load(path)
                .map_err(|msg| CompilerError::ModuleLoadError {
                    path: path.clone(),
                    message: msg,
                })?;

            // Parse the source into an AST.
            let module_ast = parse_source(&src).map_err(|msg| CompilerError::ModuleLoadError {
                path: path.clone(),
                message: msg,
            })?;

            // Compile the module recursively (handles transitive imports).
            let module_graph = compile_recursive(&module_ast, loader, in_progress, completed)?;

            // Pop from in-progress and mark as completed.
            in_progress.remove(path.as_str());
            completed.insert(path.clone());

            // Merge into our graph, namespacing all labels as "alias::label".
            // Record the (offset-adjusted) module entry so we can chain its
            // prologue (DefineEnum, global assignments, etc.) to run before the
            // importing module's own code.
            //
            // We skip the prologue entry when the module's entry node is an
            // EnterScope (meaning the module contains only labels and no
            // top-level statements). In that case there is nothing to run as a
            // prologue, and jumping to the entry would execute the first label
            // body unconditionally.
            let module_entry = module_graph.entry;
            let offset = state.graph.merge(module_graph, alias);
            let adjusted_entry = NodeId(module_entry.0 + offset);
            let entry_is_label = matches!(
                state
                    .graph
                    .nodes
                    .get(adjusted_entry.as_index())
                    .map(|n| &n.kind),
                Some(IrNodeKind::EnterScope { .. })
            );
            if !entry_is_label {
                state.import_prologues.push(adjusted_entry);
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
/// For each prologue the function walks its linear chain following `next`
/// pointers until it reaches a node whose `next` is `NODE_END`, then patches
/// that tail to continue at the next link in the chain. The first prologue's
/// entry becomes the new overall entry point.
///
/// Non-linear nodes (Branch, Switch, Choice, etc.) terminate the walk early;
/// they will not appear in a well-formed module prologue.
fn chain_prologues(graph: &mut IrGraph, prologues: &[NodeId], main_entry: NodeId) -> NodeId {
    if prologues.is_empty() {
        return main_entry;
    }

    // Build the chain right-to-left: the last prologue's tail → main_entry,
    // then each earlier prologue's tail → the next prologue's entry.
    let mut next_start = main_entry;
    for &prologue in prologues.iter().rev() {
        if prologue == NODE_END {
            continue;
        }
        patch_prologue_tail(graph, prologue, next_start);
        next_start = prologue;
    }
    next_start
}

/// Walk the linear chain starting at `from`, following `next` pointers, and
/// redirect the chain so that instead of falling into a label body or hitting
/// `NODE_END`, execution continues at `new_next`.
///
/// Two stopping conditions are handled:
///
/// 1. A node whose `next == NODE_END` — patch it directly to `new_next`.
/// 2. A node whose `next` is an `EnterScope` (a label entry point) or any
///    other non-linear node — patch *that* node's `next` to `new_next`,
///    bypassing the label body entirely.
///
/// This ensures that only top-level prologue nodes (DefineEnum, global
/// assignments, etc.) run as part of the imported module's prologue, and
/// label bodies remain reachable only via explicit `jump` instructions.
fn patch_prologue_tail(graph: &mut IrGraph, from: NodeId, new_next: NodeId) {
    let mut current = from;
    let mut visited = HashSet::new();

    loop {
        if current == NODE_END || !visited.insert(current) {
            break;
        }

        let next = match &graph.nodes[current.as_index()].kind {
            IrNodeKind::Assign { next, .. }
            | IrNodeKind::DefineEnum { next, .. }
            | IrNodeKind::DefineScriptDecorator { next, .. }
            | IrNodeKind::Nop { next }
            | IrNodeKind::Eval { next, .. } => *next,
            // Non-linear or terminal — cannot continue walking.
            _ => break,
        };

        // Determine whether to patch `current` and stop:
        // - `next == NODE_END`: end of the linear prologue, patch here.
        // - `next` points to an EnterScope (label) or other non-prologue node:
        //   patch `current` to redirect past the label into `new_next`.
        let should_patch = if next == NODE_END {
            true
        } else {
            matches!(
                &graph.nodes[next.as_index()].kind,
                IrNodeKind::EnterScope { .. }
                    | IrNodeKind::ExitScope { .. }
                    | IrNodeKind::Dialogue { .. }
                    | IrNodeKind::Choice { .. }
                    | IrNodeKind::Branch { .. }
                    | IrNodeKind::Switch { .. }
                    | IrNodeKind::Jump { .. }
                    | IrNodeKind::End
            )
        };

        if should_patch {
            match &mut graph.nodes[current.as_index()].kind {
                IrNodeKind::Assign { next, .. }
                | IrNodeKind::DefineEnum { next, .. }
                | IrNodeKind::DefineScriptDecorator { next, .. }
                | IrNodeKind::Nop { next }
                | IrNodeKind::Eval { next, .. } => {
                    *next = new_next;
                }
                _ => {}
            }
            break;
        }

        current = next;
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
