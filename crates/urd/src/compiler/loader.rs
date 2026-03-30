//! Multi-file compilation: import resolution, recursive compilation, and source parsing.

use std::collections::HashSet;

use crate::{
    ir::{IrGraph, NODE_END},
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

    // Pass 2: emit IR nodes.
    let entry = state.compile_node(ast, NODE_END)?;
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
                // Re-load and re-merge so the alias mapping is registered even
                // for the second importer, but don't recurse into it again.
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
            state.graph.merge(module_graph, alias);
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

/// Parse a raw Urd source string into an [`Ast`]. Returns `Err(message)` on
/// parse failure, collecting all parse errors into a single semicolon-separated
/// string.
pub fn parse_source(src: &str) -> Result<Ast, String> {
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
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ")
    })
}
