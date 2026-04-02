//! Multi-file compilation: 4-phase flat pipeline, import resolution, and source
//! parsing.
//!
//! ## Why 4 phases?
//!
//! The old recursive pipeline compiled each dependency depth-first, fully
//! finishing it before returning to its importer.  That made circular imports
//! impossible: neither side could finish first.
//!
//! The new **flat pipeline** separates *knowing a label exists* from *emitting
//! its IR*, which breaks the ordering dependency entirely:
//!
//! | Phase | What it does |
//! |-------|-------------|
//! | **1 · Load** | BFS over the import graph. Cycles are *stopped, not errored*. Every module is loaded exactly once. |
//! | **2 · Pre-allocate** | For every label in every module, push one `Nop` placeholder into a single shared `IrGraph`. Each label now has a stable `NodeIndex` before any IR is emitted. |
//! | **3 · Cross-link** | Build `graph.labels` from all `import` statements — whole-module aliases (`alias::label`) and direct symbol aliases — using the pre-allocated `NodeIndex` values. |
//! | **4 · Emit** | Compile each module's IR into the shared graph. All label `NodeIndex`es were reserved in phase 2, so every cross-module jump resolves immediately regardless of emit order. Prologues are chained so deeper dependencies execute first. |
//!
//! Mutual imports (`a.urd` ↔ `b.urd`) now compile cleanly.  The only
//! restriction that remains is a *value-level* cycle: a `const` whose
//! initialiser depends on a `const` in another module that depends back on it.
//! That is a semantic error which produces wrong runtime values; it is not
//! detected at compile time.

use std::collections::{HashMap, HashSet, VecDeque};

use chumsky::span::SimpleSpan;
use petgraph::stable_graph::{EdgeIndex, NodeIndex};
use petgraph::visit::EdgeRef as _;

use crate::{
    ir::{IrEdge, IrGraph, IrNodeKind, namespace},
    parser::ast::{Ast, AstContent},
};

use super::{CompilerError, CompilerState};

// ─── Public entry point ───────────────────────────────────────────────────────

/// Compile `ast` into an [`IrGraph`], resolving `import` statements via
/// `loader`.
///
/// `_in_progress` and `_completed` are retained for API compatibility but are
/// no longer used — the 4-phase flat pipeline handles circular imports without
/// a call-stack depth guard.
pub(super) fn compile_recursive(
    ast: &Ast,
    loader: &dyn crate::vm::loader::FileLoader,
    _in_progress: &mut HashSet<String>,
    _completed: &mut HashSet<String>,
) -> Result<IrGraph, CompilerError> {
    compile_flat(ast, None, loader)
}

/// Like [`compile_recursive`] but also records `root_path` as the canonical
/// filename of `ast`.  When any imported module attempts to re-import the root
/// by that filename the BFS deduplicates it against the already-loaded root
/// instead of re-compiling it — preventing duplicate preamble execution and
/// double-declared constants.
///
/// Use this variant when the root script was loaded from a known file path
/// (e.g. from the CLI or a language server that has the URI).
pub fn compile_recursive_with_root_path(
    ast: &Ast,
    root_path: &str,
    loader: &dyn crate::vm::loader::FileLoader,
) -> Result<IrGraph, CompilerError> {
    compile_flat(ast, Some(root_path), loader)
}

// ─── Top-level orchestration ──────────────────────────────────────────────────

fn compile_flat(
    root_ast: &Ast,
    root_path: Option<&str>,
    loader: &dyn crate::vm::loader::FileLoader,
) -> Result<IrGraph, CompilerError> {
    // Phase 1 — discover and load every reachable module.
    let all = load_all_modules(root_ast, root_path, loader)?;

    // Phase 2 — allocate one Nop stub per label per module in one shared graph.
    let mut shared_graph = IrGraph::new();
    let mut local_labels = pre_allocate_labels(&all, &mut shared_graph)?;

    // If the root has a known on-disk filename, register its label map under
    // that filename too.  Without this, phase 3's `build_global_labels` cannot
    // satisfy a back-import like `import "main.urd" as main` from a module
    // that was loaded as a dependency — it would look for
    // `local_labels.get("main.urd")` and find nothing, so `main.hub` etc.
    // would never be added to `graph.labels`.
    if let Some(rp) = root_path
        && let Some(root_map) = local_labels.get("").cloned()
    {
        local_labels.entry(rp.to_string()).or_insert(root_map);
    }

    // Phase 3 — build the cross-module label map from every import statement.
    let global_labels = build_global_labels(&all, &local_labels);
    shared_graph.labels = global_labels;

    // Phase 4 — emit IR for every module into the shared graph.
    emit_all(&all, local_labels, shared_graph)
}

// ─── Phase 1 · Load ───────────────────────────────────────────────────────────

/// Everything discovered by the BFS load pass.
struct AllModules {
    /// BFS discovery order.  The root module (`path = ""`) is always first.
    order: Vec<String>,
    /// Parsed AST for each module. Key `""` = root.
    asts: HashMap<String, Ast>,
    /// All `import` statements made by each module, keyed by the importer path.
    import_refs: HashMap<String, Vec<ImportRef>>,
}

/// One `import` statement as seen from the importing module.
struct ImportRef {
    /// Path string exactly as written in the source.
    imported_path: String,
    style: ImportStyle,
}

enum ImportStyle {
    /// `import "path" as alias` — every label becomes `alias::label`.
    WholeModule { alias: String },
    /// `import (sym …) from "path"` — each entry is `(original_name, alias)`.
    Symbols { symbols: Vec<(String, String)> },
}

/// BFS-load every reachable module.
///
/// When a module path is encountered that has already been seen (even in a
/// cycle) it is silently skipped — no [`CompilerError::CircularImport`] is
/// raised.  Each module appears in [`AllModules::order`] exactly once.
///
/// `root_path` is the on-disk filename of the root AST (e.g. `"main.urd"`).
/// When provided it is added to `seen` before the BFS begins, so any imported
/// module that tries to re-import the root by that filename is deduplicated
/// against the already-loaded root entry (`""`) instead of being loaded again
/// as a separate module.  Without this, a pattern like
///
/// ```text
/// main.urd  →  tavern.urd  →  import "main.urd" as main
/// ```
///
/// would produce two separate entries for the same source, causing the root's
/// preamble (const / global declarations) to execute twice and trigger a
/// "cannot reassign constant" runtime error.
fn load_all_modules(
    root_ast: &Ast,
    root_path: Option<&str>,
    loader: &dyn crate::vm::loader::FileLoader,
) -> Result<AllModules, CompilerError> {
    let mut order: Vec<String> = Vec::new();
    let mut asts: HashMap<String, Ast> = HashMap::new();
    let mut import_refs: HashMap<String, Vec<ImportRef>> = HashMap::new();

    let mut queue: VecDeque<(String, Ast)> = VecDeque::new();
    let mut seen: HashSet<String> = HashSet::new();

    // Seed with the root (given directly, so its path is the empty string).
    seen.insert(String::new());
    // If the caller knows the root's on-disk filename, register it as an
    // alias in `seen` so that back-imports of e.g. `"main.urd"` are recognised
    // as the already-loaded root and skipped by the BFS instead of being
    // loaded again as a separate module.
    if let Some(rp) = root_path {
        seen.insert(rp.to_string());
    }
    order.push(String::new());
    asts.insert(String::new(), root_ast.clone());
    queue.push_back((String::new(), root_ast.clone()));

    while let Some((current_path, current_ast)) = queue.pop_front() {
        let mut refs: Vec<ImportRef> = Vec::new();
        collect_import_refs_from_ast(&current_ast, &mut refs);

        for iref in &refs {
            // Cycle / diamond: already queued or processed — skip silently.
            if seen.insert(iref.imported_path.clone()) {
                let src = loader.load(&iref.imported_path).map_err(|msg| {
                    CompilerError::ModuleLoadError {
                        path: iref.imported_path.clone(),
                        message: msg,
                    }
                })?;
                let module_ast =
                    parse_source(&src).map_err(|msg| CompilerError::ModuleLoadError {
                        path: iref.imported_path.clone(),
                        message: msg,
                    })?;
                order.push(iref.imported_path.clone());
                asts.insert(iref.imported_path.clone(), module_ast.clone());
                queue.push_back((iref.imported_path.clone(), module_ast));
            }
        }

        import_refs.insert(current_path, refs);
    }

    Ok(AllModules {
        order,
        asts,
        import_refs,
    })
}

/// Walk `ast` recursively and collect every `Import` node into `out`.
fn collect_import_refs_from_ast(ast: &Ast, out: &mut Vec<ImportRef>) {
    match ast.content() {
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_import_refs_from_ast(stmt, out);
            }
        }
        AstContent::Import { path, symbols } => {
            // A single ImportSymbol with `original == None` signals a whole-
            // module import (`import "p" as alias`).
            let is_whole = symbols.first().is_some_and(|s| s.original.is_none());

            let style = if is_whole {
                ImportStyle::WholeModule {
                    alias: symbols[0].alias.clone(),
                }
            } else {
                // Symbol import — collect (original_name, alias) pairs.
                let pairs = symbols
                    .iter()
                    .filter_map(|s| s.original.as_ref().map(|o| (o.clone(), s.alias.clone())))
                    .collect();
                ImportStyle::Symbols { symbols: pairs }
            };

            out.push(ImportRef {
                imported_path: path.clone(),
                style,
            });
        }
        AstContent::If {
            then_block,
            else_block,
            ..
        } => {
            collect_import_refs_from_ast(then_block, out);
            if let Some(eb) = else_block {
                collect_import_refs_from_ast(eb, out);
            }
        }
        AstContent::Menu { options } => {
            for opt in options {
                if let AstContent::MenuOption { content, .. } = opt.content() {
                    collect_import_refs_from_ast(content, out);
                }
            }
        }
        AstContent::Match { arms, .. } => {
            for arm in arms {
                collect_import_refs_from_ast(&arm.body, out);
            }
        }
        _ => {}
    }
}

// ─── Phase 2 · Pre-allocate ───────────────────────────────────────────────────

/// For every label in every module push one [`IrNodeKind::Nop`] placeholder
/// into `graph` and record its [`NodeIndex`].
///
/// Returns `module_path → (bare_label_name → NodeIndex)`.  The `NodeIndex`
/// values are valid in `graph` and serve two roles later:
///
/// - As `label_placeholders` during each module's emit phase so that
///   `compile_node` can locate and patch Nop → `EnterScope`.
/// - As the canonical identity of each label when building the global label map.
fn pre_allocate_labels(
    all: &AllModules,
    graph: &mut IrGraph,
) -> Result<HashMap<String, HashMap<String, NodeIndex>>, CompilerError> {
    let mut result: HashMap<String, HashMap<String, NodeIndex>> = HashMap::new();

    for path in &all.order {
        if let Some(ast) = all.asts.get(path) {
            let mut labels: HashMap<String, NodeIndex> = HashMap::new();
            scan_label_nops(ast, graph, &mut labels)?;
            result.insert(path.clone(), labels);
        }
    }

    Ok(result)
}

/// Walk `ast` and push one [`IrNodeKind::Nop`] into `graph` for each
/// [`AstContent::LabeledBlock`] that has not yet been recorded in `labels`.
fn scan_label_nops(
    ast: &Ast,
    graph: &mut IrGraph,
    labels: &mut HashMap<String, NodeIndex>,
) -> Result<(), CompilerError> {
    match ast.content() {
        AstContent::LabeledBlock { label, block } => {
            if labels.contains_key(label) {
                return Err(CompilerError::DuplicateLabel(label.clone()));
            }
            labels.insert(label.clone(), graph.push(IrNodeKind::Nop));
            scan_label_nops(block, graph, labels)?;
        }
        AstContent::Block(stmts) => {
            for stmt in stmts {
                scan_label_nops(stmt, graph, labels)?;
            }
        }
        AstContent::If {
            then_block,
            else_block,
            ..
        } => {
            scan_label_nops(then_block, graph, labels)?;
            if let Some(eb) = else_block {
                scan_label_nops(eb, graph, labels)?;
            }
        }
        AstContent::Menu { options } => {
            for opt in options {
                if let AstContent::MenuOption { content, .. } = opt.content() {
                    scan_label_nops(content, graph, labels)?;
                }
            }
        }
        AstContent::Match { arms, .. } => {
            for arm in arms {
                scan_label_nops(&arm.body, graph, labels)?;
            }
        }
        // DecoratorDef bodies are stored as raw Ast for lazy apply-time
        // evaluation; their labels are private and never externally reachable.
        _ => {}
    }

    Ok(())
}

// ─── Phase 3 · Cross-link ─────────────────────────────────────────────────────

/// Build `graph.labels` from every `import` statement in every module.
///
/// - `import "foo.urd" as f`  → registers `"f::label"` for each label in foo.
/// - `import (sym) from "foo.urd"` → registers `"sym"` directly.
///
/// All [`NodeIndex`] values come from `local_labels` (the Nop stubs from
/// phase 2) so they are immediately valid in the shared graph.
fn build_global_labels(
    all: &AllModules,
    local_labels: &HashMap<String, HashMap<String, NodeIndex>>,
) -> HashMap<String, NodeIndex> {
    let mut global: HashMap<String, NodeIndex> = HashMap::new();

    for refs in all.import_refs.values() {
        for iref in refs {
            let Some(imported_locals) = local_labels.get(&iref.imported_path) else {
                continue;
            };

            match &iref.style {
                ImportStyle::WholeModule { alias } => {
                    for (label_name, &idx) in imported_locals {
                        global.insert(namespace(alias, label_name), idx);
                    }
                }
                ImportStyle::Symbols { symbols } => {
                    for (orig, alias) in symbols {
                        if let Some(&idx) = imported_locals.get(orig.as_str()) {
                            global.insert(alias.clone(), idx);
                        } else {
                            log::warn!(
                                "symbol import: label '{}' not found in '{}'",
                                orig,
                                iref.imported_path
                            );
                        }
                    }
                }
            }
        }
    }

    global
}

// ─── Phase 4 · Emit ───────────────────────────────────────────────────────────

/// Emit IR for every module into `shared_graph` and return the finished graph.
///
/// A single [`CompilerState`] is reused for all modules.  Before each
/// `compile_top_level` call its `label_placeholders` field is replaced with
/// the current module's pre-allocated Nop map.  The shared `graph.labels` (set
/// in phase 3) is never cleared, so every module can resolve cross-module jumps
/// throughout the emit phase.
///
/// **Prologue ordering** — top-level DefineEnum / global-assignment nodes from
/// imported modules must run before the root module's code uses them.  Preamble
/// entries are collected in BFS discovery order (shallowest first), then
/// reversed before calling [`chain_prologues`], so deeper dependencies execute
/// earlier at runtime.
fn emit_all(
    all: &AllModules,
    local_labels: HashMap<String, HashMap<String, NodeIndex>>,
    shared_graph: IrGraph,
) -> Result<IrGraph, CompilerError> {
    let mut state = CompilerState::new();
    // Install the pre-populated shared graph (Nop stubs + global label map).
    state.graph = shared_graph;

    let mut preamble_entries: Vec<NodeIndex> = Vec::new();

    // Emit all imported modules in BFS order (skip the root, emitted last).
    for path in all.order.iter().skip(1) {
        let Some(ast) = all.asts.get(path) else {
            continue;
        };

        state.label_placeholders = local_labels.get(path).cloned().unwrap_or_default();
        let entry = state.compile_top_level(ast)?;

        // Track modules that have a preamble (non-label executable nodes).
        if let Some(idx) = entry {
            let is_scope_entry = matches!(
                state.graph.graph.node_weight(idx),
                Some(IrNodeKind::EnterScope { .. })
            );
            if !is_scope_entry {
                preamble_entries.push(idx);
            }
        }
    }

    // Emit the root module last.
    let root_ast = all.asts.get("").ok_or_else(|| {
        CompilerError::InvalidStatement("internal: root AST missing from compilation unit".into())
    })?;
    state.label_placeholders = local_labels.get("").cloned().unwrap_or_default();
    let root_entry = state.compile_top_level(root_ast)?;

    // Reverse so chain_prologues wires them in deepest-dependency-first order:
    // preamble_entries was collected shallowest-first (BFS); chain_prologues
    // iterates its slice in reverse, so after reversing here the final
    // execution order becomes deepest-first, then root.
    preamble_entries.reverse();

    let entry = chain_prologues(&mut state.graph, &preamble_entries, root_entry);
    state.graph.entry = entry;

    // ── Populate cluster_names and label_sources ──────────────────────────
    // These two maps let renderers (a) deduplicate clusters — one per unique
    // NodeIndex rather than one per alias — and (b) draw file-boundary
    // subgraphs that group labels by source module.
    //
    // label_sources: NodeIndex → source path ("" = root module).
    for (path, mod_labels) in &local_labels {
        for &idx in mod_labels.values() {
            // local_labels may have the root stored under both "" and its
            // filename (e.g. "main.urd"); always record the canonical "" key.
            let canonical_path = if path == all.order.first().map(String::as_str).unwrap_or("") {
                String::new()
            } else {
                path.clone()
            };
            state
                .graph
                .label_sources
                .entry(idx)
                .or_insert(canonical_path);
        }
    }

    // cluster_names: exactly one canonical display name per NodeIndex.
    //   • Root-module labels use their bare name  ("hub").
    //   • Imported-module labels use "alias::name" from the first
    //     whole-module import that covers them, or the symbol alias
    //     for symbol imports.
    //
    // Seed from root first so root labels always win with bare names.
    if let Some(root_labels) = local_labels.get("") {
        for (name, &idx) in root_labels {
            state.graph.cluster_names.insert(idx, name.clone());
        }
    }
    // Walk every import ref; assign a namespaced alias to any label not yet
    // claimed by the root pass above.
    for refs in all.import_refs.values() {
        for iref in refs {
            let Some(mod_labels) = local_labels.get(&iref.imported_path) else {
                continue;
            };
            match &iref.style {
                ImportStyle::WholeModule { alias } => {
                    for (name, &idx) in mod_labels {
                        state
                            .graph
                            .cluster_names
                            .entry(idx)
                            .or_insert_with(|| namespace(alias, name));
                    }
                }
                ImportStyle::Symbols { symbols } => {
                    for (orig, sym_alias) in symbols {
                        if let Some(&idx) = mod_labels.get(orig.as_str()) {
                            state
                                .graph
                                .cluster_names
                                .entry(idx)
                                .or_insert_with(|| sym_alias.clone());
                        }
                    }
                }
            }
        }
    }

    Ok(state.graph)
}

// ─── Prologue chaining ────────────────────────────────────────────────────────

/// Chain a sequence of preamble entry points so that `prologues[0]` runs first,
/// `prologues[1]` second, …, then `main_entry`.
///
/// Pass prologues in deepest-dependency-first order.
fn chain_prologues(
    graph: &mut IrGraph,
    prologues: &[NodeIndex],
    main_entry: Option<NodeIndex>,
) -> Option<NodeIndex> {
    if prologues.is_empty() {
        return main_entry;
    }

    // Build the chain right-to-left so each prologue's tail points at the next.
    let mut next_start = main_entry;
    for &prologue in prologues.iter().rev() {
        if let Some(new_next) = next_start {
            patch_prologue_tail(graph, prologue, new_next);
        }
        next_start = Some(prologue);
    }
    next_start
}

/// Walk the linear preamble chain beginning at `from` (following
/// [`IrEdge::Next`] edges) and redirect its tail to `new_next`.
///
/// Two stopping conditions:
/// 1. A node with **no outgoing `Next` edge** — add one to `new_next`.
/// 2. A node whose `Next` target is a "stop" kind (label entry, dialogue,
///    branch, etc.) — redirect to `new_next`, bypassing the downstream node.
///
/// Non-preamble-compatible node kinds (branches, choices, …) also stop the
/// walk; they should not appear in a well-formed module prologue.
fn patch_prologue_tail(graph: &mut IrGraph, from: NodeIndex, new_next: NodeIndex) {
    let mut current = from;
    let mut visited: HashSet<NodeIndex> = HashSet::new();

    loop {
        if !visited.insert(current) {
            break; // cycle guard
        }

        let is_preamble_node = matches!(
            graph.graph.node_weight(current),
            Some(
                IrNodeKind::Assign { .. }
                    | IrNodeKind::Eval { .. }
                    | IrNodeKind::DefineEnum { .. }
                    | IrNodeKind::DefineScriptDecorator { .. }
                    | IrNodeKind::DefineFunction { .. }
                    | IrNodeKind::Nop
            )
        );
        if !is_preamble_node {
            break;
        }

        // Find the single outgoing Next edge (if any).
        let next_edge: Option<(EdgeIndex, NodeIndex)> = graph
            .graph
            .edges(current)
            .find(|e| matches!(e.weight(), IrEdge::Next))
            .map(|e| (e.id(), e.target()));

        match next_edge {
            None => {
                // Tail of the chain — extend it.
                graph.add_edge(current, new_next, IrEdge::Next);
                break;
            }
            Some((eid, target)) => {
                let is_stop = matches!(
                    graph.graph.node_weight(target),
                    Some(
                        IrNodeKind::EnterScope { .. }
                            | IrNodeKind::ExitScope { .. }
                            | IrNodeKind::Dialogue { .. }
                            | IrNodeKind::Choice { .. }
                            | IrNodeKind::Branch { .. }
                            | IrNodeKind::Switch { .. }
                            | IrNodeKind::Jump
                            | IrNodeKind::LetCall { .. }
                            | IrNodeKind::Return { .. }
                            | IrNodeKind::End
                            | IrNodeKind::Todo
                    )
                );
                if is_stop {
                    // Redirect around the stop node.
                    graph.graph.remove_edge(eid);
                    graph.add_edge(current, new_next, IrEdge::Next);
                    break;
                }
                // Keep walking.
                current = target;
            }
        }
    }
}

// ─── Source parsing ───────────────────────────────────────────────────────────

/// Parse a raw Urd source string into an [`Ast`].
///
/// Returns `Err(message)` on parse failure, with all errors joined by `"; "`.
pub fn parse_source(src: &str) -> Result<Ast, String> {
    use chumsky::{input::Stream, prelude::*};

    use crate::lexer::{Token, lex_src};
    use crate::parser::block::script;

    let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
        Ok(t) => (t, span.into()),
        Err(e) => (Token::Error(e), span.into()),
    });
    let stream =
        Stream::from_iter(lexer).map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

    let (ast, errors) = script().parse(stream).into_output_errors();

    if !errors.is_empty() {
        return Err(errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; "));
    }

    ast.ok_or_else(|| "parser produced no output".to_string())
}

/// Parse a raw Urd source string, returning both the (possibly partial)
/// recovered AST and any parse errors with their byte-offset [`SimpleSpan`]s.
///
/// Chumsky's error recovery can produce a usable AST even when parts of the
/// source are invalid (e.g. the user is mid-edit and has typed `narrator.`
/// which is not yet a complete expression).  Discarding that recovered AST
/// meant the LSP had no symbol information during any incomplete edit.
///
/// Now both are always returned:
/// - `Option<Ast>`:  `Some` when chumsky recovered a (partial) tree, `None`
///   when the source was so broken that no tree could be built at all.
/// - `Vec<(String, SimpleSpan)>`: empty on a clean parse, non-empty otherwise.
///
/// Callers that previously matched on `Ok(ast)` / `Err(errors)` should now
/// use the tuple directly: prefer the fresh AST when present, fall back to a
/// stale cached AST only when `Option<Ast>` is `None`.
pub fn parse_source_spanned(src: &str) -> (Option<Ast>, Vec<(String, SimpleSpan)>) {
    use chumsky::{input::Stream, prelude::*};

    use crate::lexer::{Token, lex_src};
    use crate::parser::block::script;

    let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
        Ok(t) => (t, span.into()),
        Err(e) => (Token::Error(e), span.into()),
    });
    let stream =
        Stream::from_iter(lexer).map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

    let (ast, errors) = script().parse(stream).into_output_errors();

    let spanned_errors = errors
        .iter()
        .map(|e| (e.to_string(), *e.span()))
        .collect::<Vec<_>>();

    (ast, spanned_errors)
}
