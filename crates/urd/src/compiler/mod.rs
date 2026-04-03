//! # Compiler Module
//!
//! This module transforms a parsed [`Ast`] into an [`IrGraph`] ready for VM
//! execution.  Compilation happens in two passes:
//!
//! 1. **Label scan** — walk the entire AST, collect every [`AstContent::LabeledBlock`]
//!    label name, and pre-allocate a [`IrNodeKind::Nop`] placeholder for each so that
//!    forward [`AstContent::Jump`] references can be resolved.
//!
//! 2. **Emit pass** — walk the AST again recursively, emitting [`IrNodeKind`]s into the
//!    graph.  The core function [`CompilerState::compile_node`] returns the *entry*
//!    [`NodeIndex`] of the sub-graph it just emitted; callers thread a `next` continuation
//!    through every call so that every chain is fully linked on the first pass.
//!    Control-flow edges are stored in the graph via [`IrEdge`] rather than inline fields.

pub mod loader;

use std::collections::{HashMap, HashSet};

use petgraph::stable_graph::NodeIndex;
use thiserror::Error;

use crate::{
    ir::{IrChoiceOption, IrEdge, IrGraph, IrNodeKind, SwitchArm},
    lexer::strings::ParsedString,
    loc::{EventKind, IdContext, extract_id_override},
    parser::ast::{Ast, AstContent, DeclKind, MatchPattern, Operator},
    runtime::value::RuntimeValue,
};

// ─── Public error type ────────────────────────────────────────────────────────

/// Errors that can occur during compilation of an [`Ast`] into an [`IrGraph`].
#[derive(Debug, Error)]
pub enum CompilerError {
    /// A [`AstContent::Jump`] targeted a label that was never defined.
    #[error("jump to unknown label `{0}`")]
    UnknownLabel(String),

    /// Two labels with the same name were declared in the same compilation unit.
    #[error("duplicate label definition `{0}`")]
    DuplicateLabel(String),

    /// An AST node appeared at statement level where it is not permitted.
    #[error("invalid statement: {0}")]
    InvalidStatement(String),

    /// Failed to load a module during import resolution.
    #[error("module load error for '{path}': {message}")]
    ModuleLoadError {
        /// The import path that failed to load.
        path: String,
        /// Human-readable description of the failure.
        message: String,
    },

    /// A circular import was detected.
    #[error("circular import detected for '{0}'")]
    CircularImport(String),
}

// ─── Public API ───────────────────────────────────────────────────────────────

/// Stateless entry-point for Urd's AST-to-IR compiler.
///
/// Call [`Compiler::compile`] with a reference to the root [`Ast`] produced by
/// the parser to obtain an [`IrGraph`].
pub struct Compiler;

impl Compiler {
    /// Compile `ast` (the root of a parsed Urd script) into an [`IrGraph`].
    ///
    /// # Errors
    /// Returns [`CompilerError::UnknownLabel`] if a `jump` statement targets a
    /// label that does not exist anywhere in the script.
    ///
    /// Returns [`CompilerError::DuplicateLabel`] when the same label name is
    /// declared more than once in a single compilation unit.
    ///
    /// Returns [`CompilerError::InvalidStatement`] if an expression-only AST
    /// node appears at a position where a statement is expected.
    pub fn compile(ast: &Ast) -> Result<IrGraph, CompilerError> {
        let mut state = CompilerState::new();

        // Pass 1 — collect all label names and pre-allocate Nop placeholders.
        state.scan_labels(ast)?;

        // Pass 2 — emit IR nodes using top-level partitioning (@entry support).
        let entry = state.compile_top_level(ast)?;
        state.graph.entry = entry;

        Ok(state.graph)
    }

    /// Compile `ast` with access to a [`FileLoader`] for resolving `import` statements.
    ///
    /// This is the multi-file variant of [`Compiler::compile`]. When an
    /// `import "path" as alias` statement is encountered, the loader is called
    /// to fetch the source, which is then parsed and compiled recursively
    /// before being merged into the main graph.
    ///
    /// Circular imports are detected via a `visited` set of paths.
    pub fn compile_with_loader(
        ast: &Ast,
        loader: &dyn crate::vm::loader::FileLoader,
    ) -> Result<IrGraph, CompilerError> {
        // `in_progress` tracks modules currently on the recursion stack so we
        // can detect true import cycles (A→B→A).  `completed` tracks modules
        // that have already been fully compiled so we can skip them on
        // subsequent imports of the same path (diamond dependencies).
        let mut in_progress = HashSet::new();
        let mut completed = HashSet::new();
        loader::compile_recursive(ast, loader, &mut in_progress, &mut completed)
    }

    /// Compile `ast` into an [`IrGraph`] with localization ID generation enabled.
    ///
    /// `file_stem` should be the filename without extension (e.g. `"intro"` for `intro.urd`).
    /// All [`IrNodeKind::Dialogue`], [`IrNodeKind::Choice`], and option nodes will have
    /// their `loc_id` fields populated.
    ///
    /// The existing [`Compiler::compile`] remains unchanged — it produces `loc_id: None`
    /// everywhere, keeping all existing tests unaffected.
    pub fn compile_named(ast: &Ast, file_stem: &str) -> Result<IrGraph, CompilerError> {
        let mut state = CompilerState {
            graph: IrGraph::new(),
            label_placeholders: HashMap::new(),
            id_ctx: Some(IdContext::new(file_stem)),
        };
        state.scan_labels(ast)?;
        let entry = state.compile_top_level(ast)?;
        state.graph.entry = entry;
        Ok(state.graph)
    }
}

// ─── Speaker normalisation ────────────────────────────────────────────────────

/// Convert the `speakers` AST node so that every `IdentPath` leaf is replaced
/// with an equivalent `Str` value.
///
/// In the bracketless dialogue syntax (`narrator: "text"`) the parser emits
/// speaker names as `Value(IdentPath([…]))`.  Left as-is, the VM's expression
/// evaluator would try to look them up as runtime variables and fail.  Speakers
/// are always *character names*, never variable references, so we canonicalise
/// them to strings here at compile time — e.g. `IdentPath(["Alice"])` becomes
/// `Str("Alice")` and `IdentPath(["chars", "narrator"])` becomes
/// `Str("chars.narrator")`.
///
/// `Str` nodes (from the old `<"Alice">:` form or any other string literal) are
/// passed through unchanged.
fn normalize_speakers(speakers: &Ast) -> Ast {
    match speakers.content() {
        AstContent::ExprList(items) => {
            let normalised: Vec<Ast> = items.iter().map(normalise_speaker_item).collect();
            Ast::expr_list(normalised)
        }
        _ => normalise_speaker_item(speakers),
    }
}

/// Normalise a single speaker item: `IdentPath` → `Str`, everything else
/// unchanged.
fn normalise_speaker_item(ast: &Ast) -> Ast {
    if let AstContent::Value(RuntimeValue::IdentPath(path)) = ast.content() {
        let name = path.join(".");
        Ast::value(RuntimeValue::Str(ParsedString::new_plain(&name)))
    } else {
        ast.clone()
    }
}

// ─── Internal compiler state ──────────────────────────────────────────────────

/// Internal mutable state threaded through both compilation passes.
pub(super) struct CompilerState {
    pub(super) graph: IrGraph,
    /// Maps label names → the [`NodeIndex`] of their pre-allocated Nop placeholder.
    pub(super) label_placeholders: HashMap<String, NodeIndex>,
    /// ID generation context. `None` when no file stem was provided (inline tests, etc.).
    pub(super) id_ctx: Option<IdContext>,
}

impl CompilerState {
    pub(super) fn new() -> Self {
        CompilerState {
            graph: IrGraph::new(),
            label_placeholders: HashMap::new(),
            id_ctx: None,
        }
    }

    // ── Pass 1: label scan ────────────────────────────────────────────────────

    /// Recursively walk `ast` and pre-allocate a [`IrNodeKind::Nop`] for every
    /// [`AstContent::LabeledBlock`] encountered.
    ///
    /// # Errors
    /// Returns [`CompilerError::DuplicateLabel`] if the same label name is
    /// encountered more than once.
    pub(super) fn scan_labels(&mut self, ast: &Ast) -> Result<(), CompilerError> {
        match ast.content() {
            AstContent::LabeledBlock { label, block, .. } => {
                if self.label_placeholders.contains_key(label) {
                    return Err(CompilerError::DuplicateLabel(label.clone()));
                }

                // Pre-allocate the placeholder (we will patch it in pass 2).
                let placeholder = self.graph.push(IrNodeKind::Nop);
                self.label_placeholders.insert(label.clone(), placeholder);
                // Recurse into the block body.
                self.scan_labels(block)?;
            }
            AstContent::Block(stmts) => {
                for stmt in stmts {
                    self.scan_labels(stmt)?;
                }
            }
            AstContent::If {
                then_block,
                else_block,
                ..
            } => {
                self.scan_labels(then_block)?;
                if let Some(eb) = else_block {
                    self.scan_labels(eb)?;
                }
            }
            AstContent::Menu { options } => {
                for opt in options {
                    if let AstContent::MenuOption { content, .. } = opt.content() {
                        self.scan_labels(content)?;
                    }
                }
            }
            AstContent::Match { arms, .. } => {
                for arm in arms {
                    self.scan_labels(&arm.body)?;
                }
            }
            // DecoratorDef bodies are stored as raw Ast for lazy apply-time
            // evaluation — they are never compiled into IR nodes, so any labels
            // inside them are private to the body and must not be pre-allocated
            // in the outer label_placeholders map.
            // All other nodes either have no sub-statements or only expressions.
            _ => {}
        }

        Ok(())
    }

    // ── Top-level compilation with @entry support ─────────────────────────────

    /// Compile a top-level block, partitioning its children into preamble
    /// definitions and labeled blocks.  The `@entry` decorator on a
    /// [`AstContent::LabeledBlock`] determines which label execution begins
    /// at after the preamble runs.
    ///
    /// **Compilation order:**
    /// 1. Labels are compiled first (each independently with `next = None`
    ///    — they are jumped to, not fallen through).
    /// 2. The entry label is determined: `@entry`-decorated label wins,
    ///    otherwise the first label in source order, otherwise `None`.
    /// 3. Preamble definitions are compiled right-to-left, with the last
    ///    definition's `next` pointing at the entry label's placeholder
    ///    [`NodeIndex`].
    /// 4. The returned [`Option<NodeIndex>`] is the first preamble node (so
    ///    definitions execute before entering the label), or the entry label
    ///    directly when there is no preamble.
    ///
    /// If `ast` is not a [`AstContent::Block`], falls back to
    /// [`Self::compile_node`].
    pub(super) fn compile_top_level(
        &mut self,
        ast: &Ast,
    ) -> Result<Option<NodeIndex>, CompilerError> {
        let stmts = match ast.content() {
            AstContent::Block(stmts) => stmts,
            // Not a block — fall back to the general compile path.
            _ => return self.compile_node(ast, None),
        };

        if stmts.is_empty() {
            let id = self.graph.push(IrNodeKind::Nop);
            return Ok(Some(id));
        }

        // Partition into preamble (definitions / non-label statements) and labels.
        let mut preamble: Vec<&Ast> = Vec::new();
        let mut labels: Vec<&Ast> = Vec::new();

        for stmt in stmts {
            match stmt.content() {
                AstContent::LabeledBlock { .. } => labels.push(stmt),
                _ => preamble.push(stmt),
            }
        }

        // ── Phase 1: compile all labels independently ─────────────────────
        // Each label is a jump target; they do not fall through to one another.
        for label_ast in &labels {
            self.compile_node(label_ast, None)?;
        }

        // ── Phase 2: determine the entry label ───────────────────────────
        // Priority: @entry-decorated label > first label in source order > None
        let entry_label_node = self.find_entry_label(&labels);

        // ── Phase 3: compile preamble right-to-left, chaining into entry ──
        if preamble.is_empty() {
            // No preamble — entry is the label directly (or None).
            return Ok(entry_label_node);
        }

        // If there are no labels, preamble chains to None (backward compat
        // for pure-definition modules like characters.urd).
        let mut continuation = entry_label_node;
        for stmt in preamble.iter().rev() {
            continuation = self.compile_node(stmt, continuation)?;
        }

        Ok(continuation)
    }

    /// Find the entry label [`NodeIndex`] among the given label AST nodes.
    ///
    /// 1. If a label has an `@entry` decorator, use its placeholder [`NodeIndex`].
    /// 2. Otherwise, use the first label in source order.
    /// 3. If no labels exist, return `None`.
    fn find_entry_label(&self, labels: &[&Ast]) -> Option<NodeIndex> {
        // Look for @entry decorator first.
        for label_ast in labels {
            if let AstContent::LabeledBlock { label, .. } = label_ast.content()
                && label_ast.decorators().iter().any(|d| d.name() == "entry")
                && let Some(&id) = self.label_placeholders.get(label)
            {
                return Some(id);
            }
        }

        // Fallback: first label in source order.
        if let Some(first) = labels.first()
            && let AstContent::LabeledBlock { label, .. } = first.content()
            && let Some(&id) = self.label_placeholders.get(label)
        {
            return Some(id);
        }

        None
    }

    // ── Pass 2: emit IR nodes ─────────────────────────────────────────────────

    /// Compile `ast` into one or more graph nodes.
    ///
    /// Returns the **entry** [`NodeIndex`] of the sub-graph produced (always
    /// `Some`). The VM will continue to `next` after this sub-graph finishes
    /// (unless the sub-graph contains a [`IrNodeKind::Return`] or
    /// [`IrNodeKind::Jump`], which ignore `next`).
    ///
    /// Control-flow edges (`Next`, `Then`, `Else`, `Jump`, etc.) are added to
    /// the graph via [`IrGraph::add_edge`]; successor information is no longer
    /// stored as inline fields on [`IrNodeKind`] variants.
    fn compile_node(
        &mut self,
        ast: &Ast,
        next: Option<NodeIndex>,
    ) -> Result<Option<NodeIndex>, CompilerError> {
        match ast.content() {
            // ── Block ────────────────────────────────────────────────────────
            AstContent::Block(stmts) => {
                if stmts.is_empty() {
                    // Empty block — emit a Nop so there is always a valid NodeIndex.
                    let id = self.graph.push(IrNodeKind::Nop);
                    if let Some(n) = next {
                        self.graph.add_edge(id, n, IrEdge::Next);
                    }
                    return Ok(Some(id));
                }
                // Compile right-to-left, threading `next` through each statement.
                let mut continuation = next;
                for stmt in stmts.iter().rev() {
                    continuation = self.compile_node(stmt, continuation)?;
                }
                Ok(continuation)
            }

            // ── Declaration ──────────────────────────────────────────────────
            AstContent::Declaration {
                kind,
                decl_name,
                decl_defs,
                ..
            } => {
                let var = extract_name(decl_name)?;
                let fluent_alias = extract_fluent_alias(ast.decorators(), &var);
                let id = self.graph.push(IrNodeKind::Assign {
                    var,
                    scope: kind.clone(),
                    expr: *decl_defs.clone(),
                    fluent_alias,
                });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── Assignment BinOp (x = expr) ──────────────────────────────────
            AstContent::BinOp { op, left, right } if *op == Operator::Assign => {
                // Detect cross-module assignment: `alias.var = value` → store as "alias::var"
                // with Global scope so it persists in the module's namespace.
                let (var, scope) = match left.content() {
                    AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(path))
                        if path.len() == 2 =>
                    {
                        (crate::ir::namespace(&path[0], &path[1]), DeclKind::Global)
                    }
                    _ => (extract_name(left)?, DeclKind::Variable),
                };
                let id = self.graph.push(IrNodeKind::Assign {
                    var,
                    scope,
                    expr: *right.clone(),
                    fluent_alias: None,
                });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── Call (side-effecting) ────────────────────────────────────────
            AstContent::Call { func_path, .. } => {
                // Detect built-in terminators by their canonical name.
                let call_name = match func_path.content() {
                    AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(p))
                        if p.len() == 1 =>
                    {
                        Some(p[0].as_str())
                    }
                    _ => None,
                };

                match call_name {
                    Some("end!") => {
                        let id = self.graph.push(IrNodeKind::End);
                        Ok(Some(id))
                    }
                    Some("todo!") => {
                        let id = self.graph.push(IrNodeKind::Todo);
                        Ok(Some(id))
                    }
                    _ => {
                        let id = self.graph.push(IrNodeKind::Eval { expr: ast.clone() });
                        if let Some(n) = next {
                            self.graph.add_edge(id, n, IrEdge::Next);
                        }
                        Ok(Some(id))
                    }
                }
            }

            // ── If / else ────────────────────────────────────────────────────
            AstContent::If {
                condition,
                then_block,
                else_block,
            } => {
                // Allocate a merge-point Nop that both branches converge to.
                let merge = self.graph.push(IrNodeKind::Nop);
                if let Some(n) = next {
                    self.graph.add_edge(merge, n, IrEdge::Next);
                }

                // Push if_N container for nested dialogue scoping.
                let if_override = if self.id_ctx.is_some() {
                    extract_id_override(ast.decorators())
                } else {
                    None
                };
                if let Some(ctx) = &mut self.id_ctx {
                    ctx.push_container(EventKind::If, if_override);
                }

                let then_entry = self.compile_node(then_block, Some(merge))?.unwrap_or(merge);
                let else_entry = match else_block {
                    Some(eb) => self.compile_node(eb, Some(merge))?.unwrap_or(merge),
                    None => merge,
                };

                if let Some(ctx) = &mut self.id_ctx {
                    ctx.pop_container();
                }

                let id = self.graph.push(IrNodeKind::Branch {
                    condition: *condition.clone(),
                });
                self.graph.add_edge(id, then_entry, IrEdge::Then);
                self.graph.add_edge(id, else_entry, IrEdge::Else);
                Ok(Some(id))
            }

            // ── LabeledBlock ─────────────────────────────────────────────────
            AstContent::LabeledBlock { label, block, .. } => {
                // Retrieve the pre-allocated placeholder NodeIndex from pass 1.
                let placeholder_id = self
                    .label_placeholders
                    .get(label)
                    .copied()
                    .ok_or_else(|| CompilerError::UnknownLabel(label.clone()))?;

                // Push label scope for ID generation.
                let id_override = if self.id_ctx.is_some() {
                    extract_id_override(ast.decorators())
                } else {
                    None
                };
                if let Some(ctx) = &mut self.id_ctx {
                    ctx.push_label(label, id_override);
                }

                // Emit the ExitScope node that runs *after* the block body.
                let exit_id = self.graph.push(IrNodeKind::ExitScope {
                    label: label.clone(),
                });
                if let Some(n) = next {
                    self.graph.add_edge(exit_id, n, IrEdge::Next);
                }

                // Compile the block body, continuing to ExitScope.
                let body_entry = self.compile_node(block, Some(exit_id))?.unwrap_or(exit_id);

                // Patch the placeholder Nop → EnterScope.
                *self.graph.node_mut(placeholder_id).ok_or_else(|| {
                    CompilerError::InvalidStatement(format!(
                        "internal: placeholder node for label '{}' was removed",
                        label
                    ))
                })? = IrNodeKind::EnterScope {
                    label: label.clone(),
                };
                // Add the Next edge from EnterScope to the body entry.
                self.graph
                    .add_edge(placeholder_id, body_entry, IrEdge::Next);

                // Register in the graph's public label map.
                self.graph.labels.insert(label.clone(), placeholder_id);

                // Pop label scope.
                if let Some(ctx) = &mut self.id_ctx {
                    ctx.pop_label();
                }

                Ok(Some(placeholder_id))
            }

            // ── Jump ─────────────────────────────────────────────────────────
            AstContent::Jump {
                label,
                expects_return,
                ..
            } => {
                // Check for cross-module dot-notation: "alias.label_name"
                let target = resolve_label(label, &self.label_placeholders, &self.graph.labels)?;

                if *expects_return {
                    // `jump label and return` — subroutine call without a binding.
                    // Use an empty var name as the "discard return value" sentinel.
                    let id = self.graph.push(IrNodeKind::LetCall { var: String::new() });
                    self.graph.add_edge(id, target, IrEdge::Call);
                    if let Some(n) = next {
                        self.graph.add_edge(id, n, IrEdge::Ret);
                    }
                    Ok(Some(id))
                } else {
                    // Unconditional jump — `next` is intentionally ignored.
                    let id = self.graph.push(IrNodeKind::Jump);
                    self.graph.add_edge(id, target, IrEdge::Jump);
                    Ok(Some(id))
                }
            }

            // ── LetCall ───────────────────────────────────────────────────────
            AstContent::LetCall { name, target, .. } => {
                // Check for cross-module dot-notation: "alias.label_name"
                let target_id =
                    resolve_label(target, &self.label_placeholders, &self.graph.labels)?;

                let id = self.graph.push(IrNodeKind::LetCall { var: name.clone() });
                self.graph.add_edge(id, target_id, IrEdge::Call);
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Ret);
                }
                Ok(Some(id))
            }

            // ── Return ───────────────────────────────────────────────────────
            AstContent::Return { value } => {
                let id = self.graph.push(IrNodeKind::Return {
                    value: value.as_deref().cloned(),
                });
                Ok(Some(id))
            }

            // ── Dialogue ─────────────────────────────────────────────────────
            AstContent::Dialogue { speakers, content } => {
                let id_override = if self.id_ctx.is_some() {
                    extract_id_override(ast.decorators())
                } else {
                    None
                };
                let loc_id = if let Some(ctx) = &mut self.id_ctx {
                    ctx.next_dialogue_id(id_override)
                } else {
                    None
                };
                let id = self.graph.push(IrNodeKind::Dialogue {
                    speakers: normalize_speakers(speakers),
                    lines: *content.clone(),
                    decorators: ast.decorators().to_vec(),
                    loc_id,
                });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── Menu ─────────────────────────────────────────────────────────
            AstContent::Menu { options } => {
                // Push the menu container scope first; get choice_loc_id after push.
                let menu_override = if self.id_ctx.is_some() {
                    extract_id_override(ast.decorators())
                } else {
                    None
                };
                let choice_loc_id = if let Some(ctx) = &mut self.id_ctx {
                    ctx.push_container(EventKind::Menu, menu_override);
                    ctx.current_full_path()
                } else {
                    None
                };

                let mut ir_options = Vec::with_capacity(options.len());
                let mut option_entries: Vec<NodeIndex> = Vec::with_capacity(options.len());

                for opt_ast in options {
                    match opt_ast.content() {
                        AstContent::MenuOption { label, content } => {
                            let opt_override = if self.id_ctx.is_some() {
                                extract_id_override(opt_ast.decorators())
                            } else {
                                None
                            };
                            let opt_loc_id = if let Some(ctx) = &mut self.id_ctx {
                                ctx.next_option_id(label, opt_override)
                            } else {
                                None
                            };

                            let entry = self
                                .compile_node(content, next)?
                                .unwrap_or_else(|| self.graph.push(IrNodeKind::Nop));
                            option_entries.push(entry);
                            ir_options.push(IrChoiceOption {
                                label: label.clone(),
                                decorators: opt_ast.decorators().to_vec(),
                                loc_id: opt_loc_id,
                            });
                        }
                        _ => {
                            return Err(CompilerError::InvalidStatement(
                                "expected MenuOption inside Menu".to_string(),
                            ));
                        }
                    }
                }

                // Pop the menu container.
                if let Some(ctx) = &mut self.id_ctx {
                    ctx.pop_container();
                }

                let id = self.graph.push(IrNodeKind::Choice {
                    options: ir_options,
                    decorators: ast.decorators().to_vec(),
                    loc_id: choice_loc_id,
                });
                for (i, entry) in option_entries.iter().enumerate() {
                    self.graph.add_edge(id, *entry, IrEdge::Option(i));
                }
                Ok(Some(id))
            }

            // ── Match ────────────────────────────────────────────────────────
            AstContent::Match { scrutinee, arms } => {
                // Allocate a merge Nop that all arms converge to.
                let merge = self.graph.push(IrNodeKind::Nop);
                if let Some(n) = next {
                    self.graph.add_edge(merge, n, IrEdge::Next);
                }

                // Push match_N container.
                let match_override = if self.id_ctx.is_some() {
                    extract_id_override(ast.decorators())
                } else {
                    None
                };
                if let Some(ctx) = &mut self.id_ctx {
                    ctx.push_container(EventKind::Match, match_override);
                }

                let mut switch_arms: Vec<SwitchArm> = Vec::with_capacity(arms.len());
                let mut arm_entries: Vec<NodeIndex> = Vec::new();
                let mut default_entry: Option<NodeIndex> = None;

                for arm in arms {
                    let target = self.compile_node(&arm.body, Some(merge))?.unwrap_or(merge);
                    match &arm.pattern {
                        MatchPattern::Wildcard => {
                            default_entry = Some(target);
                        }
                        MatchPattern::Value(_) => {
                            switch_arms.push(SwitchArm {
                                pattern: arm.pattern.clone(),
                            });
                            arm_entries.push(target);
                        }
                    }
                }

                if let Some(ctx) = &mut self.id_ctx {
                    ctx.pop_container();
                }

                let id = self.graph.push(IrNodeKind::Switch {
                    scrutinee: *scrutinee.clone(),
                    arms: switch_arms,
                });
                for (i, arm_entry) in arm_entries.iter().enumerate() {
                    self.graph.add_edge(id, *arm_entry, IrEdge::Arm(i));
                }
                if let Some(def_entry) = default_entry {
                    self.graph.add_edge(id, def_entry, IrEdge::Default);
                }
                Ok(Some(id))
            }

            // ── EnumDecl ─────────────────────────────────────────────────────
            AstContent::EnumDecl { name, variants } => {
                let id = self.graph.push(IrNodeKind::DefineEnum {
                    name: name.clone(),
                    variants: variants.iter().map(|(n, _)| n.clone()).collect(),
                });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── Pure expressions at statement level ──────────────────────────
            // These are wrapped in an Eval node so that any side effects
            // (e.g. a non-Call BinOp that triggers a runtime hook) are preserved.
            AstContent::BinOp { .. }
            | AstContent::UnaryOp { .. }
            | AstContent::ExprList(_)
            | AstContent::Value(_)
            | AstContent::List(_)
            | AstContent::Map(_) => {
                let id = self.graph.push(IrNodeKind::Eval { expr: ast.clone() });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── MenuOption outside a Menu — shouldn't happen ─────────────────
            AstContent::MenuOption { label, .. } => Err(CompilerError::InvalidStatement(format!(
                "MenuOption `{}` appeared outside a Menu",
                label
            ))),

            // ── DecoratorDef ─────────────────────────────────────────────────
            AstContent::DecoratorDef {
                name,
                event_constraint,
                params,
                body,
            } => {
                // Strip type annotations — runtime only needs parameter names.
                let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
                let id = self.graph.push(IrNodeKind::DefineScriptDecorator {
                    name: name.clone(),
                    event_constraint: event_constraint.clone(),
                    params: param_names,
                    body: *body.clone(),
                });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── Subscript (index read at statement level) ────────────────────
            AstContent::Subscript { .. } => {
                let id = self.graph.push(IrNodeKind::Eval { expr: ast.clone() });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── SubscriptAssign (index write) ────────────────────────────────
            AstContent::SubscriptAssign { .. } => {
                let id = self.graph.push(IrNodeKind::Eval { expr: ast.clone() });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── Import ───────────────────────────────────────────────────────
            // Import nodes are fully resolved during pass 0 (collect_imports).
            // At compile-node time they are no-ops that simply fall through.
            AstContent::Import { .. } => {
                let id = self.graph.push(IrNodeKind::Nop);
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── StructDecl ───────────────────────────────────────────────────
            // Struct declarations register the struct schema in the VM environment
            // so that struct constructor calls can build RuntimeValue::Struct instances.
            AstContent::StructDecl { name, fields } => {
                let id = self.graph.push(IrNodeKind::DefineStruct {
                    name: name.clone(),
                    fields: fields.iter().map(|f| f.name.clone()).collect(),
                });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }

            // ── FnDef ────────────────────────────────────────────────────────
            //
            // Named functions are stored as `RuntimeValue::Function` in the
            // environment via `IrNodeKind::DefineFunction`, making them
            // callable by name from any expression that follows in scope.
            //
            // Anonymous functions in statement position are wrapped in an
            // `IrNodeKind::Eval` node; `eval_expr` handles the
            // `AstContent::FnDef { name: None }` arm and produces the
            // `RuntimeValue::Function` value there.
            AstContent::FnDef {
                name, params, body, ..
            } => {
                match name {
                    Some(fn_name) => {
                        // Named function — strip type annotations; store as DefineFunction.
                        let param_names: Vec<String> =
                            params.iter().map(|p| p.name.clone()).collect();
                        let id = self.graph.push(IrNodeKind::DefineFunction {
                            name: fn_name.clone(),
                            params: param_names,
                            body: *body.clone(),
                        });
                        if let Some(n) = next {
                            self.graph.add_edge(id, n, IrEdge::Next);
                        }
                        Ok(Some(id))
                    }
                    None => {
                        // Anonymous function in statement position — emit as Eval.
                        // `eval_expr` handles `FnDef { name: None }` →
                        // `RuntimeValue::Function`.
                        let id = self.graph.push(IrNodeKind::Eval { expr: ast.clone() });
                        if let Some(n) = next {
                            self.graph.add_edge(id, n, IrEdge::Next);
                        }
                        Ok(Some(id))
                    }
                }
            }

            // ── Extern declaration ──────────────────────────────────────────
            AstContent::ExternDeclaration { name, .. } => {
                let var = extract_name(name)?;
                let id = self.graph.push(IrNodeKind::ExternDecl { name: var });
                if let Some(n) = next {
                    self.graph.add_edge(id, n, IrEdge::Next);
                }
                Ok(Some(id))
            }
        }
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
fn extract_fluent_alias(
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
fn extract_name(ast: &Ast) -> Result<String, CompilerError> {
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
fn resolve_label(
    label: &str,
    label_placeholders: &HashMap<String, NodeIndex>,
    graph_labels: &HashMap<String, NodeIndex>,
) -> Result<NodeIndex, CompilerError> {
    if let Some(dot_pos) = label.find('.') {
        let alias = &label[..dot_pos];
        let label_name = &label[dot_pos + 1..];
        let namespaced = crate::ir::namespace(alias, label_name);
        graph_labels
            .get(&namespaced)
            .copied()
            .ok_or_else(|| CompilerError::UnknownLabel(label.to_owned()))
    } else {
        // Try locally-defined labels first, then directly-imported labels.
        label_placeholders
            .get(label)
            .or_else(|| graph_labels.get(label))
            .copied()
            .ok_or_else(|| CompilerError::UnknownLabel(label.to_owned()))
    }
}

// ─── Unit tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ir::{IrEdge, IrGraph, IrNodeKind},
        parser::ast::{Ast, AstContent, DeclKind, MatchArm, MatchPattern},
        runtime::value::RuntimeValue,
        vm::loader::MemLoader,
    };
    use petgraph::stable_graph::NodeIndex;
    use petgraph::visit::EdgeRef;

    // ── decorator tests ───────────────────────────────────────────────────────

    #[test]
    fn test_decorator_def_compiles_to_define_script_decorator() {
        use crate::parser::ast::{DecoratorParam, EventConstraint};

        let body = Ast::block(vec![]);
        let decorator_ast = Ast::decorator_def(
            "shake".to_string(),
            EventConstraint::Any,
            vec![DecoratorParam {
                name: "amount".to_string(),
                type_annotation: None,
            }],
            body,
        );
        let script_ast = Ast::block(vec![decorator_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        let entry_idx = graph.entry.expect("graph must have an entry node");
        assert!(
            matches!(
                node_kind(&graph, entry_idx),
                IrNodeKind::DefineScriptDecorator { name, params, .. }
                if name == "shake" && params == &["amount"]
            ),
            "expected DefineScriptDecorator at entry, got {:?}",
            node_kind(&graph, entry_idx)
        );
    }

    #[test]
    fn test_decorator_def_then_dialogue_chains() {
        use crate::parser::ast::{AstContent, EventConstraint};

        let body = Ast::block(vec![]);
        let dec_ast = Ast::decorator_def("voiced".to_string(), EventConstraint::Any, vec![], body);
        let dialogue_ast = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(Ast::value(RuntimeValue::IdentPath(vec![
                    "Alice".to_string(),
                ]))),
                content: Box::new(Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()]))),
            },
            vec![],
        );
        let script_ast = Ast::block(vec![dec_ast, dialogue_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        let entry_idx = graph.entry.expect("graph must have an entry node");
        let entry_kind = node_kind(&graph, entry_idx);
        let IrNodeKind::DefineScriptDecorator { .. } = entry_kind else {
            panic!("expected DefineScriptDecorator, got {:?}", entry_kind)
        };

        // Follow the Next edge from DefineScriptDecorator to find Dialogue.
        let next_idx =
            next_of(&graph, entry_idx).expect("DefineScriptDecorator must have a Next edge");
        assert!(
            matches!(node_kind(&graph, next_idx), IrNodeKind::Dialogue { .. }),
            "expected Dialogue after DefineScriptDecorator, got {:?}",
            node_kind(&graph, next_idx)
        );
    }

    #[test]
    fn test_decorator_def_no_params() {
        use crate::parser::ast::EventConstraint;

        let body = Ast::block(vec![]);
        let decorator_ast = Ast::decorator_def(
            "highlight".to_string(),
            EventConstraint::Dialogue,
            vec![],
            body,
        );
        let script_ast = Ast::block(vec![decorator_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        let entry_idx = graph.entry.expect("entry");
        assert!(
            matches!(
                node_kind(&graph, entry_idx),
                IrNodeKind::DefineScriptDecorator {
                    name,
                    params,
                    event_constraint,
                    ..
                }
                if name == "highlight"
                    && params.is_empty()
                    && matches!(event_constraint, crate::parser::ast::EventConstraint::Dialogue)
            ),
            "expected DefineScriptDecorator(highlight) at entry, got {:?}",
            node_kind(&graph, entry_idx)
        );
    }

    #[test]
    fn test_decorator_def_body_labels_are_private() {
        use crate::parser::ast::EventConstraint;

        // Labels declared inside a decorator body are private — the body is
        // stored as raw Ast for lazy apply-time evaluation and is never compiled
        // into IR nodes, so its labels must NOT appear in graph.labels and a
        // jump targeting one from outside must fail with UnknownLabel.
        let inner_labeled = Ast::labeled_block("inner_scene".to_string(), Ast::block(vec![]));
        let body = Ast::block(vec![inner_labeled]);
        let decorator_ast =
            Ast::decorator_def("wrapper".to_string(), EventConstraint::Any, vec![], body);
        // Compiling the decorator alone must succeed and must NOT register the
        // inner label in the public graph.labels map.
        let script_ast = Ast::block(vec![decorator_ast]);
        let graph = Compiler::compile(&script_ast).expect("compile failed");
        assert!(
            !graph.labels.contains_key("inner_scene"),
            "inner_scene is private to the decorator body — must not appear in graph.labels"
        );

        // A jump to a label that only exists inside a decorator body must
        // produce a CompilerError::UnknownLabel.
        let inner_labeled2 = Ast::labeled_block("inner_scene".to_string(), Ast::block(vec![]));
        let body2 = Ast::block(vec![inner_labeled2]);
        let decorator_ast2 =
            Ast::decorator_def("wrapper2".to_string(), EventConstraint::Any, vec![], body2);
        let jump_ast = Ast::jump_stmt("inner_scene".to_string(), false);
        let script_with_jump = Ast::block(vec![decorator_ast2, jump_ast]);
        let result = Compiler::compile(&script_with_jump);
        assert!(
            matches!(result, Err(CompilerError::UnknownLabel(ref l)) if l == "inner_scene"),
            "expected UnknownLabel for jump into decorator body, got {:?}",
            result
        );
    }

    // ── helpers ──────────────────────────────────────────────────────────────

    fn ident(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_string()]))
    }

    fn int(n: i64) -> Ast {
        Ast::value(RuntimeValue::Int(n))
    }

    fn str_lit(s: &str) -> Ast {
        use crate::lexer::strings::ParsedString;
        Ast::value(RuntimeValue::Str(ParsedString::new_plain(s)))
    }

    fn decl(name: &str, val: Ast) -> Ast {
        Ast::decl(DeclKind::Variable, ident(name), val)
    }

    /// Borrow the [`IrNodeKind`] for a given [`NodeIndex`] from the graph.
    ///
    /// # Panics
    /// Panics if `id` is not a valid node in the graph.
    fn node_kind(graph: &IrGraph, id: NodeIndex) -> &IrNodeKind {
        &graph.graph[id]
    }

    /// Follow the single outgoing [`IrEdge::Next`] edge from `id`, returning
    /// the target [`NodeIndex`] if one exists.
    fn next_of(graph: &IrGraph, id: NodeIndex) -> Option<NodeIndex> {
        graph
            .graph
            .edges(id)
            .find(|e| matches!(e.weight(), IrEdge::Next))
            .map(|e| e.target())
    }

    /// Follow an outgoing edge of a specific kind from `id`.
    fn edge_target(graph: &IrGraph, id: NodeIndex, kind: &IrEdge) -> Option<NodeIndex> {
        graph
            .graph
            .edges(id)
            .find(|e| e.weight() == kind)
            .map(|e| e.target())
    }

    // ── tests ─────────────────────────────────────────────────────────────────

    /// A Block with two declarations compiles to a right-linked Assign chain
    /// (connected by Next edges).
    #[test]
    fn test_block_declarations_chain() {
        let ast = Ast::block(vec![decl("x", int(1)), decl("y", int(2))]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        // entry should be the first Assign (x = 1)
        let entry = graph.entry.expect("graph must have entry");
        match node_kind(&graph, entry) {
            IrNodeKind::Assign { var, scope, .. } => {
                assert_eq!(var, "x");
                assert_eq!(*scope, DeclKind::Variable);
                // Follow the Next edge to the second Assign.
                let next2 = next_of(&graph, entry).expect("first Assign must have a Next edge");
                match node_kind(&graph, next2) {
                    IrNodeKind::Assign { var: var2, .. } => {
                        assert_eq!(var2, "y");
                        // After the last statement the chain must end (no Next edge).
                        assert!(
                            next_of(&graph, next2).is_none(),
                            "last Assign must have no outgoing Next edge"
                        );
                    }
                    other => panic!("expected second Assign, got {:?}", other),
                }
            }
            other => panic!("expected Assign, got {:?}", other),
        }
    }

    /// An If without else: the `Else` edge of the Branch should point directly
    /// at the merge `Nop`, while the `Then` edge points at the compiled then-body.
    #[test]
    fn test_if_without_else_branch_else_is_merge() {
        let condition = Ast::equals_op(ident("a"), int(0));
        let then_block = Ast::block(vec![decl("x", int(1))]);
        let ast = Ast::if_stmt(condition.clone(), then_block, None);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        let entry = graph.entry.expect("entry");
        match node_kind(&graph, entry) {
            IrNodeKind::Branch { .. } => {
                let then_idx = edge_target(&graph, entry, &IrEdge::Then)
                    .expect("Branch must have a Then edge");
                let else_idx = edge_target(&graph, entry, &IrEdge::Else)
                    .expect("Branch must have an Else edge");

                // else_idx must be the merge Nop itself (no else body was compiled).
                match node_kind(&graph, else_idx) {
                    IrNodeKind::Nop => {}
                    other => panic!(
                        "expected else target to be a Nop merge point, got {:?}",
                        other
                    ),
                }

                // then_idx must be a different node (the compiled then-body).
                assert_ne!(
                    then_idx, else_idx,
                    "then target must differ from the merge Nop else target"
                );

                // The then-body's last node must link back to the same merge Nop.
                match node_kind(&graph, then_idx) {
                    IrNodeKind::Assign { .. } => {
                        let assign_next = next_of(&graph, then_idx)
                            .expect("then-body Assign must have a Next edge");
                        assert_eq!(
                            assign_next, else_idx,
                            "then-body's Next must point at the merge Nop"
                        );
                    }
                    other => panic!("expected Assign as then-body entry, got {:?}", other),
                }
            }
            other => panic!("expected Branch, got {:?}", other),
        }
    }

    /// LabeledBlock + Jump: the Jump edge target should equal the EnterScope NodeIndex.
    #[test]
    fn test_labeled_block_and_jump_resolve() {
        // label scene1 { let x = 42 }
        // jump scene1
        let labeled =
            Ast::labeled_block("scene1".to_string(), Ast::block(vec![decl("x", int(42))]));
        let jump = Ast::jump_stmt("scene1".to_string(), false);
        let ast = Ast::block(vec![labeled, jump]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        // The labels map must contain "scene1".
        let enter_id = match graph.labels.get("scene1") {
            Some(&id) => id,
            None => panic!("label 'scene1' not registered in graph.labels"),
        };

        // Find the Jump node by iterating over all nodes.
        let jump_idx = graph
            .graph
            .node_indices()
            .find(|&idx| matches!(graph.graph[idx], IrNodeKind::Jump))
            .expect("no Jump node found in graph");

        // The Jump edge must target the EnterScope node.
        let jump_target =
            edge_target(&graph, jump_idx, &IrEdge::Jump).expect("Jump node must have a Jump edge");
        assert_eq!(
            jump_target, enter_id,
            "Jump edge target must point at EnterScope NodeIndex"
        );

        // The entry for "scene1" must be an EnterScope node.
        match node_kind(&graph, enter_id) {
            IrNodeKind::EnterScope { label, .. } => {
                assert_eq!(label, "scene1");
            }
            other => panic!("expected EnterScope, got {:?}", other),
        }
    }

    /// Dialogue node preserves decorators.
    #[test]
    fn test_dialogue_preserves_decorators() {
        use crate::parser::ast::Decorator;

        let speakers = Ast::expr_list(vec![ident("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hello!")]);
        let deco = Decorator::bare("mood".to_string());

        let ast = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco.clone()],
        );

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        let entry = graph.entry.expect("entry");
        match node_kind(&graph, entry) {
            IrNodeKind::Dialogue { decorators, .. } => {
                assert_eq!(decorators.len(), 1);
                assert_eq!(decorators[0].name(), "mood");
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// Menu with two options compiles to a Choice with two IrChoiceOptions and
    /// outgoing Option(0)/Option(1) edges.
    #[test]
    fn test_menu_two_options() {
        let opt1 = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![decl("picked", int(1))]),
        );
        let opt2 = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![decl("picked", int(2))]),
        );
        let ast = Ast::menu(vec![opt1, opt2]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        let entry = graph.entry.expect("entry");
        match node_kind(&graph, entry) {
            IrNodeKind::Choice { options, .. } => {
                assert_eq!(options.len(), 2, "expected 2 options");
                assert_eq!(options[0].label, "Option A");
                assert_eq!(options[1].label, "Option B");
                // Each option must have a corresponding outgoing Option(i) edge.
                let opt0_entry = graph
                    .graph
                    .edges(entry)
                    .find(|e| matches!(e.weight(), IrEdge::Option(0)))
                    .map(|e| e.target());
                let opt1_entry = graph
                    .graph
                    .edges(entry)
                    .find(|e| matches!(e.weight(), IrEdge::Option(1)))
                    .map(|e| e.target());
                assert!(opt0_entry.is_some(), "Choice must have an Option(0) edge");
                assert!(opt1_entry.is_some(), "Choice must have an Option(1) edge");
                assert_ne!(
                    opt0_entry, opt1_entry,
                    "options must target different entry nodes"
                );
            }
            other => panic!("expected Choice, got {:?}", other),
        }
    }

    /// Match statement compiles to Switch with correct arm count and a Default edge.
    #[test]
    fn test_match_compiles_to_switch() {
        let scrutinee = ident("direction");
        let arm_north = MatchArm::new(
            MatchPattern::Value(Ast::value(RuntimeValue::IdentPath(vec![
                "North".to_string(),
            ]))),
            Ast::block(vec![decl("heading", int(0))]),
        );
        let arm_wild = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![decl("heading", int(-1))]),
        );
        let ast = Ast::match_stmt(scrutinee, vec![arm_north, arm_wild]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        let entry = graph.entry.expect("entry");
        match node_kind(&graph, entry) {
            IrNodeKind::Switch { arms, .. } => {
                assert_eq!(arms.len(), 1, "expected one non-wildcard arm");
                // The wildcard arm must produce a Default edge.
                let has_default = graph
                    .graph
                    .edges(entry)
                    .any(|e| matches!(e.weight(), IrEdge::Default));
                assert!(
                    has_default,
                    "expected a Default edge from Switch for wildcard arm"
                );
            }
            other => panic!("expected Switch, got {:?}", other),
        }
    }

    /// Jump to an undefined label returns a CompilerError::UnknownLabel.
    #[test]
    fn test_jump_unknown_label_error() {
        let ast = Ast::jump_stmt("nonexistent".to_string(), false);
        let result = Compiler::compile(&ast);
        assert!(
            matches!(result, Err(CompilerError::UnknownLabel(ref l)) if l == "nonexistent"),
            "expected UnknownLabel error, got {:?}",
            result,
        );
    }

    /// Duplicate labels in a single module are rejected at compile time.
    #[test]
    fn test_duplicate_label_in_single_module_errors() {
        let ast = Ast::block(vec![
            Ast::labeled_block("scene".to_string(), Ast::block(vec![])),
            Ast::labeled_block("scene".to_string(), Ast::block(vec![])),
        ]);

        let result = Compiler::compile(&ast);

        assert!(
            matches!(result, Err(CompilerError::DuplicateLabel(ref l)) if l == "scene"),
            "expected DuplicateLabel('scene'), got {:?}",
            result
        );
    }

    /// Duplicate labels inside an imported module are rejected in loader path.
    #[test]
    fn test_duplicate_label_in_imported_module_errors() {
        let mut loader = MemLoader::new();
        loader.add(
            "dup.urd",
            "label same { let x = 1 }\nlabel same { let y = 2 }\n",
        );

        let main_ast = Ast::block(vec![Ast::import_module(
            "dup.urd".to_string(),
            "dup".to_string(),
        )]);

        let result = Compiler::compile_with_loader(&main_ast, &loader);

        assert!(
            matches!(result, Err(CompilerError::DuplicateLabel(ref l)) if l == "same"),
            "expected DuplicateLabel('same') from imported module, got {:?}",
            result
        );
    }

    // ── import / compile_with_loader tests ───────────────────────────────────

    /// compile_with_loader resolves a simple import: the merged graph contains
    /// the module's label under "alias::label_name".
    #[test]
    fn test_compile_with_loader_resolves_import() {
        let mut loader = MemLoader::new();
        loader.add("lib.urd", "label greet { let msg = \"hello\" }");

        let main_ast = Ast::block(vec![Ast::import_module(
            "lib.urd".to_string(),
            "lib".to_string(),
        )]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        assert!(
            graph.labels.contains_key("lib::greet"),
            "expected 'lib::greet' in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// The same module imported from two different places (diamond dependency)
    /// does not trigger a CircularImport error.
    #[test]
    fn test_diamond_import_does_not_error() {
        let mut loader = MemLoader::new();
        // base.urd is imported by both mid.urd and directly by main
        loader.add("base.urd", "label entry { let x = 1 }");
        loader.add("mid.urd", "import \"base.urd\" as base");

        let main_ast = Ast::block(vec![
            Ast::import_module("mid.urd".to_string(), "mid".to_string()),
            Ast::import_module("base.urd".to_string(), "base".to_string()),
        ]);

        let result = Compiler::compile_with_loader(&main_ast, &loader);
        assert!(
            result.is_ok(),
            "diamond import should not produce a circular error, got: {:?}",
            result
        );
        let graph = result.unwrap();
        assert!(
            graph.labels.contains_key("base::entry"),
            "'base::entry' must be in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// A `jump alias.label` resolves to the correct (merged) NodeIndex.
    #[test]
    fn test_cross_module_jump_resolves_to_merged_node_id() {
        let mut loader = MemLoader::new();
        loader.add("scenes.urd", "label intro { let x = 1 }");

        let import_node = Ast::import_module("scenes.urd".to_string(), "scenes".to_string());
        // jump scenes.intro  → stored as Jump { label: "scenes.intro" }
        let jump_node = Ast::jump_stmt("scenes.intro".to_string(), false);
        let main_ast = Ast::block(vec![import_node, jump_node]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        // The merged label must exist.
        let enter_id = *graph
            .labels
            .get("scenes::intro")
            .expect("'scenes::intro' not in graph.labels");

        // Find the Jump node and verify its Jump edge target.
        let jump_idx = graph
            .graph
            .node_indices()
            .find(|&idx| matches!(graph.graph[idx], IrNodeKind::Jump))
            .expect("no Jump node in graph");

        let jump_target =
            edge_target(&graph, jump_idx, &IrEdge::Jump).expect("Jump node must have a Jump edge");
        assert_eq!(
            jump_target, enter_id,
            "Jump edge target must equal the merged EnterScope NodeIndex"
        );
    }

    /// Circular imports (a.urd ↔ b.urd) now compile successfully.
    ///
    /// Because top-level is definitions-only, neither module needs the other
    /// to be fully compiled before its own labels can be pre-allocated.
    /// The 4-phase flat pipeline handles this by pre-allocating all label Nop
    /// stubs before any IR is emitted.
    #[test]
    fn test_circular_import_compiles_successfully() {
        let mut loader = MemLoader::new();
        loader.add(
            "a.urd",
            "import \"b.urd\" as b\nlabel a_label {\n  jump b.b_label\n}\n",
        );
        loader.add(
            "b.urd",
            "import \"a.urd\" as a\nlabel b_label {\n  jump a.a_label\n}\n",
        );

        let main_ast = Ast::block(vec![Ast::import_module(
            "a.urd".to_string(),
            "a".to_string(),
        )]);

        let result = Compiler::compile_with_loader(&main_ast, &loader);
        assert!(
            result.is_ok(),
            "circular import should compile successfully, got: {:?}",
            result
        );

        let graph = result.unwrap();
        assert!(
            graph.labels.contains_key("a::a_label"),
            "a::a_label must be in the label map; got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
        assert!(
            graph.labels.contains_key("b::b_label"),
            "b::b_label (from a's whole-module import of b as 'b') must be in the label map; got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// A mutual symbol import also compiles cleanly.
    #[test]
    fn test_circular_symbol_import_compiles_successfully() {
        let mut loader = MemLoader::new();
        loader.add(
            "items.urd",
            "import (greet) from \"greetings.urd\"\nlabel show_items {\n  jump greet\n}\n",
        );
        loader.add(
            "greetings.urd",
            "import (show_items) from \"items.urd\"\nlabel greet {\n  jump show_items\n}\n",
        );

        let main_src =
            "import (show_items) from \"items.urd\"\nlabel start {\n  jump show_items\n}\n";
        let main_ast = crate::compiler::loader::parse_source(main_src).expect("parse");
        let result = Compiler::compile_with_loader(&main_ast, &loader);
        assert!(
            result.is_ok(),
            "circular symbol import should compile successfully, got: {:?}",
            result
        );
        let graph = result.unwrap();
        assert!(
            graph.labels.contains_key("show_items"),
            "show_items must be directly accessible; labels: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// A missing module produces CompilerError::ModuleLoadError.
    #[test]
    fn test_missing_module_returns_load_error() {
        let loader = MemLoader::new(); // empty — no files registered

        let main_ast = Ast::block(vec![Ast::import_module(
            "missing.urd".to_string(),
            "missing".to_string(),
        )]);

        let result = Compiler::compile_with_loader(&main_ast, &loader);
        assert!(
            matches!(
                result,
                Err(CompilerError::ModuleLoadError { ref path, .. }) if path == "missing.urd"
            ),
            "expected ModuleLoadError for 'missing.urd', got {:?}",
            result
        );
    }

    /// `end!()` call compiles to an `IrNodeKind::End` terminal node.
    #[test]
    fn test_end_bang_compiles_to_end_node() {
        let func_path = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_string()]));
        let params = Ast::block(vec![]);
        let call_ast = Ast::call(func_path, params);
        let script_ast = Ast::block(vec![call_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        assert!(
            graph
                .graph
                .node_weights()
                .any(|k| matches!(k, IrNodeKind::End)),
            "expected an End node in the graph, got: {:?}",
            graph.graph.node_weights().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_todo_bang_compiles_to_todo_node() {
        let func_path = Ast::value(RuntimeValue::IdentPath(vec!["todo!".to_string()]));
        let params = Ast::block(vec![]);
        let call_ast = Ast::call(func_path, params);
        let script_ast = Ast::block(vec![call_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        assert!(
            graph
                .graph
                .node_weights()
                .any(|k| matches!(k, IrNodeKind::Todo)),
            "expected a Todo node in the graph, got: {:?}",
            graph.graph.node_weights().collect::<Vec<_>>()
        );
    }

    /// Single-symbol import: `import greet as hello from "lib.urd"` should
    /// expose "hello" directly in graph.labels (no "lib::" prefix).
    #[test]
    fn test_single_symbol_import_exposes_alias_directly() {
        let mut loader = MemLoader::new();
        loader.add("lib.urd", "label greet { let msg = \"hello\" }");

        let import_node = Ast::import(
            "lib.urd".to_string(),
            vec![crate::parser::ast::ImportSymbol {
                original: Some("greet".to_string()),
                alias: "hello".to_string(),
            }],
        );
        let main_ast = Ast::block(vec![import_node]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        assert!(
            graph.labels.contains_key("hello"),
            "expected 'hello' in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
        // The whole-module namespace prefix must NOT be present.
        assert!(
            !graph.labels.contains_key("lib::greet"),
            "whole-module namespace 'lib::greet' must not appear for a symbol import"
        );
    }

    /// Single-symbol import with no alias: `import greet from "lib.urd"` should
    /// expose "greet" directly (alias == original).
    #[test]
    fn test_single_symbol_import_no_alias_uses_original_name() {
        let mut loader = MemLoader::new();
        loader.add("lib.urd", "label greet { let msg = \"hello\" }");

        let import_node = Ast::import(
            "lib.urd".to_string(),
            vec![crate::parser::ast::ImportSymbol {
                original: Some("greet".to_string()),
                alias: "greet".to_string(),
            }],
        );
        let main_ast = Ast::block(vec![import_node]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        assert!(
            graph.labels.contains_key("greet"),
            "expected 'greet' in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// Multi-symbol import: `import (intro as start, outro) from "scenes.urd"`
    /// should expose both "start" and "outro" directly in graph.labels.
    #[test]
    fn test_multi_symbol_import_exposes_all_aliases() {
        let mut loader = MemLoader::new();
        loader.add(
            "scenes.urd",
            "label intro { let x = 1 }\nlabel outro { let y = 2 }",
        );

        let import_node = Ast::import(
            "scenes.urd".to_string(),
            vec![
                crate::parser::ast::ImportSymbol {
                    original: Some("intro".to_string()),
                    alias: "start".to_string(),
                },
                crate::parser::ast::ImportSymbol {
                    original: Some("outro".to_string()),
                    alias: "outro".to_string(),
                },
            ],
        );
        let main_ast = Ast::block(vec![import_node]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        assert!(
            graph.labels.contains_key("start"),
            "expected 'start' in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
        assert!(
            graph.labels.contains_key("outro"),
            "expected 'outro' in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
        // The symbol labels should point to valid (different) nodes.
        assert_ne!(
            graph.labels["start"], graph.labels["outro"],
            "intro and outro must map to different NodeIndexes"
        );
    }

    /// Symbol import aliases point to the same node as the original label would
    /// after a whole-module import.  Verify that the NodeIndex for "hello" (aliasing
    /// "greet") matches the EnterScope node emitted for that label.
    #[test]
    fn test_symbol_import_alias_points_to_correct_enter_scope_node() {
        let mut loader = MemLoader::new();
        loader.add("lib.urd", "label greet { let msg = \"hello\" }");

        let import_node = Ast::import(
            "lib.urd".to_string(),
            vec![crate::parser::ast::ImportSymbol {
                original: Some("greet".to_string()),
                alias: "hello".to_string(),
            }],
        );
        let main_ast = Ast::block(vec![import_node]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        let hello_id = *graph.labels.get("hello").expect("'hello' not in labels");
        assert!(
            matches!(node_kind(&graph, hello_id), IrNodeKind::EnterScope { .. }),
            "alias 'hello' must point to an EnterScope node, got: {:?}",
            node_kind(&graph, hello_id)
        );
    }

    /// Symbol import from an already-completed module (diamond-like) still
    /// registers the alias without re-running the prologue.
    #[test]
    fn test_symbol_import_from_already_completed_module_still_aliases() {
        let mut loader = MemLoader::new();
        // lib.urd is first imported as a whole module, then its symbol is
        // also imported directly — the second import hits the `completed` path.
        loader.add("lib.urd", "label greet { let msg = \"hello\" }");

        let whole_import = Ast::import_module("lib.urd".to_string(), "lib".to_string());
        let sym_import = Ast::import(
            "lib.urd".to_string(),
            vec![crate::parser::ast::ImportSymbol {
                original: Some("greet".to_string()),
                alias: "greet_direct".to_string(),
            }],
        );
        let main_ast = Ast::block(vec![whole_import, sym_import]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        // Whole-module label must still be there.
        assert!(
            graph.labels.contains_key("lib::greet"),
            "whole-module label 'lib::greet' must be present"
        );
        // Symbol alias must also be registered even though the path was already compiled.
        assert!(
            graph.labels.contains_key("greet_direct"),
            "symbol alias 'greet_direct' must be present after completed-path import, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// Cross-module assignment `alias.var = value` compiles to an Assign node
    /// with Global scope and a namespaced key "alias::var".
    #[test]
    fn test_single_symbol_import_label_jumpable() {
        // `jump show_inventory` should compile when `show_inventory` is a
        // directly-imported symbol (registered in graph.labels via apply_aliases,
        // NOT in label_placeholders).
        let items_src = "label show_inventory {\n  return\n}\n";
        let main_src =
            "import (show_inventory) from \"items.urd\"\nlabel start {\n  jump show_inventory\n}\n";
        let mut loader = MemLoader::new();
        loader.add("items.urd", items_src);
        let ast = crate::compiler::loader::parse_source(main_src).expect("parse");
        let graph = Compiler::compile_with_loader(&ast, &loader).expect("compile");
        // The graph should have a Jump edge leading to the show_inventory EnterScope node.
        assert!(
            graph.labels.contains_key("show_inventory"),
            "show_inventory must be in labels; got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_cross_module_assignment_compiles_to_global_scope() {
        // Build AST for: `mod.counter = 42`
        let lhs = Ast::value(RuntimeValue::IdentPath(vec![
            "mod".to_string(),
            "counter".to_string(),
        ]));
        let rhs = int(42);
        let assign = Ast::assign_op(lhs, rhs);
        let ast = Ast::block(vec![assign]);

        let graph = Compiler::compile(&ast).expect("compile failed");

        let entry = graph.entry.expect("entry");
        match node_kind(&graph, entry) {
            IrNodeKind::Assign { var, scope, .. } => {
                assert_eq!(var, "mod::counter", "variable name must be namespaced");
                assert_eq!(
                    *scope,
                    DeclKind::Global,
                    "cross-module assignment must use Global scope"
                );
            }
            other => panic!("expected Assign, got {:?}", other),
        }
    }

    // ── Localization ID tests ────────────────────────────────────────────────

    #[test]
    fn test_compile_named_dialogue_gets_loc_id() {
        let speakers = Ast::expr_list(vec![str_lit("narrator")]);
        let lines = Ast::expr_list(vec![str_lit("Hello")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![dialogue]));
        let ast = Ast::block(vec![labeled]);

        let graph = Compiler::compile_named(&ast, "intro").expect("compile_named failed");

        let mut found = false;
        for node_idx in graph.graph.node_indices() {
            if let IrNodeKind::Dialogue { loc_id, .. } = &graph.graph[node_idx] {
                assert_eq!(loc_id.as_deref(), Some("intro-start-line_1"));
                found = true;
            }
        }
        assert!(found, "no Dialogue node found in graph");
    }

    #[test]
    fn test_compile_no_file_stem_loc_id_is_none() {
        let speakers = Ast::expr_list(vec![str_lit("narrator")]);
        let lines = Ast::expr_list(vec![str_lit("Hello")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![dialogue]));
        let ast = Ast::block(vec![labeled]);

        let graph = Compiler::compile(&ast).expect("compile failed");

        for node_idx in graph.graph.node_indices() {
            if let IrNodeKind::Dialogue { loc_id, .. } = &graph.graph[node_idx] {
                assert!(
                    loc_id.is_none(),
                    "loc_id should be None when no file stem given"
                );
            }
        }
    }

    #[test]
    fn test_compile_named_menu_gets_loc_id() {
        let opt1 = Ast::menu_option("alcohol".to_string(), Ast::block(vec![]));
        let opt2 = Ast::menu_option("nicotine".to_string(), Ast::block(vec![]));
        let menu = Ast::menu(vec![opt1, opt2]);
        let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![menu]));
        let ast = Ast::block(vec![labeled]);

        let graph = Compiler::compile_named(&ast, "intro").expect("compile_named failed");

        let mut found_choice = false;
        for node_idx in graph.graph.node_indices() {
            if let IrNodeKind::Choice {
                loc_id, options, ..
            } = &graph.graph[node_idx]
            {
                assert_eq!(loc_id.as_deref(), Some("intro-start-menu_1"));
                assert_eq!(
                    options[0].loc_id.as_deref(),
                    Some("intro-start-menu_1-alcohol")
                );
                assert_eq!(
                    options[1].loc_id.as_deref(),
                    Some("intro-start-menu_1-nicotine")
                );
                found_choice = true;
            }
        }
        assert!(found_choice, "no Choice node found in graph");
    }

    #[test]
    fn test_compile_named_two_menus_independent_counters() {
        let menu1 = Ast::menu(vec![Ast::menu_option("a".to_string(), Ast::block(vec![]))]);
        let menu2 = Ast::menu(vec![Ast::menu_option("b".to_string(), Ast::block(vec![]))]);
        let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![menu1, menu2]));
        let ast = Ast::block(vec![labeled]);

        let graph = Compiler::compile_named(&ast, "file").expect("compile_named failed");

        let mut choice_ids: Vec<String> = graph
            .graph
            .node_indices()
            .filter_map(|idx| {
                if let IrNodeKind::Choice { loc_id, .. } = &graph.graph[idx] {
                    loc_id.clone()
                } else {
                    None
                }
            })
            .collect();
        choice_ids.sort();
        assert_eq!(choice_ids, vec!["file-start-menu_1", "file-start-menu_2"]);
    }
}
