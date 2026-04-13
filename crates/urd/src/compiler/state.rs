// ─── Speaker normalisation ────────────────────────────────────────────────────

// Convert the `speakers` AST node so that every `IdentPath` leaf is replaced
// with an equivalent `Str` value.
//
// In the bracketless dialogue syntax (`narrator: "text"`) the parser emits
// speaker names as `Value(IdentPath([…]))`.  Left as-is, the VM's expression
// evaluator would try to look them up as runtime variables and fail.  Speakers
// are always *character names*, never variable references, so we canonicalise
// them to strings here at compile time — e.g. `IdentPath(["Alice"])` becomes
// `Str("Alice")` and `IdentPath(["chars", "narrator"])` becomes
// `Str("chars.narrator")`.
//
// `Str` nodes (from the old `<"Alice">:` form or any other string literal) are
// passed through unchanged.

// ─── Internal compiler state ──────────────────────────────────────────────────

use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::stable_graph::NodeIndex;

use crate::{
    ir::{IrChoiceOption, IrEdge, IrGraph, IrNodeKind, SwitchArm},
    loc::{EventKind, IdContext, extract_id_override},
    parser::ast::{Ast, AstContent, DeclKind, MatchPattern, Operator},
};

use super::helpers::{extract_fluent_alias, extract_name, preassign_subtree, resolve_label};
use super::{CompilerError, MAX_COMPILER_DEPTH};

/// Internal mutable state threaded through both compilation passes.
pub(super) struct CompilerState {
    pub(super) graph: IrGraph,
    /// Maps label names → the [`NodeIndex`] of their pre-allocated Nop placeholder.
    pub(super) label_placeholders: HashMap<String, NodeIndex>,
    /// ID generation context. `None` when no file stem was provided (inline tests, etc.).
    pub(super) id_ctx: Option<IdContext>,
    /// Pre-assigned localisation IDs consumed during right-to-left block compilation.
    ///
    /// `Some` while inside a "replay" pass (i.e. the forward pre-pass has already
    /// assigned all IDs in source order and packed them into this queue).  Every
    /// ID-generating site pops from the front of this queue instead of calling
    /// [`IdContext`] methods directly, keeping counters stable.  `None` at all
    /// other times.
    pub(super) preassign_ids: Option<VecDeque<Option<String>>>,
    /// Namespaced keys of labels that are valid cross-module jump targets.
    /// Contains `@entry` labels from whole-module imports and all symbol-imported labels.
    /// `None` for single-file compilation (no restriction).
    /// `Some(set)` for multi-file compilation (enforce restriction).
    pub(super) exported_labels: Option<HashSet<String>>,
    /// When `true`, the compiler is emitting nodes for an imported module.
    /// Bare label names are **not** inserted into `graph.labels` so that
    /// they cannot be reached without their module-qualified prefix
    /// (e.g. `jump lib.greeting` instead of `jump greeting`).
    pub(super) is_imported_module: bool,
    /// Current recursion depth for stack-overflow protection.
    depth: usize,
}

impl CompilerState {
    pub(super) fn new() -> Self {
        CompilerState {
            graph: IrGraph::new(),
            label_placeholders: HashMap::new(),
            id_ctx: None,
            preassign_ids: None,
            exported_labels: None,
            is_imported_module: false,
            depth: 0,
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
        self.depth += 1;
        if self.depth > MAX_COMPILER_DEPTH {
            self.depth -= 1;
            return Err(CompilerError::Internal(
                "maximum nesting depth exceeded during label scan".into(),
            ));
        }
        let result = self.scan_labels_impl(ast);
        self.depth -= 1;
        result
    }

    fn scan_labels_impl(&mut self, ast: &Ast) -> Result<(), CompilerError> {
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

        // Record all @entry-decorated label names so that `Vm::new_at` can
        // validate its `label` argument.  In multi-file compilation we only
        // record bare names for the root module — imported modules contribute
        // their namespaced forms via `build_global_labels` instead.
        if !self.is_imported_module {
            for label_ast in &labels {
                if let AstContent::LabeledBlock { label, .. } = label_ast.content()
                    && label_ast.decorators().iter().any(|d| d.name() == "entry")
                {
                    self.graph.entry_labels.insert(label.clone());
                }
            }
        }

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
        self.depth += 1;
        if self.depth > MAX_COMPILER_DEPTH {
            self.depth -= 1;
            return Err(CompilerError::Internal(
                "maximum nesting depth exceeded during compilation".into(),
            ));
        }
        let result = self.compile_node_impl(ast, next);
        self.depth -= 1;
        result
    }

    fn compile_node_impl(
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

                // Run forward pre-pass only at the outermost block entry (not for
                // nested blocks already inside a pre-assigned subtree).
                let was_outermost = self.id_ctx.is_some() && self.preassign_ids.is_none();
                // `was_outermost` is only true when `id_ctx.is_some()`, so the
                // `if let` branch is always taken.  NLL ends the immutable borrow
                // of `self.id_ctx` (via `ctx`) at the `clone()` call, allowing the
                // reassignment below without a conflicting borrow.
                if was_outermost && let Some(ctx) = self.id_ctx.as_ref() {
                    let mut id_ctx_pre = ctx.clone();
                    let pre_ids = preassign_subtree(ast, &mut id_ctx_pre);
                    // Advance actual IdContext to post-block state so counters
                    // are not re-incremented during the compilation pass.
                    self.id_ctx = Some(id_ctx_pre);
                    self.preassign_ids = Some(pre_ids);
                }

                // Compile right-to-left, threading `next` through each statement.
                let mut continuation = next;
                for stmt in stmts.iter().rev() {
                    continuation = self.compile_node(stmt, continuation)?;
                }

                if was_outermost {
                    // Exit replay mode; queue must be exhausted at this point.
                    if let Some(ref q) = self.preassign_ids
                        && !q.is_empty()
                    {
                        return Err(CompilerError::Internal(
                            "preassign_ids queue not exhausted after block compilation".into(),
                        ));
                    }
                    self.preassign_ids = None;
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
                    _ => (extract_name(left)?, DeclKind::Assignment),
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
                // Skip push/pop in replay mode — counters were already advanced
                // by the forward pre-pass.
                if self.preassign_ids.is_none()
                    && let Some(ctx) = &mut self.id_ctx
                {
                    ctx.push_container(EventKind::If, if_override);
                }

                // Then branch: wrap in PushScope/PopScope
                let then_pop = self.graph.push(IrNodeKind::PopScope);
                self.graph.add_edge(then_pop, merge, IrEdge::Next);
                let then_body = self
                    .compile_node(then_block, Some(then_pop))?
                    .unwrap_or(then_pop);
                let then_push = self.graph.push(IrNodeKind::PushScope);
                self.graph.add_edge(then_push, then_body, IrEdge::Next);
                let then_entry = then_push;

                // Else branch: wrap in PushScope/PopScope (only if else block exists)
                let else_entry = match else_block {
                    Some(eb) => {
                        let else_pop = self.graph.push(IrNodeKind::PopScope);
                        self.graph.add_edge(else_pop, merge, IrEdge::Next);
                        let else_body = self.compile_node(eb, Some(else_pop))?.unwrap_or(else_pop);
                        let else_push = self.graph.push(IrNodeKind::PushScope);
                        self.graph.add_edge(else_push, else_body, IrEdge::Next);
                        else_push
                    }
                    None => merge,
                };

                if self.preassign_ids.is_none()
                    && let Some(ctx) = &mut self.id_ctx
                {
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

                // Push label scope for ID generation (skip in replay mode).
                let id_override = if self.id_ctx.is_some() {
                    extract_id_override(ast.decorators())
                } else {
                    None
                };
                if self.preassign_ids.is_none()
                    && let Some(ctx) = &mut self.id_ctx
                {
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
                // Skip bare-name registration for imported modules so that
                // their labels are only reachable via qualified names.
                if !self.is_imported_module {
                    self.graph.labels.insert(label.clone(), placeholder_id);
                }

                // Pop label scope (skip in replay mode).
                if self.preassign_ids.is_none()
                    && let Some(ctx) = &mut self.id_ctx
                {
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
                let target = resolve_label(
                    label,
                    &self.label_placeholders,
                    &self.graph.labels,
                    &self.exported_labels,
                )?;

                if *expects_return {
                    // `jump label and return` — subroutine call without a binding.
                    // Use an empty var name as the "discard return value" sentinel.
                    let id = self.graph.push(IrNodeKind::LetCall { var: String::new() });
                    self.graph.add_edge(id, target, IrEdge::Call);
                    let ret_target = next.unwrap_or_else(|| self.graph.push(IrNodeKind::End));
                    self.graph.add_edge(id, ret_target, IrEdge::Ret);
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
                let target_id = resolve_label(
                    target,
                    &self.label_placeholders,
                    &self.graph.labels,
                    &self.exported_labels,
                )?;

                let id = self.graph.push(IrNodeKind::LetCall { var: name.clone() });
                self.graph.add_edge(id, target_id, IrEdge::Call);
                let ret_target = next.unwrap_or_else(|| self.graph.push(IrNodeKind::End));
                self.graph.add_edge(id, ret_target, IrEdge::Ret);
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
                let loc_id = if let Some(q) = &mut self.preassign_ids {
                    // Replay mode: consume the pre-assigned ID from the queue.
                    q.pop_front().flatten()
                } else if let Some(ctx) = &mut self.id_ctx {
                    ctx.next_dialogue_id(id_override)
                } else {
                    None
                };
                let id = self.graph.push(IrNodeKind::Dialogue {
                    speakers: *speakers.clone(),
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
                let choice_loc_id = if let Some(q) = &mut self.preassign_ids {
                    // Replay mode: consume the pre-assigned choice ID.
                    q.pop_front().flatten()
                } else if let Some(ctx) = &mut self.id_ctx {
                    ctx.push_container(EventKind::Menu, menu_override);
                    ctx.current_full_path()
                } else {
                    None
                };

                let mut ir_options = Vec::with_capacity(options.len());
                let mut option_entries: Vec<NodeIndex> = Vec::with_capacity(options.len());

                for opt_ast in options {
                    match opt_ast.content() {
                        AstContent::MenuOption {
                            label,
                            content,
                            is_default,
                        } => {
                            let opt_override = if self.id_ctx.is_some() {
                                extract_id_override(opt_ast.decorators())
                            } else {
                                None
                            };
                            let opt_loc_id = if *is_default {
                                // Default/wildcard options are not player-visible
                                // text — they never get a localization ID.
                                if let Some(q) = &mut self.preassign_ids {
                                    // Still consume the pre-assigned slot (always None).
                                    q.pop_front();
                                }
                                None
                            } else if let Some(q) = &mut self.preassign_ids {
                                // Replay mode: consume the pre-assigned option ID.
                                q.pop_front().flatten()
                            } else if let Some(ctx) = &mut self.id_ctx {
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
                                is_default: *is_default,
                            });
                        }
                        _ => {
                            return Err(CompilerError::InvalidStatement(
                                "expected MenuOption inside Menu".to_string(),
                            ));
                        }
                    }
                }

                // Pop the menu container (skip in replay mode — already advanced).
                if self.preassign_ids.is_none()
                    && let Some(ctx) = &mut self.id_ctx
                {
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

                // Push match_N container (skip in replay mode).
                let match_override = if self.id_ctx.is_some() {
                    extract_id_override(ast.decorators())
                } else {
                    None
                };
                if self.preassign_ids.is_none()
                    && let Some(ctx) = &mut self.id_ctx
                {
                    ctx.push_container(EventKind::Match, match_override);
                }

                let mut switch_arms: Vec<SwitchArm> = Vec::with_capacity(arms.len());
                let mut arm_entries: Vec<NodeIndex> = Vec::new();
                let mut default_entry: Option<NodeIndex> = None;

                for arm in arms {
                    let arm_pop = self.graph.push(IrNodeKind::PopScope);
                    self.graph.add_edge(arm_pop, merge, IrEdge::Next);
                    let arm_body = self
                        .compile_node(&arm.body, Some(arm_pop))?
                        .unwrap_or(arm_pop);
                    let arm_push = self.graph.push(IrNodeKind::PushScope);
                    self.graph.add_edge(arm_push, arm_body, IrEdge::Next);
                    let target = arm_push;
                    match &arm.pattern {
                        MatchPattern::Wildcard => {
                            default_entry = Some(target);
                        }
                        MatchPattern::Value(_)
                        | MatchPattern::Range { .. }
                        | MatchPattern::Array(_) => {
                            switch_arms.push(SwitchArm {
                                pattern: arm.pattern.clone(),
                            });
                            arm_entries.push(target);
                        }
                    }
                }

                if self.preassign_ids.is_none()
                    && let Some(ctx) = &mut self.id_ctx
                {
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
