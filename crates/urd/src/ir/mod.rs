//! # Intermediate Representation (IR) Module
//!
//! This module defines the Intermediate Representation used internally by the Urd VM.
//! The compiler transforms an [`Ast`] into an [`IrGraph`], which the VM then executes.
//!
//! ## Key Types
//!
//! - [`IrGraph`]: The compiled script — a [`petgraph::stable_graph::StableDiGraph`] of
//!   [`IrNodeKind`] nodes connected by [`IrEdge`]-typed arcs.
//! - [`IrNodeKind`]: The payload of a node, discriminating between all node kinds.
//!   No variant embeds a successor index; all control-flow is expressed as edges.
//! - [`IrEdge`]: The semantic label on every directed arc in the graph.
//! - [`Event`]: Serialisable output events emitted by the VM to its consumer.
//!
//! ## Submodules
//!
//! - [`dot`]: Graphviz DOT renderer for [`IrGraph`].
//! - [`mermaid`]: Mermaid flowchart renderer for [`IrGraph`].

pub mod analysis;
pub mod dot;
pub mod mermaid;
mod render_common;

use std::collections::{HashMap, HashSet};

use petgraph::stable_graph::{NodeIndex, StableDiGraph};

use crate::parser::ast::{Ast, DeclKind, Decorator, MatchPattern};

// ─── IrEdge ──────────────────────────────────────────────────────────────────

/// Edge label in an [`IrGraph`], encoding the semantic role of a control-flow arc.
///
/// Every directed arc in the graph carries exactly one `IrEdge` value.  Node
/// variants in [`IrNodeKind`] contain *no* successor indices; all control-flow
/// relationships are expressed exclusively through these typed edges.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrEdge {
    /// Linear continuation (next statement).
    Next,
    /// True-branch of a [`IrNodeKind::Branch`] node.
    Then,
    /// False-branch of a [`IrNodeKind::Branch`] node.
    Else,
    /// Nth selectable option of a [`IrNodeKind::Choice`] node (stores option index).
    Option(usize),
    /// Pattern-match arm of a [`IrNodeKind::Switch`] node (stores arm index for
    /// [`SwitchArm`] lookup).
    Arm(usize),
    /// Default arm of a [`IrNodeKind::Switch`] node when no arm pattern matches.
    Default,
    /// Call edge from a [`IrNodeKind::LetCall`] to its callee entry node.
    Call,
    /// Return-continuation edge from a [`IrNodeKind::LetCall`] to its `next` node.
    Ret,
    /// Unconditional jump edge from a [`IrNodeKind::Jump`] node to its target.
    Jump,
}

// ─── IrGraph ─────────────────────────────────────────────────────────────────

/// The compiled form of an Urd script.
///
/// Nodes are stored in a [`StableDiGraph`] where each node weight is an
/// [`IrNodeKind`] and each edge weight is an [`IrEdge`] encoding the semantic
/// role of the arc.
///
/// Control flow is expressed *entirely* through edges — [`IrNodeKind`] variants
/// carry only data (expressions, variable names, patterns) and never embed
/// successor [`NodeIndex`] pointers.
///
/// The VM begins execution at [`IrGraph::entry`] and follows edges until it
/// reaches a terminal node ([`IrNodeKind::End`], [`IrNodeKind::Todo`], or
/// [`IrNodeKind::Return`]).
#[derive(Debug, Clone)]
pub struct IrGraph {
    /// The directed graph of IR nodes and control-flow edges.
    pub(crate) graph: StableDiGraph<IrNodeKind, IrEdge>,
    /// The [`NodeIndex`] at which execution begins, or `None` for an empty graph.
    pub entry: Option<NodeIndex>,
    /// Maps every `label ident { … }` label name to the [`NodeIndex`] of the
    /// corresponding [`IrNodeKind::EnterScope`] node.
    ///
    /// May contain multiple keys for the same [`NodeIndex`] when a label is
    /// reachable under both a bare name (`hub`) and a namespaced alias
    /// (`main::hub`).  Use [`IrGraph::cluster_names`] when you need exactly
    /// one canonical name per node (e.g. for rendering cluster subgraphs).
    pub labels: HashMap<String, NodeIndex>,
    /// Names of all `@entry`-decorated labels.
    ///
    /// In single-file compilation this contains the bare label names.
    /// In multi-file compilation this includes both bare and namespaced
    /// forms (e.g. `"start"` and `"tavern::enter"`).
    ///
    /// Use [`Vm::new_at`] to start the VM at a specific entry label.
    pub entry_labels: HashSet<String>,
    /// Canonical display name for each unique label entry [`NodeIndex`].
    ///
    /// Unlike [`IrGraph::labels`] (which holds all resolvable aliases), this
    /// map has **exactly one entry per unique label node** — no duplicates.
    ///
    /// - Root-module labels use their bare name (`"hub"`).
    /// - Imported-module labels use their namespaced alias (`"tavern::enter_tavern"`).
    ///
    /// Populated by the multi-file compiler (`compile_flat`); empty for
    /// single-file compilation (fall back to iterating `labels` in that case).
    pub cluster_names: HashMap<NodeIndex, String>,
    /// Source file path for each label entry [`NodeIndex`].
    ///
    /// `""` means the root (entry) module.  For imported modules the value is
    /// the path string as written in the `import` statement (e.g. `"tavern.urd"`).
    ///
    /// Populated by the multi-file compiler; empty for single-file compilation.
    pub label_sources: HashMap<NodeIndex, String>,
}

impl IrGraph {
    /// Returns a read-only reference to the underlying directed graph.
    pub fn graph(&self) -> &StableDiGraph<IrNodeKind, IrEdge> {
        &self.graph
    }

    /// Creates an empty graph with no nodes and no entry point.
    pub(crate) fn new() -> Self {
        IrGraph {
            graph: StableDiGraph::new(),
            entry: None,
            labels: HashMap::new(),
            entry_labels: HashSet::new(),
            cluster_names: HashMap::new(),
            label_sources: HashMap::new(),
        }
    }

    /// Appends a node to the graph and returns its freshly-assigned [`NodeIndex`].
    pub(crate) fn push(&mut self, kind: IrNodeKind) -> NodeIndex {
        self.graph.add_node(kind)
    }

    /// Mutably borrows the node weight at `idx`, returning `None` if the index
    /// does not refer to a live node.
    pub(crate) fn node_mut(&mut self, idx: NodeIndex) -> Option<&mut IrNodeKind> {
        self.graph.node_weight_mut(idx)
    }

    /// Adds a directed edge from `from` to `to` with semantic label `edge`.
    pub(crate) fn add_edge(&mut self, from: NodeIndex, to: NodeIndex, edge: IrEdge) {
        self.graph.add_edge(from, to, edge);
    }

    /// Returns the names of all `@entry`-decorated labels.
    pub fn entry_labels(&self) -> &HashSet<String> {
        &self.entry_labels
    }

    /// Returns the set of decorator names used across all `Dialogue` and `Choice`
    /// nodes in this graph. Useful for pre-registering passthrough handlers before
    /// constructing a [`crate::vm::Vm`].
    pub fn used_decorators(&self) -> std::collections::HashSet<&str> {
        let mut names = std::collections::HashSet::new();
        for node in self.graph.node_weights() {
            match node {
                IrNodeKind::Dialogue { decorators, .. } => {
                    for d in decorators {
                        names.insert(d.name());
                    }
                }
                IrNodeKind::Choice {
                    decorators,
                    options,
                    ..
                } => {
                    for d in decorators {
                        names.insert(d.name());
                    }
                    for opt in options {
                        for d in &opt.decorators {
                            names.insert(d.name());
                        }
                    }
                }
                _ => {}
            }
        }
        names
    }

    /// Returns the names of script-defined (`decorator name { ... }`) decorators
    /// declared in this graph. These are registered at VM init time and should
    /// not be overridden with passthrough handlers.
    pub fn script_defined_decorators(&self) -> std::collections::HashSet<&str> {
        self.graph
            .node_weights()
            .filter_map(|k| match k {
                IrNodeKind::DefineScriptDecorator { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect()
    }

    /// Merges `other` into `self`, remapping all [`NodeIndex`] values via a
    /// stable old→new mapping built during the merge.
    ///
    /// All nodes from `other` are added to `self.graph`.  Edges are re-added
    /// using the remapped indices.  Labels from `other` are inserted under the
    /// key `"alias::label_name"` so they don't conflict with local labels.
    ///
    /// [`IrNodeKind::DefineEnum`] names are namespace-prefixed the same way,
    /// so `enum Faction` from a module imported as `chars` becomes
    /// `chars::Faction` in the merged graph.
    ///
    /// # Deprecation
    ///
    /// This method is no longer used by any production code path.
    /// [`compile_flat`](crate::compiler::compile_flat) now builds all modules
    /// into a single shared graph directly, making post-hoc merging
    /// unnecessary. This method is retained only to avoid a breaking API
    /// change; prefer `compile_flat` for all new code.
    ///
    /// # Complexity
    ///
    /// **O(N · E)** worst-case, where *N* is the number of nodes in `other`
    /// and *E* is the number of edges.  Each call to
    /// [`StableGraph::remove_node`] is O(E) because it must scan the
    /// adjacency lists to drop incident edges, and we call it once per node.
    ///
    /// # Returns
    /// A `HashMap<NodeIndex, NodeIndex>` mapping every node index from `other`
    /// to its freshly-assigned index in `self`.  Callers can use this map to
    /// translate any `NodeIndex` that was valid in `other` (e.g. `other.entry`)
    /// into the corresponding index valid in the merged graph.
    ///
    /// # Label namespacing
    /// A label `"start"` from a module imported as `"foo"` becomes `"foo::start"`.
    #[deprecated(
        since = "0.2.0",
        note = "No longer used in production. `compile_flat` builds into a single shared graph, \
                making post-hoc merging unnecessary. This method has O(N·E) worst-case complexity."
    )]
    pub fn merge(&mut self, other: IrGraph, alias: &str) -> HashMap<NodeIndex, NodeIndex> {
        // Snapshot all edges *before* we begin consuming `other.graph`.
        // `remove_node` drops incident edges from the StableGraph's adjacency
        // lists, so we must capture them while the graph is still intact.
        let edges: Vec<(NodeIndex, NodeIndex, IrEdge)> = other
            .graph
            .edge_indices()
            .filter_map(|e| {
                let (src, dst) = other.graph.edge_endpoints(e)?;
                let weight = *other.graph.edge_weight(e)?;
                Some((src, dst, weight))
            })
            .collect();

        // Collect node indices in a stable iteration order.
        let node_indices: Vec<NodeIndex> = other.graph.node_indices().collect();

        // Consume `other.graph` node by node, building the old→new index map
        // as we go.  `remove_node` on a StableGraph is O(E) per call, but
        // provides ownership of the weight without requiring `Clone`.
        let mut index_map: HashMap<NodeIndex, NodeIndex> =
            HashMap::with_capacity(node_indices.len());
        let mut other_graph = other.graph;

        for old_idx in node_indices {
            if let Some(mut kind) = other_graph.remove_node(old_idx) {
                // Namespace DefineEnum names so `enum Faction` from a module
                // imported as `chars` becomes `chars::Faction` in the merged
                // graph.  This lets the VM resolve `chars.Faction.Rebel`
                // (a 3-segment IdentPath) by looking up enum `chars::Faction`.
                if let IrNodeKind::DefineEnum { name, .. } = &mut kind {
                    *name = namespace(alias, name);
                }
                let new_idx = self.graph.add_node(kind);
                index_map.insert(old_idx, new_idx);
            }
        }

        // Re-add all edges using the remapped indices.
        for (old_src, old_dst, weight) in edges {
            if let (Some(&new_src), Some(&new_dst)) =
                (index_map.get(&old_src), index_map.get(&old_dst))
            {
                self.graph.add_edge(new_src, new_dst, weight);
            }
        }

        // Insert all of other's labels under `"alias::label_name"`.
        for (label_name, old_idx) in other.labels {
            if let Some(&new_idx) = index_map.get(&old_idx) {
                self.labels.insert(namespace(alias, &label_name), new_idx);
            }
        }

        // Preserve cluster_names from `other`, namespaced like labels.
        for (old_idx, name) in other.cluster_names {
            if let Some(&new_idx) = index_map.get(&old_idx) {
                self.cluster_names
                    .entry(new_idx)
                    .or_insert_with(|| namespace(alias, &name));
            }
        }

        // Preserve label_sources from `other`.
        for (old_idx, source) in other.label_sources {
            if let Some(&new_idx) = index_map.get(&old_idx) {
                self.label_sources.entry(new_idx).or_insert(source);
            }
        }

        // Return the full old→new index map so callers (e.g. the compiler's
        // loader) can translate any `NodeIndex` that was valid in `other` into
        // the corresponding index valid in the merged graph.
        index_map
    }
}

impl Default for IrGraph {
    fn default() -> Self {
        Self::new()
    }
}

// ─── Supporting types ─────────────────────────────────────────────────────────

/// A single selectable option inside a [`IrNodeKind::Choice`] node.
///
/// The entry node for this option is encoded as an [`IrEdge::Option`]`(i)` edge
/// in the graph, where `i` is the index of this option in
/// [`IrNodeKind::Choice::options`].
#[derive(Debug, Clone)]
pub struct IrChoiceOption {
    /// The display text shown to the player for this option.
    pub label: String,
    /// Decorators attached to the `MenuOption` AST node.
    pub decorators: Vec<Decorator>,
    /// Localization key for this option, if any.
    /// `None` when the compiler was invoked without a file stem context.
    pub loc_id: Option<String>,
    /// `true` when this option is a wildcard/default (`_`) — selected when
    /// the host passes `None` to a pending choice (e.g. on timer expiry).
    /// Default options are not included in the emitted [`Event::Choice`] options.
    pub is_default: bool,
}

/// One arm of a compiled [`IrNodeKind::Switch`] node.
///
/// The target node for this arm is encoded as an [`IrEdge::Arm`]`(i)` edge in
/// the graph, where `i` is the index of this arm in
/// [`IrNodeKind::Switch::arms`].
#[derive(Debug, Clone)]
pub struct SwitchArm {
    /// The pattern to test the scrutinee against.
    pub pattern: MatchPattern,
}

// ─── IrNodeKind ──────────────────────────────────────────────────────────────

/// The payload of an IR node, discriminating between every kind of IR node.
///
/// Nodes fall into two broad categories:
///
/// * **Internal nodes** — consumed only by the VM; never surfaced to the
///   script consumer (game engine, test harness, etc.).
/// * **Output-event nodes** — [`Dialogue`][IrNodeKind::Dialogue] and
///   [`Choice`][IrNodeKind::Choice] — cause the VM to emit a serialisable
///   [`Event`] and pause execution until the consumer responds.
///
/// **All successor / continuation relationships are expressed as [`IrEdge`]-typed
/// edges in the containing [`IrGraph`].**  No variant embeds a [`NodeIndex`].
/// See each variant's documentation for which [`IrEdge`] labels its outgoing
/// arcs carry.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum IrNodeKind {
    // ── Internal nodes ──────────────────────────────────────────────────────
    /// Declare or assign a variable.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    Assign {
        /// The variable name being bound.
        var: String,
        /// Whether this is a `global`, `const`, or `let` declaration.
        scope: DeclKind,
        /// The initialiser / right-hand-side expression (kept as raw [`Ast`]).
        expr: Ast,
        /// Fluent variable name for this binding, if tagged `@fluent`.
        /// `None` — not fluent-tagged.
        /// `Some(alias)` — inject into Fluent context under `alias` each time this is assigned.
        fluent_alias: Option<String>,
    },

    /// Evaluate an expression purely for its side effects and discard the result.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    Eval {
        /// The expression to evaluate (kept as raw [`Ast`]).
        expr: Ast,
    },

    /// Conditional branch.
    ///
    /// Outgoing edges: [`IrEdge::Then`] to the true-path entry node,
    /// [`IrEdge::Else`] to the false-path entry node.
    Branch {
        /// The boolean condition expression.
        condition: Ast,
    },

    /// Multi-way pattern match.
    ///
    /// Outgoing edges: one [`IrEdge::Arm`]`(i)` edge per arm (pointing to
    /// `arms[i]`'s target node) and an optional [`IrEdge::Default`] edge when
    /// there is a default case.
    Switch {
        /// The expression being matched.
        scrutinee: Ast,
        /// Ordered list of pattern descriptors.  Each arm's *target* node is
        /// the destination of the corresponding [`IrEdge::Arm`]`(i)` edge.
        arms: Vec<SwitchArm>,
    },

    /// Unconditional jump to a previously-compiled node.
    ///
    /// Outgoing edge: a single [`IrEdge::Jump`] to the target node.
    Jump,

    /// Subroutine call with result binding.
    ///
    /// Pushes a `CallFrame` with `assign_to_var = Some(var)` (or `None` when
    /// `var` is empty) and follows the [`IrEdge::Call`] edge to the callee
    /// entry.  When a `Return` pops the frame the VM resumes at the
    /// [`IrEdge::Ret`] successor.
    ///
    /// Outgoing edges: [`IrEdge::Call`] → callee entry, [`IrEdge::Ret`] →
    /// return continuation.
    LetCall {
        /// Variable name to store the return value in.
        /// An empty string means "discard the return value".
        var: String,
    },

    /// Return from the current script (or sub-routine).  Terminal node — no
    /// outgoing edges.
    Return {
        /// Optional return value expression.
        value: Option<Ast>,
    },

    /// Marks the entry point of a labeled block (`label ident { … }`).
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    EnterScope {
        /// The label name.
        label: String,
    },

    /// Marks the exit point of a labeled block; resumes normal control flow.
    ///
    /// Continues via a single [`IrEdge::Next`] edge (or has no outgoing edge
    /// when it is the final node in a label with no fall-through).
    ExitScope {
        /// The label name (mirrors the matching [`EnterScope`][IrNodeKind::EnterScope]).
        label: String,
    },

    /// Pushes an anonymous block scope onto the environment stack.
    ///
    /// Used to give `if` / `match` branches their own variable scope so that
    /// `let` bindings inside a branch do not leak into the surrounding label
    /// scope.  Unlike [`EnterScope`] this node does **not** interact with the
    /// call stack — it is purely an environment push.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    PushScope,

    /// Pops the anonymous block scope pushed by the matching [`PushScope`].
    ///
    /// Any variables declared (via `let`) inside the block are discarded when
    /// this node executes.  If the block is exited early (e.g. via `jump` or
    /// `return`), the VM's scope-unwinding in those handlers takes care of
    /// popping instead, so an unreachable `PopScope` is harmless.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    PopScope,

    /// Declare an enum type.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    DefineEnum {
        /// The enum's name (may be namespace-prefixed after [`IrGraph::merge`]).
        name: String,
        /// Ordered list of variant names.
        variants: Vec<String>,
    },

    /// Struct declaration: registers the struct's field names in the VM environment.
    ///
    /// Executing this node stores the struct schema (ordered field name list) in
    /// [`crate::vm::env::Environment`] so that struct constructor calls can build
    /// [`crate::runtime::value::RuntimeValue::Struct`] instances.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    DefineStruct {
        /// The struct type name.
        name: String,
        /// Ordered list of field names (types are not enforced at runtime).
        fields: Vec<String>,
    },

    /// Register a script-defined decorator in the VM's decorator table.
    ///
    /// Executing this node stores a `RuntimeValue::ScriptDecorator` into the
    /// environment under `name`, making it available for `@name(args)` applications.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    DefineScriptDecorator {
        /// The decorator's name (used as the environment key).
        name: String,
        /// Optional event-kind constraint (informational; checked at apply-time).
        event_constraint: crate::parser::ast::EventConstraint,
        /// Ordered parameter names (type annotations stripped — runtime ignores them).
        params: Vec<String>,
        /// The body block, kept as raw `Ast` for inline evaluation at apply-time.
        body: crate::parser::ast::Ast,
    },

    /// Register a user-defined pure function in the VM's environment.
    ///
    /// Executing this node stores a `RuntimeValue::Function` into the
    /// environment under `name`, making it callable via `name(args)`.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    DefineFunction {
        /// The function's name (the environment key).
        name: String,
        /// Ordered parameter names (type annotations stripped at compile time).
        params: Vec<String>,
        /// The function body, kept as raw `Ast` for inline evaluation on each call.
        body: crate::parser::ast::Ast,
    },

    /// Validate that a runtime-provided extern value was injected before execution.
    ///
    /// When executed, the VM checks that the host called
    /// [`crate::vm::env::Environment::provide_extern`] for `name` before the first
    /// [`crate::vm::Vm::next`] call. Returns [`crate::vm::VmError::ExternNotProvided`]
    /// if the value was not injected.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    ExternDecl {
        /// The extern variable name.
        name: String,
    },

    /// A merge point or pre-allocated placeholder.
    ///
    /// Used as a forward-reference during two-pass compilation; the compiler
    /// patches `Nop` nodes into real nodes once target addresses are known.
    ///
    /// Continues via a single [`IrEdge::Next`] edge, or has no outgoing edge
    /// when used as an unresolved terminal placeholder.
    Nop,

    /// Terminal node — execution ends here.  No outgoing edges.
    End,

    /// `todo!()` — a placeholder terminator.
    ///
    /// Signals that the author has not yet written this path.  The VM logs a
    /// warning and terminates execution cleanly, just like `End`.
    /// No outgoing edges.
    Todo,

    // ── Output-event nodes ──────────────────────────────────────────────────
    /// Emit a [`Event::Dialogue`] event and then continue.
    ///
    /// Continues via a single [`IrEdge::Next`] edge.
    Dialogue {
        /// The speakers expression (kept as raw [`Ast`]).
        speakers: Ast,
        /// The dialogue lines expression (kept as raw [`Ast`]).
        lines: Ast,
        /// Decorators attached to the `Dialogue` AST node.
        decorators: Vec<Decorator>,
        /// Localization key for this dialogue event, if any.
        /// `None` when the compiler was invoked without a file stem context.
        loc_id: Option<String>,
    },

    /// Emit a [`Event::Choice`] event and suspend until the player chooses.
    ///
    /// Each option's entry node is reachable via an [`IrEdge::Option`]`(i)` edge,
    /// where `i` is the option's index in [`options`][IrNodeKind::Choice::options].
    Choice {
        /// The available options.
        options: Vec<IrChoiceOption>,
        /// Decorators attached to the `Menu` AST node.
        decorators: Vec<Decorator>,
        /// Localization key for this choice event (the menu node itself), if any.
        /// `None` when the compiler was invoked without a file stem context.
        loc_id: Option<String>,
    },
}

// ─── Output Event types ───────────────────────────────────────────────────────

/// A serialisable event emitted by the VM to its consumer (e.g. a game engine).
///
/// Events are the *only* IR-level type that crosses the VM/consumer boundary,
/// and therefore the *only* IR-level type that requires `serde` support.
#[non_exhaustive]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Event {
    /// A character (or characters) speaks one or more lines.
    Dialogue {
        /// Evaluated list of speaker values.
        speakers: Vec<crate::runtime::value::RuntimeValue>,
        /// Evaluated list of dialogue lines.
        lines: Vec<crate::runtime::value::RuntimeValue>,
        /// Evaluated decorator fields (name → value).
        fields: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
        /// Localization key for this dialogue, or `None` if no file context was provided.
        loc_id: Option<String>,
        /// Fluent variable bindings collected from the active scope when this event fired.
        /// Includes both `@fluent`-tagged variables and string-interpolation variables.
        fluent_vars: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
        /// Pre-localized dialogue text populated when a `Localizer` is attached to the VM.
        /// `None` if no localizer is present or `loc_id` is `None`.
        localized_text: Option<String>,
    },
    /// The player is presented with a set of options to choose from.
    Choice {
        /// The available choices.
        options: Vec<ChoiceEvent>,
        /// Evaluated decorator fields (name → value).
        fields: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
        /// Localization key for this choice event (the menu as a whole), or `None`.
        loc_id: Option<String>,
        /// Fluent variable bindings for this choice event's scope context.
        fluent_vars: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
        /// `true` when the menu has a wildcard/default option (`_ { ... }`).
        /// The host can use this to display a timeout indicator or skip UI.
        /// To trigger the default, call `vm.next(None)` while the choice is pending.
        has_default: bool,
    },
}

/// A single choice option as emitted inside a [`Event::Choice`] event.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ChoiceEvent {
    /// The display label shown to the player.
    pub label: String,
    /// Evaluated decorator fields for this option.
    pub fields: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
    /// Localization key for this option, or `None` if no file context was provided.
    pub loc_id: Option<String>,
    /// Fluent variable bindings for this option.
    pub fluent_vars: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
    /// Pre-localized option label populated when a `Localizer` is attached.
    pub localized_label: Option<String>,
}

// ─── VmStep ──────────────────────────────────────────────────────────────────

/// The result of a single [`crate::vm::Vm::next`] call.
///
/// This enum replaces the `Option<Result<Event, VmError>>` return type with a
/// value that is self-documenting and pattern-matches cleanly in game engine
/// integration code.
///
/// # Example
///
/// ```rust,ignore
/// loop {
///     match vm.next(choice.take()) {
///         VmStep::Event(event) => handle(event),
///         VmStep::Ended        => break,
///         VmStep::Error(e)     => return Err(e),
///     }
/// }
/// ```
#[non_exhaustive]
#[derive(Debug)]
pub enum VmStep {
    /// The VM produced an observable [`Event`] (dialogue or choice).
    Event(Event),
    /// The script has ended — no more events will be produced.
    Ended,
    /// A runtime error occurred.
    Error(crate::vm::VmError),
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

/// Format a cross-module namespaced label: `"alias::label_name"`.
///
/// Used consistently wherever a module-qualified name is stored or resolved.
pub fn namespace(alias: &str, name: &str) -> String {
    format!("{alias}::{name}")
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use petgraph::Direction;
    use petgraph::visit::EdgeRef;

    use super::*;

    // ── helpers ──────────────────────────────────────────────────────────────

    /// Build a trivial one-node graph (just an `End` node) with a given entry.
    fn single_end_graph() -> IrGraph {
        let mut g = IrGraph::new();
        let e = g.push(IrNodeKind::End);
        g.entry = Some(e);
        g
    }

    // ── basic node sanity ─────────────────────────────────────────────────────

    #[test]
    fn todo_node_has_no_successors() {
        let mut graph = IrGraph::new();
        let id = graph.push(IrNodeKind::Todo);
        // Weight is accessible and is the right kind.
        assert!(matches!(
            graph.graph.node_weight(id),
            Some(IrNodeKind::Todo)
        ));
        // Todo is a terminal — it must carry no outgoing edges.
        assert_eq!(
            graph
                .graph
                .neighbors_directed(id, Direction::Outgoing)
                .count(),
            0,
            "Todo is a terminal node and must have no outgoing edges"
        );
    }

    // ── IrGraph::merge ────────────────────────────────────────────────────────

    #[test]
    #[allow(deprecated)]
    fn merge_offsets_node_ids() {
        // base: one End node; module: one End node.
        // After merge the graph must contain both nodes, and the returned
        // index map must contain the module's original End node index.
        let mut base = single_end_graph(); // 1 node
        let module = single_end_graph(); // 1 node
        let module_end = module.entry.unwrap();

        let index_map = base.merge(module, "foo");

        assert_eq!(
            base.graph.node_count(),
            2,
            "base must contain both nodes after merge"
        );
        let mapped = index_map.get(&module_end);
        assert!(
            mapped.is_some(),
            "module's entry node must appear in the returned index map"
        );
        assert!(
            matches!(
                base.graph.node_weight(*mapped.unwrap()),
                Some(IrNodeKind::End)
            ),
            "mapped entry must point to an End node in the merged graph"
        );
    }

    #[test]
    #[allow(deprecated)]
    fn merge_namespaces_labels() {
        let mut base = IrGraph::new();
        let e = base.push(IrNodeKind::End);
        base.entry = Some(e);

        let mut module = IrGraph::new();
        let start = module.push(IrNodeKind::End);
        module.entry = Some(start);
        module.labels.insert("start".to_string(), start);

        let index_map = base.merge(module, "mymod");

        // Label must be namespaced as "mymod::start".
        assert!(
            base.labels.contains_key("mymod::start"),
            "label must be namespaced after merge"
        );
        // The remapped NodeIndex must still point to a live End node.
        let label_idx = base.labels["mymod::start"];
        assert!(
            matches!(base.graph.node_weight(label_idx), Some(IrNodeKind::End)),
            "namespaced label must resolve to the merged End node"
        );
        // The index map must also contain the module's start node.
        assert!(
            index_map.contains_key(&start),
            "module start node must appear in the returned index map"
        );
    }

    #[test]
    #[allow(deprecated)]
    fn merge_jump_target_remapped() {
        // Build a module:  End ←[Jump]─ Jump  (edge connects jump→target).
        let mut module = IrGraph::new();
        let target = module.push(IrNodeKind::End);
        let jump = module.push(IrNodeKind::Jump);
        module.add_edge(jump, target, IrEdge::Jump);
        module.entry = Some(jump);

        // Base already has 2 nodes so there is a non-trivial offset scenario.
        let mut base = IrGraph::new();
        let e0 = base.push(IrNodeKind::End);
        let _e1 = base.push(IrNodeKind::End);
        base.entry = Some(e0);

        let index_map = base.merge(module, "mod");

        // After merge: 4 nodes total.
        assert_eq!(base.graph.node_count(), 4);
        // The module's target and jump nodes must be in the map.
        assert!(
            index_map.contains_key(&target),
            "target must be in index_map"
        );
        assert!(index_map.contains_key(&jump), "jump must be in index_map");

        // Find the Jump node in the merged graph.
        let jump_idx = base
            .graph
            .node_indices()
            .find(|&idx| matches!(base.graph.node_weight(idx), Some(IrNodeKind::Jump)));
        assert!(jump_idx.is_some(), "Jump node must exist after merge");
        let jump_idx = jump_idx.unwrap();

        // The Jump node must carry exactly one outgoing IrEdge::Jump arc.
        let jump_edges: Vec<_> = base
            .graph
            .edges_directed(jump_idx, Direction::Outgoing)
            .collect();
        assert_eq!(
            jump_edges.len(),
            1,
            "Jump must have exactly one outgoing edge"
        );
        assert_eq!(jump_edges[0].weight(), &IrEdge::Jump);

        // The destination of that arc must be an End node.
        let target_idx = jump_edges[0].target();
        assert!(
            matches!(base.graph.node_weight(target_idx), Some(IrNodeKind::End)),
            "jump target must be an End node"
        );
    }

    #[test]
    #[allow(deprecated)]
    fn merge_node_end_sentinel_preserved() {
        // A Nop node with *no outgoing edges* is the petgraph equivalent of the
        // old `Nop { next: NODE_END }`.  After merging it must still have zero
        // outgoing edges — the absence of an edge is the sentinel.
        let mut module = IrGraph::new();
        let nop = module.push(IrNodeKind::Nop); // no edge added → no continuation
        module.entry = Some(nop);

        let mut base = IrGraph::new();
        let e0 = base.push(IrNodeKind::End);
        base.entry = Some(e0);

        let _index_map = base.merge(module, "m");

        // Find the Nop in the merged graph.
        let nop_idx = base
            .graph
            .node_indices()
            .find(|&idx| matches!(base.graph.node_weight(idx), Some(IrNodeKind::Nop)));
        assert!(nop_idx.is_some(), "Nop must exist after merge");
        let nop_idx = nop_idx.unwrap();

        assert_eq!(
            base.graph
                .neighbors_directed(nop_idx, Direction::Outgoing)
                .count(),
            0,
            "Nop with no continuation must retain zero outgoing edges after merge"
        );
    }

    #[test]
    #[allow(deprecated)]
    fn merge_empty_other_is_noop() {
        let mut base = single_end_graph();
        let original_count = base.graph.node_count();

        let empty = IrGraph::new(); // no nodes pushed
        let index_map = base.merge(empty, "empty");

        assert_eq!(
            base.graph.node_count(),
            original_count,
            "merging an empty graph must not add nodes"
        );
        assert!(
            index_map.is_empty(),
            "merging an empty graph must yield an empty index map"
        );
    }

    #[test]
    #[allow(deprecated)]
    fn merge_multiple_labels_all_namespaced() {
        let mut base = IrGraph::new();
        let _e = base.push(IrNodeKind::End);
        base.entry = Some(_e);

        let mut module = IrGraph::new();
        let a = module.push(IrNodeKind::End); // first label entry
        let b = module.push(IrNodeKind::End); // second label entry
        module.entry = Some(a);
        module.labels.insert("alpha".to_string(), a);
        module.labels.insert("beta".to_string(), b);

        let index_map = base.merge(module, "lib");

        assert!(
            base.labels.contains_key("lib::alpha"),
            "alpha must be namespaced"
        );
        assert!(
            base.labels.contains_key("lib::beta"),
            "beta must be namespaced"
        );
        // Both original node indices must appear in the map.
        assert!(
            index_map.contains_key(&a),
            "alpha node must be in index_map"
        );
        assert!(index_map.contains_key(&b), "beta node must be in index_map");

        let alpha_idx = base.labels["lib::alpha"];
        let beta_idx = base.labels["lib::beta"];

        // Both must resolve to live End nodes.
        assert!(matches!(
            base.graph.node_weight(alpha_idx),
            Some(IrNodeKind::End)
        ));
        assert!(matches!(
            base.graph.node_weight(beta_idx),
            Some(IrNodeKind::End)
        ));
        // They must be distinct nodes.
        assert_ne!(
            alpha_idx, beta_idx,
            "alpha and beta must be different nodes"
        );
    }

    #[test]
    #[allow(deprecated)]
    fn merge_branch_targets_remapped() {
        let mut base = IrGraph::new();
        let _e0 = base.push(IrNodeKind::End);
        base.entry = Some(_e0);

        let mut module = IrGraph::new();
        let then_end = module.push(IrNodeKind::End);
        let else_end = module.push(IrNodeKind::End);

        use crate::parser::ast::{Ast, AstContent};
        use crate::runtime::value::RuntimeValue;
        let cond = Ast::new(AstContent::Value(RuntimeValue::Bool(true)));
        let branch = module.push(IrNodeKind::Branch { condition: cond });
        module.add_edge(branch, then_end, IrEdge::Then);
        module.add_edge(branch, else_end, IrEdge::Else);
        module.entry = Some(branch);

        let index_map = base.merge(module, "br");

        // After merge: base (1) + module (3) = 4 nodes.
        assert_eq!(base.graph.node_count(), 4);
        // All three module nodes must appear in the map.
        assert!(
            index_map.contains_key(&then_end),
            "then_end must be in index_map"
        );
        assert!(
            index_map.contains_key(&else_end),
            "else_end must be in index_map"
        );
        assert!(
            index_map.contains_key(&branch),
            "branch must be in index_map"
        );

        // Find the Branch node in the merged graph.
        let branch_idx = base
            .graph
            .node_indices()
            .find(|&idx| matches!(base.graph.node_weight(idx), Some(IrNodeKind::Branch { .. })));
        assert!(branch_idx.is_some(), "Branch node must exist after merge");
        let branch_idx = branch_idx.unwrap();

        // Branch must carry exactly two outgoing edges: Then and Else.
        let edges: Vec<_> = base
            .graph
            .edges_directed(branch_idx, Direction::Outgoing)
            .collect();
        assert_eq!(
            edges.len(),
            2,
            "Branch must have exactly two outgoing edges after merge"
        );

        let has_then = edges.iter().any(|e| matches!(e.weight(), IrEdge::Then));
        let has_else = edges.iter().any(|e| matches!(e.weight(), IrEdge::Else));
        assert!(has_then, "Branch must have a Then edge after merge");
        assert!(has_else, "Branch must have an Else edge after merge");

        // Both targets must be End nodes.
        for edge in &edges {
            assert!(
                matches!(base.graph.node_weight(edge.target()), Some(IrNodeKind::End)),
                "both branch targets must be End nodes after merge"
            );
        }
    }

    #[test]
    #[allow(deprecated)]
    fn merge_preserves_cluster_names_and_label_sources() {
        let mut base = IrGraph::new();
        let n0 = base.push(IrNodeKind::Nop);
        base.entry = Some(n0);

        let mut other = IrGraph::new();
        let n1 = other.push(IrNodeKind::EnterScope {
            label: "greet".into(),
        });
        other.labels.insert("greet".into(), n1);
        other.cluster_names.insert(n1, "greet".into());
        other.label_sources.insert(n1, "lib.urd".into());
        other.entry = Some(n1);

        let index_map = base.merge(other, "lib");

        // Verify cluster_names were preserved with namespacing.
        let new_idx = index_map[&n1];
        assert_eq!(
            base.cluster_names.get(&new_idx),
            Some(&"lib::greet".to_string()),
            "cluster_names must be preserved and namespaced after merge"
        );

        // Verify label_sources were preserved.
        assert_eq!(
            base.label_sources.get(&new_idx),
            Some(&"lib.urd".to_string()),
            "label_sources must be preserved after merge"
        );
    }
}
