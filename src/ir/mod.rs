//! # Intermediate Representation (IR) Module
//!
//! This module defines the Intermediate Representation used internally by the Urd VM.
//! The compiler transforms an [`Ast`] into an [`IrGraph`], which the VM then executes.
//!
//! ## Key Types
//!
//! - [`NodeId`]: A lightweight index into the node arena.
//! - [`IrGraph`]: The compiled script — an arena of [`IrNode`]s with an entry point.
//! - [`IrNode`]: A single node in the graph, identified by its [`NodeId`].
//! - [`IrNodeKind`]: The payload of a node, discriminating between all node kinds.
//! - [`Event`]: Serialisable output events emitted by the VM to its consumer.
//!
//! ## Submodules
//!
//! - [`dot`]: Graphviz DOT renderer for [`IrGraph`].

pub mod dot;

use std::collections::HashMap;

use crate::parser::ast::{Ast, DeclKind, Decorator, MatchPattern};

// ─── NodeId ──────────────────────────────────────────────────────────────────

/// Opaque index into an [`IrGraph`]'s node arena.
///
/// A `NodeId` is simply a `u32` wrapped in a newtype so that the compiler
/// cannot accidentally confuse it with plain integers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct NodeId(pub u32);

/// Sentinel value meaning "no continuation" (e.g. the last node of a branch).
///
/// Any node that holds a `next: NodeId` equal to `NODE_END` has no successor —
/// execution stops (or returns) at that point.
pub const NODE_END: NodeId = NodeId(u32::MAX);

// ─── IrGraph ─────────────────────────────────────────────────────────────────

/// The compiled form of an Urd script.
///
/// Nodes are stored in a flat arena (`Vec<IrNode>`); a [`NodeId`] is just an
/// index into that vector.  The VM begins execution at `entry` and follows
/// `next` links until it reaches a terminal node ([`IrNodeKind::End`] or a
/// node whose `next` is [`NODE_END`]).
#[derive(Debug)]
pub struct IrGraph {
    /// Flat arena of all nodes.  `nodes[id.0 as usize]` is the node for `id`.
    pub nodes: Vec<IrNode>,
    /// The [`NodeId`] at which execution begins.
    pub entry: NodeId,
    /// Maps every `label ident { … }` label name to the [`NodeId`] of the
    /// corresponding [`IrNodeKind::EnterScope`] node.
    pub labels: HashMap<String, NodeId>,
}

impl IrGraph {
    /// Creates an empty graph with a placeholder [`IrNodeKind::End`] as node 0.
    pub(crate) fn new() -> Self {
        IrGraph {
            nodes: Vec::new(),
            entry: NODE_END,
            labels: HashMap::new(),
        }
    }

    /// Appends a node to the arena and returns its freshly-assigned [`NodeId`].
    pub(crate) fn push(&mut self, kind: IrNodeKind) -> NodeId {
        let id = NodeId(self.nodes.len() as u32);
        self.nodes.push(IrNode { id, kind });
        id
    }

    /// Mutably borrows the node at `id`.
    ///
    /// # Panics
    /// Panics if `id` is out of bounds (should never happen inside the compiler).
    pub(crate) fn node_mut(&mut self, id: NodeId) -> &mut IrNode {
        &mut self.nodes[id.0 as usize]
    }

    /// Merge `other` into `self`, renumbering all `NodeId`s in `other` by an
    /// offset of `self.nodes.len()` so they remain unique.
    ///
    /// All nodes from `other` are appended to `self.nodes`. Labels from `other`
    /// are added to `self.labels` prefixed with `"alias::"` so they don't
    /// conflict with local labels.
    ///
    /// # Label namespacing
    /// A label `"start"` from a module imported as `"foo"` becomes `"foo::start"`.
    ///
    /// Returns the `offset` that was applied to all incoming NodeIds (useful for
    /// callers that need to translate the imported module's `entry` NodeId).
    pub fn merge(&mut self, other: IrGraph, alias: &str) -> u32 {
        let offset = self.nodes.len() as u32;

        // Append the renumbered nodes from `other`.
        for mut node in other.nodes {
            node.id = NodeId(node.id.0 + offset);
            remap_node_kind(&mut node.kind, offset);
            self.nodes.push(node);
        }

        // Insert all of other's labels under "alias::label_name".
        for (label_name, node_id) in other.labels {
            let namespaced = format!("{}::{}", alias, label_name);
            self.labels.insert(namespaced, NodeId(node_id.0 + offset));
        }

        offset
    }
}

// ─── IrNode ──────────────────────────────────────────────────────────────────

/// A single node in the [`IrGraph`] arena.
#[derive(Debug)]
pub struct IrNode {
    /// This node's unique identifier (== its index in `IrGraph::nodes`).
    pub id: NodeId,
    /// The semantic payload of this node.
    pub kind: IrNodeKind,
}

// ─── Supporting types ─────────────────────────────────────────────────────────

/// A single selectable option inside a [`IrNodeKind::Choice`] node.
#[derive(Debug, Clone)]
pub struct IrChoiceOption {
    /// The display text shown to the player for this option.
    pub label: String,
    /// The [`NodeId`] of the first node to execute when this option is chosen.
    pub entry: NodeId,
    /// Decorators attached to the `MenuOption` AST node.
    pub decorators: Vec<Decorator>,
}

/// One arm of a compiled [`IrNodeKind::Switch`] node.
#[derive(Debug, Clone)]
pub struct SwitchArm {
    /// The pattern to test the scrutinee against.
    pub pattern: MatchPattern,
    /// The [`NodeId`] of the node to jump to when this arm matches.
    pub target: NodeId,
}

// ─── IrNodeKind ──────────────────────────────────────────────────────────────

/// The payload of an [`IrNode`], discriminating between every kind of IR node.
///
/// Nodes fall into two broad categories:
///
/// * **Internal nodes** — consumed only by the VM; never surfaced to the
///   script consumer (game engine, test harness, etc.).
/// * **Output-event nodes** — [`Dialogue`][IrNodeKind::Dialogue] and
///   [`Choice`][IrNodeKind::Choice] — cause the VM to emit a serialisable
///   [`Event`] and pause execution until the consumer responds.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum IrNodeKind {
    // ── Internal nodes ──────────────────────────────────────────────────────
    /// Declare or assign a variable.
    Assign {
        /// The variable name being bound.
        var: String,
        /// Whether this is a `global`, `const`, or `let` declaration.
        scope: DeclKind,
        /// The initialiser / right-hand-side expression (kept as raw [`Ast`]).
        expr: Ast,
        /// The node to execute next.
        next: NodeId,
    },

    /// Evaluate an expression purely for its side effects and discard the result.
    Eval {
        /// The expression to evaluate (kept as raw [`Ast`]).
        expr: Ast,
        /// The node to execute next.
        next: NodeId,
    },

    /// Conditional branch.
    Branch {
        /// The boolean condition expression.
        condition: Ast,
        /// Node to execute when the condition is truthy.
        then_node: NodeId,
        /// Node to execute when the condition is falsy.
        else_node: NodeId,
    },

    /// Multi-way pattern match.
    Switch {
        /// The expression being matched.
        scrutinee: Ast,
        /// Ordered list of pattern → target pairs.
        arms: Vec<SwitchArm>,
        /// Node to execute when no arm matches, or `None` to fall through.
        default: Option<NodeId>,
    },

    /// Unconditional jump to a previously-compiled node.
    Jump {
        /// The target node (always a resolved [`NodeId`]).
        target: NodeId,
    },

    /// Return from the current script (or sub-routine).
    Return {
        /// Optional return value expression.
        value: Option<Ast>,
    },

    /// Marks the entry point of a labeled block (`label ident { … }`).
    EnterScope {
        /// The label name.
        label: String,
        /// The first node inside the block.
        next: NodeId,
    },

    /// Marks the exit point of a labeled block; resumes normal control flow.
    ExitScope {
        /// The label name (mirrors the matching [`EnterScope`][IrNodeKind::EnterScope]).
        label: String,
        /// The node to execute after the block exits.
        next: NodeId,
    },

    /// Declare an enum type.
    DefineEnum {
        /// The enum's name.
        name: String,
        /// Ordered list of variant names.
        variants: Vec<String>,
        /// The node to execute next.
        next: NodeId,
    },

    /// Register a script-defined decorator in the VM's decorator table.
    ///
    /// Executing this node stores a `RuntimeValue::ScriptDecorator` into the
    /// environment under `name`, making it available for `@name(args)` applications.
    DefineScriptDecorator {
        /// The decorator's name (used as the environment key).
        name: String,
        /// Optional event-kind constraint (informational; checked at apply-time).
        event_constraint: crate::parser::ast::EventConstraint,
        /// Ordered parameter names (type annotations stripped — runtime ignores them).
        params: Vec<String>,
        /// The body block, kept as raw `Ast` for inline evaluation at apply-time.
        body: crate::parser::ast::Ast,
        /// The next node to execute.
        next: NodeId,
    },

    /// A merge point or pre-allocated placeholder.
    ///
    /// Used as a forward-reference during two-pass compilation; the compiler
    /// patches `Nop` nodes into real nodes once target addresses are known.
    Nop {
        /// The node to execute next.
        next: NodeId,
    },

    /// Terminal node — execution ends here.
    End,

    // ── Output-event nodes ──────────────────────────────────────────────────
    /// Emit a [`Event::Dialogue`] event and then continue.
    Dialogue {
        /// The speakers expression (kept as raw [`Ast`]).
        speakers: Ast,
        /// The dialogue lines expression (kept as raw [`Ast`]).
        lines: Ast,
        /// Decorators attached to the `Dialogue` AST node.
        decorators: Vec<Decorator>,
        /// The node to execute after the dialogue event is acknowledged.
        next: NodeId,
    },

    /// Emit a [`Event::Choice`] event and suspend until the player chooses.
    ///
    /// Unlike most nodes, `Choice` has **no `next` field** — control transfers
    /// via the `entry` field of whichever [`IrChoiceOption`] the player picks.
    Choice {
        /// The available options.
        options: Vec<IrChoiceOption>,
        /// Decorators attached to the `Menu` AST node.
        decorators: Vec<Decorator>,
    },
}

// ─── Output Event types ───────────────────────────────────────────────────────

/// A serialisable event emitted by the VM to its consumer (e.g. a game engine).
///
/// Events are the *only* IR-level type that crosses the VM/consumer boundary,
/// and therefore the *only* IR-level type that requires `serde` support.
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
    },
    /// The player is presented with a set of options to choose from.
    Choice {
        /// The available choices.
        options: Vec<ChoiceEvent>,
        /// Evaluated decorator fields (name → value).
        fields: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
    },
}

/// A single choice option as emitted inside a [`Event::Choice`] event.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ChoiceEvent {
    /// The display label shown to the player.
    pub label: String,
    /// Evaluated decorator fields for this option.
    pub fields: std::collections::HashMap<String, crate::runtime::value::RuntimeValue>,
}

// ─── NodeId remapping helper ─────────────────────────────────────────────────

/// Adds `offset` to every [`NodeId`] embedded inside `kind`.
///
/// The [`NODE_END`] sentinel is never shifted — it must always retain its
/// "no successor" meaning regardless of the offset applied to real node ids.
fn remap_node_kind(kind: &mut IrNodeKind, offset: u32) {
    /// Offset a single [`NodeId`], leaving the [`NODE_END`] sentinel unchanged.
    fn shift(id: NodeId, offset: u32) -> NodeId {
        if id == NODE_END {
            NODE_END
        } else {
            NodeId(id.0 + offset)
        }
    }

    match kind {
        IrNodeKind::Assign { next, .. } => *next = shift(*next, offset),
        IrNodeKind::Eval { next, .. } => *next = shift(*next, offset),
        IrNodeKind::Branch {
            then_node,
            else_node,
            ..
        } => {
            *then_node = shift(*then_node, offset);
            *else_node = shift(*else_node, offset);
        }
        IrNodeKind::Switch { arms, default, .. } => {
            for arm in arms.iter_mut() {
                arm.target = shift(arm.target, offset);
            }
            *default = default.map(|id| shift(id, offset));
        }
        IrNodeKind::Jump { target } => *target = shift(*target, offset),
        IrNodeKind::Return { .. } => {}
        IrNodeKind::EnterScope { next, .. } => *next = shift(*next, offset),
        IrNodeKind::ExitScope { next, .. } => *next = shift(*next, offset),
        IrNodeKind::DefineEnum { next, .. } => *next = shift(*next, offset),
        IrNodeKind::DefineScriptDecorator { next, .. } => *next = shift(*next, offset),
        IrNodeKind::Nop { next } => *next = shift(*next, offset),
        IrNodeKind::End => {}
        IrNodeKind::Dialogue { next, .. } => *next = shift(*next, offset),
        IrNodeKind::Choice { options, .. } => {
            for opt in options.iter_mut() {
                opt.entry = shift(opt.entry, offset);
            }
        }
    }
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a trivial one-node graph (just an End node) with a given entry.
    fn single_end_graph() -> IrGraph {
        let mut g = IrGraph::new();
        let e = g.push(IrNodeKind::End);
        g.entry = e;
        g
    }

    #[test]
    fn merge_offsets_node_ids() {
        // base: one End node (id=0)
        let mut base = single_end_graph();
        // module: one End node (id=0 in isolation)
        let module = single_end_graph();

        let offset = base.merge(module, "foo");
        // base originally had 1 node, so offset must be 1
        assert_eq!(offset, 1);
        // After merge, base should have 2 nodes
        assert_eq!(base.nodes.len(), 2);
        // The merged node's id should have been renumbered to 1
        assert_eq!(base.nodes[1].id, NodeId(1));
    }

    #[test]
    fn merge_namespaces_labels() {
        let mut base = IrGraph::new();
        let e = base.push(IrNodeKind::End);
        base.entry = e;

        let mut module = IrGraph::new();
        let start = module.push(IrNodeKind::End);
        module.entry = start;
        module.labels.insert("start".to_string(), start);

        base.merge(module, "mymod");

        // Label must be namespaced as "mymod::start"
        assert!(base.labels.contains_key("mymod::start"));
        // The NodeId must be offset by 1 (base had 1 node)
        assert_eq!(base.labels["mymod::start"], NodeId(1));
    }

    #[test]
    fn merge_jump_target_remapped() {
        // Build a module with a Jump pointing at node 0
        let mut module = IrGraph::new();
        let target = module.push(IrNodeKind::End); // NodeId(0)
        let jump = module.push(IrNodeKind::Jump { target }); // NodeId(1)
        module.entry = jump;

        // Base has 2 nodes first
        let mut base = IrGraph::new();
        base.push(IrNodeKind::End); // NodeId(0)
        base.push(IrNodeKind::End); // NodeId(1)
        base.entry = NodeId(0);

        base.merge(module, "mod");

        // After merge, offset = 2. The jump's target was 0, now must be 2.
        if let IrNodeKind::Jump { target } = &base.nodes[3].kind {
            assert_eq!(*target, NodeId(2));
        } else {
            panic!("expected Jump node");
        }
    }

    #[test]
    fn merge_node_end_sentinel_preserved() {
        // Nodes with NODE_END next-links must NOT have offset added.
        let mut module = IrGraph::new();
        let nop = module.push(IrNodeKind::Nop { next: NODE_END });
        module.entry = nop;

        let mut base = IrGraph::new();
        base.push(IrNodeKind::End); // offset will be 1
        base.entry = NodeId(0);

        base.merge(module, "m");

        if let IrNodeKind::Nop { next } = &base.nodes[1].kind {
            assert_eq!(*next, NODE_END, "NODE_END sentinel must not be offset");
        } else {
            panic!("expected Nop");
        }
    }

    #[test]
    fn merge_empty_other_is_noop() {
        let mut base = single_end_graph();
        let original_len = base.nodes.len();

        let empty = IrGraph::new(); // no nodes pushed
        let offset = base.merge(empty, "empty");

        assert_eq!(offset, original_len as u32);
        assert_eq!(base.nodes.len(), original_len);
    }

    #[test]
    fn merge_multiple_labels_all_namespaced() {
        let mut base = IrGraph::new();
        base.push(IrNodeKind::End);
        base.entry = NodeId(0);

        let mut module = IrGraph::new();
        let a = module.push(IrNodeKind::End); // NodeId(0)
        let b = module.push(IrNodeKind::End); // NodeId(1)
        module.entry = a;
        module.labels.insert("alpha".to_string(), a);
        module.labels.insert("beta".to_string(), b);

        base.merge(module, "lib");

        assert!(base.labels.contains_key("lib::alpha"));
        assert!(base.labels.contains_key("lib::beta"));
        assert_eq!(base.labels["lib::alpha"], NodeId(1)); // 0 + offset(1)
        assert_eq!(base.labels["lib::beta"], NodeId(2)); // 1 + offset(1)
    }

    #[test]
    fn merge_branch_targets_remapped() {
        let mut base = IrGraph::new();
        base.push(IrNodeKind::End); // NodeId(0) — offset will be 1
        base.entry = NodeId(0);

        let mut module = IrGraph::new();
        let then_end = module.push(IrNodeKind::End); // NodeId(0)
        let else_end = module.push(IrNodeKind::End); // NodeId(1)
        use crate::parser::ast::{Ast, AstContent};
        use crate::runtime::value::RuntimeValue;
        let cond = Ast::new(AstContent::Value(RuntimeValue::Bool(true)));
        module.push(IrNodeKind::Branch {
            condition: cond,
            then_node: then_end,
            else_node: else_end,
        }); // NodeId(2)
        module.entry = NodeId(2);

        base.merge(module, "br");

        // The branch node is at index 3 (base had 1 node, module had 3).
        if let IrNodeKind::Branch {
            then_node,
            else_node,
            ..
        } = &base.nodes[3].kind
        {
            assert_eq!(*then_node, NodeId(1), "then_node should be offset by 1");
            assert_eq!(*else_node, NodeId(2), "else_node should be offset by 1");
        } else {
            panic!("expected Branch node at index 3");
        }
    }
}
