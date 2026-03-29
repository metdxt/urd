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

use std::collections::HashMap;

use crate::parser::ast::{Ast, DeclKind, Decorator, MatchPattern};

// ─── NodeId ──────────────────────────────────────────────────────────────────

/// Opaque index into an [`IrGraph`]'s node arena.
///
/// A `NodeId` is simply a `u32` wrapped in a newtype so that the compiler
/// cannot accidentally confuse it with plain integers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
