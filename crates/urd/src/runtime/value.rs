//! # Runtime Value Module
//!
//! This module defines the value types used at runtime when executing Urd scripts.
//! The `RuntimeValue` enum represents all possible values that can be produced
//! by the interpreter during script execution.

use petgraph::stable_graph::NodeIndex;

use crate::lexer::{Token, strings::ParsedString};

// ─── NodeIndex serde helpers ──────────────────────────────────────────────────

/// Serde helper module for `petgraph::stable_graph::NodeIndex`.
///
/// `NodeIndex` serde support is gated behind petgraph's `serde-1` feature,
/// which we do not enable.  Instead we round-trip through the raw `usize`
/// index, which is stable for the lifetime of a single compiled [`crate::ir::IrGraph`].
mod node_index_serde {
    use petgraph::stable_graph::NodeIndex;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    #[allow(clippy::trivially_copy_pass_by_ref)]
    pub fn serialize<S: Serializer>(idx: &NodeIndex, ser: S) -> Result<S::Ok, S::Error> {
        idx.index().serialize(ser)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(de: D) -> Result<NodeIndex, D::Error> {
        let n = usize::deserialize(de)?;
        Ok(NodeIndex::new(n))
    }
}

/// Represents a value in the Urd runtime environment.
///
/// Runtime values are the result of evaluating expressions and the operands
/// for operations during script execution.
///
/// ## Serde notes
///
/// `Map` and `ScriptDecorator` are marked `#[serde(skip)]` because they hold
/// `Ast` nodes (which are not `Serialize`/`Deserialize`) and because they are
/// ephemeral, in-body-execution-only values that never appear in a serialised
/// [`crate::ir::Event`].  Attempting to serialise a `RuntimeValue` that is
/// `Map` or `ScriptDecorator` will silently omit the field; deserialising
/// back will never reconstruct them (which is correct — they only exist
/// transiently during script execution).
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum RuntimeValue {
    /// The null value
    Null,
    /// Boolean value (true or false)
    Bool(bool),
    /// 64-bit signed integer
    Int(i64),
    /// Double-precision floating point number
    Float(f64),
    /// String value with support for interpolation
    Str(ParsedString),
    /// Dice roll value (count, sides)
    Dice(u8, u8),
    /// Identifier representing a variable or property path
    IdentPath(Vec<String>),

    /// A reference to a compiled label in the script.
    ///
    /// Label values are pre-seeded into the VM environment at startup (one per
    /// `label name { }` block) and are fully serialisable.
    ///
    /// - `node_id` is the **reliable execution reference**: the concrete
    ///   [`NodeIndex`] of the label's [`crate::ir::IrNodeKind::EnterScope`] node
    ///   in the compiled [`crate::ir::IrGraph`].  This is unambiguous within a
    ///   single graph and is what the VM uses to navigate.  When multi-file
    ///   support arrives, this becomes a `(module_id, NodeIndex)` pair.
    /// - `name` is the human-readable label identifier, used for display,
    ///   string interpolation, and as the serialised form the game engine sees.
    Label {
        /// Human-readable label name (e.g. `"intro_scene"`).
        name: String,
        /// Concrete graph node this label resolves to.
        ///
        /// Serialised as a raw `usize` index via [`node_index_serde`] since
        /// petgraph's serde support requires an optional cargo feature.
        #[serde(with = "node_index_serde")]
        node_id: NodeIndex,
    },

    /// A runtime map value: `:{key: value, ...}` literals or the implicit
    /// `event` map passed to decorator bodies.
    ///
    /// `Ast` is not serialisable, so this variant is excluded from serde
    /// entirely — it is an ephemeral, in-execution-only value.
    #[serde(skip)]
    Map(std::collections::HashMap<String, Box<RuntimeValue>>),

    /// A script-defined decorator, stored as a first-class runtime value.
    ///
    /// `Ast` is not serialisable, so this variant is excluded from serde
    /// entirely — it is an ephemeral, in-execution-only value.
    #[serde(skip)]
    ScriptDecorator {
        /// Optional event-kind constraint (checked at apply-time).
        event_constraint: crate::parser::ast::EventConstraint,
        /// Ordered list of parameter names (type annotations already stripped
        /// by the compiler).
        params: Vec<String>,
        /// The decorator body, kept as raw `Ast` for inline evaluation.
        body: Box<crate::parser::ast::Ast>,
    },
}

#[allow(missing_docs)]
impl TryFrom<Token> for RuntimeValue {
    type Error = ();
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Null => Ok(Self::Null),
            Token::BoolLit(b) => Ok(Self::Bool(b)),
            Token::FloatLit(f) => Ok(Self::Float(f)),
            Token::IntLit(i) => Ok(Self::Int(i)),
            Token::StrLit(s) => Ok(Self::Str(s)),
            Token::Dice((count, sides)) => Ok(Self::Dice(count, sides)),
            Token::IdentPath(path) => Ok(Self::IdentPath(path)),
            _ => Err(()),
        }
    }
}
