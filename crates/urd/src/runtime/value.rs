//! # Runtime Value Module
//!
//! This module defines the value types used at runtime when executing Urd scripts.
//! The `RuntimeValue` enum represents all possible values that can be produced
//! by the interpreter during script execution.

use std::sync::{Arc, RwLock};

use super::extern_object::ExternHandle;
use crate::lexer::{Token, strings::ParsedString};

/// A shared, mutable reference to a value, used for complex runtime types
/// that require reference semantics (e.g., Map, List, Struct).
#[derive(Debug, Default)]
pub struct Shared<T>(Arc<RwLock<T>>);

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}

impl<T> Shared<T> {
    /// Acquires a read lock on the shared value.
    pub fn borrow(&self) -> std::sync::RwLockReadGuard<'_, T> {
        self.0.read().unwrap_or_else(|e| e.into_inner())
    }

    /// Acquires a write lock on the shared value.
    pub fn borrow_mut(&self) -> std::sync::RwLockWriteGuard<'_, T> {
        self.0.write().unwrap_or_else(|e| e.into_inner())
    }
}

impl<T: PartialEq> PartialEq for Shared<T> {
    fn eq(&self, other: &Self) -> bool {
        *self.borrow() == *other.borrow()
    }
}

impl<T: serde::Serialize> serde::Serialize for Shared<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.borrow().serialize(serializer)
    }
}

impl<'de, T: serde::Deserialize<'de>> serde::Deserialize<'de> for Shared<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        T::deserialize(deserializer).map(shared)
    }
}

/// Wraps a value in a new [`Shared`] container.
pub fn shared<T>(t: T) -> Shared<T> {
    Shared(Arc::new(RwLock::new(t)))
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

    /// The individual results of evaluating a [`RuntimeValue::Dice`] expression.
    ///
    /// Produced at runtime when a dice expression (e.g. `2d6`) is evaluated.
    /// Each element is a single die result in the range `1..=sides`.
    /// This variant is never present in an AST node and is never serialised.
    #[serde(skip)]
    Roll(Vec<i64>),

    /// Identifier representing a variable or property path
    IdentPath(Vec<String>),

    /// An integer range value: `start..end` (exclusive) or `start..=end` (inclusive).
    ///
    /// Ranges are integer-only. The `inclusive` flag distinguishes `..` from `..=`.
    /// Ranges are fully serialisable since all fields are primitive integers.
    ///
    /// ## Semantics
    ///
    /// - `len()`: exclusive → `max(0, end - start)`, inclusive → `max(0, end - start + 1)`
    /// - `contains(n)`: exclusive → `start <= n && n < end`, inclusive → `start <= n && n <= end`
    Range {
        /// The lower bound (inclusive start of the range)
        start: i64,
        /// The upper bound
        end: i64,
        /// `true` for `..=` (inclusive end), `false` for `..` (exclusive end)
        inclusive: bool,
    },

    /// A runtime map value: `:{key: value, ...}` literals or the implicit
    /// `event` map passed to decorator bodies.
    ///
    /// `Ast` is not serialisable, so this variant is excluded from serde
    /// entirely — it is an ephemeral, in-execution-only value.
    #[serde(skip)]
    Map(Shared<std::collections::HashMap<String, Box<RuntimeValue>>>),

    /// An ordered list of runtime values: `[a, b, c]` literals.
    ///
    /// Unlike `Map` and `ScriptDecorator`, `List` is fully serialisable as
    /// long as all its elements are themselves serialisable.
    ///
    /// ## Invariant (best-effort)
    ///
    /// A `List` should not contain non-serialisable elements
    /// (`ScriptDecorator`, `Function`, `Map`, `Roll`, `Struct`).
    /// Violating this will cause silent field omission or a panic during
    /// serialisation.
    ///
    /// The [`RuntimeValue::list`] constructor fires a `debug_assert!` in
    /// debug builds, but this check is compiled out in release builds and
    /// only covers a subset of non-serialisable variants. Direct
    /// construction of `List(vec![...])` bypasses the check entirely.
    List(Shared<Vec<RuntimeValue>>),

    /// A user-defined pure function value: `fn(x: int) -> int { x + 1 }`.
    ///
    /// Functions run in an isolated environment containing only their bound
    /// parameters — no access to the outer scope (pure).
    ///
    /// `Ast` is not serialisable, so this variant is excluded from serde.
    #[serde(skip)]
    Function {
        /// Ordered parameter names (type annotations stripped at compile time).
        params: Vec<String>,
        /// The function body, kept as raw `Ast` for inline evaluation on each call.
        body: Box<crate::parser::ast::Ast>,
    },

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

    /// A struct instance: `Point(1, 2)` after `struct Point { x: int, y: int }`.
    ///
    /// Constructed by the VM when a call expression names a registered struct
    /// type.  Field values are keyed by the field names declared in the struct.
    ///
    /// `Ast` may be referenced transitively via field values that are
    /// `Function` or `ScriptDecorator`, so this variant is excluded from serde.
    #[serde(skip)]
    Struct {
        /// The struct type name (e.g. `"Point"`).
        name: String,
        /// Field values keyed by field name.
        fields: Shared<std::collections::HashMap<String, RuntimeValue>>,
    },

    /// A live reference to a host game object exposed via [`ExternObject`].
    ///
    /// Cloning a value of this variant is cheap (`Arc::clone`) and preserves
    /// identity — all copies point to the **same** underlying object.
    /// Mutations through one copy (e.g. field writes) are visible through
    /// all others.
    ///
    /// Not serialisable — extern handles are ephemeral runtime links that
    /// only make sense while the host process is alive.
    ///
    /// [`ExternObject`]: super::extern_object::ExternObject
    #[serde(skip)]
    Extern(ExternHandle),
}

impl RuntimeValue {
    /// Constructs a [`RuntimeValue::List`] value.
    ///
    /// In debug builds this asserts that no element is a non-serialisable
    /// variant (`ScriptDecorator`, `Function`, `Map`, `Roll`, `Struct`).
    /// A `List` that contains any of these violates the serialisation
    /// invariant (see the `List` variant doc) and will produce incorrect
    /// output when the enclosing [`crate::ir::Event`] is serialised.
    ///
    /// # Panics (debug only)
    ///
    /// Panics in debug builds if any element is a non-serialisable variant.
    pub fn list(elements: Vec<RuntimeValue>) -> Self {
        debug_assert!(
            !elements.iter().any(|e| matches!(
                e,
                RuntimeValue::ScriptDecorator { .. }
                    | RuntimeValue::Function { .. }
                    | RuntimeValue::Map(_)
                    | RuntimeValue::Roll { .. }
                    | RuntimeValue::Struct { .. }
                    | RuntimeValue::Extern(_)
            )),
            "List elements must not contain non-serialisable values \
             (ScriptDecorator, Function, Map, Roll, Struct, Extern)"
        );
        RuntimeValue::List(shared(elements))
    }
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
