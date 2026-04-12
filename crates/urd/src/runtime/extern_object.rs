//! # External Object Bridge
//!
//! This module defines the [`ExternObject`] trait and [`ExternHandle`] wrapper
//! that allow host game engines to expose live objects into Urd scripts as
//! transparent, reference-counted handles.
//!
//! ## Design
//!
//! An `ExternObject` is a trait object behind `Arc<RwLock<_>>`, wrapped in an
//! [`ExternHandle`].  Cloning the handle creates a new reference to the **same**
//! underlying object — mutations through one reference are visible through all
//! others.  This makes externs behave like references/links to game objects
//! rather than value-type copies.
//!
//! The handle provides interior mutability via [`RwLock`] so that field writes
//! from script code (`player["hp"] = 42`) can flow through to the live game
//! object without requiring `&mut RuntimeValue`.
//!
//! ## Thread safety
//!
//! `ExternObject: Send + Sync` and `ExternHandle: Send + Sync + Clone` — the
//! same constraints the rest of Urd's pluggable traits use.

use std::fmt;
use std::sync::{Arc, RwLock};

use super::value::RuntimeValue;

// ── Trait ─────────────────────────────────────────────────────────────────────

/// A live game object exposed to Urd scripts.
///
/// Implement this trait for any host-side type (Godot node, ECS entity handle,
/// asset reference, …) that you want scripts to interact with transparently.
///
/// # Contract
///
/// - [`get`](ExternObject::get) / [`set`](ExternObject::set) should be cheap —
///   they run inline during expression evaluation.  Avoid blocking I/O.
/// - Field names are stringly-typed on purpose — the set of fields can be
///   dynamic (e.g. ECS components added at runtime).
/// - Returning `Err(message)` from any method surfaces as a [`VmError`] at
///   the script level; the message is shown to the script author.
///
/// [`VmError`]: crate::vm::VmError
pub trait ExternObject: Send + Sync {
    /// The type name of this external object (e.g. `"Node3D"`,
    /// `"CharacterBody3D"`).
    ///
    /// Used for error messages, debug output, and as the discriminator in
    /// `match` / pattern contexts if you choose to support that later.
    fn type_name(&self) -> &str;

    /// Human-readable string representation, used when the value appears in
    /// string interpolation (`"Hello {player}"`) or `to_string()` method
    /// calls.
    fn display(&self) -> String {
        format!("<{}>", self.type_name())
    }

    /// Read a field by name.
    ///
    /// Return `Ok(value)` on success or `Err(message)` if the field does not
    /// exist or cannot be read.
    fn get(&self, field: &str) -> Result<RuntimeValue, String>;

    /// Write a field by name.
    ///
    /// Return `Ok(())` on success or `Err(message)` if the field does not
    /// exist, is read-only, or the value type is incompatible.
    fn set(&mut self, field: &str, value: RuntimeValue) -> Result<(), String>;

    /// List the names of all readable fields.
    ///
    /// Primarily useful for editor auto-complete and introspection from
    /// scripts.  The default implementation returns an empty list.
    fn fields(&self) -> Vec<String> {
        vec![]
    }

    /// Try to cast / convert this object into a plain [`RuntimeValue`].
    ///
    /// `target` is the type name the script asked for (e.g. `"map"`,
    /// `"str"`, `"int"`).  Return `Err` if the cast is not supported.
    fn cast(&self, target: &str) -> Result<RuntimeValue, String> {
        Err(format!("cannot cast {} to '{target}'", self.type_name()))
    }
}

// ── Handle ────────────────────────────────────────────────────────────────────

/// A reference-counted, interiorly-mutable handle to a [`dyn ExternObject`].
///
/// This is the type stored inside [`RuntimeValue::Extern`].  Cloning is
/// cheap (`Arc::clone`) and preserves identity — two handles are equal
/// (`PartialEq`) if and only if they point to the exact same allocation.
///
/// All public methods acquire the internal [`RwLock`] for the duration of
/// the call.  In the unlikely event that the lock is poisoned (a thread
/// panicked while holding it), the methods return an `Err` rather than
/// panicking themselves.
#[derive(Clone)]
pub struct ExternHandle(Arc<RwLock<dyn ExternObject>>);

impl ExternHandle {
    /// Wrap an owned [`ExternObject`] in a new handle.
    pub fn new(obj: impl ExternObject + 'static) -> Self {
        Self(Arc::new(RwLock::new(obj)))
    }

    /// Wrap a pre-existing `Arc<RwLock<dyn ExternObject>>`.
    ///
    /// Useful when the host wants to retain its own `Arc` to the same object
    /// so it can mutate it between VM steps.
    pub fn from_arc(arc: Arc<RwLock<dyn ExternObject>>) -> Self {
        Self(arc)
    }

    /// Returns the underlying `Arc` (e.g. for the host to keep a reference).
    pub fn inner(&self) -> &Arc<RwLock<dyn ExternObject>> {
        &self.0
    }

    // ── Delegating helpers ────────────────────────────────────────────────

    /// The type name of the wrapped object.
    pub fn type_name(&self) -> Result<String, String> {
        self.0
            .read()
            .map(|obj| obj.type_name().to_owned())
            .map_err(|e| format!("extern lock poisoned: {e}"))
    }

    /// Human-readable string representation.
    pub fn display(&self) -> Result<String, String> {
        self.0
            .read()
            .map(|obj| obj.display())
            .map_err(|e| format!("extern lock poisoned: {e}"))
    }

    /// Read a field value.
    pub fn get(&self, field: &str) -> Result<RuntimeValue, String> {
        self.0
            .read()
            .map_err(|e| format!("extern lock poisoned: {e}"))?
            .get(field)
    }

    /// Write a field value.
    pub fn set(&self, field: &str, value: RuntimeValue) -> Result<(), String> {
        self.0
            .write()
            .map_err(|e| format!("extern lock poisoned: {e}"))?
            .set(field, value)
    }

    /// List available field names.
    pub fn fields(&self) -> Result<Vec<String>, String> {
        self.0
            .read()
            .map(|obj| obj.fields())
            .map_err(|e| format!("extern lock poisoned: {e}"))
    }

    /// Try to cast to a [`RuntimeValue`] of the given type.
    pub fn cast(&self, target: &str) -> Result<RuntimeValue, String> {
        self.0
            .read()
            .map_err(|e| format!("extern lock poisoned: {e}"))?
            .cast(target)
    }
}

// ── Trait impls for derive-friendliness ───────────────────────────────────────

impl fmt::Debug for ExternHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.read() {
            Ok(obj) => write!(f, "Extern<{}>", obj.type_name()),
            Err(_) => write!(f, "Extern<locked>"),
        }
    }
}

/// Two handles are equal iff they point to the exact same allocation.
///
/// This is *identity* equality, not structural equality — consistent with the
/// semantics of object references in most game engines.
impl PartialEq for ExternHandle {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

// ── Display helper ────────────────────────────────────────────────────────────

/// Format a [`RuntimeValue`] as a short, human-readable string.
///
/// This is a standalone helper used by the derive macro's generated `display()`
/// implementation.  It lives here (rather than in `vm::eval`) to avoid a
/// circular dependency between the `runtime` and `vm` modules.
pub fn display_brief(v: &RuntimeValue) -> String {
    match v {
        RuntimeValue::Null => "null".into(),
        RuntimeValue::Bool(b) => b.to_string(),
        RuntimeValue::Int(i) => i.to_string(),
        RuntimeValue::Float(f) => f.to_string(),
        RuntimeValue::Str(ps) => format!("\"{}\"", ps),
        RuntimeValue::Dice(n, s) => format!("{n}d{s}"),
        RuntimeValue::Roll(r) => {
            let parts: Vec<String> = r.iter().map(|v| v.to_string()).collect();
            format!("[{}]", parts.join(", "))
        }
        RuntimeValue::IdentPath(p) => p.join("."),
        RuntimeValue::Range {
            start,
            end,
            inclusive,
        } => {
            if *inclusive {
                format!("{start}..={end}")
            } else {
                format!("{start}..{end}")
            }
        }
        RuntimeValue::Map(m) => format!("map({})", m.len()),
        RuntimeValue::List(items) => {
            let parts: Vec<String> = items.iter().map(display_brief).collect();
            format!("[{}]", parts.join(", "))
        }
        RuntimeValue::Function { params, .. } => format!("fn({})", params.join(", ")),
        RuntimeValue::ScriptDecorator { .. } => "<decorator>".into(),
        RuntimeValue::Struct { name, fields } => format!("{name}({})", fields.len()),
        RuntimeValue::Extern(h) => h.display().unwrap_or_else(|e| format!("<extern: {e}>")),
    }
}

// ── Value conversion traits ───────────────────────────────────────────────────

/// Convert a Rust value into a [`RuntimeValue`].
///
/// Implement this for any type you want to expose as an extern object field.
/// Blanket implementations are provided for common primitive types.
pub trait IntoRuntimeValue {
    /// Convert `&self` into a [`RuntimeValue`].
    fn to_runtime_value(&self) -> RuntimeValue;
}

/// Reconstruct a Rust value from a [`RuntimeValue`].
///
/// Implement this for any type you want to be writable through an extern
/// object field.
pub trait FromRuntimeValue: Sized {
    /// Try to extract a value of `Self` from the given [`RuntimeValue`].
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String>;
}

impl IntoRuntimeValue for RuntimeValue {
    fn to_runtime_value(&self) -> RuntimeValue {
        self.clone()
    }
}

impl FromRuntimeValue for RuntimeValue {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        Ok(value.clone())
    }
}

// ── bool ──────────────────────────────────────────────────────────────────────

impl IntoRuntimeValue for bool {
    fn to_runtime_value(&self) -> RuntimeValue {
        RuntimeValue::Bool(*self)
    }
}

impl FromRuntimeValue for bool {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        match value {
            RuntimeValue::Bool(b) => Ok(*b),
            other => Err(format!("expected Bool, got {other:?}")),
        }
    }
}

// ── i64 ───────────────────────────────────────────────────────────────────────

impl IntoRuntimeValue for i64 {
    fn to_runtime_value(&self) -> RuntimeValue {
        RuntimeValue::Int(*self)
    }
}

impl FromRuntimeValue for i64 {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        match value {
            RuntimeValue::Int(i) => Ok(*i),
            other => Err(format!("expected Int, got {other:?}")),
        }
    }
}

// ── f64 ───────────────────────────────────────────────────────────────────────

impl IntoRuntimeValue for f64 {
    fn to_runtime_value(&self) -> RuntimeValue {
        RuntimeValue::Float(*self)
    }
}

impl FromRuntimeValue for f64 {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        match value {
            RuntimeValue::Float(f) => Ok(*f),
            RuntimeValue::Int(i) => Ok(*i as f64),
            other => Err(format!("expected Float, got {other:?}")),
        }
    }
}

// ── String ────────────────────────────────────────────────────────────────────

impl IntoRuntimeValue for String {
    fn to_runtime_value(&self) -> RuntimeValue {
        RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain(self))
    }
}

impl FromRuntimeValue for String {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        match value {
            RuntimeValue::Str(ps) => Ok(ps.to_string()),
            other => Err(format!("expected Str, got {other:?}")),
        }
    }
}

macro_rules! impl_int_via_i64 {
    ($($ty:ty),+) => { $(
        impl IntoRuntimeValue for $ty {
            fn to_runtime_value(&self) -> RuntimeValue {
                RuntimeValue::Int(*self as i64)
            }
        }

        impl FromRuntimeValue for $ty {
            fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
                let i = i64::from_runtime_value(value)?;
                <$ty>::try_from(i).map_err(|_| format!(
                    "integer {} out of range for {}",
                    i,
                    std::any::type_name::<$ty>()
                ))
            }
        }
    )+ };
}

impl_int_via_i64!(i8, i16, i32, u8, u16, u32);

impl IntoRuntimeValue for u64 {
    fn to_runtime_value(&self) -> RuntimeValue {
        RuntimeValue::Int(*self as i64)
    }
}

impl FromRuntimeValue for u64 {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        let i = i64::from_runtime_value(value)?;
        u64::try_from(i).map_err(|_| format!("integer {i} out of range for u64"))
    }
}

impl IntoRuntimeValue for f32 {
    fn to_runtime_value(&self) -> RuntimeValue {
        RuntimeValue::Float(*self as f64)
    }
}

impl FromRuntimeValue for f32 {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        let f = f64::from_runtime_value(value)?;
        Ok(f as f32)
    }
}

impl<T: IntoRuntimeValue> IntoRuntimeValue for Option<T> {
    fn to_runtime_value(&self) -> RuntimeValue {
        match self {
            Some(v) => v.to_runtime_value(),
            None => RuntimeValue::Null,
        }
    }
}

impl<T: FromRuntimeValue> FromRuntimeValue for Option<T> {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        match value {
            RuntimeValue::Null => Ok(None),
            other => T::from_runtime_value(other).map(Some),
        }
    }
}

impl<T: IntoRuntimeValue> IntoRuntimeValue for Vec<T> {
    fn to_runtime_value(&self) -> RuntimeValue {
        RuntimeValue::List(self.iter().map(|v| v.to_runtime_value()).collect())
    }
}

impl<T: FromRuntimeValue> FromRuntimeValue for Vec<T> {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String> {
        match value {
            RuntimeValue::List(items) => items.iter().map(T::from_runtime_value).collect(),
            other => Err(format!("expected List, got {other:?}")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    /// Minimal test object that stores fields in a map.
    struct FakeObject {
        name: String,
        props: HashMap<String, RuntimeValue>,
    }

    impl FakeObject {
        fn new(name: &str) -> Self {
            Self {
                name: name.to_string(),
                props: HashMap::new(),
            }
        }
    }

    impl ExternObject for FakeObject {
        fn type_name(&self) -> &str {
            "FakeObject"
        }

        fn display(&self) -> String {
            format!("FakeObject({})", self.name)
        }

        fn get(&self, field: &str) -> Result<RuntimeValue, String> {
            self.props
                .get(field)
                .cloned()
                .ok_or_else(|| format!("no field '{field}' on FakeObject"))
        }

        fn set(&mut self, field: &str, value: RuntimeValue) -> Result<(), String> {
            self.props.insert(field.to_string(), value);
            Ok(())
        }

        fn fields(&self) -> Vec<String> {
            self.props.keys().cloned().collect()
        }
    }

    #[test]
    fn handle_clones_share_identity() {
        let h1 = ExternHandle::new(FakeObject::new("a"));
        let h2 = h1.clone();
        assert_eq!(h1, h2, "cloned handles must be pointer-equal");
    }

    #[test]
    fn distinct_handles_are_not_equal() {
        let h1 = ExternHandle::new(FakeObject::new("a"));
        let h2 = ExternHandle::new(FakeObject::new("a"));
        assert_ne!(h1, h2, "different allocations must not compare equal");
    }

    #[test]
    fn get_set_round_trip() {
        let handle = ExternHandle::new(FakeObject::new("thing"));
        assert!(handle.get("hp").is_err());

        handle.set("hp", RuntimeValue::Int(100)).unwrap();
        assert_eq!(handle.get("hp").unwrap(), RuntimeValue::Int(100));
    }

    #[test]
    fn mutation_visible_through_clones() {
        let h1 = ExternHandle::new(FakeObject::new("shared"));
        let h2 = h1.clone();

        h1.set("x", RuntimeValue::Int(1)).unwrap();
        assert_eq!(h2.get("x").unwrap(), RuntimeValue::Int(1));
    }

    #[test]
    fn display_and_type_name() {
        let h = ExternHandle::new(FakeObject::new("hero"));
        assert_eq!(h.type_name().unwrap(), "FakeObject");
        assert_eq!(h.display().unwrap(), "FakeObject(hero)");
    }

    #[test]
    fn debug_format() {
        let h = ExternHandle::new(FakeObject::new("dbg"));
        let s = format!("{h:?}");
        assert_eq!(s, "Extern<FakeObject>");
    }

    #[test]
    fn default_cast_returns_err() {
        let h = ExternHandle::new(FakeObject::new("x"));
        assert!(h.cast("int").is_err());
    }

    #[test]
    fn from_arc_preserves_identity() {
        let arc: Arc<RwLock<dyn ExternObject>> =
            Arc::new(RwLock::new(FakeObject::new("shared_arc")));
        let h1 = ExternHandle::from_arc(Arc::clone(&arc));
        let h2 = ExternHandle::from_arc(arc);
        assert_eq!(h1, h2);
    }

    #[test]
    fn to_runtime_value_i32() {
        assert_eq!(42i32.to_runtime_value(), RuntimeValue::Int(42));
    }

    #[test]
    fn from_runtime_value_i32() {
        assert_eq!(
            i32::from_runtime_value(&RuntimeValue::Int(42)).unwrap(),
            42i32
        );
    }

    #[test]
    fn from_runtime_value_i32_overflow() {
        assert!(i32::from_runtime_value(&RuntimeValue::Int(i64::MAX)).is_err());
    }

    #[test]
    fn to_runtime_value_string() {
        let s = "hello".to_string();
        match s.to_runtime_value() {
            RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "hello"),
            other => panic!("expected Str, got {other:?}"),
        }
    }

    #[test]
    fn option_none_is_null() {
        let v: Option<i64> = None;
        assert_eq!(v.to_runtime_value(), RuntimeValue::Null);
    }

    #[test]
    fn option_some_unwraps() {
        let v: Option<i64> = Some(7);
        assert_eq!(v.to_runtime_value(), RuntimeValue::Int(7));
    }

    #[test]
    fn vec_round_trip() {
        let v = vec![1i64, 2, 3];
        let rv = v.to_runtime_value();
        assert_eq!(Vec::<i64>::from_runtime_value(&rv).unwrap(), vec![1, 2, 3]);
    }

    #[test]
    fn f64_from_int_coerces() {
        assert_eq!(f64::from_runtime_value(&RuntimeValue::Int(5)).unwrap(), 5.0);
    }
}
