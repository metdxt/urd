# Extern Values

Extern values let you bridge game engine state into an Urd script without
recompilation. The script declares what it needs; the host provides the actual
values at runtime.

## Declaring Externs in Script

In your `.urd` file, declare extern variables with a type annotation:

```urd
extern player_name: str
extern player_level: int
extern difficulty: str

[player_name]: "I've reached level {player_level} on {difficulty} mode!"
```

The script can read these values anywhere — in dialogue interpolation,
conditions, match expressions, and so on. If a script reads an extern that the
host has not provided, the VM returns `VmError::ExternNotProvided`.

## Providing Externs from Rust

Use `vm.provide_extern(name, value)` to inject values into the VM's
environment:

```rust
use urd::prelude::*;

// After creating the VM...
vm.provide_extern("player_name", RuntimeValue::Str("Alice".into()));
vm.provide_extern("player_level", RuntimeValue::Int(12));
vm.provide_extern("difficulty", RuntimeValue::Str("hard".into()));
```

Each call associates a `RuntimeValue` with the name declared in the script.

## Timing

Externs can be provided at two points:

1. **Before execution** — set all externs after `Vm::new()` but before the
   first `vm.next()` call. This is the most common pattern.

2. **Between steps** — update externs between `vm.next()` calls to reflect
   changing game state. For example, if a player's health changes mid-dialogue,
   you can update the extern before advancing.

```rust
let mut vm = Vm::new(graph, registry)?;

// Provide initial values before execution starts.
vm.provide_extern("gold", RuntimeValue::Int(100));

loop {
    match vm.next(None) {
        VmStep::Event(Event::Dialogue { .. }) => {
            // Maybe the player spent gold in a shop UI...
            vm.provide_extern("gold", RuntimeValue::Int(75));
        }
        VmStep::Ended => break,
        VmStep::Error(e) => return Err(e.into()),
    }
}
```

## Error: `ExternNotProvided`

If the script evaluates an extern variable that was never provided, the VM
returns:

```rust
VmStep::Error(VmError::ExternNotProvided("player_name".into()))
```

This is a hard error — the VM cannot continue past it. Always ensure every
`extern` declaration in the script has a corresponding `provide_extern` call on
the Rust side.

A defensive pattern is to provide all externs immediately after VM construction:

```rust
let mut vm = Vm::new(graph, registry)?;

// Provide every extern the script expects.
vm.provide_extern("player_name", RuntimeValue::Str("Unknown".into()));
vm.provide_extern("player_level", RuntimeValue::Int(1));
vm.provide_extern("difficulty", RuntimeValue::Str("normal".into()));

// Now safe to run.
loop {
    match vm.next(None) {
        // ...
# VmStep::Ended => break,
# VmStep::Error(e) => return Err(e.into()),
# _ => {}
    }
}
```

## Common Use Cases

Externs are ideal for injecting information that originates outside the dialogue
system:

| Extern | Type | Purpose |
|--------|------|---------|
| `player_name` | `str` | Display the player's chosen name |
| `player_level` | `int` | Gate dialogue behind level checks |
| `gold` | `int` | Branch on whether the player can afford something |
| `difficulty` | `str` | Adjust tone or options by difficulty |
| `has_key` | `bool` | Unlock dialogue paths based on inventory |
| `npc_mood` | `str` | NPCs react based on prior interactions |

## Type Safety

The script declares a type annotation on each extern (`extern x: int`), but
this is currently documentation-level — the VM does not enforce the type at
injection time. If you provide a `Str` where the script expects an `Int`, you
will get a `VmError::TypeError` when the script tries to use the value in an
incompatible operation.

Make sure the `RuntimeValue` variant you provide matches what the script
expects:

| Script type | RuntimeValue variant |
|-------------|---------------------|
| `str`       | `RuntimeValue::Str(...)` |
| `int`       | `RuntimeValue::Int(...)` |
| `float`     | `RuntimeValue::Float(...)` |
| `bool`      | `RuntimeValue::Bool(...)` |

## Externs vs. Script Variables

Externs and regular script variables serve different purposes:

- **Script variables** (`let x = 10`) are internal to the script. The host
  cannot set them, and they are initialized by the script itself.
- **Extern variables** (`extern x: int`) are declared by the script but
  *provided* by the host. They act as the script's contract with the outside
  world.

This separation keeps scripts portable — a script can declare what data it
needs, and any host environment can satisfy those declarations with its own
values. No recompilation needed.

## Extern Objects

While primitive externs work for simple values, game engines often need to
expose **live objects** — a player character, an NPC, a quest tracker — where
the script can read and write individual fields. The `ExternObject` trait makes
this possible.

### The `ExternObject` Trait

```rust
use urd::prelude::*;

pub trait ExternObject: Send + Sync {
    /// Type name for display and pattern matching (e.g. "Player").
    fn type_name(&self) -> &str;

    /// Human-readable string representation (used in string interpolation).
    fn display(&self) -> String { format!("<{}>", self.type_name()) }

    /// Read a field by name.
    fn get(&self, field: &str) -> Result<RuntimeValue, String>;

    /// Write a field by name.
    fn set(&mut self, field: &str, value: RuntimeValue) -> Result<(), String>;

    /// List available field names (for introspection).
    fn fields(&self) -> Vec<String> { vec![] }

    /// Try to cast this object to a RuntimeValue of the given type.
    fn cast(&self, target: &str) -> Result<RuntimeValue, String> {
        Err(format!("cannot cast {} to '{target}'", self.type_name()))
    }
}
```

Implement this trait for any type you want scripts to interact with. Field
names are stringly-typed — the set of fields can be dynamic.

### Deriving `ExternObject`

For structs with named fields, use the derive macro to generate the full
implementation automatically:

```rust
use urd::prelude::*;

#[derive(ExternObject)]
#[extern_object(type_name = "Player")]
struct Player {
    hp: i32,
    name: String,

    #[extern_object(readonly)]
    id: u64,

    #[extern_object(skip)]
    internal_state: SomeInternalType,

    #[extern_object(rename = "pos_x")]
    position_x: f64,
}
```

**Container attributes** (`#[extern_object(...)]` on the struct):

| Attribute | Description |
|-----------|-------------|
| `type_name = "..."` | Override the type name. Defaults to the struct name. |

**Field attributes** (`#[extern_object(...)]` on a field):

| Attribute | Description |
|-----------|-------------|
| `skip` | Omit entirely — not readable, writable, or listed. |
| `readonly` | Readable but not writable from scripts. |
| `rename = "..."` | Expose under a different name in scripts. |

The derive generates `type_name()`, `display()`, `get()`, `set()`, and
`fields()` — all from the struct definition. Field types must implement
`IntoRuntimeValue` and `FromRuntimeValue`.

### Conversion Traits

The derive macro uses two traits to convert between Rust types and
`RuntimeValue`:

```rust
pub trait IntoRuntimeValue {
    fn into_runtime_value(&self) -> RuntimeValue;
}

pub trait FromRuntimeValue: Sized {
    fn from_runtime_value(value: &RuntimeValue) -> Result<Self, String>;
}
```

Built-in implementations are provided for:

| Rust type | RuntimeValue variant | Notes |
|-----------|---------------------|-------|
| `bool` | `Bool` | |
| `i8`, `i16`, `i32`, `i64` | `Int` | Narrowing types range-check on `FromRuntimeValue` |
| `u8`, `u16`, `u32`, `u64` | `Int` | Range-checked; `u64` may overflow |
| `f32`, `f64` | `Float` | `f32` widens/narrows through `f64` |
| `String` | `Str` | |
| `Option<T>` | `Null` or `T` | `None` ↔ `Null` |
| `Vec<T>` | `List` | Elements converted recursively |
| `RuntimeValue` | (identity) | Pass-through |

For custom types, implement these traits manually.

### Providing Extern Objects

Wrap your object in an `ExternHandle` and provide it as a
`RuntimeValue::Extern`:

```rust
use urd::prelude::*;

let player = Player { hp: 100, name: "Kael".into(), id: 1, internal_state: ..., position_x: 0.0 };
let handle = ExternHandle::new(player);

vm.provide_extern("player", RuntimeValue::Extern(handle));
```

### Shared References

`ExternHandle` uses `Arc<RwLock<dyn ExternObject>>` internally — cloning is
cheap and preserves identity. The host can keep its own reference to mutate the
object between VM steps:

```rust
let handle = ExternHandle::new(player);
let host_handle = handle.clone();

vm.provide_extern("player", RuntimeValue::Extern(handle));

// Drive the VM...
loop {
    match vm.next(None) {
        VmStep::Event(Event::Dialogue { .. }) => {
            // Host-side mutation: update HP based on game logic
            host_handle.set("hp", RuntimeValue::Int(75)).unwrap();
        }
        VmStep::Ended => break,
        VmStep::Error(e) => { eprintln!("{e}"); break; }
    }
}
```

Mutations through the host handle are immediately visible to the script on the
next field access.

You can also use `ExternHandle::from_arc()` if you need to construct the
`Arc<RwLock<_>>` yourself:

```rust
use std::sync::{Arc, RwLock};

let arc: Arc<RwLock<dyn ExternObject>> = Arc::new(RwLock::new(player));
let handle = ExternHandle::from_arc(Arc::clone(&arc));
```

### What Scripts Can Do

From the script's perspective, extern objects support:

| Operation | Syntax | Example |
|-----------|--------|---------|
| Field read (dot) | `obj.field` | `player.hp` |
| Field read (subscript) | `obj["field"]` | `player["hp"]` |
| Field write | `obj["field"] = value` | `player["hp"] = 50` |
| String interpolation | `"{obj.field}"` | `"HP: {player.hp}"` |
| Display | `"{obj}"` | Uses `ExternObject::display()` |
| Method: `to_string()` | `obj.to_string()` | Returns display string |
| Method: `type_name()` | `obj.type_name()` | Returns type name |
| Method: `fields()` | `obj.fields()` | Returns field name list |
| Method: `cast(type)` | `obj.cast("map")` | Attempts conversion |
| Truthiness | `if obj { ... }` | Always truthy |
| Equality | `a == b` | Identity (pointer) equality |