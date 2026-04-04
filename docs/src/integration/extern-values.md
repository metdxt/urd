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

```
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