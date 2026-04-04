# Extern Values

Extern values are the bridge between your game engine and your Urd scripts.
They let you declare a name and type inside the script while leaving the actual
value to be supplied by the host application at runtime.

---

## Declaring an Extern

Use the `extern` keyword followed by the variable name and an optional type
annotation:

```urd
extern player_name: str
extern difficulty: int
extern has_dlc: bool
```

An extern declaration tells the compiler "this name exists and has this type, but
its value comes from outside the script." There is no initializer — the right
side of the declaration is always empty.

---

## Using Extern Values

Once declared, extern values are used exactly like any other variable — in
dialogue interpolation, conditions, function arguments, and more:

```urd
extern player_name: str
extern difficulty: int

const narrator = :{ name: "Narrator", name_color: "white" }

@entry
label greeting {
    narrator: "Welcome, {player_name}!"

    if difficulty >= 3 {
        narrator: "You've chosen a punishing difficulty. Good luck."
    } else {
        narrator: "A pleasant stroll through the countryside awaits."
    }
}
```

The script reads `player_name` and `difficulty` as though they were constants,
but their actual values are injected by the host before execution begins.

---

## Providing Values from the Host

On the Rust side, the host calls `vm.provide_extern()` to inject each value
before stepping the VM:

```rust
use urd::prelude::*;

let graph = urd::compile("script.urd", &mut loader)?;
let mut vm = Vm::new(graph, DecoratorRegistry::default())?;

// Inject extern values before execution
vm.provide_extern("player_name", RuntimeValue::Str("Kael".into()));
vm.provide_extern("difficulty", RuntimeValue::Int(3));
vm.provide_extern("has_dlc", RuntimeValue::Bool(true));

// Now drive the VM
loop {
    match vm.next(None) {
        VmStep::Event(event) => { /* handle event */ }
        VmStep::Ended => break,
        VmStep::Error(e) => {
            eprintln!("Error: {e}");
            break;
        }
    }
}
```

### Updating Extern Values

The host can call `provide_extern()` multiple times with the same name to update
a value mid-execution. This is useful for values that change over time — a
player's position, current health, time of day, etc.:

```rust
// Initial injection
vm.provide_extern("time_of_day", RuntimeValue::Str("morning".into()));

// ... later, between VM steps ...
vm.provide_extern("time_of_day", RuntimeValue::Str("evening".into()));
```

Each call to `provide_extern` overwrites the previous value for that name.

---

## The `ExternNotProvided` Error

If the VM reaches an extern variable that has not been injected by the host, it
halts with a `VmError::ExternNotProvided` error:

```rust
VmError::ExternNotProvided("player_name")
// "extern value 'player_name' was not provided by the host runtime before execution"
```

This is a **runtime error**, not a compile-time error. The compiler cannot know
whether the host will remember to call `provide_extern()` — it only knows that
the declaration exists.

> **Tip:** Always provide all extern values before calling `vm.next()` for the
> first time. A good practice is to iterate over your script's extern
> declarations and inject each one during VM initialization.

---

## Read-Only from Script Code

Extern values are **read-only** from the script's perspective. Any attempt to
reassign an extern variable inside the script produces a runtime error:

```urd
extern player_name: str

@entry
label start {
    # ✗ Runtime error — cannot reassign an extern value
    player_name = "Imposter"
}
```

Only the host runtime can modify extern values (via `provide_extern()`). This
guarantees that game-engine state cannot be corrupted by script logic.

---

## Type Annotations

While the type annotation on an extern is optional, it is strongly recommended.
It enables the compiler to perform type checking at every usage site:

```urd
# With type — the compiler verifies all usages expect a str
extern player_name: str

# Without type — valid, but no compile-time type checking
extern some_flag
```

Supported type annotations include all of Urd's runtime types: `str`, `int`,
`float`, `bool`, `list`, `map`, and named types like enums and structs.

---

## Practical Patterns

### Player Identity

```urd
extern player_name: str
extern player_title: str

const narrator = :{ name: "Narrator", name_color: "white" }

@entry
label intro {
    narrator: "All hail {player_title} {player_name}!"
}
```

### Difficulty Scaling

```urd
extern difficulty: int

fn enemy_health(base: int) -> int {
    return base * difficulty
}

label combat {
    let hp = enemy_health(50)
    narrator: "The goblin has {hp} hit points."
}
```

### Feature Flags

```urd
extern has_dlc: bool
extern debug_mode: bool

@entry
label hub {
    narrator: "You stand in the town square."

    if has_dlc {
        narrator: "A mysterious portal shimmers near the fountain."
    }

    if debug_mode {
        narrator: "[DEBUG] Current state: hub"
    }
}
```

### Namespaced Extern Access

When working with imports, extern values from other modules are accessed through
the import alias, just like any other cross-module binding:

```urd
import "config.urd" as config

@entry
label start {
    narrator: "Difficulty is set to {config.difficulty}."
}
```

---

## Extern vs. Global

It is worth understanding when to use `extern` versus `global`:

| Feature | `extern` | `global` |
|---------|----------|----------|
| Value source | Host application | Script initializer |
| Mutable in script | No | Yes |
| Mutable by host | Yes (via `provide_extern`) | No |
| Persists across labels | Yes | Yes |
| Requires host setup | Yes | No |
| Typical use | Engine state, player data | Game-tracked script state |

Use `extern` for values that the game engine owns — player name, settings, world
state. Use `global` for values that the script itself manages — quest progress,
dialogue flags, inventory counts.

---

## Summary

- `extern name: Type` declares a host-provided value
- The host injects it via `vm.provide_extern("name", value)`
- `ExternNotProvided` error if used but never injected
- Read-only from script code; only the host can update
- Type annotations are optional but recommended
- Use externs for game engine ↔ script communication