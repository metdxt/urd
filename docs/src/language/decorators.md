# Decorators

Decorators are annotations that enrich dialogue events, menus, and labels with
extra metadata. They are Urd's primary mechanism for passing game-specific
information — animation cues, timing hints, UI instructions — from script
authors to the host engine without polluting the dialogue logic itself.

---

## Syntax

Decorators are prefixed with `@` and placed immediately before the node they
annotate:

```urd
@slow(0.5)
narrator: "The wind howls through the canyon..."

@timed(10.0)
menu {
    "Run!" { jump escape }
    "Stand your ground" { jump fight }
}
```

A decorator can take zero or more arguments in parentheses. When there are no
arguments, the parentheses are omitted:

```urd
@entry
label start {
    narrator: "Welcome."
}
```

Multiple decorators can be stacked on the same node — they are applied in order,
top to bottom:

```urd
@slow(0.3)
@camera_shake(0.5)
narrator: "The ground trembles beneath your feet!"
```

---

## What Decorators Produce

Every decorator ultimately contributes key-value pairs to the `fields` map on
the emitted event. When the VM yields an `Event::Dialogue` or `Event::Choice`,
the `fields` HashMap contains everything that the applied decorators returned:

```rust
VmStep::Event(Event::Dialogue {
    speakers,
    lines,
    fields,  // ← decorator-injected metadata lives here
    ..
})
```

The host engine reads these fields and acts on them however it sees fit — play a
sound effect, change the text speed, shake the camera, highlight a menu option,
or anything else.

---

## Three Kinds of Decorators

Urd supports three sources of decorator definitions:

### 1. Built-in Decorators

These are part of the language itself and have special compiler-level behaviour:

| Decorator | Purpose |
|-----------|---------|
| `@entry` | Marks the starting label of a script |
| `@fluent` | Tags a variable for Fluent localization injection |
| `@id("key")` | Overrides the auto-generated localization ID |
| `@lint(check)` | Enables opt-in static analysis checks |

Built-in decorators are always available — no imports or registration required.
See [Built-in Decorators](./builtin-decorators.md) for full details.

### 2. Script-Defined Decorators

Authors can define decorators directly in their `.urd` scripts using the
`decorator` keyword:

```urd
decorator slow<event: dialogue>(speed: float) {
    return :{ speed: speed }
}

label intro {
    @slow(0.5)
    narrator: "This text appears slowly..."
}
```

Script-defined decorators specify an event constraint (which node types they can
be applied to) and return a map whose entries become event fields.

See [Script-Defined Decorators](./script-decorators.md) for the full syntax and
constraint system.

### 3. Rust-Registered Decorators

For host-engine decorators that need to exist across all scripts without being
redefined in each file, you can register them from Rust via the
`DecoratorRegistry`:

```rust
use urd::prelude::*;

let mut registry = DecoratorRegistry::default();
registry.register("camera_shake", |args| {
    let mut fields = HashMap::new();
    if let Some(intensity) = args.first() {
        fields.insert("camera_shake".to_string(), intensity.clone());
    }
    Ok(fields)
});

let mut vm = Vm::new(graph, registry)?;
```

Rust-registered decorators are resolved at runtime when the VM encounters them.
They are invisible to static analysis but available to every script the VM runs.

See the [Decorator Registry](../integration/decorator-registry.md) integration
guide for details.

---

## Where Decorators Can Be Applied

Decorators can annotate several kinds of nodes:

| Target | Example |
|--------|---------|
| Labels | `@entry` before `label start { ... }` |
| Dialogue lines | `@slow(0.5)` before `narrator: "..."` |
| Menu blocks | `@timed(10.0)` before `menu { ... }` |
| Individual menu options | `@highlight("green")` before `"Option text" { ... }` |

The decorator's event constraint (for script-defined decorators) determines
which targets are valid. Applying a dialogue-only decorator to a menu produces a
`ConstraintViolation` error.

---

## Decorator Resolution Order

When the VM encounters a decorated node, it resolves decorators in this order:

1. **Built-in decorators** are handled by the compiler/VM directly.
2. **Script-defined decorators** are looked up in the current file's scope.
3. **Rust-registered decorators** are looked up in the `DecoratorRegistry`.

If a decorator name is not found in any of these sources, the VM produces an
error.

---

## Combining Decorators

Multiple decorators compose additively — each one contributes its fields to the
same `fields` map. If two decorators write to the same key, the later one
(lower in the source) wins:

```urd
decorator slow<event: dialogue>(speed: float) {
    return :{ text_speed: speed }
}

decorator bold<event: dialogue>() {
    return :{ text_style: "bold" }
}

@slow(0.3)
@bold
narrator: "This is slow and bold."
# fields = { "text_speed": 0.3, "text_style": "bold" }
```

---

## Next Steps

- [Built-in Decorators](./builtin-decorators.md) — `@entry`, `@fluent`, `@id`, `@lint`
- [Script-Defined Decorators](./script-decorators.md) — the `decorator` keyword and event constraints
- [Decorator Registry](../integration/decorator-registry.md) — registering decorators from Rust