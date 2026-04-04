# Script-Defined Decorators

Beyond the built-in decorators (`@entry`, `@fluent`, `@id`, `@lint`), Urd lets
you define your own decorators directly in script code. Script-defined decorators
inject custom metadata into dialogue and choice events — text speed, camera
effects, audio cues, emotion tags, or anything else your game engine needs.

---

## Defining a Decorator

Use the `decorator` keyword, followed by a name, an event constraint, optional
parameters, and a body that returns a map:

```urd
decorator slow<event: dialogue>(speed: float) {
    return :{ speed: speed }
}
```

Breaking this down:

| Part | Meaning |
|------|---------|
| `slow` | The decorator's name — used as `@slow(...)` |
| `<event: dialogue>` | Event constraint — this decorator can only be applied to dialogue events |
| `(speed: float)` | Parameters — values passed when the decorator is used |
| `return :{ speed: speed }` | The body must return a map whose entries become event `fields` |

---

## Using a Script-Defined Decorator

Once defined, apply it with the `@name(args)` syntax just like any built-in
decorator:

```urd
decorator slow<event: dialogue>(speed: float) {
    return :{ speed: speed }
}

const narrator = :{ name: "Narrator", name_color: "white" }

label example {
    @slow(0.5)
    narrator: "This text appears slowly..."

    @slow(2.0)
    narrator: "This text appears quickly!"
}
```

When the VM emits the `Event::Dialogue` for the first line, its `fields` map
will contain `{ "speed": 0.5 }`. The host engine can read this field and adjust
the text rendering speed accordingly.

---

## Event Constraints

The `<event: constraint>` clause restricts which event types the decorator can be
applied to. Three constraints are available:

| Constraint | Applies to |
|------------|------------|
| `dialogue` | Dialogue events (`Event::Dialogue`) only |
| `choice`   | Choice/menu events (`Event::Choice`) only |
| `any`      | Both dialogue and choice events |

### Dialogue-Only Decorator

```urd
decorator emotion<event: dialogue>(mood: str) {
    return :{ emotion: mood }
}

label scene {
    @emotion("angry")
    guard: "You shall not pass!"
}
```

### Choice-Only Decorator

```urd
decorator highlight<event: choice>(color: str) {
    return :{ highlight_color: color }
}

label shop {
    narrator: "What would you like to buy?"

    menu {
        @highlight("green")
        "Buy a potion" {
            narrator: "You purchase a potion."
        }
        "Leave" {
            jump town_square
        }
    }
}
```

### Any-Event Decorator

```urd
decorator debug_tag<event: any>(tag: str) {
    return :{ debug: tag }
}

label test {
    @debug_tag("line_1")
    narrator: "Some dialogue."

    @debug_tag("menu_1")
    menu {
        "Option A" { jump a }
        "Option B" { jump b }
    }
}
```

### The `ConstraintViolation` Error

If you apply a decorator to the wrong event type, the compiler (or VM) emits a
`ConstraintViolation` error:

```urd
decorator slow<event: dialogue>(speed: float) {
    return :{ speed: speed }
}

label broken {
    # ✗ ConstraintViolation: @slow is constrained to 'dialogue' events,
    #   but is applied to a 'choice' event
    @slow(0.5)
    menu {
        "Option A" { jump a }
    }
}
```

This catches misuse at compile time — you don't have to wait until the script
runs to discover the mismatch.

---

## Decorator Body

The decorator body is a block of statements that **must return a map**. The
key-value pairs in the returned map are merged into the `fields` map on the
emitted event:

```urd
decorator cinematic<event: dialogue>(camera: str, duration: float) {
    return :{
        camera_mode: camera,
        camera_duration: duration
    }
}

label dramatic_reveal {
    @cinematic("zoom_in", 2.0)
    narrator: "The ancient door creaks open."
}
```

The resulting `Event::Dialogue` will have:

```rust
fields = {
    "camera_mode": RuntimeValue::Str("zoom_in"),
    "camera_duration": RuntimeValue::Float(2.0),
}
```

### Computed Fields

The body can contain logic — conditionals, arithmetic, or anything else that
ultimately returns a map:

```urd
decorator typed<event: dialogue>(speed: float) {
    let delay = 1.0 / speed
    return :{ char_delay: delay, is_slow: speed < 1.0 }
}
```

---

## Multiple Decorators

Multiple decorators can be stacked on a single statement. Their fields are merged
into the same `fields` map:

```urd
decorator slow<event: dialogue>(speed: float) {
    return :{ speed: speed }
}

decorator emotion<event: dialogue>(mood: str) {
    return :{ emotion: mood }
}

label confrontation {
    @slow(0.3)
    @emotion("menacing")
    villain: "You have no idea what you're dealing with."
}
```

The emitted event's `fields` will contain both `{ "speed": 0.3, "emotion": "menacing" }`.

If two decorators return the same key, the last one applied wins.

---

## Decorators on Menu Options

Decorators can be applied to individual menu options, not just the menu itself.
Each option's `ChoiceEvent` carries its own `fields` map:

```urd
decorator tooltip<event: choice>(text: str) {
    return :{ tooltip: text }
}

label merchant {
    narrator: "What would you like?"

    menu {
        @tooltip("Restores 50 HP")
        "Buy healing potion (10g)" {
            gold = gold - 10
        }
        @tooltip("Cures all status effects")
        "Buy antidote (15g)" {
            gold = gold - 15
        }
        "Leave" {
            jump town_square
        }
    }
}
```

---

## Parameters

Decorator parameters support type annotations and accept any expression at the
call site:

```urd
decorator timed<event: choice>(seconds: float, fallback: str) {
    return :{ timeout: seconds, timeout_target: fallback }
}

label quick_choice {
    @timed(5.0, "fallback_scene")
    menu {
        "Fight" { jump combat }
        "Flee"  { jump escape }
    }
}

label fallback_scene {
    narrator: "You hesitated too long."
}
```

Note that `fallback` has type `str` — it receives the label name as a string,
which the host engine can use to determine where to jump if the timer expires.
Labels are a compile-time construct and do not exist as runtime values, so
decorator parameters that reference labels use their names as strings.

---

## Scope and Visibility

Script-defined decorators are **top-level declarations**. They are available
throughout the file in which they are defined. Decorators cannot be defined
inside labels, functions, or other blocks.

```urd
# ✓ Top-level — correct
decorator slow<event: dialogue>(speed: float) {
    return :{ speed: speed }
}

label scene {
    # ✗ Cannot define a decorator inside a label
    # decorator fast<event: dialogue>(speed: float) { ... }

    @slow(0.5)
    narrator: "Slowly..."
}
```

To share decorators across files, define them in a shared file and import the
file. The decorator definitions become available to any script that imports
them.

---

## Host-Side Consumption

The fields injected by script decorators appear in the `fields` map on the
event. In Rust, you read them like any other map entry:

```rust
match vm.next(None) {
    VmStep::Event(Event::Dialogue { fields, lines, speakers, .. }) => {
        // Check for decorator-injected fields
        if let Some(RuntimeValue::Float(speed)) = fields.get("speed") {
            set_text_speed(*speed);
        }
        if let Some(RuntimeValue::Str(emotion)) = fields.get("emotion") {
            set_character_emotion(emotion);
        }

        // Render dialogue as usual
        render_dialogue(&speakers, &lines);
    }
    // ...
}
```

This decoupled design means your scripts declare *intent* (slow text, angry
emotion, camera shake) and the host engine decides *how* to realize it. The
same script can drive a terminal prototype, a 2D engine, or a full 3D game —
the decorators carry the metadata, and each host interprets it differently.

---

## Comparison with Rust-Registered Decorators

Urd supports two kinds of custom decorators:

| Feature | Script-Defined | Rust-Registered |
|---------|---------------|-----------------|
| Defined in | `.urd` files | Rust code via `DecoratorRegistry` |
| Logic runs in | The Urd VM | Rust (native code) |
| Can access Rust APIs | No | Yes |
| Portable across hosts | Yes | No (tied to Rust host) |
| Ideal for | Metadata, tags, simple computed fields | Complex logic, API calls, validation |

Use script-defined decorators when the decorator's job is to attach data. Use
Rust-registered decorators when you need native logic, external API access, or
host-specific computation. See the
[Decorator Registry](../integration/decorator-registry.md) integration guide for
Rust-side registration.

---

## Quick Reference

```urd
# Define a decorator
decorator name<event: constraint>(params) {
    return :{ key: value }
}

# Event constraints
# <event: dialogue>  — dialogue events only
# <event: choice>    — choice/menu events only
# <event: any>       — both dialogue and choice events

# Apply a decorator
@name(args)
speaker: "Dialogue line."

@name(args)
menu {
    "Option" { ... }
}

# Stack multiple decorators
@slow(0.5)
@emotion("sad")
speaker: "A sorrowful whisper..."
```
