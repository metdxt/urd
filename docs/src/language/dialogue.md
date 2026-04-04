# Dialogue

Dialogue is the core construct of Urd. Every game conversation, narration beat, and spoken line ultimately compiles down to a dialogue event that the VM yields to the host engine.

## Basic Syntax

The simplest form of dialogue is a speaker followed by a colon and a quoted string:

```urd
narrator: "The wind howls across the barren moor."
```

This produces a single `Event::Dialogue` that the VM yields when execution reaches this line. The host engine receives the speaker value, the line of text, and any attached decorator fields.

## What Constitutes a Speaker

The speaker position accepts any expression that resolves to a runtime value. In practice, speakers are almost always `const` bindings that hold a map with display metadata:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const zara     = :{ name: "Zara",     name_color: "cyan"  }

narrator: "You stand at the cave entrance."
zara: "Be careful in there."
```

However, any expression is valid in the speaker position — a variable, a function call, or a struct instance. Whatever value sits on the left side of the colon ends up in the `speakers` field of the emitted event.

See [Speakers](./speakers.md) for detailed guidance on defining speaker values.

## Multi-line Dialogue (Block Syntax)

When a character speaks multiple lines in sequence, use the block syntax with braces:

```urd
narrator: {
    "The wind howls across the barren moor."
    "Before you yawns the entrance to an ancient cave,"
    "its dark mouth breathing cold air like a sleeping beast."
}
```

Each quoted string becomes a separate entry in the `lines` Vec of the emitted event. This is a single dialogue event with three lines — **not** three separate events.

See [Multi-line Dialogue](./multiline-dialogue.md) for more details.

## String Interpolation in Dialogue

Dialogue lines support string interpolation with `{expression}` syntax:

```urd
global gold = 50
global health = 100

narrator: "You have {gold} gold and {health} health remaining."
narrator: "That comes to {gold + health} total resource points."
```

Interpolation can include field access:

```urd
narrator: "{zara.name} beckons you forward."
```

See [Strings & Interpolation](./strings.md) for full interpolation syntax.

## The Dialogue Event

When the VM encounters a dialogue node during execution, it yields a `VmStep::Event(Event::Dialogue { ... })` to the host. The event carries:

| Field | Type | Description |
|-------|------|-------------|
| `speakers` | `Vec<RuntimeValue>` | Evaluated speaker values (typically maps with `name`, `name_color`, etc.) |
| `lines` | `Vec<RuntimeValue>` | Evaluated dialogue lines (strings after interpolation) |
| `fields` | `HashMap<String, RuntimeValue>` | Decorator fields attached to this dialogue |
| `loc_id` | `Option<String>` | Localization key for translation lookup |
| `fluent_vars` | `HashMap<String, RuntimeValue>` | `@fluent`-tagged variables in scope when this event fired |
| `localized_text` | `Option<String>` | Pre-localized text if a `Localizer` is attached to the VM |

A typical host integration loop handles dialogue events like this:

```rust
match vm.next(None) {
    VmStep::Event(Event::Dialogue { speakers, lines, fields, .. }) => {
        // Render the dialogue in your game UI
        let speaker_name = speakers[0]["name"];
        for line in &lines {
            display_text(speaker_name, line);
        }
    }
    // ... handle other events
}
```

## Decorators on Dialogue

Decorators can enrich dialogue events with additional metadata. When a decorator is applied to a dialogue line, the decorator body can inject arbitrary key-value pairs into the event's `fields` map:

```urd
@camera_shake(0.5)
narrator: "The ground trembles beneath your feet!"
```

The host engine receives `fields["camera_shake"] = 0.5` alongside the dialogue text, allowing it to trigger visual effects, audio cues, or any other game-side behaviour.

See [Decorators](./decorators.md) for the full decorator system.

## Localization

Every dialogue node can carry a `loc_id` — a unique localization key derived from the file name, label name, and line position. When a `Localizer` is attached to the VM, it uses this key to look up translated text from Fluent `.ftl` files.

```urd
@fluent
global gold = 50

# This dialogue automatically gets a loc_id like "merchant-start-line_1"
narrator: "You have {gold} gold."
```

The `loc_id` is generated automatically when the script is compiled with a file stem context. Translators work with the generated `.ftl` files and never need to touch the `.urd` source.

You can also set the `loc_id` manually using the `@id("custom-key")` decorator on a dialogue line. This overrides the auto-generated key, which is useful when you need stable, human-readable localization identifiers:

```urd
@id("greeting-line")
narrator: "Hello, traveler!"
```

The above dialogue event will carry `loc_id: Some("greeting-line")` instead of the default positional key.

See [Localization](../localization/overview.md) for the full workflow.

## Dialogue vs. Menus

Dialogue is a one-way communication — the VM yields text and then continues to the next node. When you need the player to make a decision, use a `menu` block instead, which yields `Event::Choice`:

```urd
narrator: "A fork in the road lies ahead."

menu {
    "Go left" {
        jump left_path
    }
    "Go right" {
        jump right_path
    }
}
```

Dialogue events require no response from the host (pass `None` to `vm.next()`). Choice events require the host to pass back the selected option index.