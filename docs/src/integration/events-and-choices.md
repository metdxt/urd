# Events & Choices

The VM communicates with your game engine through two event types: **Dialogue** and **Choice**. Every call to `vm.next()` that doesn't return `VmStep::Ended` or `VmStep::Error` will yield one of these events. Understanding their structure is essential for rendering dialogue and handling player input.

## Event::Dialogue

A `Dialogue` event is emitted whenever the VM encounters a spoken line in the script.

```rust
Event::Dialogue {
    speakers: Vec<RuntimeValue>,
    lines: Vec<RuntimeValue>,
    fields: HashMap<String, RuntimeValue>,
    loc_id: Option<String>,
    fluent_vars: HashMap<String, RuntimeValue>,
    localized_text: Option<String>,
}
```

### Fields

| Field | Type | Description |
|-------|------|-------------|
| `speakers` | `Vec<RuntimeValue>` | The characters speaking this line. Usually a `Str` with the character name, but can be a `Map` or `Struct` carrying `name`, `name_color`, portrait info, etc. |
| `lines` | `Vec<RuntimeValue>` | The dialogue text lines. Typically `Str` values, but string interpolation may produce other types. |
| `fields` | `HashMap<String, RuntimeValue>` | Extra data injected by decorators (e.g. `@slow`, `@mood`). Empty if no decorators are attached. |
| `loc_id` | `Option<String>` | A localization key for this dialogue line. `None` if the script was compiled without a file stem (see [Compiling Scripts](./compiling.md)). |
| `fluent_vars` | `HashMap<String, RuntimeValue>` | Variable bindings collected from the current scope for Fluent localization. |
| `localized_text` | `Option<String>` | Pre-translated text if a `Localizer` is attached to the VM. `None` otherwise. |

### Rendering Dialogue

A typical dialogue handler extracts the speaker name and text:

```rust
VmStep::Event(Event::Dialogue { speakers, lines, localized_text, .. }) => {
    // Use localized text if available, otherwise the raw line
    let text = localized_text
        .or_else(|| lines.first().map(|l| l.to_string()))
        .unwrap_or_default();

    let speaker = speakers.first()
        .map(|s| s.to_string())
        .unwrap_or_else(|| "???".to_string());

    println!("{}: {}", speaker, text);
}
```

If your scripts use structured speakers (maps or structs with `name` and `name_color` fields), you'll want to destructure them:

```rust
let speaker_name = match speakers.first() {
    Some(RuntimeValue::Map(m)) => {
        m.get("name")
            .map(|v| v.to_string())
            .unwrap_or_else(|| "???".to_string())
    }
    Some(RuntimeValue::Str(s)) => s.clone(),
    _ => "???".to_string(),
};
```

### Decorator Fields

Decorators attached to dialogue lines populate the `fields` map. For example, if your script has:

```urd
@slow(0.5)
alice: "I have something important to tell you..."
```

And you've registered a `slow` decorator in Rust (see [Decorator Registry](./decorator-registry.md)), the `fields` map will contain the data your handler returned — e.g. `{"speed": Float(0.5)}`.

Your renderer can then use these fields to adjust text display speed, trigger animations, play sound effects, or anything else your engine supports.

## Event::Choice

A `Choice` event is emitted when the VM encounters a `menu` block. It pauses execution and waits for the player to select an option.

```rust
Event::Choice {
    options: Vec<ChoiceEvent>,
    fields: HashMap<String, RuntimeValue>,
    loc_id: Option<String>,
    fluent_vars: HashMap<String, RuntimeValue>,
    has_default: bool,
}
```

### Fields

| Field | Type | Description |
|-------|------|-------------|
| `options` | `Vec<ChoiceEvent>` | The available choices the player can pick from. Default (`_`) options are **excluded**. |
| `fields` | `HashMap<String, RuntimeValue>` | Decorator fields applied to the menu as a whole. |
| `loc_id` | `Option<String>` | Localization key for the entire menu node. |
| `fluent_vars` | `HashMap<String, RuntimeValue>` | Fluent variable bindings for the menu scope. |
| `has_default` | `bool` | `true` if the menu has a `_ { ... }` fallback option. |

### ChoiceEvent

Each option in the `options` vector is a `ChoiceEvent`:

```rust
pub struct ChoiceEvent {
    pub label: String,
    pub fields: HashMap<String, RuntimeValue>,
    pub loc_id: Option<String>,
    pub fluent_vars: HashMap<String, RuntimeValue>,
    pub localized_label: Option<String>,
}
```

| Field | Type | Description |
|-------|------|-------------|
| `label` | `String` | The display text for this choice. |
| `fields` | `HashMap<String, RuntimeValue>` | Decorator fields specific to this option. |
| `loc_id` | `Option<String>` | Localization key for this individual option. |
| `fluent_vars` | `HashMap<String, RuntimeValue>` | Fluent variable bindings for this option. |
| `localized_label` | `Option<String>` | Pre-translated label if a `Localizer` is attached. |

## Responding to Choices

When you receive an `Event::Choice`, you must present the options to the player, collect their selection, and pass it back to the VM on the **next** call to `vm.next()`.

The choice index is **0-based**.

```rust
VmStep::Event(Event::Choice { options, .. }) => {
    // Display options to the player
    for (i, option) in options.iter().enumerate() {
        let text = option.localized_label.as_deref()
            .unwrap_or(&option.label);
        println!("  [{}] {}", i + 1, text);
    }

    // Get the player's selection (your input method here)
    let selection: usize = get_player_input();

    // Pass the 0-based index on the next call
    // (if player sees "1, 2, 3", subtract 1)
    choice = Some(selection - 1);
}
```

Then on the next iteration:

```rust
let step = vm.next(choice.take());
```

### Default / Fallback Choices

When a menu includes a `_ { ... }` wildcard option in the script, the
`Event::Choice` will have `has_default: true`. This tells the host that a
fallback path exists — typically used for timer-based menus where the player
can run out of time to choose.

**Key points:**

- The default option is **never** included in the `options` vector. The host
  never sees its label because it has none.
- Choice indices `0..N` map only to the non-default options — the index space
  is unaffected by the presence of a `_` arm.
- To trigger the default arm, the host calls `vm.next(None)` while the choice
  is pending. Instead of re-emitting the choice event (the normal behaviour for
  `None` on a pending choice), the VM follows the `_ { ... }` branch.

**Typical flow:**

1. Call `vm.next(None)` → receive `Event::Choice { has_default: true, .. }`.
2. Display the options to the player.
3. Start a timer (duration comes from a decorator field, e.g. `@timed(10.0)`).
4. If the player picks before the timer expires → `vm.next(Some(index))`.
5. If the timer expires first → `vm.next(None)` → VM enters the `_` branch.

```rust
VmStep::Event(Event::Choice { options, has_default, fields, .. }) => {
    // Show options to the player
    display_menu(&options);

    // If the menu has a fallback, start a countdown
    if has_default {
        let duration = fields.get("timeout")
            .and_then(|v| match v {
                RuntimeValue::Float(f) => Some(*f),
                _ => None,
            })
            .unwrap_or(10.0);
        start_timer(duration);
    }

    // Later, when input arrives or the timer fires:
    if timer_expired {
        // Trigger the _ { ... } fallback branch
        vm.next(None)
    } else {
        // Player made a selection
        vm.next(Some(player_choice))
    }
}
```

> **Note:** If `has_default` is `false` and you call `vm.next(None)`, the VM
> re-emits the same `Event::Choice` — the existing behaviour is unchanged.

### Invalid Choice Index

If you pass an index that is out of bounds, the VM returns a `VmStep::Error`:

```rust
VmStep::Error(VmError::ChoiceOutOfBounds { index, len })
```

Where `index` is the value you supplied and `len` is the number of available options. Always validate the index before passing it, or handle this error gracefully.

## The Complete Event Loop

Here's a full event loop that handles both dialogue and choices:

```rust
use urd::prelude::*;

fn run_dialogue(mut vm: Vm) -> Result<(), VmError> {
    let mut choice: Option<usize> = None;

    loop {
        match vm.next(choice.take()) {
            VmStep::Event(Event::Dialogue { speakers, lines, fields, localized_text, .. }) => {
                let speaker = speakers.first()
                    .map(|s| s.to_string())
                    .unwrap_or_default();

                let text = localized_text
                    .or_else(|| lines.first().map(|l| l.to_string()))
                    .unwrap_or_default();

                // Check for decorator-injected fields
                let speed = fields.get("speed")
                    .and_then(|v| match v {
                        RuntimeValue::Float(f) => Some(*f),
                        _ => None,
                    })
                    .unwrap_or(1.0);

                render_dialogue(&speaker, &text, speed);
            }

            VmStep::Event(Event::Choice { options, .. }) => {
                let labels: Vec<&str> = options.iter()
                    .map(|o| o.localized_label.as_deref().unwrap_or(&o.label))
                    .collect();

                let selected = show_choice_menu(&labels);
                choice = Some(selected);
            }

            VmStep::Ended => {
                println!("--- End of dialogue ---");
                break;
            }

            VmStep::Error(e) => {
                return Err(e);
            }
        }
    }

    Ok(())
}
```

## Tips

- **Don't skip events.** Every `Event::Choice` must be answered before the VM can continue. If you call `vm.next(None)` when a choice is pending and the menu has no default option, the VM will re-emit the same `Choice` event. If the menu *does* have a default, `vm.next(None)` triggers the fallback branch instead — see [Default / Fallback Choices](#default--fallback-choices) above.
- **Multiple speakers.** A dialogue line can have multiple speakers (e.g. `<alice, bob>: "Together!"`). Always check the full `speakers` vector if your game supports group dialogue.
- **Empty lines.** The `lines` vector is never empty for a `Dialogue` event, but a line's string content could be empty if the script contains `alice: ""`.
- **Decorator fields are per-event.** Each `Dialogue` and each `ChoiceEvent` carries its own `fields` map. A decorator on the menu block affects `Event::Choice.fields`, while a decorator on an individual option affects that `ChoiceEvent.fields`.