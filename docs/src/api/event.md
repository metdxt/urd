# Event & ChoiceEvent

The `Event` enum is the primary output of the Urd virtual machine. Every call to `vm.next()` that produces observable content returns a `VmStep::Event(event)` where `event` is one of the two `Event` variants.

Both `Event` and `ChoiceEvent` derive `Serialize` and `Deserialize`, so you can pass them directly to your game engine's event bus, log them to disk, or send them over the network.

## `Event` Enum

```rust
pub enum Event {
    Dialogue { speakers, lines, fields, loc_id, fluent_vars, localized_text },
    Choice   { options, fields, loc_id, fluent_vars },
}
```

### `Event::Dialogue`

Emitted when one or more speakers deliver one or more lines of dialogue.

| Field | Type | Description |
|-------|------|-------------|
| `speakers` | `Vec<RuntimeValue>` | Evaluated speaker values — usually struct or map instances with fields like `name` and `name_color`. |
| `lines` | `Vec<RuntimeValue>` | The text content of the dialogue. Each element is typically a `RuntimeValue::Str`, but multi-line dialogue blocks produce multiple entries. |
| `fields` | `HashMap<String, RuntimeValue>` | Key-value pairs produced by decorators applied to this dialogue node. Empty if no decorators are present. |
| `loc_id` | `Option<String>` | The localization key for this dialogue, generated automatically from the file stem, label name, and node position. `None` if no file context was provided during compilation. |
| `fluent_vars` | `HashMap<String, RuntimeValue>` | Fluent variable bindings collected from the active scope when this event fired. Includes both `@fluent`-tagged variables and variables referenced via string interpolation. |
| `localized_text` | `Option<String>` | Pre-translated text populated when a `Localizer` is attached to the VM and `loc_id` is `Some`. `None` if no localizer is present or the key was not found. |

**Typical usage:**

```rust
match event {
    Event::Dialogue { speakers, lines, localized_text, .. } => {
        let speaker_name = speakers.first()
            .and_then(|s| match s {
                RuntimeValue::Map(m) => m.get("name").map(|v| v.to_string()),
                RuntimeValue::Struct { fields, .. } => fields.get("name").map(|v| v.to_string()),
                _ => Some(s.to_string()),
            })
            .unwrap_or_else(|| "???".to_string());

        // Prefer localized text if available, fall back to raw lines
        let text = localized_text.unwrap_or_else(|| {
            lines.iter().map(|l| l.to_string()).collect::<Vec<_>>().join("\n")
        });

        display_dialogue(&speaker_name, &text);
    }
    _ => {}
}
```

### `Event::Choice`

Emitted when the script reaches a `menu` block. The VM pauses and waits for the player to select an option. You must call `vm.next(Some(index))` with the chosen index to resume execution.

| Field | Type | Description |
|-------|------|-------------|
| `options` | `Vec<ChoiceEvent>` | The available choices. Each option has its own label, decorator fields, and localization data. |
| `fields` | `HashMap<String, RuntimeValue>` | Decorator output applied to the menu as a whole (not to individual options). |
| `loc_id` | `Option<String>` | Localization key for the entire choice event (the menu node). |
| `fluent_vars` | `HashMap<String, RuntimeValue>` | Fluent variable bindings for this choice event's scope context. |

**Typical usage:**

```rust
match event {
    Event::Choice { options, .. } => {
        for (i, opt) in options.iter().enumerate() {
            let label = opt.localized_label.as_deref()
                .unwrap_or(&opt.label);
            println!("  [{}] {}", i + 1, label);
        }

        let choice = get_player_input() - 1;  // 0-based index
        vm.next(Some(choice));
    }
    _ => {}
}
```

## `ChoiceEvent` Struct

Each element of `Event::Choice::options` is a `ChoiceEvent`:

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
| `label` | `String` | The display text for this option, as written in the script (e.g., `"Buy a health potion"`). String interpolation is already resolved. |
| `fields` | `HashMap<String, RuntimeValue>` | Decorator output for this specific option. For example, a `@color("red")` decorator on a menu option would place `"color" → "red"` here. |
| `loc_id` | `Option<String>` | Localization key for this individual option. The key is derived from the file stem, label name, menu index, and a slugified version of the option text. |
| `fluent_vars` | `HashMap<String, RuntimeValue>` | Fluent variables available for this option's localization. Includes `@fluent`-tagged globals and any variables interpolated in the option label. |
| `localized_label` | `Option<String>` | Pre-translated option text. Populated when a `Localizer` is attached. `None` otherwise. |

## Localization ID Format

Localization IDs are generated automatically during named compilation (`Compiler::compile_named`). The format is:

```text
<file_stem>-<label>-<node_description>
```

For example, the file `merchant.urd` with a label `browse` produces IDs like:

| Node | Generated ID |
|------|-------------|
| First dialogue in `browse` | `merchant-browse-line_1` |
| Second dialogue in `browse` | `merchant-browse-line_2` |
| First menu's option "Buy it" | `merchant-browse-menu_1-buy_it_for_price_gold` |
| Dialogue inside an `if` | `merchant-buy-if_1-line_1` |

These IDs appear in the generated `.ftl` files and are used by the `Localizer` trait to look up translations at runtime.

## Decorator Fields

The `fields` map on both `Dialogue` and `Choice` events is populated by the decorator system. When a decorator is applied to a dialogue or menu node, the decorator's handler function receives the decorator arguments and returns a `HashMap<String, RuntimeValue>`. This map is merged into the event's `fields`.

**Script-side:**

```urd
@color("red")
@mood("angry")
villain: "You dare challenge me?"
```

**Rust-side (registration):**

```rust
registry.register("color", |args| {
    let mut m = HashMap::new();
    if let Some(RuntimeValue::Str(s)) = args.first() {
        m.insert("color".into(), RuntimeValue::Str(s.clone()));
    }
    m
});
```

**Resulting event fields:**

```json
{
    "color": "red",
    "mood": "angry"
}
```

Your game engine reads these fields to control rendering — text color, animation triggers, sound effects, portrait expressions, or whatever your UI needs.

## Serialization

Both `Event` and `ChoiceEvent` derive `serde::Serialize` and `serde::Deserialize`. This means you can:

- **Serialize to JSON** for logging, debugging, or a web-based dialogue viewer.
- **Send over FFI** by serializing to a format your target language can parse.
- **Save/restore state** by persisting the last event alongside VM state.

> **Note:** The `speakers` and `lines` fields contain `RuntimeValue` instances. Most runtime values are serializable (`Null`, `Bool`, `Int`, `Float`, `Str`, `Dice`, `IdentPath`, `Range`, `List`). However, `Map`, `Function`, `ScriptDecorator`, and `Struct` variants are `#[serde(skip)]` and will be silently omitted during serialization. In practice, speaker values that are struct instances (the most common case) will not round-trip through serde — use the `fields` or extract the data you need before serializing.