# Speakers

In Urd, a **speaker** is the value on the left side of a dialogue statement. It identifies *who* is talking. Speakers are typically defined as `const` map literals at the top of a script, but any expression that resolves to a value can serve as a speaker.

## Defining Speakers as Maps

The most common pattern is a `const` binding to a map literal with `name` and `name_color` fields:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const zara     = :{ name: "Zara",     name_color: "cyan"  }
const elara    = :{ name: "Elara",    name_color: "yellow" }
```

When one of these constants is used as a speaker, the entire map value is included in the `Event::Dialogue` payload under the `speakers` field. The host engine can then read `name`, `name_color`, or any other fields you define to render the dialogue box.

### Custom Fields

You are not limited to `name` and `name_color`. Add whatever metadata your game engine needs:

```urd
const narrator = :{
    name: "Narrator",
    name_color: "white",
    portrait: "narrator_neutral.png",
    voice_bank: "narrator_calm"
}
```

The VM does not interpret these fields — it passes them through to the host verbatim.

## Typed Speakers with Structs

For stricter guarantees, define a `struct` type and annotate your speaker constants:

```urd
struct Character {
    name: str
    name_color: str
}

const narrator: Character = :{ name: "Narrator", name_color: "white" }
const zara: Character     = :{ name: "Zara",     name_color: "cyan"  }
```

With a struct annotation, the compiler will emit a `StructMismatch` diagnostic if any field is missing, misspelled, or has the wrong type. This catches typos like `name_colour` at compile time instead of at runtime in the game.

A more detailed struct:

```urd
struct Speaker {
    name: str
    name_color: str
    portrait: str
    voice_bank: str
}

const narrator: Speaker = :{
    name: "Narrator",
    name_color: "#a0a0b0",
    portrait: "portraits/narrator.png",
    voice_bank: "narrator_default"
}
```

## Using Speakers in Dialogue

Once defined, use the constant name on the left side of the `:` in a dialogue statement:

```urd
narrator: "The wind howls across the barren moor."

zara: "Halt, traveler."

elara: {
    "Welcome! I am Elara, purveyor of fine curiosities."
    "Might I interest you in a Health Potion today?"
}
```

The speaker expression is evaluated, and the resulting value is placed into the `speakers` Vec of the emitted `Event::Dialogue`.

## What the Host Receives

On the integration side, a dialogue event looks like this:

```rust
VmStep::Event(Event::Dialogue { speakers, lines, fields, .. }) => {
    // speakers: Vec<RuntimeValue>
    // Each speaker is typically a Map with "name", "name_color", etc.
    for speaker in &speakers {
        if let RuntimeValue::Map(map) = speaker {
            let name = map.get("name");       // Some(Str("Zara"))
            let color = map.get("name_color"); // Some(Str("cyan"))
            // ... render dialogue UI
        }
    }
}
```

## Multiple Speakers

Urd supports multiple speakers on a single dialogue line. Separate them with commas before the colon:

```urd
const alice = :{ name: "Alice", name_color: "blue" }
const bob   = :{ name: "Bob",   name_color: "green" }

alice, bob: "We agree on this!"
```

The emitted `Event::Dialogue` will have a `speakers` Vec with two entries. Your game engine can render this however makes sense — dual portraits, a combined name display, etc.

## Non-English Speakers

Urd is fully UTF-8, so speaker names can use any script. Here is an example from Russian:

```urd
const nar    = :{ name: "Рассказчик", name_color: "#a0a0b0" }
const yaga   = :{ name: "Баба-Яга",   name_color: "#9370DB" }
const leshiy = :{ name: "Леший",      name_color: "#228B22" }

nar: "Густой туман стелется по земле."

yaga: "Фу-фу-фу! Русским духом пахнет!"
```

The language server's spellcheck automatically detects the script language and loads the appropriate dictionary — no configuration needed.

## Speakers Are Just Expressions

While `const` bindings are the idiomatic way to define speakers, the left side of a dialogue statement can be any expression that resolves to a value. This means you could technically use a variable, a function call, or even a map literal inline:

```urd
# Inline map (works, but not recommended for readability)
:{ name: "Mystery Voice", name_color: "gray" }: "Who goes there?"

# Variable speaker
let current_speaker = narrator
current_speaker: "The story continues..."
```

In practice, stick to top-level `const` definitions. They are self-documenting, reusable across labels, and enable static analysis.