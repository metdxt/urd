# Urd

Urd is a dialogue scripting language designed to be easily embeddable into any game engine.

It compiles `.urd` scripts into a directed graph that a lightweight VM walks one step at a time,
yielding events your engine decides how to render. Urd doesn't care about your UI, your renderer,
or your opinions — it just emits dialogue and choice events and gets out of the way.

## Quick taste

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const zara     = :{ name: "Zara",     name_color: "cyan"  }

global gold      = 0
global has_torch = false

@entry
label start {
    narrator: {
        "The wind howls across the barren moor."
        "Before you yawns the entrance to an ancient cave."
    }

    zara: "Halt, traveler. That cave has swallowed many who entered unprepared."

    menu {
        "Enter the cave" {
            jump cave_entrance
        }
        "Take a torch from Zara" {
            has_torch = true
            zara: "Here — take this torch. May it keep the shadows at bay."
            jump cave_entrance
        }
        "Walk away" {
            narrator: "The Forgotten Cave remains just that — forgotten."
            end!()
        }
    }
}
```

## Notable features

### Dialogue is a first-class construct

Speakers are values, not magic strings. Define a struct, give your characters names and colors
(or whatever metadata your engine needs), and write dialogue that reads like a screenplay:

```urd
struct Character {
    name: str
    name_color: str
}

const merchant: Character = :{ name: "Aldric", name_color: "#42c8f5" }

merchant: "Welcome, stranger! These are dark times."
```

Multi-line dialogue blocks keep things tidy:

```urd
narrator: {
    "In the age before the Sundering, three great powers held the world"
    "in uneasy balance: the Guild, the Empire, and the Rebels."
}
```

### Branching with menus

Player choices are expressed with `menu` blocks. Each option has a label and a body:

```urd
menu {
    "Ask about the Guild" {
        jump guild_lore
    }
    "Buy supplies and head out" {
        gold = gold - 10
        jump dungeon_entry
    }
    "Sleep on it" {
        narrator: "You wake refreshed. The crossroads awaits."
        jump choose_path
    }
}
```

### Labels and jumps

Labels are named sections of dialogue. `jump` transfers control. Think `goto`, but for conversations:

```urd
@entry
label start {
    narrator: "Welcome."
    jump chapter_one
}

label chapter_one {
    narrator: "It begins."
    end!()
}
```

Need subroutine-style calls? `jump ... and return` pushes a call frame:

```urd
let result = jump some_helper and return
```

### Conditionals and match

Standard `if`/`elif`/`else`:

```urd
if gold >= 60 {
    narrator: "The bards will sing of this."
} elif gold >= 10 {
    narrator: "Not empty-handed, and still breathing."
} else {
    narrator: "Survival is its own reward."
}
```

Pattern matching on enums, literals, ranges, and dice:

```urd
enum Faction { Guild, Empire, Rebel }

match player_faction {
    Faction.Guild  { hero: "I used to be one of them." }
    Faction.Empire { hero: "The Empire never trusted the Guild." }
    Faction.Rebel  { hero: "Nobody listened." }
}
```

### String interpolation

```urd
narrator: "You carry {gold} gold and {inventory_count} item(s)."
narrator: "Your {starting_dagger.name} is still sharp enough. Barely."
```

### Dice as first-class values

Roll dice, match on results:

```urd
let roll = 2d6

match roll {
    2..=4  { narrator: "Critical failure." }
    5..=9  { narrator: "Partial success." }
    10..=12 { narrator: "Critical hit!" }
}
```

### Multi-file imports

Split your story across files and import what you need:

```urd
import "village.urd" as village
import (Faction, narrator, hero) from "characters.urd"
import Item from "items.urd"
```

Circular imports work fine — the compiler handles it.

### Decorators

Attach custom metadata to dialogue events. Define them in-script or register them from your engine at runtime:

```urd
decorator slow<event: dialogue>(speed: float) {
    event.text_speed = speed
}

@slow(0.3)
narrator: "Time seemed to stop..."
```

The built-in `@entry` marks where execution begins. `@fluent` plugs variables into
[Project Fluent](https://projectfluent.org/) for localization.

### Localization

First-class Fluent integration. Tag globals with `@fluent`, generate `.ftl` scaffolds,
and let translators handle pluralization and grammatical case without touching your scripts:

```urd
@fluent
global gold = 50

narrator: "You have {gold} gold."
```

### Functions

Pure functions for when you need logic but not dialogue:

```urd
fn clamp(value: int, low: int, high: int) -> int {
    if value < low { return low }
    if value > high { return high }
    return value
}
```

### Frontend-agnostic VM

The VM is pull-based. Your engine drives the loop:

```rust
let mut vm = Vm::new(graph, registry)?;

loop {
    match vm.next(None) {
        VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
            // render however you want
        }
        VmStep::Event(Event::Choice { options, .. }) => {
            let picked = show_menu(&options);
            vm.next(Some(picked));
        }
        VmStep::Ended => break,
        VmStep::Error(e) => eprintln!("runtime error: {e}"),
    }
}
```

Urd doesn't draw anything. It doesn't know what a screen is. It yields structured events
and your engine does whatever it wants with them.

## Tooling

- **Tree-sitter grammar** — syntax highlighting and indentation for Neovim, Helix, and Zed
- **Language server** (`urd-lsp`) — diagnostics, hover, go-to-definition, rename, completion, and spellcheck
- **CLI runner** (`quest`) — run scripts in the terminal, export graphs, generate localization scaffolds
- **20+ static analysis passes** — from type checking and exhaustiveness to typo detection and infinite loop warnings

## License

MIT