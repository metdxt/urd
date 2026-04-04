# Circular Imports

This walkthrough covers the `examples/circular/` project — a two-file script that demonstrates how Urd handles circular imports cleanly, enabling interconnected game areas that can jump freely between modules.

## File Structure

```
examples/circular/
├── main.urd      ← Village of Greyhollow (entry point)
└── tavern.urd    ← The Tarnished Flagon (tavern scenes)
```

The import relationship is circular:

```
main.urd  ──import "tavern.urd" as tavern──▶  tavern.urd
tavern.urd ──import "main.urd"  as main  ──▶  main.urd
```

`main.urd` needs to jump into `tavern.enter_tavern`. `tavern.urd` needs to jump back to `main.hub`. Neither file can be compiled without the other — a classic circular dependency.

## How to Run

```bash
cargo run --bin quest -- run examples/circular/main.urd
```

To visualise the cross-module control-flow graph:

```bash
cargo run --bin quest -- export -f mermaid examples/circular/main.urd
```

## The Story

The player arrives at the village of Greyhollow — a muddy crossroads with a tavern, a merchant, and a guard who is pretending not to watch you. The village hub lets you visit each location and return to the square. The tavern is a separate module with its own labels and state, but when the player leaves the tavern, control flow jumps back to the village hub in `main.urd`.

## main.urd — The Village Hub

The file starts with an import of the tavern module:

```urd
import "tavern.urd" as tavern
```

It defines shared types, characters, and global state:

```urd
struct Character {
    name: str
    name_color: str
}

const narrator: Character = :{ name: "Narrator", name_color: "#a0a0b0" }
const hero:     Character = :{ name: "Hero",      name_color: "#f5c542" }
const guard:    Character = :{ name: "Guard",     name_color: "#8888aa" }
const merchant: Character = :{ name: "Merchant",  name_color: "#42c8f5" }

global visited_tavern:   int = 0
global spoke_to_guard:   int = 0
global bought_supplies:  int = 0
```

The `hub` label is the central gameplay loop. Note the cross-module jump inside the "Go into the tavern" option:

```urd
label hub {
    if visited_tavern > 0 {
        narrator: {
            "You are back in the village square."
            "The Tarnished Flagon's sign creaks in the wind behind you."
        }
    } else {
        narrator: {
            "The village square is a muddy triangle of cobblestones."
            "Three things: a tavern with a broken sign, a merchant's cart,"
            "and a guard who is pretending not to watch you."
        }
    }

    menu {
        "Go into the tavern" {
            visited_tavern = visited_tavern + 1
            jump tavern.enter_tavern    # ← cross-module jump INTO tavern.urd
        }
        "Talk to the merchant" {
            jump speak_to_merchant
        }
        "Talk to the guard" {
            jump speak_to_guard
        }
        "Leave Greyhollow" {
            jump depart
        }
    }
}
```

The `jump tavern.enter_tavern` is the key line — it transfers control to a label defined in `tavern.urd`, using the module alias `tavern` established by the import.

## tavern.urd — The Tarnished Flagon

The tavern file imports `main.urd` right back:

```urd
import "main.urd" as main
import (narrator, hero, Character) from "main.urd"
```

There are two imports here:

1. **Whole-module import** (`as main`) — needed for `jump main.hub` to work.
2. **Named symbol import** — pulls `narrator`, `hero`, and `Character` directly so the tavern scenes can use them without a `main.` prefix. This avoids re-declaring constants that `main.urd` already registered.

The tavern defines its own local characters and state:

```urd
const barkeep: Character = :{ name: "Old Morryn",    name_color: "#c8934a" }
const patron:  Character = :{ name: "Gruff Patron",  name_color: "#7a9a7a" }

global heard_rumour: int = 0
global bought_drink: int = 0
```

The `enter_tavern` label is what `main.urd` jumps into:

```urd
@entry
label enter_tavern {
    narrator: {
        "The Tarnished Flagon is warm and low-ceilinged, smelling of"
        "tallow candles, old wood, and something fermented and forgotten."
    }

    barkeep: "Door. You're letting the cold in."
    hero: "Sorry."
    barkeep: "Sit down then. What'll it be?"

    menu {
        "Order a drink"                         { jump order_drink }
        "Ask if anyone's heard anything unusual" { jump ask_rumours }
        "Just warm up by the fire and leave"    {
            narrator: "You stand by the hearth for a moment. The warmth is almost enough."
            jump leave_tavern
        }
    }
}
```

The crucial `leave_tavern` label completes the circle:

```urd
label leave_tavern {
    narrator: "You pull your coat on and push back out into the cold."

    if heard_rumour > 0 {
        hero: "Riders. North. Right."
        narrator: "The information sits in your chest like a stone."
    }

    narrator: "The village square is quiet. Your breath fogs the air."

    jump main.hub    # ← cross-module jump BACK to main.urd
}
```

The `jump main.hub` sends the player back to the village square, where they can choose to visit the tavern again, talk to the merchant, or leave.

## The Jump Flow

```
main.start ─────────────▶ main.hub
                              │
                         (menu choice)
                         │    │    │    │
                    tavern  merchant guard depart
                         │
                         ▼
              tavern.enter_tavern
                         │
                    (menu choice)
                    │         │         │
              order_drink  ask_rumours  leave
                    │         │
                    └────┬────┘
                         ▼
              tavern.leave_tavern
                         │
                         ▼
                    main.hub  ◀──── back to the start
```

The player can loop between the village and the tavern as many times as they want. Each visit updates global state (`visited_tavern`, `heard_rumour`, `bought_drink`), and the dialogue adapts accordingly.

## How the Compiler Handles Cycles

The Urd compiler uses a **4-phase flat compilation** strategy:

1. **Phase 1 — Parse**: Both files are parsed into ASTs independently.
2. **Phase 2 — Scan labels**: The compiler scans all modules and pre-allocates `NodeIndex` stubs for every label. At this point, `tavern.enter_tavern` and `main.hub` both exist as stubs — even though neither module's IR has been emitted yet.
3. **Phase 3 — Resolve imports**: Import edges are recorded. When the compiler encounters `import "main.urd"` inside `tavern.urd` and sees that `main.urd` is already being compiled, it recognises the cycle and breaks it — the already-allocated stubs are sufficient.
4. **Phase 4 — Emit IR**: Both modules emit their IR. Every `jump` instruction resolves its target against the pre-allocated stubs. Because all stubs were created in Phase 2, both `jump tavern.enter_tavern` and `jump main.hub` resolve cleanly.

The key insight: **top-level Urd is definitions-only**. There is no executable code at the module level — only label definitions, type declarations, and constant bindings. This means the compiler never needs to "execute" one module before it can compile the other. All it needs are the label names, and those are available after Phase 2.

## Why Circular Imports Are Useful

In game dialogue, locations are inherently interconnected:

- A tavern scene needs to return to the village hub
- The village hub needs to enter the tavern
- A dungeon might lead back to the overworld, and the overworld leads to the dungeon
- Quest NPCs in different areas might reference each other's dialogue

Without circular import support, you would need to flatten everything into a single file (unmanageable for large projects) or introduce awkward indirection through a central routing module. Urd's approach lets you organise files by *location* or *scene* while maintaining free-form jumps between them.

## State Sharing

Both modules can read and write each other's globals. In this example, `main.urd` declares `visited_tavern` and increments it before jumping into the tavern. The tavern module doesn't need to touch that variable — but it could if it wanted to, because globals are shared across the entire compilation unit after merging.

The tavern's own globals (`heard_rumour`, `bought_drink`) are similarly accessible from `main.urd` if needed.

## Imported Symbols vs. Module Aliases

The tavern file demonstrates both import styles:

```urd
# Whole-module alias — needed for jump targets
import "main.urd" as main

# Named symbol imports — avoids re-declaring shared constants
import (narrator, hero, Character) from "main.urd"
```

With the named imports, the tavern can write `narrator:` and `hero:` directly instead of `main.narrator:` and `main.hero:`. This keeps the dialogue clean and readable while still making it clear (via the import statement) that these symbols come from another module.

> **Important:** Do not re-declare a `const` that was already registered by the imported module. The compiler will not allow two constants with the same name in the merged compilation unit. Use `import (name) from "file.urd"` instead.

## Key Takeaways

1. **Circular imports work out of the box.** No special syntax or configuration needed — just `import` both ways.
2. **The 4-phase compiler handles the cycle** by pre-allocating label stubs before emitting any IR.
3. **Cross-module jumps use the module alias** — `jump tavern.enter_tavern`, `jump main.hub`.
4. **Named symbol imports** let you use shared constants without a module prefix.
5. **Globals are shared** across the entire merged compilation unit.
6. **Organise by location**, not by dependency order. Each `.urd` file can represent a self-contained area of your game world.