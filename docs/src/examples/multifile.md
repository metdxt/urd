# Multi-file Project

This walkthrough covers the `examples/multifile/` project — a five-file Urd script demonstrating how to organize a dialogue-heavy game into separate modules with shared types, cross-module jumps, and global state.

## Project Structure

```
examples/multifile/
├── main.urd          ← entry point, story spine
├── characters.urd    ← shared Character struct, Faction enum, speaker constants
├── items.urd         ← Item struct, inventory globals, shop labels
├── village.urd       ← village scenes (square, market, tavern, elder)
└── dungeon.urd       ← dungeon scenes (entrance, combat, boss, epilogue)
```

The dependency graph looks like this:

```
         main.urd
        /    |    \
       /     |     \
characters  items  village ──→ characters, items
  .urd      .urd    .urd
       \     |     /
        \    |    /
        dungeon.urd ──→ characters, items
```

## Import Patterns

Urd supports three import styles, and this project uses all of them.

### Whole-module imports (aliased)

When you need to jump into many labels from another file, import the entire module under an alias:

```urd
import "village.urd" as village
import "dungeon.urd" as dungeon
```

This gives you access to every label and top-level symbol via the alias prefix — `village.village_square`, `dungeon.dungeon_entrance`, etc. It's the right choice when you're navigating into a module's control flow rather than using its data definitions.

### Named imports (specific symbols)

When you need specific types, constants, or globals by name — without a module prefix — pull them in directly:

```urd
import (Faction, narrator, hero, merchant, villain) from "characters.urd"
```

After this import, you can write `narrator:` and `Faction.Rebel` directly, without any qualifier. This is the preferred pattern for shared types and speaker constants.

### Renamed imports

You can also rename a symbol on import to avoid collisions or improve readability:

```urd
import Item as Item from "items.urd"
import gold          from "items.urd"
import inventory_count from "items.urd"
```

In this case `Item` keeps its original name, but the rename syntax (`import X as Y from "file"`) is available when you need it.

## Shared Types

### `characters.urd` — Character Struct and Faction Enum

This file defines the types that every other file uses:

```urd
struct Character {
    name: str
    name_color: str
}

enum Faction {
    Guild
    Empire
    Rebel
}
```

Speaker constants are defined here as typed `const` bindings:

```urd
const narrator:  Character = :{ name: "Narrator",           name_color: "#a0a0b0" }
const hero:      Character = :{ name: "Hero",               name_color: "#f5c542" }
const merchant:  Character = :{ name: "Aldric the Merchant", name_color: "#42c8f5" }
const villain:   Character = :{ name: "Malachar the Undying", name_color: "#c040c0" }
const innkeeper: Character = :{ name: "Berta Stonehearth",  name_color: "#e8a060" }
const guard:     Character = :{ name: "Captain Voss",       name_color: "#8888aa" }
```

Every other file imports the speakers it needs. This avoids duplicating character definitions and ensures a rename in `characters.urd` propagates everywhere.

### `items.urd` — Item Struct and Inventory State

This file owns the `Item` struct and all inventory-related globals:

```urd
struct Item {
    name: str
    description: str
    value: int
    weight: int
}

enum Rarity {
    Common
    Uncommon
    Rare
    Legendary
}

global gold: int = 100
global inventory_count: int = 0
global has_dungeon_key: int = 0
global potions_held: int = 1
global courage_taken: int = 0
```

It also defines item constants and helper labels like `show_inventory` and `buy_item`. The `show_inventory` label is marked `@entry` so it can be tested independently, and other files can `jump show_inventory` to display the inventory screen.

## Cross-Module Jumps

The main story spine lives in `main.urd`. When the player chooses to visit the village or enter the dungeon, `main.urd` jumps into the appropriate module:

```urd
label village_entry {
    narrator: "[ Entering Ashfen — see village.urd -> village_square ]"
    hero: "A proper village. Haven't seen one of these in months."
    jump village_scene
}
```

Within `village.urd`, the `village_farewell` label jumps to a label in `items.urd`:

```urd
label village_farewell {
    elder: "Return to us safe, traveller."
    narrator: "You shoulder your pack — {inventory_count} item(s) stored inside — and set off."
    jump show_inventory
}
```

This works because `show_inventory` was imported:

```urd
import (Item, gold, inventory_count, show_inventory) from "items.urd"
```

The key insight: **label references are resolved at compile time**. When you import a label, the compiler resolves it to a concrete node in the IR graph. `jump` follows a hard-wired edge baked in during compilation — no runtime lookup is needed.

## Cross-Module Variable Sharing

Globals declared in one file are accessible from any file that imports them. In this project, `gold` and `inventory_count` are declared in `items.urd` and imported by both `village.urd` and `dungeon.urd`.

The village market modifies the shared gold directly:

```urd
# In village.urd
import (Item, gold, inventory_count, show_inventory) from "items.urd"

label market {
    if gold >= 30 {
        gold = gold - 30
        inventory_count = inventory_count + 1
        merchant_vera: "Pleasure doing business."
        narrator: "You now have {gold} gold and {inventory_count} item(s) in your pack."
    }
}
```

The dungeon entrance also reads and modifies gold:

```urd
# In dungeon.urd
import (Item, gold, inventory_count, iron_sword) from "items.urd"

label dungeon_entrance {
    narrator: "You clutch your coin purse — {gold} gold pieces."

    menu {
        "Bribe the warden (costs 30 gold)" {
            if gold >= 30 {
                let new_gold: int = gold - 30
                gold = new_gold
                jump treasure_room
            }
        }
    }
}
```

Because globals are compiled into the shared VM environment, mutations in one module are immediately visible to all others. There is no need for explicit synchronization.

## Enum Usage Across Files

The `Faction` enum is defined in `characters.urd` and used for match expressions in multiple files. In `main.urd`:

```urd
import (Faction, narrator, hero, merchant, villain) from "characters.urd"

const player_faction: Faction = Faction.Rebel

label guild_lore {
    match player_faction {
        Faction.Guild {
            hero: "I... I used to be one of them. Before the purge."
        }
        Faction.Empire {
            hero: "The Empire never trusted the Guild anyway."
        }
        Faction.Rebel {
            hero: "The Rebels warned everyone this would happen."
        }
    }
    jump choose_path
}
```

In `dungeon.urd`, the same enum drives faction-based branching in the treasure room:

```urd
import (Character, Faction, narrator) from "characters.urd"

global ally_faction: Faction = Faction.Rebel

label treasure_room_choice {
    match ally_faction {
        Faction.Guild {
            ancient_spirit: "You carry the mark of the Guild."
        }
        Faction.Empire {
            ancient_spirit: "The Empire's reach extends even here?"
        }
        Faction.Rebel {
            ancient_spirit: "A Rebel. Good."
        }
    }
    jump boss_fight
}
```

Because all match arms cover every variant, the `NonExhaustiveMatch` lint stays quiet. If a fourth variant were added to `Faction` in `characters.urd`, every `match` across every file would immediately report an error — the compiler enforces exhaustiveness across module boundaries.

## Running the Example

```bash
quest run examples/multifile/main.urd
```

The entry point is the `start` label in `main.urd`, which is decorated with `@entry`. The compiler loads all imported files recursively, merges them into a single IR graph, and begins execution.

## LSP Features

This example is specifically designed to exercise cross-file LSP features:

- **Go-to-definition**: Hover over `Character` in `village.urd` and jump straight to its definition in `characters.urd`.
- **Cross-file completion**: After typing an imported module alias like `village.`, the LSP offers all labels and symbols from that module.
- **Find references**: Right-click `narrator` in `characters.urd` to see every file that uses it.
- **Rename**: Rename `Character` in `characters.urd` and watch the change propagate to every importing file.
- **Hover**: Hover over `gold` in any file to see its type (`int`), initial value (`100`), and definition location (`items.urd`).

## Best Practices for Multi-file Projects

1. **One file per domain.** Group related types, constants, and labels together. `characters.urd` owns character data, `items.urd` owns inventory, etc.

2. **Prefer named imports for types and constants.** `import (Character, Faction) from "characters.urd"` is clearer than `import "characters.urd" as chars` when you only need a few symbols.

3. **Use whole-module imports for navigation targets.** When you need to jump into many labels in another file, `import "village.urd" as village` keeps the jump targets explicit: `jump village.village_square`.

4. **Keep globals in the file that owns the concept.** `gold` lives in `items.urd` because it's inventory state. Other files import it rather than declaring their own copy.

5. **Define enums in one place.** Shared enums should live in a single file. The exhaustiveness checker works across modules — a new variant forces every `match` to be updated.

6. **Avoid circular imports when possible.** Urd handles them (see [Circular Imports](./circular-imports.md)), but linear dependency chains are easier to reason about. If `village.urd` and `dungeon.urd` both need to jump back to `main.urd`, consider using a bridge label in `main.urd` that each module jumps to directly.