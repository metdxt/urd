# Imports & Multi-File Projects

As your scripts grow, splitting them across multiple files keeps things
manageable. Urd's import system lets you share labels, functions, enums, structs,
constants, and speakers between files while maintaining clear namespacing.

---

## Aliased Import

The most common form imports an entire file under a namespace alias:

```urd
import "village.urd" as village
```

Everything exported by `village.urd` is now accessible through the `village`
prefix:

```urd
import "village.urd" as village

@entry
label start {
    narrator: "You arrive at the village."
    jump village.village_square
}
```

Cross-module label jumps, variable access, type references, and function calls
all use the same dot notation:

```urd
import "characters.urd" as chars
import "types.urd" as types

let faction: types.Faction = types.Faction.Guild
chars.narrator: "Welcome to the guild hall."
jump chars.guild_intro
```

The alias name is yours to choose — it does not need to match the file name.

---

## Named Import

When you only need specific items from a file, pull them directly into the local
scope:

```urd
import (Faction, narrator) from "characters.urd"
```

Now `Faction` and `narrator` are available without any prefix:

```urd
import (Faction, narrator) from "characters.urd"

const faction: Faction = Faction.Rebel

@entry
label start {
    narrator: "The rebellion begins."
}
```

You can import any combination of labels, functions, enums, structs, constants,
and globals in a single statement.

---

## Renamed Import

If an imported name collides with a local declaration (or you simply prefer a
different name), rename it with `as`:

```urd
import Item as InventoryItem from "items.urd"

const loot: InventoryItem = :{ name: "Sword", value: 10 }
```

Renaming works with both single and multi-item imports:

```urd
import (Item as InventoryItem, Weapon as WeaponType) from "items.urd"
```

---

## What Can Be Imported

Anything defined at the top level of a file is importable:

| Declaration | Aliased access | Named import |
|-------------|----------------|--------------|
| `label`     | `alias.label_name` | `label_name` |
| `fn`        | `alias.fn_name()` | `fn_name()` |
| `enum`      | `alias.EnumName.Variant` | `EnumName.Variant` |
| `struct`    | `alias.StructName` | `StructName` |
| `const`     | `alias.name` | `name` |
| `global`    | `alias.name` | `name` |
| `decorator` | `alias.decorator_name` | `decorator_name` |

---

## Cross-Module Label Jumps

One of the most important uses of imports is jumping to labels defined in other
files. This lets you organize a large game script into logical chapters or
locations:

```urd
# main.urd
import "tavern.urd" as tavern
import "forest.urd" as forest
import "castle.urd" as castle

@entry
label crossroads {
    narrator: "Three paths stretch before you."

    menu {
        "Head to the tavern" {
            jump tavern.enter_tavern
        }
        "Venture into the forest" {
            jump forest.forest_entrance
        }
        "Approach the castle" {
            jump castle.castle_gate
        }
    }
}
```

```urd
# tavern.urd
const barkeep = :{ name: "Barkeep", name_color: "brown" }

@entry
label enter_tavern {
    barkeep: "Welcome! What'll it be?"
}
```

Each file can have its own `@entry` for standalone testing, but when imported,
only the labels (and other declarations) are exposed — the importer's `@entry`
is what drives execution.

---

## Cross-Module Variable & Type Access

Imported constants, globals, and types are accessed through the alias just like
labels:

```urd
import "characters.urd" as chars

@entry
label start {
    chars.narrator: "You see a familiar face."
    
    if chars.reputation > 50 {
        chars.narrator: "Ah, a friend! Come in."
    }
}
```

---

## Circular Imports

Urd supports circular imports. If file A imports file B and file B imports file
A, the compiler handles the cycle gracefully — there is no need to restructure
your project to avoid circular dependencies:

```urd
# overworld.urd
import "dungeon.urd" as dungeon

label town {
    narrator: "The dungeon entrance looms nearby."
    jump dungeon.entrance
}
```

```urd
# dungeon.urd
import "overworld.urd" as overworld

label entrance {
    narrator: "You enter the dungeon."
    # ...
    narrator: "You retreat to town."
    jump overworld.town
}
```

This is intentional — game scripts naturally form cycles (go to dungeon → return
to town → go to dungeon again), and the import system does not get in the way.

---

## Import Resolution and File Loading

Under the hood, imports are resolved through the `FileLoader` trait. Urd ships
with two implementations:

### `FsLoader` — Filesystem Loading

The default loader resolves paths relative to the importing file's directory.
Given this project structure:

```text
project/
├── main.urd
├── tavern.urd
└── npcs/
    └── characters.urd
```

Imports resolve as you would expect:

```urd
# main.urd
import "tavern.urd" as tavern
import "npcs/characters.urd" as chars
```

> **Security:** `FsLoader` rejects path traversal (`..`) in import paths. An
> import like `import "../secret.urd" as s` will fail. This prevents scripts
> from accessing files outside the project directory.

### `MemLoader` — In-Memory Loading

For testing, embedding, or procedural generation, `MemLoader` resolves imports
from a `HashMap<String, String>` of file names to source code. No filesystem
access is needed:

```rust
use urd::prelude::*;

let mut loader = MemLoader::new();
loader.add("main.urd", r#"
    import "npcs.urd" as npcs
    @entry
    label start {
        npcs.narrator: "Hello!"
    }
"#);
loader.add("npcs.urd", r#"
    const narrator = :{ name: "Narrator", name_color: "white" }
"#);
```

Custom loaders can be implemented by providing the `FileLoader` trait — useful
for loading scripts from asset bundles, databases, or network sources.

---

## Best Practices

- **One responsibility per file.** Put all NPC definitions in `characters.urd`,
  quest logic in `quests/dragon.urd`, location scripts in `locations/tavern.urd`,
  and so on.

- **Use aliased imports for location files.** `import "tavern.urd" as tavern`
  reads naturally in cross-module jumps: `jump tavern.enter_tavern`.

- **Use named imports for types and speakers.** `import (Faction, narrator) from "types.urd"` avoids verbose prefixes in dialogue and type annotations.

- **Avoid deep nesting.** A flat or shallow directory structure is easier to
  navigate and produces shorter import paths.

- **Rename on conflict.** If two files export an `Item` type, rename one at the
  import site rather than refactoring the source file.

---

## Quick Reference

| Syntax | Effect |
|--------|--------|
| `import "file.urd" as alias` | Import everything under `alias` |
| `import (A, B) from "file.urd"` | Import `A` and `B` directly into scope |
| `import A as B from "file.urd"` | Import `A` renamed to `B` |
| `alias.label_name` | Access a cross-module label |
| `alias.EnumName.Variant` | Access a cross-module enum variant |
| `jump alias.label` | Jump to a label in another file |