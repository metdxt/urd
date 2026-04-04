# Structs

Structs define record types with named, typed fields. They give structure to the
maps that Urd scripts use constantly — speaker definitions, inventory items,
quest data, and anything else that benefits from a known shape.

---

## Defining a Struct

Use the `struct` keyword followed by a name and a block of field declarations:

```urd
struct Character {
    name: str
    name_color: str
}
```

Each field has a name and a type annotation separated by a colon. Fields are
listed one per line with no commas or separators.

```urd
struct Item {
    name: str
    value: int
}

struct QuestEntry {
    title: str
    description: str
    reward: int
    completed: bool
}
```

Struct definitions are top-level declarations — they cannot be nested inside
labels or functions.

---

## Creating Struct Instances

Struct instances are created by assigning a map literal to a variable with a
struct type annotation:

```urd
const hero: Character = :{ name: "Hero", name_color: "gold" }
const rusty_sword: Item = :{ name: "Rusty Sword", value: 5 }
```

The `:{` syntax is Urd's map literal (colon + left brace). The type annotation
(`: Character`, `: Item`) tells the compiler to validate the map's shape against
the struct definition.

You can also use `let` or `global` with struct types:

```urd
let current_speaker: Character = :{ name: "Narrator", name_color: "white" }
global active_quest: QuestEntry = :{
    title: "The Lost Amulet"
    description: "Find the ancient amulet in the cave."
    reward: 100
    completed: false
}
```

---

## Field Access

Access struct fields with dot notation:

```urd
const zara: Character = :{ name: "Zara", name_color: "cyan" }

narrator: "{zara.name} steps forward."
# Outputs: "Zara steps forward."
```

Field access works anywhere an expression is expected — in string interpolation,
conditions, function arguments, and assignments:

```urd
if active_quest.completed {
    narrator: "You already finished '{active_quest.title}'."
}

let reward_text = "Reward: {active_quest.reward} gold"
```

---

## Speakers as Structs

The most common use of structs in Urd is defining speaker values. A speaker is
any value placed on the left side of a dialogue colon — and giving it a struct
type ensures every speaker has the fields your engine expects:

```urd
struct Speaker {
    name: str
    name_color: str
}

const narrator: Speaker = :{ name: "Narrator", name_color: "white" }
const zara: Speaker     = :{ name: "Zara",     name_color: "cyan"  }
const merchant: Speaker = :{ name: "Merchant",  name_color: "green" }

label market {
    narrator: "You approach the market stall."
    merchant: "Fine wares for fine folk!"
    zara: "Don't trust him."
}
```

Without the `Speaker` struct, a typo like `name_colour` (British spelling) would
silently produce a map with an unexpected key. With the struct, the compiler
catches it immediately.

---

## The `StructMismatch` Diagnostic

When a map literal is assigned to a struct-typed variable, the compiler checks
that:

1. Every field declared in the struct is present in the map.
2. No extra fields exist that the struct does not declare.
3. Each field's value matches the declared type.

Violations produce a `StructMismatch` error at compile time.

### Missing Field

```urd
struct Item {
    name: str
    value: int
}

# ✗ StructMismatch: missing field 'value' for struct Item
let loot: Item = :{ name: "Sword" }
```

### Extra Field

```urd
# ✗ StructMismatch: unexpected field 'rarity' for struct Item
let loot: Item = :{ name: "Sword", value: 10, rarity: "rare" }
```

### Wrong Type

```urd
# ✗ StructMismatch: field 'value' expects int, got str
let loot: Item = :{ name: "Sword", value: "ten" }
```

These checks run during static analysis, so you catch shape errors before the
script ever reaches the VM.

---

## Structs Across Files

Structs can be imported from other files and used as types in the importing
script:

```urd
# speakers.urd
struct Speaker {
    name: str
    name_color: str
}

const narrator: Speaker = :{ name: "Narrator", name_color: "white" }
```

```urd
# main.urd
import (Speaker, narrator) from "speakers.urd"

const guard: Speaker = :{ name: "Guard", name_color: "red" }

@entry
label start {
    narrator: "You approach the city gate."
    guard: "Halt! State your business."
}
```

See [Imports](./imports.md) for the full import system.

---

## Structs vs. Plain Maps

You can always use plain maps without struct types — Urd does not force you into
structured data. But structs add value in several ways:

| Feature                    | Plain Map | Struct-Typed Map |
|----------------------------|-----------|------------------|
| Compile-time field checks  | No        | Yes              |
| Missing field detection    | No        | Yes              |
| Extra field detection      | No        | Yes              |
| Field type validation      | No        | Yes              |
| LSP autocompletion         | Limited   | Full             |
| Self-documenting           | Somewhat  | Yes              |

For throwaway data or dynamic maps, plain `:{...}` literals work fine. For
anything that appears repeatedly or crosses file boundaries — speakers, items,
quests, config — define a struct.

---

## Quick Reference

```urd
# Define a struct
struct Character {
    name: str
    name_color: str
}

# Instantiate with a typed map literal
const hero: Character = :{ name: "Hero", name_color: "gold" }

# Access fields
narrator: "{hero.name} draws their weapon."

# Compiler catches shape errors
# let bad: Character = :{ name: "Oops" }
# ✗ StructMismatch: missing field 'name_color'
```
