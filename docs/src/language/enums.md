# Enums

Enums define named sets of variants — a fixed vocabulary of values that a
variable can hold. They are ideal for representing factions, quest states, item
categories, and any other domain where you need a closed set of named options.

---

## Defining an Enum

Use the `enum` keyword followed by a name and a list of variants:

```urd
enum Faction {
    Guild
    Empire
    Rebel
}
```

Each variant is a simple identifier. Enums do not carry associated data — they
are purely symbolic constants.

---

## Accessing Variants

Variants are accessed with dot notation on the enum name:

```urd
let allegiance = Faction.Guild
```

The fully qualified form `EnumName.Variant` is always required — you cannot use
a bare variant name like `Guild` without the enum prefix.

---

## Using Enums in Declarations

Enums pair naturally with type annotations. Annotating a variable with an enum
type ensures the compiler can verify correctness:

```urd
const faction: Faction = Faction.Rebel
global quest_state: QuestState = QuestState.NotStarted
let target: Faction = Faction.Empire
```

If you assign a value that doesn't match the declared enum type, the compiler
emits a `TypeMismatch` diagnostic.

---

## Enums in Conditionals

You can compare enum values directly with `==` and `!=`:

```urd
if faction == Faction.Guild {
    narrator: "The Guild recognizes your loyalty."
} elif faction == Faction.Rebel {
    narrator: "You fight for freedom."
} else {
    narrator: "The Empire demands your obedience."
}
```

While this works, `match` is usually the better choice for enum branching — see
below.

---

## Pattern Matching with Enums

The `match` statement is the idiomatic way to branch on an enum value. Urd
performs **exhaustiveness checking** on enum matches, ensuring every variant is
handled:

```urd
enum QuestState {
    NotStarted
    InProgress
    Complete
    Failed
}

match quest_state {
    QuestState.NotStarted {
        narrator: "You haven't begun this quest yet."
    }
    QuestState.InProgress {
        narrator: "The quest is underway."
    }
    QuestState.Complete {
        narrator: "Well done — quest complete!"
    }
    QuestState.Failed {
        narrator: "The quest has failed."
    }
}
```

### Exhaustiveness Checking

If you forget a variant, the compiler emits a `NonExhaustiveMatch` diagnostic:

```urd
# ✗ NonExhaustiveMatch: missing variant 'Failed'
match quest_state {
    QuestState.NotStarted { narrator: "Not started." }
    QuestState.InProgress { narrator: "In progress." }
    QuestState.Complete   { narrator: "Complete." }
}
```

This is a compile-time error — your script won't build until every variant is
accounted for.

### Using a Wildcard

If you don't need to handle every variant individually, a wildcard `_` arm
catches all remaining cases:

```urd
match faction {
    Faction.Guild {
        narrator: "Welcome, Guild member."
    }
    _ {
        narrator: "You are not part of the Guild."
    }
}
```

The wildcard satisfies exhaustiveness — the compiler considers all variants
covered.

---

## Enums Across Files

Enums defined in one file can be imported and used in another. Both import
styles work:

### Aliased Import

```urd
# items.urd
import "types.urd" as types

let faction: types.Faction = types.Faction.Guild
```

### Named Import

```urd
# items.urd
import (Faction) from "types.urd"

let faction: Faction = Faction.Guild
```

Named imports bring the enum into the local scope so you can use it without a
prefix. See [Imports](./imports.md) for the full import system.

---

## Practical Example

A complete example showing enums used for quest tracking:

```urd
enum QuestState {
    NotStarted
    InProgress
    Complete
    Failed
}

const narrator = :{ name: "Narrator", name_color: "white" }
const quest_giver = :{ name: "Elder Mira", name_color: "gold" }

global dragon_quest: QuestState = QuestState.NotStarted

@entry
label village {
    match dragon_quest {
        QuestState.NotStarted {
            quest_giver: "A dragon terrorizes our village. Will you help?"
            menu {
                "Accept the quest" {
                    dragon_quest = QuestState.InProgress
                    narrator: "You steel yourself for the journey."
                    jump dragon_lair
                }
                "Decline" {
                    narrator: "Perhaps another day."
                    end!()
                }
            }
        }
        QuestState.InProgress {
            quest_giver: "Have you slain the beast yet?"
            jump dragon_lair
        }
        QuestState.Complete {
            quest_giver: "You saved us all. Thank you, hero!"
            end!()
        }
        QuestState.Failed {
            narrator: "The village lies in ruins."
            end!()
        }
    }
}
```

---

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Define an enum | `enum Name { Variant1, Variant2 }` |
| Access a variant | `Name.Variant` |
| Type annotation | `let x: Name = Name.Variant` |
| Match on enum | `match x { Name.A { ... } Name.B { ... } }` |
| Wildcard arm | `_ { ... }` |
| Import an enum | `import (Name) from "file.urd"` |