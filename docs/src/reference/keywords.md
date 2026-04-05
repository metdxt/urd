# Keyword Reference

This page lists every reserved keyword in the Urd language, its purpose, and a brief usage example.

## Bindings & Declarations

| Keyword | Purpose | Example |
|---------|---------|---------|
| `const` | Immutable binding | `const MAX_HP: int = 100` |
| `let` | Mutable local variable | `let gold = 100` |
| `global` | Mutable persistent variable | `global health = 100` |
| `extern` | Host-provided value declaration | `extern player_name: str` |

### `const`

Immutable binding — reassignment is a compile-time error. See [Constants](../language/constants.md).

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
```

### `let`

Mutable local variable — scoped to the enclosing block or label. See [Variables & Types](../language/variables-and-types.md).

```urd
let total = 0
total = total + 30
```

### `global`

Mutable variable that persists across label jumps. See [Globals](../language/globals.md).

```urd
global gold = 50
```

### `extern`

Value provided by the host runtime — not defined in the script. See [Extern Values](../language/extern-values.md).

```urd
extern player_name: str
```

---

## Control Flow

| Keyword | Purpose | Example |
|---------|---------|---------|
| `if` | Conditional branch | `if gold >= 30 { ... }` |
| `elif` | Else-if branch | `elif gold >= 10 { ... }` |
| `else` | Default branch | `else { ... }` |
| `match` | Pattern matching on enums, dice, and values | `match faction { ... }` |
| `jump` | Transfer control to another label | `jump tavern` |
| `return` | Return a value from a label or function | `return` |

### `if` / `elif` / `else`

Standard conditional branching. Conditions must evaluate to a boolean.

```urd
if gold >= 60 {
    narrator: "A fortune!"
} elif gold >= 10 {
    narrator: "Not bad."
} else {
    narrator: "Empty pockets."
}
```

### `match`

Pattern matching over enum variants, dice results, ranges, and literal values.

```urd
match faction {
    Faction.Guild  { narrator: "Guild member." }
    Faction.Empire { narrator: "Imperial." }
    Faction.Rebel  { narrator: "Rebel." }
}
```

The compiler enforces exhaustiveness — all enum variants must be covered, or a wildcard (`_`) arm must be present.

### `jump`

Transfers control to a named label. Optionally combined with `and return` for subroutine-style calls.

```urd
jump cave_entrance
jump tavern.enter_tavern       # cross-module jump
jump show_inventory and return  # subroutine call
```

### `return`

Returns from the current label or function. In labels invoked via `jump ... and return`, this sends control back to the call site. In functions, it produces the return value.

```urd
label helper {
    narrator: "This is a subroutine."
    return
}

fn double(x: int) {
    return x * 2
}
```

---

## Definitions

| Keyword | Purpose | Example |
|---------|---------|---------|
| `label` | Named block definition — the basic unit of dialogue flow | `label tavern { ... }` |
| `menu` | Choice menu — presents options to the player | `menu { "Option" { ... } }` |
| `fn` | Function definition | `fn double(x: int) { x * 2 }` |
| `enum` | Enumeration type | `enum Faction { Guild, Empire, Rebel }` |
| `struct` | Record type with named, typed fields | `struct Character { name: str }` |
| `decorator` | Decorator definition (script-defined) | `decorator mood<event: dialogue>(m) { ... }` |

### `label`

Labels are the fundamental building blocks of Urd scripts. Every piece of executable code lives inside a label.

```urd
@entry
label start {
    narrator: "Hello, world!"
    end!()
}
```

### `menu`

Menus present the player with a set of choices. Each option has a string label and a block of code that executes when selected.

```urd
menu {
    "Enter the cave" {
        jump cave_entrance
    }
    "Walk away" {
        jump road_end
    }
}
```

### `fn`

Functions are pure — they run in an isolated environment with only their parameters. No access to outer scope.

```urd
fn clamp(value: int, lo: int, hi: int) {
    if value < lo { return lo }
    if value > hi { return hi }
    return value
}
```

### `enum`

Enumerations define a closed set of named variants.

```urd
enum Status {
    Free
    Arrested
    Detained
}
```

### `struct`

Structs define record types with named, typed fields. Instances are created with map literal syntax.

```urd
struct Character {
    name: str
    name_color: str
}

const hero: Character = :{ name: "Hero", name_color: "#f5c542" }
```

### `decorator`

Script-defined decorators add metadata to dialogue and choice events. They can optionally constrain which event type they apply to.

```urd
decorator mood<event: dialogue>(m) {
    :{ mood: m }
}

@mood("angry")
villain: "You dare challenge me?"
```

---

## Imports

| Keyword | Purpose | Example |
|---------|---------|---------|
| `import` | File or symbol import | `import "village.urd" as village` |
| `from` | Import source specifier | `import (Faction) from "characters.urd"` |
| `as` | Import alias | `import "items.urd" as inv` |

### Import Styles

**Whole-module import:**

```urd
import "village.urd" as village
# Use: jump village.village_square
```

**Named symbol import:**

```urd
import (Character, Faction, narrator) from "characters.urd"
# Use: narrator: "Hello."
```

**Renamed symbol import:**

```urd
import Item as GameItem from "items.urd"
# Use: const sword: GameItem = :{ ... }
```

---

## Operators (keyword form)

| Keyword | Purpose | Example |
|---------|---------|---------|
| `and` | Logical AND (short-circuiting) | `if alive and healthy { ... }` |
| `or` | Logical OR (short-circuiting) | `if has_key or has_lockpick { ... }` |
| `not` | Logical NOT (unary) | `if not game_over { ... }` |
| `in` | Range membership test (`Int in Range`) | `if 5 in 1..10 { ... }` |

The `and`, `or`, and `not` operators are equivalent to their symbolic counterparts (`&&`, `||`, `!`) but read more naturally in dialogue-heavy scripts.

> **Note:** The `in` operator **only** works with ranges (`Int in Range`). It does not support lists, maps, or strings.

---

## Patterns

| Keyword | Purpose | Example |
|---------|---------|---------|
| `_` | Wildcard pattern — matches anything in `match` arms; default/fallback option in `menu` blocks | `_ { narrator: "Default." }` |

The wildcard is used in `match` arms as a catch-all:

```urd
match status {
    Status.Free { narrator: "Free." }
    _           { narrator: "Not free." }
}
```

It is also used in `menu` blocks as a default/fallback option. The default
option is **not** shown to the player — it executes when the host passes `None`
to a pending choice (e.g., when a timer expires):

```urd
@timed(10.0)
menu {
    "Fight" { jump combat }
    "Flee"  { jump escape }
    _ {
        narrator: "You hesitate too long..."
        jump ambush
    }
}
```

A menu may have at most one `_` option. Multiple wildcards produce a
`MultipleMenuDefaults` compile-time error.

---

## Literals

| Keyword | Purpose | Example |
|---------|---------|---------|
| `true` | Boolean literal (true) | `let alive = true` |
| `false` | Boolean literal (false) | `let game_over = false` |
| `null` | Null literal — absence of value | `let result = null` |

---

## Terminators

| Keyword | Purpose | Example |
|---------|---------|---------|
| `end!` | Script terminator — ends execution | `end!()` |
| `todo!` | Placeholder terminator — marks unfinished code | `todo!()` |

### `end!()`

Terminates script execution. The VM returns `VmStep::Ended` on the next call to `vm.next()`.

```urd
label finale {
    narrator: "The end."
    end!()
}
```

### `todo!()`

Marks a label as intentionally incomplete. The VM treats it the same as `end!()` at runtime, but the static analysis pass treats it differently — a `todo!()` satisfies the `DeadEnd` check without implying the label is finished.

```urd
label future_content {
    todo!()
}
```

---

## Complete Table

| Keyword | Category | Purpose |
|---------|----------|---------|
| `const` | Binding | Immutable binding |
| `let` | Binding | Mutable local variable |
| `global` | Binding | Mutable persistent variable |
| `extern` | Binding | Host-provided value declaration |
| `fn` | Definition | Function definition |
| `if` | Control flow | Conditional branch |
| `elif` | Control flow | Else-if branch |
| `else` | Control flow | Default branch |
| `label` | Definition | Named block definition |
| `menu` | Definition | Choice menu |
| `return` | Control flow | Return value from label/function |
| `jump` | Control flow | Transfer control |
| `enum` | Definition | Enumeration type |
| `struct` | Definition | Record type |
| `match` | Control flow | Pattern matching |
| `decorator` | Definition | Decorator definition |
| `import` | Import | File/symbol import |
| `from` | Import | Import source |
| `as` | Import | Import alias |
| `in` | Operator | Membership/iteration |
| `_` | Pattern | Wildcard pattern (match arms & menu default) |
| `and` | Operator | Logical AND |
| `or` | Operator | Logical OR |
| `not` | Operator | Logical NOT |
| `true` | Literal | Boolean literal |
| `false` | Literal | Boolean literal |
| `null` | Literal | Null literal |
| `end!` | Terminator | Script terminator |
| `todo!` | Terminator | Placeholder terminator |