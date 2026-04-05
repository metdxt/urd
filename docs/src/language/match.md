# Match

The `match` construct lets you branch on a single value against multiple
patterns — enum variants, literal values, numeric ranges, dice results, and
wildcards. It is the workhorse for RPG-style mechanics and any logic that goes
beyond simple boolean conditions.

---

## Basic Syntax

```urd
match expression {
    pattern { body }
    pattern { body }
    _ { body }
}
```

Each arm is a **pattern** followed by a **block** in curly braces — there is no
`=>` arrow. The `expression` is evaluated once, then each arm is tested
top-to-bottom. The first arm whose pattern matches executes its body; the
remaining arms are skipped.

---

## Matching on Literals

Match against specific `int`, `str`, or `bool` values:

```urd
match chosen_class {
    "warrior" {
        narrator: "You grip your sword tightly."
    }
    "mage" {
        narrator: "Arcane energy crackles at your fingertips."
    }
    "rogue" {
        narrator: "You melt into the shadows."
    }
    _ {
        narrator: "An unusual choice."
    }
}
```

```urd
match level {
    1 { narrator: "You are a novice." }
    2 { narrator: "You are an apprentice." }
    3 { narrator: "You are a journeyman." }
    _ { narrator: "You are a master." }
}
```

```urd
match is_alive {
    true  { narrator: "The creature stirs." }
    false { narrator: "Nothing but silence." }
}
```

---

## Matching on Enum Variants

Enums are one of the most common match targets. Access variants with the
`Enum.Variant` syntax:

```urd
enum Faction {
    Guild
    Empire
    Rebel
}

match player_faction {
    Faction.Guild  { narrator: "The Guild welcomes you." }
    Faction.Empire { narrator: "For the Empire!" }
    Faction.Rebel  { narrator: "Freedom or death." }
}
```

This is a real pattern used throughout Urd projects. For example, from a
multi-file dungeon crawler:

```urd
match ally_faction {
    Faction.Guild {
        ancient_spirit: "You carry the mark of the Guild. Pragmatic folk. Useful in a fight."
        narrator: "The spirit gestures — and a hidden cache of gold coins slides from the wall."
    }
    Faction.Empire {
        ancient_spirit: "The Empire's reach extends even here."
        narrator: "A sealed imperial scroll materialises on the altar."
    }
    Faction.Rebel {
        ancient_spirit: "A rebel spirit. Bold, if nothing else."
        narrator: "A flickering torch reveals a hidden passage behind the wall."
    }
}
```

### Exhaustiveness Checking

When matching on an enum, the compiler verifies that every variant is covered.
If you forget a variant and there is no wildcard `_` arm, a
**`NonExhaustiveMatch`** error is emitted:

```urd
# ✗ NonExhaustiveMatch — missing variant 'Rebel'
match player_faction {
    Faction.Guild  { narrator: "The Guild welcomes you." }
    Faction.Empire { narrator: "For the Empire!" }
}
```

You can satisfy exhaustiveness with either an explicit arm for every variant or
a wildcard catch-all:

```urd
match player_faction {
    Faction.Guild { narrator: "The Guild welcomes you." }
    _ { narrator: "You belong to another faction." }
}
```

---

## Range Patterns

Match against numeric ranges using `..=` (inclusive upper bound) or `..`
(exclusive upper bound):

```urd
match reputation {
    0..=20   { narrator: "You are despised." }
    21..=50  { narrator: "You are tolerated." }
    51..=80  { narrator: "You are respected." }
    81..=100 { narrator: "You are revered." }
    _ { narrator: "Your reputation is beyond measure." }
}
```

Exclusive ranges omit the `=`:

```urd
match score {
    0..50  { narrator: "Below average." }
    50..80 { narrator: "Respectable." }
    80..=100 { narrator: "Outstanding." }
}
```

Ranges are tested in order, so overlapping ranges use whichever arm appears
first.

---

## Range Patterns with Binding

You can capture the matched value into a variable using the `as name` syntax
after a range pattern. This is useful when you need to reference the exact value
that matched inside the arm body:

```urd
match 1d20 {
    1 {
        narrator: "Critical miss."
    }
    2..=9 as roll {
        narrator: "You rolled a {roll}. Not great."
    }
    10..=19 as roll {
        narrator: "You rolled a {roll}. Solid effort."
    }
    20 {
        narrator: "Natural 20!"
    }
}
```

The `as name` binding is **only available on range patterns**. It does not apply
to literal, enum, wildcard, or array patterns. The bound variable is scoped to
the arm body.

---

## Array Patterns for Dice

When you care about **individual die results** (not just the sum), use array
patterns. Array patterns match element-by-element against each die in a roll.

Each element in an array pattern can be either an **integer literal** or a
**wildcard** (`_`). A wildcard in an element position matches any value for that
die, while integer literals require an exact match.

```urd
match 2d6 {
    [1, 1] {
        narrator: "Snake eyes! Catastrophic failure."
    }
    [6, 6] {
        narrator: "Boxcars! Legendary success."
    }
    [_, 6] {
        narrator: "At least one six (second die)."
    }
    [6, _] {
        narrator: "At least one six (first die)."
    }
    [_, _] {
        narrator: "Catch-all for any 2-die combination."
    }
    _ {
        narrator: "A normal roll."
    }
}
```

The array length must match the dice count — `2d6` requires two-element arrays,
`3d8` requires three-element arrays, and so on.

Use `_` inside an array to match a single element position; use `_` as a
top-level arm to catch any remaining values (including sums or combinations not
covered by earlier patterns).

---

## The Wildcard Pattern

The underscore `_` matches any value. It is typically used as the final arm to
handle all cases not covered by earlier patterns:

```urd
match weather {
    "rain" { narrator: "Droplets patter against your hood." }
    "snow" { narrator: "Snowflakes drift past your face." }
    _ { narrator: "The sky is clear." }
}
```

For enum matches, `_` satisfies exhaustiveness — it covers all variants not
explicitly listed. For dice matches, `_` covers any sum or die combination not
handled by preceding arms.

---

## Matching on Dice Rolls

Dice expressions are first-class in Urd, and `match` is the natural way to
branch on their results. When a dice expression is used as the scrutinee, the
compiler knows the possible range of sums and can check coverage accordingly.

```urd
let roll = 2d6

match roll {
    2..=4  { narrator: "Critical failure!" }
    5..=9  { narrator: "Moderate success." }
    10..=12 { narrator: "Critical hit!" }
}
```

Since `2d6` produces sums in the range 2–12, the three arms above are
exhaustive. If they were not, the compiler would flag the gap.

You can also use a dice expression directly as the scrutinee:

```urd
match 1d20 {
    1 { narrator: "Fumble!" }
    2..=10 as roll { narrator: "You rolled {roll}. Miss." }
    11..=19 as roll { narrator: "You rolled {roll}. Hit!" }
    20 { narrator: "Critical hit!" }
}
```

---

## Match Inside Labels and Menus

`match` can appear anywhere a statement is valid — inside labels, menu option
bodies, or conditionals:

```urd
label combat_round {
    let attack = 1d20

    match attack {
        1 {
            narrator: "You fumble your weapon!"
            jump fumble_table
        }
        2..=10 {
            narrator: "Your attack misses."
        }
        11..=19 {
            narrator: "Your attack connects!"
            jump deal_damage
        }
        20 {
            narrator: "Critical hit!"
            jump critical_damage
        }
    }
}
```

---

## RPG-Style Mechanics

Combining `match` with dice rolls is Urd's primary mechanism for tabletop-style
game logic. Here is a more complete example of a skill check:

```urd
label lockpick_attempt {
    narrator: "You kneel before the locked chest and produce your tools."

    let check = 1d20

    match check {
        1 {
            narrator: "Your pick snaps. The lock is jammed."
            end!()
        }
        2..=8 {
            narrator: "The lock resists your efforts."
            narrator: "You'll need to find another way in."
        }
        9..=15 as roll {
            narrator: "After some fiddling (rolled {roll}), the lock clicks open."
            jump chest_contents
        }
        16..=19 as roll {
            narrator: "Rolled {roll} — the lock yields easily to your skilled hands."
            jump chest_contents
        }
        20 {
            narrator: "The lock practically opens itself. You find a hidden compartment!"
            jump secret_compartment
        }
    }
}
```

---

## Static Analysis

### `NonExhaustiveMatch`

Emitted when matching on an enum type and not all variants are covered (with no
wildcard arm). This is an **error** — the compiler refuses to let a value slip
through unhandled.

### Dice Coverage

When the scrutinee is a dice literal (e.g. `2d6`), the compiler computes the
valid sum range and checks whether the arms cover every possible outcome. Gaps
in coverage produce a diagnostic so you can ensure no roll goes unhandled.
Patterns that fall outside the possible range (e.g. matching `1` on a `2d6`
whose minimum sum is 2) are flagged as dead patterns.

### Arm Ordering

Arms are evaluated top-to-bottom. If an earlier arm's pattern fully subsumes a
later arm, the later arm is unreachable. Keep specific patterns (like exact
literals or array patterns) above broader ones (like ranges or `_`).

---

## Quick Reference

| Pattern | Example | Matches |
|---|---|---|
| Integer literal | `42` | Exact integer value |
| String literal | `"hello"` | Exact string value |
| Boolean literal | `true` | Exact boolean value |
| Enum variant | `Faction.Guild` | Specific enum variant |
| Inclusive range | `1..=5` | Integers 1 through 5 |
| Exclusive range | `6..12` | Integers 6 through 11 |
| Range with binding | `1..=20 as roll` | Integers 1–20, captured as `roll` |
| Array (dice) | `[1, 6]` | First die is 1, second is 6 |
| Array with wildcard | `[_, 6]` | First die is anything, second is 6 |
| Wildcard | `_` | Any value (catch-all) |

> **Remember:** Arms use `pattern { body }` syntax — no `=>` arrow. Bindings
> (`as name`) are only available on range patterns. Array pattern elements can
> be integer literals or wildcards (`_`) to match any value in that position.