# If / Elif / Else

Urd provides standard conditional branching with `if`, `elif`, and `else`. Conditions can be any expression that evaluates to a boolean (or a truthy/falsy value), and branches can appear anywhere statements are allowed — inside labels, menu options, or at the top level.

## Basic Syntax

```urd
if condition {
    # executed when condition is true
}
```

With an else branch:

```urd
if has_torch {
    narrator: "The torch casts flickering shadows on the cave walls."
} else {
    narrator: "You stumble through the pitch-dark tunnel."
}
```

## Elif Chains

When you need to test multiple conditions in sequence, use `elif` to add intermediate branches:

```urd
if health > 75 {
    narrator: "You feel strong and confident."
} elif health > 25 {
    narrator: "You're wounded but still standing."
} elif health > 0 {
    narrator: "You can barely stay on your feet."
} else {
    narrator: "You collapse to the ground."
}
```

Only the first branch whose condition evaluates to `true` is executed. If no condition matches and an `else` block is present, the `else` body runs. If no condition matches and there is no `else`, execution continues past the entire construct.

## Conditions

The condition position accepts any expression. Common patterns include:

### Comparisons

```urd
if gold >= 50 {
    narrator: "You can afford the shield."
}
```

### Boolean Variables

```urd
if has_key {
    narrator: "You unlock the door."
}
```

### Boolean Operators

Urd uses the keywords `and`, `or`, and `not` for boolean logic:

```urd
if has_key and not is_locked {
    narrator: "You open the door."
}

if health <= 0 or has_fled {
    narrator: "The battle is over."
}

if not (gold > 100 and reputation > 50) {
    narrator: "The merchant refuses to deal with you."
}
```

### The `in` Operator

Test whether an integer falls within a range:

```urd
let roll = 1d20

if roll in 10..=20 {
    narrator: "A solid roll — you succeed!"
}
```

The `in` operator works **only** with ranges (`Int in Range`). It does not
support lists, maps, or strings.

### Function Calls

```urd
fn is_critical(roll: int) -> bool {
    return roll >= 18
}

if is_critical(attack_roll) {
    narrator: "Critical hit!"
}
```

## Inside Labels and Menus

Conditionals nest naturally within labels and menu options:

```urd
label shop {
    narrator: "The merchant spreads their wares before you."

    menu {
        "Buy healing potion (10 gold)" {
            if gold >= 10 {
                gold = gold - 10
                narrator: "You purchase a healing potion."
            } else {
                narrator: "You can't afford that."
            }
        }
        "Leave" {
            jump town_square
        }
    }
}
```

## Dead Branch Analysis

The Urd compiler performs constant-folding analysis on `if` conditions. When a condition is composed entirely of compile-time constants, the compiler knows which branch will always execute and emits an `AlwaysDeadBranch` warning for the unreachable side:

```urd
const debug = false

# ⚠ AlwaysDeadBranch: the 'then' branch is never reachable
if debug {
    narrator: "Debug mode is on."
} else {
    narrator: "Running in production mode."
}
```

This catches common mistakes like leftover debug flags or conditions that were accidentally hard-coded. The warning tells you which branch is dead:

- If the condition is always `false`, the `then` branch (the `if` body) is dead.
- If the condition is always `true`, the `else` branch is dead.

> **Tip:** If you intentionally want a compile-time toggle, you can suppress the warning by using a `let` binding instead of `const`, since `let` values are not constant-folded.

## Nesting

Conditionals can be nested to any depth:

```urd
if has_torch {
    if courage > 50 {
        narrator: "You boldly enter the dark cave."
        jump cave_depths
    } else {
        narrator: "You peer nervously into the darkness."
    }
} else {
    narrator: "You can't see anything without a light source."
}
```

However, deeply nested conditionals can be hard to read. Consider using [match](./match.md) for complex branching, or breaking logic into separate labels with [jump and return](./jump-and-return.md).

## Truthiness

Conditions are expected to evaluate to `bool`. Non-boolean values in the condition position will produce a type error at runtime. Always ensure your conditions resolve to `true` or `false`.