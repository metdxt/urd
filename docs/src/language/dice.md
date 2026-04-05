# Dice

Urd has first-class support for dice rolls — a core mechanic in tabletop RPGs
and many narrative games. Dice are literal expressions in the language, not
library functions, which means the compiler understands them and static analysis
can reason about their possible outcomes.

---

## Dice Literal Syntax

Dice are written in the familiar tabletop notation `NdM` or `NDM`, where `N` is
the number of dice and `M` is the number of sides per die:

```urd
let attack = 2d6
let check = 1d20
let fireball = 8D6
```

Both lowercase `d` and uppercase `D` are accepted — they are interchangeable.

### Limits

The count (`N`) and sides (`M`) are each capped at **255** (stored as `u8`
internally). This covers every practical tabletop scenario:

```urd
let big_roll = 255d255   # legal, if unusual
```

A dice literal with zero sides (`Nd0`) will produce a `TypeError` at runtime
when evaluated.

---

## Dice Literals Produce Rolls

A dice literal like `2d6` is **immediately evaluated** into a `Roll` the moment
it is encountered at runtime. There is no separate "Dice" runtime type that
scripts can observe — the `Dice` variant exists only in the AST/IR as an
intermediate representation and is converted to a `Roll` by the evaluator when
it calls the `DiceRoller`.

A `Roll` is a `Vec<i64>` of individual die results:

```urd
let roll = 2d6
# roll now holds a Roll value, e.g. Roll([3, 5])
# NOT Dice(2, 6) — the dice have already been rolled
```

| Concept        | Where it exists | What it represents                                        |
|----------------|-----------------|-----------------------------------------------------------|
| `Dice(N, M)`   | AST / IR only   | A roll specification (count + sides) — never seen at runtime |
| `Roll([...])` | Runtime         | The result of rolling — a list of individual die results  |

> **Key point:** You will never encounter a `Dice` value in a variable, event,
> or the VM environment. Every dice literal is rolled eagerly on evaluation, and
> the result is always a `Roll`.

---

## Rolling Dice

The most common pattern is to roll dice and react to the result:

```urd
let roll = 1d20

if roll >= 15 {
    narrator: "Success! You leap across the chasm."
} else {
    narrator: "You stumble and barely catch the edge."
}
```

### Sum Comparison

When you compare a `Roll` value to an integer, Urd uses the **sum** of all
individual die results:

```urd
let damage = 3d6
narrator: "You deal {damage} damage."
# {damage} interpolates as the sum of the three dice
```

---

## Matching on Dice Results

The `match` statement integrates naturally with dice. You can match on the sum
using range patterns:

```urd
let roll = 2d6

match roll {
    2..=4 {
        narrator: "Critical failure!"
    }
    5..=9 {
        narrator: "Moderate success."
    }
    10..=12 {
        narrator: "Critical hit!"
    }
}
```

The compiler performs **dice-aware exhaustiveness checking**. For the expression
`2d6`, the minimum possible sum is 2 and the maximum is 12. The compiler verifies
that your range patterns cover every possible outcome in that span.

### Array-Style Matching

For fine-grained control over individual die results, use array patterns:

```urd
let roll = 2d6

match roll {
    [1, 1] {
        narrator: "Snake eyes! Double ones!"
    }
    [6, 6] {
        narrator: "Boxcars! Double sixes!"
    }
    [3, 3] {
        narrator: "Double threes — an omen of balance."
    }
    _ {
        narrator: "An ordinary roll."
    }
}
```

Array patterns match against the individual die results in order. Each element
in the array must be an integer literal — wildcards (`_`) are **not** valid
inside array brackets. Use the top-level `_` arm as the default catch-all for
any combination not explicitly listed.

> **Note:** When array patterns are present, the compiler requires a wildcard
> `_` arm because exhaustiveness analysis over individual die combinations is
> intractable for large dice pools.

---

## RPG-Style Skill Checks

Dice rolls combine naturally with the rest of Urd's features to model tabletop
mechanics:

```urd
global strength = 14

label arm_wrestling {
    narrator: "The orc challenges you to arm wrestling."

    let roll = 1d20
    let total = roll + strength

    if total >= 20 {
        narrator: "You slam the orc's hand down. Victory!"
    } elif total >= 12 {
        narrator: "A close match, but you edge ahead."
    } else {
        narrator: "The orc overpowers you easily."
    }
}
```

### Advantage / Disadvantage

Model D&D-style advantage by rolling twice and picking the better result:

```urd
fn roll_with_advantage() -> int {
    let a = 1d20
    let b = 1d20
    if a > b {
        return a
    }
    return b
}

label stealth_check {
    let roll = roll_with_advantage()
    narrator: "You rolled {roll} with advantage."
}
```

---

## The `DiceRoller` Trait

At the Rust integration level, the conversion from `Dice` (AST) to `Roll`
(runtime) is delegated to a pluggable `DiceRoller` trait:

```rust
pub trait DiceRoller: Send + Sync {
    /// Roll `count` dice each with `sides` faces.
    /// Returns each individual result in the range 1..=sides.
    fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64>;

    /// Roll and return the total (sum of all individual results).
    fn roll(&self, count: u32, sides: u32) -> i64 {
        self.roll_individual(count, sides).iter().sum()
    }
}
```

The VM ships with a `DefaultDiceRoller` that uses random number generation. You
can replace it with a deterministic roller for testing:

```rust
struct FixedRoller(Vec<i64>);

impl DiceRoller for FixedRoller {
    fn roll_individual(&self, count: u32, _sides: u32) -> Vec<i64> {
        self.0[..count as usize].to_vec()
    }
}

// Inject the fixed roller into the VM's environment
vm.set_dice_roller(Box::new(FixedRoller(vec![3, 5])));
```

This is essential for writing reproducible tests for scripts that involve dice.
See the [Dice Roller](../integration/dice-roller.md) integration guide for full
details.

---

## Summary

- Dice literals: `NdM` or `NDM` (e.g. `2d6`, `1d20`, `3D8`)
- Count and sides capped at 255 each
- Dice literals are **immediately evaluated** into `Roll(Vec<i64>)` at runtime — there is no `Dice` runtime type
- `match` with range patterns for sum-based branching
- `match` with array patterns (`[1, 1]`, `[6, 6]`) for individual die inspection
- Pluggable `DiceRoller` trait enables deterministic testing
- First-class language support — no imports or libraries needed