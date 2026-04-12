# Operators

Urd provides a full set of operators for arithmetic, comparison, logic, bitwise
manipulation, ranges, and membership testing. This page covers the essentials —
for the complete precedence table and detailed per-operator semantics, see the
[Operator Reference](../reference/operators.md).

---

## Arithmetic

| Operator | Description        | Example              |
|----------|--------------------|----------------------|
| `+`      | Addition           | `3 + 4` → `7`       |
| `-`      | Subtraction        | `10 - 3` → `7`      |
| `*`      | Multiplication     | `6 * 7` → `42`      |
| `/`      | Division           | `7 / 2` → `3`       |
| `//`     | Floor division     | `-7 // 2` → `-4`    |
| `%`      | Modulo (remainder) | `7 % 3` → `1`       |

Both `/` and `//` perform integer division when both operands are `Int`, but
they differ in rounding direction: `/` **truncates toward zero** while `//`
**floors toward negative infinity**. For positive operands the results are
identical; the difference shows with negative values (`-7 / 2` → `-3`,
`-7 // 2` → `-4`).

When one numeric operand is `Int` and the other is `Float`, the result of `+`,
`-`, `*`, `/`, and `//` is promoted to `Float`. The `%` operator is
integer-only.

```urd
let coins = 100
let party_size = 3
let share = coins // party_size   # 33
let remainder = coins % party_size # 1
```

---

## Comparison

| Operator | Description              |
|----------|--------------------------|
| `==`     | Equal to                 |
| `!=`     | Not equal to             |
| `>`      | Greater than             |
| `<`      | Less than                |
| `>=`     | Greater than or equal to |
| `<=`     | Less than or equal to    |

All comparison operators return a `bool`. Ordering comparisons (`>`, `<`, `>=`,
`<=`) work across `Int` and `Float` types with careful precision handling.

```urd
let alive = health > 0
let exact = gold == 50
```

---

## Logical

| Operator | Description                 |
|----------|-----------------------------|
| `and` / `&&` | Logical AND            |
| `or` / `\|\|`  | Logical OR          |
| `not`    | Logical NOT (unary prefix)  |

`and`/`&&` and `or`/`||` are interchangeable pairs. Use whichever reads best in
context — most Urd scripts prefer the keyword forms for clarity.

Logical operators use [truthiness](../reference/operators.md#truthiness) to
evaluate their operands: `null`, `0`, `0.0`, `NaN`, and empty collections are
falsy; everything else is truthy.

```urd
if has_torch and health > 0 {
    narrator: "You press onward, torch blazing."
}

if not has_key or gold < 10 {
    narrator: "You're not ready for this door."
}
```

> **Important:** `not` is logical NOT and `!` is bitwise NOT — they are
> **not** interchangeable. See [Bitwise](#bitwise) below.

---

## Bitwise

| Operator | Description                 |
|----------|-----------------------------|
| `&`      | Bitwise AND                 |
| `\|`     | Bitwise OR                  |
| `^`      | Bitwise XOR                 |
| `!`      | Bitwise NOT (unary prefix)  |
| `<<`     | Left shift                  |
| `>>`     | Right shift                 |

Bitwise operators work on `Int` values. Note that `!` (bitwise NOT) and `not`
(logical NOT) are **separate** operators — `!` flips every bit of an integer,
while `not` inverts truthiness.

```urd
let flags = 0b1010
let mask  = 0b1100
let result = flags & mask   # 0b1000
let shifted = 1 << 4        # 16
let inverted = !flags       # flips all bits
```

---

## Range

| Operator | Description      | Example                    |
|----------|------------------|----------------------------|
| `..`     | Exclusive range  | `0..5` → 0, 1, 2, 3, 4    |
| `..=`    | Inclusive range   | `0..=5` → 0, 1, 2, 3, 4, 5|

Ranges produce a `range` value. They are integer-only and are commonly used
with the `in` operator or in iteration contexts.

```urd
let r = 1..10
let inclusive = 1..=10
```

---

## Membership (`in`)

The `in` operator tests whether a value is contained in a range, list, map, or
string. The meaning depends on the type of the right-hand side:

### Range membership — `Int in Range`

Check whether an integer falls within a range:

```urd
if roll in 1..=6 {
    narrator: "Valid die roll."
}

if gold in 10..=100 {
    narrator: "You have a modest purse."
}
```

The left-hand side must be an `Int` and the right-hand side must be a `Range`.

### List membership — `value in List`

Check whether a value is an element of a list. Comparison uses structural
equality, so any type that supports `==` works on the left-hand side:

```urd
let inventory = ["sword", "shield", "potion"]

if "sword" in inventory {
    narrator: "You draw your sword."
}

if 42 in [10, 20, 42, 99] {
    narrator: "Found the answer."
}
```

### Map key membership — `Str in Map`

Check whether a string key exists in a map. The left-hand side must be a `Str`:

```urd
let stats = :{ health: 100, mana: 50 }

if "mana" in stats {
    narrator: "You channel your magical energy."
}
```

### Substring check — `Str in Str`

Check whether the left string is a substring of the right string. Both sides
must be `Str`:

```urd
let greeting = "hello, world"

if "hello" in greeting {
    narrator: "A friendly greeting."
}
```

---

## Assignment

| Operator | Description                    |
|----------|--------------------------------|
| `=`      | Assign a value to a variable   |

Assignment is right-associative and has the lowest precedence of all operators.

```urd
let x = 10
x = x + 5
```

> There are no compound assignment operators (`+=`, `-=`, etc.) in Urd.
> Use the explicit form: `gold = gold + 10`.

---

## Member Access and Subscript

| Operator  | Description                        |
|-----------|------------------------------------|
| `.`       | Access a field or call a method    |
| `[index]` | Index into a list, map, or string  |

```urd
# Field access
narrator: "{player.name} draws their weapon."

# Method call
let upper = "hello".to_upper()   # "HELLO"

# List indexing
let first = items[0]

# Map indexing
let sword_count = inventory["sword"]
```

---

## Precedence Overview

In brief, operators bind from tightest to loosest in this order: **unary** (`-`,
`!`, `not`) → **multiplicative** (`*`, `/`, `//`, `%`) → **additive** (`+`,
`-`) → **shift / range** (`<<`, `>>`, `..`, `..=`) → **comparison** (`>`, `<`,
`>=`, `<=`, `in`) → **equality** (`==`, `!=`) → **bitwise** (`&`, `^`, `|`) →
**logical AND** (`and` / `&&`) → **logical OR** (`or` / `||`) → **assignment**
(`=`).

For the complete precedence table with binding powers and detailed per-operator
semantics, see the [Operator Reference](../reference/operators.md#precedence-table).

Parentheses `( )` can always be used to override the default precedence:

```urd
let result = (a + b) * c
let check = gold > 10 and (has_key or has_torch)
```
