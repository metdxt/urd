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

The `in` operator tests whether an integer is contained within a range.

```urd
if gold in 10..=100 {
    narrator: "You have a modest purse."
}

if roll in 1..=6 {
    narrator: "Valid die roll."
}
```

The right-hand side **must** be a `Range` and the left-hand side must be an
`Int`. Using `in` with other collection types (`List`, `Map`, `String`) is a
`TypeError`.

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

Operators bind from tightest (top) to loosest (bottom). For the full table with
binding powers and detailed semantics, see the
[Operator Reference](../reference/operators.md#precedence-table).

| Category              | Operators                              | Associativity |
|-----------------------|----------------------------------------|:-------------:|
| Unary                 | `!` `not` `-` (negate)                 | prefix        |
| Multiplicative        | `*` `/` `//` `%`                       | left          |
| Additive              | `+` `-`                                | left          |
| Shift / Range         | `<<` `>>` `..` `..=`                   | left          |
| Comparison            | `>` `<` `>=` `<=` `in`                 | left          |
| Equality              | `==` `!=`                              | left          |
| Bitwise AND / XOR / OR| `&` then `^` then `\|`                 | left          |
| Logical AND           | `and` / `&&`                           | left          |
| Logical OR            | `or` / `\|\|`                          | left          |
| Assignment            | `=`                                    | right         |

Parentheses `( )` can always be used to override the default precedence:

```urd
let result = (a + b) * c
let check = gold > 10 and (has_key or has_torch)
```
