# Operator Reference

This page documents every operator in Urd, grouped by category and listed in
order of precedence from highest (tightest binding) to lowest (loosest binding).

## Precedence Table

| Precedence | Category                | Operators                       | Associativity |
|:----------:|-------------------------|---------------------------------|:-------------:|
| 11         | Unary                   | `!` `not` `-` (negate)          | prefix        |
| 10         | Multiplicative          | `*` `/` `//` `%`               | left          |
| 9          | Additive                | `+` `-`                         | left          |
| 8          | Shift / Range           | `<<` `>>` `..` `..=`           | left          |
| 7          | Comparison / Membership | `>` `<` `>=` `<=` `in`         | left          |
| 6          | Equality                | `==` `!=`                       | left          |
| 5          | Bitwise AND             | `&`                             | left          |
| 4          | Bitwise XOR             | `^`                             | left          |
| 3          | Bitwise OR              | `\|`                            | left          |
| 2          | Logical AND             | `and` / `&&`                    | left          |
| 1          | Logical OR              | `or` / `\|\|`                   | left          |
| 0          | Assignment              | `=`                             | right         |

Higher precedence means the operator binds more tightly. For example,
`2 + 3 * 4` is parsed as `2 + (3 * 4)` because multiplicative (`*`) has higher
precedence than additive (`+`).

---

## Unary Operators (Precedence 11)

### `-` (Negation)

Negates a numeric value.

```urd
let x = -42
let y = -3.14
```

Operand must be `Int` or `Float`. Applying `-` to any other type is a runtime
`TypeError`.

### `not` (Logical NOT)

Inverts the [truthiness](#truthiness) of a value.

```urd
let alive = true
let dead = not alive        # false
```

`not` does **not** require a `Bool` operand — any value is accepted and
evaluated for truthiness (see [Truthiness](#truthiness) below).

### `!` (Bitwise NOT)

Flips every bit of an integer value.

```urd
let mask = !0xFF            # inverts all bits
```

Operand must be `Int`. Applying `!` to any other type is a runtime `TypeError`.

> **Important:** `!` and `not` are **different operators** with different
> tokens. `!` always performs bitwise NOT on integers. `not` always performs
> logical NOT using truthiness. They are **not** interchangeable.

---

## Multiplicative Operators (Precedence 10)

### `*` (Multiplication)

Multiplies two numeric values.

```urd
let damage = 3 * 10        # 30
let scaled = 1.5 * 2.0     # 3.0
```

`Int * Int → Int`, `Float * Float → Float`. When one operand is `Int` and the
other is `Float`, both are coerced to `Float`. `Roll` values are coerced to
`Float` via summation.

### `/` (Division)

Divides the left operand by the right. **Integer division truncates toward
zero** (not toward negative infinity).

```urd
let share = 7 / 2          # 3  (truncated toward zero)
let neg   = -7 / 2         # -3 (truncated toward zero, NOT -4)
let ratio = 10.0 / 3.0     # 3.3333...
```

`Int / Int → Int` (truncated toward zero). `Float / Float → Float`. Mixed
`Int`/`Float` operands are coerced to `Float`. Division by zero is a runtime
error.

### `//` (Floor Division)

Divides and **floors the result toward negative infinity**. This is the key
difference from `/`.

```urd
let floored = 7 // 2       # 3
let neg = -7 // 2          # -4 (floors toward negative infinity)
```

`Int // Int → Int` (floored). `Float // Float → Float` (floored). Mixed types
are coerced to `Float`. For positive operands `/` and `//` give the same result;
they diverge for negative operands: `-7 / 2` gives `-3` (truncation toward
zero) while `-7 // 2` gives `-4` (floor toward negative infinity).

### `%` (Modulo)

Returns the remainder after division.

```urd
let remainder = 10 % 3     # 1
let even_check = 42 % 2    # 0
```

Both operands must be `Int`. Applying `%` to `Float` values is a `TypeError`.
The sign of the result follows the dividend (left operand), consistent with
Rust's `%` operator.

---

## Additive Operators (Precedence 9)

### `+` (Addition / String Concatenation)

Adds two numbers or concatenates two strings.

```urd
let total = gold + 50
let full_name = first_name + " " + last_name
```

`Int + Int → Int`, `Float + Float → Float`, `Str + Str → Str`. When one
numeric operand is `Int` and the other is `Float`, both are coerced to `Float`.
`Roll` values are coerced to `Float` via summation.

There is **no** automatic string coercion: `"gold: " + 10` is a `TypeError`.
Use string interpolation instead: `"gold: {10}"`.

### `-` (Subtraction)

Subtracts the right operand from the left.

```urd
let remaining = health - damage
```

Same type rules as `+` (excluding string concatenation).

---

## Shift / Range Operators (Precedence 8)

### `<<` (Left Shift)

Shifts bits to the left. Both operands must be `Int`.

```urd
let flags = 1 << 3         # 8
```

### `>>` (Right Shift)

Shifts bits to the right (arithmetic shift — sign bit is preserved). Both
operands must be `Int`.

```urd
let halved = 16 >> 1       # 8
```

### `..` (Exclusive Range)

Creates a range from `start` (inclusive) to `end` (exclusive).

```urd
let indices = 0..10         # contains 0, 1, 2, ..., 9
```

### `..=` (Inclusive Range)

Creates a range from `start` (inclusive) to `end` (inclusive).

```urd
let dice_range = 1..=6      # contains 1, 2, 3, 4, 5, 6
```

Ranges are integer-only and produce a `Range` value.

---

## Comparison / Membership Operators (Precedence 7)

### `>`, `<`, `>=`, `<=` (Ordering)

Compare two values and return `Bool`.

```urd
if health < 20 {
    narrator: "You are critically wounded."
}
```

Ordering comparisons work on `Int`, `Float`, and cross-type `Int`/`Float`
pairs (the integer is carefully compared against the float without precision
loss). Comparing incompatible types is a `TypeError`.

### `in` (Membership)

Tests whether a value is contained in a collection or range. The behaviour
depends on the type of the right-hand side:

| LHS type | RHS type | Semantics |
|----------|----------|-----------|
| `Int` | `Range` | Checks if the integer falls within the range bounds |
| any | `List` | Checks if the value is an element of the list (structural equality) |
| `Str` | `Map` | Checks if the string is a key in the map |
| `Str` | `Str` | Checks if the left string is a substring of the right string |

#### Range membership

```urd
if roll in 1..=6 {
    narrator: "Valid die roll."
}
```

The left-hand side must be `Int` and the right-hand side must be `Range`.

#### List membership

```urd
if "sword" in inventory {
    narrator: "You draw your sword."
}
```

The left-hand side can be any type. Elements are compared using structural
equality.

#### Map key membership

```urd
if "hp" in stats {
    narrator: "HP is tracked."
}
```

The left-hand side must be `Str` (map keys are always strings).

#### Substring check

```urd
if "hello" in greeting {
    narrator: "What a friendly message!"
}
```

Both sides must be `Str`. Returns `true` if the left string appears anywhere
inside the right string.

---

## Equality Operators (Precedence 6)

### `==` (Equal)

Returns `true` if both operands are equal.

```urd
if gold == 0 {
    narrator: "You are penniless."
}
```

### `!=` (Not Equal)

Returns `true` if the operands are not equal.

```urd
if faction != Faction.Empire {
    narrator: "You are not aligned with the Empire."
}
```

---

## Bitwise Operators (Precedence 5–3)

### `&` (Bitwise AND) — Precedence 5

```urd
let masked = flags & 0xFF
```

### `^` (Bitwise XOR) — Precedence 4

```urd
let toggled = state ^ mask
```

### `|` (Bitwise OR) — Precedence 3

```urd
let combined = flag_a | flag_b
```

All bitwise binary operators require `Int` operands and produce `Int` results.

---

## Logical Operators (Precedence 2–1)

### `and` / `&&` (Logical AND) — Precedence 2

Returns `true` if both operands are [truthy](#truthiness). Short-circuits: if
the left operand is falsy, the right operand is not evaluated.

```urd
if has_key and gold >= 10 {
    narrator: "You can open the door."
}
```

`and` and `&&` are interchangeable. Use whichever style you prefer.

### `or` / `||` (Logical OR) — Precedence 1

Returns `true` if either operand is [truthy](#truthiness). Short-circuits: if
the left operand is truthy, the right operand is not evaluated.

```urd
if has_torch or has_lantern {
    narrator: "You have a light source."
}
```

`or` and `||` are interchangeable.

Logical operators evaluate their operands for [truthiness](#truthiness) — they
do **not** require strict `Bool` operands.

---

## Assignment Operator (Precedence 0)

### `=` (Assignment)

Assigns a value to a variable. The variable must have been previously declared
with `let`, `global`, or `const` (though assigning to `const` is a compile-time
error).

```urd
gold = gold + 50
has_torch = true
health = health - damage
```

Assignment is right-associative and has the lowest precedence of all operators.
There are no compound assignment operators (`+=`, `-=`, etc.) in Urd; write the
expanded form instead.

---

## Truthiness

Logical operators (`and`, `or`) and `not` evaluate their operands for
truthiness rather than requiring strict `Bool` values. The rules are:

| Value                          | Truthy? |
|--------------------------------|---------|
| `null`                         | false   |
| `Bool(b)`                      | `b`     |
| `Int(0)`                       | false   |
| `Float(0.0)` or `Float(NaN)`  | false   |
| Empty `List`                   | false   |
| Empty `Roll`                   | false   |
| Empty `Range`                  | false   |
| Everything else                | true    |

---

## Numeric Type Coercion

Arithmetic operators (`+`, `-`, `*`, `/`, `//`) follow these type rules:

| Left     | Right    | Result                                     |
|----------|----------|--------------------------------------------|
| `Int`    | `Int`    | `Int`                                      |
| `Float`  | `Float`  | `Float`                                    |
| `Int`    | `Float`  | `Float` (both coerced via `to_float`)      |
| `Float`  | `Int`    | `Float` (both coerced via `to_float`)      |
| `Roll`   | *any numeric* | `Float` (`Roll` sums its dice, then coerces) |

`%` (modulo) is an exception: it requires both operands to be `Int`.

Bitwise operators (`&`, `^`, `|`, `<<`, `>>`, `!`) always require `Int`
operands.

---

## Operator Overloading

Urd does **not** support operator overloading. All operators have fixed
semantics determined by the operand types. User-defined types (structs, enums)
cannot define custom operator behaviour.

## String Interpolation

While not technically an operator, string interpolation (`{variable}`) is
worth noting here because it interacts with the expression system:

```urd
narrator: "You have {gold} gold and {health} HP."
narrator: "{player.name} dealt {damage} damage."
```

Only variable names and dot-paths (e.g. `{gold}`, `{player.name}`) are
supported inside interpolation braces. Arbitrary expressions like arithmetic
or function calls are **not** allowed. If you need a computed value, store it
in a variable first:

```urd
let total_damage = base_damage * multiplier
narrator: "Damage dealt: {total_damage}"
```

The value is evaluated at runtime, and the result's `Display` representation
is substituted into the string.