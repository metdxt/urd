# Operator Reference

This page documents every operator in Urd, grouped by category and listed in order of precedence from highest (tightest binding) to lowest (loosest binding).

## Precedence Table

| Precedence | Category | Operators | Associativity |
|:----------:|----------|-----------|---------------|
| 1 | Unary | `-` `!` `not` | Right |
| 2 | Multiplicative | `*` `/` `//` `%` | Left |
| 3 | Additive | `+` `-` | Left |
| 4 | Shift | `<<` `>>` | Left |
| 5 | Bitwise AND | `&` | Left |
| 6 | Bitwise XOR | `^` | Left |
| 7 | Bitwise OR | `|` | Left |
| 8 | Comparison | `==` `!=` `<` `>` `<=` `>=` | Left |
| 9 | Logical AND | `and` `&&` | Left |
| 10 | Logical OR | `or` `||` | Left |
| 11 | Range | `..` `..=` | Left |
| 12 | Membership | `in` | Left |
| 13 | Assignment | `=` | Right |

Higher precedence means the operator binds more tightly. For example, `2 + 3 * 4` is parsed as `2 + (3 * 4)` because multiplicative (`*`) has higher precedence than additive (`+`).

---

## Unary Operators (Precedence 1)

### `-` (Negation)

Negates a numeric value.

```urd
let x = -42
let y = -3.14
```

Operand must be `Int` or `Float`. Applying `-` to any other type is a runtime `TypeError`.

### `!` / `not` (Logical NOT)

Inverts a boolean value. `!` and `not` are interchangeable — use whichever reads better in context.

```urd
let alive = true
let dead = not alive        # false
let also_dead = !alive      # false
```

Operand must be `Bool`.

---

## Multiplicative Operators (Precedence 2)

### `*` (Multiplication)

Multiplies two numeric values.

```urd
let damage = 3 * 10        # 30
let scaled = 1.5 * 2.0     # 3.0
```

Both operands must be the same numeric type (`Int * Int → Int`, `Float * Float → Float`). There is no implicit coercion between `Int` and `Float`.

### `/` (Division)

Divides the left operand by the right.

```urd
let share = 100 / 3        # 33 (integer division, truncates toward zero)
let ratio = 10.0 / 3.0     # 3.3333...
```

Integer division truncates toward zero (not floor division). Division by zero is a runtime `TypeError`.

### `//` (Floor Division)

Divides and floors the result toward negative infinity.

```urd
let floored = 7 // 2       # 3
let neg = -7 // 2          # -4 (floors toward negative infinity)
```

This differs from `/` for negative operands: `-7 / 2` gives `-3` (truncation toward zero), while `-7 // 2` gives `-4` (floor toward negative infinity).

### `%` (Modulo)

Returns the remainder after division.

```urd
let remainder = 10 % 3     # 1
let even_check = 42 % 2    # 0
```

The sign of the result follows the dividend (left operand), consistent with Rust's `%` operator.

---

## Additive Operators (Precedence 3)

### `+` (Addition / String Concatenation)

Adds two numbers or concatenates two strings.

```urd
let total = gold + 50
let full_name = first_name + " " + last_name
```

Both operands must be the same type: `Int + Int → Int`, `Float + Float → Float`, `Str + Str → Str`. Cross-type addition (e.g., `Int + Str`) is a `TypeError`.

### `-` (Subtraction)

Subtracts the right operand from the left.

```urd
let remaining = health - damage
```

---

## Shift Operators (Precedence 4)

### `<<` (Left Shift)

Shifts bits to the left.

```urd
let flags = 1 << 3         # 8
```

The shift count must be non-negative and less than 64. Out-of-range counts produce a runtime `TypeError`.

### `>>` (Right Shift)

Shifts bits to the right (arithmetic shift — sign bit is preserved).

```urd
let halved = 16 >> 1       # 8
```

The shift count must be non-negative and no greater than 63. Out-of-range counts produce a runtime `TypeError`.

---

## Bitwise Operators (Precedence 5–7)

### `&` (Bitwise AND) — Precedence 5

```urd
let masked = flags & 0xFF
```

### `^` (Bitwise XOR) — Precedence 6

```urd
let toggled = state ^ mask
```

### `|` (Bitwise OR) — Precedence 7

```urd
let combined = flag_a | flag_b
```

All bitwise operators require `Int` operands and produce `Int` results.

---

## Comparison Operators (Precedence 8)

### `==` (Equal)

Returns `true` if both operands are equal.

```urd
if gold == 0 {
    narrator: "You are penniless."
}
```

Comparison is type-strict: `1 == 1.0` is `false` because `Int` and `Float` are different types. There is no implicit coercion.

### `!=` (Not Equal)

Returns `true` if the operands are not equal.

```urd
if faction != Faction.Empire {
    narrator: "You are not aligned with the Empire."
}
```

### `<` (Less Than)

```urd
if health < 20 {
    narrator: "You are critically wounded."
}
```

### `>` (Greater Than)

```urd
if gold > 100 {
    narrator: "You are wealthy."
}
```

### `<=` (Less Than or Equal)

```urd
if potions <= 0 {
    narrator: "You have no potions left."
}
```

### `>=` (Greater Than or Equal)

```urd
if gold >= price {
    narrator: "You can afford it."
}
```

All comparison operators work on `Int`, `Float`, and `Str` (lexicographic ordering for strings). Comparing incompatible types is a `TypeError`.

---

## Logical Operators (Precedence 9–10)

### `and` / `&&` (Logical AND) — Precedence 9

Returns `true` if both operands are `true`. Short-circuits: if the left operand is `false`, the right operand is not evaluated.

```urd
if has_key and gold >= 10 {
    narrator: "You can open the door."
}
```

`and` and `&&` are interchangeable. Use whichever style you prefer.

### `or` / `||` (Logical OR) — Precedence 10

Returns `true` if either operand is `true`. Short-circuits: if the left operand is `true`, the right operand is not evaluated.

```urd
if has_torch or has_lantern {
    narrator: "You have a light source."
}
```

`or` and `||` are interchangeable.

Both operands must be `Bool`. Urd does not have truthy/falsy values — `0`, `""`, and `null` are **not** implicitly `false`.

---

## Range Operators (Precedence 11)

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

Ranges are integer-only. See the [RuntimeValue](../api/runtime-value.md) documentation for range semantics (`len`, `contains`).

---

## Membership Operator (Precedence 12)

### `in`

Tests whether a value is contained in a collection (list, range, map, or string).

```urd
if "sword" in inventory {
    narrator: "You are armed."
}

if roll in 1..=6 {
    narrator: "Valid die roll."
}
```

The right-hand operand determines the semantics:
- **List**: checks if the value is an element of the list
- **Range**: checks if the integer is within the range bounds
- **Map**: checks if the string is a key in the map
- **String**: checks if the left string is a substring of the right string

---

## Assignment Operator (Precedence 13)

### `=` (Assignment)

Assigns a value to a variable. The variable must have been previously declared with `let`, `global`, or `const` (though assigning to `const` is a compile-time error).

```urd
gold = gold + 50
has_torch = true
health = health - damage
```

Assignment is a statement, not an expression — you cannot use `=` inside another expression. There are no compound assignment operators (`+=`, `-=`, etc.) in Urd; write the expanded form instead.

---

## Operator Overloading

Urd does **not** support operator overloading. All operators have fixed semantics determined by the operand types. User-defined types (structs, enums) cannot define custom operator behaviour.

## String Interpolation

While not technically an operator, string interpolation (`{expression}`) is worth noting here because it interacts with the expression system:

```urd
narrator: "You have {gold} gold and {health} HP."
narrator: "Damage dealt: {base_damage * multiplier}"
```

Any expression valid in a normal context is valid inside `{}` interpolation braces. The expression is evaluated at runtime, and the result's `Display` representation is substituted into the string.