# Operators

Urd provides a full set of operators for arithmetic, comparison, logic, bitwise
manipulation, ranges, and membership testing. Operator precedence is handled by
a Pratt parser — the table at the end of this page gives the exact binding
powers.

---

## Arithmetic

| Operator | Description        | Example         |
|----------|--------------------|-----------------|
| `+`      | Addition           | `3 + 4` → `7`  |
| `-`      | Subtraction        | `10 - 3` → `7` |
| `*`      | Multiplication     | `6 * 7` → `42` |
| `/`      | Division           | `7 / 2` → `3.5`|
| `//`     | Integer division   | `7 // 2` → `3` |
| `%`      | Modulo (remainder) | `7 % 3` → `1`  |

Integer division (`//`) always truncates toward zero and returns an `int`.
Regular division (`/`) promotes to `float` when either operand is a float.

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

All comparison operators return a `bool`.

```urd
let alive = health > 0
let exact = gold == 50
```

---

## Logical

| Operator    | Description |
|-------------|-------------|
| `and` / `&&` | Logical AND |
| `or` / `\|\|`  | Logical OR  |
| `not` / `!`  | Logical NOT (unary, prefix) |

The keyword forms (`and`, `or`, `not`) and the symbolic forms (`&&`, `||`, `!`)
are interchangeable. Use whichever reads best in context — most Urd scripts
prefer the keyword forms for clarity.

```urd
if has_torch and health > 0 {
    narrator: "You press onward, torch blazing."
}

if not has_key or gold < 10 {
    narrator: "You're not ready for this door."
}
```

> **Note:** `!` also serves as bitwise NOT when applied to an integer operand.
> The parser distinguishes the two based on the operand type at compile time.

---

## Bitwise

| Operator | Description |
|----------|-------------|
| `&`      | Bitwise AND |
| `\|`     | Bitwise OR  |
| `^`      | Bitwise XOR |
| `!`      | Bitwise NOT (unary, prefix) |
| `<<`     | Left shift  |
| `>>`     | Right shift |

Bitwise operators work on `int` values.

```urd
let flags = 0b1010
let mask  = 0b1100
let result = flags & mask   # 0b1000
let shifted = 1 << 4        # 16
```

---

## Range

| Operator | Description      | Example              |
|----------|------------------|----------------------|
| `..`     | Exclusive range  | `0..5` → 0, 1, 2, 3, 4 |
| `..=`    | Inclusive range   | `0..=5` → 0, 1, 2, 3, 4, 5 |

Ranges produce a `range` value. They are integer-only and are commonly used
with the `in` operator or in iteration contexts.

```urd
let r = 1..10
let inclusive = 1..=10
```

---

## Membership (`in`)

The `in` operator tests whether a value is contained within a range, list, or
map.

```urd
if gold in 10..=100 {
    narrator: "You have a modest purse."
}

let items = ["sword", "shield", "potion"]
if "potion" in items {
    narrator: "You rummage through your pack and find a potion."
}
```

When used with a `map`, `in` checks for key membership:

```urd
let inventory = :{ sword: 1, shield: 1 }
if "sword" in inventory {
    narrator: "Your blade is at your side."
}
```

---

## Assignment

| Operator | Description |
|----------|-------------|
| `=`      | Assign a value to a variable |

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

## Precedence Table

Operators are listed from **highest** to **lowest** precedence. Operators on
the same row share the same binding power.

| Precedence | Operators                          | Associativity | Category       |
|:----------:|-------------------------------------|:-------------:|----------------|
| 11         | `!` (unary), `not`, `-` (negate)   | prefix        | Unary          |
| 10         | `*`, `/`, `//`, `%`                | left          | Multiplicative |
| 9          | `+`, `-`                           | left          | Additive       |
| 8          | `<<`, `>>`, `..`, `..=`            | left          | Shift / Range  |
| 7          | `>`, `<`, `>=`, `<=`, `in`         | left          | Comparison     |
| 6          | `==`, `!=`                         | left          | Equality       |
| 5          | `&`                                | left          | Bitwise AND    |
| 4          | `^`                                | left          | Bitwise XOR    |
| 3          | `\|`                               | left          | Bitwise OR     |
| 2          | `and` / `&&`                       | left          | Logical AND    |
| 1          | `or` / `\|\|`                      | left          | Logical OR     |
| 0          | `=`                                | right         | Assignment     |

Parentheses `( )` can always be used to override the default precedence:

```urd
let result = (a + b) * c
let check = gold > 10 and (has_key or has_torch)
```

---

## Operator Type Behavior

Most operators follow predictable type-promotion rules:

- **int ○ int** → `int` (for arithmetic operators)
- **int ○ float** or **float ○ int** → `float`
- **str + str** → `str` (concatenation)
- **str + int** or **str + float** → `str` (coercion, then concatenation)
- **Comparison operators** always return `bool`
- **Logical operators** always return `bool`
- **Bitwise operators** require `int` operands

Type mismatches that cannot be resolved (e.g. `"hello" * true`) produce a
runtime error.