# Variables & Types

Urd provides four variable declaration keywords, each with distinct scoping and
mutability rules. All values carry a runtime type, and you may optionally
annotate declarations for compile-time checking.

---

## Declaration Keywords

### `let` — Mutable Local

`let` creates a mutable binding scoped to the current block (label body, if-branch, etc.).

```urd
let x = 10
let name = "Traveler"
x = x + 1
```

### `const` — Immutable Binding

`const` creates a binding that cannot be reassigned after its initial declaration.

```urd
const max_health = 100
```

See [Constants](./constants.md) for full details on usage patterns, character
definitions, and compile-time enforcement.

### `global` — Mutable, Persistent State

`global` creates a mutable binding that persists across label boundaries.

```urd
global gold = 0
```

See [Globals](./globals.md) for full details on persistence, the `@fluent`
decorator, and common use cases.

### `extern` — Host-Provided Value

`extern` declares a value that the host application supplies at runtime. The
script declares the name (and optionally a type) but does **not** define a value.

```urd
extern player_name
extern difficulty: int
```

See [Extern Values](./extern-values.md) for integration details.

---

## Runtime Types

Every value in Urd carries one of the following runtime types.

### `null`

The absence of a value. Produced by expressions that have no meaningful result.

```urd
let nothing = null
```

### `bool`

Boolean truth values.

```urd
let alive = true
let fled = false
```

### `int` (i64)

64-bit signed integers. All whole-number literals produce `int` values.

```urd
let score = 42
let negative = -7
let big = 9_999_999
```

### `float` (f64)

Double-precision floating-point numbers. Any numeric literal containing a decimal
point is a `float`.

```urd
let pi = 3.14159
let half = 0.5
```

### `str`

Strings, delimited by double quotes. Strings support interpolation with `{expression}`
syntax — see [Strings & Interpolation](./strings.md).

```urd
let greeting = "Hello, world!"
let message = "You have {gold} gold"
```

### `roll`

The result of a dice expression — a list of individual die results. Each
element is an integer in the range `1..=sides`. Dice literals (`2d6`, `1d20`,
etc.) are **immediately evaluated** into a `Roll` the moment they appear in an
expression context. There is no separate "dice" runtime type — the `Dice`
variant exists only in the AST/IR and is converted to a `Roll` by the evaluator
when it invokes the `DiceRoller`.

```urd
let attack = 2d6    # attack holds a Roll, e.g. Roll([3, 5])
let check = 1d20    # check holds a Roll, e.g. Roll([14])
```

Roll values behave like integer lists and support list methods.

### `list`

Ordered, heterogeneous collections.

```urd
let items = ["sword", "shield", "potion"]
let mixed = [1, "two", true]
```

### `map`

Key-value maps, created with the `:{…}` literal syntax.

```urd
let config = :{ difficulty: "hard", volume: 80 }
let speaker = :{ name: "Zara", name_color: "cyan" }
```

### `range`

Integer ranges, created with `..` (exclusive end) or `..=` (inclusive end).

```urd
let r = 0..10
let inclusive = 1..=5
```

### `function`

First-class function values, created with the `fn` keyword. Functions in Urd are
pure — they run in an isolated environment containing only their parameters.

```urd
let double = fn(x: int) -> int { x * 2 }
let result = double(21)
```

### `struct`

Struct instances, created by calling a struct type as a constructor or by
assigning a map literal to a struct-typed variable.

```urd
struct Item { name: str, value: int }

let sword: Item = :{ name: "Sword", value: 10 }
```

### `extern`

Extern object references are live links to host game objects. They support field
access (dot notation and subscript), field writes (subscript-assignment), and
built-in methods like `type_name()`, `fields()`, and `cast()`. Extern objects
are always truthy and use identity equality — two references are equal only if
they point to the same underlying game object.

```urd
extern player
narrator: "{player.name} has {player.hp} HP"
player["hp"] = player.hp - 10
```

See [Extern Values](./extern-values.md) for full details.

---

For the full annotation reference, see [Type Annotations](./type-annotations.md).

---

## Quick Reference

| Keyword  | Mutable | Scope                  | Defined in script |
|----------|---------|------------------------|-------------------|
| `let`    | Yes     | Current block          | Yes               |
| `const`  | No      | Current block / global | Yes               |
| `global` | Yes     | Persists across labels | Yes               |
| `extern` | No      | Global                 | No (host provides)|