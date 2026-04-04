# Type Annotations

Urd is dynamically typed — variables can hold any value at runtime. However, you
can add **optional type annotations** to variable declarations. When an
annotation is present the compiler checks assignments at compile time and reports
mismatches before your script ever runs.

## Syntax

Place a colon and a type name between the variable name and the `=` sign:

```urd
let x: int = 42
const name: str = "Elara"
global health: int = 100
```

The annotation is always optional. These two lines are equivalent at runtime:

```urd
let x = 42
let x: int = 42
```

The difference is that the second form tells the compiler *"this variable must
always hold an `int`"*, enabling earlier error detection.

## Available Types

The following built-in type names are recognised in annotations:

| Annotation | Runtime type            | Example value              |
|------------|-------------------------|----------------------------|
| `int`      | 64-bit signed integer   | `42`, `-7`                 |
| `float`    | 64-bit floating point   | `3.14`, `-0.5`             |
| `bool`     | Boolean                 | `true`, `false`            |
| `str`      | String                  | `"hello"`                  |
| `null`     | Null                    | `null`                     |
| `list`     | Ordered list            | `[1, 2, 3]`               |
| `map`      | Key-value map           | `:{name: "Ada"}`           |
| `dice`     | Dice expression (evaluates to `roll` at runtime) | `2d6` |
| `range`    | Integer range           | `0..10`, `1..=5`           |

```urd
let damage: int = 15
let ratio: float = 0.75
let alive: bool = true
let greeting: str = "Hello, traveler"
let items: list = ["sword", "shield"]
let stats: map = :{str: 10, dex: 14}
let attack: dice = 2d6   # at runtime, holds a Roll value (e.g. [3, 5])
let span: range = 1..=10
```

## Named Types — Enums

When you define an `enum`, its name becomes a valid type annotation. This lets
the compiler verify that only variants of that enum are assigned to the variable:

```urd
enum Faction {
    Rebel
    Imperial
    Neutral
}

let faction: Faction = Faction.Rebel
```

If you later try to assign an incompatible value, the compiler emits a
`TypeMismatch` diagnostic:

```urd
faction = 42   # ✗ TypeMismatch: expected Faction, got int
```

## Named Types — Structs

Struct names also work as type annotations. When a struct-typed variable is
assigned a map literal, the compiler checks that the map's keys match the
struct's declared fields:

```urd
struct Item {
    name: str
    value: int
}

let loot: Item = :{name: "Sword", value: 10}
```

The compiler validates:
- **Missing fields** — every field declared in the struct must be present.
- **Extra fields** — keys that do not appear in the struct definition are
  flagged.
- **Field types** — if struct fields carry type annotations, assigned values are
  checked against them.

A mismatch produces a `StructMismatch` diagnostic:

```urd
let loot: Item = :{name: "Sword"}
# ✗ StructMismatch: missing field 'value' for struct Item

let loot: Item = :{name: "Sword", value: 10, rarity: "rare"}
# ✗ StructMismatch: unexpected field 'rarity' for struct Item
```

## Typed Constants and Globals

Annotations work on every declaration form:

```urd
const max_health: int = 100
global gold: int = 0
extern difficulty: int
```

For `extern` declarations this is especially useful because the compiler has no
initialiser to infer the type from — the annotation is the only source of type
information.

## Typed Function Parameters

Function parameters can also carry annotations:

```urd
fn clamp(value: int, lo: int, hi: int) -> int {
    if value < lo { return lo }
    if value > hi { return hi }
    return value
}
```

## Compile-Time Diagnostics

Two diagnostics are directly related to type annotations:

### `TypeMismatch`

Raised when a value is assigned to a variable whose declared type is
incompatible:

```
error: Type mismatch for variable 'health'
   expected int, got str
   --> cave.urd:12:5
```

### `StructMismatch`

Raised when a map literal assigned to a struct-typed variable has field errors
(missing fields, extra fields, or field-type mismatches):

```
error: Struct mismatch for variable 'loot' (struct Item)
   missing field: 'value'
   --> items.urd:8:5
```

## When to Use Annotations

Type annotations are never required, but they shine in a few scenarios:

- **Public interfaces** — `extern` values and function signatures benefit from
  explicit types so that both the script author and the host engine agree on what
  to expect.
- **Struct-shaped data** — annotating a variable with a struct type gives you
  compile-time field validation for free.
- **Long-lived state** — `global` variables that persist across labels are less
  likely to be accidentally overwritten with the wrong type when annotated.
- **Team projects** — annotations serve as lightweight documentation for other
  authors reading your scripts.

> **Note on `dice` annotations:** The `dice` type annotation is accepted by the
> compiler, but dice literals (e.g. `2d6`) are **immediately evaluated** into a
> `roll` value (a list of individual die results) the moment they are encountered
> at runtime. There is no separate "dice" runtime type — `Dice` exists only in
> the AST/IR as an intermediate representation. A variable declared as
> `let attack: dice = 2d6` will hold a `Roll` value such as `[3, 5]` at runtime,
> not a `Dice(2, 6)` descriptor.

When in doubt, start without annotations and add them where the compiler's
help would save you debugging time.