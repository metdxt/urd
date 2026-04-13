# Functions

Urd supports named functions with parameters, return types, and return values.
Functions are first-class values — they can be stored in variables, passed as
arguments, and called with standard `name(args)` syntax.

---

## Defining a Function

Use the `fn` keyword followed by the function name, parameter list, optional
return type, and a body block:

```urd
fn greet(name: str) -> str {
    return "Hello, {name}!"
}
```

A more complete example:

```urd
fn clamp(value: int, low: int, high: int) -> int {
    if value < low {
        return low
    }
    if value > high {
        return high
    }
    return value
}
```

---

## Parameters

Parameters are declared inside parentheses, separated by commas. Each parameter
can have an optional type annotation:

```urd
fn add(a: int, b: int) -> int {
    return a + b
}

fn describe(item) {
    narrator: "You look at the {item}."
}
```

When type annotations are present, the compiler can perform type checking at the
call site and inside the function body.

### No Parameters

Functions with no parameters use empty parentheses:

```urd
fn roll_initiative() -> int {
    let roll = 1d20
    return roll
}
```

---

## Return Type

The return type is specified with `->` after the parameter list:

```urd
fn is_alive(health: int) -> bool {
    return health > 0
}
```

If a function has no return type annotation, it can still return a value — the
annotation is optional but recommended for clarity and compile-time checking.

---

## The `return` Statement

Use `return value` to return a value from the function:

```urd
fn max(a: int, b: int) -> int {
    if a > b {
        return a
    }
    return b
}
```

A bare `return` (with no value) returns `null` and exits the function
immediately:

```urd
fn maybe_warn(health: int) {
    if health > 25 {
        return
    }
    narrator: "Warning: health is critically low!"
}
```

---

## Calling Functions

Functions are called with the standard `name(arguments)` syntax:

```urd
let clamped = clamp(150, 0, 100)
narrator: "Your score is {clamped}."
```

Arguments are positional — they are matched to parameters in order.

### The Let-Call Pattern

A common pattern is to capture the return value in a variable:

```urd
let damage = calculate_damage(weapon, armor)
narrator: "You deal {damage} damage!"
```

### Calling Without Capturing

Urd does **not** support bare expression statements. A standalone function call
like `log_event("Player entered the dungeon")` is a parse error — every
statement must be a declaration, assignment, jump, dialogue line, or other
explicitly recognised form.

If you want to call a function for its side effects and discard the return
value, capture the result in a named variable:

```urd
fn log_event(message: str) {
    # In a real integration, this might write to a log
    narrator: "LOG: {message}"
}

let _result = log_event("Player entered the dungeon")
```

> **Why?** Urd is a dialogue scripting language — functions are pure and cannot
> mutate ambient state. A bare `add(1, 2)` would have no observable effect, so
> requiring an explicit binding keeps intent clear and prevents "do-nothing"
> lines that are almost always bugs in a narrative context.
>
> Note: bare `_` is a wildcard token in Urd, not an identifier, so
> `let _ = …` is a parse error — use a named variable like `_result` instead.

---

## Functions as First-Class Values

Functions in Urd are first-class values. You can store them in variables and pass
them to other functions:

```urd
fn double(x: int) -> int {
    return x * 2
}

fn triple(x: int) -> int {
    return x * 3
}

let transform = double
let result = transform(5)
narrator: "Result: {result}"
# Output: "Result: 10"
```

This is particularly useful with list methods like `.map()`, `.filter()`, and
`.fold()`:

```urd
fn is_positive(n: int) -> bool {
    return n > 0
}

let values = [-3, 5, -1, 8, 0, 2]
let positives = values.filter(is_positive)
# positives = [5, 8, 2]
```

---

## Practical Examples

### Damage Calculation

```urd
fn calculate_damage(base: int, armor: int, modifier: float) -> int {
    let raw = base - armor
    if raw < 0 {
        return 0
    }
    return raw * modifier
}

label combat {
    let dmg = calculate_damage(25, 10, 1.5)
    narrator: "The attack deals {dmg} damage!"
}
```

### Stat Checking

```urd
fn check_stat(stat: int, difficulty: int) -> bool {
    let roll = 1d20
    return (roll + stat) >= difficulty
}

label locked_door {
    narrator: "A heavy locked door blocks your path."

    if check_stat(strength, 15) {
        narrator: "You force the door open!"
        jump next_room
    } else {
        narrator: "The door won't budge."
    }
}
```

### Building Strings

```urd
fn format_gold(amount: int) -> str {
    if amount == 1 {
        return "1 gold coin"
    }
    return "{amount} gold coins"
}

narrator: "You have {format_gold(gold)}."
```

---

## Scope

Functions are **pure** — they execute in a fully isolated environment that
contains only their parameters. They have no access to outer-scope variables,
globals, externs, or constants at runtime. This is the purity guarantee:
function bodies cannot read or write ambient state.

```urd
global difficulty = 3

fn add_bonus(roll: int, bonus: int) -> int {
    # Only 'roll' and 'bonus' are visible here.
    # 'difficulty' is NOT accessible — it would produce an UndefinedVariable error.
    return roll + bonus
}

label check {
    let roll = 1d20
    let result = add_bonus(roll, difficulty)
    narrator: "You rolled {result}."
}
```

If a function needs external data, pass it explicitly as a parameter. This
keeps functions predictable and testable — their output depends only on their
inputs.

---

## Summary

| Feature | Syntax |
|---------|--------|
| Define a function | `fn name(params) -> ReturnType { body }` |
| Parameter with type | `param: Type` |
| Return type | `-> Type` after the parameter list |
| Return a value | `return value` |
| Return nothing | `return` (returns `null`) |
| Call a function | `name(args)` |
| Capture result | `let x = name(args)` |
| Store in variable | `let f = name` |