# Language Overview

Urd is a dialogue scripting language designed for narrative games. It compiles to a directed graph IR that is walked by a pull-based virtual machine, giving the host engine full control over execution timing.

This chapter covers the core syntax philosophy. The sections that follow dive into each feature in detail.

## Syntax Philosophy

### Braces, Not Indentation

Urd uses curly braces `{ }` to delimit blocks. Indentation is purely cosmetic — use whatever style your team prefers.

```urd
label intro {
    if has_torch {
        narrator: "The flame dances against the walls."
    } else {
        narrator: "Darkness swallows everything."
    }
}
```

### Newline-Significant

Statements are separated by newlines. There are no semicolons. Each statement occupies its own line.

```urd
let x = 10
let y = 20
narrator: "The sum is {x + y}"
```

### Comments

Line comments start with `#`. Doc comments start with `##` and attach to the declaration that follows them.

```urd
# This is a regular comment.

## The player's current gold count.
## Injected into Fluent context for localization.
@fluent
global gold = 0
```

### Dynamically Typed with Optional Annotations

Values are dynamically typed at runtime, but you can add optional type annotations that are enforced at compile time.

```urd
let name = "Zara"              # inferred as str
let health: int = 100           # explicitly typed
const pi: float = 3.14159       # type-checked at compile time
```

See [Type Annotations](./type-annotations.md) for the full story.

### First-Class Dialogue and Choices

Dialogue and player choices are language-level constructs, not library calls. This makes narrative scripts read naturally.

```urd
zara: "Halt, traveler."

menu {
    "Enter the cave" {
        jump cave_entrance
    }
    "Walk away" {
        jump road_end
    }
}
```

### Expression-Oriented Where Possible

Many constructs in Urd are expressions that produce values. Arithmetic, comparisons, logical operators, string interpolation, and function calls can all appear anywhere an expression is expected.

```urd
let damage = if has_sword { 20 } else { 5 }
let greeting = "Hello, {player.name}!"
let roll_result = 2d6
```

## What Comes Next

| Section | What You'll Learn |
|---|---|
| [Variables & Types](./variables-and-types.md) | `let`, `const`, `global`, `extern`, and every runtime type |
| [Operators](./operators.md) | Arithmetic, comparison, logical, bitwise, and precedence |
| [Strings & Interpolation](./strings.md) | String literals, escape sequences, `{expr}` interpolation |
| [Dialogue](./dialogue.md) | The core `speaker: "text"` construct |
| [Labels & Jump](./labels-and-jump.md) | Navigating the dialogue graph |
| [Menus & Choices](./menus.md) | Presenting options to the player |
| [Control Flow](./control-flow.md) | `if` / `elif` / `else`, `match` |
| [Functions](./functions.md) | Pure functions and return values |
| [Decorators](./decorators.md) | Enriching events with metadata |