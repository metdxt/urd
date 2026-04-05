# Diagnostics & Lints

The Urd language server runs all static analysis passes on every edit, providing real-time feedback as you type. Diagnostics are split into **errors** (problems that will cause incorrect behavior) and **warnings** (suspicious patterns that are likely bugs or style issues).

All diagnostics are reported with precise source spans, so your editor highlights exactly the problematic code.

---

## Errors

### TypeMismatch

A value was assigned to a variable whose declared type is incompatible.

```urd
let name: string = 42  # error: expected string, got int
```

### StructMismatch

A map literal assigned to a struct-typed variable has field errors — missing fields, extra fields, or fields with the wrong type.

```urd
struct Player { name: string, hp: int }
let p: Player = :{ name: "Ada" }  # error: missing field 'hp'
```

### TopLevelFlow

A statement that is not a definition appeared at the top level. Only `const`, `global`, `enum`, `struct`, `import`, `label`, and decorator declarations are allowed outside of labels.

```urd
jump start  # error: flow statement at top level
```

### ConstReassignment

A `const` variable was assigned a new value after its initial declaration.

```urd
const MAX_HP = 100
MAX_HP = 200  # error: cannot reassign constant
```

### EmptyMenu

A `menu` block has no options — the player can never make a choice.

```urd
menu {
    # error: empty menu
}
```

### NonExhaustiveMatch

A `match` over an enum did not cover all variants and has no wildcard arm.

```urd
enum Color { Red, Green, Blue }
let c = Color::Red

match c {
    Color::Red { ... }
    Color::Green { ... }
    # error: missing variant 'Blue'
}
```

### NonExhaustiveDiceMatch

Some achievable sums of a dice expression are not covered by any arm in a `match`.

```urd
match 1d6 {
    1..3 { ... }
    # error: missing sums 4, 5, 6
}
```

### DiceMatchRequiresWildcard

A dice `match` contains array patterns but has no `_` wildcard fallback. Array-pattern exhaustiveness cannot be verified automatically, so a wildcard arm is required.

### DeadDicePattern

A match arm pattern can never fire because its value falls entirely outside the range of achievable dice sums.

```urd
match 1d20 {
    25 { ... }      # error: dead pattern — max sum is 20
   *_ { ... }
}
```

### UndefinedLabel

A `jump` statement references a label that is not defined anywhere in the script (or imported files). If a close match exists, the diagnostic includes a suggestion.

```urd
jump statr  # error: undefined label 'statr' — did you mean 'start'?
```

### DeadEnd

An execution path reaches its end without a recognized terminator (`end!`, `todo!`, `return`, or `jump`).

```urd
label orphan {
    narrator: "This goes nowhere."
    # error: dead end — no jump or end!
}
```

### UndefinedVariable

A variable was referenced but was never declared in any visible scope.

```urd
narrator: "You have {goldd} coins."  # error: undefined variable 'goldd'
```

### InvalidFluentDecorator

The `@fluent` decorator was used incorrectly — wrong arity, wrong argument type, or applied to something other than a variable declaration.

```urd
@fluent(42)       # error: @fluent argument must be a string
global gold = 50
```

### InvalidIdDecorator

The `@id` decorator was used with the wrong arity, wrong type, or an interpolated string.

```urd
@id("{dynamic}")  # error: @id argument must be a plain string
narrator: "Hello"
```

### DuplicateId

Two nodes in the same parent scope carry the same explicit `@id` value, which would produce identical localization IDs.

```urd
@id("greeting")
narrator: "Hello!"

@id("greeting")       # error: duplicate @id 'greeting'
narrator: "Welcome!"
```

---

## Warnings

### UnreachableLabel

A labeled block can never be reached from any `jump`, `let`-call, or `@entry` decorator.

```urd
label secret {
    # warning: unreachable — no jump targets this label
    narrator: "Nobody comes here."
    end!()
}
```

### EmptyDialogue

A dialogue line has an empty string or an empty block body.

```urd
narrator: ""  # warning: empty dialogue
```

### SingleOptionMenu

A `menu` block has only one option — the player has no real choice.

```urd
menu {
    "Continue" {  # warning: single option menu
        jump next
    }
}
```

### DuplicateMenuDest

Two or more options in the same menu have structurally identical bodies, making the player's choice illusory.

```urd
menu {
    "Go left" {
        jump cave
    }
    "Go right" {    # warning: same destination as 'Go left'
        jump cave
    }
}
```

### OverwrittenAssignment

A variable was assigned a value that was immediately overwritten without being read in between — the first write has no effect.

```urd
gold = 50
gold = 100  # warning: previous assignment to 'gold' is never read
```

### UnusedVariable

A `let` or `const` variable was declared but never subsequently read anywhere in its enclosing label scope.

```urd
label start {
    let temp = 42  # warning: unused variable 'temp'
    narrator: "Hello!"
    end!()
}
```

### AlwaysDeadBranch

An `if` condition is composed entirely of `const` values and evaluates to a known constant at analysis time, making one branch permanently unreachable.

```urd
const DEBUG = false
if DEBUG {
    # warning: condition is always false — this branch is dead
    narrator: "Debug mode on."
}
```

### PossibleTypo

An identifier closely resembles a known name in the same namespace (edit distance ≤ 2), suggesting a typo.

```urd
const narrator = :{ name: "Narrator" }
narartor: "Hello!"  # warning: possible typo — did you mean 'narrator'?
```

### UndefinedVar

A variable is referenced in a context where it might not be defined, depending on runtime flow.

### InfiniteDialogueLoop

A set of labels forms a cycle with no escaping path to a terminator. This diagnostic is opt-in — it is only emitted for labels decorated with `@lint(check_loops)`.

```urd
@lint(check_loops)
label ping {
    jump pong
}

label pong {
    jump ping  # warning: infinite loop — ping → pong → ping
}
```

### IdOnUnsupportedNode

The `@id` decorator was placed on a node type that does not support localization IDs (e.g., a variable declaration). The decorator has no effect.

### FluentOnUnsupportedNode

The `@fluent` decorator was applied to a node type that is not a variable declaration. The decorator has no effect.

---

## Diagnostic Sources

In editor UI, each diagnostic is tagged with a source label so you can tell where it came from:

| Source | Meaning |
|---|---|
| `urd` | Parse errors and static analysis diagnostics |
| `urd-spell` | Spellcheck diagnostics (requires `spellcheck` feature) |

---

## Suppressing Diagnostics

Currently, Urd does not have a comment-based suppression mechanism (like `// lint:ignore`). If a diagnostic is a false positive:

- For **spellcheck** warnings: add the word to your `.urd-dict` file via the "Add to dictionary" code action
- For **analysis** warnings: restructure the code to avoid the pattern

If you believe a diagnostic is incorrect, please [file an issue](https://github.com/metdxt/urd/issues).