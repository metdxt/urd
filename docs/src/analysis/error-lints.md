# Error Lints

Error-level lints represent problems that will cause compilation failure or semantically broken scripts. When the analyzer reports an error, it **must** be fixed before the script can run correctly.

All error lints run automatically during compilation and in the LSP.

---

## `TypeMismatch`

**What it catches:** A typed declaration or assignment where the literal value's type does not match the declared type annotation.

**Why it's an error:** Urd uses type annotations to guarantee correctness at compile time. Allowing a string where an integer is expected would cause unpredictable runtime behaviour.

**Example:**

```urd
let x: int = "hello"
```

**Diagnostic:**

```text
Type mismatch for 'x': expected Int, got Str
```

The analyzer compares the declared `TypeAnnotation` against the actual value's type. Supported type annotations include `int`, `str`, `float`, and `bool`, as well as user-defined struct types.

---

## `StructMismatch`

**What it catches:** A typed declaration where the value is a map literal (`:{ ... }`) but it is missing required struct fields or has fields with the wrong type.

**Why it's an error:** Structs enforce a fixed schema. If a field is missing or mistyped, any code that accesses that field will fail at runtime.

**Example:**

```urd
struct Suspect {
    name: str
    alibi: str
    age: int
}

const bob: Suspect = :{ name: "Bob", age: "thirty" }
```

**Diagnostic:**

```text
Struct mismatch for 'bob': expected struct 'Suspect': missing field 'alibi': str; field 'age': expected Int, got Str
```

The analyzer checks every field declared in the struct definition:
- **`MissingField`** — a required field is absent from the map literal.
- **`WrongFieldType`** — a field is present but its literal value has the wrong type.

---

## `TopLevelFlow`

**What it catches:** Flow statements — assignments, dialogue lines, menu blocks, control flow — that appear outside any `label` block at the top level of a script.

**Why it's an error:** Only definitions (`let`, `const`, `global`, `extern`, `enum`, `struct`, `decorator`, `fn`, `import`, `label`) are allowed at the top level. Executable statements must live inside a label because the VM executes labels, not loose top-level code.

**Example:**

```urd
global gold = 3
gold = gold + 10

label start {
    narrator: "Hello"
    end!()
}
```

**Diagnostic:**

```text
Top-level assignment is not allowed; only definitions (let/const/global, enum, struct, decorator, import, label) may appear at the top level
```

The `gold = gold + 10` line is an assignment at the top level. Move it inside a label to fix the error.

---

## `ConstReassignment`

**What it catches:** An assignment statement that targets a `const`-declared binding.

**Why it's an error:** Constants are immutable by definition. Allowing reassignment would violate the guarantee that `const` values never change, which other parts of the script (and the optimizer) may rely on.

**Example:**

```urd
const MAX_HEALTH = 100

@entry
label start {
    MAX_HEALTH = 200
    end!()
}
```

**Diagnostic:**

```text
Constant reassignment: 'MAX_HEALTH' is declared as `const` and cannot be reassigned
```

If the value needs to change, declare it with `global` instead of `const`.

---

## `EmptyMenu`

**What it catches:** A `menu` block that contains zero options.

**Why it's an error:** An empty menu means the player can never make a choice and the VM cannot proceed past the choice event. This is a guaranteed deadlock.

**Example:**

```urd
@entry
label stuck {
    narrator: "What do you do?"
    menu { }
    end!()
}
```

**Diagnostic:**

```text
Empty menu: the player can never make a choice (menu has no options)
```

Add at least one option to the menu, or remove the menu entirely.

---

## `MultipleMenuDefaults`

**What it catches:** A `menu` block that contains more than one `_` (wildcard/default) option.

**Why it's an error:** A menu can have at most one fallback path. Multiple `_` options create an ambiguous default — the VM would not know which branch to follow when the host passes `None`.

**Example:**

```urd
@entry
label ambush {
    narrator: "What do you do?"
    menu {
        "Fight" { jump combat }
        _ { narrator: "Too slow!" }
        _ { narrator: "Way too slow!" }
    }
    end!()
}
```

**Diagnostic:**

```text
Multiple default options: a menu can have at most one _ wildcard option
```

Remove one of the `_` arms so the menu has a single, unambiguous fallback.

> **Note:** A menu that contains *only* a `_` option and no real string options still triggers `EmptyMenu`, because the player would see zero choices. The default option is invisible to the player — it exists only as a fallback for the host to invoke programmatically.

---

## `NonExhaustiveMatch`

**What it catches:** A `match` expression over an enum that does not cover all variants and has no wildcard (`_`) arm.

**Why it's an error:** If the scrutinee holds a variant that no arm matches, the VM has no code path to follow. This is a runtime crash waiting to happen.

**Example:**

```urd
enum Status { Free, Arrested, Detained }

global suspect_status: Status = Status.Free

@entry
label check {
    match suspect_status {
        Status.Free {
            narrator: "The suspect walks free."
        }
        Status.Arrested {
            narrator: "The suspect is in custody."
        }
    }
    end!()
}
```

**Diagnostic:**

```text
Non-exhaustive match on enum 'Status': missing variants: Detained
```

Either add an arm for `Status.Detained` or add a wildcard arm:

```urd
match suspect_status {
    Status.Free     { narrator: "Free." }
    Status.Arrested { narrator: "In custody." }
    _               { narrator: "Status unknown." }
}
```

---

## `NonExhaustiveDiceMatch`

**What it catches:** A `match` over a dice expression where some achievable sums are not covered by any arm and there is no wildcard (`_`) fallback.

**Why it's an error:** Unlike enum matches, dice matches operate on numeric sums. If the player rolls a value that no arm handles, the VM has no code path to follow — identical to `NonExhaustiveMatch` but for dice expressions.

**Example:**

```urd
@entry
label roll {
    match 1d6 {
        1..3 {
            narrator: "Low roll."
        }
    }
    end!()
}
```

**Diagnostic:**

```text
Non-exhaustive dice match: missing sums 4, 5, 6
```

Either add arms to cover the remaining sums or add a wildcard arm:

```urd
match 1d6 {
    1..3 { narrator: "Low roll." }
    _    { narrator: "High roll." }
}
```

---

## `DiceMatchRequiresWildcard`

**What it catches:** A dice `match` that uses array patterns but has no `_` wildcard arm. Array-pattern exhaustiveness cannot be verified automatically, so a wildcard fallback is mandatory.

**Why it's an error:** When match arms use array patterns (matching individual die faces rather than sums), the combinatorial space is too large for the analyzer to prove coverage. Requiring a wildcard guarantees the VM always has a code path.

**Example:**

```urd
@entry
label roll {
    match 2d6 {
        [1, 1] {
            narrator: "Snake eyes!"
        }
        [6, 6] {
            narrator: "Boxcars!"
        }
    }
    end!()
}
```

**Diagnostic:**

```text
Dice match requires a wildcard '_' arm: array pattern exhaustiveness cannot be verified automatically
```

Add a wildcard arm:

```urd
match 2d6 {
    [1, 1] { narrator: "Snake eyes!" }
    [6, 6] { narrator: "Boxcars!" }
    _      { narrator: "Ordinary roll." }
}
```

---

## `DeadEnd`

**What it catches:** A label or execution path that has no terminator — no `jump`, `end!()`, `todo!()`, or `return` statement to transfer or end control flow.

**Why it's an error:** Without a terminator, the VM reaches the end of a label's block and has nowhere to go. This silently drops the player out of the dialogue with no event, which is almost certainly a bug.

**Example:**

```urd
@entry
label greeting {
    narrator: "Hello, traveler."
    narrator: "Welcome to the village."
}
```

**Diagnostic:**

```text
Dead end in label 'greeting': execution path has no terminator (use `end!`, `todo!`, `return`, or `jump`)
```

Add a terminator:

```urd
@entry
label greeting {
    narrator: "Hello, traveler."
    narrator: "Welcome to the village."
    end!()
}
```

> **Note:** The `DeadEnd` check is path-sensitive. If all branches of an `if`/`else` terminate but the code after the `if` does not, the analyzer still reports the dead end for the fallthrough path.

---

## `FluentDecorator`

**What it catches:** Invalid usage of the `@fluent` decorator — applied to a node type that doesn't support it, or with malformed arguments.

**Why it's an error:** The `@fluent` decorator injects a variable into the Fluent localization context. It only makes sense on `global` declarations (and scoped `let` bindings within labels). Applying it to other constructs (e.g., a struct definition or a label) has no defined behaviour and indicates a misunderstanding of the localization system.

**Example:**

```urd
@fluent
struct Character {
    name: str
}
```

**Diagnostic:**

```text
@fluent has no effect on `struct` nodes
```

Move `@fluent` to a `global` or `let` declaration:

```urd
@fluent
global player_name = "Hero"
```

The `@fluent("alias")` form is also validated — the argument must be a single string literal that serves as the FTL variable name.

---

## `IdDecorator`

**What it catches:** Invalid usage of the `@id` decorator — malformed arguments, duplicate `@id` values within the same scope, or `@id` applied to unsupported node types.

**Why it's an error:** The `@id` decorator assigns an explicit localization key to a dialogue or menu node. A malformed or duplicate ID would break the localization pipeline, producing ambiguous or missing translations.

**Example — malformed argument:**

```urd
@entry
label shop {
    @id
    narrator: "Welcome to the shop."
    end!()
}
```

**Diagnostic:**

```text
invalid @id decorator: @id requires exactly one string argument
```

**Example — duplicate ID:**

```urd
@entry
label shop {
    @id("greeting")
    narrator: "Welcome to the shop."

    @id("greeting")
    narrator: "What would you like to buy?"
    end!()
}
```

**Diagnostic:**

```text
duplicate @id value `greeting` in the same scope
```

Each `@id` value must be unique within its scope.

---

## `UndefinedVariable`

**What it catches:** A variable that is read (used in an expression, interpolation, or assignment target) before it has been declared in any visible scope.

**Why it's an error:** Using an undefined variable would produce a runtime error. The static analysis catches this at compile time and, when possible, suggests a similarly-named variable using Levenshtein distance.

**Example:**

```urd
@entry
label start {
    let player_health = 100
    narrator: "Your health is {playr_health}."
    end!()
}
```

**Diagnostic:**

```text
Undefined variable 'playr_health': not declared in any visible scope — did you mean 'player_health'?
```

The analyzer walks the scope stack (globals, label-local `let` bindings, function parameters) to verify that every referenced name has a visible declaration. When no exact match is found, it computes edit distance against all known names and suggests the closest match if the distance is ≤ 2.

---

## Summary Table

| Lint | Severity | Description |
|------|----------|-------------|
| `TypeMismatch` | Error | Wrong primitive type in a typed declaration |
| `StructMismatch` | Error | Missing or wrongly-typed struct fields |
| `TopLevelFlow` | Error | Flow statement outside any label |
| `ConstReassignment` | Error | Assigning to a `const` binding |
| `EmptyMenu` | Error | Menu block with zero options |
| `MultipleMenuDefaults` | Error | More than one `_` wildcard option in a menu |
| `NonExhaustiveMatch` | Error | Match missing enum variants |
| `NonExhaustiveDiceMatch` | Error | Dice match missing achievable sums |
| `DiceMatchRequiresWildcard` | Error | Dice match with array patterns needs a `_` arm |
| `DeadEnd` | Error | Label/path with no terminator |
| `FluentDecorator` | Error | Invalid `@fluent` usage |
| `IdDecorator` | Error | Invalid `@id` usage |
| `UndefinedVariable` | Error | Variable used before definition |