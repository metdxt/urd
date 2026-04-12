# Warning Lints

Warning-level diagnostics highlight code that is technically valid but almost certainly wrong, wasteful, or confusing. The compiler will still succeed, but you should treat warnings as bugs waiting to happen.

> **Tip:** The LSP surfaces warnings as squiggly underlines in your editor. Fix them as you go — don't let them pile up.

---

## `UnreachableLabel`

**What it catches:** A label that no `jump`, `let-call`, or `@entry` decorator can ever reach.

**Why it matters:** Unreachable labels are dead code. They clutter the script, confuse readers, and will never execute.

**Example:**

```urd
@entry
label start {
    narrator: "Hello, world!"
    jump ending
}

# ⚠ No jump or @entry points here
label orphan {
    narrator: "Nobody will ever hear this."
    end!()
}

label ending {
    end!()
}
```

**Diagnostic message:**

```text
Unreachable label 'orphan': no `jump`, `let-call`, or `@entry` can reach this label
```

**Suggestion:** Remove the label, or add a `jump orphan` / `@entry` so it becomes reachable.

---

## `EmptyDialogue`

**What it catches:** A dialogue line whose content is empty or whitespace-only.

**Why it matters:** An empty dialogue line produces a visible event with no text. This is almost always a mistake — it creates a blank speech bubble or log entry that confuses the player.

**Example:**

```urd
label scene {
    narrator: ""
    end!()
}
```

**Diagnostic message:**

```text
Empty dialogue for speaker 'narrator': the dialogue content is blank
```

**Suggestion:** Either add text content or remove the dialogue line entirely.

---

## `SingleOptionMenu`

**What it catches:** A `menu` block containing exactly one option.

**Why it matters:** A menu with one choice is not really a choice — the player has no agency. This usually means a second option was accidentally deleted or hasn't been written yet.

**Example:**

```urd
label tavern {
    narrator: "The barkeep watches you expectantly."

    menu {
        "Order a drink" {
            jump order_drink
        }
    }
}
```

**Diagnostic message:**

```text
Single-option menu: the player has no real choice (only one option)
```

**Suggestion:** Add at least one more option, or replace the menu with a direct `jump` if the "choice" is intentionally linear.

---

## `DuplicateMenuDest`

**What it catches:** Two menu options whose bodies are structurally identical.

**Why it matters:** If two options do the exact same thing, one of them is redundant or one was supposed to do something different. Either way, the player is being misled into thinking the options matter.

**Example:**

```urd
label crossroads {
    narrator: "Which path do you take?"

    menu {
        "Go left" {
            jump forest
        }
        "Go right" {
            jump forest
        }
    }
}
```

**Diagnostic message:**

```text
Duplicate menu destination: options 'Go left' and 'Go right' have identical bodies
```

**Suggestion:** Differentiate the option bodies (different jumps, different variable assignments, different dialogue) or merge the options into one.

---

## `OverwrittenAssignment`

**What it catches:** A variable that is assigned a value and then immediately overwritten without the first value ever being read.

**Why it matters:** The first assignment is wasted work. This often indicates a copy-paste error or a logical mistake where the wrong variable was targeted.

**Example:**

```urd
label setup {
    let gold = 100
    gold = 50        # ⚠ The value 100 was never read

    narrator: "You have {gold} gold."
    end!()
}
```

**Diagnostic message:**

```text
Overwritten assignment: 'gold' is assigned a value that is immediately overwritten without being read
```

**Suggestion:** Remove the first assignment, or check whether the variable name is correct.

---

## `UnusedVariable`

**What it catches:** A `let` or `const` variable that is declared but never read anywhere in the script.

**Why it matters:** Unused variables are dead code. They add noise and can indicate forgotten logic or typos in later references.

**Example:**

```urd
label intro {
    let debug_mode = true    # ⚠ Never read
    narrator: "Welcome to the game."
    end!()
}
```

**Diagnostic message:**

```text
Unused variable: 'debug_mode' is declared but never read
```

**Suggestion:** Use the variable or remove the declaration. If it's intentionally unused (e.g., reserved for future use), consider adding a comment.

---

## `AlwaysDeadBranch`

**What it catches:** An `if` condition that is a compile-time constant, making one branch impossible to execute.

**Why it matters:** If a condition is always `true`, the `else` branch is dead code. If always `false`, the `then` branch is dead code. Either way, the conditional is pointless and may be hiding a logic error.

**Example (always true):**

```urd
label check {
    if true {
        narrator: "This always runs."
    } else {
        narrator: "This never runs."   # ⚠ Dead branch
    }
    end!()
}
```

**Example (always false):**

```urd
label check {
    if false {
        narrator: "This never runs."   # ⚠ Dead branch
    } else {
        narrator: "This always runs."
    }
    end!()
}
```

**Diagnostic messages:**

```text
Always-dead branch: condition is always `true`, the `else` branch is never executed
```

```text
Always-dead branch: condition is always `false`, the `then` branch is never executed
```

**Suggestion:** Replace the `if` with the branch that actually executes, or fix the condition so it is actually dynamic.

---

## `PossibleTypo`

**What it catches:** An identifier (speaker, label, or variable reference) that closely resembles a known name — specifically within Levenshtein edit distance ≤ 2.

**Why it matters:** A single character typo can silently introduce a new, undefined symbol instead of referencing the one you meant. This pass catches mistakes before they become runtime errors.

**Example:**

```urd
const narrator = :{ name: "Narrator", name_color: "white" }

label intro {
    # ⚠ 'narator' is one character away from 'narrator'
    narator: "The wind howls."
    end!()
}
```

**Diagnostic message:**

```text
Possible typo: 'narator' looks like it could be the speaker 'narrator'
```

**Suggestion:** The diagnostic names the most likely intended symbol. Correct the spelling to match.

---

## `IdOnUnsupportedNode`

**What it catches:** The `@id` decorator was placed on a node type that does not support localization IDs — for example, a variable declaration or a control-flow statement.

**Why it matters:** The `@id` decorator only has meaning on dialogue lines and menu options, where it overrides the auto-generated localization key. On any other node, it silently does nothing, which likely means it was placed on the wrong line.

**Example:**

```urd
label intro {
    @id("greeting")
    let x = 42          # ⚠ @id has no effect on a variable declaration
    narrator: "Hello!"
    end!()
}
```

**Diagnostic message:**

```text
@id on unsupported node: @id has no effect here
```

**Suggestion:** Move the `@id` decorator to the dialogue line or menu option you intended to tag.

---

## `FluentOnUnsupportedNode`

**What it catches:** The `@fluent` decorator was applied to a node that is not a variable declaration (i.e., not a `global` or `let` binding).

**Why it matters:** `@fluent` injects a variable into the Fluent localization context. It only makes sense on declarations whose value should be available as a Fluent variable. Placing it on a dialogue line, menu, or other construct has no effect and indicates a misunderstanding of the localization pipeline.

**Example:**

```urd
label intro {
    @fluent("greeting")
    narrator: "Hello!"   # ⚠ @fluent has no effect on dialogue lines
    end!()
}
```

**Diagnostic message:**

```text
@fluent on unsupported node: @fluent has no effect here
```

**Suggestion:** Move the `@fluent` decorator to a `global` or `let` declaration whose value you want exposed as a Fluent variable.

---

## `DeadDicePattern`

**What it catches:** A match arm in a dice `match` whose pattern can never fire because its value falls entirely outside the range of achievable dice sums.

**Why it matters:** A dead pattern is wasted code that gives the author a false sense of coverage. It often indicates a misunderstanding of the dice range.

**Example:**

```urd
match 1d6 {
    1..6 { narrator: "Normal roll." }
    10   { narrator: "Impossible!" }   # ⚠ max sum of 1d6 is 6
    _ { narrator: "Fallback." }
}
```

**Diagnostic message:**

```text
Dead dice pattern: value 10 can never match (1d6 produces sums in range 1..=6)
```

**Suggestion:** Remove the dead arm or fix the pattern to fall within the achievable range.

---

## `Misspelling`

**What it catches:** A word in dialogue text that does not appear in the SymSpell dictionary for the detected (or configured) language.

**Why it matters:** Typos in player-facing dialogue are embarrassing and break immersion. Catching them at edit time saves a QA round-trip.

> **Note:** This warning is only emitted when the `spellcheck` Cargo feature is enabled. See [Spellcheck](../tools/lsp-spellcheck.md) for configuration details.

**Example:**

```urd
narrator: "You enter the mystirious cave."
#                        ^^^^^^^^^^^ ⚠ misspelling
```

**Diagnostic message:**

```text
Misspelling: 'mystirious' — did you mean 'mysterious'?
```

Diagnostics from the spellcheck pass appear with the source tag `urd-spell` (rather than `urd`).

**Suggestion:** Accept the "Replace with '…'" code action to fix the typo, or use "Add to dictionary" to suppress it for intentional non-standard words.

---

## `UndefinedLabel` (warning)

**What it catches:** A `jump` or `let-call` that targets a label name not defined anywhere in the script (including imports).

**Why it matters:** Jumping to a non-existent label will fail at runtime. The analysis pass flags this early and, when possible, suggests a similarly-named label that you probably meant.

> **Note:** This is a *warning* at the analysis level because the label might be defined in a module that analysis can't see. The compiler will promote it to a hard error if it still can't resolve the target during compilation.

**Example:**

```urd
label start {
    narrator: "Let's go!"
    jump endig    # ⚠ Did you mean 'ending'?
}

label ending {
    end!()
}
```

**Diagnostic message:**

```text
Undefined label 'endig' — did you mean 'ending'?
```

**Suggestion:** Fix the typo. If the label is defined in another module, make sure the import is correct and the name is properly qualified.

---

## `InfiniteDialogueLoop` (opt-in)

**What it catches:** A label that is part of a cycle where no path can escape to a terminator (`end!`, `todo!`, `return`) or a label outside the cycle.

**Why it matters:** The player gets stuck in an infinite loop of dialogue with no way out.

> **Note:** This warning only fires on labels decorated with `@lint(check_loops)`. See [Loop Detection](./loop-detection.md) for full details.

**Example:**

```urd
@lint(check_loops)
label ping {
    narrator: "Ping."
    jump pong
}

@lint(check_loops)
label pong {
    narrator: "Pong."
    jump ping
}
```

**Diagnostic message:**

```text
Infinite dialogue loop: label 'ping' is part of a cycle with no escaping path to a terminator
```

**Suggestion:** Add a `menu` with an exit option, a conditional `jump` to a label outside the cycle, or an `end!()` terminator.

---

## Summary Table

| Lint | Severity | Description |
|------|----------|-------------|
| `UnreachableLabel` | Warning | Label never targeted by any jump or `@entry` |
| `EmptyDialogue` | Warning | Dialogue with empty content |
| `SingleOptionMenu` | Warning | Menu with exactly one choice |
| `DuplicateMenuDest` | Warning | Two menu options with identical bodies |
| `OverwrittenAssignment` | Warning | Value assigned then immediately overwritten without being read |
| `UnusedVariable` | Warning | Declared but never used |
| `AlwaysDeadBranch` | Warning | Condition is a compile-time constant (always true/false) |
| `PossibleTypo` | Warning | Identifier resembles a known name (Levenshtein ≤ 2) |
| `IdOnUnsupportedNode` | Warning | `@id` on a node that doesn't support localization IDs |
| `FluentOnUnsupportedNode` | Warning | `@fluent` on a non-declaration node |
| `DeadDicePattern` | Warning | Dice match arm that can never fire |
| `Misspelling` | Warning | Misspelled word in dialogue text (requires `spellcheck` feature) |
| `UndefinedLabel` | Warning | Jump target doesn't exist (with typo suggestion) |
| `InfiniteDialogueLoop` | Warning | Opt-in cycle detection via `@lint(check_loops)` |

See also: [Error Lints](./error-lints.md) for diagnostics that indicate definitively broken code.