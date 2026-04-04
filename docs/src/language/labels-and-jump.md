# Labels & Jump

Labels are the fundamental unit of structure in Urd. Every scene, conversation
beat, and branching path lives inside a label. The `jump` statement transfers
execution from one label to another.

---

## Defining a Label

A label is a named block of script content enclosed in braces:

```urd
label cave_entrance {
    narrator: "You stand before a gaping cave mouth."
    narrator: "Cold air seeps from the darkness within."
}
```

Labels are compiled into discrete nodes in the execution graph. The VM enters a
label, executes its contents top-to-bottom, and — unless a `jump`, `return`,
or `end!()` is encountered — falls off the end.

## Jumping Between Labels

The `jump` statement transfers control to another label:

```urd
label cave_entrance {
    narrator: "You peer into the darkness."
    jump explore_cave
}

label explore_cave {
    narrator: "You step cautiously into the cave."
}
```

`jump` is a **one-way transfer** — execution moves to the target label and does
not return. If you need call-and-return semantics, see
[Jump and Return](./jump-and-return.md).

## Label Names

Label names must be valid identifiers. Within a single module (file), label names
must be unique — the compiler rejects duplicates.

```urd
# ✓ Valid label names
label village_square { ... }
label act_2_intro { ... }

# ✗ Duplicate label name in the same file — compiler error
label village_square { ... }
```

## Labels as Jump Targets from Menus

Labels are the natural destination for menu options. Each choice typically jumps
to a label that continues that branch of the story:

```urd
label fork_in_road {
    narrator: "The path splits ahead."

    menu {
        "Take the left path" {
            jump dark_forest
        }
        "Take the right path" {
            jump river_crossing
        }
    }
}

label dark_forest {
    narrator: "Twisted trees close in around you."
}

label river_crossing {
    narrator: "A rushing river blocks your way."
}
```

## Cross-Module Jumps

In multi-file projects, labels in other files are accessed through their import
alias using dot notation:

```urd
import "tavern.urd" as tavern

label village_square {
    narrator: "You see a warm light spilling from the tavern door."
    jump tavern.enter_tavern
}
```

The alias (`tavern`) acts as a namespace prefix. Named imports bring labels
directly into scope without a prefix — see [Imports](./imports.md) for the full
import system.

## The `end!()` Terminator

When a label represents a true ending point in the script (no further
progression), use `end!()` to signal that execution should stop:

```urd
label game_over {
    narrator: "Your journey ends here."
    end!()
}
```

`end!()` tells the VM to transition to the `VmStep::Ended` state. It also
satisfies the dead-end analysis — the compiler knows this path terminates
intentionally.

> **Convention note:** You may encounter references to a `_end` label convention
> in older examples. Using `end!()` is the preferred approach — it is explicit,
> requires no separate label definition, and is understood by static analysis.

## The `todo!()` Placeholder

During development, `todo!()` marks an unfinished label. It terminates execution
(like `end!()`) but signals that the label is incomplete:

```urd
label secret_passage {
    todo!()
}
```

This satisfies the dead-end checker without requiring you to write placeholder
content.

## Dead-End Analysis

The compiler performs dead-end analysis on every label. A label whose execution
can fall off the end without encountering a valid terminator triggers a warning:

```urd
# ⚠ Warning: Dead end — execution path has no terminator
label oops {
    narrator: "This label just... stops."
}
```

Valid terminators that silence this warning are:

| Terminator | Meaning |
|------------|---------|
| `jump target` | Transfers to another label |
| `end!()` | Ends script execution |
| `todo!()` | Placeholder for unfinished content |
| `return` | Returns to a `jump ... and return` caller |

The diagnostic message reads:

> Dead end in label 'oops': execution path has no terminator (use `end!`, `todo!`, `return`, or `jump`)

This analysis catches a common mistake — forgetting to connect a label to the
rest of the script's flow.

## Static Jumps Only — No Dynamic Dispatch

It is important to understand that `jump` is **always static**. The label name
is resolved at compile time into a concrete graph edge in the IR. You cannot
jump to a label stored in a variable or computed by an expression:

```urd
# ✗ This does NOT work — jump requires a literal label name
jump target   # ERROR: 'target' is not a label name
```

`jump` always takes a literal identifier (e.g. `jump cave_entrance`) or a
qualified module path (e.g. `jump tavern.enter_tavern`), never an expression.
Labels are purely a compile-time construct — they do not exist as runtime values.
The compiler resolves each `jump` into a hard-wired graph edge during compilation;
no label lookup happens at runtime.

If you need dynamic dispatch (choosing a destination at runtime), use a
conditional or a `menu` block instead:

```urd
if condition {
    jump path_a
} else {
    jump path_b
}
```

---

## Summary

- Labels are named blocks: `label name { ... }`
- `jump name` transfers control one-way
- Cross-module: `jump alias.label_name`
- `jump` is always static — the target is a literal name resolved at compile time
- You cannot jump to a label stored in a variable
- End execution with `end!()`, mark incomplete with `todo!()`
- Labels without a terminator trigger dead-end warnings
- Labels are a compile-time construct — they are not runtime values
