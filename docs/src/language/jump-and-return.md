# Jump and Return

While a plain `jump` is a one-way transfer of control, Urd supports a
**call-and-return** pattern that lets you treat labels as subroutines. This is
the backbone of reusable dialogue sequences and modular script architecture.

---

## Basic Jump (One-Way)

A plain `jump` transfers control permanently — the VM moves to the target label
and never comes back:

```urd
label tavern {
    narrator: "You enter the tavern."
    jump tavern_conversation
    # Nothing below this line will ever execute.
}

label tavern_conversation {
    barkeep: "What'll it be?"
}
```

See [Labels & Jump](./labels-and-jump.md) for full details on basic jump
semantics.

---

## Jump and Return

Adding `and return` to a jump turns it into a **subroutine call**. Control
transfers to the target label, and when that label **finishes**, execution
resumes at the statement after the call site.

"Finishes" here means one of two things:

1. The label reaches the **end of its block** (falls off the bottom).
2. The label executes an explicit **`return`** statement.

In both cases, control is handed back to the caller. Crucially, `end!()` does
**not** count as "finishing" — it is a hard terminator that kills the entire
script regardless of call stack depth. See [end! vs return](#end-vs-return)
below for details.

```urd
label main_quest {
    narrator: "You arrive at the crossroads."
    jump side_quest and return
    narrator: "Back at the crossroads, you continue your journey."
}

label side_quest {
    narrator: "A villager asks for your help."
    narrator: "You help the villager. Task complete."
    # Control returns to main_quest automatically when the label ends.
}
```

The flow here is:

1. `main_quest` runs until `jump side_quest and return`
2. The VM pushes a return address onto the call stack
3. `side_quest` runs to completion
4. Control returns to `main_quest`, resuming after the jump

This is conceptually identical to a function call in traditional programming
languages.

---

## Capturing a Return Value

You can capture the value returned by the called label using a `let` binding:

```urd
label negotiate {
    narrator: "The merchant eyes you suspiciously."
    let discount = jump haggle and return
    narrator: "You secured a {discount}% discount."
}

label haggle {
    narrator: "You plead your case..."
    return 15
}
```

If the called label ends without an explicit `return` (or uses a bare `return`
with no value), the captured variable receives `null`.

---

## The `return` Statement

Inside any label, `return` hands control back to the caller:

```urd
label check_inventory {
    if has_key {
        return true
    }
    return false
}
```

### Bare Return

A `return` with no value returns `null` and simply resumes execution at the
call site:

```urd
label greet_npc {
    narrator: "The guard nods at you."
    return
}
```

### Return with a Value

`return value` sends a value back to the caller. The value can be any
expression — an integer, string, boolean, list, map, or any other runtime
value:

```urd
label roll_for_damage {
    let roll = 2d6
    return roll
}
```

### Return Without a Caller

If `return` is executed when the call stack is empty (i.e. the label was
reached via a plain `jump`, not `jump ... and return`), the script **ends
gracefully**. This is equivalent to reaching the end of execution — the VM
yields `VmStep::Ended`.

---

## `end!()` vs `return`

It is critical to understand the difference between these two terminators:

| Construct | Behaviour |
|-----------|-----------|
| `return` / `return value` | **Soft end.** Pops one frame from the call stack and resumes at the caller. If the call stack is empty, ends the script gracefully. |
| `end!()` | **Hard end.** Terminates the entire script immediately, regardless of how deep the call stack is. No control is returned to any caller — the VM transitions straight to `VmStep::Ended`. |

This distinction matters most inside subroutines:

```urd
label main {
    narrator: "Before the quest."
    jump side_quest and return
    narrator: "After the quest."   # ← Does this line run?
}

label side_quest {
    narrator: "Doing the quest..."
    return           # ✓ "After the quest." WILL print
    # vs.
    # end!()         # ✗ "After the quest." will NEVER print
}
```

If `side_quest` uses `return`, control flows back to `main` and the second
narrator line executes. If `side_quest` uses `end!()`, the entire script stops
immediately — `main` never resumes.

**Rule of thumb:** Use `return` when a label is designed to be called as a
subroutine. Reserve `end!()` for true terminal points (game-over screens,
final endings) where you intentionally want to halt everything.

---

## Subroutine Patterns

### Reusable Dialogue Sequences

Use `jump ... and return` to factor out dialogue that appears in multiple
contexts:

```urd
label campfire_scene {
    narrator: "You sit by the fire."
    jump tell_legend and return
    narrator: "The flames crackle as the story ends."
}

label tavern_scene {
    narrator: "The bard clears his throat."
    jump tell_legend and return
    narrator: "The crowd applauds."
}

label tell_legend {
    bard: "Long ago, in the age of dragons..."
    bard: "A hero rose from the ashes of a forgotten kingdom."
}
```

### Conditional Subroutines

Combine `jump ... and return` with conditionals to build branching logic
that converges back to a single point:

```urd
label shop {
    narrator: "Welcome to the shop."

    if gold >= 100 {
        let bought = jump buy_premium_item and return
        narrator: "You purchased: {bought}"
    } else {
        jump browse_cheap_items and return
    }

    narrator: "You leave the shop."
}
```

### Nested Calls

Labels called with `and return` can themselves call other labels the same way.
The VM maintains a call stack to track the chain:

```urd
label quest_hub {
    narrator: "The quest board has several postings."
    jump fetch_quest and return
    narrator: "Quest complete. Returning to the hub."
}

label fetch_quest {
    narrator: "You need to find three herbs."
    jump forest_search and return
    narrator: "You gathered all the herbs."
}

label forest_search {
    narrator: "You search the forest floor..."
    return
}
```

---

## Call Stack Depth Limit

The VM enforces a maximum call stack depth of **256 frames**. If a script
exceeds this limit — typically due to unbounded mutual recursion — the VM
returns a `VmError::StackOverflow`:

```rust
VmError::StackOverflow(256)
```

This is a hard error that halts execution. If you hit this limit, your script
likely has runaway recursion. Consider restructuring with plain `jump` (which
does not push a frame) or breaking the cycle with a menu or conditional exit.

> **Tip:** Plain `jump` does *not* push a call frame. Only `jump ... and return`
> (and its `let`-binding variant) grows the call stack. You can chain as many
> plain jumps as you like without risk of overflow.

---

## Quick Reference

| Syntax | Behaviour |
|--------|-----------|
| `jump label` | One-way transfer, no return |
| `jump label and return` | Call-style, resumes after target ends |
| `let x = jump label and return` | Call-style, captures return value in `x` |
| `return` | Return `null` to caller (or end script gracefully) |
| `return value` | Return `value` to caller (or end script gracefully) |
| `end!()` | Hard-terminate the entire script immediately |