# Loop Detection

Loop detection is an **opt-in** static analysis pass that identifies infinite dialogue loops — label cycles where the player can never escape.

## Why Opt-In?

Loop detection is the most expensive analysis pass in Urd. It compiles the AST to IR, builds a label-to-label jump graph, runs Kosaraju's strongly connected component (SCC) algorithm, and then performs escape analysis on each cyclic SCC. For most scripts this cost is negligible, but for large projects with hundreds of labels it can add up.

Because of this, the pass **only runs on labels explicitly decorated with `@lint(check_loops)`**. If no label in the project carries the decorator, the pass returns immediately with zero cost.

## Enabling Loop Detection

Add `@lint(check_loops)` to any label you want checked:

```urd
@lint(check_loops)
label dangerous {
    narrator: "This loops forever."
    jump dangerous
}
```

The compiler will emit:

```text
warning: Infinite dialogue loop: label 'dangerous' is part of a cycle with no
         escaping path to a terminator
```

## What Counts as a Loop?

A diagnostic is emitted for an opted-in label when **either** of these conditions is true:

1. **Trapped cycle** — the label belongs to a strongly connected component (a set of labels that can all reach each other via jumps), and no path from any label in that component can reach a terminator (`end!`, `todo!`, `return`) or a label outside the component.

2. **Feeder into a trapped cycle** — the label is not itself part of a cycle, but every path from it leads exclusively into a trapped cycle. The label is a "funnel" into a black hole.

## What Is NOT a Loop?

The pass is conservative. These patterns are **not** reported:

### Cycles with menu escapes

If any label in the cycle contains a `menu` with an option that jumps outside the cycle, the entire cycle is considered to have an escape path:

```urd
@lint(check_loops)
label hub {
    narrator: "You are in the village square."
    menu {
        "Visit the shop" { jump shop }
        "Leave"          { jump farewell }  # ← escape path
    }
}

label shop {
    narrator: "The shopkeeper waves."
    jump hub   # Back to the cycle — but 'farewell' is reachable
}

label farewell {
    narrator: "Goodbye."
    end!()
}
```

No warning — the player can always choose "Leave" to break out.

### Cycles with `return` or `end!`

If any label in the cycle contains a `return` or `end!()`, the cycle has a terminator and is not trapped:

```urd
@lint(check_loops)
label recursive_talk {
    narrator: "The philosopher drones on."
    menu {
        "Keep listening" { jump recursive_talk }
        "Stop"           { return }
    }
}
```

### `jump and return` (subroutine calls)

A `jump X and return` is treated conservatively as an escape — the control flow returns to the caller after the subroutine completes, so the cycle is not sealed.

## Multi-Label Cycles

You can opt in multiple labels individually. Only opted-in labels produce diagnostics, even if they share a cycle with non-opted-in labels:

```urd
@lint(check_loops)
label a {
    narrator: "A speaks."
    jump b
}

label b {
    narrator: "B speaks."
    jump a
}
```

Here only `a` is opted-in, so only `a` gets the warning — even though `b` is equally trapped. If you want both flagged, decorate both.

## A Complete Example

```urd
# A dialogue that traps the player in an inescapable loop.

const narrator = :{ name: "Narrator", name_color: "white" }
const ghost    = :{ name: "Ghost",    name_color: "grey"  }

@entry
label entrance {
    narrator: "You enter the haunted corridor."
    jump loop_start
}

@lint(check_loops)
label loop_start {
    ghost: "You cannot leave."
    jump loop_middle
}

@lint(check_loops)
label loop_middle {
    ghost: "There is no escape."
    narrator: "The walls close in."
    jump loop_start   # ← back to loop_start, forming a cycle
}
```

Running analysis produces two warnings:

```text
warning: Infinite dialogue loop: label 'loop_start' is part of a cycle with no
         escaping path to a terminator
warning: Infinite dialogue loop: label 'loop_middle' is part of a cycle with no
         escaping path to a terminator
```

### Fixing It

Add an escape hatch — a menu, a conditional `end!`, or a `return`:

```urd
@lint(check_loops)
label loop_start {
    ghost: "You cannot leave."
    menu {
        "Struggle"    { jump loop_middle }
        "Accept fate" { end!() }           # ← escape path added
    }
}
```

Now neither label produces a warning.

## How It Works (Implementation Details)

The pass is **hybrid** — it uses both the AST and the compiled IR:

1. **AST phase**: Scan the AST for labels decorated with `@lint(check_loops)` and collect their names and source spans.

2. **IR compilation**: Compile the AST to an `IrGraph`. If compilation fails, the pass returns no diagnostics (the compiler errors already cover the underlying issues).

3. **Label jump graph**: Project the full IR control-flow graph down to a label-to-label directed graph, where each edge represents a `jump` from one label to another.

4. **SCC detection**: Run Kosaraju's algorithm (via `petgraph`) to find all strongly connected components in the label graph.

5. **Escape analysis**: For each cyclic SCC, walk the IR nodes of every label in the component. If any node is a terminator (`end!`, `todo!`, `return`) or a jump to a label outside the SCC, the component has an escape path and is not trapped.

6. **Feeder analysis**: For labels that are opted-in but not part of any cycle, check whether all forward paths lead exclusively into trapped SCCs.

7. **Emit diagnostics**: Report `InfiniteDialogueLoop` for every opted-in label that is either trapped or a feeder into a trap.

## Best Practices

- **Use on critical paths.** Decorate labels that form the spine of your dialogue — hubs, quest loops, conversation trees — where an accidental infinite loop would softlock the game.

- **Don't blanket-decorate everything.** The pass is designed to be surgical. Decorating every label in a 500-label project is wasteful and noisy.

- **Combine with `end!` / `todo!` discipline.** The [DeadEnd](./error-lints.md) lint already ensures every label has a terminator. Loop detection goes further by checking that terminators are actually *reachable* through the cycle.

- **Test in CI.** Run `quest run` or compile your scripts in your CI pipeline. Loop detection runs automatically when any label carries `@lint(check_loops)`.