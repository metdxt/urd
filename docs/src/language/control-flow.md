# Control Flow

Urd provides two primary constructs for branching logic within your scripts: conditional branching with `if`/`elif`/`else`, and pattern matching with `match`.

Both constructs can appear inside labels, menu option bodies, function bodies, or anywhere a statement is expected.

---

## Conditional Branching

Use `if`, `elif`, and `else` for straightforward boolean conditions:

```urd
if gold >= 100 {
    narrator: "You can afford the sword."
} elif gold >= 50 {
    narrator: "You can only afford the dagger."
} else {
    narrator: "You can't afford anything."
}
```

Conditions can be any expression that evaluates to a boolean. Urd supports `and`, `or`, and `not` as boolean operators.

See [If / Elif / Else](./if-elif-else.md) for full details and diagnostics.

---

## Pattern Matching

Use `match` when you need to branch on a value against multiple possible cases — enum variants, literal values, ranges, or dice results:

```urd
match faction {
    Faction.Guild  { narrator: "The Guild welcomes you." }
    Faction.Empire { narrator: "For the Empire!" }
    Faction.Rebel  { narrator: "Freedom or death." }
}
```

Pattern matching supports exhaustiveness checking for enums, range patterns for numeric values, array patterns for dice rolls, and a wildcard `_` arm for catch-all cases.

See [Match](./match.md) for the full pattern matching reference.

---

## Combining with Other Constructs

Control flow constructs compose naturally with the rest of the language. You can nest conditionals inside menu options, match on dice results to determine dialogue, or use `jump` inside any branch:

```urd
label shop {
    narrator: "The merchant eyes you carefully."

    if has_torch {
        narrator: "Ah, I see you're prepared for the caves."
    }

    menu {
        "Buy sword (50 gold)" {
            if gold >= 50 {
                gold = gold - 50
                narrator: "A fine choice."
            } else {
                narrator: "You can't afford that."
            }
        }
        "Leave" {
            jump town_square
        }
    }
}
```
