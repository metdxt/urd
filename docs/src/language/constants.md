# Constants

Constants in Urd are immutable bindings declared with the `const` keyword. Once assigned, a constant's value cannot be changed for the lifetime of the script. They are the idiomatic way to define fixed configuration, character speakers, and any value that should never be reassigned.

## Declaration

```urd
const max_health = 100
const greeting = "Hello, adventurer!"
const pi = 3.14159
```

Constants follow the same `name = value` pattern as other variable forms, but the compiler enforces that no subsequent assignment targets the same name.

## Typed Constants

You can add an optional type annotation to make intent explicit and let the compiler verify correctness:

```urd
const max_health: int = 100
const title: str = "The Forgotten Cave"
const debug_mode: bool = false
```

If the right-hand side doesn't match the declared type, the compiler emits a `TypeMismatch` diagnostic at compile time.

## Character Definitions

The most common use of constants is defining dialogue speakers as map literals:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const zara     = :{ name: "Zara",     name_color: "cyan"  }
const elara    = :{ name: "Elara",    name_color: "yellow" }
```

These constants are then used as the speaker in dialogue lines:

```urd
narrator: "The wind howls across the barren moor."
zara: "Halt, traveler."
```

If you have a `struct` type for characters, you can type the constant accordingly:

```urd
struct Character { name: str, name_color: str }

const narrator: Character = :{ name: "Narrator", name_color: "white" }
```

A mismatch between the struct's declared fields and the map literal triggers a `StructMismatch` diagnostic.

## Configuration Values

Constants work well for game-balance numbers, thresholds, and other fixed data:

```urd
const base_damage: int = 10
const crit_multiplier: float = 1.5
const max_inventory_size: int = 20
const starting_gold: int = 50
```

## Reassignment Is an Error

Attempting to reassign a constant — anywhere in the script, including inside labels, `if` blocks, or `menu` options — produces a **`ConstReassignment`** lint error:

```urd
const LIMIT = 10
LIMIT = 20          # ❌ error: Constant reassignment: 'LIMIT' is declared
                    #    as `const` and cannot be reassigned
```

This check is performed during static analysis, so the error is caught before the script ever runs. It applies regardless of where the reassignment occurs:

```urd
const FLAG = true

label start {
    if some_condition {
        FLAG = false    # ❌ still an error — const is const everywhere
    }
}
```

## Constants vs. Globals

| Feature | `const` | `global` |
|---|---|---|
| Mutable | No | Yes |
| Persists across labels | Yes (immutable, always available) | Yes (mutable, always available) |
| Typical use | Speakers, config, fixed data | Game state, counters, flags |
| Reassignment | Compile-time error | Allowed |

If you need a value that changes over time — gold, health, quest flags — use [`global`](./globals.md) instead.

## Best Practices

- **Name characters as constants.** This keeps speaker definitions in one place and makes refactoring easy.
- **Use constants for magic numbers.** Instead of scattering `30` throughout your script, define `const potion_price = 30` and reference it by name.
- **Prefer `const` over `let` when the value won't change.** The compiler enforces immutability for you, preventing accidental bugs.
- **Group constants at the top of your script** (before labels) for readability. This is a convention, not a requirement — constants are valid anywhere at the top level.