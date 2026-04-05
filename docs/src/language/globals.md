# Globals

**Globals** are mutable variables that persist across label boundaries. While `let` bindings are scoped to the block or label they appear in, `global` declarations live for the entire lifetime of the VM session — making them the primary mechanism for tracking game state.

## Declaration

Use the `global` keyword at the top level of your script (outside any label):

```urd
global gold = 0
global health = 100
global has_torch = false
global player_name = "Adventurer"
```

Globals can also include type annotations:

```urd
global gold: int = 0
global player_name: str = "Adventurer"
global has_torch: bool = false
```

## Mutability

Unlike `const`, globals are fully mutable. You can reassign them from any label:

```urd
global gold = 0

@entry
label start {
    narrator: "You find a pouch of coins on the ground."
    gold = gold + 10
    narrator: "You now have {gold} gold."
    jump shop
}

label shop {
    # gold is still 10 here — it persisted across the jump
    narrator: "Welcome! You have {gold} gold to spend."
}
```

## Persistence Across Labels

This is the key difference between `global` and `let`. A `let` binding inside a label is gone once you leave that label. A `global` carries its value everywhere:

```urd
global health = 100
global has_key = false

label cave_entrance {
    has_key = true
    health = health - 20
    jump cave_depths
}

label cave_depths {
    # has_key is true, health is 80
    if has_key {
        narrator: "The rusty key fits the lock!"
    }
    narrator: "Your health: {health}"
}
```

## Common Use Cases

Globals are the right tool for any state that must survive across label transitions:

- **Resource counters**: gold, health, mana, experience
- **Boolean flags**: `has_torch`, `met_merchant`, `door_unlocked`
- **Story progression**: `chapter`, `reputation`, `days_passed`
- **Inventory tracking**: item counts, equipped weapon

```urd
global gold = 0
global health = 100
global reputation = 0
global has_torch = false
global has_key = false
global visited_cave = false
```

## The `@fluent` Decorator

Marking a global with `@fluent` makes it available as a variable in Fluent localization files. This is how you feed dynamic game state into translated strings:

```urd
@fluent
global gold = 50

@fluent
global price = 30

@fluent
global has_potion = false
```

With these declarations, translators can write Fluent messages that reference `$gold`, `$price`, and `$has_potion` — enabling pluralization, gender agreement, and other grammatical features without changing any Urd code.

```ftl
# en-US/merchant.ftl
buy-prompt = Buy it for { $price } gold? You have { $gold } gold remaining.
farewell =
    { $has_potion ->
        [true] You leave with a potion and { $gold } gold.
       *[false] You leave with { $gold } gold and your curiosity intact.
    }
```

If a global is purely internal bookkeeping (e.g. a loop counter or a flag the player never sees in dialogue), leave off `@fluent` — translators don't need it cluttering their context:

```urd
# Exposed to localization
@fluent
global gold = 50

# Internal only — no @fluent
global haggled = false
```

For a comparison of all variable kinds, see [Variables & Types — Quick Reference](./variables-and-types.md#quick-reference).

## Placement

Globals should be declared at the top level of your script, before any labels. While the compiler doesn't strictly enforce ordering, placing them at the top is the conventional style and makes your script's state easy to audit at a glance:

```urd
# ── Characters ──────────────────────────────
const narrator = :{ name: "Narrator", name_color: "white" }

# ── Global state ────────────────────────────
global gold = 0
global health = 100
global has_torch = false

# ── Labels ──────────────────────────────────
@entry
label start {
    # ...
}
```
