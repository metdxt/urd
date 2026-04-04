# Menus & Choices

Menus are how you give the player agency. A `menu` block presents a set of
options; the VM suspends execution, yields an `Event::Choice` to the host
engine, and waits for the player to pick one.

---

## Basic Syntax

```urd
menu {
    "Option text" {
        # body executed when the player picks this option
    }
    "Another option" {
        jump somewhere
    }
}
```

Each option consists of a quoted string (the display label) followed by a block
of statements that execute when that option is chosen. The block can contain
dialogue, jumps, variable assignments — anything you would put inside a label.

## A Practical Example

```urd
const narrator = :{ name: "Narrator", name_color: "white" }

label crossroads {
    narrator: "A fork in the road lies ahead."

    menu {
        "Go left" {
            narrator: "You take the overgrown path to the left."
            jump forest_path
        }
        "Go right" {
            narrator: "You follow the well-worn trail to the right."
            jump village_road
        }
        "Turn back" {
            narrator: "You retreat the way you came."
            jump camp
        }
    }
}
```

---

## The Choice Event

When the VM reaches a `menu` node, it builds an `Event::Choice` and yields it
as a `VmStep::Event`. The event carries:

| Field          | Type                                | Description                                    |
|----------------|-------------------------------------|------------------------------------------------|
| `options`      | `Vec<ChoiceEvent>`                  | One entry per menu option                      |
| `fields`       | `HashMap<String, RuntimeValue>`     | Decorator fields attached to the menu itself   |
| `loc_id`       | `Option<String>`                    | Localization key for the menu as a whole        |
| `fluent_vars`  | `HashMap<String, RuntimeValue>`     | `@fluent`-tagged variables in scope            |

Each `ChoiceEvent` contains:

| Field             | Type                                | Description                                 |
|-------------------|-------------------------------------|---------------------------------------------|
| `label`           | `String`                            | The display text for this option            |
| `fields`          | `HashMap<String, RuntimeValue>`     | Decorator fields for this specific option   |
| `loc_id`          | `Option<String>`                    | Localization key for this option            |
| `fluent_vars`     | `HashMap<String, RuntimeValue>`     | Fluent variables for this option            |
| `localized_label` | `Option<String>`                    | Pre-localized text if a `Localizer` is set  |

---

## Host Integration

The host engine is responsible for displaying the options and passing the
player's selection back to the VM. The flow looks like this:

```rust
loop {
    match vm.next(None) {
        VmStep::Event(Event::Choice { options, .. }) => {
            // 1. Display the options in your game UI
            for (i, opt) in options.iter().enumerate() {
                println!("[{}] {}", i, opt.label);
            }

            // 2. Wait for the player to pick one
            let selected: usize = get_player_input();

            // 3. Pass the chosen index back to the VM
            match vm.next(Some(selected)) {
                VmStep::Event(e) => { /* handle next event */ }
                VmStep::Ended => break,
                VmStep::Error(e) => panic!("{e}"),
            }
        }
        VmStep::Event(Event::Dialogue { .. }) => {
            // handle dialogue — pass None to advance
        }
        VmStep::Ended => break,
        VmStep::Error(e) => panic!("{e}"),
    }
}
```

Key point: after an `Event::Choice`, the *next* call to `vm.next()` **must**
include `Some(index)` where `index` is the zero-based position of the chosen
option. If you pass `None`, the VM re-emits the same choice event.

---

## Default / Fallback Option

A menu can include a **wildcard option** written as `_ { ... }`. This is a
fallback branch that executes when the host passes `None` to a pending choice —
for example, when a timer expires and the player hasn't picked anything.

```urd
@timed(10.0)
menu {
    "Fight" { jump combat }
    "Flee" { jump escape }
    _ {
        narrator: "You hesitate too long..."
        jump ambush
    }
}
```

### How It Works

- The `_` option is **not** included in the `options` list on `Event::Choice`.
  The player never sees it — it is invisible to the host's UI.
- `Event::Choice` carries a `has_default: bool` field. When `true`, the host
  knows a fallback path exists and can act accordingly (e.g., start a countdown
  timer).
- The host triggers the default branch by calling `vm.next(None)` while the
  choice is pending. Instead of re-emitting the same `Choice` event (the normal
  behaviour for `None`), the VM follows the `_` arm.
- Choice indices `0..N` map only to the non-default options. The wildcard does
  not occupy an index slot.

### Rules

- **At most one `_` per menu.** Declaring two or more wildcard options is a
  compile-time error (`MultipleMenuDefaults`).
- **A menu with only `_` and no real options triggers `EmptyMenu`.** The
  wildcard alone does not count as a player-facing option — the menu still needs
  at least one selectable choice.

### Host-Side Flow

A typical integration looks like this:

1. Call `vm.next(None)` → receive `Event::Choice` with `has_default: true`.
2. Display the `options` list in the game UI.
3. Start a timer (duration comes from a decorator like `@timed`).
4. If the player picks before the timer expires → `vm.next(Some(index))`.
5. If the timer expires first → `vm.next(None)` → the VM executes the `_` body.

See [Events & Choices](../integration/events-and-choices.md) for the full Rust
integration example.

---

## Menus Inside Control Flow

Menus can appear anywhere a statement is valid — inside labels, inside `if`
branches, even nested inside other menu option bodies:

```urd
label shop {
    narrator: "Welcome to the shop."

    if gold >= 50 {
        menu {
            "Buy a sword (50g)" {
                gold = gold - 50
                narrator: "You purchase a fine blade."
            }
            "Buy a shield (30g)" {
                gold = gold - 30
                narrator: "You strap the shield to your arm."
            }
            "Leave" {
                jump town_square
            }
        }
    } else {
        narrator: "You can't afford anything here."
        jump town_square
    }
}
```

---

## Decorators on Menus

Decorators can be applied to the menu as a whole or to individual options:

```urd
@timed(10.0)
menu {
    @highlight("green")
    "Accept the quest" {
        jump quest_start
    }
    "Decline" {
        narrator: "Maybe another time."
        jump town_square
    }
}
```

Decorators on the menu populate the top-level `fields` map on `Event::Choice`.
Decorators on individual options populate the `fields` map on that option's
`ChoiceEvent`.

---

## Localization

Menu option labels are automatically assigned localization IDs derived from the
file name, label name, and option position. These IDs appear in generated `.ftl`
files and allow translators to localize every option string without touching the
`.urd` source.

You can override the generated ID with `@id("custom_key")` on an option:

```urd
menu {
    @id("shop-buy-sword")
    "Buy a sword" {
        jump buy_sword
    }
}
```

---

## Static Analysis

The compiler performs several checks on menu blocks:

### `EmptyMenu` (Error)

A menu with zero options is a compile-time error. The VM cannot present an empty
choice to the player. A menu that contains only a `_` wildcard and no real
options also triggers this error — the wildcard alone is not a player-facing
choice.

```urd
# ✗ EmptyMenu: this menu has no options
menu {
}

# ✗ EmptyMenu: wildcard alone does not count
menu {
    _ { jump fallback }
}
```

### `MultipleMenuDefaults` (Error)

A menu can have at most one `_` wildcard option. Declaring two or more is a
compile-time error — the VM would have no way to decide which fallback to take.

```urd
# ✗ MultipleMenuDefaults
menu {
    "Accept" { jump quest_start }
    _ { narrator: "Too slow." }
    _ { narrator: "Way too slow." }
}
```

**Diagnostic:**

```
Multiple default options: a menu can have at most one _ wildcard option
```

### `SingleOptionMenu` (Warning)

A menu with exactly one option is technically valid but almost certainly a
mistake — there is no real choice for the player:

```urd
# ⚠ SingleOptionMenu: menu has only one option
menu {
    "Continue" {
        jump next_scene
    }
}
```

### `DuplicateMenuDestination` (Warning)

If two options in the same menu have identical bodies (same jump target, same
dialogue, etc.), the compiler warns that they are likely copy-paste errors:

```urd
# ⚠ DuplicateMenuDestination
menu {
    "Go north" {
        jump village
    }
    "Head to the village" {
        jump village
    }
}
```

---

## Tips

- **Always terminate option bodies.** Each option body should end with a `jump`,
  `end!()`, or fall through to subsequent dialogue. A body that reaches the end
  of its block without a terminator triggers a `DeadEnd` warning.

- **Keep menus focused.** A menu with more than five or six options becomes hard
  for players to parse. Consider splitting large menus across multiple labels or
  using conditionals to show only relevant options.

- **Use decorators for UI hints.** Need to gray out an option, highlight it, or
  attach a tooltip? Decorators let you push arbitrary metadata to the host
  engine without polluting the dialogue logic.