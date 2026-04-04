# Cave Adventure

The **Cave Adventure** (`examples/quest/cave.urd`) is a self-contained fantasy exploration script that demonstrates core Urd features in a single file: globals, menus, conditionals, string interpolation, and multi-label flow.

## The Story

You arrive at the entrance to an ancient, forgotten cave. A mysterious wanderer named **Zara** warns you of the danger. From there, you choose your own path:

- Take a torch or go in blind
- Search the entrance for loot and a rusty key
- Sneak past a sleeping goblin or fight it head-on
- Open a treasure chest (if you found the key)
- Survive — or fall

The script has **three endings**: a tiered victory (based on gold collected), a quiet walk-away ending, and a game-over death.

## Running It

```bash
quest run examples/quest/cave.urd
```

## Key Concepts Demonstrated

### Characters as Constants

Speakers are defined as anonymous struct constants at the top of the file:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const zara     = :{ name: "Zara",     name_color: "cyan"  }
```

These are used throughout the script as dialogue speakers. Defining them as `const` ensures they cannot be accidentally overwritten.

### Global State

Four global variables track the player's state across labels:

```urd
global gold      = 0
global health    = 100
global has_torch = false
global has_key   = false
```

Globals persist across `jump` transitions, which is what makes them suitable for game state that needs to survive label boundaries.

### The Entry Point

The `@entry` decorator on the `start` label tells the VM where to begin execution:

```urd
@entry
label start {
    narrator: {
        "The wind howls across the barren moor."
        "Before you yawns the entrance to an ancient cave,"
        "its dark mouth breathing cold air like a sleeping beast."
    }

    zara: {
        "Halt, traveler."
        "I have wandered these parts for years."
        "That cave has swallowed many who entered unprepared."
    }

    menu {
        "Enter the cave" {
            jump cave_entrance
        }
        "Take a torch from Zara" {
            has_torch = true
            zara: {
                "Here — take this torch."
                "May it keep the shadows at bay."
                "And the goblins… well, that is another matter."
            }
            jump cave_entrance
        }
        "Walk away" {
            jump road_end
        }
    }
}
```

Note how the menu option "Take a torch from Zara" sets `has_torch = true` before jumping — state changes can happen inside menu option blocks.

### Multi-line Dialogue

The `{ ... }` block syntax after a speaker delivers multiple lines in sequence as a single dialogue event:

```urd
narrator: {
    "Your torch blazes to life, pushing back the darkness."
    "Rough carvings cover the walls — warnings, or perhaps prayers."
}
```

Each string becomes a separate element in the `lines` vector of the `Dialogue` event.

### Conditionals Driving Narrative

The torch changes the game in two places. At the cave entrance, it alters the atmosphere:

```urd
if has_torch {
    narrator: {
        "Your torch blazes to life, pushing back the darkness."
        "Rough carvings cover the walls — warnings, or perhaps prayers."
    }
} else {
    narrator: {
        "Without a light, your eyes strain against the oppressive dark."
        "Shapes loom just beyond perception. Every sound is amplified."
    }
}
```

Later, in the goblin sneak sequence, the torch becomes a *liability* — it wakes the goblin and forces a fight:

```urd
label goblin_sneak {
    if has_torch {
        narrator: {
            "You creep forward — but the torchlight dances across the goblin's eyelids."
            "One yellow eye snaps open."
            "Then the other."
            "It lets out a shriek and lunges for its club!"
        }
        jump goblin_fight
    } else {
        narrator: {
            "Darkness is your ally."
            "Step by careful step you edge around the sleeping creature."
            "Its snoring never falters."
            "A thin sliver of pale light ahead marks a hidden exit!"
        }
        jump victory
    }
}
```

This is a deliberate design tension: taking the torch helps you *see* but hurts you in the stealth path. The player's earlier choice has downstream consequences.

### String Interpolation

Variables are interpolated directly into dialogue strings using `{variable}` syntax:

```urd
gold = gold + 10
narrator: "You found {gold} gold and a rusty key!"
```

```urd
health = health - 30
narrator: "Your health is now {health}."
```

The VM evaluates the expression at runtime and substitutes the current value into the string.

### The Treasure Chest — Key-Gated Content

The `open_chest` label uses the `has_key` global (set in `search_entrance`) to gate access to the big gold reward:

```urd
label open_chest {
    if has_key {
        narrator: "The rusty key fits! The lock pops open with a resonant clunk."
        narrator: "Inside, nestled on rotting velvet, lies a fortune in old coins."
        gold = gold + 50
        narrator: "You now carry {gold} gold!"
        jump cave_depths
    } else {
        narrator: "The chest is locked. The keyhole stares back at you, unyielding."
        narrator: "You will need a key to open it."
        jump cave_depths
    }
}
```

Both branches jump back to `cave_depths`, allowing the player to try other options.

### Deterministic Combat

The goblin fight is not random — it always costs 30 health, and the outcome depends on whether the player can survive the hit:

```urd
health = health - 30

if health > 0 {
    narrator: {
        "Gritting your teeth, you drive the goblin back."
        "One final blow sends it crashing to the ground."
    }
    jump victory
} else {
    narrator: {
        "Your legs buckle."
        "The club strikes again and the cavern spins into black."
    }
    jump game_over
}
```

Since the player starts with 100 health and the goblin deals 30 damage, the first fight is always survivable. But if you took damage from another source (a future expansion), this threshold could matter.

### Tiered Victory Ending

The victory label uses `elif` chains to deliver a different closing message based on total gold collected:

```urd
label victory {
    narrator: "You escaped the Forgotten Cave with {gold} gold!"

    if gold >= 60 {
        narrator: "A fortune and your life — few can claim the same."
    } elif gold >= 10 {
        narrator: "Not empty-handed, and still breathing."
    } else {
        narrator: "Your pockets are light, but your heart is not."
    }

    narrator: "Your remaining health: {health}."
    jump _end
}
```

### The Terminal Label

All paths eventually converge on the `_end` label, which calls `end!()` to terminate execution:

```urd
label _end {
    end!()
}
```

This is a clean pattern for scripts with multiple endings — every ending label jumps to a shared terminal, ensuring there is exactly one `end!()` in the script.

## Branching Structure

```
start
├── "Enter the cave"         → cave_entrance
├── "Take a torch from Zara" → cave_entrance (has_torch = true)
└── "Walk away"              → road_end → _end

cave_entrance
├── "Go deeper"              → cave_depths
├── "Search the entrance"    → search_entrance → cave_entrance (loop back)
└── "Leave the cave"         → road_end → _end

cave_depths
├── "Try to open the chest"  → open_chest → cave_depths (loop back)
├── "Sneak past the goblin"  → goblin_sneak
│   ├── has_torch?  → goblin_fight
│   └── !has_torch? → victory → _end
├── "Attack the goblin"      → goblin_fight
│   ├── health > 0? → victory → _end
│   └── health ≤ 0? → game_over → _end
└── "Go back"                → cave_entrance (loop back)
```

## What to Explore Next

- **[Multi-file Project](./multifile.md)** — the same concepts scaled across multiple files with imports
- **[Localized Merchant](./localized-merchant.md)** — adding translations with `@fluent`
- **[Labels & Jump](../language/labels-and-jump.md)** — deeper coverage of label mechanics
- **[Menus & Choices](../language/menus.md)** — menu syntax and patterns