# Multi-line Dialogue

When a character needs to deliver several lines in sequence — narration, monologues, exposition — Urd provides a **block dialogue** syntax that keeps everything grouped under a single speaker.

## Block Syntax

Wrap multiple string literals in braces after the speaker:

```urd
narrator: {
    "The wind howls across the barren moor."
    "Before you yawns the entrance to an ancient cave,"
    "its dark mouth breathing cold air like a sleeping beast."
}
```

Each quoted string becomes a separate entry in the `lines` Vec of the resulting `Event::Dialogue`. The VM emits **one** dialogue event containing all the lines, not one event per line.

## How It Differs from Single-line Dialogue

| Form | Syntax | Events emitted |
|------|--------|----------------|
| Single-line | `speaker: "text"` | 1 event, 1 line |
| Multi-line block | `speaker: { "line 1" "line 2" }` | 1 event, N lines |
| Separate statements | `speaker: "line 1"` then `speaker: "line 2"` | 2 events, 1 line each |

The block form groups lines into a single event, which is important for your game engine's rendering logic. If you want the player to advance through each line individually, your host code should iterate over the `lines` Vec and present them one at a time.

Separate single-line statements produce independent events, meaning the VM yields control back to the host between each one.

## String Interpolation in Blocks

Each line in a block is an independent string literal, so interpolation works on a per-line basis:

```urd
global gold = 42
global health = 80

narrator: {
    "You emerge from the cave, battered but alive."
    "Gold collected: {gold}"
    "Remaining health: {health}"
}
```

This produces one `Event::Dialogue` with three lines, each with its interpolated values resolved at the time the event fires.

## Long Narration Example

Multi-line blocks shine when you need extended narration or monologue passages:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const yaga = :{ name: "Баба-Яга", name_color: "#9370DB" }

label intro {
    narrator: {
        "Густой туман стелется по земле."
        "Кроны деревьев смыкаются над головой, скрывая небо."
        "Ты заблудился в дремучем лесу и не знаешь пути домой."
    }

    yaga: {
        "Фу-фу-фу! Русским духом пахнет!"
        "Зачем пожаловал, добрый молодец?"
    }
}
```

## Decorators on Multi-line Blocks

Decorators apply to the entire block, not to individual lines:

```urd
@slow_text
narrator: {
    "The ancient door creaks open."
    "Dust billows outward in a choking cloud."
    "Something stirs in the darkness beyond."
}
```

The decorator enriches the single `Event::Dialogue` that contains all three lines. Your host code receives the decorator's fields alongside the full `lines` Vec and can apply the effect (in this case, a slow text reveal) to each line as it renders them.

## Formatting Conventions

While the language does not enforce any particular indentation, the idiomatic style is:

- Opening brace on the same line as the speaker
- Each string literal on its own line, indented one level
- Closing brace on its own line, aligned with the speaker

```urd
# Idiomatic
narrator: {
    "Line one."
    "Line two."
}

# Also valid, but harder to read
narrator: { "Line one." "Line two." }
```

## When to Use Multi-line Blocks

Use block dialogue when:

- A character delivers multiple consecutive lines without interruption
- You want the host to receive all lines as a single logical unit
- The passage is long enough that separate single-line statements would be noisy

Use separate single-line statements when:

- Different game logic (variable changes, conditionals) needs to run between lines
- You want the VM to yield back to the host between each line
- Lines belong to different speakers