# Your First Script

This tutorial walks you through writing your first Urd script from scratch. By the end you'll have a small interactive story that demonstrates speakers, dialogue, menus, jumps, conditionals, and the `end!()` builtin.

## Creating the File

Create a new file called `hello.urd` in any directory you like:

```bash
touch hello.urd
```

Urd scripts are plain UTF-8 text files with the `.urd` extension. Open it in your editor and let's start writing.

## Defining a Speaker

Every line of dialogue in Urd needs a **speaker**. Speakers are values — struct instances that carry whatever metadata your game engine needs (name, portrait, color, voice ID, etc.). For now, we'll keep it simple:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
```

This declares a **constant** called `narrator` using an anonymous struct literal (`:{ ... }`). The fields `name` and `name_color` are just conventions — your engine can look for whatever keys it expects. Constants cannot be reassigned, which is exactly what you want for speaker definitions.

Let's add a second speaker:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const old_sage = :{ name: "Alaric",   name_color: "yellow" }
```

## The Entry Point

Every Urd script needs exactly one label marked with the `@entry` decorator. This tells the VM where execution begins:

```urd
@entry
label start {
    narrator: "Welcome, traveler."
}
```

The `@entry` decorator must appear immediately before a `label` declaration. You can name the label anything — `start`, `begin`, `main`, `chapter_one` — the name doesn't matter, only the decorator.

## Single-Line Dialogue

The most basic dialogue statement is a speaker followed by a colon and a quoted string:

```urd
narrator: "Welcome, traveler."
old_sage: "Ah, a new face. It has been some time."
```

When the VM encounters these lines, it yields a `Dialogue` event containing the speaker's metadata and the text. Your engine decides how to render it — text box, speech bubble, subtitles, whatever.

## Multi-Line Dialogue

When a speaker has several consecutive lines to deliver, wrap them in a block:

```urd
narrator: {
    "The forest path opens into a mossy clearing."
    "Sunlight filters through the canopy in golden shafts."
    "An old man sits on a fallen log, watching you approach."
}
```

Each string inside the block becomes a separate line in the `Dialogue` event. The VM yields them all as a single event — your engine can display them one at a time, all at once, or however it likes.

## Adding a Menu

Player choices are expressed with `menu` blocks. Each option has a label string and a body:

```urd
menu {
    "Greet the old man" {
        old_sage: "Well met! Sit, sit. Let me tell you about this place."
    }
    "Walk past silently" {
        narrator: "You nod and keep walking. The old man watches you go."
    }
}
```

When the VM reaches a `menu`, it yields a `Choice` event listing the options. Your engine displays them, the player picks one, and you feed the index back to the VM with `vm.next(Some(index))`. Execution continues inside the chosen branch.

## Jumping Between Labels

Labels are named sections of dialogue. The `jump` statement transfers control from one label to another — think `goto`, but for conversations:

```urd
label clearing {
    narrator: "You stand in the clearing."
    jump talk_to_sage
}

label talk_to_sage {
    old_sage: "What brings you to the Whispering Woods?"
}
```

Labels and jumps are how you structure branching stories. Menu options typically end with a `jump` to route the player into the next section.

## Ending the Script

Every execution path must eventually terminate. The `end!()` builtin signals the VM that the conversation is over:

```urd
label farewell {
    old_sage: "Safe travels, wanderer."
    end!()
}
```

When the VM executes `end!()`, the next call to `vm.next()` returns `VmStep::Ended`. If you forget `end!()` and a label runs out of statements without jumping anywhere, the static analysis pass will warn you about a dead-end label.

## The Complete Script

Let's put it all together into a small interactive story. This script demonstrates everything covered above — speakers, single-line and multi-line dialogue, menus, jumps, globals, conditionals, and `end!()`:

```urd
# hello.urd — A brief encounter in the Whispering Woods.

# ── Speakers ──────────────────────────────────────────────────────────────────
const narrator = :{ name: "Narrator", name_color: "white" }
const old_sage = :{ name: "Alaric",   name_color: "yellow" }

# ── State ─────────────────────────────────────────────────────────────────────
global heard_story = false

# ── Entry point ───────────────────────────────────────────────────────────────

@entry
label clearing {
    narrator: {
        "The forest path opens into a mossy clearing."
        "Sunlight filters through the canopy in golden shafts."
        "An old man sits on a fallen log, watching you approach."
    }

    jump encounter
}

label encounter {
    menu {
        "Greet the old man" {
            jump talk_to_sage
        }
        "Ask about the woods" {
            jump ask_about_woods
        }
        "Walk away" {
            jump farewell_silent
        }
    }
}

label talk_to_sage {
    old_sage: "Well met, traveler! Sit, sit."

    if heard_story {
        old_sage: "I've already told you the tale. Anything else on your mind?"
    } else {
        old_sage: {
            "These woods have stood since before the kingdom was founded."
            "They remember things that men have long forgotten."
        }
        heard_story = true
    }

    jump encounter
}

label ask_about_woods {
    old_sage: {
        "The Whispering Woods, they call them."
        "Listen closely and you can hear the trees murmuring."
        "They speak of rain, of root, and of the slow turning of seasons."
    }

    narrator: "A cool breeze stirs the branches overhead."

    jump encounter
}

label farewell_silent {
    narrator: "You nod politely and continue down the path."
    old_sage: "Hm. Another one in a hurry. They always are."

    narrator: {
        "The clearing shrinks behind you."
        "The whispering fades to silence."
    }

    end!()
}
```

## Running It

Save the file and run it with Quest:

```bash
quest run hello.urd
```

You'll see the narrator's opening lines, then a menu. Pick an option, explore the branches, and eventually walk away to end the story. Try choosing "Greet the old man" twice to see the conditional — the second time, Alaric acknowledges that he's already told you the tale.

## Key Takeaways

| Concept | Syntax | Purpose |
|---------|--------|---------|
| Speaker definition | `const name = :{ ... }` | Declare a speaker with metadata |
| Entry point | `@entry` before a label | Tell the VM where to start |
| Single-line dialogue | `speaker: "text"` | One line of dialogue |
| Multi-line dialogue | `speaker: { "line 1" "line 2" }` | Multiple lines in one event |
| Menu | `menu { "option" { ... } }` | Present player choices |
| Jump | `jump label_name` | Transfer control to another label |
| Global variable | `global name = value` | Mutable state that persists across labels |
| Conditional | `if cond { ... } else { ... }` | Branch on runtime state |
| End | `end!()` | Terminate execution |

## What's Next?

- **[Running with Quest](./running-with-quest.md)** — learn about Quest's export, localization, and pipe modes.
- **[Language Overview](../language/overview.md)** — dive deeper into the full Urd language.