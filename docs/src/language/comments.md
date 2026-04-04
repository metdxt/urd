# Comments & Documentation

Urd supports line comments for developer notes and doc comments for structured
documentation that tooling can consume. There are no block comments.

---

## Line Comments

Line comments begin with `#` and extend to the end of the line:

```urd
# This is a comment
narrator: "Hello, traveler."  # Inline comment after a statement
```

Comments are ignored by the compiler and have no effect on script execution.
Use them to explain intent, leave notes for collaborators, or temporarily
disable lines of code:

```urd
# TODO: add a branching path here
label placeholder {
    narrator: "Nothing to see yet."
    # jump secret_room
    end!()
}
```

---

## Doc Comments

Doc comments begin with `##` (double hash) and are attached to the AST node
that immediately follows them:

```urd
## The main entry point for the cave exploration sequence.
## The player arrives here after leaving the village.
@entry
label cave_entrance {
    narrator: "You stand before the yawning cave mouth."
}
```

```urd
## Calculates effective damage after armor reduction.
## Returns 0 if armor exceeds the raw damage value.
fn effective_damage(raw: int, armor: int) -> int {
    if raw < armor {
        return 0
    }
    return raw - armor
}
```

### What Doc Comments Attach To

Doc comments associate with the **next** declaration or statement in the source.
They can be placed above:

- Labels
- Functions
- Enums and enum variants
- Structs and struct fields
- Constants, globals, and variable declarations
- Extern declarations

```urd
## The factions available in the game world.
enum Faction {
    ## The mercantile Guild — neutral traders and craftspeople.
    Guild
    ## The ruling Empire — law, order, and taxation.
    Empire
    ## The underground Rebel movement — freedom fighters or terrorists,
    ## depending on who you ask.
    Rebel
}
```

### Storage and Access

Doc comments are preserved by the parser and stored in the `doc_comment` field
of the corresponding AST node. This means they are available to:

- **The Language Server (LSP)** — hover over a label, function, or type to see
  its doc comment rendered as documentation.
- **Documentation generators** — tooling can extract doc comments to produce
  reference material for your scripts.
- **Static analysis** — future lint passes could warn about undocumented public
  declarations.

```urd
## The village elder who gives the player their first quest.
## Used as a speaker in Act 1 dialogue sequences.
const elder: Speaker = :{ name: "Elder Mira", name_color: "gold" }
```

Hovering over `elder` in an editor with the Urd LSP active will display the doc
comment as hover documentation.

### Multi-line Doc Comments

Each `##` line contributes to a single doc comment block. Consecutive `##` lines
are joined together:

```urd
## This is the first line of the doc comment.
## This is the second line.
##
## A blank doc-comment line creates a paragraph break.
label documented_label {
    narrator: "Well-documented code is happy code."
}
```

---

## No Block Comments

Urd does not support block comments (e.g. `/* ... */` or similar multi-line
delimiters). If you need to comment out a large section of code, prefix each
line with `#`:

```urd
# label disabled_scene {
#     narrator: "This scene is temporarily disabled."
#     jump somewhere
# }
```

> **Tip:** Most editors with Urd support can toggle line comments on a selection
> with a keyboard shortcut (e.g. `Ctrl+/` or `Cmd+/`), making it quick to
> comment out multiple lines.

---

## Conventions

### Use `#` for Implementation Notes

```urd
label combat {
    # Roll initiative before the first round
    let init = 1d20
    narrator: "Combat begins!"
}
```

### Use `##` for Public-Facing Documentation

```urd
## Rolls for initiative and returns the modified result.
## Adds the character's dexterity modifier to the roll.
fn roll_initiative(dex_mod: int) -> int {
    let roll = 1d20
    return roll + dex_mod
}
```

### Comment Sparingly Inside Dialogue

Avoid cluttering dialogue-heavy sections with comments. Let the dialogue speak
for itself and reserve comments for non-obvious logic:

```urd
label tavern {
    barkeep: "What'll it be?"

    menu {
        "Order ale (2 gold)" {
            # Deduct cost before serving — prevents negative gold exploits
            gold = gold - 2
            barkeep: "Here you go."
        }
        "Leave" {
            jump town_square
        }
    }
}
```

---

## Quick Reference

| Syntax | Purpose | Preserved in AST |
|--------|---------|------------------|
| `# text` | Line comment — developer notes | No |
| `## text` | Doc comment — attached to the next node | Yes (`ast.doc_comment`) |