# Built-in Decorators

Urd ships with a small set of built-in decorators that control core language
behaviour — entry points, localization, custom IDs, and opt-in analysis passes.
These decorators are always available without imports or registration.

---

## `@entry`

Marks a label as the script's starting point. The VM begins execution at the
decorated label when the script is loaded.

```urd
@entry
label start {
    narrator: "Your adventure begins."
}
```

**Rules:**

- Exactly one `@entry` per script file. A second `@entry` produces a
  `DuplicateEntry` diagnostic.
- Can only be applied to labels.
- In multi-file projects, only the root script's `@entry` is used to start
  execution. Imported files may have their own `@entry` for standalone testing.

See [Entry Points](./entry-points.md) for full details.

---

## `@fluent`

Marks a `global` variable for automatic injection into Fluent localization
contexts. When a `Localizer` is attached to the VM, every `@fluent`-tagged
variable is passed to the Fluent resolver as a variable binding, making it
available inside `.ftl` translation strings.

### Basic Usage

```urd
@fluent
global gold = 50

@fluent
global player_name = "Traveler"
```

With these declarations, Fluent templates can reference `$gold` and
`$player_name`:

```ftl
merchant-greeting = You have { $gold } gold, { $player_name }.
```

Every time a dialogue or choice event fires, the VM snapshots all `@fluent`
variables in scope and attaches them to the event's `fluent_vars` map.

### Custom Variable Name

By default, the Fluent variable name matches the Urd variable name. If you need
a different name (e.g. to match an existing `.ftl` file or to avoid hyphens),
pass a string argument:

```urd
@fluent("player-name")
global player_name = "Traveler"
```

Now the Fluent variable is `$player-name` instead of `$player_name`.

```ftl
greeting = Welcome, { $player-name }!
```

### Where It Applies

`@fluent` is designed for `global` declarations. Applying it to other
constructs has no effect — the decorator is only meaningful for persistent
variables that the localization system can reference throughout the script's
lifetime.

---

## `@id`

Overrides the automatically generated localization ID for a dialogue line, menu,
or menu option. By default, Urd generates `loc_id` values from the file stem,
label name, and node position. `@id` lets you set a stable, human-readable key
instead.

### Usage

```urd
@id("intro-greeting")
narrator: "Welcome to the world of Urd."
```

The emitted `Event::Dialogue` will carry `loc_id: Some("intro-greeting")`
instead of the auto-generated ID.

### On Menu Options

```urd
menu {
    @id("shop-buy-sword")
    "Buy a sword" {
        jump buy_sword
    }
    @id("shop-buy-shield")
    "Buy a shield" {
        jump buy_shield
    }
}
```

### Why Use `@id`

- **Stable localization keys.** Auto-generated IDs change when you reorder lines
  or rename labels. `@id` gives you a key that survives refactors.
- **Readable `.ftl` files.** Translators see `intro-greeting` instead of
  `main-start-line_3`.
- **Cross-reference.** Lets you reference the same key in documentation, bug
  trackers, or QA tools.

### Validation

The `@id` decorator accepts exactly one string argument. If the argument is
missing or not a string, the compiler emits an `InvalidIdDecorator` diagnostic.
Duplicate `@id` values within the same file are also flagged.

---

## `@lint`

Enables opt-in analysis passes that are too expensive or too opinionated to run
by default. Pass the name of the lint check as a string argument.

### `@lint(check_loops)`

Enables loop-detection analysis for the decorated label. The compiler walks the
control-flow graph starting from this label and reports an
`InfiniteDialogueLoop` warning if it finds a cycle with no player-facing event
(dialogue or choice) that could break the loop.

```urd
@lint(check_loops)
label patrol_route {
    narrator: "The guard walks north."
    jump patrol_south
}

label patrol_south {
    narrator: "The guard walks south."
    jump patrol_route
}
```

Without `@lint(check_loops)`, this cycle compiles without warnings. With it, the
compiler flags `patrol_route` as part of an infinite loop — useful for catching
accidental cycles in complex scripts.

### Why Opt-In

Loop detection requires traversing the full control-flow graph from the
decorated label. For large scripts this can be expensive, and some cycles are
intentional (e.g. game loops that rely on the host to break out). Making it
opt-in keeps the default analysis fast while giving authors a tool to audit
critical paths.

---

## Summary

| Decorator | Target | Purpose |
|-----------|--------|---------|
| `@entry` | Labels | Marks the script entry point |
| `@fluent` | Globals | Injects variable into Fluent localization context |
| `@fluent("alias")` | Globals | Same, with a custom Fluent variable name |
| `@id("key")` | Dialogue, menus, options | Sets a custom localization ID |
| `@lint(check_loops)` | Labels | Enables infinite-loop detection analysis |