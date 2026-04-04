# Localization Overview

Urd has built-in localization support powered by [Project Fluent](https://projectfluent.org/), Mozilla's localization system designed for natural-sounding translations. Every piece of player-facing text — dialogue lines, menu options, and choice labels — gets a stable **localization ID** (`loc_id`) that translators can target without touching your script code.

## How It Works

The localization pipeline follows a straightforward compile → generate → translate → load workflow:

1. **Compile** — The Urd compiler assigns a hierarchical `loc_id` to every dialogue and menu option node in the IR graph.
2. **Generate** — Run `quest gen-l10n` to produce a `.ftl` stub file pre-filled with source-language text.
3. **Translate** — Translators copy the `.ftl` file and replace English values with their language.
4. **Load** — At runtime, attach a `Localizer` to the VM. It intercepts every event and substitutes translated text.

## Localization IDs

Every localizable node receives a hierarchical ID built from the script's structure:

| Node Type | ID Pattern | Example |
|---|---|---|
| Dialogue line | `file_slug-label-line_N` | `merchant-start-line_1` |
| Menu option | `file_slug-label-menu_N-option_slug` | `merchant-start-menu_1-browse_the_wares` |
| Inline dialogue (inside menu) | `file_slug-label-menu_N-line_N` | `merchant-start-menu_1-line_1` |
| Conditional dialogue | `file_slug-label-if_N-line_N` | `merchant-buy-if_1-line_1` |

IDs are deterministic: they depend only on the script's structure, not on the text content. This means translators can work independently and IDs remain stable as long as the script structure doesn't change.

## The `@fluent` Decorator

The `@fluent` decorator marks a variable for injection into the Fluent context. Translators can then reference it with `$variable_name` in their `.ftl` files for pluralization, grammatical agreement, and dynamic content:

```urd
@fluent
global gold = 50

@fluent
global price = 30
```

Translators can use `$gold` and `$price` in Fluent messages:

```ftl
merchant-buy-if_1-line_1 = You now have { $gold } gold.
```

String interpolation variables (e.g. `{gold}` in dialogue text) are also automatically passed to the Fluent context, so even without `@fluent` they are available for translation.

See [The @fluent Decorator](./fluent-decorator.md) for full details.

## The `quest gen-l10n` Command

The `quest` CLI can generate `.ftl` stub files from any compiled script:

```bash
quest gen-l10n script.urd
```

This produces a ready-to-translate `.ftl` file with all dialogue and menu text pre-filled as Fluent messages. See [Generating .ftl Files](./generating-ftl.md) for details.

## The `Localizer` Trait

The `Localizer` trait bridges Urd with any Fluent runtime. It has a single method:

```rust
fn localize(&self, id: &str, vars: &HashMap<String, RuntimeValue>) -> Option<String>;
```

Return `Some(text)` for translated text, or `None` to keep the original. Attach it to the VM before execution:

```rust
vm.with_localizer(Arc::new(my_localizer));
```

The `quest` CLI includes a reference `FsLocalizer` implementation that loads `.ftl` files from disk. See [Implementing a Localizer](./implementing-localizer.md) for a complete walkthrough.

## Architecture Diagram

```text
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│  script.urd  │────▶│   Compiler   │────▶│   IrGraph   │
└─────────────┘     └──────────────┘     └──────┬──────┘
                                                │
                    ┌───────────────────────────┤
                    ▼                           ▼
           ┌───────────────┐          ┌─────────────────┐
           │ quest gen-l10n │          │       VM        │
           └───────┬───────┘          │  + Localizer    │
                   │                  └────────┬────────┘
                   ▼                           │
           ┌───────────────┐                   ▼
           │  en-US/*.ftl  │          ┌─────────────────┐
           │  pl-PL/*.ftl  │────────▶│  Translated text │
           │  ja-JP/*.ftl  │          │  in Events       │
           └───────────────┘          └─────────────────┘
```

## Directory Layout

A typical localized project looks like this:

```text
my_game/
├── scripts/
│   ├── merchant.urd
│   └── i18n/
│       ├── en-US/
│       │   └── merchant.ftl
│       ├── pl-PL/
│       │   └── merchant.ftl
│       └── ja-JP/
│           └── merchant.ftl
└── ...
```

The `quest run` command auto-discovers the `i18n/` directory next to the script and offers a locale picker if multiple translations are available.

## What's Next

- [The @fluent Decorator](./fluent-decorator.md) — injecting game state into translations
- [Generating .ftl Files](./generating-ftl.md) — creating translation stubs from scripts
- [Implementing a Localizer](./implementing-localizer.md) — wiring up Fluent in your game
- [Plural Rules & Grammar](./plural-rules.md) — handling pluralization and grammatical case