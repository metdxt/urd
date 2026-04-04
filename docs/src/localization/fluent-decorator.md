# The `@fluent` Decorator

The `@fluent` decorator marks a variable as a **Fluent variable binding** — making it available to translators inside `.ftl` files as a `$variable`.

## Basic Usage

Place `@fluent` on the line immediately before a `global`, `const`, or `let` declaration:

```urd
@fluent
global gold = 50
```

This injects `gold` into the Fluent context for every message in the script. Translators can reference it as `{ $gold }` in their `.ftl` files:

```ftl
merchant-start-line_1 = You have { $gold } gold coins.
```

Multiple globals can each carry `@fluent`:

```urd
@fluent
global gold       = 50
@fluent
global price      = 30
@fluent
global has_potion = false
```

All three are now available as `$gold`, `$price`, and `$has_potion` in every Fluent message generated from this script.

## Custom Variable Name

By default, the Fluent variable name matches the Urd variable name. You can override this by passing a string argument to `@fluent`:

```urd
@fluent("item")
let item = "health_potion"
```

In this case the Urd variable `item` is exposed as `$item` in Fluent messages. The alias is especially useful when the Urd variable name would be awkward or ambiguous in the translation context.

## Scope Rules

The scope of the Fluent binding depends on the declaration kind:

| Declaration | Fluent Scope |
|---|---|
| `global` | Every message in the entire script |
| `const` | Every message in the entire script |
| `let` (inside a label) | Only messages within that label |

For example, a scoped `let` binding inside a label:

```urd
label browse {
    @fluent("item")
    let item = "health_potion"

    elara: "Behold: the Health Potion! I ask {price} gold."

    menu {
        "Buy it for {price} gold" {
            jump buy
        }
    }
}
```

Here, `$item` is available only in messages generated from the `browse` label. Messages in other labels will not see it. The global `$price`, on the other hand, is available everywhere.

## How Variables Reach the Localizer

When the VM emits a `Dialogue` or `Choice` event, it collects all Fluent bindings currently in scope and attaches them to the event via the `fluent_vars` field:

```rust
Event::Dialogue {
    speakers,
    lines,
    loc_id: Some("merchant-browse-line_1".into()),
    fluent_vars,   // ← contains {"gold": Int(50), "price": Int(30), "item": Str("health_potion")}
    localized_text,
    ..
}
```

If a `Localizer` is attached to the VM, it receives these variables in its `localize()` call and can pass them as `FluentArgs` to the Fluent bundle formatter.

## String Interpolation Variables

You don't always need `@fluent` to get variables into Fluent messages. Any variable used in a **string interpolation** (`{gold}`, `{price}`) is automatically included in `fluent_vars` for that specific event.

The difference:

| Mechanism | When to use |
|---|---|
| `@fluent` | Variable is needed by translators but not interpolated in the Urd source text (e.g., for plural rules or grammatical selectors) |
| String interpolation `{var}` | Variable appears directly in the dialogue text — automatically available in Fluent |
| Both | Perfectly fine — the variable appears once in `fluent_vars` either way |

## Dotted Paths

When a string interpolation uses a dotted path like `{inv.gold}`, the Fluent variable key uses hyphens instead of dots: `$inv-gold`. This follows Fluent's identifier conventions.

```urd
narrator: "You have {player.gold} gold."
```

In the `.ftl` file:

```ftl
# interpolation: $player-gold
narrator-line_1 = You have { $player-gold } gold.
```

## Format Specifiers

If a string interpolation includes a format specifier (e.g., `{price:.2}`), the value passed to `fluent_vars` is the **pre-formatted string** rather than the raw number. This ensures the translator's Fluent message receives exactly the formatted representation.

```urd
elara: "That will be {price:.2} gold."
# fluent_vars will contain: {"price": Str("30.00")} instead of Float(30.0)
```

## Variables Without `@fluent`

Variables that are **not** decorated with `@fluent` and **not** used in string interpolations are invisible to the localization system. This is intentional — internal bookkeeping variables that translators don't need should be kept out of the Fluent context:

```urd
# Internal bookkeeping — deliberately NOT @fluent; translators do not need it.
global haggled = false
```

## Validation

The LSP and compiler validate `@fluent` usage:

- `@fluent` must be placed on a variable declaration (`global`, `const`, or `let`)
- Placing `@fluent` on a dialogue line, label, or other non-declaration node produces a warning (`FluentOnUnsupportedNode`)
- Invalid alias arguments (wrong arity, non-string argument) produce an error (`InvalidFluentDecorator`)