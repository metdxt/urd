# Localization Example — Wandering Bazaar

A short merchant encounter that demonstrates all localization features of `urd`,
including the **item ID pattern** — the correct way to pass dynamic item names
to a localizer without locking translators into English grammar.

## Features showcased

| Feature | Where |
|---|---|
| `@fluent global gold` | Always in Fluent context — enables pluralization |
| `@fluent global price` | Injected everywhere — used in `browse` / `buy` messages |
| `@fluent("item") let item` | Scope-bound alias: urd calls it `item`, FTL uses `$item` |
| String interpolation `{gold}` | Auto-injected into fluent vars even without `@fluent` |
| `localized_text` on events | When `i18n/<locale>/` exists, quest shows the FTL translation |
| Item ID pattern | `item = "health_potion"` — slug, not a display string |

## Workflow

```bash
# 1. Generate the FTL scaffold (source-language stubs for translators):
quest gen-l10n examples/localization/merchant.urd \
      --output examples/localization/i18n/en-US/

# 2. Run in English (loads i18n/en-US/merchant.ftl automatically):
quest run examples/localization/merchant.urd

# 3. Run in Polish (edit quest source to pass locale="pl-PL", or copy the
#    pl-PL directory to en-US for a quick test):
#    cp -r examples/localization/i18n/pl-PL examples/localization/i18n/en-US-test
```

## File structure

```
examples/localization/
├── merchant.urd              ← the urd script
├── README.md                 ← this file
└── i18n/
    ├── en-US/
    │   └── merchant.ftl      ← English (with Fluent plurals)
    └── pl-PL/
        └── merchant.ftl      ← Polish (with Polish plural rules)
```

## Key observations

- **`$gold` is always available** because `@fluent global gold = 50` is declared at the top.
  The FTL translator can write `{ $gold -> [one] one gold coin *[other] { $gold } coins }`
  even in messages that never use `{gold}` in the urd source.

- **`$item` is scope-bound** to the `browse` label. The generated FTL correctly annotates
  which labels have access to `$item` and which do not.

- **`$price` updates at runtime** — if the player haggles, `price` drops from 30 to 25.
  The FTL file always uses `{ $price }` and gets the current value automatically.

- **Interpolation overrides stale `@fluent` bindings** — for any variable that appears in
  both an `@fluent` declaration and a `{var}` string interpolation, the *current* runtime
  value wins. This means `$gold` in a Fluent plural selector correctly reflects the amount
  after any purchases, not the initial 50.

---

## The item ID pattern

### The problem

It is tempting to write:

```urd
let item_name = "Health Potion"   # ← WRONG
```

and then have the Polish FTL substitute `{ $item }` directly. This produces:

> Oto: **Health Potion**! Proszę 30 złotych — to prawdziwa okazja.

The item name is never translated. Worse, even if the translator writes the Polish name,
the grammatical case is fixed. Polish (like Russian, Czech, Latin, …) requires different
endings depending on a word's role in the sentence:

| Sentence position | Polish form | Grammatical case |
|---|---|---|
| `Oto: ___` (Behold: ___) | `mikstura zdrowia` | Nominative |
| `Kup ___` (Buy ___) | `miksturę zdrowia` | Accusative |
| `Nie mam ___` (I have no ___) | `mikstury zdrowia` | Genitive |

A raw English string passed from urd makes it impossible to express any of this.

### The solution

Pass an **item ID slug** from urd and let each FTL locale map it to the correct form:

```urd
@fluent("item")
let item = "health_potion"   # ← slug, not a display string
```

Each locale file then uses a **Fluent selector** to resolve the slug context-by-context:

```ftl
# "Oto:" requires nominative
merchant-browse-line_1 =
    Oto: { $item ->
        [health_potion] mikstura zdrowia
        [mana_potion]   mikstura many
       *[other]         { $item }
    }! …

# "Kup" requires accusative — same slug, different ending
merchant-browse-menu_1-buy_it_for_price_gold =
    Kup { $item ->
        [health_potion] miksturę zdrowia
        [mana_potion]   miksturę many
       *[other]         { $item }
    } za …
```

The `*[other] { $item }` fallback arm displays the raw slug for any item not yet
added to the FTL — a safe, visible reminder that a translation is missing.

### Adding a new item

1. In urd: set `item = "mana_potion"` (or any new slug).
2. In every locale FTL: add a `[mana_potion]` arm to each selector that references `$item`.
3. No urd code changes required.