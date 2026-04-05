# Localization Example — Wandering Bazaar

A short merchant encounter that demonstrates **multi-file localization** in `urd`.
The bazaar is split across two source files — `bazaar.urd` (arrival and farewell)
and `merchant.urd` (browse, haggle, buy) — and `gen-l10n` produces a separate
`.ftl` file for each, with no duplicated entries.

The example also showcases the **item ID pattern**: the correct way to pass
dynamic item names to a localizer without locking translators into English grammar.

## Features showcased

| Feature | Where |
|---|---|
| `@fluent global gold` | Always in Fluent context — enables pluralization |
| `@fluent global price` | Injected everywhere — used in browse/buy messages |
| `@fluent("item") let item` | Scope-bound alias in merchant.urd's `browse` label |
| String interpolation `{gold}` | Auto-injected into Fluent vars even without `@fluent` |
| Multi-file FTL generation | `gen-l10n bazaar.urd` produces `bazaar.ftl` + `merchant.ftl`, no duplication |
| Item ID pattern | `item = "health_potion"` — slug, not a display string |
| Three locales | en-US (English), pl-PL (Polish), ru-RU (Russian) |

## Workflow

```bash
# 1. Generate FTL stubs (one per source module):
quest gen-l10n examples/localization/bazaar.urd \
      --output examples/localization/i18n/en-US/

# 2. Run in English:
quest run examples/localization/bazaar.urd

# 3. Run in Polish or Russian:
quest run examples/localization/bazaar.urd --locale pl-PL
quest run examples/localization/bazaar.urd --locale ru-RU
```

## File structure

```
examples/localization/
├── bazaar.urd                ← entry point (imports merchant.urd)
├── merchant.urd              ← merchant stall (imported by bazaar.urd)
├── README.md                 ← this file
└── i18n/
    ├── en-US/
    │   ├── bazaar.ftl        ← English: arrival + farewell
    │   └── merchant.ftl      ← English: browse, haggle, buy
    ├── pl-PL/
    │   ├── bazaar.ftl        ← Polish translation
    │   └── merchant.ftl      ← Polish translation
    └── ru-RU/
        ├── bazaar.ftl        ← Russian translation
        └── merchant.ftl      ← Russian translation
```

## Multi-file localization

Previously, `gen-l10n` produced one monolithic `.ftl` file per script. If
multiple scripts imported a common module, the common module's entries were
duplicated in every generated `.ftl`. Translators saw the same messages twice,
and keeping them in sync was error-prone.

Now `gen-l10n` produces **one `.ftl` per source module**. Running
`quest gen-l10n bazaar.urd` walks the import graph and emits:

- **`bazaar.ftl`** — labels defined in `bazaar.urd` (`start`, `farewell`).
- **`merchant.ftl`** — labels defined in `merchant.urd` (`browse`, `haggle`, `buy`).

Each `.ftl` contains only the labels that belong to its source file. Translators
never see duplicate entries, and adding a new locale is as simple as copying the
`en-US/` directory and translating the strings.

The circular import between the two files (`bazaar.urd` imports `merchant.urd`
for the shop labels; `merchant.urd` imports back from `bazaar.urd` for shared
characters and globals) is resolved cleanly — shared symbols like `narrator`,
`elara`, and `gold` live in whichever module defines them, and `gen-l10n` never
duplicates them across `.ftl` files.

## Key observations

- **`$gold` is always available** because `@fluent global gold = 50` is declared
  in `bazaar.urd` (the root). The variable is visible in both `bazaar.ftl` and
  `merchant.ftl`, so translators can write plural selectors like
  `{ $gold -> [one] one gold coin *[other] { $gold } coins }` in any label.

- **`$item` is scope-bound** to merchant.urd's `browse` label. The generated FTL
  correctly annotates which labels have access to `$item` and which do not.

- **`$price` updates at runtime** — if the player haggles, `price` drops from 30
  to 25. The FTL files always use `{ $price }` and get the current value
  automatically.

- **Interpolation overrides stale `@fluent` bindings** — for any variable that
  appears in both an `@fluent` declaration and a `{var}` string interpolation,
  the *current* runtime value wins. This means `$gold` in a Fluent plural
  selector correctly reflects the amount after any purchases, not the initial 50.

---

## The item ID pattern

### The problem

It is tempting to write:

```urd
let item_name = "Health Potion"   # ← WRONG
```

and then have the Polish FTL substitute `{ $item }` directly. This produces:

> Oto: **Health Potion**! Proszę 30 złotych — to prawdziwa okazja.

The item name is never translated. Worse, even if the translator writes the
Polish name, the grammatical case is fixed. Polish (like Russian, Czech,
Latin, …) requires different endings depending on a word's role in the sentence:

**Polish:**

| Sentence position | Polish form | Grammatical case |
|---|---|---|
| `Oto: ___` (Behold: ___) | `mikstura zdrowia` | Nominative |
| `Kup ___` (Buy ___) | `miksturę zdrowia` | Accusative |
| `Nie mam ___` (I have no ___) | `mikstury zdrowia` | Genitive |

**Russian:**

| Sentence position | Russian form | Grammatical case |
|---|---|---|
| `Вот: ___` (Behold: ___) | `зелье здоровья` | Nominative |
| `Купить ___` (Buy ___) | `зелье здоровья` | Accusative |
| `Нет ___` (There is no ___) | `зелья здоровья` | Genitive |

A raw English string passed from urd makes it impossible to express any of this.

### The solution

Pass an **item ID slug** from urd and let each FTL locale map it to the correct
form:

```urd
@fluent("item")
let item = "health_potion"   # ← slug, not a display string
```

Each locale file then uses a **Fluent selector** to resolve the slug
context-by-context:

```ftl
# Polish — "Oto:" requires nominative
merchant-browse-line_1 =
    Oto: { $item ->
        [health_potion] mikstura zdrowia
        [mana_potion]   mikstura many
       *[other]         { $item }
    }! …

# Polish — "Kup" requires accusative — same slug, different ending
merchant-browse-menu_1-buy_it_for_price_gold =
    Kup { $item ->
        [health_potion] miksturę zdrowia
        [mana_potion]   miksturę many
       *[other]         { $item }
    } za …
```

```ftl
# Russian — "Вот:" requires nominative
merchant-browse-line_1 =
    Вот: { $item ->
        [health_potion] зелье здоровья
        [mana_potion]   зелье маны
       *[other]         { $item }
    }! …

# Russian — "Купить" takes accusative (identical to nominative for neuter nouns)
merchant-browse-menu_1-buy_it_for_price_gold =
    Купить { $item ->
        [health_potion] зелье здоровья
        [mana_potion]   зелье маны
       *[other]         { $item }
    } за …
```

The `*[other] { $item }` fallback arm displays the raw slug for any item not yet
added to the FTL — a safe, visible reminder that a translation is missing.

### Adding a new item

1. In urd: set `item = "mana_potion"` (or any new slug).
2. In every locale FTL: add a `[mana_potion]` arm to each selector that references `$item`.
3. No urd code changes required.

---

## Russian plural rules

Russian has four CLDR plural categories: **one**, **few**, **many**, and
**other**. This is more complex than English (one/other) or Polish
(one/few/other).

| Category | Integers that match | Example |
|---|---|---|
| one | 1, 21, 31, 101, … | 1 золот**ой** |
| few | 2–4, 22–24, 32–34, … | 2 золот**ых** |
| many | 0, 5–20, 25–30, … | 5 золот**ых** |
| other | fractions, etc. | 1.5 золот**ых** |

For the word *золотой/золотых* ("gold coin(s)"), the **few**, **many**, and
**other** forms are identical, so the FTL can collapse them:

```ftl
# ru-RU/bazaar.ftl — farewell with gold pluralization
bazaar-farewell-if_1-line_1 = Ты уходишь с зельем в котомке и { $gold ->
        [one] { $gold } золотым
       *[other] { $gold } золотыми
    } в кошельке.
```

Compare with Polish, which keeps three distinct arms (one/few/other) for many
nouns — the right number of selector arms depends on the word, not just the
language. Fluent's `*[other]` fallback means you only need to spell out the
forms that actually differ.