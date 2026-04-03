# Localization Example — Wandering Bazaar

A short merchant encounter that demonstrates all localization features of `urd`.

## Features showcased

| Feature | Where |
|---|---|
| `@fluent global gold` | Always in Fluent context — enables pluralization |
| `@fluent global price` | Injected everywhere — used in `browse` / `buy` messages |
| `@fluent("item") let item_name` | Scope-bound alias: urd calls it `item_name`, FTL uses `$item` |
| String interpolation `{gold}` | Auto-injected into fluent vars even without `@fluent` |
| `localized_text` on events | When `i18n/<locale>/` exists, quest shows the FTL translation |

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