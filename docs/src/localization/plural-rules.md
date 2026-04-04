# Plural Rules & Grammar

One of Fluent's greatest strengths is its first-class support for **plural rules** and **grammatical selectors**. This chapter shows how to use these features in your Urd translations.

## Pluralization with `$gold`

Fluent uses [CLDR plural categories](https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html) — `zero`, `one`, `two`, `few`, `many`, and `other` — to select the right form of a word based on a numeric variable. The `*[other]` arm is the required default.

Here's how the Polish translation of the merchant example handles the `$gold` variable:

```ftl
merchant-buy-if_1-line_1 = Masz teraz { $gold ->
    [0]     ani grosza — warta każdej monety!
    [one]   tylko jedną złotą monetę.
    [few]   { $gold } złote monety.
   *[other] { $gold } złotych monet.
}
```

In Polish, the word for "gold coins" changes form depending on the number:

| Count | Category | Polish form |
|-------|----------|-------------|
| 0 | (exact match) | ani grosza |
| 1 | `one` | jedną złotą monetę |
| 2–4 | `few` | złote monety |
| 5+ | `other` | złotych monet |

English only needs `one` and `other`, but languages like Polish, Arabic, and Welsh have richer plural rules. Fluent handles all of this automatically — you just write the patterns.

### Exact matches vs. plural categories

Note the difference between `[0]` (an exact numeric match) and `[one]` (a plural category). The exact match `[0]` fires only when `$gold` is literally zero. The category `[one]` fires when the number belongs to the language's "one" category — which in Polish includes 1 but not 21, 31, etc.

You can mix exact and category matches freely:

```ftl
score = You defeated { $enemies ->
    [0]     no enemies
    [1]     a single enemy
   *[other] { $enemies } enemies
}.
```

## Grammatical Case with Item IDs

Many languages require different word forms depending on a word's **role in the sentence** (its grammatical case). Urd solves this by passing **item slugs** — stable string identifiers — rather than raw display strings.

In the Urd script, you pass a slug:

```urd
@fluent("item")
let item = "health_potion"
```

In the FTL file, the translator maps each slug to the correct grammatical form for that sentence position:

```ftl
# Nominative case — used after "Oto:" (Behold:)
merchant-browse-line_1 =
    Oto: { $item ->
        [health_potion] mikstura zdrowia
        [mana_potion]   mikstura many
       *[other]         { $item }
    }! ...

# Accusative case — used after "Kup" (Buy)
merchant-browse-menu_1-buy_it_for_price_gold =
    Kup { $item ->
        [health_potion] miksturę zdrowia
        [mana_potion]   miksturę many
       *[other]         { $item }
    } za ...
```

The same slug `health_potion` produces different surface forms depending on the sentence context:

| Context | Case | Polish form |
|---------|------|-------------|
| "Behold: X" | Nominative | mikstura zdrowia |
| "Buy X for…" | Accusative | miksturę zdrowia |

This approach has several advantages:

- **No code changes** when adding new items — only the `.ftl` files need updating
- **Each language controls its own grammar** — the Urd script is language-agnostic
- **The `*[other]` fallback** ensures untranslated items still display something reasonable

## Combining Selectors

You can nest selectors or use multiple variables in a single message. Here's a Polish example that combines item-ID declension with price pluralization:

```ftl
merchant-browse-menu_1-buy_it_for_price_gold =
    Kup { $item ->
        [health_potion] miksturę zdrowia
        [mana_potion]   miksturę many
       *[other]         { $item }
    } za { $price ->
        [one]   { $price } złoty
        [few]   { $price } złote
       *[other] { $price } złotych
    }
```

This single message handles:
- Declining the item name into accusative case
- Pluralizing the price with the correct currency form

## Adding a New Language

To add plural rules for a new language:

1. **Copy the `en-US` FTL file** as your starting point
2. **Look up your language's plural categories** in the [CLDR plural rules chart](https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html)
3. **Add selector arms** for each category your language uses
4. **Always include `*[other]`** as the default fallback
5. **Test with edge-case numbers** (0, 1, 2, 5, 21, 100, etc.)

### Common plural category sets by language

| Language | Categories used |
|----------|----------------|
| English | `one`, `other` |
| French | `one`, `many`, `other` |
| Polish | `one`, `few`, `many`, `other` |
| Arabic | `zero`, `one`, `two`, `few`, `many`, `other` |
| Japanese | `other` (no plural distinction) |

## Reference

- [Project Fluent Syntax Guide](https://projectfluent.org/fluent/guide/)
- [CLDR Plural Rules](https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html)
- [Fluent Playground](https://projectfluent.org/play/) — test your messages interactively