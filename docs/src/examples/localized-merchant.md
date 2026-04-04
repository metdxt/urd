# Localized Merchant

This walkthrough covers the `examples/localization/` directory — a short merchant encounter that demonstrates every localization feature Urd offers, including the **item ID pattern** for grammatically correct translations across languages.

## File Structure

```text
examples/localization/
├── merchant.urd              ← the Urd script
├── README.md                 ← detailed notes on the item ID pattern
└── i18n/
    ├── en-US/
    │   └── merchant.ftl      ← English (generated, then hand-tuned)
    └── pl-PL/
        └── merchant.ftl      ← Polish (hand-translated with plural rules)
```

## The Story

You step into the Wandering Bazaar and meet Elara, a merchant selling a Health Potion. You can browse, haggle the price down from 30 to 25 gold, buy the potion, or walk away. The ending changes based on whether you bought the potion and how much gold you have left.

It is a small script — six labels, two menus, one conditional — but it exercises every localization feature in the language.

## The Script: `merchant.urd`

### Characters and Global State

```urd
const narrator = :{ name: "Narrator",  name_color: "white"  }
const elara    = :{ name: "Elara",     name_color: "yellow" }

@fluent
global gold       = 50
@fluent
global price      = 30
@fluent
global has_potion = false

global haggled = false
```

Three globals carry the `@fluent` decorator: `gold`, `price`, and `has_potion`. This means they are automatically injected into the Fluent localization context for **every** message in the script. Translators can reference `$gold`, `$price`, and `$has_potion` in their `.ftl` files — including for pluralization, conditional grammar, and gendered endings.

The fourth global, `haggled`, is intentionally **not** `@fluent`. It is internal bookkeeping that translators do not need.

### Entry Point

```urd
@entry
label start {
    narrator: {
        "You step into the Wandering Bazaar."
        "Canvas stalls stretch in every direction, lit by swaying lanterns."
    }

    elara: {
        "Welcome! I am Elara, purveyor of fine curiosities."
        "Might I interest you in a Health Potion today?"
    }

    menu {
        "Browse the wares" { jump browse }
        "Ask about the bazaar" {
            elara: {
                "The Wandering Bazaar follows the ancient trade routes."
                "We appear where weary travelers need us most."
            }
            jump browse
        }
        "Walk away" { jump farewell }
    }
}
```

Every dialogue line and menu option automatically gets a localization ID derived from the file name and label. For example, the first `narrator:` block becomes `merchant-start-line_1` in the generated `.ftl` file.

### The Item ID Pattern

The `browse` label demonstrates the most important localization technique:

```urd
label browse {
    @fluent("item")
    let item = "health_potion"

    elara: "Behold: the Health Potion! I ask {price} gold — a true bargain."

    menu {
        "Buy it for {price} gold" { jump buy }
        "Try to haggle"           { jump haggle }
        "Never mind"              { jump farewell }
    }
}
```

The key insight: `item` is assigned the **slug** `"health_potion"`, not the English display string `"Health Potion"`. The `@fluent("item")` decorator injects this slug into the Fluent context as `$item`.

Why does this matter? Because different languages need different grammatical forms of the same item name depending on its role in the sentence. In Polish:

| Sentence position | Polish form | Case |
|---|---|---|
| "Behold: ___" (subject) | mikstura zdrowia | Nominative |
| "Buy ___" (object) | miksturę zdrowia | Accusative |
| "I have no ___" | mikstury zdrowia | Genitive |

A raw English string makes this impossible. A slug lets each locale's `.ftl` file map the ID to the correct grammatical form in context.

### Haggling and Purchase

```urd
label haggle {
    haggled = true
    price   = 25

    elara: {
        "You drive a hard bargain."
        "Twenty-five gold — that is my absolute lowest. Take it or leave it."
    }

    menu {
        "Deal! {price} gold"         { jump buy }
        "Too rich for my blood"      {
            elara: "Another time, friend. Safe roads to you."
            jump farewell
        }
    }
}
```

When `price` is reassigned from 30 to 25, the Fluent context is updated automatically. The next time `$price` appears in a translated message, it reflects the haggled price — no manual plumbing required.

```urd
label buy {
    if gold >= price {
        gold       = gold - price
        has_potion = true
        elara: "Splendid! The potion is yours. You now have {gold} gold."
        jump farewell
    } else {
        elara: "You only have {gold} gold — not quite {price}. Come back when fortune smiles."
        jump farewell
    }
}
```

### Farewell

```urd
label farewell {
    if has_potion {
        narrator: "You leave with a potion in your pack and {gold} gold."
        elara: "That potion has saved many a life. May it save yours too."
    } else {
        narrator: "You leave the Wandering Bazaar with {gold} gold and your curiosity intact."
        elara: "Return whenever you need something. I will be here."
    }
    jump _end
}

label _end {
    end!()
}
```

## The English `.ftl` File

Generated by running:

```bash
quest gen-l10n examples/localization/merchant.urd \
      --output examples/localization/i18n/en-US/
```

This produces `i18n/en-US/merchant.ftl` with entries like:

```ftl
## label: start
# @fluent variables: $has_potion, $price, $gold

merchant-start-line_1 = You step into the Wandering Bazaar.
    Canvas stalls stretch in every direction, lit by swaying lanterns.

merchant-start-line_2 = Welcome! I am Elara, purveyor of fine curiosities.
    Might I interest you in a Health Potion today?

merchant-start-menu_1-browse_the_wares = Browse the wares
merchant-start-menu_1-ask_about_the_bazaar = Ask about the bazaar
merchant-start-menu_1-walk_away = Walk away
```

Each entry is annotated with a comment listing the `@fluent` variables available in that label's scope. The keys are deterministic — derived from the file stem, label name, and node position.

The generated `.ftl` is a **scaffold**. Translators copy it, translate the values, and add Fluent features (plurals, selectors) as needed.

## The Polish `.ftl` File

The hand-translated `i18n/pl-PL/merchant.ftl` demonstrates the real power of Fluent:

### Plural Rules

Polish has complex plural rules (one, few, many, other). The translator uses Fluent selectors on `$gold` and `$price`:

```ftl
merchant-buy-if_1-line_1 = Znakomicie! Mikstura jest twoja. Masz teraz { $gold ->
    [0]     ani grosza — warta każdej monety!
    [one]   tylko jedną złotą monetę.
    [few]   { $gold } złote monety.
   *[other] { $gold } złotych monet.
}
```

The same `$gold` value produces four different surface forms depending on the number.

### Item ID Selectors

The item slug is used in Fluent selectors to produce the correct grammatical case:

```ftl
# "Oto:" (Behold:) requires nominative case
merchant-browse-line_1 =
    Oto: { $item ->
        [health_potion] mikstura zdrowia
        [mana_potion]   mikstura many
       *[other]         { $item }
    }! Proszę { $price ->
        [one]   { $price } złoty — to prawdziwa okazja.
        [few]   { $price } złote — to prawdziwa okazja.
       *[other] { $price } złotych — to prawdziwa okazja.
    }
```

```ftl
# "Kup" (Buy) requires accusative case — same slug, different ending
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

The `*[other] { $item }` fallback arm displays the raw slug for any item not yet added to the `.ftl` — a visible reminder that a translation is missing.

## Running the Example

### Without Localization (development mode)

```bash
quest run examples/localization/merchant.urd
```

When no localizer is attached, the VM uses the fallback text from the script itself (the English strings written inline).

### With English Localization

```bash
quest run examples/localization/merchant.urd --locale en-US
```

The runner loads `i18n/en-US/merchant.ftl` and populates the `localized_text` field on each event. If a key is found, the Fluent-formatted text replaces the inline fallback.

### With Polish Localization

```bash
quest run examples/localization/merchant.urd --locale pl-PL
```

Now every event carries Polish text with correct pluralization and grammatical case.

## How Localization IDs Map to Script Structure

The ID generation follows a deterministic pattern:

| Script location | Generated ID |
|---|---|
| 1st dialogue in `start` | `merchant-start-line_1` |
| 2nd dialogue in `start` | `merchant-start-line_2` |
| Menu option "Browse" in `start` | `merchant-start-menu_1-browse_the_wares` |
| 1st dialogue in `browse` | `merchant-browse-line_1` |
| Menu option "Buy it" in `browse` | `merchant-browse-menu_1-buy_it_for_price_gold` |
| Dialogue inside `if` in `buy` | `merchant-buy-if_1-line_1` |
| Dialogue inside `if` in `farewell` | `merchant-farewell-if_1-line_1` |

The pattern is: `<file_stem>-<label>-<context>-<slug>`. Menu option slugs are derived from the option text, lowercased and with spaces replaced by underscores.

## Key Takeaways

1. **`@fluent` on globals injects them everywhere.** Declare `@fluent global gold = 50` once, and every message in the script can use `$gold` for pluralization without any extra work.

2. **Scoped `@fluent` bindings stay scoped.** The `@fluent("item") let item` in `browse` is only available in the `browse` label. The generated `.ftl` annotations tell translators exactly which variables are in scope for each label.

3. **Runtime mutations propagate.** When the player haggles and `price` drops from 30 to 25, the next Fluent message sees the updated value. String interpolation variables always reflect the current runtime state.

4. **Pass slugs, not display strings.** The item ID pattern (`item = "health_potion"`) is the correct way to handle dynamic item names. It lets translators produce grammatically correct forms in any language without touching the Urd script.

5. **The `*[other]` fallback is your safety net.** Every Fluent selector should have a `*[other]` arm that displays the raw value. This makes missing translations visible instead of silent.

6. **Adding a new item requires only `.ftl` changes.** To add `"mana_potion"` to the merchant's stock, set `item = "mana_potion"` in the script and add `[mana_potion]` arms to the `.ftl` selectors. No other Urd code changes are needed.