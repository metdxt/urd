# Lists & Maps

Urd provides two built-in collection types: **lists** (ordered, indexed sequences) and **maps** (key-value dictionaries). Both are first-class values that can be stored in variables, passed to functions, and nested inside each other.

> **Immutable by design.** All list and map methods are **pure** — they return new values without mutating the receiver. To update a variable, reassign the result: `items = items.append("bow")`.

---

## Lists

### Creating Lists

Lists are created with square-bracket literals. Elements can be of any type — lists are heterogeneous:

```urd
let items = ["sword", "shield", "potion"]
let mixed = [1, "two", true, null]
let empty = []
```

### Accessing Elements

Use zero-based subscript notation to read elements:

```urd
let weapons = ["dagger", "bow", "staff"]
narrator: "Your weapon is: {weapons[0]}"
# → "Your weapon is: dagger"
```

### Assigning Elements

Subscript assignment replaces the value at a given index:

```urd
let items = ["sword", "shield", "potion"]
items[0] = "axe"
narrator: "You swapped your sword for an {items[0]}."
# → "You swapped your sword for an axe."
```

### Working with Lists

Since all methods are pure, you reassign the result to update a variable:

```urd
let inventory = ["sword", "shield"]

# Adding items (returns a new list)
inventory = inventory.append("potion")
narrator: "You now carry {inventory.len()} items."
# → "You now carry 3 items."

# Removing the last element (returns a new list without it)
inventory = inventory.pop()
narrator: "You dropped the last item. Now carrying {inventory.len()}."
# → "You dropped the last item. Now carrying 2."

# Checking membership
if inventory.contains("shield") {
    narrator: "Your shield is ready."
}
```

### Functional Methods

The `.map()`, `.filter()`, and `.reduce()` methods accept function values, enabling a functional programming style:

```urd
let prices = [10, 25, 50, 5, 100]

# Double all prices
let inflated = prices.map(fn(p: int) -> int { return p * 2 })

# Keep only affordable items
let affordable = prices.filter(fn(p: int) -> bool { return p <= 30 })

# Sum all prices
let total = prices.reduce(0, fn(acc: int, p: int) -> int { return acc + p })
narrator: "Total cost: {total} gold."
```

For the complete list of list methods, see the [Built-in Methods Reference](../reference/builtin-methods.md#list-methods).

---

## Maps

### Creating Maps

Maps are created with the `:{` literal syntax — a colon followed by a left brace:

```urd
let stats = :{ health: 100, mana: 50, stamina: 75 }
let empty_map = :{}
```

The `:{` token is a single syntactic unit (colon + left brace). No space is required between them, though `:{` is the idiomatic form.

Keys are identifiers (unquoted) in the literal syntax. Values can be any expression.

### Accessing Values

Use dot notation for known keys, or subscript notation with a string key:

```urd
let hero = :{ name: "Kael", health: 100 }

narrator: "Name: {hero.name}"
narrator: "Health: {hero["health"]}"
```

### Assigning Values

Subscript assignment creates or updates a key:

```urd
let stats = :{ health: 100, mana: 50 }
stats["health"] = 80
stats["armor"] = 25
```

### Working with Maps

Like lists, all map methods are pure and return new maps:

```urd
let config = :{ difficulty: "hard", volume: 80, subtitles: true }

narrator: "Settings count: {config.len()}"

if config.has("subtitles") {
    narrator: "Subtitles are enabled."
}

let all_keys = config.keys()
# all_keys is ["difficulty", "subtitles", "volume"] (sorted)

config = config.set("brightness", 70)
config = config.remove("volume")
```

For the complete list of map methods, see the [Built-in Methods Reference](../reference/builtin-methods.md#map-methods).

---

## Membership Testing

### The `in` Operator

The `in` operator tests membership across several types: ranges, lists, maps, and strings.

#### Range Membership (`Int in Range`)

Check whether an integer falls within a range:

```urd
let level = 5

if level in 1..10 {
    narrator: "You are in the beginner zone."
}

if level in 1..=5 {
    narrator: "You qualify for the tutorial."
}
```

#### List Membership (`value in List`)

Check whether a value is an element of a list. Comparison uses structural equality, so any value type is supported on the left-hand side:

```urd
let inventory = ["sword", "shield", "potion"]

if "sword" in inventory {
    narrator: "You draw your sword."
}

if not ("bow" in inventory) {
    narrator: "You don't have a ranged weapon."
}
```

#### Map Key Membership (`Str in Map`)

Check whether a string key exists in a map. The left-hand side must be a `Str`:

```urd
let stats = :{ health: 100, mana: 50 }

if "mana" in stats {
    narrator: "You channel your magical energy."
}
```

### Method Alternatives

The `.contains()` and `.has()` methods still work and may be preferred in some contexts:

```urd
# Equivalent to: "shield" in inventory
if inventory.contains("shield") {
    narrator: "Your shield is ready."
}

# Equivalent to: "mana" in stats
if stats.has("mana") {
    narrator: "You have mana."
}
```

---

## Maps as Struct-Like Values

Maps are commonly used to define speaker metadata and other structured data:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }
const zara     = :{ name: "Zara",     name_color: "cyan"  }

narrator: "You approach the stranger."
zara: "I've been expecting you."
```

For cases where you want compile-time field validation, consider using [Structs](./structs.md) instead — they enforce that all required fields are present and correctly typed.

---

## Nesting Collections

Lists and maps can be nested freely:

```urd
let party = [
    :{ name: "Kael", class: "warrior", hp: 100 },
    :{ name: "Lyra", class: "mage", hp: 60 },
    :{ name: "Finn", class: "rogue", hp: 75 }
]

narrator: "Party leader: {party[0].name}"
narrator: "Party size: {party.len()}"
```

Maps can contain lists:

```urd
let quest_log = :{
    active: ["Find the lost sword", "Deliver the message"],
    completed: ["Talk to the innkeeper"]
}

narrator: "Active quests: {quest_log["active"].len()}"
```

---

## Quick Reference

| Syntax | Description |
|--------|-------------|
| `[a, b, c]` | List literal |
| `:{key: val}` | Map literal |
| `list[i]` | Subscript read (list) |
| `map["key"]` | Subscript read (map) |
| `map.key` | Dot access (map) |
| `list[i] = val` | Subscript assign (list) |
| `map["key"] = val` | Subscript assign (map) |
| `n in range` | Range membership (`Int in Range`) |
| `val in list` | List membership (`value in List`) |
| `key in map` | Map key membership (`Str in Map`) |
| `list.contains(val)` | List membership test (method form) |
| `map.has(key)` | Map key existence test (method form) |