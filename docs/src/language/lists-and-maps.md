# Lists & Maps

Urd provides two built-in collection types: **lists** (ordered, indexed sequences) and **maps** (key-value dictionaries). Both are first-class values that can be stored in variables, passed to functions, and nested inside each other.

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

### List Methods

Lists support a rich set of built-in methods:

| Method | Description |
|--------|-------------|
| `.len()` | Returns the number of elements |
| `.push(val)` | Appends `val` to the end of the list |
| `.pop()` | Removes and returns the last element |
| `.contains(val)` | Returns `true` if `val` is in the list |
| `.remove(index)` | Removes the element at `index` and returns it |
| `.reverse()` | Reverses the list in place |
| `.sort()` | Sorts the list in place (elements must be comparable) |
| `.map(fn)` | Returns a new list with `fn` applied to each element |
| `.filter(fn)` | Returns a new list with only elements where `fn` returns `true` |
| `.fold(init, fn)` | Reduces the list to a single value using `fn(accumulator, element)` |

#### Examples

```urd
let inventory = ["sword", "shield"]

# Adding and removing items
inventory.push("potion")
narrator: "You now carry {inventory.len()} items."
# → "You now carry 3 items."

let last = inventory.pop()
narrator: "You dropped the {last}."
# → "You dropped the potion."

# Checking membership
if inventory.contains("shield") {
    narrator: "Your shield is ready."
}

# Removing by index
inventory.remove(0)
narrator: "You discarded the {inventory[0]}."
```

#### Functional Methods

The `.map()`, `.filter()`, and `.fold()` methods accept function values, enabling a functional programming style:

```urd
let prices = [10, 25, 50, 5, 100]

# Double all prices
let inflated = prices.map(fn(p: int) -> int { return p * 2 })

# Keep only affordable items
let affordable = prices.filter(fn(p: int) -> bool { return p <= 30 })

# Sum all prices
let total = prices.fold(0, fn(acc: int, p: int) -> int { return acc + p })
narrator: "Total cost: {total} gold."
```

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

### Map Methods

Maps provide the following built-in methods:

| Method | Description |
|--------|-------------|
| `.len()` | Returns the number of key-value pairs |
| `.keys()` | Returns a list of all keys (as strings) |
| `.values()` | Returns a list of all values |
| `.contains_key(key)` | Returns `true` if the map has the given key |
| `.remove(key)` | Removes the entry for `key` and returns the value |
| `.insert(key, val)` | Inserts or updates a key-value pair |

#### Examples

```urd
let config = :{ difficulty: "hard", volume: 80, subtitles: true }

narrator: "Settings count: {config.len()}"

if config.contains_key("subtitles") {
    narrator: "Subtitles are enabled."
}

let all_keys = config.keys()
# all_keys is ["difficulty", "volume", "subtitles"]

config.insert("brightness", 70)
config.remove("volume")
```

---

## The `in` Operator

The `in` operator tests membership in both lists and maps:

### Lists — Value Membership

```urd
let inventory = ["sword", "shield", "potion"]

if "sword" in inventory {
    narrator: "You draw your sword."
}

if not "bow" in inventory {
    narrator: "You don't have a ranged weapon."
}
```

### Maps — Key Membership

For maps, `in` checks whether a **key** exists:

```urd
let stats = :{ health: 100, mana: 50 }

if "mana" in stats {
    narrator: "You channel your magical energy."
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
| `val in list` | List membership test |
| `key in map` | Map key existence test |