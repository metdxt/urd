# Built-in Methods

Urd provides built-in methods on its primitive and collection types. All methods are **pure** — they return new values without mutating the receiver. To update a variable, reassign the result:

```urd
let name = "  hello  "
name = name.trim()
```

> **Note:** This reference is derived from the source implementation. Check the source files (`str_methods.rs`, `int_methods.rs`, `float_methods.rs`, `list_methods.rs`, `map_methods.rs`, `range_methods.rs`) for the definitive, up-to-date list.

---

## String Methods

Methods callable on `Str` values.

### Queries

| Method | Signature | Description |
|--------|-----------|-------------|
| `len` | `.len() → Int` | Returns the character count (not byte count). Unicode-aware. |
| `is_empty` | `.is_empty() → Bool` | Returns `true` if the string has zero characters. |

### Case Conversion

| Method | Signature | Description |
|--------|-----------|-------------|
| `to_upper` | `.to_upper() → Str` | Returns a new string with all characters uppercased. |
| `to_lower` | `.to_lower() → Str` | Returns a new string with all characters lowercased. |

### Whitespace

| Method | Signature | Description |
|--------|-----------|-------------|
| `trim` | `.trim() → Str` | Strips leading and trailing whitespace. |
| `trim_start` | `.trim_start() → Str` | Strips leading whitespace only. |
| `trim_end` | `.trim_end() → Str` | Strips trailing whitespace only. |

### Predicates

| Method | Signature | Description |
|--------|-----------|-------------|
| `contains` | `.contains(s: Str) → Bool` | Returns `true` if the string contains the substring `s`. |
| `starts_with` | `.starts_with(s: Str) → Bool` | Returns `true` if the string starts with `s`. |
| `ends_with` | `.ends_with(s: Str) → Bool` | Returns `true` if the string ends with `s`. |

### Splitting & Replacing

| Method | Signature | Description |
|--------|-----------|-------------|
| `split` | `.split(sep: Str) → List[Str]` | Splits the string by `sep` and returns a list of parts. |
| `replace` | `.replace(from: Str, to: Str) → Str` | Replaces all occurrences of `from` with `to`. |

### Slicing

| Method | Signature | Description |
|--------|-----------|-------------|
| `slice` | `.slice(start: Int) → Str` | Returns a substring from `start` to the end. Character-based, not byte-based. Negative indices count from the end. |
| `slice` | `.slice(start: Int, end: Int) → Str` | Returns a substring from `start` to `end`. Both bounds are clamped. An inverted range yields an empty string. |

### Type Coercions

| Method | Signature | Description |
|--------|-----------|-------------|
| `to_int` | `.to_int() → Int` | Parses the string as an integer. Trims whitespace first. Returns a `TypeError` if parsing fails. |
| `to_float` | `.to_float() → Float` | Parses the string as a float. Returns a `TypeError` if parsing fails. |

### Character Decomposition

| Method | Signature | Description |
|--------|-----------|-------------|
| `chars` | `.chars() → List[Str]` | Returns a list where each element is a single-character string. Unicode-safe. |

### Repetition & Lines

| Method | Signature | Description |
|--------|-----------|-------------|
| `repeat` | `.repeat(n: Int) → Str` | Returns the string repeated `n` times. `n` must be non-negative. |
| `lines` | `.lines() → List[Str]` | Splits the string by line endings (`\n` or `\r\n`) and returns a list. |

**Example:**

```urd
let msg = "  Hello, World!  "
let trimmed = msg.trim()           # "Hello, World!"
let upper = trimmed.to_upper()     # "HELLO, WORLD!"
let parts = "a,b,c".split(",")    # ["a", "b", "c"]
let has_h = "hello".contains("h") # true
let chars = "abc".chars()          # ["a", "b", "c"]
```

---

## Int Methods

Methods callable on `Int` values.

### Conversions

| Method | Signature | Description |
|--------|-----------|-------------|
| `to_string` | `.to_string() → Str` | Formats the integer as a string. |
| `to_float` | `.to_float() → Float` | Converts the integer to a float. |

### Arithmetic

| Method | Signature | Description |
|--------|-----------|-------------|
| `abs` | `.abs() → Int` | Returns the absolute value. Returns a `TypeError` on `i64::MIN` (no positive counterpart). |
| `min` | `.min(other: Int) → Int` | Returns the smaller of `self` and `other`. |
| `max` | `.max(other: Int) → Int` | Returns the larger of `self` and `other`. |
| `clamp` | `.clamp(min: Int, max: Int) → Int` | Clamps `self` into `[min, max]`. Errors if `min > max`. |
| `pow` | `.pow(exp: Int) → Int` | Raises `self` to the power `exp`. Exponent must be non-negative. Returns a `TypeError` on overflow. |
| `signum` | `.signum() → Int` | Returns `-1`, `0`, or `1` depending on the sign. |

**Example:**

```urd
let health = -30
let positive = health.abs()            # 30
let clamped = health.clamp(0, 100)     # 0
let power = 2.pow(10)                  # 1024
let sign = (-42).signum()              # -1
```

---

## Float Methods

Methods callable on `Float` values.

### Conversions

| Method | Signature | Description |
|--------|-----------|-------------|
| `to_string` | `.to_string() → Str` | Formats the float as a string. |
| `to_int` | `.to_int() → Int` | Truncates toward zero and returns an integer. |

### Arithmetic

| Method | Signature | Description |
|--------|-----------|-------------|
| `abs` | `.abs() → Float` | Returns the absolute value. |
| `floor` | `.floor() → Int` | Rounds down to the nearest integer. |
| `ceil` | `.ceil() → Int` | Rounds up to the nearest integer. |
| `round` | `.round() → Int` | Rounds to the nearest integer (half-away-from-zero). |
| `sqrt` | `.sqrt() → Float` | Returns the square root. Negative inputs produce NaN. |
| `min` | `.min(other: Float) → Float` | Returns the smaller of `self` and `other`. Also accepts `Int` arguments. |
| `max` | `.max(other: Float) → Float` | Returns the larger of `self` and `other`. Also accepts `Int` arguments. |
| `clamp` | `.clamp(min: Float, max: Float) → Float` | Clamps `self` into `[min, max]`. Errors if bounds are NaN or `min > max`. |
| `pow` | `.pow(exp: Float) → Float` | Raises `self` to the power `exp`. Also accepts `Int` arguments. |

### Predicates

| Method | Signature | Description |
|--------|-----------|-------------|
| `is_nan` | `.is_nan() → Bool` | Returns `true` if the value is NaN. |
| `is_finite` | `.is_finite() → Bool` | Returns `true` if the value is not NaN and not infinite. |

### Sign

| Method | Signature | Description |
|--------|-----------|-------------|
| `signum` | `.signum() → Float` | Returns `-1.0` or `1.0` based on the IEEE 754 sign. NaN propagates. |

**Example:**

```urd
let ratio = 3.7
let floored = ratio.floor()        # 3
let ceiled = ratio.ceil()          # 4
let rounded = ratio.round()        # 4
let root = 16.0.sqrt()             # 4.0
let safe = ratio.clamp(0.0, 1.0)  # 1.0
```

---

## List Methods

Methods callable on `List` values.

### Queries

| Method | Signature | Description |
|--------|-----------|-------------|
| `len` | `.len() → Int` | Returns the number of elements. |
| `get` | `.get(index: Int) → value` | Returns the element at `index`. Negative indices count from the end. Errors if out of bounds. |
| `first` | `.first() → value` | Returns the first element. Errors on empty list. |
| `last` | `.last() → value` | Returns the last element. Errors on empty list. |
| `contains` | `.contains(val) → Bool` | Returns `true` if any element equals `val`. |

### Transformers (pure — return a new list)

| Method | Signature | Description |
|--------|-----------|-------------|
| `append` | `.append(val) → List` | Returns a new list with `val` added at the end. |
| `prepend` | `.prepend(val) → List` | Returns a new list with `val` added at the beginning. |
| `pop` | `.pop() → List` | Returns a new list with the last element removed. Errors on empty list. |
| `concat` | `.concat(other: List) → List` | Returns a new list with all elements of `other` appended. |
| `reversed` | `.reversed() → List` | Returns a new list with elements in reverse order. |
| `with` | `.with(index: Int, val) → List` | Returns a new list with the element at `index` replaced by `val`. |
| `slice` | `.slice(start: Int) → List` | Returns elements from `start` to the end. Negative indices count from the end. |
| `slice` | `.slice(start: Int, end: Int) → List` | Returns elements from `start` to `end`. Both bounds are clamped. |

### String Conversion

| Method | Signature | Description |
|--------|-----------|-------------|
| `join` | `.join() → Str` | Joins all elements into a string with no separator. |
| `join` | `.join(sep: Str) → Str` | Joins all elements into a string with `sep` between them. |

### Higher-Order Functions

| Method | Signature | Description |
|--------|-----------|-------------|
| `map` | `.map(fn(x) → y) → List` | Applies `fn` to each element and returns a new list of results. |
| `filter` | `.filter(fn(x) → Bool) → List` | Returns a new list containing only elements where `fn` returns `true`. |
| `reduce` | `.reduce(init, fn(acc, x) → acc) → value` | Folds the list left-to-right with an initial accumulator. |
| `fold` | `.fold(init, fn(acc, x) → acc) → value` | Alias for `reduce`. |
| `find` | `.find(fn(x) → Bool) → value` | Returns the first element where `fn` returns `true`, or `null` if none match. |
| `any` | `.any(fn(x) → Bool) → Bool` | Returns `true` if `fn` returns `true` for at least one element. |
| `all` | `.all(fn(x) → Bool) → Bool` | Returns `true` if `fn` returns `true` for every element. Returns `true` for empty lists. |
| `sort_by` | `.sort_by(fn(a, b) → Int or Bool) → List` | Returns a sorted list. The comparator returns negative/zero/positive `Int`, or `Bool` where `true` means `a < b`. |
| `zip` | `.zip(other: List) → List[List]` | Pairs elements from both lists. Truncates to the shorter list. Each pair is a 2-element list. |

### Aggregates

| Method | Signature | Description |
|--------|-----------|-------------|
| `min` | `.min() → Int` | Returns the smallest element (all elements must be `Int`). Returns `null` for empty list. |
| `max` | `.max() → Int` | Returns the largest element (all elements must be `Int`). Returns `null` for empty list. |
| `sum` | `.sum() → Int` | Returns the sum of all elements (all elements must be `Int`). Returns `0` for empty list. |

**Example:**

```urd
let items = ["sword", "shield", "potion"]
let count = items.len()                         # 3
let first = items.first()                       # "sword"
let with_bow = items.append("bow")              # ["sword", "shield", "potion", "bow"]

let nums = [3, 1, 4, 1, 5]
let sorted = nums.sort_by(fn(a: int, b: int) { a - b })  # [1, 1, 3, 4, 5]
let total = nums.sum()                                     # 14
let evens = nums.filter(fn(x: int) { x % 2 == 0 })       # [4]
let doubled = nums.map(fn(x: int) { x * 2 })              # [6, 2, 8, 2, 10]
```

---

## Map Methods

Methods callable on `Map` values (`:{ key: value }` literals).

### Queries

| Method | Signature | Description |
|--------|-----------|-------------|
| `get` | `.get(key: Str) → value` | Returns the value for `key`, or `null` if not found. |
| `has` | `.has(key: Str) → Bool` | Returns `true` if the map contains `key`. |
| `keys` | `.keys() → List[Str]` | Returns a sorted list of all keys. |
| `values` | `.values() → List` | Returns a list of all values, sorted by key for determinism. |
| `len` | `.len() → Int` | Returns the number of key-value pairs. |
| `is_empty` | `.is_empty() → Bool` | Returns `true` if the map has no entries. |

### Transformers (pure — return a new map)

| Method | Signature | Description |
|--------|-----------|-------------|
| `set` | `.set(key: Str, val) → Map` | Returns a new map with `key` set to `val`. Overwrites if the key exists. |
| `remove` | `.remove(key: Str) → Map` | Returns a new map with `key` removed. No-op if the key doesn't exist. |
| `merge` | `.merge(other: Map) → Map` | Returns a new map with all entries from `other` merged in. `other` wins on key conflicts. |

**Example:**

```urd
let stats = :{ hp: 100, mp: 50 }
let has_hp = stats.has("hp")             # true
let keys = stats.keys()                  # ["hp", "mp"]
let updated = stats.set("hp", 80)        # :{ hp: 80, mp: 50 }
let without_mp = stats.remove("mp")      # :{ hp: 100 }
```

---

## Range Methods

Methods callable on `Range` values (`start..end` or `start..=end`).

| Method | Signature | Description |
|--------|-----------|-------------|
| `len` | `.len() → Int` | Returns the number of integers in the range. Exclusive: `max(0, end - start)`. Inclusive: `max(0, end - start + 1)`. Returns `0` for empty/inverted ranges. |
| `contains` | `.contains(val: Int) → Bool` | Returns `true` if `val` lies within the range. Exclusive: `start <= val < end`. Inclusive: `start <= val <= end`. |

**Example:**

```urd
let r = 0..10
let length = r.len()           # 10
let has_five = r.contains(5)   # true
let has_ten = r.contains(10)   # false (exclusive)

let ri = 0..=10
let has_ten_i = ri.contains(10)  # true (inclusive)
```

---

## Extern Object Methods

Methods callable on extern object references (values provided by the host via `ExternObject`).

| Method | Signature | Description |
|--------|-----------|-------------|
| `to_string` | `.to_string() → Str` | Returns the human-readable string representation (calls `ExternObject::display()`). |
| `type_name` | `.type_name() → Str` | Returns the type name of the underlying object (e.g. `"Player"`, `"Node3D"`). |
| `fields` | `.fields() → List[Str]` | Returns a list of all available field names. |
| `cast` | `.cast(target: Str) → value` | Attempts to convert the object to the given type. The host defines what conversions are supported. |

Extern objects also support field access via dot notation (`obj.field`) and subscript syntax (`obj["field"]`), as well as field writes via subscript assignment (`obj["field"] = value`). See [Extern Values](../language/extern-values.md) for details.

**Example:**

```urd
extern player

@entry
label debug {
    let t = player.type_name()
    let f = player.fields()
    narrator: "Type: {t}, Fields: {f}"
    
    narrator: "HP: {player.hp}"
    player["hp"] = player.hp - 10
    narrator: "After hit: {player.hp}"
}
```
