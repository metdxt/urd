# RuntimeValue

`RuntimeValue` is the core value type in Urd's runtime. Every expression evaluation, variable binding, decorator argument, and event payload ultimately produces or consumes a `RuntimeValue`.

```rust
use urd::RuntimeValue;
// or via the prelude:
use urd::prelude::*;
```

## Variants

### `Null`

The absence of a value. Produced by functions that return nothing and by accessing missing map keys.

```urd
let x = null
```

### `Bool(bool)`

A boolean value — `true` or `false`.

```urd
let alive = true
let game_over = false
```

### `Int(i64)`

A 64-bit signed integer.

```urd
let gold = 100
let temperature = -15
```

### `Float(f64)`

A 64-bit double-precision floating-point number.

```urd
let ratio = 3.14
let negative = -0.5
```

### `Str(ParsedString)`

A string value with support for interpolation placeholders. By the time a `Str` reaches an event, all interpolation has been resolved to plain text.

```urd
let name = "Zara"
let greeting = "Hello, {name}!"
```

### `Dice(u8, u8)`

Dice notation — `(count, sides)`. This variant exists as an **AST/IR-level sentinel** and is **immediately converted to `Roll`** during evaluation. When the VM encounters a `Dice` node, it calls the registered `DiceRoller` and produces a `Roll(Vec<i64>)` before the value is ever bound to a variable or used in an expression.

In practice, you will **never** encounter a `Dice` value in events, the environment, or serialized state. It is an internal implementation detail of the compilation pipeline.

```urd
let attack = 2d6    # AST contains Dice(2, 6), but at runtime attack holds Roll([3, 5])
let check = 1d20    # AST contains Dice(1, 20), but at runtime check holds Roll([14])
```

### `Roll(Vec<i64>)`

The individual results of evaluating a dice expression at runtime. Each element is a single die result in the range `1..=sides`. This is the **actual runtime type** produced by dice literals — every `2d6` in your script becomes a `Roll` the moment it is evaluated.

```urd
let attack = 2d6
# At runtime, attack is Roll([3, 5]) — the Dice variant never reaches the environment
```

> **Note:** `Roll` is the runtime counterpart to the AST-level `Dice` variant. It is **not serializable** (`#[serde(skip)]`) because it is an ephemeral execution value.

### `IdentPath(Vec<String>)`

A dotted identifier path, used internally to represent variable or property references before they are resolved.

```urd
player.inventory.gold    # IdentPath(["player", "inventory", "gold"])
```

### `Range { start, end, inclusive }`

An integer range value.

- `start: i64` — the lower bound (inclusive)
- `end: i64` — the upper bound
- `inclusive: bool` — `true` for `..=` (inclusive end), `false` for `..` (exclusive end)

```urd
let exclusive = 0..10     # Range { start: 0, end: 10, inclusive: false }
let inclusive = 1..=6     # Range { start: 1, end: 6, inclusive: true }
```

**Semantics:**

- `len()`: exclusive → `max(0, end - start)`, inclusive → `max(0, end - start + 1)`
- `contains(n)`: exclusive → `start <= n && n < end`, inclusive → `start <= n && n <= end`

### `Map(HashMap<String, Box<RuntimeValue>>)`

A key-value map, produced by map literal syntax (`:{ key: value, ... }`) or the implicit `event` map passed to decorator bodies.

```urd
let stats = :{ hp: 100, mp: 50, name: "Hero" }
```

> **Note:** `Map` is **not serializable** because it may hold `Ast` nodes transitively. It is an ephemeral, execution-only value.

### `List(Vec<RuntimeValue>)`

An ordered list of runtime values.

```urd
let items = ["sword", "shield", "potion"]
let numbers = [1, 2, 3]
```

Lists are serializable as long as all their elements are themselves serializable.

**Invariant:** A `List` must never contain a `Function`, `Map`, `Roll`, `ScriptDecorator`, or `Struct` element. Use `RuntimeValue::list()` to construct lists from Rust code — it fires a `debug_assert!` in debug builds to catch violations early.

### `Function { params, body }`

A first-class function value. Functions run in an isolated environment containing only their bound parameters — no access to the outer scope (pure).

- `params: Vec<String>` — ordered parameter names (type annotations stripped at compile time)
- `body: Box<Ast>` — the function body, kept as raw AST for inline evaluation on each call

```urd
let double = fn(x: int) { x * 2 }
let result = double(21)   # 42
```

> **Note:** `Function` is **not serializable** because it holds `Ast` nodes.

### `ScriptDecorator { event_constraint, params, body }`

A script-defined decorator, stored as a first-class runtime value.

- `event_constraint: EventConstraint` — optional event-kind constraint checked at apply-time
- `params: Vec<String>` — ordered parameter names
- `body: Box<Ast>` — the decorator body, kept as raw AST

```urd
decorator mood<event: dialogue>(m) {
    :{ mood: m }
}
```

> **Note:** `ScriptDecorator` is **not serializable**. It exists only during script execution.

### `Struct { name, fields }`

A named struct instance, constructed by the VM when a call expression names a registered struct type.

- `name: String` — the struct type name (e.g. `"Character"`)
- `fields: HashMap<String, RuntimeValue>` — field values keyed by field name

```urd
struct Character {
    name: str
    name_color: str
}

const hero: Character = :{ name: "Hero", name_color: "#f5c542" }
```

> **Note:** `Struct` is **not serializable** because field values may transitively reference `Function` or `ScriptDecorator` values.

## Serialization

`RuntimeValue` derives `Serialize` and `Deserialize` (via serde), but not all variants participate:

| Variant | Serializable | Notes |
|---------|:------------:|-------|
| `Null` | ✅ | |
| `Bool` | ✅ | |
| `Int` | ✅ | |
| `Float` | ✅ | |
| `Str` | ✅ | |
| `Dice` | ✅ | AST-level only; never present in practice at runtime |
| `Roll` | ❌ | `#[serde(skip)]` — ephemeral runtime value (the actual type produced by dice literals) |
| `IdentPath` | ✅ | |
| `Range` | ✅ | |
| `Map` | ❌ | `#[serde(skip)]` — may hold `Ast` nodes |
| `List` | ✅ | Only if all elements are serializable |
| `Function` | ❌ | `#[serde(skip)]` — holds `Ast` |
| `ScriptDecorator` | ❌ | `#[serde(skip)]` — holds `Ast` |
| `Struct` | ❌ | `#[serde(skip)]` — fields may hold non-serializable values |

Attempting to serialize a skipped variant will silently omit the field. Deserialization will never reconstruct them — which is correct, because they only exist transiently during script execution and never appear in serialized `Event` payloads.

## Common Methods

### Comparison Semantics

`RuntimeValue` derives `PartialEq`. Two values are equal if they are the same variant with the same inner data. Cross-type comparisons (e.g. `Int(1) == Float(1.0)`) return `false` — there is no implicit coercion.

## Constructing from Rust

When integrating Urd into a game engine, you'll most commonly construct `RuntimeValue`s to provide `extern` values:

```rust
use urd::RuntimeValue;

// Primitives
let gold = RuntimeValue::Int(100);
let name = RuntimeValue::Str("Hero".into());
let alive = RuntimeValue::Bool(true);

// Lists (use the constructor for debug-mode invariant checks)
let items = RuntimeValue::list(vec![
    RuntimeValue::Str("sword".into()),
    RuntimeValue::Str("shield".into()),
]);
```

## Pattern Matching from Rust

When processing events, pattern-match on the variants to extract data:

```rust
use urd::{RuntimeValue, Event, VmStep};

match vm.next(None) {
    VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
        for speaker in &speakers {
            if let RuntimeValue::Struct { fields, .. } = speaker {
                if let Some(RuntimeValue::Str(name)) = fields.get("name") {
                    println!("{}: ", name);
                }
            }
        }
        for line in &lines {
            if let RuntimeValue::Str(text) = line {
                println!("  {}", text);
            }
        }
    }
    _ => {}
}
```
