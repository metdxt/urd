# VmStep & VmError

The `VmStep` enum is the return type of every `Vm::next()` call. It replaces the older `Option<Result<Event, VmError>>` pattern with a self-documenting three-variant enum that pattern-matches cleanly in game engine integration code.

## `VmStep`

```rust
pub enum VmStep {
    /// The VM produced an observable event (dialogue or choice).
    Event(Event),
    /// The script has finished — no more events will be produced.
    Ended,
    /// A runtime error occurred.
    Error(VmError),
}
```

### Usage Pattern

The idiomatic way to drive the VM is a loop over `vm.next()`:

```rust
use urd::prelude::*;

let mut choice: Option<usize> = None;

loop {
    match vm.next(choice.take()) {
        VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
            // Render dialogue to screen
        }
        VmStep::Event(Event::Choice { options, .. }) => {
            // Show options to the player, collect their selection
            choice = Some(selected_index);
        }
        VmStep::Ended => break,
        VmStep::Error(e) => {
            eprintln!("runtime error: {e}");
            break;
        }
    }
}
```

### Variant Details

| Variant | When it occurs | What to do |
|---------|----------------|------------|
| `Event(event)` | The VM reached a dialogue or choice node | Process the event, then call `vm.next()` again |
| `Ended` | The script hit `end!()` or fell off the last node | Stop the loop — the script is done |
| `Error(e)` | A runtime error occurred | Log the error; decide whether to abort or attempt recovery |

### Trait Implementations

- `Debug` — all variants are printable for logging.

> **Note:** `VmStep` does not implement `Serialize`/`Deserialize`. If you need to serialize events, extract the `Event` from the `Event` variant and serialize that directly.

---

## `VmError`

`VmError` is the error type carried by `VmStep::Error`. It covers every runtime failure mode the VM can encounter. All variants implement `Display` and `Error` (via `thiserror`).

### Variants

#### `UnknownDecorator`

```rust
UnknownDecorator { name: String, node_id: NodeIndex }
```

A decorator name used in the script was not registered in the `DecoratorRegistry`. The `node_id` identifies which IR node triggered the error.

**Message:** `unknown decorator '@{name}' used at node {node_id:?}`

**Fix:** Register the decorator before creating the VM:

```rust
registry.register("color", |args| { /* ... */ });
```

---

#### `UnknownLabel`

```rust
UnknownLabel(String)
```

A `jump` targeted a label not present in the compiled graph. This should never occur after a successful compilation pass — it exists as a defensive runtime check.

**Message:** `unknown label '{0}'`

---

#### `UndefinedVariable`

```rust
UndefinedVariable(String)
```

A variable was referenced before it was declared in any visible scope.

**Message:** `undefined variable '{0}'`

---

#### `TypeError`

```rust
TypeError(String)
```

A runtime type mismatch — for example, adding a string to an integer, or calling a method with the wrong argument type.

**Message:** `type error: {0}`

---

#### `InvalidExpression`

```rust
InvalidExpression(String)
```

An AST node appeared in an expression context where it is not valid.

**Message:** `invalid expression context: {0}`

---

#### `ChoiceOutOfBounds`

```rust
ChoiceOutOfBounds { index: usize, len: usize }
```

The player (or host code) supplied a choice index that exceeds the number of available options. When this occurs, the VM re-emits the choice event so the host can retry with a valid index.

**Message:** `choice index {index} out of bounds (len={len})`

---

#### `IndexOutOfBounds`

```rust
IndexOutOfBounds { index: i64, len: usize }
```

A list subscript operation used an index outside the list's bounds. The `index` field is `i64` because Urd supports negative indexing (which may still be out of range).

**Message:** `index {index} out of bounds for list of length {len}`

---

#### `CompilerError`

```rust
CompilerError(CompilerError)
```

An error propagated from the compiler — typically encountered when constructing test graphs or when the VM is initialized from an invalid IR graph.

**Message:** `compiler error: {0}`

See [CompilerError](./compiler-error.md) for the inner variants.

---

#### `NotImplemented`

```rust
NotImplemented(String)
```

A language feature that is not yet implemented was encountered during execution. This is a temporary variant used during development; production scripts should never trigger it.

**Message:** `not yet implemented: {0}`

---

#### `ExternNotProvided`

```rust
ExternNotProvided(String)
```

A value declared with `extern` in the script was not injected by the host runtime before execution started.

**Message:** `extern value '{0}' was not provided by the host runtime before execution`

**Fix:** Provide the value before calling `vm.next()`:

```rust
vm.provide_extern("player_name", RuntimeValue::Str("Hero".into()));
```

---

#### `UnknownMethod`

```rust
UnknownMethod(String)
```

A method was called on a value type that does not support it — for example, calling `.push()` on an integer.

**Message:** `unknown method '{0}' for the given type`

---

#### `UndefinedFunction`

```rust
UndefinedFunction(String)
```

A free-function call referenced a name that is not defined in any reachable scope.

**Message:** `undefined function '{0}'`

---

#### `ConstraintViolation`

```rust
ConstraintViolation {
    decorator: String,
    constraint: String,
    actual_event: String,
}
```

A script-defined decorator was applied to an event kind it does not accept, as declared by its `<event: ...>` constraint clause. For example, a decorator constrained to `choice` events was applied to a `dialogue` node.

**Message:** `constraint violation: decorator '@{decorator}' requires a '{constraint}' event, but was applied to a '{actual_event}' event`

---

#### `NonExhaustiveMatch`

```rust
NonExhaustiveMatch(String)
```

A `match` expression at runtime had no arm that matched the scrutinee and no wildcard/default arm was present. The static analysis pass catches most of these at compile time, but dynamically-computed scrutinees can slip through.

**Message:** `non-exhaustive match: {0}`

---

#### `BudgetExceeded`

```rust
BudgetExceeded
```

The script exceeded its step budget — the maximum number of VM steps allowed without producing an event or ending. This is the infinite-loop guard.

The default budget is **1,000,000 steps**. You can adjust it:

```rust
// Raise the limit
vm.set_step_budget(Some(5_000_000));

// Disable the limit entirely (use with caution)
vm.set_step_budget(None);
```

**Message:** `execution budget exceeded: script executed too many steps without ending (possible infinite loop)`

---

#### `StackOverflow`

```rust
StackOverflow(usize)
```

The call stack depth limit was reached. The default limit is **256 frames**. This indicates deeply nested `jump ... and return` calls or recursive `let` calls in the script.

**Message:** `call stack overflow: maximum call depth ({0}) exceeded`

---

## Error Handling Strategies

### Fail Fast

The simplest strategy — treat any error as fatal:

```rust
loop {
    match vm.next(choice.take()) {
        VmStep::Event(e) => handle(e),
        VmStep::Ended => break,
        VmStep::Error(e) => return Err(e.into()),
    }
}
```

### Log and Continue

For non-critical systems (e.g., ambient NPC barks), you may want to log the error and move on:

```rust
VmStep::Error(e) => {
    log::warn!("dialogue error: {e}");
    break; // End this dialogue, but don't crash the game
}
```

### Retry on `ChoiceOutOfBounds`

`ChoiceOutOfBounds` is the one error you can recover from — the VM re-emits the choice event on the next call:

```rust
VmStep::Error(VmError::ChoiceOutOfBounds { len, .. }) => {
    // Re-prompt the player with the correct range
    choice = Some(prompt_player(len));
}
```

---

## `Vm` Methods

Beyond the core `vm.next(choice)` loop, the `Vm` type exposes several additional public methods for construction and introspection.

### `Vm::new(graph, registry)`

The primary constructor. Creates a VM that begins execution at the graph's default entry point.

```rust
let mut vm = Vm::new(graph, registry)?;
```

Returns `Err(VmError::UnknownDecorator)` if any decorator used in the script is not registered in the `DecoratorRegistry`.

---

### `Vm::new_at(graph, registry, label)`

Creates a VM that begins execution at a specific `@entry` label instead of the default entry point.

```rust
let mut vm = Vm::new_at(graph, registry, "tavern_intro")?;
```

Returns `Err(VmError::UnknownLabel(...))` if `label` is not an `@entry` label in the graph. Non-entry labels cannot be targeted — they are private to their module.

---

### `Vm::with_localizer(self, localizer) -> Self`

Builder method to attach a `Localizer` implementation to the VM. The `localizer` parameter is an `Arc<dyn Localizer>`, allowing shared ownership across threads. When a localizer is present, the VM populates the `localized_text` / `localized_label` fields on `Event::Dialogue` and `ChoiceEvent`.

```rust
let vm = Vm::new(graph, registry)?
    .with_localizer(Arc::new(my_localizer));
```

---

### `Vm::with_dice_roller(self, roller) -> Self`

Builder method to attach a custom `DiceRoller` implementation. By default the VM uses `DefaultDiceRoller` (which delegates to `rand`). Supply your own roller for deterministic testing or custom probability distributions.

```rust
let vm = Vm::new(graph, registry)?
    .with_dice_roller(SeededRoller::new(42));
```

---

### `Vm::env(&self)`

Read-only access to the current variable environment. Returns a reference to the VM's environment, which you can inspect to read variable bindings during execution.

```rust
let env = vm.env();
```

---

### `Vm::graph(&self)`

Read-only access to the compiled IR graph backing this VM instance.

```rust
let graph = vm.graph();
```

---

## Relationship to Other Types

| Type | Role |
|------|------|
| [`Event`](./event.md) | The payload inside `VmStep::Event` |
| [`CompilerError`](./compiler-error.md) | The inner type of `VmError::CompilerError` |
| [`Vm`](../integration/vm.md) | The virtual machine that produces `VmStep` values |