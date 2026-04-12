# The Virtual Machine

The `Vm` is the heart of Urd's runtime. It owns a compiled IR graph and walks it node-by-node, executing assignments, evaluating expressions, and pausing only when it reaches an observable event (dialogue or choice) that the host engine needs to handle.

## Creating a VM

```rust
use urd::prelude::*;
use urd::compiler::Compiler;

let graph = Compiler::compile(&ast)?;
let registry = DecoratorRegistry::new();
let mut vm = Vm::new(graph, registry)?;
```

`Vm::new` performs several up-front tasks:

1. **Decorator validation** — walks every `Dialogue` and `Choice` node in the graph and checks that all `@decorator` names are registered in the provided `DecoratorRegistry` (or defined as script decorators). Returns `VmError::UnknownDecorator` on the first unregistered name.
2. **Exit-scope map** — pre-computes the `Next` successor of every `ExitScope` node so that returning from a label block is a constant-time operation.

The result is a `Vm` that is ready to execute. All the expensive work happens here, not during the step loop.

## VM State

The VM is **stateful**. It holds:

| Component | Description |
|---|---|
| **Cursor** | The current node in the IR graph (or `None` when execution has ended). |
| **Environment** | All runtime variables, extern bindings, and the dice roller. |
| **Call stack** | Return addresses for `jump ... and return` calls (max depth: 256). |
| **Pending choice** | Tracks whether the last event was a `Choice` awaiting a player response. |
| **Step budget** | Remaining steps before `BudgetExceeded` fires (default: 1,000,000). |

Because the VM is stateful, you cannot rewind or fork execution without creating a new VM from the same graph. The graph itself (`IrGraph`) is immutable and can be shared.

## Advancing Execution

```rust
let step = vm.next(None);       // no choice — advance normally
let step = vm.next(Some(0));    // provide choice index 0
```

`vm.next(choice)` is the only method you call in the main loop. It advances the VM through the IR graph, silently executing all intermediate nodes (assignments, branches, function calls), and returns when one of three things happens:

- **`VmStep::Event(event)`** — a `Dialogue` or `Choice` node was reached.
- **`VmStep::Ended`** — the script has finished executing.
- **`VmStep::Error(e)`** — a runtime error occurred.

See [Pull-Based Execution](./pull-based-execution.md) for a deeper explanation of this model.

## Inspecting VM State

### Reading the environment

```rust
let env = vm.env();
// env exposes the current variable bindings, extern values, etc.
```

`vm.env()` returns an immutable reference to the VM's `Environment`. This is useful for debugging, inspecting variable state between events, or reading computed values that the script has set.

### Reading the compiled graph

```rust
let graph = vm.graph();
// graph is the immutable IrGraph — useful for introspection or tooling
```

`vm.graph()` returns a reference to the compiled `IrGraph`. Since the graph is immutable during execution, this is always safe to call.

## Injecting Extern Values

Scripts can declare values they expect the host to provide:

```urd
extern player_name: str
extern difficulty: int
```

From Rust, inject them before (or during) execution:

```rust
vm.provide_extern("player_name", RuntimeValue::Str("Alice".into()));
vm.provide_extern("difficulty", RuntimeValue::Int(3));
```

`provide_extern` can be called at any point — before the first `vm.next()` or between steps. If the script reads an extern that hasn't been provided yet, the VM returns `VmError::ExternNotProvided`.

See [Extern Values](./extern-values.md) for more details.

## Step Budget

```rust
vm.set_step_budget(Some(500_000));   // custom limit
vm.set_step_budget(None);            // no limit (dangerous in production!)
```

The default budget is **1,000,000 steps**. Each internal VM operation (assignment, branch, expression evaluation) counts as one step. When the budget is exhausted, `vm.next()` returns `VmStep::Error(VmError::BudgetExceeded)`.

This is a safety rail against infinite loops in untrusted scripts. See [Step Budget & Limits](./budget-and-limits.md) for full details.

## Builder Pattern

The VM supports a builder-style chain for attaching a localizer:

```rust
use std::sync::Arc;

let vm = Vm::new(graph, registry)?
    .with_localizer(Arc::new(my_localizer));
```

`with_localizer` attaches a `Localizer` implementation that automatically populates the `localized_text` field on `Dialogue` events and the `localized_label` field on `ChoiceEvent` options. Call it before the first `vm.next()` invocation.

See the [Localization](../localization/overview.md) section for how to implement a `Localizer`.

## Summary

| Method | Purpose |
|---|---|
| `Vm::new(graph, registry)` | Create and validate a new VM |
| `vm.next(choice)` | Advance to the next event, end, or error |
| `vm.env()` | Read the current variable environment |
| `vm.graph()` | Read the compiled IR graph |
| `vm.provide_extern(name, value)` | Inject a host-provided value |
| `vm.set_step_budget(budget)` | Set the infinite-loop guard limit |
| `vm.with_localizer(loc)` | Attach a localizer (builder pattern) |
| `Vm::new_at(graph, registry, label)` | Create a VM that starts execution at a specific `@entry` label |
| `vm.with_dice_roller(roller)` | Attach a custom dice roller (builder pattern) |