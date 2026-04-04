# Step Budget & Limits

Urd includes built-in safety rails that prevent runaway scripts from hanging your game. These are especially important when running user-authored or modded content.

## Step Budget

Every VM starts with a **step budget** — a cap on the number of internal operations it will execute before forcefully stopping. Each IR node the VM visits counts as one step.

The default budget is **1,000,000 steps**, which is generous enough for any reasonable script but catches infinite loops quickly.

```rust
use urd::prelude::*;

// Default: 1,000,000 steps
let mut vm = Vm::new(graph, registry)?;

// Custom budget: 10,000 steps
vm.set_step_budget(Some(10_000));

// No limit — only use in trusted/test contexts!
vm.set_step_budget(None);
```

When the budget is exhausted, `vm.next()` returns:

```rust
VmStep::Error(VmError::BudgetExceeded)
```

The error message reads:

> execution budget exceeded: script executed too many steps without ending (possible infinite loop)

### Choosing a Budget

| Context | Suggested Budget |
|---|---|
| Shipped game, trusted scripts | `Some(1_000_000)` (default) |
| Modding / user content | `Some(100_000)` or lower |
| Unit tests | `Some(10_000)` |
| Benchmarks / profiling | `None` |

The budget is consumed by *all* internal operations — assignments, branches, expression evaluations, jumps — not just observable events. A script that does heavy computation between dialogue lines will burn through its budget faster.

### Resetting the Budget

The budget is **not** automatically reset between `vm.next()` calls. It is a total lifetime cap for the VM instance. If you need to reset it (for example, between chapters), call `set_step_budget` again:

```rust
// Reset to a fresh 1M steps
vm.set_step_budget(Some(1_000_000));
```

## Call Stack Depth

Urd supports `jump ... and return`, which pushes a return address onto an internal call stack. The maximum call depth is **256 frames**.

If a script exceeds this limit — typically through unbounded mutual recursion — the VM returns:

```rust
VmStep::Error(VmError::StackOverflow(depth))
```

where `depth` is the depth at which the overflow was detected (256 by default).

### Example: What Triggers a Stack Overflow

```urd
label ping {
    jump pong and return
}

label pong {
    jump ping and return
}
```

This creates unbounded mutual recursion: `ping → pong → ping → pong → …` until the 256-frame limit is hit.

## Handling Limit Errors

Both `BudgetExceeded` and `StackOverflow` are variants of `VmError`, so they appear as `VmStep::Error(...)` in your main loop:

```rust
loop {
    match vm.next(choice.take()) {
        VmStep::Event(event) => handle_event(event),
        VmStep::Ended => break,
        VmStep::Error(VmError::BudgetExceeded) => {
            log::error!("Script exceeded step budget — possible infinite loop");
            show_error_to_player("The script encountered an error.");
            break;
        }
        VmStep::Error(VmError::StackOverflow(depth)) => {
            log::error!("Script call stack overflow at depth {depth}");
            show_error_to_player("The script encountered an error.");
            break;
        }
        VmStep::Error(e) => {
            log::error!("VM error: {e}");
            break;
        }
    }
}
```

## Security Considerations

These limits exist specifically to make it safe to run **untrusted** scripts:

- **Modding support**: If your game allows players to write or install `.urd` scripts, the step budget prevents a malicious or buggy script from freezing the game.
- **Sandboxing**: Combined with `FsLoader`'s path traversal rejection, these limits form a basic security sandbox. Scripts cannot escape their directory *or* consume unbounded CPU time.
- **Deterministic failure**: Both limits produce clear, catchable errors rather than silent hangs or crashes. Your game can always recover gracefully.

> **Rule of thumb**: If the script is yours and tested, the defaults are fine. If the script comes from users, consider lowering the budget and always handle the error cases.