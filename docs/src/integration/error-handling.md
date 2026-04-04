# Error Handling

Urd surfaces errors through two types: `VmError` for runtime failures and
`CompilerError` for compilation failures. Both are designed to be actionable —
every variant tells you *what* went wrong and carries enough context to log a
meaningful message or recover gracefully.

## `VmError` Variants

`VmError` is returned inside `VmStep::Error(e)` during execution. Here is the
complete list of variants:

### `UnknownDecorator`

A `@decorator` name used in the script was not registered in the
`DecoratorRegistry` and is not a script-defined decorator.

```rust
VmError::UnknownDecorator { name: String, node_id: NodeIndex }
```

**Fix:** register the decorator before constructing the VM, or define it in the
script.

### `UnknownLabel`

A `jump` targeted a label not present in the compiled graph. This should not
occur after a successful compilation pass, but is kept as a defensive runtime
check.

```rust
VmError::UnknownLabel(String)
```

**Fix:** ensure the label exists in the script (check for typos).

### `UndefinedVariable`

A variable was read before it was assigned a value.

```rust
VmError::UndefinedVariable(String)
```

**Fix:** assign the variable before referencing it, or provide it as an extern.

### `TypeError`

A runtime type mismatch occurred — for example, adding a string to an integer.

```rust
VmError::TypeError(String)
```

The message describes the specific mismatch.

### `InvalidExpression`

An AST node appeared in an expression context where it is not valid.

```rust
VmError::InvalidExpression(String)
```

### `ChoiceOutOfBounds`

The host passed a choice index to `vm.next(Some(index))` that exceeds the
number of available options.

```rust
VmError::ChoiceOutOfBounds { index: usize, len: usize }
```

**Fix:** validate the index before passing it. Valid indices are `0..len`.

### `IndexOutOfBounds`

A list subscript in the script was out of range.

```rust
VmError::IndexOutOfBounds { index: i64, len: usize }
```

### `CompilerError`

An error propagated from the compiler (e.g. when constructing test graphs at
runtime).

```rust
VmError::CompilerError(CompilerError)
```

### `NotImplemented`

A language feature that is not yet implemented was encountered at runtime.

```rust
VmError::NotImplemented(String)
```

### `ExternNotProvided`

A value declared with `extern` in the script was not injected by the host
before execution reached the point where it was read.

```rust
VmError::ExternNotProvided(String)
```

**Fix:** call `vm.provide_extern(name, value)` before the first `vm.next()`.

### `UnknownMethod`

A method was called on a type that does not support it — for example,
`.push()` on an integer.

```rust
VmError::UnknownMethod(String)
```

### `UndefinedFunction`

A free-function call referenced a name that is not defined in any reachable
scope.

```rust
VmError::UndefinedFunction(String)
```

### `ConstraintViolation`

A script-defined decorator was applied to an event kind it does not accept, as
declared by its `<event: ...>` constraint clause.

```rust
VmError::ConstraintViolation {
    decorator: String,
    constraint: String,
    actual_event: String,
}
```

For example, a decorator constrained to `choice` events was applied to a
`dialogue` node.

### `NonExhaustiveMatch`

A `match` expression had no arm that matched the scrutinee and no wildcard or
default arm was present.

```rust
VmError::NonExhaustiveMatch(String)
```

**Fix:** add a `_ =>` wildcard arm to the match block.

### `BudgetExceeded`

The script executed more steps than the configured budget allows (default
1,000,000). This usually indicates an infinite loop.

```rust
VmError::BudgetExceeded
```

**Fix:** review the script for infinite loops, or raise the budget with
`vm.set_step_budget(Some(n))`.

### `StackOverflow`

The call stack depth limit was reached (default 256 frames). This indicates
unbounded recursion via `jump ... and return`.

```rust
VmError::StackOverflow(usize)  // the depth that was exceeded
```

---

## `CompilerError` Variants

`CompilerError` is returned from `Compiler::compile()` and its variants. These
are *compile-time* errors — they fire before the VM even starts.

### `UnknownLabel`

A `jump` statement targets a label that was never defined.

```rust
CompilerError::UnknownLabel(String)
```

### `DuplicateLabel`

Two labels with the same name were declared in the same compilation unit.

```rust
CompilerError::DuplicateLabel(String)
```

### `InvalidStatement`

An AST node appeared at statement level where it is not permitted.

```rust
CompilerError::InvalidStatement(String)
```

### `ModuleLoadError`

Failed to load a module during import resolution.

```rust
CompilerError::ModuleLoadError { path: String, message: String }
```

### `CircularImport`

A circular import chain was detected (e.g. `A` imports `B` which imports `A`).

```rust
CompilerError::CircularImport(String)
```

---

## Best Practices

### Always match on `VmStep::Error`

Never ignore the error arm. At minimum, log and break:

```rust
loop {
    match vm.next(choice.take()) {
        VmStep::Event(event) => handle(event),
        VmStep::Ended => break,
        VmStep::Error(e) => {
            eprintln!("[urd] runtime error: {e}");
            break;
        }
    }
}
```

### Handle compiler errors before constructing the VM

```rust
let graph = match Compiler::compile(&ast) {
    Ok(g) => g,
    Err(CompilerError::UnknownLabel(label)) => {
        eprintln!("Script references unknown label '{label}'");
        return;
    }
    Err(CompilerError::DuplicateLabel(label)) => {
        eprintln!("Label '{label}' defined more than once");
        return;
    }
    Err(e) => {
        eprintln!("Compilation failed: {e}");
        return;
    }
};
```

### Graceful recovery in games

For shipped games running player-authored scripts, you may want to recover
rather than crash:

```rust
VmStep::Error(VmError::BudgetExceeded) => {
    // Show an in-game message instead of crashing
    show_toast("Script took too long and was stopped.");
    break;
}
VmStep::Error(VmError::ExternNotProvided(name)) => {
    // Provide a fallback and retry
    vm.provide_extern(&name, RuntimeValue::Str("???".into()));
    continue;
}
```

### Use `Display` for user-facing messages

Both `VmError` and `CompilerError` implement `std::fmt::Display` (via the
`thiserror` crate), so you can use them directly in format strings:

```rust
Err(e) => eprintln!("Error: {e}"),
```

The messages are human-readable and include the relevant context (variable
names, label names, indices, etc.).

### Defense in depth

Even if you trust your scripts, always:

1. **Keep the step budget enabled** — it's your last line of defense against
   infinite loops.
2. **Match exhaustively** on `VmStep` — new variants may be added in the
   future.
3. **Validate choice indices** on the host side before passing them to
   `vm.next(Some(index))` to avoid `ChoiceOutOfBounds`.
4. **Provide all externs early** — call `provide_extern` for every declared
   extern before the first `vm.next()` call.