# CompilerError

`CompilerError` is the error type returned by the Urd compiler when it fails to produce a valid IR graph from a parsed AST. These errors represent structural problems that prevent compilation from completing.

## Import

```rust
use urd::CompilerError;
// or
use urd::prelude::*;
```

## Variants

### `UnknownLabel(String)`

A `jump` statement targeted a label that was never defined in any compilation unit (including imports).

```urd
@entry
label start {
    narrator: "Off we go!"
    jump nonexistent_label    # ← no such label
}
```

**Error message:**

```
jump to unknown label `nonexistent_label`
```

This error is emitted during the final compilation phase, after all modules have been loaded and all label stubs have been pre-allocated. If the label name still cannot be resolved at that point, it is a hard error.

> **Note:** The static analysis pass `labels` catches this earlier as a *warning* (with a typo suggestion). If you see the `CompilerError` variant, it means compilation was attempted despite the analysis warning.

---

### `DuplicateLabel(String)`

Two labels with the same name were declared in the same compilation unit, or a name collision occurred between an imported module and the importing script.

```urd
label greeting {
    narrator: "Hello."
    end!()
}

label greeting {    # ← duplicate
    narrator: "Hi."
    end!()
}
```

**Error message:**

```
duplicate label definition `greeting`
```

Label names must be unique within a single compilation unit. When importing another module, its labels are namespaced (e.g., `tavern::enter_tavern`), so cross-module collisions are rare — but importing specific symbols with `import (greeting) from "other.urd"` can introduce duplicates if the importing file also defines a `greeting` label.

---

### `InvalidStatement(String)`

An AST node appeared at statement level where it is not permitted. This typically means the parser produced a valid AST node, but the compiler encountered it in a context where it cannot generate IR.

**Error message:**

```
invalid statement: <description>
```

This is a catch-all for structural problems that don't fit into a more specific category. Examples include unsupported expression forms in statement position or internal inconsistencies in the AST.

---

### `ModuleLoadError { path, message }`

A file referenced by an `import` statement could not be loaded by the configured `FileLoader`.

```urd
import "missing_module.urd" as missing
```

**Error message:**

```
module load error for 'missing_module.urd': file not found
```

**Fields:**

| Field | Type | Description |
|-------|------|-------------|
| `path` | `String` | The import path that failed to load |
| `message` | `String` | Human-readable description of the failure |

Common causes:
- The file does not exist on disk (when using `FsLoader`)
- The file was not registered (when using `MemLoader` in tests)
- A filesystem permission error
- The file path is relative and the working directory is wrong

---

### `CircularImport(String)`

A circular import was detected during module resolution.

```
main.urd  ──import "tavern.urd" as tavern──▶  tavern.urd
tavern.urd ──import "main.urd"  as main  ──▶  main.urd   (circular!)
```

**Error message:**

```
circular import detected for 'main.urd'
```

> **Important:** Despite being an error *variant*, circular imports are actually **handled gracefully** by the Urd compiler. The 4-phase flat compiler pre-allocates `NodeIndex` stubs for every label in both modules before any IR is emitted, so cross-module `jump` targets resolve cleanly even in circular import scenarios.
>
> In practice, you will rarely see this error surface. The compiler detects the cycle, breaks it, and proceeds. This variant exists as a defensive measure for edge cases where the cycle cannot be resolved.

See the [Circular Imports example](../examples/circular-imports.md) for a working demonstration of circular imports in action.

---

## Usage in the VM

`CompilerError` also appears as a variant inside `VmError`:

```rust
pub enum VmError {
    // ...
    CompilerError(CompilerError),
    // ...
}
```

This allows compiler errors to propagate through the VM when test graphs or dynamically compiled scripts fail. In normal usage (compile first, then run), you will encounter `CompilerError` at compile time, not at VM runtime.

## Display and Error Trait

`CompilerError` implements both `Display` and `std::error::Error` (via `thiserror`), so it integrates cleanly with Rust's `?` operator and error-handling ecosystem:

```rust
let graph = urd::compiler::Compiler::compile(&ast)?;
// CompilerError is automatically converted if compilation fails
```

## Summary

| Variant | Cause |
|---------|-------|
| `UnknownLabel(String)` | Jump target not found during compilation |
| `DuplicateLabel(String)` | Two labels with the same name |
| `InvalidStatement(String)` | Statement not allowed in context |
| `ModuleLoadError { path, message }` | File couldn't be loaded |
| `CircularImport(String)` | Circular import detected (handled gracefully) |