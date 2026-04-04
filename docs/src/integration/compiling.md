# Compiling Scripts

Before the VM can execute anything, your Urd source code must be parsed into an
AST and then compiled into an `IrGraph` — the intermediate representation the
VM walks at runtime. This chapter covers every step of that pipeline.

## Parsing Source

The `parse_source` function takes a raw source string and produces an `Ast`:

```rust
use urd::compiler::loader::parse_source;

let source = std::fs::read_to_string("story.urd")?;
let ast = parse_source(&source).map_err(|e| format!("parse error: {e}"))?;
```

`parse_source` returns a `Result<Ast, String>`. The error string contains a
human-readable description of the syntax error, including line and column
information when available.

## Single-File Compilation

For scripts that don't use `import` statements, `Compiler::compile` is all you
need:

```rust
use urd::prelude::*;

let graph = Compiler::compile(&ast)?;
```

The compiler performs two passes:

1. **Label scan** — collects all label names and pre-allocates placeholder nodes.
2. **IR emission** — walks the AST top-down, emitting `IrNode` values into the graph.

The result is an immutable `IrGraph` ready to be handed to `Vm::new`.

### Errors

`Compiler::compile` returns `Result<IrGraph, CompilerError>`. You should handle
these variants:

| Variant | When it fires |
|---------|---------------|
| `CompilerError::UnknownLabel(name)` | A `jump` targets a label that was never defined. |
| `CompilerError::DuplicateLabel(name)` | Two labels share the same name in one compilation unit. |
| `CompilerError::InvalidStatement(msg)` | An expression-only AST node appears where a statement is expected. |

```rust
match Compiler::compile(&ast) {
    Ok(graph) => { /* proceed */ }
    Err(CompilerError::UnknownLabel(name)) => {
        eprintln!("Script references undefined label '{name}'");
    }
    Err(CompilerError::DuplicateLabel(name)) => {
        eprintln!("Label '{name}' is defined more than once");
    }
    Err(e) => {
        eprintln!("Compilation failed: {e}");
    }
}
```

## Named Compilation (Localization IDs)

If you plan to localize your game's dialogue, use `Compiler::compile_named`
instead. It generates stable localization IDs for every dialogue and choice node:

```rust
let graph = Compiler::compile_named(&ast, "intro")?;
```

The `file_stem` argument should be the filename without its extension — for
example, `"intro"` for `intro.urd`. The compiler uses this stem as a prefix
when generating `loc_id` values on dialogue, choice, and option nodes.

When you don't use `compile_named`, every node's `loc_id` is `None`.

## Multi-File Compilation

Scripts that use `import` statements need a **file loader** so the compiler can
resolve and fetch imported modules:

```rust
use urd::prelude::*;
use urd::vm::loader::FsLoader;

let loader = FsLoader::new("./scripts");
let graph = Compiler::compile_with_loader(&ast, &loader)?;
```

The compiler recursively resolves imports, parsing and compiling each module
before merging it into the main graph. It tracks modules in progress to detect
circular imports.

### Additional Error Variants

Multi-file compilation can produce two extra error variants:

| Variant | When it fires |
|---------|---------------|
| `CompilerError::ModuleLoadError { path, message }` | The loader failed to fetch a module (file not found, I/O error, etc.). |
| `CompilerError::CircularImport(path)` | Module A imports B which imports A (directly or transitively). |

```rust
match Compiler::compile_with_loader(&ast, &loader) {
    Err(CompilerError::ModuleLoadError { path, message }) => {
        eprintln!("Failed to load module '{path}': {message}");
    }
    Err(CompilerError::CircularImport(path)) => {
        eprintln!("Circular import detected: '{path}'");
    }
    // ... other variants ...
    # _ => {}
}
```

## File Loaders

Two loaders ship with Urd. See the [File Loaders](./file-loaders.md) chapter
for full details.

**`FsLoader`** — filesystem-backed, for standalone tools and native games:

```rust
use urd::vm::loader::FsLoader;

let loader = FsLoader::new("./scripts");
```

**`MemLoader`** — in-memory, for tests and embedded sources:

```rust
use urd::vm::loader::MemLoader;

let mut loader = MemLoader::new();
loader.add("helpers.urd", include_str!("scripts/helpers.urd"));
```

## Inspecting the IrGraph

The compiled `IrGraph` is an immutable data structure. You can inspect it for
debugging or tooling purposes:

```rust
let graph = Compiler::compile(&ast)?;

// The graph is a petgraph-backed directed graph of IrNode values.
// You can examine it, serialize it, or pass it to visualization tools.
println!("Graph has {} nodes", graph.node_count());
```

The graph is cheap to clone (it's `Clone`) and can be shared across multiple VM
instances if you want to run the same script concurrently with different state.

## Putting It Together

A complete compile pipeline for a multi-file, localized project:

```rust
use urd::prelude::*;
use urd::compiler::loader::parse_source;
use urd::vm::loader::FsLoader;

fn compile_project(entry: &str, scripts_dir: &str) -> Result<IrGraph, Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(format!("{scripts_dir}/{entry}.urd"))?;
    let ast = parse_source(&source).map_err(|e| format!("parse error: {e}"))?;

    let loader = FsLoader::new(scripts_dir);
    let graph = Compiler::compile_with_loader(&ast, &loader)?;

    Ok(graph)
}
```

> **Tip:** Compilation happens once at load time (or even at build time). The
> resulting `IrGraph` is what you ship or cache — it's the only thing the VM
> needs at runtime.