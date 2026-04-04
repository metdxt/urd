# File Loaders

Urd's multi-file compilation system uses **file loaders** to resolve `import` statements at compile time. The loader is consulted during compilation — the entire multi-file graph is merged into a single `IrGraph` before the VM starts.

## The `FileLoader` Trait

All loaders implement the `FileLoader` trait:

```rust
pub trait FileLoader: std::fmt::Debug {
    /// Load the source text at `path`.
    ///
    /// Returns the raw Urd source as a `String` on success,
    /// or a human-readable error message on failure.
    fn load(&self, path: &str) -> Result<String, String>;
}
```

The `path` argument is exactly the string literal from the `import` statement, unmodified. It's the loader's responsibility to resolve it to actual source text.

## `FsLoader` — Filesystem-Backed

`FsLoader` resolves imports relative to a base directory on the native filesystem:

```rust
use urd::prelude::*;
use urd::vm::loader::FsLoader;

let loader = FsLoader::new("./scripts");
let ast = urd::compiler::loader::parse_source(&source)?;
let graph = Compiler::compile_with_loader(&ast, &loader)?;
```

### Security

`FsLoader` includes built-in protections against path traversal attacks:

- **Rejects `..` components** — `import "../../../etc/passwd"` is an immediate error
- **Rejects absolute paths** — `import "/etc/passwd"` is rejected before any I/O
- **Canonicalizes paths** — after resolving, the canonical path is checked to ensure it still falls within the base directory, preventing symlink escapes

This makes `FsLoader` safe to use with untrusted script content. An attacker cannot use import statements to read files outside the script root.

```urd
--- These are all rejected by FsLoader:
import "../secret.urd" as secret
import "/etc/passwd" as passwd
import "symlink_to_outside.urd" as escape
```

## `MemLoader` — In-Memory

`MemLoader` stores source strings in a `HashMap`, keyed by module path. It's ideal for testing and for environments where scripts are embedded in binary assets:

```rust
use urd::vm::loader::MemLoader;

let mut loader = MemLoader::new();
loader.add("characters.urd", r#"
    label greet_alice {
        Alice: "Hello!"
    }
"#);
loader.add("items.urd", r#"
    label show_inventory {
        Narrator: "You have nothing."
    }
"#);

let main_source = r#"
    import "characters.urd" as chars
    import "items.urd" as items

    label start {
        jump chars.greet_alice
    }
"#;

let ast = urd::compiler::loader::parse_source(main_source)?;
let graph = Compiler::compile_with_loader(&ast, &loader)?;
```

### Method Chaining

`MemLoader::add` returns `&mut Self`, so you can chain registrations:

```rust
let mut loader = MemLoader::new();
loader
    .add("a.urd", source_a)
    .add("b.urd", source_b)
    .add("c.urd", source_c);
```

## Custom Loaders

Implement `FileLoader` for your own source backend to integrate with game engine asset systems, databases, or network resources:

```rust
use urd::vm::loader::FileLoader;

/// Loads Urd scripts from a game engine's asset bundle.
#[derive(Debug)]
struct AssetBundleLoader {
    bundle: AssetBundle,
}

impl FileLoader for AssetBundleLoader {
    fn load(&self, path: &str) -> Result<String, String> {
        self.bundle
            .read_text(path)
            .map_err(|e| format!("asset load failed for '{}': {}", path, e))
    }
}
```

### Examples of Custom Backends

| Backend | Use Case |
|---------|----------|
| Godot `res://` loader | Load from Godot's virtual filesystem |
| SQLite loader | Scripts stored in a database |
| HTTP loader | Fetch scripts from a content server |
| Encrypted loader | Decrypt scripts from a protected archive |

## When Loaders Are Used

Loaders are only consulted at **compile time**, not at runtime:

1. The main script is parsed into an AST
2. `Compiler::compile_with_loader` walks the AST
3. When it encounters an `import` statement, it calls `loader.load(path)`
4. The loaded source is parsed and compiled recursively
5. The resulting IR is merged into the main graph
6. The final `IrGraph` is self-contained — no loader is needed at runtime

This means:
- The VM never touches the loader
- All imports are resolved before execution begins
- Circular imports are detected and reported as `CompilerError::CircularImport`
- Diamond dependencies (A imports B and C, both of which import D) are handled correctly — D is compiled once and reused

## Error Handling

If a loader fails, compilation produces a `CompilerError::ModuleLoadError`:

```rust
match Compiler::compile_with_loader(&ast, &loader) {
    Ok(graph) => { /* success */ }
    Err(CompilerError::ModuleLoadError { path, message }) => {
        eprintln!("Failed to load '{}': {}", path, message);
    }
    Err(CompilerError::CircularImport(path)) => {
        eprintln!("Circular import detected: {}", path);
    }
    Err(e) => {
        eprintln!("Compilation error: {}", e);
    }
}
```

## Choosing a Loader

| Loader | Best For |
|--------|----------|
| No loader (`Compiler::compile`) | Single-file scripts with no imports |
| `FsLoader` | CLI tools, dev workflows, standalone apps |
| `MemLoader` | Unit tests, embedded scripts, build-time compilation |
| Custom `FileLoader` | Game engine integration, exotic asset pipelines |

If your scripts don't use `import` statements, you don't need a loader at all — just use `Compiler::compile`.