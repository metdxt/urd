# Prelude

The `urd::prelude` module re-exports every type a game engine integration typically needs. A single wildcard import gets you started:

```rust
use urd::prelude::*;
```

## Exported Types

```rust
pub use urd::{
    ChoiceEvent, CompilerError, DecoratorRegistry, DefaultDiceRoller,
    DiceRoller, Event, ExternHandle, ExternObject, FileLoader,
    FromRuntimeValue, FsLoader, IntoRuntimeValue, IrGraph, Localizer,
    MemLoader, RuntimeValue, Vm, VmError, VmStep,
};
// The `ExternObject` derive macro is also available in the macro namespace.
```

### Quick Reference

| Type | Role | Details |
|------|------|---------|
| [`Vm`](./vm-step.md) | The virtual machine that drives script execution | Call `Vm::new(graph, registry)` to create, then loop on `vm.next(choice)` |
| [`VmStep`](./vm-step.md) | Return type of `Vm::next` — either an event, end-of-script, or error | Pattern-match on `Event`, `Ended`, `Error` |
| [`VmError`](./vm-step.md) | Runtime error type carried by `VmStep::Error` | 16 variants covering everything from type errors to stack overflow |
| [`Event`](./event.md) | An observable event emitted by the VM — dialogue or choice | Two variants: `Dialogue { ... }` and `Choice { ... }` |
| [`ChoiceEvent`](./event.md) | A single option inside a `Choice` event | Contains the label text, decorator fields, localization data |
| [`RuntimeValue`](./runtime-value.md) | The universal value type used throughout the runtime | Null, Bool, Int, Float, Str, Dice, List, Map, Struct, Extern, and more |
| `ExternObject` | Trait for host game objects exposed to scripts | Implement for any type you want scripts to read/write fields on. Also available as `#[derive(ExternObject)]`. |
| `ExternHandle` | Reference-counted handle to a `dyn ExternObject` | Stored inside `RuntimeValue::Extern`. Cloning is cheap (Arc). |
| `IntoRuntimeValue` | Trait: convert Rust type → `RuntimeValue` | Implemented for primitives, `String`, `Option<T>`, `Vec<T>` |
| `FromRuntimeValue` | Trait: convert `RuntimeValue` → Rust type | Implemented for primitives, `String`, `Option<T>`, `Vec<T>` |
| [`IrGraph`](../integration/vm.md) | The compiled intermediate representation of a script | Produced by `Compiler::compile`, consumed by `Vm::new` |
| [`DecoratorRegistry`](../integration/decorator-registry.md) | Registry for host-side decorator handlers | Register Rust closures that receive decorator arguments and return field maps |
| [`CompilerError`](./compiler-error.md) | Error type from the compilation phase | Unknown labels, duplicate labels, module load failures, circular imports |
| [`DiceRoller`](../integration/vm.md) | Trait for custom dice-rolling implementations | Implement to control how `NdM` dice expressions are resolved at runtime |
| [`DefaultDiceRoller`](../integration/vm.md) | Built-in `DiceRoller` using a thread-local RNG | The default roller used when no custom roller is attached |
| [`FileLoader`](../integration/file-loaders.md) | Trait for loading script files during import resolution | Implement this to control how `import "path"` resolves |
| [`FsLoader`](../integration/file-loaders.md) | Built-in `FileLoader` that reads from the filesystem | The default loader used by `quest run` |
| [`MemLoader`](../integration/file-loaders.md) | Built-in `FileLoader` backed by an in-memory map | Useful for testing, embedding scripts in binaries, or WASM targets |
| [`Localizer`](../localization/implementing-localizer.md) | Trait for plugging translated text into the VM | Implement to provide Fluent (or any other) translations at runtime |

## Minimal Example

```rust
use urd::prelude::*;
use urd::compiler::Compiler; // Not in the prelude — import explicitly

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Load and compile
    let src = std::fs::read_to_string("dialogue.urd")?;
    let ast = urd::compiler::loader::parse_source(&src)?;
    let graph = Compiler::compile(&ast)?;

    // 2. Create the VM with an empty decorator registry
    let registry = DecoratorRegistry::new();
    let mut vm = Vm::new(graph, registry)?;

    // 3. Run the event loop
    let mut choice: Option<usize> = None;
    loop {
        match vm.next(choice.take()) {
            VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
                for line in &lines {
                    println!("{}: {}", speakers[0], line);
                }
            }
            VmStep::Event(Event::Choice { options, .. }) => {
                for (i, opt) in options.iter().enumerate() {
                    println!("  [{}] {}", i, opt.label);
                }
                // In a real game, get the player's selection here
                choice = Some(0);
            }
            VmStep::Ended => {
                println!("--- Script finished ---");
                break;
            }
            VmStep::Error(e) => {
                eprintln!("Runtime error: {e}");
                break;
            }
        }
    }

    Ok(())
}
```

## When to Use the Prelude vs. Direct Imports

The prelude is designed for application code — game engines, CLI tools, test harnesses — where convenience matters more than import precision.

If you are writing a library that depends on `urd`, prefer importing only the specific types you need:

```rust
use urd::{Vm, VmStep, Event, RuntimeValue};
```

This keeps your public API surface clean and avoids re-exporting symbols your users don't need.