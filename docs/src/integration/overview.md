# Integration Overview

Urd is a Rust library for dialogue scripting. You embed it in your game engine
by compiling `.urd` scripts into an intermediate representation, creating a
virtual machine, and driving a pull-based event loop. No threads, no callbacks,
no async runtime required.

## Adding Urd as a Dependency

Add `urd` to your `Cargo.toml`:

```toml
[dependencies]
urd = "0.1"
```

The prelude re-exports the runtime types you need for driving the VM:

```rust
use urd::prelude::*;
```

The compiler and parser live in their own modules and must be imported
separately:

```rust
use urd::compiler::Compiler;
use urd::compiler::loader::parse_source;
```

## The Integration Pattern

Every Urd integration follows the same three-step pattern:

1. **Compile** — parse a `.urd` source file into an AST, then compile the AST
   into an `IrGraph`.
2. **Create the VM** — construct a `Vm` from the compiled graph and a
   `DecoratorRegistry`.
3. **Drive the loop** — call `vm.next()` repeatedly, handling `Dialogue` and
   `Choice` events as they arrive.

The VM never calls back into your code. You pull events out of it at your own
pace — one per frame, one per click, whatever suits your engine.

## Minimal Example

```rust
use urd::prelude::*;
use urd::compiler::Compiler;
use urd::compiler::loader::parse_source;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Parse the source into an AST.
    let source = std::fs::read_to_string("story.urd")?;
    let ast = parse_source(&source)?;

    // 2. Compile the AST into an IR graph.
    let graph = Compiler::compile(&ast)?;

    // 3. Create the VM with an empty decorator registry.
    let registry = DecoratorRegistry::new();
    let mut vm = Vm::new(graph, registry)?;

    // 4. Drive the event loop.
    let mut choice: Option<usize> = None;
    loop {
        match vm.next(choice.take()) {
            VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
                // Render dialogue to your UI.
                println!("{:?}: {:?}", speakers, lines);
            }
            VmStep::Event(Event::Choice { options, .. }) => {
                // Present choices to the player.
                for (i, opt) in options.iter().enumerate() {
                    println!("  [{}] {}", i, opt.label);
                }
                // In a real game, this would come from player input.
                choice = Some(get_player_choice(&options));
            }
            VmStep::Ended => break,
            VmStep::Error(e) => return Err(e.into()),
        }
    }

    Ok(())
}

fn get_player_choice(options: &[ChoiceEvent]) -> usize {
    // Placeholder — read from stdin, UI button press, etc.
    0
}
```

## What Happens Under the Hood

When you call `vm.next()`, the VM walks through the compiled IR graph
internally. It silently executes variable assignments, conditional branches,
function calls, and other non-observable operations. It only pauses and returns
control to you when it encounters a **dialogue node** or a **choice node** —
the two event types that require the host engine to do something visible.

This means a single `vm.next()` call may execute hundreds of internal steps
before producing an event. The VM's [step budget](./budget-and-limits.md)
prevents infinite loops from hanging your game.

## Next Steps

- [Compiling Scripts](./compiling.md) — parsing, single-file and multi-file compilation
- [The Virtual Machine](./vm.md) — VM construction, configuration, and state
- [Pull-Based Execution](./pull-based-execution.md) — the execution model in detail
- [Events & Choices](./events-and-choices.md) — handling dialogue and choice events
- [Decorator Registry](./decorator-registry.md) — enriching events from Rust
- [Error Handling](./error-handling.md) — runtime and compiler errors