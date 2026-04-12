# Pull-Based Execution

Urd uses a **pull-based** execution model. The host engine drives the dialogue
loop — the VM never calls back into your code unprompted, never spawns threads,
and never requires async runtimes. You call `vm.next()`, and the VM hands you
the next observable event. That's it.

This design makes Urd trivial to integrate with any game engine, regardless of
its architecture.

## How It Works

When you call `vm.next(choice)`, the VM walks through the compiled IR graph
internally. It silently executes variable assignments, evaluates expressions,
resolves branches, and follows jumps — all in a single synchronous call. The VM
only **pauses** and returns control to you when it encounters one of two things:

1. **A dialogue node** → returns `VmStep::Event(Event::Dialogue { .. })`
2. **A choice node** → returns `VmStep::Event(Event::Choice { .. })`

Everything else — math, control flow, function calls, decorator evaluation — is
handled internally and never surfaces as an event.

```rust
loop {
    match vm.next(None) {
        VmStep::Event(event) => {
            // The VM paused here. Process the event, then call next() again.
        }
        VmStep::Ended => break,
        VmStep::Error(e) => panic!("runtime error: {e}"),
    }
}
```

## No Threads, No Callbacks, No Async

Traditional dialogue systems often use one of these patterns:

| Pattern | Problem |
|---------|---------|
| Callback-based | Inversion of control; hard to reason about lifetimes |
| Thread-based | Synchronisation headaches; not available on all platforms |
| Async/await | Requires a runtime; infectious across your codebase |
| Coroutine-based | Language-specific; often allocation-heavy |

Urd avoids all of them. The VM is a plain Rust struct with no hidden threads, no
`Future` implementations, no channel receivers. You own it, you call it, you get
a result back synchronously.

## Frame-by-Frame Integration

In a typical game engine update loop, you call `vm.next()` when you're ready for
the next dialogue beat — not every frame:

```rust
// Pseudocode for a game engine integration
fn on_dialogue_advance_pressed(&mut self) {
    match self.vm.next(self.pending_choice.take()) {
        VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
            self.ui.show_dialogue(&speakers, &lines);
        }
        VmStep::Event(Event::Choice { options, .. }) => {
            self.ui.show_choices(&options);
            // Don't call next() again until the player picks one
        }
        VmStep::Ended => {
            self.ui.close_dialogue();
        }
        VmStep::Error(e) => {
            log::error!("Dialogue error: {e}");
            self.ui.close_dialogue();
        }
    }
}

fn on_choice_selected(&mut self, index: usize) {
    self.pending_choice = Some(index);
    self.on_dialogue_advance_pressed();
}
```

The key insight: **you control the pacing**. Whether you advance on a button
press, after a timer, or after an animation finishes — the VM doesn't care. It
sits idle until you call `next()`.

## The Choice Flow

Choices require a two-step interaction:

1. **First `next()` call** — the VM returns `Event::Choice { options, .. }`.
   Display these options to the player.
2. **Second `next()` call** — pass the player's selection as
   `Some(index)` where `index` is 0-based.

```rust
let mut choice: Option<usize> = None;

loop {
    match vm.next(choice.take()) {
        VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
            render_dialogue(&speakers, &lines);
            wait_for_advance();
        }
        VmStep::Event(Event::Choice { options, .. }) => {
            render_choices(&options);
            choice = Some(get_player_selection(&options));
        }
        VmStep::Ended => break,
        VmStep::Error(e) => return Err(e.into()),
    }
}
```

If you pass `None` when the VM is waiting for a choice, the behaviour depends on
whether the menu has a default branch:

- **No default** — the VM re-emits the same `Event::Choice`, giving the host
  another chance to provide a selection.
- **`has_default: true`** — the VM follows the `_ { ... }` fallback branch.

Only passing `Some(index)` where `index >= options.len()` produces
`VmStep::Error(VmError::ChoiceOutOfBounds { .. })`.

## Why Pull-Based?

The pull-based model gives you several advantages:

- **Deterministic execution** — no race conditions, no ordering surprises.
- **Easy serialisation** — save/load the VM state between `next()` calls.
- **Testability** — drive the VM in unit tests with a simple loop.
- **Engine agnosticism** — works with Bevy, Godot, Unity (via FFI), a terminal
  app, or a web server. Anything that can call a Rust function.
- **No lifetime headaches** — the VM doesn't borrow anything from your engine.
  It owns its data, you own yours.

The VM is essentially a state machine that you crank forward one event at a time.