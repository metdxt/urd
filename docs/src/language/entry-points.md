# Entry Points

The `@entry` decorator marks labels as public entry points. A script may contain
multiple `@entry` labels; the VM uses the first one (or a host-specified one) to
begin execution.

---

## The `@entry` Decorator

Apply `@entry` directly above a label declaration to designate it as the
script's starting point:

```urd
@entry
label start {
    narrator: "Welcome, traveler."
    narrator: "Your journey begins here."
}
```

When the VM loads this script, it begins execution at the `start` label. Without
`@entry`, the VM has no way to know where to begin.

---

## Multiple Entry Points

A script file may contain any number of `@entry` decorators. Each decorated
label is treated as a public entry point — a place the host or VM can begin
execution from:

```urd
@entry
label intro {
    narrator: "This is the intro."
}

@entry
label tutorial {
    narrator: "Let me show you around."
}
```

When no specific label is requested, the VM starts at the first `@entry` label
it encounters.

---

## What Happens Without `@entry`

If a script contains no `@entry` decorator at all, the VM has no cursor position
to start from. Depending on context:

- **Standalone scripts** — the VM cannot begin execution. The host receives no
  events.
- **Imported modules** — this is perfectly fine. Imported files don't need their
  own entry point; they provide labels, functions, types, and variables for the
  importing script to use.

As a rule of thumb: your **main script** needs `@entry`; supporting files
imported via `import` typically do not.

---

## Multi-File Projects

In a multi-file project, each file *can* have its own `@entry`, but only the
entry point of the **root script** (the one you pass to the compiler) is used
to start execution. Entry points in imported files are ignored during normal
execution — they exist so that each file can be tested or run independently.

```urd
# main.urd — the root script
import "tavern.urd" as tavern

@entry
label start {
    narrator: "You arrive at the crossroads."
    jump tavern.enter_tavern
}
```

```urd
# tavern.urd — an imported module
# Has its own @entry so it can be run/tested standalone

@entry
label enter_tavern {
    narrator: "The tavern is warm and noisy."
}
```

When the compiler processes `main.urd`, execution begins at `start`. The
`@entry` on `enter_tavern` inside `tavern.urd` is only relevant if you compile
and run `tavern.urd` directly.

> **Important:** Labels in imported files **must** be decorated with `@entry` to
> be visible as cross-module jump targets. Without `@entry`, the label is
> private to its file and attempting to jump to it from another module will
> produce a `PrivateLabel` compiler error.

---

## Naming Conventions

The label decorated with `@entry` can have any valid name. Common conventions
include:

| Name     | When to use                         |
|----------|-------------------------------------|
| `start`  | General-purpose default             |
| `main`   | If you come from a programming background |
| `intro`  | For scripts that begin with a narrative introduction |
| `begin`  | Another common alternative          |

The name itself carries no special meaning — only the `@entry` decorator
matters.

---

## Entry Points and the Event Loop

Once the VM starts at the `@entry` label, it proceeds through the script's
control flow like any other label. The host drives execution by calling
`vm.next(None)` in a loop:

```rust
let mut vm = Vm::new(graph, DecoratorRegistry::default())?;

loop {
    match vm.next(None) {
        VmStep::Event(Event::Dialogue { speakers, lines, .. }) => {
            // Render dialogue in your game UI
        }
        VmStep::Event(Event::Choice { options, .. }) => {
            // Present choices, then call vm.next(Some(index))
        }
        VmStep::Ended => break,
        VmStep::Error(e) => {
            eprintln!("VM error: {e}");
            break;
        }
    }
}
```

The `@entry` label is simply the first node the VM visits — after that, `jump`
statements, menus, and the rest of the control flow system take over.