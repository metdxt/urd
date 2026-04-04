# Decorator Registry

The `DecoratorRegistry` lets you register Rust-side handlers that enrich
dialogue and choice events with extra data. When the VM encounters a
`@decorator` annotation in a script, it evaluates the decorator's arguments,
calls your handler, and merges the returned fields into the event.

## Creating a Registry

Every `Vm` requires a `DecoratorRegistry` — even if you have no custom
decorators, you must pass an empty one:

```rust
use urd::prelude::*;

let registry = DecoratorRegistry::new();
```

## Registering Handlers

Use `registry.register(name, handler)` to add a named handler. The handler
receives a slice of evaluated `RuntimeValue` arguments and returns a
`HashMap<String, RuntimeValue>` whose entries are merged into the event's
`fields` map.

```rust
use std::collections::HashMap;
use urd::prelude::*;

let mut registry = DecoratorRegistry::new();

registry.register("slow", |args| {
    let speed = match args.first() {
        Some(RuntimeValue::Float(f)) => *f,
        _ => 1.0,
    };
    HashMap::from([("speed".into(), RuntimeValue::Float(speed))])
});
```

The corresponding Urd script would look like:

```urd
@slow(0.5)
narrator: "The wind howls through the canyon..."
```

When the VM emits this dialogue event, its `fields` map will contain
`"speed" → Float(0.5)`.

### Handler Signature

The handler must satisfy:

```rust
Fn(&[RuntimeValue]) -> HashMap<String, RuntimeValue> + Send + Sync + 'static
```

- **Arguments** — a slice of `RuntimeValue` items, one per argument in the
  script's `@decorator(arg1, arg2, ...)` call. If the decorator has no
  arguments the slice is empty (but will contain a single `Null` value from the
  parser's default).
- **Return** — a `HashMap` of field name → value pairs. These are merged into
  the `fields` on the emitted `Event::Dialogue` or `Event::Choice`.

> **Note:** Internally, the registry wraps your infallible closure in `Ok(...)`
> to satisfy the fallible `DecoratorHandler` storage type. Script-defined
> decorator bodies can return errors, but Rust-registered handlers cannot fail.

## Multiple Decorators

A single dialogue or choice node can have multiple decorators. Their field maps
are merged in order — later decorators overwrite earlier ones if they produce
the same key.

```urd
@slow(0.3)
@mood("angry")
captain: "You call that a landing?!"
```

```rust
use std::collections::HashMap;
use urd::prelude::*;

let mut registry = DecoratorRegistry::new();

registry.register("slow", |args| {
    let speed = match args.first() {
        Some(RuntimeValue::Float(f)) => *f,
        _ => 1.0,
    };
    HashMap::from([("speed".into(), RuntimeValue::Float(speed))])
});

registry.register("mood", |args| {
    let mood = match args.first() {
        Some(RuntimeValue::Str(s)) => RuntimeValue::Str(s.clone()),
        _ => RuntimeValue::Str("neutral".into()),
    };
    HashMap::from([("mood".into(), mood)])
});
```

The resulting event's `fields` will contain both `"speed"` and `"mood"`.

## Script-Defined Decorators

Urd scripts can define their own decorators using the `decorator` keyword. These
coexist with Rust-registered ones — the VM checks both the registry and the
script's environment when resolving a decorator name.

```urd
decorator shake<dialogue>(intensity: float) {
    return { "shake_intensity": intensity }
}

@shake(2.5)
narrator: "The ground trembles beneath your feet."
```

Script-defined decorators are compiled into the `IrGraph` and executed by the
VM at runtime. They can access script variables and perform computations that
Rust-registered handlers cannot.

When both a Rust handler and a script-defined decorator exist for the same name,
the first registration wins — the Rust handler takes precedence if it was
registered before the VM encounters the script definition.

## Validation at VM Construction

When you call `Vm::new(graph, registry)`, the VM performs a **validation pass**
over every dialogue and choice node in the graph. If any `@decorator` name is
found that is neither registered in the `DecoratorRegistry` nor defined as a
script-level decorator, construction fails immediately:

```rust
use urd::prelude::*;

let source = r#"
@unknown_decorator
narrator: "This will fail."
"#;
let ast = urd::compiler::loader::parse_source(source).unwrap();
let graph = Compiler::compile(&ast).unwrap();
let registry = DecoratorRegistry::new(); // empty — no handlers

match Vm::new(graph, registry) {
    Err(VmError::UnknownDecorator { name, .. }) => {
        eprintln!("Unregistered decorator: @{name}");
    }
    _ => {}
}
```

This fail-fast behaviour catches typos and missing registrations before your
game even starts executing dialogue.

## Inspecting Registered Names

You can iterate over all registered decorator names with `known_names()`:

```rust
use urd::prelude::*;

let mut registry = DecoratorRegistry::new();
registry.register("slow", |_| std::collections::HashMap::new());
registry.register("mood", |_| std::collections::HashMap::new());

for name in registry.known_names() {
    println!("Registered: @{name}");
}
```

## Practical Patterns

### Typewriter Effect

```rust
use std::collections::HashMap;
use urd::prelude::*;

let mut registry = DecoratorRegistry::new();

registry.register("typewriter", |args| {
    let cps = match args.first() {
        Some(RuntimeValue::Int(n)) => *n,
        _ => 30, // default: 30 characters per second
    };
    HashMap::from([("typewriter_cps".into(), RuntimeValue::Int(cps))])
});
```

### Camera Shake

```rust
use std::collections::HashMap;
use urd::prelude::*;

let mut registry = DecoratorRegistry::new();

registry.register("shake", |args| {
    let intensity = match args.first() {
        Some(RuntimeValue::Float(f)) => *f,
        _ => 1.0,
    };
    let duration = match args.get(1) {
        Some(RuntimeValue::Float(f)) => *f,
        _ => 0.5,
    };
    HashMap::from([
        ("shake_intensity".into(), RuntimeValue::Float(intensity)),
        ("shake_duration".into(), RuntimeValue::Float(duration)),
    ])
});
```

### Reading Fields in Your Engine

On the game engine side, after receiving an event, inspect `fields` for
decorator data:

```rust
use urd::prelude::*;

fn handle_event(event: &Event) {
    match event {
        Event::Dialogue { fields, .. } => {
            if let Some(RuntimeValue::Float(speed)) = fields.get("speed") {
                set_text_speed(*speed);
            }
            if let Some(RuntimeValue::Float(intensity)) = fields.get("shake_intensity") {
                trigger_camera_shake(*intensity);
            }
        }
        _ => {}
    }
}

fn set_text_speed(_speed: f64) { /* engine-specific */ }
fn trigger_camera_shake(_intensity: f64) { /* engine-specific */ }
```
