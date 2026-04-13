# Implementing a Localizer

The `Localizer` trait is the bridge between the Urd VM and your game's localization system. By implementing this single trait, you plug translated text into every dialogue and menu event the VM emits.

## The Trait

The trait lives in `urd::loc::Localizer` (re-exported via the prelude):

```rust
pub trait Localizer: Send + Sync {
    fn localize(
        &self,
        id: &str,
        vars: &HashMap<String, RuntimeValue>,
    ) -> Option<String>;
}
```

### Parameters

| Parameter | Description |
|-----------|-------------|
| `id`      | The localization key for the current dialogue line or menu option (e.g. `merchant-start-line_1`) |
| `vars`    | All Fluent variable bindings in scope — both `@fluent`-tagged variables and string-interpolation variables |

### Return value

- **`Some(text)`** — the translated text. The VM stores it in the emitted event as `localized_text` (for dialogue) or `localized_label` (for menu options).
- **`None`** — no translation available. The VM leaves the original source text untouched.

### Thread safety

The trait requires `Send + Sync` because the localizer is wrapped in `Arc<dyn Localizer>` and may be shared across threads. Use `fluent_bundle::concurrent::FluentBundle` (not the default non-concurrent variant) if you're using the `fluent-bundle` crate directly.

## Attaching to the VM

Pass your localizer to the VM builder before the first call to `next()`:

```rust
use std::sync::Arc;

let localizer = MyLocalizer::new("pl-PL")?;
let mut vm = Vm::new(graph, registry)?
    .with_localizer(Arc::new(localizer));
```

The `with_localizer` method takes `Arc<dyn Localizer>` and returns `Self`, so it chains naturally with other builder methods.

## Example: Fluent Bundle Implementation

Here is a minimal but complete implementation using the `fluent-bundle` crate. This mirrors the approach used by the `quest` CLI's own `FsLocalizer`.

```rust
use std::collections::HashMap;
use std::path::Path;

use fluent_bundle::concurrent::FluentBundle;
use fluent_bundle::{FluentArgs, FluentResource, FluentValue};
use unic_langid::LanguageIdentifier;

use urd::prelude::*;
use urd::runtime::value::RuntimeValue;

struct FluentLocalizer {
    bundle: FluentBundle<FluentResource>,
}

impl FluentLocalizer {
    fn load(locale_dir: &Path, locale: &str) -> Result<Self, String> {
        let langid: LanguageIdentifier = locale
            .parse()
            .map_err(|e| format!("invalid locale '{locale}': {e}"))?;

        let mut bundle = FluentBundle::new_concurrent(vec![langid]);
        bundle.set_use_isolating(false);

        // Load every .ftl file in the locale directory.
        let dir = locale_dir.join(locale);
        for entry in std::fs::read_dir(&dir)
            .map_err(|e| format!("cannot read '{}': {e}", dir.display()))?
        {
            let path = entry.map_err(|e| e.to_string())?.path();
            if path.extension().and_then(|e| e.to_str()) != Some("ftl") {
                continue;
            }
            let content = std::fs::read_to_string(&path)
                .map_err(|e| format!("cannot read '{}': {e}", path.display()))?;
            let resource = FluentResource::try_new(content)
                .map_err(|(_, errs)| format!("parse errors: {errs:?}"))?;
            bundle.add_resource(resource)
                .map_err(|errs| format!("duplicate IDs: {errs:?}"))?;
        }

        Ok(Self { bundle })
    }
}

impl Localizer for FluentLocalizer {
    fn localize(
        &self,
        id: &str,
        vars: &HashMap<String, RuntimeValue>,
    ) -> Option<String> {
        let msg = self.bundle.get_message(id)?;
        let pattern = msg.value()?;

        // Convert RuntimeValue map to FluentArgs.
        let mut args = FluentArgs::new();
        for (key, value) in vars {
            let fluent_val = match value {
                RuntimeValue::Int(n)   => FluentValue::from(*n as f64),
                RuntimeValue::Float(f) => FluentValue::from(*f),
                RuntimeValue::Bool(b)  => FluentValue::from(
                    if *b { "true" } else { "false" }
                ),
                RuntimeValue::Str(ps)  => FluentValue::from(ps.to_string()),
                other                  => FluentValue::from(format!("{other:?}")),
            };
            args.set(key.as_str(), fluent_val);
        }

        let mut errors = vec![];
        let result = self.bundle
            .format_pattern(pattern, Some(&args), &mut errors);

        if !errors.is_empty() {
            eprintln!("[l10n] errors formatting '{id}': {errors:?}");
        }

        Some(result.into_owned())
    }
}
```

### Using it

```rust
use std::sync::Arc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_script("merchant.urd")?;

    let localizer = FluentLocalizer::load(
        Path::new("i18n"),
        "pl-PL",
    )?;

    let mut vm = Vm::new(graph, registry)?
        .with_localizer(Arc::new(localizer));

    loop {
        match vm.next() {
            VmStep::Event(Event::Dialogue {
                localized_text,
                lines,
                ..
            }) => {
                // Prefer localized text; fall back to source.
                let text = localized_text.as_deref()
                    .unwrap_or(&lines.join("\n"));
                println!("{text}");
            }
            VmStep::Event(Event::Choice { options, .. }) => {
                for (i, opt) in options.iter().enumerate() {
                    let label = opt.localized_label.as_deref()
                        .unwrap_or(&opt.label);
                    println!("  [{i}] {label}");
                }
                // ... read player choice ...
            }
            VmStep::Ended => break,
            _ => {}
        }
    }

    Ok(())
}
```

## How Variables Reach the Localizer

When the VM encounters a dialogue or menu node, it collects all Fluent variable bindings from the current scope:

1. **`@fluent`-tagged globals** — available in every label's scope.
2. **`@fluent`-tagged locals** — scoped to the label they're declared in.
3. **String-interpolation variables** — any `{var}` in dialogue text is also passed automatically.

These are combined into the `vars` HashMap. Dotted paths like `inv.gold` become hyphenated keys (`inv-gold`) to comply with Fluent identifier rules.

If a variable has a format specifier in the source (e.g. `{price:.2}`), the pre-formatted string is passed instead of the raw numeric value. This ensures the Fluent message sees `"30.00"` rather than `Float(30.0)`.

## Reference Implementation

The `quest` CLI ships a production-quality `FsLocalizer` in `crates/urd-quest/src/localizer.rs`. It demonstrates:

- Loading all `.ftl` files from a locale directory into a single bundle
- Locale-name validation to prevent path traversal attacks
- Disabling Unicode bidi isolation markers for terminal output
- Full `RuntimeValue` → `FluentValue` conversion

Use it as a reference when building your own game-specific localizer. For many projects, the `FsLocalizer` pattern — load `.ftl` from disk at startup, format on demand — is exactly the right approach.

## Alternative Approaches

The `Localizer` trait is deliberately I/O-free. Your implementation decides how to load translation data:

| Approach | Use case |
|----------|----------|
| Read `.ftl` from disk | Desktop games, CLI tools |
| `include_str!` at compile time | Embedded / single-binary distribution |
| Fetch over HTTP | WASM games, cloud-hosted translations |
| In-memory cache with hot-reload | Development / live editing workflows |

As long as you implement the single `localize` method, the VM doesn't care where the translations come from.