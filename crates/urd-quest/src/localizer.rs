//! Reference [`Localizer`] implementation that loads `.ftl` files from disk.
//!
//! `FsLocalizer` is the reference implementation of the frontend-agnostic
//! [`urd::Localizer`] trait. It demonstrates the recommended pattern for
//! real game projects:
//!
//! 1. At startup, enumerate available `.ftl` files in a locale directory.
//! 2. Parse each file into a [`FluentBundle`].
//! 3. On each [`Localizer::localize`] call, look up the message and format it
//!    with the provided variable bindings as `FluentArgs`.
//!
//! # Directory layout
//!
//! ```text
//! <locale_dir>/
//!   en-US/
//!     cave.ftl
//!     intro.ftl
//!   pl-PL/
//!     cave.ftl
//! ```
//!
//! Pass the `locale_dir` path and the desired locale tag (e.g. `"en-US"`) to
//! [`FsLocalizer::load`].

use std::collections::HashMap;
use std::path::Path;

use fluent_bundle::concurrent::FluentBundle;
use fluent_bundle::{FluentArgs, FluentResource, FluentValue};
use unic_langid::LanguageIdentifier;

use urd::{Localizer, RuntimeValue};

/// A [`Localizer`] that reads `.ftl` files from a locale directory at startup.
pub struct FsLocalizer {
    /// The Fluent bundle containing all messages for the loaded locale.
    /// Uses the concurrent (`Send + Sync`) memoizer so `FsLocalizer` can be
    /// wrapped in `Arc<dyn Localizer>` and shared across threads.
    bundle: FluentBundle<FluentResource>,
    /// The locale tag this instance was loaded for (e.g. `"en-US"`).
    /// Exposed via [`FsLocalizer::locale`]; stored for diagnostics and tests.
    #[allow(dead_code)]
    locale: String,
}

impl FsLocalizer {
    /// Load all `.ftl` files from `<locale_dir>/<locale>/` into a single
    /// [`FluentBundle`].
    ///
    /// Returns an error string if the directory does not exist or any `.ftl`
    /// file cannot be read.
    pub fn load(locale_dir: &Path, locale: &str) -> Result<Self, String> {
        // Guard against path injection via the locale name.  Only the
        // characters that legitimately appear in BCP 47 locale tags are
        // permitted; anything else (slashes, dots, null bytes, …) is rejected
        // before the value is ever joined onto a filesystem path.
        if !locale
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            return Err(format!("invalid locale name: '{locale}'"));
        }
        let dir = locale_dir.join(locale);
        if !dir.exists() {
            return Err(format!(
                "locale directory '{}' does not exist",
                dir.display()
            ));
        }

        let langid: LanguageIdentifier = locale
            .parse()
            .map_err(|e| format!("invalid locale '{}': {}", locale, e))?;

        let mut bundle = FluentBundle::new_concurrent(vec![langid]);
        // Disable Unicode bidi isolation markers (U+2068/U+2069) around
        // interpolated values — not useful in a terminal game runner and makes
        // string comparisons and display cleaner.
        bundle.set_use_isolating(false);

        // Load every .ftl file in the directory.
        let entries = std::fs::read_dir(&dir)
            .map_err(|e| format!("cannot read '{}': {}", dir.display(), e))?;

        for entry in entries {
            let entry = entry.map_err(|e| format!("directory entry error: {}", e))?;
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("ftl") {
                continue;
            }

            let content = std::fs::read_to_string(&path)
                .map_err(|e| format!("cannot read '{}': {}", path.display(), e))?;

            let resource = FluentResource::try_new(content)
                .map_err(|(_, errs)| format!("parse errors in '{}': {:?}", path.display(), errs))?;

            bundle.add_resource(resource).map_err(|errs| {
                format!("duplicate message IDs in '{}': {:?}", path.display(), errs)
            })?;
        }

        Ok(Self {
            bundle,
            locale: locale.to_string(),
        })
    }

    /// Returns the locale tag this localizer was loaded for (e.g. `"en-US"`).
    ///
    /// Primarily useful for diagnostics and tests. Not called by the runner
    /// itself, hence the `#[allow(dead_code)]` on the backing field.
    #[allow(dead_code)]
    pub fn locale(&self) -> &str {
        &self.locale
    }
}

impl Localizer for FsLocalizer {
    fn localize(&self, id: &str, vars: &HashMap<String, RuntimeValue>) -> Option<String> {
        let msg = self.bundle.get_message(id)?;
        let pattern = msg.value()?;

        // Build FluentArgs from the RuntimeValue map.
        let mut args = FluentArgs::new();
        for (key, value) in vars {
            let fluent_val = runtime_value_to_fluent(value);
            args.set(key.as_str(), fluent_val);
        }

        let mut errors = Vec::new();
        let result = self
            .bundle
            .format_pattern(pattern, Some(&args), &mut errors);

        if !errors.is_empty() {
            log::warn!("[l10n] errors formatting '{}': {:?}", id, errors);
        }

        Some(result.into_owned())
    }
}

/// Convert a [`RuntimeValue`] to a [`FluentValue`].
fn runtime_value_to_fluent(value: &RuntimeValue) -> FluentValue<'static> {
    match value {
        RuntimeValue::Int(n) => FluentValue::from(*n as f64),
        RuntimeValue::Float(f) => FluentValue::from(*f),
        RuntimeValue::Bool(b) => FluentValue::from(if *b { "true" } else { "false" }),
        RuntimeValue::Str(ps) => FluentValue::from(ps.to_string()),
        // Fall back to string representation for other types.
        other => FluentValue::from(format!("{:?}", other)),
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    fn make_localizer_from_ftl(locale: &str, ftl: &str) -> FsLocalizer {
        let langid: LanguageIdentifier = locale.parse().expect("valid locale");
        let mut bundle = FluentBundle::new_concurrent(vec![langid.clone()]);
        bundle.set_use_isolating(false);
        let resource = FluentResource::try_new(ftl.to_owned()).expect("valid FTL");
        bundle.add_resource(resource).expect("no duplicates");
        FsLocalizer {
            bundle,
            locale: locale.to_string(),
        }
    }

    #[test]
    fn localizes_simple_message() {
        let loc = make_localizer_from_ftl("en-US", "hello = Hello, world!\n");
        let result = loc.localize("hello", &HashMap::new());
        assert_eq!(result.as_deref(), Some("Hello, world!"));
    }

    #[test]
    fn returns_none_for_missing_id() {
        let loc = make_localizer_from_ftl("en-US", "hello = Hello!\n");
        assert!(loc.localize("missing-id", &HashMap::new()).is_none());
    }

    #[test]
    fn interpolates_string_variable() {
        let loc = make_localizer_from_ftl("en-US", "greet = Hello, { $name }!\n");
        let mut vars = HashMap::new();
        vars.insert(
            "name".to_string(),
            RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("Alice")),
        );
        let result = loc.localize("greet", &vars);
        assert_eq!(result.as_deref(), Some("Hello, Alice!"));
    }

    #[test]
    fn interpolates_int_variable() {
        let loc = make_localizer_from_ftl("en-US", "score = You have { $gold } gold.\n");
        let mut vars = HashMap::new();
        vars.insert("gold".to_string(), RuntimeValue::Int(42));
        let result = loc.localize("score", &vars);
        assert_eq!(result.as_deref(), Some("You have 42 gold."));
    }

    #[test]
    fn locale_getter_returns_tag() {
        let loc = make_localizer_from_ftl("pl-PL", "test = Cześć!\n");
        assert_eq!(loc.locale(), "pl-PL");
    }

    // ── locale name validation ────────────────────────────────────────────

    #[test]
    fn load_rejects_dotdot_traversal_in_locale() {
        let result = FsLocalizer::load(std::path::Path::new("/tmp"), "../etc");
        assert!(result.is_err());
        let err = result.err().expect("expected an error");
        assert!(
            err.contains("invalid locale name"),
            "expected 'invalid locale name' in error, got: {err}"
        );
    }

    #[test]
    fn load_rejects_slash_in_locale() {
        let result = FsLocalizer::load(std::path::Path::new("/tmp"), "en-US/../../etc");
        assert!(result.is_err());
        let err = result.err().expect("expected an error");
        assert!(
            err.contains("invalid locale name"),
            "expected 'invalid locale name' in error, got: {err}"
        );
    }

    #[test]
    fn load_rejects_absolute_path_as_locale() {
        let result = FsLocalizer::load(std::path::Path::new("/tmp"), "/etc/passwd");
        assert!(result.is_err());
        let err = result.err().expect("expected an error");
        assert!(
            err.contains("invalid locale name"),
            "expected 'invalid locale name' in error, got: {err}"
        );
    }

    #[test]
    fn load_accepts_valid_bcp47_locale_tag() {
        // The validation gate must not block legitimate locale strings.
        // Use a non-existent directory so the function returns a "directory
        // does not exist" error rather than a locale-validation error.
        let result = FsLocalizer::load(
            std::path::Path::new("/nonexistent_urd_locale_dir_xyzzy"),
            "en-US",
        );
        assert!(result.is_err());
        let err = result.err().expect("expected an error");
        assert!(
            !err.contains("invalid locale name"),
            "a valid locale tag should pass validation, got: {err}"
        );
    }
}
