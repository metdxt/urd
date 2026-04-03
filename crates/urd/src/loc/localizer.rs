//! Frontend-agnostic localisation trait.
//!
//! Implement [`Localizer`] to plug translated text into the Urd VM. The trait
//! is deliberately I/O-free: the implementation decides how to load `.ftl`
//! files (embedded via `include_str!`, read from disk, fetched over HTTP for
//! WASM, etc.).
//!
//! # Example
//!
//! ```rust,ignore
//! use urd::loc::Localizer;
//! use urd::RuntimeValue;
//! use std::collections::HashMap;
//!
//! struct MyLocalizer { /* ... */ }
//!
//! impl Localizer for MyLocalizer {
//!     fn localize(&self, id: &str, vars: &HashMap<String, RuntimeValue>) -> Option<String> {
//!         // look up `id` in your Fluent bundle, pass `vars` as FluentArgs
//!         todo!()
//!     }
//! }
//! ```

use std::collections::HashMap;

use crate::runtime::value::RuntimeValue;

/// Frontend-agnostic localisation provider.
///
/// The VM calls [`Localizer::localize`] when it encounters a dialogue or
/// choice node that has a `loc_id`. If the method returns `Some(text)`, the
/// translated text is stored in the emitted [`crate::ir::Event`] as
/// `localized_text` / `localized_label`.
///
/// Implementations are required to be `Send + Sync` so they can be shared
/// across threads (e.g. wrapped in `Arc<dyn Localizer>`).
pub trait Localizer: Send + Sync {
    /// Attempt to translate the message identified by `id`.
    ///
    /// `vars` contains all Fluent variable bindings collected from the current
    /// scope — both `@fluent`-tagged variables and string-interpolation
    /// variables. Implementations should pass these as `FluentArgs` to the
    /// Fluent bundle's formatter.
    ///
    /// Return `None` if the message ID is not found or no translation is
    /// available for the current locale. The VM will leave the original
    /// `lines` / `label` untouched in that case.
    fn localize(&self, id: &str, vars: &HashMap<String, RuntimeValue>) -> Option<String>;
}
