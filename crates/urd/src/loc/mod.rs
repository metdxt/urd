//! # Localisation ID Module
//!
//! This module provides utilities for generating stable, human-readable
//! localisation IDs for Urd dialogue scripts.  IDs are constructed from the
//! file stem and the nesting structure of labels, menus, and dialogue nodes,
//! giving every line and option a deterministic, slug-based path such as
//! `intro-start-menu_1-alcohol`.
//!
//! ## Components
//!
//! - [`to_slug`] – converts arbitrary strings into safe ID segments.
//! - [`extract_id_override`] – reads a `@id("…")` decorator from a node.
//! - [`EventKind`] – classifies AST nodes that introduce counters or scopes.
//! - [`IdContext`] – stateful cursor that vends the next ID during an AST walk.
//! - [`Localizer`] – frontend-agnostic localisation provider trait.

pub mod ftl;
pub mod localizer;
pub use localizer::Localizer;

use std::collections::HashMap;

use crate::lexer::strings::StringPart;
use crate::parser::ast::{AstContent, Decorator};
use crate::runtime::value::RuntimeValue;

// ── to_slug ───────────────────────────────────────────────────────────────────

/// Converts an arbitrary string into a valid localisation-ID segment.
///
/// Steps applied in order:
/// 1. Unicode-lowercase the input.
/// 2. Replace every character that is **not** Unicode-alphanumeric with `_`.
/// 3. Collapse consecutive `_` into a single `_`.
/// 4. Trim leading and trailing `_`.
/// 5. Return `None` if the result is empty, `Some(slug)` otherwise.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(to_slug("The Story"),    Some("the_story".to_string()));
/// assert_eq!(to_slug("hello-world"),  Some("hello_world".to_string()));
/// assert_eq!(to_slug("café au lait"), Some("café_au_lait".to_string()));
/// assert_eq!(to_slug("---"),          None);
/// assert_eq!(to_slug(""),             None);
/// ```
pub fn to_slug(s: &str) -> Option<String> {
    // Step 1: Unicode-lowercase.  `char::to_lowercase` is an iterator because
    // some code-points expand to more than one char (e.g. ß → ss).
    let lowercased: String = s.chars().flat_map(|c| c.to_lowercase()).collect();

    // Step 2: replace every non-alphanumeric character with '_'.
    let replaced: String = lowercased
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect();

    // Step 3: collapse consecutive '_' into one.
    let mut result = String::with_capacity(replaced.len());
    let mut prev_underscore = false;
    for c in replaced.chars() {
        if c == '_' {
            if !prev_underscore {
                result.push('_');
            }
            prev_underscore = true;
        } else {
            result.push(c);
            prev_underscore = false;
        }
    }

    // Steps 4 & 5: trim and guard against empty result.
    let trimmed = result.trim_matches('_').to_string();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed)
    }
}

// ── extract_id_override ───────────────────────────────────────────────────────

/// Extracts the string value carried by a `@id("…")` decorator.
///
/// Returns `Some(value)` only when all of the following hold:
/// - A decorator named `"id"` exists in `decorators`.
/// - Its first argument is a [`RuntimeValue::Str`] (parsed string).
/// - The string is **static**: every part is [`StringPart::Literal`] or
///   [`StringPart::EscapedChar`] — no [`StringPart::Interpolation`].
///
/// Returns `None` if any condition is not met.  The caller may assume the
/// analysis pass has already rejected ill-formed `@id` decorators.
pub fn extract_id_override(decorators: &[Decorator]) -> Option<String> {
    let decorator = decorators.iter().find(|d| d.name() == "id")?;

    let items = match decorator.args().content() {
        AstContent::ExprList(items) => items,
        _ => return None,
    };

    let first = items.first()?;

    let ps = match first.content() {
        AstContent::Value(RuntimeValue::Str(ps)) => ps,
        _ => return None,
    };

    // Only accept fully-static strings — no runtime interpolation.
    let is_static = ps
        .parts()
        .iter()
        .all(|part| matches!(part, StringPart::Literal(_) | StringPart::EscapedChar(_)));

    if is_static {
        Some(ps.to_string())
    } else {
        None
    }
}

// ── EventKind ─────────────────────────────────────────────────────────────────

/// Classifies AST nodes that introduce auto-numbered counters or scope segments.
///
/// Used as part of the composite counter key inside [`IdContext`] so that each
/// event type maintains independent numbering within a given scope.
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum EventKind {
    /// A spoken-line (`dialogue`) node.
    Dialogue,
    /// A player-choice (`menu`) node.
    Menu,
    /// A pattern-matching (`match`) node.
    Match,
    /// A conditional (`if`) node.
    If,
    /// An individual choice item (`MenuOption`).
    Option,
}

// ── IdContext ─────────────────────────────────────────────────────────────────

/// Stateful cursor used during an AST walk to assign stable localisation IDs.
///
/// Create one `IdContext` per source file, push/pop scopes as you enter and
/// leave [`AstContent::LabeledBlock`] and container nodes, then call
/// [`IdContext::next_dialogue_id`] / [`IdContext::next_option_id`] at every
/// dialogue or option site.
///
/// # Scope model
///
/// ```text
/// file_slug  ←  derived from the file stem
///   scope[0] ←  push_label
///   scope[1] ←  push_container (e.g. menu_1)
///   scope[2] ←  push_container (nested, e.g. if_1)
///   …
/// ```
///
/// IDs are assembled with all segments joined by `"-"`.
pub struct IdContext {
    /// Slug derived from the file stem.
    file_slug: String,
    /// Scope stack.  Index 0 is the label slug; each `push_container` appends
    /// one more segment.
    scope: Vec<String>,
    /// Per-`(scope_snapshot, EventKind)` auto-increment counters.
    counters: HashMap<(Vec<String>, EventKind), usize>,
    /// Per-`(scope_snapshot, base_slug)` collision counters for option text slugs.
    option_slug_counts: HashMap<(Vec<String>, String), usize>,
}

impl IdContext {
    /// Creates a new context for a given file.
    ///
    /// `file_stem` is passed through [`to_slug`]; if the result is empty the
    /// fallback `"unknown"` is used.
    pub fn new(file_stem: &str) -> Self {
        IdContext {
            file_slug: to_slug(file_stem).unwrap_or_else(|| "unknown".to_string()),
            scope: Vec::new(),
            counters: HashMap::new(),
            option_slug_counts: HashMap::new(),
        }
    }

    /// Pushes a `LabeledBlock` scope segment and returns the segment used.
    ///
    /// `id_override` (from `@id` on the label) takes precedence over the
    /// slug-ified `name`.  Falls back to `"label"` if `name` slugifies to
    /// nothing.
    pub fn push_label(&mut self, name: &str, id_override: Option<String>) -> String {
        let seg =
            id_override.unwrap_or_else(|| to_slug(name).unwrap_or_else(|| "label".to_string()));
        self.scope.push(seg.clone());
        seg
    }

    /// Pops the label scope pushed by [`IdContext::push_label`].
    ///
    /// Must be called symmetrically with every [`IdContext::push_label`].
    pub fn pop_label(&mut self) {
        self.scope.pop();
    }

    /// Pushes a container scope (menu / match / if) and returns the segment used.
    ///
    /// The segment prefix is determined by `kind`:
    /// - [`EventKind::Menu`]  → `"menu"`
    /// - [`EventKind::Match`] → `"match"`
    /// - [`EventKind::If`]    → `"if"`
    /// - others               → `"cont"`
    ///
    /// When `id_override` is `Some(s)`, `s` is used verbatim.  Otherwise the
    /// counter for `(current_scope, kind)` is incremented and the segment
    /// becomes `"{prefix}_{n}"` (counting from 1).
    pub fn push_container(&mut self, kind: EventKind, id_override: Option<String>) -> String {
        // Borrow `kind` by reference so it remains available for next_counter.
        let prefix = match &kind {
            EventKind::Menu => "menu",
            EventKind::Match => "match",
            EventKind::If => "if",
            _ => "cont",
        };

        let seg = match id_override {
            Some(s) => s,
            None => {
                let n = self.next_counter(self.scope.clone(), kind);
                format!("{prefix}_{n}")
            }
        };

        self.scope.push(seg.clone());
        seg
    }

    /// Pops the container scope pushed by [`IdContext::push_container`].
    ///
    /// Must be called symmetrically with every [`IdContext::push_container`].
    pub fn pop_container(&mut self) {
        self.scope.pop();
    }

    /// Computes and returns the localisation ID for the next `Dialogue` node.
    ///
    /// Advances the `Dialogue` counter for the current scope.
    ///
    /// Returns `None` when the scope is empty (node is outside any label).
    /// When `id_override` is `Some(s)`, `s` is used as the leaf verbatim;
    /// otherwise the leaf is `"line_{n}"` with an auto-incremented counter.
    pub fn next_dialogue_id(&mut self, id_override: Option<String>) -> Option<String> {
        if self.scope.is_empty() {
            return None;
        }

        let leaf = match id_override {
            Some(s) => s,
            None => {
                let n = self.next_counter(self.scope.clone(), EventKind::Dialogue);
                format!("line_{n}")
            }
        };

        Some(join_path_parts(&self.file_slug, &self.scope, &leaf))
    }

    /// Computes the localisation ID for a `MenuOption` node.
    ///
    /// Returns `None` when the scope is empty.
    ///
    /// Leaf selection priority:
    /// 1. `id_override`, used verbatim when present.
    /// 2. [`to_slug`] of `text`.  On collision (same slug in the same scope),
    ///    a `_{count}` suffix is appended, starting at `_2`.
    /// 3. If `text` slugifies to nothing, the fallback is `"option_{n}"` using
    ///    an auto-incremented counter.
    pub fn next_option_id(&mut self, text: &str, id_override: Option<String>) -> Option<String> {
        if self.scope.is_empty() {
            return None;
        }

        let leaf = match id_override {
            Some(s) => s,
            None => match to_slug(text) {
                Some(slug) => {
                    let count = self
                        .option_slug_counts
                        .entry((self.scope.clone(), slug.clone()))
                        .or_insert(0);
                    *count += 1;
                    let c = *count;
                    if c == 1 { slug } else { format!("{slug}_{c}") }
                }
                None => {
                    let n = self.next_counter(self.scope.clone(), EventKind::Option);
                    format!("option_{n}")
                }
            },
        };

        Some(join_path_parts(&self.file_slug, &self.scope, &leaf))
    }

    /// Returns the full localisation path for the current scope position.
    ///
    /// Assembles `file_slug` and all current scope segments joined by `"-"`.
    /// Call this **after** [`IdContext::push_container`] to obtain the `Choice`
    /// event's `loc_id`.
    ///
    /// Returns `None` when the scope is empty.
    pub fn current_full_path(&self) -> Option<String> {
        if self.scope.is_empty() {
            return None;
        }

        let mut parts: Vec<&str> = Vec::with_capacity(self.scope.len() + 1);
        parts.push(&self.file_slug);
        for seg in &self.scope {
            parts.push(seg.as_str());
        }
        Some(parts.join("-"))
    }

    // ── private helpers ───────────────────────────────────────────────────────

    /// Increments the counter for `(scope_key, kind)` and returns the new value.
    ///
    /// Counters start at `0`; the first call for a given key returns `1`.
    fn next_counter(&mut self, scope_key: Vec<String>, kind: EventKind) -> usize {
        let count = self.counters.entry((scope_key, kind)).or_insert(0);
        *count += 1;
        *count
    }
}

// ── join_path_parts ───────────────────────────────────────────────────────────

/// Assembles a localisation ID from a file slug, scope segments, and a leaf.
///
/// All parts are joined with `"-"`.
fn join_path_parts(file: &str, scope: &[String], leaf: &str) -> String {
    let mut parts: Vec<&str> = Vec::with_capacity(scope.len() + 2);
    parts.push(file);
    for s in scope {
        parts.push(s.as_str());
    }
    parts.push(leaf);
    parts.join("-")
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── to_slug ───────────────────────────────────────────────────────────────

    #[test]
    fn slug_basic_phrase() {
        assert_eq!(to_slug("The Story"), Some("the_story".to_string()));
    }

    #[test]
    fn slug_already_valid() {
        assert_eq!(
            to_slug("pick_your_poison"),
            Some("pick_your_poison".to_string())
        );
    }

    #[test]
    fn slug_all_dashes_returns_none() {
        assert_eq!(to_slug("---"), None);
    }

    #[test]
    fn slug_empty_string_returns_none() {
        assert_eq!(to_slug(""), None);
    }

    #[test]
    fn slug_leading_digits_preserved() {
        assert_eq!(to_slug("2fast2furious"), Some("2fast2furious".to_string()));
    }

    #[test]
    fn slug_unicode_letters_preserved() {
        // Unicode alphanumeric characters (é, etc.) must survive the filter.
        assert_eq!(to_slug("café au lait"), Some("café_au_lait".to_string()));
    }

    #[test]
    fn slug_hyphen_becomes_underscore() {
        assert_eq!(to_slug("hello-world"), Some("hello_world".to_string()));
    }

    // ── IdContext ─────────────────────────────────────────────────────────────

    #[test]
    fn ctx_flat_label_increments_dialogue() {
        let mut ctx = IdContext::new("intro");
        ctx.push_label("start", None);
        assert_eq!(
            ctx.next_dialogue_id(None),
            Some("intro-start-line_1".to_string())
        );
        assert_eq!(
            ctx.next_dialogue_id(None),
            Some("intro-start-line_2".to_string())
        );
        ctx.pop_label();
    }

    #[test]
    fn ctx_push_label_id_override() {
        let mut ctx = IdContext::new("intro");
        ctx.push_label("start", Some("the_beginning".to_string()));
        assert_eq!(
            ctx.next_dialogue_id(None),
            Some("intro-the_beginning-line_1".to_string())
        );
        ctx.pop_label();
    }

    #[test]
    fn ctx_nested_menu_option_ids() {
        let mut ctx = IdContext::new("intro");
        ctx.push_label("start", Some("the_beginning".to_string()));
        ctx.push_container(EventKind::Menu, Some("pick_your_poison".to_string()));

        assert_eq!(
            ctx.next_option_id("alcohol", None),
            Some("intro-the_beginning-pick_your_poison-alcohol".to_string())
        );
        assert_eq!(
            ctx.next_option_id("nicotine", Some("cigs".to_string())),
            Some("intro-the_beginning-pick_your_poison-cigs".to_string())
        );

        ctx.pop_container();
        ctx.pop_label();
    }

    #[test]
    fn ctx_two_menus_independent_counters() {
        let mut ctx = IdContext::new("file");
        ctx.push_label("act1", None);

        let seg1 = ctx.push_container(EventKind::Menu, None);
        assert_eq!(seg1, "menu_1");
        ctx.pop_container();

        let seg2 = ctx.push_container(EventKind::Menu, None);
        assert_eq!(seg2, "menu_2");
        ctx.pop_container();

        ctx.pop_label();
    }

    #[test]
    fn ctx_if_container_nests_dialogue() {
        let mut ctx = IdContext::new("file");
        ctx.push_label("lab", None);
        ctx.push_container(EventKind::If, None);
        assert_eq!(
            ctx.next_dialogue_id(None),
            Some("file-lab-if_1-line_1".to_string())
        );
        ctx.pop_container();
        ctx.pop_label();
    }

    #[test]
    fn ctx_option_slug_collision_appends_suffix() {
        let mut ctx = IdContext::new("file");
        ctx.push_label("lab", None);
        ctx.push_container(EventKind::Menu, None);

        let id1 = ctx.next_option_id("yes", None);
        let id2 = ctx.next_option_id("yes", None);

        assert_eq!(id1, Some("file-lab-menu_1-yes".to_string()));
        assert_eq!(id2, Some("file-lab-menu_1-yes_2".to_string()));

        ctx.pop_container();
        ctx.pop_label();
    }

    #[test]
    fn ctx_empty_text_falls_back_to_option_n() {
        let mut ctx = IdContext::new("file");
        ctx.push_label("lab", None);
        ctx.push_container(EventKind::Menu, None);

        assert_eq!(
            ctx.next_option_id("---", None),
            Some("file-lab-menu_1-option_1".to_string())
        );
        assert_eq!(
            ctx.next_option_id("---", None),
            Some("file-lab-menu_1-option_2".to_string())
        );

        ctx.pop_container();
        ctx.pop_label();
    }

    #[test]
    fn ctx_current_full_path_after_push_container() {
        let mut ctx = IdContext::new("intro");
        ctx.push_label("start", None);
        ctx.push_container(EventKind::Menu, None);

        assert_eq!(
            ctx.current_full_path(),
            Some("intro-start-menu_1".to_string())
        );

        ctx.pop_container();
        ctx.pop_label();
    }

    #[test]
    fn ctx_outside_label_returns_none() {
        let mut ctx = IdContext::new("file");
        assert_eq!(ctx.next_dialogue_id(None), None);
        assert_eq!(ctx.current_full_path(), None);
    }
}
