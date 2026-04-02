//! # Spellcheck Analysis Pass
//!
//! Emits [`AnalysisError::Misspelling`] (warning) for words inside dialogue
//! string literals that are not found in the dictionary for the selected
//! language.
//!
//! ## Multi-language support
//!
//! Pass a [`SpellcheckLanguage`] to [`check`] to select the active dictionary.
//! Dictionaries for English, German, Spanish, French, Hebrew, Italian,
//! Russian, and Chinese are embedded at compile time via `include_str!` so
//! there is no runtime file I/O.  Each language's checker is initialised
//! lazily on first use and then reused for the lifetime of the process.
//!
//! Use [`SpellcheckLanguage::from_code`] to resolve an ISO 639-1 code (e.g.
//! `"de"`, `"ru"`) to the appropriate variant.
//!
//! ## What is checked
//!
//! Only [`StringPart::Literal`] segments are checked — interpolations
//! (`{variable}`) and escape sequences are intentionally skipped.
//!
//! ## Tokenisation
//!
//! [`tokenize_words`] splits on whitespace, then trims each chunk to its
//! first and last alphabetic character.  This strips leading/trailing
//! punctuation (commas, periods, quotes, `!`, `?`) while preserving interior
//! hyphens and apostrophes intact, so `"low-ceilinged,"` → `"low-ceilinged"`
//! and `"It's"` → `"It's"`.
//!
//! ## Exclusion rules
//!
//! A token is silently skipped — never passed to SymSpell — when any of the
//! following hold:
//!
//! - Fewer than 3 Unicode scalar values.
//! - Contains an interior apostrophe (contractions: `"It's"`, `"don't"`).
//! - Contains an interior hyphen (compounds: `"low-ceilinged"`).
//! - Contains any non-alphabetic character (digits, `#`, `@`, hex codes…).
//! - All ASCII uppercase (acronyms: `"NASA"`, `"FBI"`).
//! - CamelCase or PascalCase (proper nouns / identifiers: `"TheTavern"`,
//!   `"iPhone"`).  Plain title-case (`"Hello"`) is intentionally kept.
//!
//! ## Dictionary
//!
//! Each [`symspell::SymSpell`] instance is built once per language from the
//! bundled frequency dictionary embedded at compile time.  Words with
//! edit-distance 0 from a dictionary entry are clean.  Anything else emits a
//! warning with an optional "did you mean" suggestion (the top-ranked SymSpell
//! candidate).

use std::sync::OnceLock;

use chumsky::span::SimpleSpan;
use symspell::{SymSpell, SymSpellBuilder, UnicodeStringStrategy, Verbosity};

use crate::analysis::AnalysisError;
use crate::lexer::strings::{ParsedString, StringPart};
use crate::parser::ast::{Ast, AstContent, walk_ast};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Language selector
// ---------------------------------------------------------------------------

/// The language to use for spell-checking dialogue content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SpellcheckLanguage {
    /// English (default) — 82,765-entry frequency dictionary.
    #[default]
    English,
    /// German — 100k-entry frequency dictionary.
    German,
    /// Spanish — 100k-entry frequency dictionary.
    Spanish,
    /// French — 100k-entry frequency dictionary.
    French,
    /// Hebrew — 100k-entry frequency dictionary.
    Hebrew,
    /// Italian — 100k-entry frequency dictionary.
    Italian,
    /// Russian — 100k-entry frequency dictionary.
    Russian,
    /// Chinese — 50k-entry frequency dictionary.
    Chinese,
}

impl SpellcheckLanguage {
    /// Map an ISO 639-1 language code to a [`SpellcheckLanguage`].
    ///
    /// Recognised codes: `"en"`, `"de"`, `"es"`, `"fr"`, `"he"`, `"it"`,
    /// `"ru"`, `"zh"`.  Any unrecognised code returns
    /// [`SpellcheckLanguage::English`].
    pub fn from_code(code: &str) -> Self {
        match code {
            "en" => Self::English,
            "de" => Self::German,
            "es" => Self::Spanish,
            "fr" => Self::French,
            "he" => Self::Hebrew,
            "it" => Self::Italian,
            "ru" => Self::Russian,
            "zh" => Self::Chinese,
            _ => Self::English,
        }
    }

    /// Returns the index of this variant into [`CHECKERS`].
    fn index(self) -> usize {
        match self {
            Self::English => 0,
            Self::German => 1,
            Self::Spanish => 2,
            Self::French => 3,
            Self::Hebrew => 4,
            Self::Italian => 5,
            Self::Russian => 6,
            Self::Chinese => 7,
        }
    }
}

// ---------------------------------------------------------------------------
// Per-language dictionary singletons
// ---------------------------------------------------------------------------

/// One [`OnceLock`] per language — initialised lazily on first use.
static CHECKERS: [OnceLock<SymSpell<UnicodeStringStrategy>>; 8] = [const { OnceLock::new() }; 8];

/// Return (and lazily initialise) the [`SymSpell`] checker for `language`.
///
/// The dictionary for each language is embedded at compile time via
/// `include_str!` — no runtime file I/O, no missing-file errors.
fn spell_checker(language: SpellcheckLanguage) -> &'static SymSpell<UnicodeStringStrategy> {
    CHECKERS[language.index()].get_or_init(|| build_checker(language))
}

/// Build a fresh [`SymSpell<UnicodeStringStrategy>`] loaded with the
/// frequency dictionary for `language`.
fn build_checker(language: SpellcheckLanguage) -> SymSpell<UnicodeStringStrategy> {
    let mut checker: SymSpell<UnicodeStringStrategy> = SymSpellBuilder::default()
        .max_dictionary_edit_distance(2)
        .prefix_length(7)
        .count_threshold(1)
        .build()
        .unwrap_or_default();

    let dict = match language {
        SpellcheckLanguage::English => {
            include_str!("../../data/frequency_dictionary_en_82_765.txt")
        }
        SpellcheckLanguage::German => include_str!("../../data/de-100k.txt"),
        SpellcheckLanguage::Spanish => include_str!("../../data/es-100l.txt"),
        SpellcheckLanguage::French => include_str!("../../data/fr-100k.txt"),
        SpellcheckLanguage::Hebrew => include_str!("../../data/he-100k.txt"),
        SpellcheckLanguage::Italian => include_str!("../../data/it-100k.txt"),
        SpellcheckLanguage::Russian => include_str!("../../data/ru-100k.txt"),
        SpellcheckLanguage::Chinese => include_str!("../../data/zh-50k.txt"),
    };

    let mut loaded: usize = 0;
    for line in dict.lines() {
        if checker.load_dictionary_line(line, 0, 1, " ") {
            loaded += 1;
        }
    }

    log::debug!(
        "spellcheck: {:?} dictionary initialised with {loaded} entries",
        language
    );
    checker
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the spellcheck pass over `ast` using `language` and return any
/// diagnostics found.
///
/// Walks the entire AST in DFS order.  For every [`AstContent::Dialogue`]
/// node the content sub-tree is inspected:
///
/// - `Value(Str(s))` — the string is checked directly.
/// - `Block([..])` / `ExprList([..])` — each `Value(Str(s))` child is checked
///   individually.
///
/// Each [`StringPart::Literal`] segment is tokenised into words.  Words that
/// are shorter than 3 Unicode scalar values, entirely ASCII-uppercase, or
/// contain digits are skipped.  Remaining words are lowercased and looked up
/// with SymSpell (edit distance ≤ 2, `Verbosity::Top`).  Any word whose
/// closest dictionary match has distance > 0, or that is absent from the
/// dictionary entirely, produces one [`AnalysisError::Misspelling`].
pub fn check(ast: &Ast, language: SpellcheckLanguage) -> Vec<AnalysisError> {
    let checker = spell_checker(language);
    let mut errors: Vec<AnalysisError> = Vec::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::Dialogue { content, .. } = node.content() {
            check_dialogue_content(content, checker, &mut errors);
        }
    });

    errors
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Dispatch spell-checking to the appropriate handler for `content`'s shape.
///
/// Three content shapes are recognised:
///
/// - `Value(Str(s))` — a single string literal (the common single-line form).
/// - `Block(stmts)` — a block of statements (hand-built AST / test helper
///   form; see [`crate::parser::ast::Ast::block`]).
/// - `ExprList(exprs)` — a brace-delimited list of expressions produced by
///   the real parser when the source reads `Speaker: { "line1" "line2" }`.
fn check_dialogue_content(
    content: &Ast,
    checker: &SymSpell<UnicodeStringStrategy>,
    errors: &mut Vec<AnalysisError>,
) {
    match content.content() {
        AstContent::Value(RuntimeValue::Str(s)) => {
            check_parsed_string(s, content.span(), checker, errors);
        }
        // Hand-built ASTs (test helpers) use Block; real parsed source with
        // brace-delimited content uses ExprList — handle both identically.
        AstContent::Block(items) | AstContent::ExprList(items) => {
            for item in items {
                if let AstContent::Value(RuntimeValue::Str(s)) = item.content() {
                    check_parsed_string(s, item.span(), checker, errors);
                }
            }
        }
        _ => {}
    }
}

/// Spell-check every [`StringPart::Literal`] segment of `s`.
///
/// `span` is attached to every emitted [`AnalysisError::Misspelling`] so the
/// diagnostic points at the surrounding dialogue node.
fn check_parsed_string(
    s: &ParsedString,
    span: SimpleSpan,
    checker: &SymSpell<UnicodeStringStrategy>,
    errors: &mut Vec<AnalysisError>,
) {
    for part in s.parts() {
        let StringPart::Literal(text) = part else {
            continue;
        };

        for word in tokenize_words(text) {
            if should_skip_word(word) {
                continue;
            }

            let lower = word.to_lowercase();
            let suggestions = checker.lookup(&lower, Verbosity::Top, 2);

            match suggestions.first() {
                None => {
                    // Word is completely absent from the dictionary.
                    log::trace!("spellcheck: no dictionary match for '{word}'");
                    errors.push(AnalysisError::Misspelling {
                        word: word.to_owned(),
                        suggestion: None,
                        span,
                    });
                }
                Some(top) if top.distance > 0 => {
                    // Closest match has non-zero edit distance → likely typo.
                    log::trace!(
                        "spellcheck: '{word}' → '{}' (distance {})",
                        top.term,
                        top.distance
                    );
                    errors.push(AnalysisError::Misspelling {
                        word: word.to_owned(),
                        suggestion: Some(top.term.clone()),
                        span,
                    });
                }
                Some(_) => {
                    // Exact match (distance == 0) — word is spelled correctly.
                }
            }
        }
    }
}

/// Returns `true` if `word` should be excluded from spell-checking.
///
/// Rules are applied in order; the first match short-circuits the rest:
///
/// 1. **Too short** — fewer than 3 Unicode scalar values.
/// 2. **Contraction** — contains an interior apostrophe (`"It's"`, `"don't"`).
/// 3. **Compound / hyphenated** — contains an interior hyphen
///    (`"low-ceilinged"`, `"well-known"`).
/// 4. **Non-alphabetic content** — contains any character that is not
///    alphabetic: digits, punctuation, hex codes, special chars, etc.
/// 5. **All-caps ASCII** — every character is ASCII uppercase (`"NASA"`,
///    `"FBI"`).  The ASCII guard avoids wrongly skipping words in non-Latin
///    scripts whose uppercase/lowercase semantics differ.
/// 6. **CamelCase / PascalCase** — an uppercase letter follows a lowercase
///    letter somewhere in the word (`"CamelCase"`, `"iPhone"`, `"McDonald"`).
///    Plain title-case (`"Hello"`) is intentionally not matched.
fn should_skip_word(word: &str) -> bool {
    // (1) Too short (by Unicode scalar count, not bytes).
    if word.chars().count() < 3 {
        return true;
    }
    // (2) Contraction: internal apostrophe.
    if word.contains('\'') {
        return true;
    }
    // (3) Compound / hyphenated word: internal hyphen.
    if word.contains('-') {
        return true;
    }
    // (4) Any non-alphabetic character: digits, special chars, hex codes…
    //     Note: apostrophe and hyphen are already caught above, but this
    //     acts as a catch-all for anything else (e.g. "#f5c542", "v2").
    if word.chars().any(|c| !c.is_alphabetic()) {
        return true;
    }
    // (5) All-caps ASCII → acronym.
    if word.is_ascii() && word.chars().all(|c| c.is_ascii_uppercase()) {
        return true;
    }
    // (6) CamelCase / PascalCase → proper noun or code identifier.
    if is_camel_case(word) {
        return true;
    }
    false
}

/// Returns `true` if `word` is CamelCase or PascalCase.
///
/// The heuristic: an uppercase letter appears immediately after a lowercase
/// letter somewhere in the word.  This correctly matches `"CamelCase"`,
/// `"iPhone"`, and `"McDonald"` while leaving plain title-case words like
/// `"Hello"` (uppercase only at position 0) unaffected.
fn is_camel_case(word: &str) -> bool {
    let mut prev_lower = false;
    for c in word.chars() {
        if prev_lower && c.is_uppercase() {
            return true;
        }
        prev_lower = c.is_lowercase();
    }
    false
}

/// Extract tokens from `text` for spell-checking.
///
/// ## Algorithm
///
/// 1. Split `text` on whitespace via [`str::split_whitespace`].
/// 2. For each chunk, find the first and last alphabetic character and take
///    the sub-slice between them (inclusive).  This strips leading/trailing
///    punctuation — quotes, commas, periods, `!`, `?`, etc. — while
///    **preserving interior apostrophes and hyphens exactly as they appear**,
///    so `"It's"` → `"It's"` and `"low-ceilinged,"` → `"low-ceilinged"`.
/// 3. Chunks with no alphabetic character at all (e.g. `"---"`, `"42"`) are
///    discarded.
///
/// Tokens containing interior apostrophes or hyphens are yielded intact and
/// then excluded by [`should_skip_word`], keeping tokenisation and exclusion
/// logic cleanly separated.
fn tokenize_words(text: &str) -> impl Iterator<Item = &str> + '_ {
    text.split_whitespace().filter_map(|chunk| {
        // First alphabetic character → token start.
        let start = chunk.char_indices().find(|(_, c)| c.is_alphabetic())?.0;
        // Last alphabetic character → token end (exclusive byte position).
        let (last_i, last_c) = chunk
            .char_indices()
            .rev()
            .find(|(_, c)| c.is_alphabetic())?;
        let end = last_i + last_c.len_utf8();
        Some(&chunk[start..end])
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;

    // ── Helper ──────────────────────────────────────────────────────────────

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse successfully")
    }

    // ── SpellcheckLanguage::from_code ────────────────────────────────────────

    #[test]
    fn from_code_known_codes() {
        assert_eq!(
            SpellcheckLanguage::from_code("de"),
            SpellcheckLanguage::German
        );
        assert_eq!(
            SpellcheckLanguage::from_code("fr"),
            SpellcheckLanguage::French
        );
        assert_eq!(
            SpellcheckLanguage::from_code("ru"),
            SpellcheckLanguage::Russian
        );
        assert_eq!(
            SpellcheckLanguage::from_code("zh"),
            SpellcheckLanguage::Chinese
        );
        assert_eq!(
            SpellcheckLanguage::from_code("es"),
            SpellcheckLanguage::Spanish
        );
        assert_eq!(
            SpellcheckLanguage::from_code("he"),
            SpellcheckLanguage::Hebrew
        );
        assert_eq!(
            SpellcheckLanguage::from_code("it"),
            SpellcheckLanguage::Italian
        );
        assert_eq!(
            SpellcheckLanguage::from_code("en"),
            SpellcheckLanguage::English
        );
    }

    #[test]
    fn from_code_unknown_falls_back_to_english() {
        assert_eq!(
            SpellcheckLanguage::from_code("xx"),
            SpellcheckLanguage::English
        );
        assert_eq!(
            SpellcheckLanguage::from_code(""),
            SpellcheckLanguage::English
        );
        assert_eq!(
            SpellcheckLanguage::from_code("zz"),
            SpellcheckLanguage::English
        );
    }

    // ── tokenize_words unit tests ────────────────────────────────────────────

    #[test]
    fn tokenize_plain_sentence() {
        let words: Vec<&str> = tokenize_words("Hello, world!").collect();
        assert_eq!(words, ["Hello", "world"]);
    }

    #[test]
    fn tokenize_contraction_preserved() {
        // Apostrophe is interior → token is kept whole; exclusion is
        // should_skip_word's job, not the tokeniser's.
        let words: Vec<&str> = tokenize_words("can't stop").collect();
        assert_eq!(words, ["can't", "stop"]);
    }

    #[test]
    fn tokenize_contraction_is_single_token() {
        let words: Vec<&str> = tokenize_words("It's raining").collect();
        assert_eq!(words, ["It's", "raining"]);
    }

    #[test]
    fn tokenize_hyphenated_word_is_single_token() {
        let words: Vec<&str> = tokenize_words("low-ceilinged room").collect();
        assert_eq!(words, ["low-ceilinged", "room"]);
    }

    #[test]
    fn tokenize_strips_trailing_punctuation() {
        let words: Vec<&str> = tokenize_words("Hello. Goodbye!").collect();
        assert_eq!(words, ["Hello", "Goodbye"]);
    }

    #[test]
    fn tokenize_ellipsis_stripped() {
        let words: Vec<&str> = tokenize_words("Well...").collect();
        assert_eq!(words, ["Well"]);
    }

    #[test]
    fn tokenize_leading_trailing_apostrophe_stripped() {
        let words: Vec<&str> = tokenize_words("'hello'").collect();
        assert_eq!(words, ["hello"]);
    }

    #[test]
    fn tokenize_bare_apostrophe_discarded() {
        let words: Vec<&str> = tokenize_words("' '").collect();
        assert!(words.is_empty(), "bare apostrophe should produce no words");
    }

    #[test]
    fn tokenize_empty_string() {
        let words: Vec<&str> = tokenize_words("").collect();
        assert!(words.is_empty());
    }

    // ── should_skip_word unit tests ──────────────────────────────────────────

    #[test]
    fn skip_single_char() {
        assert!(should_skip_word("I"));
        assert!(should_skip_word("a"));
    }

    #[test]
    fn skip_two_char() {
        assert!(should_skip_word("an"));
        assert!(should_skip_word("OK"));
    }

    #[test]
    fn skip_all_caps() {
        assert!(should_skip_word("NASA"));
        assert!(should_skip_word("FBI"));
    }

    #[test]
    fn skip_word_with_digit() {
        assert!(should_skip_word("B52"));
        assert!(should_skip_word("mp3"));
    }

    #[test]
    fn skip_word_with_special_chars() {
        assert!(should_skip_word("f5c542"), "contains digits");
        assert!(should_skip_word("cyberpunk2077"), "contains digits");
    }

    #[test]
    fn skip_word_with_apostrophe() {
        assert!(should_skip_word("It's"), "contraction with apostrophe");
        assert!(should_skip_word("don't"), "contraction with apostrophe");
        assert!(should_skip_word("you've"), "contraction with apostrophe");
    }

    #[test]
    fn skip_word_with_internal_hyphen() {
        assert!(should_skip_word("low-ceilinged"), "hyphenated compound");
        assert!(should_skip_word("well-known"), "hyphenated compound");
    }

    #[test]
    fn skip_camel_case() {
        assert!(should_skip_word("CamelCase"), "upper after lower");
        assert!(should_skip_word("iPhone"), "upper after lower");
    }

    #[test]
    fn skip_pascal_with_interior_upper() {
        assert!(should_skip_word("McDonald"), "upper after lower");
        assert!(should_skip_word("McGregor"), "upper after lower");
    }

    #[test]
    fn do_not_skip_title_case() {
        // "Hello": uppercase only at position 0 → not CamelCase.
        assert!(
            !should_skip_word("Hello"),
            "plain title-case must not be skipped"
        );
    }

    #[test]
    fn do_not_skip_mixed_case() {
        assert!(!should_skip_word("Hello"));
    }

    #[test]
    fn do_not_skip_three_char_lowercase() {
        assert!(!should_skip_word("the"));
    }

    /// A 2-character Unicode word (e.g. two Greek letters) must be skipped
    /// even though its byte length is 4 — char count is what matters.
    #[test]
    fn unicode_word_skip_uses_char_count() {
        // "αβ": 2 Unicode scalar values, 4 UTF-8 bytes.
        // Old byte-length check (< 3) would NOT skip this; char count does.
        assert!(
            should_skip_word("αβ"),
            "'αβ' has only 2 chars and should be skipped"
        );
        // 3-char Unicode word must NOT be skipped.
        assert!(
            !should_skip_word("αβγ"),
            "'αβγ' has 3 chars and should not be skipped by length"
        );
    }

    // ── Integration tests via parsed Urd source ──────────────────────────────

    #[test]
    fn clean_dialogue_is_silent() {
        let ast = parse(
            r#"
label start {
    Narrator: "Hello, world!"
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn misspelled_word_is_flagged() {
        let ast = parse(
            r#"
label start {
    Narrator: "The rockeet launched."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let has_rockeet = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "rockeet"));
        assert!(
            has_rockeet,
            "expected 'rockeet' to be flagged as misspelling, got: {errors:?}"
        );
    }

    #[test]
    fn short_words_are_skipped() {
        // "I" (1), "am" (2), "a" (1), "go" (2) — all below the 3-char threshold.
        let ast = parse(
            r#"
label start {
    Narrator: "I am a go."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        assert!(
            errors.is_empty(),
            "short words should not be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn all_caps_words_are_skipped() {
        let ast = parse(
            r#"
label start {
    Narrator: "The NASA launched the rocket."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let nasa_flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "NASA"));
        assert!(
            !nasa_flagged,
            "all-caps 'NASA' should not be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn interpolation_skipped() {
        // Interpolation segments must never produce spelling errors.
        let ast = parse(
            r#"
label start {
    Narrator: "Your name is {player.name}."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        assert!(
            errors.is_empty(),
            "interpolation should not produce misspelling errors, got: {errors:?}"
        );
    }

    #[test]
    fn suggestion_present_for_close_typo() {
        // "rockeet" is edit-distance 1 from "rocket"; SymSpell should suggest it.
        let ast = parse(
            r#"
label start {
    Narrator: "The rockeet launched."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let misspelling = errors
            .iter()
            .find(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "rockeet"));
        assert!(
            misspelling.is_some(),
            "expected 'rockeet' to be flagged, got: {errors:?}"
        );
        if let Some(AnalysisError::Misspelling { suggestion, .. }) = misspelling {
            assert!(
                suggestion.is_some(),
                "expected a suggestion for 'rockeet', got None"
            );
            assert_eq!(
                suggestion.as_deref(),
                Some("rocket"),
                "expected suggestion 'rocket' for 'rockeet'"
            );
        }
    }

    #[test]
    fn block_dialogue_lines_are_checked() {
        // The real Urd parser emits AstContent::ExprList for brace-delimited
        // dialogue content: `Narrator: { "line1" "line2" }`.
        // A misspelling inside any string in that list must be caught.
        let ast = parse(
            r#"
label start {
    Narrator: { "The launcch was successful." }
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "launcch"));
        assert!(
            flagged,
            "expected 'launcch' in a block dialogue to be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn hand_built_block_dialogue_is_checked() {
        // make_dialogue_block (like empty_dialogue tests use) produces
        // AstContent::Block — verify that path is also covered.
        use crate::lexer::strings::ParsedString;
        use crate::runtime::value::RuntimeValue;

        let spk = Ast::value(RuntimeValue::IdentPath(vec!["Narrator".to_owned()]));
        let stmts = vec![Ast::value(RuntimeValue::Str(ParsedString::new_plain(
            "The launcch was successful.",
        )))];
        let content = Ast::block(stmts);
        let node = Ast::dialogue(spk, content);

        let errors = check(&node, SpellcheckLanguage::English);
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "launcch"));
        assert!(
            flagged,
            "expected 'launcch' in a hand-built Block dialogue to be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn misspelling_diagnostic_is_a_warning() {
        let ast = parse(
            r#"
label start {
    Narrator: "The rockeet launched."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let all_warnings = errors
            .iter()
            .all(|e| matches!(e, AnalysisError::Misspelling { .. }) && e.is_warning());
        assert!(
            all_warnings,
            "all spellcheck diagnostics should be warnings, got: {errors:?}"
        );
    }

    /// A very common German word ("und" = "and", rank 3 in the de-100k
    /// dictionary) must produce zero errors when the German checker is used.
    #[test]
    fn german_word_is_clean() {
        let ast = parse(
            r#"
label start {
    Narrator: "Die und nicht."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::German);
        assert!(
            errors.is_empty(),
            "common German words should not be flagged with the German dictionary, got: {errors:?}"
        );
    }

    // ── False-positive regression tests ─────────────────────────────────────

    /// "It's" must not be flagged — the apostrophe exclusion covers all
    /// contractions before they ever reach SymSpell.
    #[test]
    fn contraction_in_dialogue_not_flagged() {
        let ast = parse(
            r#"
label start {
    Narrator: "It's a beautiful day."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word.contains('\'')));
        assert!(
            !flagged,
            "contractions must not be flagged as misspellings, got: {errors:?}"
        );
    }

    /// "low-ceilinged" must not be flagged — the hyphen exclusion covers all
    /// compound words before they ever reach SymSpell.
    #[test]
    fn hyphenated_compound_in_dialogue_not_flagged() {
        let ast = parse(
            r#"
label start {
    Narrator: "The low-ceilinged room felt oppressive."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word.contains('-')));
        assert!(
            !flagged,
            "hyphenated compounds must not be flagged as misspellings, got: {errors:?}"
        );
    }

    /// CamelCase / PascalCase tokens (proper nouns, game identifiers) must not
    /// be flagged — the camel-case exclusion covers them before SymSpell.
    #[test]
    fn camel_case_in_dialogue_not_flagged() {
        let ast = parse(
            r#"
label start {
    Narrator: "Visit TheTavern today."
    end!()
}
"#,
        );
        let errors = check(&ast, SpellcheckLanguage::English);
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "TheTavern"));
        assert!(
            !flagged,
            "CamelCase words must not be flagged as misspellings, got: {errors:?}"
        );
    }
}
