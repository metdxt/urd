//! # Spellcheck Analysis Pass
//!
//! Emits [`AnalysisError::Misspelling`] (warning) for words inside dialogue
//! string literals that are not found in the dictionary for the selected
//! language.
//!
//! ## Pipeline
//!
//! 1. **Text aggregation pre-pass** — [`aggregate_dialogue_text`] walks the
//!    entire AST and collects all [`StringPart::Literal`] segments from every
//!    [`AstContent::Dialogue`] node into a single whitespace-separated buffer.
//!
//! 2. **Language auto-detection** — [`detect_language`] feeds the buffer to
//!    [`whatlang`] for trigram-based language identification.  Detection is
//!    skipped (returning `None`) when:
//!    - The buffer is shorter than [`MIN_DETECT_BYTES`] bytes.
//!    - [`whatlang`] returns `None` (pure digits/punctuation, no trigrams).
//!    - Detection is not reliable (`info.is_reliable()` is `false`).
//!    - The detected language has no bundled dictionary.
//!
//!    In all of these cases the caller receives an empty diagnostics `Vec`
//!    (no spellcheck performed) rather than a silent fallback to English.
//!
//! 3. **Dictionary routing** — the detected (or forced) language is mapped to
//!    the appropriate [`symspell::SymSpell`] dictionary via [`spell_checker`].
//!
//! ## Language selection in [`check`]
//!
//! - `forced_language = Some(lang)` — use `lang` directly, skipping detection.
//!   Use this when the user has explicitly configured a language in their
//!   project settings.
//! - `forced_language = None` — auto-detect via [`aggregate_dialogue_text`] +
//!   [`detect_language`].  If the buffer is too short, detection is unreliable,
//!   or the detected language has no bundled dictionary, the function returns
//!   an empty `Vec` (no spellcheck performed).
//!
//! ## Multi-language support
//!
//! Pass `Some(`[`SpellcheckLanguage`]`)` to [`check`] to force a specific
//! dictionary, or `None` to auto-detect the language from the dialogue content
//! via whatlang (returns no diagnostics when detection fails or the text is
//! too short).  Dictionaries for English, German, Spanish, French, Hebrew,
//! Italian, Russian, and Chinese are downloaded on first use and cached
//! locally — see the [`Dictionary`] section below.  Each language's checker is
//! initialised lazily on first use and then reused for the lifetime of the
//! process.
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
//! Dictionaries are downloaded lazily from the
//! [hermitdave/FrequencyWords](https://github.com/hermitdave/FrequencyWords)
//! corpus on first use and cached under `{cache_dir}/urd-spellcheck/`.
//!
//! On subsequent runs the cached file is loaded directly from disk — no
//! network access.  If the cache directory cannot be determined, the download
//! fails, or the file cannot be read, spellcheck is silently disabled for that
//! session (an empty [`SymSpell`] instance is used).
//!
//! Downloads are initiated synchronously inside [`OnceLock::get_or_init`] the
//! first time a given language's checker is needed.  Each language's checker is
//! built at most once per process lifetime.

#[cfg(not(test))]
use std::io::BufRead;
use std::sync::OnceLock;

use chumsky::span::SimpleSpan;
use symspell::{SymSpell, SymSpellBuilder, UnicodeStringStrategy, Verbosity};
use whatlang;

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
    /// English (default) — hermitdave/FrequencyWords full corpus; downloaded on first use.
    #[default]
    English,
    /// German — hermitdave/FrequencyWords full corpus; downloaded on first use.
    German,
    /// Spanish — hermitdave/FrequencyWords full corpus; downloaded on first use.
    Spanish,
    /// French — hermitdave/FrequencyWords full corpus; downloaded on first use.
    French,
    /// Hebrew — hermitdave/FrequencyWords full corpus; downloaded on first use.
    Hebrew,
    /// Italian — hermitdave/FrequencyWords full corpus; downloaded on first use.
    Italian,
    /// Russian — hermitdave/FrequencyWords full corpus; downloaded on first use.
    Russian,
    /// Chinese — hermitdave/FrequencyWords full corpus; downloaded on first use.
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

    /// File name used when caching this language's dictionary.
    #[cfg(not(test))]
    fn cache_filename(self) -> &'static str {
        match self {
            Self::English => "en_full.txt",
            Self::German => "de_full.txt",
            Self::Spanish => "es_full.txt",
            Self::French => "fr_full.txt",
            Self::Hebrew => "he_full.txt",
            Self::Italian => "it_full.txt",
            Self::Russian => "ru_full.txt",
            Self::Chinese => "zh_cn_full.txt",
        }
    }

    /// Full raw-content URL of this language's frequency dictionary on GitHub.
    #[cfg(not(test))]
    fn download_url(self) -> &'static str {
        match self {
            Self::English => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/en/en_full.txt"
            }
            Self::German => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/de/de_full.txt"
            }
            Self::Spanish => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/es/es_full.txt"
            }
            Self::French => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/fr/fr_full.txt"
            }
            Self::Hebrew => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/he/he_full.txt"
            }
            Self::Italian => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/it/it_full.txt"
            }
            Self::Russian => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/ru/ru_full.txt"
            }
            Self::Chinese => {
                "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/zh_cn/zh_cn_full.txt"
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Minimum byte length of aggregated dialogue text required to attempt
/// language detection.  Texts shorter than this are too sparse for reliable
/// trigram-based detection.
const MIN_DETECT_BYTES: usize = 50;

/// Minimum corpus frequency for a dictionary entry to be included in the
/// spell-check index.
///
/// Raw web-corpus frequency dictionaries (such as hermitdave/FrequencyWords)
/// contain every token that ever appeared in the crawl — including typos.
/// A word like "доргу" (a slip of "дорогу") may occur hundreds of times
/// across billions of pages, giving it `count ≥ 1` and making SymSpell treat
/// it as a correctly-spelled word (edit-distance 0 exact match).
///
/// Raising this threshold discards entries whose frequency falls below the
/// cut-off, which removes corpus noise while retaining the entire everyday
/// vocabulary: even an uncommon word like "замшелый" appears thousands of
/// times in a large corpus.  For narrative game dialogue a value around 100
/// strikes a good balance.
const MIN_DICT_ENTRY_COUNT: i64 = 50;

// ---------------------------------------------------------------------------
// Per-language dictionary singletons
// ---------------------------------------------------------------------------

/// One [`OnceLock`] per language — initialised lazily on first use.
static CHECKERS: [OnceLock<SymSpell<UnicodeStringStrategy>>; 8] = [const { OnceLock::new() }; 8];

/// Process-wide [`ureq::Agent`] used for all dictionary downloads.
///
/// A 5-minute wall-clock timeout and a 60-second per-read stall timeout
/// guard against hung connections while still allowing large dictionaries
/// (30+ MB) to transfer on slow connections.
#[cfg(not(test))]
fn http_agent() -> &'static ureq::Agent {
    static AGENT: OnceLock<ureq::Agent> = OnceLock::new();
    AGENT.get_or_init(|| {
        ureq::AgentBuilder::new()
            .timeout(std::time::Duration::from_secs(300))
            .timeout_read(std::time::Duration::from_secs(60))
            .build()
    })
}

/// Returns the local cache path for `language`'s dictionary file.
///
/// Returns `None` if the platform cache directory cannot be determined.
#[cfg(not(test))]
fn dict_cache_path(language: SpellcheckLanguage) -> Option<std::path::PathBuf> {
    dirs::cache_dir().map(|d| d.join("urd-spellcheck").join(language.cache_filename()))
}

/// Ensures the dictionary for `language` exists on disk, downloading it if
/// necessary.  Returns the path to the cached file, or `None` on any failure.
///
/// ## Download strategy
///
/// 1. If the target path already exists, return it immediately (no network).
/// 2. Otherwise, create the parent directory, download from
///    [`SpellcheckLanguage::download_url`] using [`http_agent`], write to a
///    `.part` temp file, then atomically rename to the final path.
///    The `.part` file is removed on any I/O error so a subsequent run can
///    retry from scratch.
#[cfg(not(test))]
fn ensure_dict_cached(language: SpellcheckLanguage) -> Option<std::path::PathBuf> {
    let path = dict_cache_path(language)?;

    if path.exists() {
        log::debug!(
            "spellcheck: {:?} dictionary already cached at {}",
            language,
            path.display()
        );
        return Some(path);
    }

    // Create parent directory if needed.
    if let Some(parent) = path.parent()
        && let Err(e) = std::fs::create_dir_all(parent)
    {
        log::warn!(
            "spellcheck: cannot create cache dir {}: {e}",
            parent.display()
        );
        return None;
    }

    let url = language.download_url();
    log::info!(
        "spellcheck: {:?} dictionary not in cache — downloading from {url}",
        language
    );

    // Download to a .part temp file for atomic promotion.
    let tmp = path.with_extension("part");

    let response = match http_agent().get(url).call() {
        Ok(r) => r,
        Err(e) => {
            log::warn!("spellcheck: download failed for {:?}: {e}", language);
            return None;
        }
    };

    let mut file = match std::fs::File::create(&tmp) {
        Ok(f) => f,
        Err(e) => {
            log::warn!("spellcheck: cannot create temp file {}: {e}", tmp.display());
            return None;
        }
    };

    // into_reader() has no size limit in ureq 2.x — safe for 30+ MB files.
    let mut reader = response.into_reader();
    if let Err(e) = std::io::copy(&mut reader, &mut file) {
        log::warn!(
            "spellcheck: I/O error while writing {:?} dictionary: {e}",
            language
        );
        let _ = std::fs::remove_file(&tmp);
        return None;
    }

    // Promote .part → final path (atomic on POSIX, best-effort on Windows).
    if let Err(e) = std::fs::rename(&tmp, &path) {
        log::warn!(
            "spellcheck: cannot rename {} → {}: {e}",
            tmp.display(),
            path.display()
        );
        let _ = std::fs::remove_file(&tmp);
        return None;
    }

    log::info!(
        "spellcheck: {:?} dictionary cached at {} ({} bytes)",
        language,
        path.display(),
        std::fs::metadata(&path).map(|m| m.len()).unwrap_or(0)
    );
    Some(path)
}

/// Return (and lazily initialise) the [`SymSpell`] checker for `language`.
///
/// The dictionary for each language is downloaded on first use and cached
/// locally — see [`ensure_dict_cached`] for the full caching strategy.
fn spell_checker(language: SpellcheckLanguage) -> &'static SymSpell<UnicodeStringStrategy> {
    CHECKERS[language.index()].get_or_init(|| build_checker(language))
}

/// Build a SymSpell checker for `language` by downloading (if necessary) and
/// loading the full frequency dictionary from the user's cache.
///
/// Returns an empty checker if the dictionary is unavailable — spellcheck is
/// silently disabled for that session rather than crashing.
///
/// Dictionaries are sourced from
/// [hermitdave/FrequencyWords](https://github.com/hermitdave/FrequencyWords)
/// and cached under `{cache_dir}/urd-spellcheck/`.
#[cfg(not(test))]
fn build_checker(language: SpellcheckLanguage) -> SymSpell<UnicodeStringStrategy> {
    let mut checker: SymSpell<UnicodeStringStrategy> = SymSpellBuilder::default()
        .max_dictionary_edit_distance(2)
        .prefix_length(7)
        .count_threshold(MIN_DICT_ENTRY_COUNT)
        .build()
        .unwrap_or_default();

    let path = match ensure_dict_cached(language) {
        Some(p) => p,
        None => {
            log::warn!(
                "spellcheck: {:?} dictionary unavailable — spellcheck disabled for this session",
                language
            );
            return checker;
        }
    };

    let file = match std::fs::File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            log::warn!("spellcheck: cannot open {}: {e}", path.display());
            return checker;
        }
    };

    let reader = std::io::BufReader::new(file);
    let mut loaded: usize = 0;

    for line_result in reader.lines() {
        match line_result {
            Ok(line) => {
                if checker.load_dictionary_line(&line, 0, 1, " ") {
                    loaded += 1;
                }
            }
            Err(e) => {
                log::warn!(
                    "spellcheck: I/O error reading {:?} dictionary: {e}",
                    language
                );
                break;
            }
        }
    }

    log::info!(
        "spellcheck: {:?} dictionary loaded — {loaded} entries",
        language
    );
    checker
}

/// Test-mode checker: uses a small hard-coded word list so tests run without
/// network access or a pre-populated cache directory.
///
/// The checker is otherwise identical to the production version (same
/// [`SymSpell`] configuration, same [`UnicodeStringStrategy`]).
///
/// Words are chosen to cover every dialogue string that appears in this
/// module's test suite.  The `German` list covers `german_word_is_clean`;
/// other languages use an empty list (tests for them are skipped or use
/// `None` → auto-detect with long samples).
#[cfg(test)]
fn build_checker(language: SpellcheckLanguage) -> SymSpell<UnicodeStringStrategy> {
    let mut checker: SymSpell<UnicodeStringStrategy> = SymSpellBuilder::default()
        .max_dictionary_edit_distance(2)
        .prefix_length(7)
        .count_threshold(MIN_DICT_ENTRY_COUNT)
        .build()
        .unwrap_or_default();

    // Space-separated "word count" entries, identical to the on-disk format.
    let entries: &[&str] = match language {
        SpellcheckLanguage::English => &[
            // Core function words
            "the 10000000",
            "and 9500000",
            "was 9000000",
            // Test-dialogue nouns / verbs
            "hello 8000000",
            "world 8000000",
            "rocket 7000000",
            "launched 7000000",
            "successful 6000000",
            "your 6000000",
            "name 6000000",
            // Long auto-detect test sentence
            "adventurer 5000000",
            "walked 5000000",
            "through 5000000",
            "ancient 4500000",
            "forest 4500000",
            "carefully 4000000",
            "noticing 4000000",
            "every 3800000",
            "stone 3800000",
            "beneath 3600000",
            "tall 3600000",
            "trees 3400000",
            // Other test dialogues
            "beautiful 3400000",
            "day 3200000",
            "room 3200000",
            "felt 3000000",
            "oppressive 3000000",
            "visit 2800000",
            "today 2800000",
            "fight 2600000",
            "enemy 2400000",
            "run 2200000",
            "away 2000000",
            "attack 1800000",
        ],
        SpellcheckLanguage::German => &[
            "die 10000000",
            "und 9000000",
            "nicht 8000000",
            "ist 7000000",
            "das 6000000",
            "der 5000000",
            "ein 4000000",
            "ich 3000000",
        ],
        // All other languages: empty — their tests either use None
        // (auto-detect, which goes through the full pipeline) or are
        // expected to produce no errors (Misspelling requires a non-empty
        // suggestion vector, and an empty checker returns nothing).
        _ => &[],
    };

    for entry in entries {
        checker.load_dictionary_line(entry, 0, 1, " ");
    }
    checker
}

// ---------------------------------------------------------------------------
// whatlang helpers
// ---------------------------------------------------------------------------

/// Map a [`whatlang::Lang`] to a [`SpellcheckLanguage`] for which a bundled
/// dictionary exists.  Returns `None` for any unsupported language so that
/// spellcheck is skipped rather than silently misrouted.
fn map_whatlang_lang(lang: whatlang::Lang) -> Option<SpellcheckLanguage> {
    use whatlang::Lang;
    match lang {
        Lang::Eng => Some(SpellcheckLanguage::English),
        Lang::Deu => Some(SpellcheckLanguage::German),
        Lang::Spa => Some(SpellcheckLanguage::Spanish),
        Lang::Fra => Some(SpellcheckLanguage::French),
        Lang::Heb => Some(SpellcheckLanguage::Hebrew),
        Lang::Ita => Some(SpellcheckLanguage::Italian),
        Lang::Rus => Some(SpellcheckLanguage::Russian),
        Lang::Cmn => Some(SpellcheckLanguage::Chinese),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

/// Collect all player-visible text from the AST into a single
/// whitespace-separated buffer for language detection.
///
/// Two node kinds contribute:
///
/// - [`AstContent::Dialogue`] — only [`StringPart::Literal`] segments are
///   included; interpolations and escape sequences are skipped so variable
///   names don't skew the n-gram statistics.
/// - [`AstContent::MenuOption`] — the raw label string is appended directly;
///   it contains only plain text so no further filtering is needed.
pub fn aggregate_dialogue_text(ast: &Ast) -> String {
    let mut buf = String::new();
    walk_ast(ast, &mut |node| match node.content() {
        AstContent::Dialogue { content, .. } => {
            collect_content_literals(content, &mut buf);
        }
        AstContent::MenuOption { label, .. } => {
            buf.push_str(label);
            buf.push(' ');
        }
        _ => {}
    });
    buf
}

/// Detect the language of `text` and return the matching [`SpellcheckLanguage`].
///
/// Returns `None` when:
/// - `text` is shorter than [`MIN_DETECT_BYTES`] bytes (too sparse for reliable detection).
/// - `whatlang` returns `None` (no trigrams found — pure digits/punctuation).
/// - `whatlang` detection is not reliable (`info.is_reliable()` is `false`).
/// - The detected language has no corresponding bundled dictionary (e.g. Japanese).
///
/// The caller should treat `None` as "skip spellcheck" rather than "use English".
pub fn detect_language(text: &str) -> Option<SpellcheckLanguage> {
    if text.len() < MIN_DETECT_BYTES {
        log::debug!(
            "spellcheck: text too short ({} bytes) for language detection, skipping",
            text.len()
        );
        return None;
    }
    let info = whatlang::detect(text)?;
    if !info.is_reliable() {
        log::debug!(
            "spellcheck: detection unreliable (lang={:?}, confidence={:.2}), skipping",
            info.lang(),
            info.confidence()
        );
        return None;
    }
    let lang = map_whatlang_lang(info.lang());
    log::debug!(
        "spellcheck: detected {:?} → {:?} (confidence={:.2})",
        info.lang(),
        lang,
        info.confidence()
    );
    lang
}

/// Run the spellcheck pass over `ast`.
///
/// ## Language selection
///
/// - `forced_language = Some(lang)` — use `lang` directly, skipping detection.
///   Use this when the user has explicitly configured a language.
/// - `forced_language = None` — auto-detect via [`aggregate_dialogue_text`] +
///   [`detect_language`].  If the buffer is too short, detection is unreliable,
///   or the detected language has no bundled dictionary, the function returns
///   an empty `Vec` (no spellcheck performed).
///
/// ## Performance
///
/// Language detection is a single pre-pass over the AST that runs once per
/// `check` invocation.  SymSpell dictionaries are lazily initialised on first
/// use and cached for the process lifetime.
pub fn check(ast: &Ast, forced_language: Option<SpellcheckLanguage>) -> Vec<AnalysisError> {
    let language = match forced_language {
        Some(lang) => {
            log::debug!("spellcheck: using forced language {:?}", lang);
            lang
        }
        None => {
            let text = aggregate_dialogue_text(ast);
            match detect_language(&text) {
                Some(lang) => lang,
                None => {
                    log::debug!("spellcheck: language undetectable, skipping spellcheck");
                    return Vec::new();
                }
            }
        }
    };

    let checker = spell_checker(language);
    let mut errors = Vec::new();

    walk_ast(ast, &mut |node| {
        match node.content() {
            AstContent::Dialogue { content, .. } => {
                check_dialogue_content(content, checker, &mut errors);
            }
            // Menu option labels are player-visible text, just like dialogue
            // lines, and should be checked for spelling.
            AstContent::MenuOption { label, .. } => {
                check_text(label, node.span(), checker, &mut errors);
            }
            _ => {}
        }
    });

    errors
}

// ---------------------------------------------------------------------------
// Private helpers — aggregation
// ---------------------------------------------------------------------------

/// Dispatch literal collection to the appropriate handler for `content`'s shape.
///
/// Mirrors the same three content shapes handled by [`check_dialogue_content`]:
///
/// - `Value(Str(s))` — a single string literal.
/// - `Block(stmts)` / `ExprList(exprs)` — a list of string literals.
fn collect_content_literals(content: &Ast, buf: &mut String) {
    match content.content() {
        AstContent::Value(RuntimeValue::Str(s)) => push_literals(s, buf),
        AstContent::Block(items) | AstContent::ExprList(items) => {
            for item in items {
                if let AstContent::Value(RuntimeValue::Str(s)) = item.content() {
                    push_literals(s, buf);
                }
            }
        }
        _ => {}
    }
}

/// Append every [`StringPart::Literal`] segment in `s` to `buf`, separated by
/// spaces.  Interpolations and escape sequences are silently skipped so they
/// don't skew n-gram statistics.
fn push_literals(s: &ParsedString, buf: &mut String) {
    for part in s.parts() {
        if let StringPart::Literal(text) = part {
            buf.push_str(text);
            buf.push(' ');
        }
    }
}

// ---------------------------------------------------------------------------
// Private helpers — spell-checking
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
/// Delegates to [`check_text`] for each literal segment so that all
/// word-level logic lives in one place.
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
        check_text(text, span, checker, errors);
    }
}

/// Tokenise `text`, apply all skip predicates, and push one
/// [`AnalysisError::Misspelling`] for every word that SymSpell cannot match
/// at edit-distance 0.
///
/// `span` is used as the diagnostic location for every emitted error; callers
/// should pass the span of the surrounding AST node (dialogue content or menu
/// option label).
fn check_text(
    text: &str,
    span: SimpleSpan,
    checker: &SymSpell<UnicodeStringStrategy>,
    errors: &mut Vec<AnalysisError>,
) {
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
#[allow(clippy::expect_used)]
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

    // ── aggregate_dialogue_text includes menu options ────────────────────────

    #[test]
    fn aggregate_includes_menu_option_labels() {
        let ast = parse(
            r#"
label start {
    menu {
        "Fight the dragon" { end!() }
        "Run away quickly" { end!() }
    }
}
"#,
        );
        let buf = aggregate_dialogue_text(&ast);
        assert!(
            buf.contains("Fight"),
            "expected menu option label in buffer, got: {buf:?}"
        );
        assert!(
            buf.contains("Run"),
            "expected second menu option label in buffer, got: {buf:?}"
        );
    }

    // ── Menu option spellcheck ───────────────────────────────────────────────

    #[test]
    fn menu_option_typo_is_flagged() {
        let ast = parse(
            r#"
label start {
    menu {
        "Attak the enemy" { end!() }
        "Run away" { end!() }
    }
}
"#,
        );
        let errors = check(&ast, Some(SpellcheckLanguage::English));
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "Attak"));
        assert!(
            flagged,
            "expected 'Attak' in a menu option to be flagged, got: {errors:?}"
        );
    }

    #[test]
    fn menu_option_correct_spelling_is_silent() {
        let ast = parse(
            r#"
label start {
    menu {
        "Fight the enemy" { end!() }
        "Run away" { end!() }
    }
}
"#,
        );
        let errors = check(&ast, Some(SpellcheckLanguage::English));
        assert!(
            errors.is_empty(),
            "expected no errors for correctly-spelled menu options, got: {errors:?}"
        );
    }

    #[test]
    fn menu_option_typo_diagnostic_is_warning() {
        let ast = parse(
            r#"
label start {
    menu {
        "Attak the enemy" { end!() }
    }
}
"#,
        );
        let errors = check(&ast, Some(SpellcheckLanguage::English));
        let misspelling = errors
            .iter()
            .find(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "Attak"));
        assert!(
            misspelling.is_some(),
            "expected 'Attak' to be flagged, got: {errors:?}"
        );
        assert!(
            misspelling.is_some_and(|e| e.is_warning()),
            "menu option misspelling must be a warning"
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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

        let errors = check(&node, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::German));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
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
        let errors = check(&ast, Some(SpellcheckLanguage::English));
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "TheTavern"));
        assert!(
            !flagged,
            "CamelCase words must not be flagged as misspellings, got: {errors:?}"
        );
    }

    // ── aggregate_dialogue_text ──────────────────────────────────────────────

    #[test]
    fn aggregate_collects_literal_text_from_dialogue() {
        let ast = parse(
            r#"
label start {
    Narrator: "Hello world."
    Narrator: "Goodbye moon."
    end!()
}
"#,
        );
        let text = aggregate_dialogue_text(&ast);
        assert!(
            text.contains("Hello"),
            "expected 'Hello' in aggregated text: {text:?}"
        );
        assert!(
            text.contains("Goodbye"),
            "expected 'Goodbye' in aggregated text: {text:?}"
        );
    }

    #[test]
    fn aggregate_skips_non_dialogue_nodes() {
        // A label with only a function call — no dialogue nodes.
        let ast = parse(
            r#"
label start {
    end!()
}
"#,
        );
        let text = aggregate_dialogue_text(&ast);
        assert!(
            text.is_empty(),
            "script with no dialogue should produce empty buffer: {text:?}"
        );
    }

    // ── detect_language ──────────────────────────────────────────────────────

    #[test]
    fn detect_returns_none_for_short_text() {
        // Fewer than MIN_DETECT_BYTES bytes → None.
        assert!(detect_language("Hello world").is_none());
    }

    #[test]
    fn detect_returns_none_for_unreliable_detection() {
        // Pure digits / punctuation — whatlang returns None → our fn returns None.
        let gibberish = "1234567890 !@#$%^&*() 1234567890 !@#$%^&*() 1234567890 !@#$%^&*()";
        assert!(detect_language(gibberish).is_none());
    }

    #[test]
    fn detect_english_text() {
        // Natural prose (not a pangram) gives whatlang enough trigram signal
        // to produce a reliable English classification.
        let text = "The old library stood at the end of the narrow street, \
                    its stone walls covered in ivy and its windows dark even \
                    in the midday sun.";
        // >= 50 bytes, should be reliably detected as English.
        assert_eq!(detect_language(text), Some(SpellcheckLanguage::English));
    }

    #[test]
    fn detect_russian_text() {
        let text = "Быстрая коричневая лисица прыгает через ленивую собаку у старого рынка.";
        assert_eq!(detect_language(text), Some(SpellcheckLanguage::Russian));
    }

    #[test]
    fn detect_unsupported_language_returns_none() {
        // Japanese — no bundled dictionary → None (skip, not fallback-to-English).
        let text = "吾輩は猫である。名前はまだ無い。どこで生れたかとんと見当がつかぬ。何でも薄暗いじめじめした所でニャーニャー泣いていた事だけは記憶している。";
        assert!(
            detect_language(text).is_none(),
            "Japanese should return None (no bundled dictionary)"
        );
    }

    // ── map_whatlang_lang ────────────────────────────────────────────────────

    #[test]
    fn map_all_supported_langs() {
        use whatlang::Lang;
        assert_eq!(
            map_whatlang_lang(Lang::Eng),
            Some(SpellcheckLanguage::English)
        );
        assert_eq!(
            map_whatlang_lang(Lang::Deu),
            Some(SpellcheckLanguage::German)
        );
        assert_eq!(
            map_whatlang_lang(Lang::Spa),
            Some(SpellcheckLanguage::Spanish)
        );
        assert_eq!(
            map_whatlang_lang(Lang::Fra),
            Some(SpellcheckLanguage::French)
        );
        assert_eq!(
            map_whatlang_lang(Lang::Heb),
            Some(SpellcheckLanguage::Hebrew)
        );
        assert_eq!(
            map_whatlang_lang(Lang::Ita),
            Some(SpellcheckLanguage::Italian)
        );
        assert_eq!(
            map_whatlang_lang(Lang::Rus),
            Some(SpellcheckLanguage::Russian)
        );
        assert_eq!(
            map_whatlang_lang(Lang::Cmn),
            Some(SpellcheckLanguage::Chinese)
        );
    }

    #[test]
    fn map_unsupported_lang_returns_none() {
        use whatlang::Lang;
        assert_eq!(map_whatlang_lang(Lang::Jpn), None);
        assert_eq!(map_whatlang_lang(Lang::Kor), None);
        assert_eq!(map_whatlang_lang(Lang::Por), None);
    }

    // ── check with auto-detection ────────────────────────────────────────────

    #[test]
    fn check_auto_detects_english_and_flags_typo() {
        // Long enough English text with a deliberate typo — auto-detection picks
        // English and the typo is flagged.
        let ast = parse(
            r#"
label start {
    Narrator: "The adventurer walked through the ancient forest carefully, noticing every rockeet and stone beneath the tall trees."
    end!()
}
"#,
        );
        let errors = check(&ast, None);
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "rockeet"));
        assert!(
            flagged,
            "auto-detected English should flag 'rockeet': {errors:?}"
        );
    }

    #[test]
    fn check_auto_skips_when_text_too_short() {
        // Very short dialogue — below MIN_DETECT_BYTES → no spellcheck.
        let ast = parse(
            r#"
label start {
    Narrator: "Hi."
    end!()
}
"#,
        );
        let errors = check(&ast, None);
        assert!(
            errors.is_empty(),
            "too-short text must produce no spellcheck errors: {errors:?}"
        );
    }

    #[test]
    fn check_forced_language_skips_detection() {
        // Tiny text that would fail auto-detection, but forced language works.
        let ast = parse(
            r#"
label start {
    Narrator: "The rockeet."
    end!()
}
"#,
        );
        let errors = check(&ast, Some(SpellcheckLanguage::English));
        let flagged = errors
            .iter()
            .any(|e| matches!(e, AnalysisError::Misspelling { word, .. } if word == "rockeet"));
        assert!(
            flagged,
            "forced language should flag 'rockeet' even for short text: {errors:?}"
        );
    }
}
