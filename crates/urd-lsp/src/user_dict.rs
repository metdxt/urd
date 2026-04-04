//! User dictionary for suppressing spellcheck false-positives.
//!
//! Words are stored one per line in a plain-text file (`.urd-dict` by default,
//! placed in the workspace root).  Entries are always lowercased.  Lines
//! beginning with `#` are treated as comments and ignored on load.
//!
//! The file is created on the first call to [`UserDictionary::add`] with a
//! one-line comment header so users know what it is.

use std::collections::HashSet;
use std::fs::OpenOptions;
use std::io::{self, BufRead, Write};
#[cfg(test)]
use std::path::Path;
use std::path::PathBuf;

/// A persistent, case-insensitive word list used to suppress spellcheck
/// false-positives in the Urd language server.
///
/// Words are stored one per line in a plain-text file on disk and kept in a
/// [`HashSet`] in memory for O(1) lookups.  All words are normalised to
/// lowercase before being inserted.
///
/// # File format
///
/// ```text
/// # Urd user dictionary – words in this file are not spell-checked.
/// myword
/// anotherterm
/// ```
///
/// Lines whose first non-whitespace character is `#` are treated as comments
/// and skipped during [`load`](Self::load).  Blank lines are also ignored.
pub struct UserDictionary {
    /// Filesystem path of the backing plain-text file.
    path: PathBuf,
    /// In-memory set of lowercased words.
    words: HashSet<String>,
}

impl UserDictionary {
    /// Create an empty dictionary associated with `path`.
    ///
    /// The file is **not** read or created until [`add`](Self::add) is called.
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self {
            path: path.into(),
            words: HashSet::new(),
        }
    }

    /// Load a dictionary from `path`.
    ///
    /// - If the file does not exist, returns an empty dictionary (not an error).
    /// - If the file exists but cannot be opened or a line cannot be decoded,
    ///   logs a warning via [`log::warn!`] and returns whatever was collected
    ///   before the failure (or an empty set for open errors).
    pub fn load(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        let mut words = HashSet::new();

        let file = match std::fs::File::open(&path) {
            Ok(f) => f,
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                return Self { path, words };
            }
            Err(e) => {
                log::warn!(
                    "Failed to open user dictionary at '{}': {e}",
                    path.display()
                );
                return Self { path, words };
            }
        };

        let reader = io::BufReader::new(file);
        for line in reader.lines() {
            match line {
                Ok(raw) => {
                    let trimmed = raw.trim();
                    if trimmed.is_empty() || trimmed.starts_with('#') {
                        continue;
                    }
                    words.insert(trimmed.to_lowercase());
                }
                Err(e) => {
                    log::warn!(
                        "Error reading line in user dictionary at '{}': {e}",
                        path.display()
                    );
                    break;
                }
            }
        }

        Self { path, words }
    }

    /// Returns `true` if `word` (compared case-insensitively) is in the
    /// dictionary.
    #[cfg(test)]
    pub fn contains(&self, word: &str) -> bool {
        self.words.contains(&word.to_lowercase())
    }

    /// Add `word` (lowercased) to the in-memory set and append it to the file.
    ///
    /// # Behaviour
    ///
    /// - If the word is already present (case-insensitive), this is a no-op
    ///   and `Ok(())` is returned immediately.
    /// - If the backing file does not yet exist it is created, and a comment
    ///   header is prepended before the new word so the file is human-readable.
    /// - The in-memory set is updated **before** the disk write.  Should the
    ///   write fail, the word is still present in memory for the lifetime of
    ///   this instance (best-effort persistence).
    ///
    /// # Errors
    ///
    /// Returns an [`io::Error`] if the file cannot be opened or the write fails.
    pub fn add(&mut self, word: &str) -> io::Result<()> {
        // Validate word: only alphabetic chars, hyphens, and apostrophes; max 100 chars.
        if word.is_empty()
            || word.len() > 100
            || !word
                .chars()
                .all(|c| c.is_alphabetic() || c == '\'' || c == '-')
        {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "invalid word"));
        }

        let lower = word.to_lowercase();

        if self.words.contains(&lower) {
            return Ok(());
        }

        // Update the in-memory set unconditionally – disk write is best-effort.
        self.words.insert(lower.clone());

        let needs_header = !self.path.exists();

        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)?;

        if needs_header {
            writeln!(
                file,
                "# Urd user dictionary \u{2013} words in this file are not spell-checked."
            )?;
        }

        writeln!(file, "{lower}")?;
        Ok(())
    }

    /// The full set of lowercased words currently held in memory.
    pub fn words(&self) -> &HashSet<String> {
        &self.words
    }

    /// The filesystem path this dictionary is backed by.
    #[cfg(test)]
    pub fn path(&self) -> &Path {
        &self.path
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    // ── Helpers ───────────────────────────────────────────────────────────────

    /// Return a unique temp-file path for each test.
    ///
    /// Combines the process id with the current sub-second nanosecond count so
    /// parallel test threads get different paths.
    fn temp_path(label: &str) -> PathBuf {
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time is before the Unix epoch")
            .subsec_nanos();
        std::env::temp_dir().join(format!(
            "urd_dict_test_{label}_{pid}_{nanos}.txt",
            pid = std::process::id(),
        ))
    }

    // ── Tests ─────────────────────────────────────────────────────────────────

    #[test]
    fn new_creates_empty_dict() {
        let path = temp_path("new_creates_empty_dict");
        let dict = UserDictionary::new(&path);

        assert!(
            !dict.contains("hello"),
            "brand-new dict must not contain any words"
        );
        assert!(dict.words().is_empty(), "brand-new dict must be empty");
        // File must not have been created.
        assert!(!path.exists(), "new() must not touch the filesystem");
    }

    #[test]
    fn add_word_is_then_found() {
        let path = temp_path("add_word_is_then_found");

        let mut dict = UserDictionary::new(&path);
        dict.add("hello").expect("add should succeed");

        assert!(dict.contains("hello"), "exact lowercase must be found");
        assert!(dict.contains("Hello"), "title case must be found");
        assert!(dict.contains("HELLO"), "all-caps must be found");

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn add_is_case_insensitive_stored_as_lowercase() {
        let path = temp_path("add_is_case_insensitive_stored_as_lowercase");

        let mut dict = UserDictionary::new(&path);
        dict.add("NASA").expect("add should succeed");

        assert!(
            dict.words().contains("nasa"),
            "word must be stored in lowercase"
        );
        assert!(
            !dict.words().contains("NASA"),
            "original casing must not appear in the set"
        );

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn add_duplicate_is_noop() {
        let path = temp_path("add_duplicate_is_noop");

        let mut dict = UserDictionary::new(&path);
        dict.add("hello").expect("first add should succeed");
        dict.add("hello")
            .expect("second add (exact) should succeed");
        dict.add("Hello").expect("third add (cased) should succeed");

        assert_eq!(
            dict.words().len(),
            1,
            "all three adds refer to the same word; only one entry expected"
        );

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn load_missing_file_returns_empty() {
        let path = temp_path("load_missing_file_returns_empty");
        // Make absolutely sure the file does not exist.
        let _ = std::fs::remove_file(&path);

        let dict = UserDictionary::load(&path);

        assert!(
            dict.words().is_empty(),
            "loading a nonexistent file must yield an empty dictionary"
        );
    }

    #[test]
    fn persist_and_reload() {
        let path = temp_path("persist_and_reload");

        {
            let mut dict = UserDictionary::new(&path);
            dict.add("persistent").expect("add should succeed");
        }

        // Construct a completely fresh instance from the same path.
        let dict = UserDictionary::load(&path);

        assert!(
            dict.contains("persistent"),
            "word must survive a round-trip through disk"
        );

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn comments_are_ignored_on_load() {
        let path = temp_path("comments_are_ignored_on_load");

        {
            let mut f = std::fs::File::create(&path).expect("create temp file");
            writeln!(f, "# this is a comment").expect("write comment line");
            writeln!(f, "hello").expect("write word");
            writeln!(f, "   # indented comment").expect("write indented comment");
            writeln!(f).expect("write blank line");
            writeln!(f, "world").expect("write second word");
        }

        let dict = UserDictionary::load(&path);

        assert!(dict.contains("hello"), "'hello' must be loaded");
        assert!(dict.contains("world"), "'world' must be loaded");
        assert_eq!(
            dict.words().len(),
            2,
            "only real words must appear; comments and blank lines must be ignored"
        );
        assert!(
            !dict.words().contains("# this is a comment"),
            "comment text must not become a word"
        );

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn add_creates_header_comment() {
        let path = temp_path("add_creates_header_comment");
        // Guarantee no pre-existing file.
        let _ = std::fs::remove_file(&path);

        let mut dict = UserDictionary::new(&path);
        dict.add("testword").expect("add should succeed");

        let contents = std::fs::read_to_string(&path).expect("read back dict file");

        assert!(
            contents.starts_with('#'),
            "file must begin with a '#' header comment on first creation; got: {contents:?}"
        );

        // The word itself must also be present in the file.
        assert!(
            contents.contains("testword"),
            "added word must appear in the file"
        );

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn reload_skips_header_written_by_add() {
        let path = temp_path("reload_skips_header_written_by_add");
        let _ = std::fs::remove_file(&path);

        let mut dict = UserDictionary::new(&path);
        dict.add("omega").expect("add should succeed");

        let reloaded = UserDictionary::load(&path);

        assert!(
            reloaded.contains("omega"),
            "word must be present after reload"
        );
        assert_eq!(
            reloaded.words().len(),
            1,
            "header comment must not inflate the word count"
        );

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn path_accessor_returns_correct_path() {
        let path = temp_path("path_accessor");
        let dict = UserDictionary::new(&path);
        assert_eq!(dict.path(), path.as_path());
    }

    // ── Validation tests ──────────────────────────────────────────────────────

    #[test]
    fn add_rejects_empty_word() {
        let path = temp_path("add_rejects_empty_word");
        let mut dict = UserDictionary::new(&path);
        let err = dict.add("").expect_err("empty word must be rejected");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(
            !path.exists(),
            "no file must be created for a rejected word"
        );
    }

    #[test]
    fn add_rejects_word_over_100_chars() {
        let path = temp_path("add_rejects_word_over_100_chars");
        let long_word = "a".repeat(101);
        let mut dict = UserDictionary::new(&path);
        let err = dict
            .add(&long_word)
            .expect_err("word over 100 chars must be rejected");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }

    #[test]
    fn add_accepts_word_exactly_100_chars() {
        let path = temp_path("add_accepts_word_exactly_100_chars");
        let word = "a".repeat(100);
        let mut dict = UserDictionary::new(&path);
        dict.add(&word).expect("100-char word must be accepted");
        assert!(dict.contains(&word));
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn add_rejects_word_with_digit() {
        let path = temp_path("add_rejects_word_with_digit");
        let mut dict = UserDictionary::new(&path);
        let err = dict
            .add("word1")
            .expect_err("word containing a digit must be rejected");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }

    #[test]
    fn add_rejects_word_with_special_char() {
        let path = temp_path("add_rejects_word_with_special_char");
        let mut dict = UserDictionary::new(&path);
        let err = dict
            .add("bad@word")
            .expect_err("word with '@' must be rejected");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }

    #[test]
    fn add_accepts_hyphenated_word() {
        let path = temp_path("add_accepts_hyphenated_word");
        let mut dict = UserDictionary::new(&path);
        dict.add("low-ceilinged")
            .expect("hyphenated word must be accepted");
        assert!(
            dict.contains("low-ceilinged"),
            "hyphenated word must be retrievable"
        );
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn add_accepts_word_with_apostrophe() {
        let path = temp_path("add_accepts_word_with_apostrophe");
        let mut dict = UserDictionary::new(&path);
        dict.add("don't")
            .expect("word with apostrophe must be accepted");
        assert!(
            dict.contains("don't"),
            "word with apostrophe must be retrievable"
        );
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn add_rejected_word_not_inserted_into_memory() {
        let path = temp_path("add_rejected_word_not_inserted_into_memory");
        let mut dict = UserDictionary::new(&path);
        let _ = dict.add("bad@word");
        assert!(
            !dict.contains("bad@word"),
            "rejected word must not appear in the in-memory set"
        );
        assert!(
            dict.words().is_empty(),
            "in-memory set must remain empty after a rejected add"
        );
    }
}
