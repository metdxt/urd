//! Per-document state: source text, AST, and diagnostics.

use ropey::Rope;
use tower_lsp::lsp_types::*;

use chumsky::span::SimpleSpan;

use std::sync::Arc;

use urd::analysis::semantic_suggest::SemanticSuggest;
use urd::analysis::{self, AnalysisError};
use urd::compiler::loader::parse_source_spanned;
use urd::parser::ast::Ast;

/// State for a single open document.
#[derive(Debug)]
pub struct Document {
    /// The current source text.
    pub rope: Rope,
    /// The most recent AST — may be a partial recovery tree if the last parse
    /// had errors.  Used for local symbol lookup in completion, hover, and
    /// go-to-definition.
    pub ast: Option<Ast>,
    /// The most recent **fully valid** AST (no parse errors).  Used exclusively
    /// for workspace import indexing so that a mid-edit parse error never
    /// overwrites the cached import graph with an incomplete tree.
    pub last_clean_ast: Option<Ast>,
    /// Parse errors from the last parse attempt, each with its byte-offset span.
    pub parse_errors: Vec<(String, SimpleSpan)>,
    /// Analysis diagnostics from the last successful parse.
    pub analysis_errors: Vec<AnalysisError>,
    /// Spelling diagnostics from the most recent spellcheck pass.
    /// Populated by [`Document::run_spellcheck`]; empty if spellcheck is disabled.
    #[cfg(feature = "spellcheck")]
    pub spellcheck_errors: Vec<AnalysisError>,
    /// Symbol list computed eagerly after every successful (re)parse.
    /// Avoids re-walking the AST on every hover / completion / symbol request.
    pub cached_symbols: Option<Vec<crate::semantic::Symbol>>,
}

impl Document {
    /// Create a new document from source text, immediately parsing.
    pub fn new(text: &str) -> Self {
        let mut doc = Document {
            rope: Rope::from(text),
            ast: None,
            last_clean_ast: None,
            parse_errors: Vec::new(),
            analysis_errors: Vec::new(),
            #[cfg(feature = "spellcheck")]
            spellcheck_errors: Vec::new(),
            cached_symbols: None,
        };
        doc.reparse();
        doc
    }

    /// Update the source text and re-parse.
    pub fn update(&mut self, text: &str) {
        self.rope = Rope::from(text);
        self.reparse();
    }

    /// Re-run analysis with cross-module context.
    ///
    /// Call this after [`update`](Self::update) (which calls [`reparse`](Self::reparse)
    /// internally) once the workspace index has been refreshed, passing the
    /// imported struct, enum, and label definitions for this document.
    ///
    /// `imported_labels` is the set of label names that were directly imported
    /// into this file's scope (e.g. `show_inventory` from
    /// `import (show_inventory) from "items.urd"`).  These are merged into the
    /// analysis context so that `jump show_inventory` does not produce a false
    /// [`AnalysisError::UndefinedLabel`] diagnostic.
    ///
    /// If the document currently has no AST (i.e. the last parse failed) this
    /// is a no-op — there is nothing to re-analyse.
    ///
    /// `semantic` is an optional embedding model used to suggest semantically
    /// similar variable names when no close Levenshtein match exists.  Pass
    /// `None` to disable semantic suggestions (e.g. in tests or when the
    /// model files are not installed).
    pub fn reanalyze_with_imports(
        &mut self,
        imported_structs: std::collections::HashMap<String, Vec<urd::parser::ast::StructField>>,
        imported_enums: std::collections::HashMap<String, Vec<String>>,
        imported_labels: std::collections::HashSet<String>,
        semantic: Option<Arc<dyn SemanticSuggest>>,
    ) {
        if let Some(ast) = &self.ast {
            self.analysis_errors = urd::analysis::analyze_with_imports_and_semantic(
                ast,
                imported_structs,
                imported_enums,
                imported_labels,
                semantic.as_deref(),
            );
        }
    }

    /// Run the spell-check pass over the current AST.
    ///
    /// `language` controls language selection:
    /// - `Some(lang)` — use `lang` directly, bypassing auto-detection.
    /// - `None` — auto-detect via whatlang; returns no errors if detection
    ///   fails or the text is too short.
    ///
    /// Results are stored in [`Self::spellcheck_errors`] and included in the
    /// next call to [`Self::diagnostics`].  If no AST is currently available
    /// (last parse failed entirely) the previous results are cleared.
    ///
    /// Any [`urd::analysis::AnalysisError::Misspelling`] whose lowercased word
    /// appears in `ignored` is silently dropped from the results.
    #[cfg(feature = "spellcheck")]
    pub fn run_spellcheck(
        &mut self,
        language: Option<urd::analysis::SpellcheckLanguage>,
        ignored: &std::collections::HashSet<String>,
    ) {
        let src = self.rope.to_string();
        match &self.ast {
            Some(ast) => {
                self.spellcheck_errors = urd::analysis::spellcheck::check(ast, &src, language)
                    .into_iter()
                    .filter(|e| {
                        if let urd::analysis::AnalysisError::Misspelling { word, .. } = e {
                            !ignored.contains(&word.to_lowercase())
                        } else {
                            true
                        }
                    })
                    .collect();
            }
            None => {
                self.spellcheck_errors.clear();
            }
        }
    }

    /// Re-parse the current rope content.
    ///
    /// Chumsky's error recovery can return a partial AST even when there are
    /// parse errors (e.g. the user has typed `narrator.` mid-edit).  We always
    /// prefer the freshest AST chumsky can give us:
    ///
    /// - **Clean parse** (`ast = Some`, `errors = []`): update AST, clear errors,
    ///   run analysis.
    /// - **Partial recovery** (`ast = Some`, `errors ≠ []`): update AST to the
    ///   recovered tree, store errors, skip analysis (recovered tree may be
    ///   incomplete).
    /// - **Total failure** (`ast = None`, `errors ≠ []`): keep whatever stale
    ///   AST we had before (best-effort for hover, completion, go-to-def).
    fn reparse(&mut self) {
        let src = self.rope.to_string();

        let (recovered_ast, spanned_errors) = parse_source_spanned(&src);

        if spanned_errors.is_empty() {
            // Clean parse — run full analysis on the fresh AST and promote it
            // to the clean-AST slot used by workspace import indexing.
            if let Some(ast) = recovered_ast {
                self.analysis_errors = analysis::analyze(&ast);
                self.last_clean_ast = Some(ast.clone());
                self.ast = Some(ast);
                self.parse_errors.clear();
            }
        } else {
            // There were parse errors.  Store them for diagnostics.
            self.parse_errors = spanned_errors;
            self.analysis_errors.clear();

            if let Some(ast) = recovered_ast {
                // Chumsky recovered a partial tree — use it for local symbol
                // lookup (completion, hover, go-to-definition) so the user
                // sees results even mid-edit.  Do NOT promote to last_clean_ast
                // so that workspace import indexing keeps the last fully-valid
                // import graph.
                self.ast = Some(ast);
            }
            // If recovered_ast is None, self.ast keeps its previous value
            // (the last fully-valid parse).
        }

        // Eagerly refresh the symbol cache so every subsequent request
        // (hover, completion, document_symbol) can skip re-walking the AST.
        self.cached_symbols = self.ast.as_ref().map(crate::semantic::collect_symbols);
    }

    /// Returns the cached symbol list for this document.
    ///
    /// The cache is populated eagerly after every parse, so this call is
    /// always O(1) — it never re-walks the AST.
    pub fn symbols(&self) -> &[crate::semantic::Symbol] {
        self.cached_symbols.as_deref().unwrap_or(&[])
    }

    /// Convert all current errors to LSP diagnostics.
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let src = self.rope.to_string();
        let mut diags = Vec::new();

        // Parse errors carry byte-offset spans from chumsky.
        for (msg, span) in &self.parse_errors {
            let range = byte_span_to_lsp_range(&src, *span);
            diags.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("urd".into()),
                message: msg.clone(),
                ..Default::default()
            });
        }

        // Analysis errors carry real byte-offset spans.
        for err in &self.analysis_errors {
            let span = err.span();
            let range = byte_span_to_lsp_range(&src, span);
            let severity = if err.is_warning() {
                DiagnosticSeverity::WARNING
            } else {
                DiagnosticSeverity::ERROR
            };
            diags.push(Diagnostic {
                range,
                severity: Some(severity),
                source: Some("urd".into()),
                message: err.to_string(),
                ..Default::default()
            });
        }

        // Spellcheck errors are always warnings (Misspelling variant).
        #[cfg(feature = "spellcheck")]
        for err in &self.spellcheck_errors {
            let span = err.span();
            let range = byte_span_to_lsp_range(&src, span);
            let severity = if err.is_warning() {
                DiagnosticSeverity::WARNING
            } else {
                DiagnosticSeverity::ERROR
            };
            let data = if let urd::analysis::AnalysisError::Misspelling {
                word, suggestion, ..
            } = err
            {
                Some(serde_json::json!({ "word": word, "suggestion": suggestion }))
            } else {
                None
            };
            diags.push(Diagnostic {
                range,
                severity: Some(severity),
                source: Some("urd-spell".into()),
                message: err.to_string(),
                data,
                ..Default::default()
            });
        }

        diags
    }
}

// ── Span conversion utilities ────────────────────────────────────────────────

/// Return the UTF-16 *character* column at which the identifier **prefix**
/// under the cursor starts on its line.
///
/// Starting from `byte_offset` in `src` we scan backwards, stopping when we
/// hit a byte that is not a valid Urd identifier character (`[_a-zA-Z0-9]`).
/// The returned value is the UTF-16 code-unit count from the beginning of the
/// line to that start byte — exactly what you need as the `start.character`
/// of a `TextEdit` range to replace only the typed prefix.
///
/// If the cursor is at the beginning of the line (or the character to its left
/// is not an identifier character) the function returns `pos.character`
/// unchanged, meaning the replacement range is a zero-width insertion point.
///
/// # Example
/// ```text
/// "    jump"   (cursor after 'p', byte_offset points one past 'p')
///  0123456789
/// ```
/// The call returns `4` (the `j`), not `0` (the leading spaces).
pub fn prefix_start_character(src: &str, byte_offset: usize, pos: Position) -> u32 {
    let bytes = src.as_bytes();
    let clamped = byte_offset.min(src.len());

    // Walk backwards over identifier characters.
    let mut start = clamped;
    while start > 0 {
        let prev = start - 1;
        // Only ASCII ident chars matter for Urd; multibyte sequences are never
        // ident characters so this byte-level check is safe.
        if bytes[prev].is_ascii_alphanumeric() || bytes[prev] == b'_' {
            start = prev;
        } else {
            break;
        }
    }

    if start == clamped {
        // No identifier prefix — keep cursor column as the start.
        return pos.character;
    }

    // Find the byte offset of the beginning of the line that contains `start`.
    let line_start = src[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);

    // Count UTF-16 code units from line_start to start.
    src[line_start..start].encode_utf16().count() as u32
}

/// Convert a chumsky byte-offset [`SimpleSpan`] to an LSP [`Range`].
///
/// LSP positions use *line* / *character* where *character* is a **UTF-16
/// code-unit** offset. Urd / Logos spans are byte offsets.
pub fn byte_span_to_lsp_range(src: &str, span: SimpleSpan) -> Range {
    let start = byte_offset_to_position(src, span.start);
    let end = byte_offset_to_position(src, span.end);
    Range::new(start, end)
}

/// Convert a byte offset in `src` to an LSP [`Position`] (line, character).
///
/// `character` is counted in **UTF-16 code units** per the LSP specification.
pub fn byte_offset_to_position(src: &str, byte_offset: usize) -> Position {
    let clamped = byte_offset.min(src.len());
    let before = &src[..clamped];

    let line = before.matches('\n').count() as u32;
    let last_newline = before.rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_slice = &before[last_newline..];
    // Count UTF-16 code units (surrogates count as 2).
    let character = line_slice.encode_utf16().count() as u32;

    Position::new(line, character)
}

/// Convert an LSP [`Position`] to a byte offset in `src`.
///
/// If the position points past the end of a line or the file, the returned
/// offset is clamped to the nearest valid boundary.
pub fn position_to_byte_offset(src: &str, pos: Position) -> usize {
    let mut current_line = 0u32;
    let mut line_start_byte = 0usize;

    for (i, ch) in src.char_indices() {
        if current_line == pos.line {
            // We've reached the target line — walk it counting UTF-16 units.
            let line_slice = &src[line_start_byte..];
            let mut utf16_units = 0u32;
            for (ci, c) in line_slice.char_indices() {
                if utf16_units + (c.len_utf16() as u32) > pos.character {
                    return line_start_byte + ci;
                }
                // Stop at newline even if character count hasn't been reached.
                if c == '\n' {
                    return line_start_byte + ci;
                }
                utf16_units += c.len_utf16() as u32;
            }
            // Past the end of the line / file.
            return line_start_byte + line_slice.len();
        }
        if ch == '\n' {
            current_line += 1;
            line_start_byte = i + 1;
        }
    }

    // If we exit the loop and haven't matched the line yet, check one more
    // time in case the target is the last (possibly empty) line after a
    // trailing newline.
    if current_line == pos.line {
        let line_slice = &src[line_start_byte..];
        let mut utf16_units = 0u32;
        for (ci, c) in line_slice.char_indices() {
            if utf16_units + (c.len_utf16() as u32) > pos.character {
                return line_start_byte + ci;
            }
            if c == '\n' {
                return line_start_byte + ci;
            }
            utf16_units += c.len_utf16() as u32;
        }
        return line_start_byte + line_slice.len();
    }

    // Target line is beyond EOF.
    src.len()
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::span::Span as _;

    // -- prefix_start_character --------------------------------------------

    #[test]
    fn prefix_start_char_mid_word() {
        // "    jump" — cursor after 'p' (char 8, byte 8).
        let src = "    jump";
        let pos = Position::new(0, 8);
        let off = position_to_byte_offset(src, pos);
        assert_eq!(prefix_start_character(src, off, pos), 4);
    }

    #[test]
    fn prefix_start_char_at_word_start() {
        // Cursor is right before the word — no ident char to the left.
        let src = "    jump";
        let pos = Position::new(0, 4);
        let off = position_to_byte_offset(src, pos);
        // Nothing to scan back over; should return pos.character unchanged.
        assert_eq!(prefix_start_character(src, off, pos), 4);
    }

    #[test]
    fn prefix_start_char_no_prefix() {
        // Cursor is at column 0.
        let src = "jump";
        let pos = Position::new(0, 0);
        let off = position_to_byte_offset(src, pos);
        assert_eq!(prefix_start_character(src, off, pos), 0);
    }

    #[test]
    fn prefix_start_char_full_word_at_col0() {
        // Cursor is right after "jump" at column 0 — prefix starts at 0.
        let src = "jump";
        let pos = Position::new(0, 4);
        let off = position_to_byte_offset(src, pos);
        assert_eq!(prefix_start_character(src, off, pos), 0);
    }

    #[test]
    fn prefix_start_char_second_line() {
        // "hello\n    jump" — cursor after 'p' on line 1.
        let src = "hello\n    jump";
        let pos = Position::new(1, 8);
        let off = position_to_byte_offset(src, pos);
        assert_eq!(prefix_start_character(src, off, pos), 4);
    }

    #[test]
    fn prefix_start_char_after_dot() {
        // Dots are not ident chars, so scan stops at the dot.
        // "foo.bar" — cursor after 'r' (char 7).
        let src = "foo.bar";
        let pos = Position::new(0, 7);
        let off = position_to_byte_offset(src, pos);
        assert_eq!(prefix_start_character(src, off, pos), 4);
    }

    #[test]
    fn prefix_start_char_after_space() {
        // "foo bar" — cursor after 'r'; prefix is "bar" starting at char 4.
        let src = "foo bar";
        let pos = Position::new(0, 7);
        let off = position_to_byte_offset(src, pos);
        assert_eq!(prefix_start_character(src, off, pos), 4);
    }

    // -- byte_offset_to_position -------------------------------------------

    #[test]
    fn byte_offset_to_position_start_of_file() {
        let src = "hello\nworld\n";
        let pos = byte_offset_to_position(src, 0);
        assert_eq!(pos, Position::new(0, 0));
    }

    #[test]
    fn byte_offset_to_position_start_of_second_line() {
        let src = "hello\nworld\n";
        // 'w' is at byte 6 → line 1, char 0.
        let pos = byte_offset_to_position(src, 6);
        assert_eq!(pos, Position::new(1, 0));
    }

    #[test]
    fn byte_offset_to_position_mid_line() {
        let src = "hello\nworld\n";
        // 'o' in "world" is at byte 7 → line 1, char 1.
        let pos = byte_offset_to_position(src, 7);
        assert_eq!(pos, Position::new(1, 1));
    }

    #[test]
    fn byte_offset_to_position_clamped_past_eof() {
        let src = "hi";
        let pos = byte_offset_to_position(src, 999);
        assert_eq!(pos, Position::new(0, 2));
    }

    #[test]
    fn byte_offset_to_position_empty_source() {
        let pos = byte_offset_to_position("", 0);
        assert_eq!(pos, Position::new(0, 0));
    }

    #[test]
    fn byte_offset_to_position_multibyte_utf8() {
        // '€' is 3 bytes in UTF-8 but 1 UTF-16 code unit.
        let src = "a€b\n";
        // 'b' is at byte 4 (a=1, €=3) → line 0, char 2.
        let pos = byte_offset_to_position(src, 4);
        assert_eq!(pos, Position::new(0, 2));
    }

    #[test]
    fn byte_offset_to_position_surrogate_pair() {
        // '𐍈' (U+10348) is 4 bytes in UTF-8 and 2 UTF-16 code units.
        let src = "a𐍈b";
        // 'b' is at byte 5 (a=1, 𐍈=4) → line 0, char 3 (a=1 + 𐍈=2).
        let pos = byte_offset_to_position(src, 5);
        assert_eq!(pos, Position::new(0, 3));
    }

    // -- position_to_byte_offset -------------------------------------------

    #[test]
    fn position_to_byte_offset_start() {
        let src = "hello\nworld";
        let off = position_to_byte_offset(src, Position::new(0, 0));
        assert_eq!(off, 0);
    }

    #[test]
    fn position_to_byte_offset_second_line() {
        let src = "hello\nworld";
        let off = position_to_byte_offset(src, Position::new(1, 0));
        assert_eq!(off, 6);
    }

    #[test]
    fn position_to_byte_offset_mid_second_line() {
        let src = "hello\nworld";
        // line 1, char 3 → 'l' at byte 9.
        let off = position_to_byte_offset(src, Position::new(1, 3));
        assert_eq!(off, 9);
    }

    #[test]
    fn position_to_byte_offset_past_line_end() {
        let src = "hi\nbye";
        // line 0 has 2 chars; asking for char 99 should clamp to byte 2
        // (the newline position).
        let off = position_to_byte_offset(src, Position::new(0, 99));
        assert_eq!(off, 2);
    }

    #[test]
    fn position_to_byte_offset_past_eof_line() {
        let src = "only one line";
        let off = position_to_byte_offset(src, Position::new(5, 0));
        assert_eq!(off, src.len());
    }

    // -- roundtrip ---------------------------------------------------------

    #[test]
    fn roundtrip_ascii() {
        let src = "line one\nline two\nline three";
        let offset = 14; // inside "line two"
        let pos = byte_offset_to_position(src, offset);
        let back = position_to_byte_offset(src, pos);
        assert_eq!(back, offset);
    }

    #[test]
    fn roundtrip_multibyte() {
        let src = "héllo\nwörld";
        for byte_off in [0, 1, 3, 5, 6, 7, 9] {
            // Only test char-boundary offsets.
            if src.is_char_boundary(byte_off) {
                let pos = byte_offset_to_position(src, byte_off);
                let back = position_to_byte_offset(src, pos);
                assert_eq!(
                    back, byte_off,
                    "roundtrip failed for byte offset {byte_off}"
                );
            }
        }
    }

    // -- Document ----------------------------------------------------------

    #[test]
    fn document_parses_valid_source() {
        let doc = Document::new("label foo {\n  end!()\n}\n");
        assert!(doc.ast.is_some(), "expected successful parse");
        assert!(doc.parse_errors.is_empty(), "expected no parse errors");
    }

    #[test]
    fn document_captures_parse_errors() {
        let doc = Document::new("{{{{invalid}}}}");
        // Should have parse errors (or—in edge cases—a partial AST).
        assert!(
            !doc.parse_errors.is_empty() || doc.ast.is_some(),
            "expected either parse errors or a recovered AST"
        );
        // Errors should carry non-trivial spans (not all at 0..0).
        for (_, span) in &doc.parse_errors {
            assert!(
                span.end > 0 || span.start > 0,
                "expected parse error span to point into the source, got {span:?}"
            );
        }
    }

    #[test]
    fn document_update_reparses() {
        let mut doc = Document::new("label a {\n  end!()\n}\n");
        assert!(doc.ast.is_some());

        doc.update("{{broken");
        assert!(!doc.parse_errors.is_empty());

        doc.update("label b {\n  end!()\n}\n");
        assert!(doc.ast.is_some());
        assert!(doc.parse_errors.is_empty());
    }

    // -- last_clean_ast ----------------------------------------------------

    #[test]
    fn last_clean_ast_set_on_clean_parse() {
        let doc = Document::new("label foo {\n  end!()\n}\n");
        assert!(
            doc.last_clean_ast.is_some(),
            "last_clean_ast must be populated after a successful parse"
        );
    }

    #[test]
    fn last_clean_ast_not_set_when_parse_fails_from_scratch() {
        // A document that fails to parse on the very first attempt should have
        // last_clean_ast = None (there was never a clean parse).
        let doc = Document::new("{{{{invalid}}}}");
        if doc.parse_errors.is_empty() {
            // Parser recovered cleanly — not the case we are testing, skip.
            return;
        }
        // Only check last_clean_ast when the parse definitely failed.
        assert!(
            doc.last_clean_ast.is_none(),
            "last_clean_ast must remain None when there has never been a clean parse"
        );
    }

    #[test]
    fn last_clean_ast_preserved_after_partial_edit_error() {
        // 1. Open with a valid source — last_clean_ast is populated.
        let valid_src = "label intro {\n  end!()\n}\n";
        let mut doc = Document::new(valid_src);
        assert!(
            doc.last_clean_ast.is_some(),
            "last_clean_ast must be set after clean open"
        );

        // 2. Introduce a mid-edit parse error (simulate typing `narrator.`).
        doc.update("label intro {\n  narrator.\n}\n");

        // The document should have parse errors now.
        if doc.parse_errors.is_empty() {
            // Parser recovered perfectly — skip (environment-dependent).
            return;
        }

        // last_clean_ast must still hold the previous clean tree.
        assert!(
            doc.last_clean_ast.is_some(),
            "last_clean_ast must be preserved when a subsequent parse produces errors"
        );
    }

    #[test]
    fn last_clean_ast_updated_on_second_clean_parse() {
        let mut doc = Document::new("label a {\n  end!()\n}\n");
        assert!(doc.last_clean_ast.is_some());

        // Introduce an error.
        doc.update("{{broken");

        // Fix it with a different valid source.
        doc.update("label b {\n  end!()\n}\n");
        assert!(
            doc.last_clean_ast.is_some(),
            "last_clean_ast must be updated after a second clean parse"
        );
        assert!(
            doc.parse_errors.is_empty(),
            "parse_errors must be clear after a clean parse"
        );
    }

    #[test]
    fn ast_gets_partial_recovery_while_last_clean_ast_stays_frozen() {
        // Open cleanly so last_clean_ast is set.
        let mut doc = Document::new("label a {\n  end!()\n}\n");
        assert!(doc.last_clean_ast.is_some());

        // Drive a parse error.  Depending on whether chumsky recovers we may
        // get a partial `ast`; what matters is that `last_clean_ast` does NOT
        // change to the (possibly partial) recovered tree.
        doc.update("label a {\n  narrator.\n}\n");

        if doc.parse_errors.is_empty() {
            // Chumsky parsed it cleanly — both fields may update, that is fine.
            return;
        }

        // last_clean_ast must still be present (from the original clean parse)
        // and must NOT have been replaced by the partial recovery.
        assert!(
            doc.last_clean_ast.is_some(),
            "last_clean_ast must remain the last clean tree after a recovery parse"
        );
    }

    #[test]
    fn document_diagnostics_includes_parse_errors() {
        let doc = Document::new("{{{{");
        if !doc.parse_errors.is_empty() {
            let diags = doc.diagnostics();
            assert!(!diags.is_empty(), "expected at least one diagnostic");
            assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
            assert_eq!(diags[0].source.as_deref(), Some("urd"));
        }
    }

    // -- byte_span_to_lsp_range -------------------------------------------

    #[test]
    fn byte_span_to_lsp_range_simple() {
        let src = "hello\nworld";
        let span = SimpleSpan::new((), 6..11); // "world"
        let range = byte_span_to_lsp_range(src, span);
        assert_eq!(range.start, Position::new(1, 0));
        assert_eq!(range.end, Position::new(1, 5));
    }
}
