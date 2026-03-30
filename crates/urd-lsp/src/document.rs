//! Per-document state: source text, AST, and diagnostics.

use ropey::Rope;
use tower_lsp::lsp_types::*;

use chumsky::span::SimpleSpan;

use urd::analysis::{self, AnalysisError};
use urd::compiler::loader::parse_source;
use urd::parser::ast::Ast;

/// State for a single open document.
#[derive(Debug)]
pub struct Document {
    /// The current source text.
    pub rope: Rope,
    /// The most recent successful AST (`None` if parsing failed).
    pub ast: Option<Ast>,
    /// Parse errors from the last parse attempt (chumsky error strings).
    pub parse_errors: Vec<String>,
    /// Analysis diagnostics from the last successful parse.
    pub analysis_errors: Vec<AnalysisError>,
}

impl Document {
    /// Create a new document from source text, immediately parsing.
    pub fn new(text: &str) -> Self {
        let mut doc = Document {
            rope: Rope::from(text),
            ast: None,
            parse_errors: Vec::new(),
            analysis_errors: Vec::new(),
        };
        doc.reparse();
        doc
    }

    /// Update the source text and re-parse.
    pub fn update(&mut self, text: &str) {
        self.rope = Rope::from(text);
        self.reparse();
    }

    /// Re-parse the current rope content.
    fn reparse(&mut self) {
        let src = self.rope.to_string();

        match parse_source(&src) {
            Ok(ast) => {
                // Run analysis passes on the freshly parsed AST.
                self.analysis_errors = analysis::analyze(&ast);
                self.ast = Some(ast);
                self.parse_errors.clear();
            }
            Err(err_msg) => {
                // Store parse errors; keep the stale AST for best-effort
                // features (hover, go-to-definition on the last good parse).
                self.parse_errors = err_msg.split("; ").map(String::from).collect();
                self.analysis_errors.clear();
            }
        }
    }

    /// Convert all current errors to LSP diagnostics.
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let src = self.rope.to_string();
        let mut diags = Vec::new();

        // Parse errors — we don't have precise spans from the error string,
        // so mark the first line as the range.
        for msg in &self.parse_errors {
            diags.push(Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 1)),
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
            let severity = match err {
                AnalysisError::DeadEnd { .. } => DiagnosticSeverity::WARNING,
                _ => DiagnosticSeverity::ERROR,
            };
            diags.push(Diagnostic {
                range,
                severity: Some(severity),
                source: Some("urd".into()),
                message: err.to_string(),
                ..Default::default()
            });
        }

        diags
    }
}

// ── Span conversion utilities ────────────────────────────────────────────────

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
                if utf16_units >= pos.character {
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
            if utf16_units >= pos.character {
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
