//! # Parse Error Rendering
//!
//! Pretty-prints chumsky [`Rich`] parse errors using ariadne for rich,
//! source-annotated diagnostics — the same visual style as the static
//! analysis error output in [`crate::analysis`].
//!
//! ## Usage
//!
//! ```rust,ignore
//! use crate::parser::errors::render_parse_errors_stderr;
//!
//! let result = script().parse(stream).into_result();
//! if let Err(errs) = result {
//!     render_parse_errors_stderr(&errs, src, "my_script.urd");
//! }
//! ```
//!
//! ## Note
//!
//! This module must be declared in `parser/mod.rs` with `pub mod errors;`.

use std::collections::HashSet;

use ariadne::{Label, Report, ReportKind, sources};
use chumsky::error::{Rich, RichPattern, RichReason};
use chumsky::span::SimpleSpan;

use crate::lexer::Token;

// ── Token display helpers ──────────────────────────────────────────────────

/// Format a single [`Token`] into a short, human-readable description.
///
/// This is intentionally more user-friendly than the raw `Debug`/`Display`
/// output from `logos_display`, which exposes Rust type names and internal
/// structure. For example, `Token::IdentPath(["foo", "bar"])` becomes
/// `"identifier 'foo.bar'"` instead of `"IdentPath(["foo", "bar"])"`.
fn format_token(token: &Token) -> String {
    match token {
        // ── Literals ──────────────────────────────────────────────────────
        Token::Null => "'null'".to_owned(),
        Token::BoolLit(b) => format!("'{b}'"),
        Token::IntLit(n) => format!("integer '{n}'"),
        Token::FloatLit(f) => format!("float '{f}'"),
        Token::StrLit(_) => "string literal".to_owned(),
        Token::Dice((c, s)) => format!("dice '{c}d{s}'"),
        Token::IdentPath(p) => format!("identifier '{}'", p.join(".")),

        // ── Whitespace / structure ─────────────────────────────────────────
        Token::Newline => "newline".to_owned(),

        // ── Keywords ──────────────────────────────────────────────────────
        Token::Const => "'const'".to_owned(),
        Token::Let => "'let'".to_owned(),
        Token::Global => "'global'".to_owned(),
        Token::If => "'if'".to_owned(),
        Token::Else => "'else'".to_owned(),
        Token::Elif => "'elif'".to_owned(),
        Token::Label => "'label'".to_owned(),
        Token::Menu => "'menu'".to_owned(),
        Token::Return => "'return'".to_owned(),
        Token::Jump => "'jump'".to_owned(),
        Token::EndBang => "'end!'".to_owned(),
        Token::TodoBang => "'todo!'".to_owned(),
        Token::Enum => "'enum'".to_owned(),
        Token::Struct => "'struct'".to_owned(),
        Token::Match => "'match'".to_owned(),
        Token::DecoratorKw => "'decorator'".to_owned(),
        Token::Import => "'import'".to_owned(),
        Token::From => "'from'".to_owned(),
        Token::As => "'as'".to_owned(),
        Token::Wildcard => "'_'".to_owned(),

        // ── Logical operators (keyword forms) ─────────────────────────────
        Token::And => "'and'/'&&'".to_owned(),
        Token::Or => "'or'/'||'".to_owned(),
        Token::Not => "'not'".to_owned(),

        // ── Arithmetic / bitwise operators ────────────────────────────────
        Token::Plus => "'+'".to_owned(),
        Token::Minus => "'-'".to_owned(),
        Token::Star => "'*'".to_owned(),
        Token::Slash => "'/'".to_owned(),
        Token::DoubleSlash => "'//'".to_owned(),
        Token::Percent => "'%'".to_owned(),
        Token::BitwiseAnd => "'&'".to_owned(),
        Token::BitwiseOr => "'|'".to_owned(),
        Token::BitwiseXor => "'^'".to_owned(),
        Token::BitwiseNot => "'!'".to_owned(),
        Token::LeftShift => "'<<'".to_owned(),
        Token::RightShift => "'>>'".to_owned(),

        // ── Comparison operators ───────────────────────────────────────────
        Token::Assign => "'='".to_owned(),
        Token::Equals => "'=='".to_owned(),
        Token::NotEquals => "'!='".to_owned(),
        Token::GreaterThan => "'>'".to_owned(),
        Token::LessThan => "'<'".to_owned(),
        Token::GreaterThanOrEquals => "'>='".to_owned(),
        Token::LessThanOrEquals => "'<='".to_owned(),

        // ── Delimiters / punctuation ──────────────────────────────────────
        Token::LeftParen => "'('".to_owned(),
        Token::RightParen => "')'".to_owned(),
        Token::LeftCurly => "'{'".to_owned(),
        Token::RightCurly => "'}'".to_owned(),
        Token::LeftBracket => "'['".to_owned(),
        Token::RightBracket => "']'".to_owned(),
        Token::DictStart => "':{'".to_owned(),
        Token::Colon => "':'".to_owned(),
        Token::Comma => "','".to_owned(),
        Token::Semicolon => "';'".to_owned(),
        Token::At => "'@'".to_owned(),

        // ── Lexer error embedded in token stream ──────────────────────────
        // This happens when the lexer encounters something it cannot tokenise
        // at all (e.g. a stray `§` or a malformed number). Surfacing the
        // underlying lexer message gives the user an actionable clue.
        Token::Error(e) => format!("invalid token: {e}"),

        // ── Documentation comment ─────────────────────────────────────────
        Token::DocComment(s) => format!("doc comment '## {s}'"),
    }
}

/// Format a [`RichPattern`] into a short, human-readable string.
///
/// `RichPattern` is how chumsky describes what it *expected* to find.
fn format_pattern(pattern: &RichPattern<'_, Token>) -> String {
    match pattern {
        RichPattern::Token(t) => format_token(t),
        // Labels come from `.labelled("…")` calls on parser combinators.
        RichPattern::Label(l) => l.to_string(),
        RichPattern::Identifier(i) => format!("'{i}'"),
        RichPattern::EndOfInput => "end of input".to_owned(),
        RichPattern::SomethingElse => "something else".to_owned(),
        RichPattern::Any => "any token".to_owned(),
    }
}

// ── Expected list formatting ───────────────────────────────────────────────

/// Build a human-readable "expected …" clause from a list of [`RichPattern`]s.
///
/// Deduplicates and sorts for stable output:
///
/// | Count | Output                                      |
/// |-------|---------------------------------------------|
/// | 0     | `""` (empty string)                         |
/// | 1     | `"expected foo"`                            |
/// | 2     | `"expected foo or bar"`                     |
/// | N     | `"expected one of: foo, bar, …, or baz"`    |
#[allow(clippy::expect_used)]
fn format_expected(patterns: &[RichPattern<'_, Token>]) -> String {
    let mut seen = HashSet::new();
    let mut items: Vec<String> = patterns
        .iter()
        .map(format_pattern)
        .filter(|s| seen.insert(s.clone()))
        .collect();

    // Stable ordering makes diagnostics reproducible across runs.
    items.sort();

    match items.as_slice() {
        [] => String::new(),
        [a] => format!("expected {a}"),
        [a, b] => format!("expected {a} or {b}"),
        many => {
            let (last, rest) = many.split_last().expect("non-empty slice");
            format!("expected one of: {}, or {last}", rest.join(", "))
        }
    }
}

// ── Main renderer ──────────────────────────────────────────────────────────

/// Render chumsky [`Rich`] parse errors as ariadne diagnostics, writing to
/// `writer`.
///
/// Each error produces one ariadne [`Report`] with:
///
/// - A **primary span** underlining the offending token and a
///   `"found X — expected Y"` message.
/// - **Secondary labels** for each `.labelled("…")` context that was active
///   when the error occurred — e.g. `"while parsing expression"` or
///   `"while parsing declaration"`.  These help orient the user in larger
///   constructs.
///
/// # Errors
///
/// Propagates any [`std::io::Error`] from writing to `writer`.
pub fn render_parse_errors<W: std::io::Write>(
    errors: &[Rich<'_, Token, SimpleSpan>],
    src: &str,
    source_name: &str,
    writer: &mut W,
) -> std::io::Result<()> {
    for error in errors {
        let span = *error.span();
        let src_len = src.len();
        let range_start = span.start.min(src_len);
        let range_end = span.end.min(src_len).max(range_start);
        let range = range_start..range_end;
        // Pin the primary caret to a single character at the token's start so
        // that ariadne's `┬` always points at the beginning of the offending
        // token rather than somewhere inside a wide (potentially multi-token)
        // span that chumsky produces after backtracking through `.or()` chains.
        //
        // Clamp to source bounds so EOF errors (where start == src.len()) never
        // create an out-of-bounds range like `len..len+1`.
        let caret_end = range_start.saturating_add(1).min(src_len);
        let caret_range = range_start..caret_end.max(range_start);
        let name = source_name.to_owned();

        // ── Derive the top-level message and primary label ─────────────────
        let (top_message, primary_label_msg) = match error.reason() {
            RichReason::ExpectedFound { expected, found } => {
                let found_str = match found {
                    Some(t) => format_token(t),
                    None => "end of input".to_owned(),
                };
                let expected_str = format_expected(expected);

                let top = if expected_str.is_empty() {
                    format!("unexpected {found_str}")
                } else {
                    format!("unexpected {found_str}, {expected_str}")
                };

                let label = if expected_str.is_empty() {
                    format!("unexpected {found_str} here")
                } else {
                    format!("found {found_str} — {expected_str}")
                };

                (top, label)
            }

            // Custom messages come from `.map_err` / `.validate` calls.
            RichReason::Custom(msg) => (msg.clone(), msg.clone()),
        };

        let mut report = Report::<(String, std::ops::Range<usize>)>::build(
            ReportKind::Error,
            (name.clone(), range.clone()),
        )
        .with_message(&top_message)
        .with_label(Label::new((name.clone(), caret_range)).with_message(&primary_label_msg));

        // ── Context labels from .labelled() calls ─────────────────────────
        //
        // These are added as secondary labels pointing at the *start* of the
        // construct that was being parsed, telling the user which grammar rule
        // the error falls inside.  We skip contexts whose span is identical
        // to the primary span (no extra information) or zero-length.
        for (ctx_pattern, ctx_span) in error.contexts() {
            let ctx_range = ctx_span.start..ctx_span.end;

            if ctx_range.is_empty() || ctx_range == range {
                continue;
            }

            report = report.with_label(
                Label::new((name.clone(), ctx_range))
                    .with_message(format!("while parsing {}", format_pattern(ctx_pattern))),
            );
        }

        report
            .finish()
            .write(sources([(name.clone(), src.to_owned())]), &mut *writer)
            .map_err(|e| std::io::Error::other(e.to_string()))?;
    }

    Ok(())
}

/// Convenience wrapper: render parse errors directly to `stderr`.
pub fn render_parse_errors_stderr(
    errors: &[Rich<'_, Token, SimpleSpan>],
    src: &str,
    source_name: &str,
) {
    let mut stderr = std::io::stderr();
    if let Err(e) = render_parse_errors(errors, src, source_name, &mut stderr) {
        eprintln!("warning: failed to render parse errors: {e}");
    }
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_test;
    use crate::parser::block::script;

    /// Parse `src` and return any errors. Panics if there are no errors (the
    /// test source must be syntactically broken on purpose).
    fn parse_errors(src: &str) -> Vec<String> {
        // We need owned errors — use into_output_errors so we can inspect them.
        use crate::lexer::{Token, lex_src};
        use chumsky::input::Stream;
        use chumsky::prelude::*;

        let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        });
        let stream = Stream::from_iter(lexer)
            .map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

        let (_, errs) = script().parse(stream).into_output_errors();
        let mut buf = Vec::new();
        // Collect owned errors so they can be rendered.
        for e in &errs {
            render_parse_errors(std::slice::from_ref(e), src, "test.urd", &mut buf).unwrap();
        }
        let text = String::from_utf8_lossy(&buf).into_owned();
        text.lines().map(str::to_owned).collect()
    }

    fn strip_ansi(s: &str) -> String {
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\x1b' {
                for ch in chars.by_ref() {
                    if ch.is_ascii_alphabetic() {
                        break;
                    }
                }
            } else {
                out.push(c);
            }
        }
        out
    }

    #[test]
    fn format_token_newline_is_human_readable() {
        assert_eq!(format_token(&Token::Newline), "newline");
    }

    #[test]
    fn format_token_ident_path_joined_with_dot() {
        let tok = Token::IdentPath(vec!["my_mod".to_owned(), "Color".to_owned()]);
        assert_eq!(format_token(&tok), "identifier 'my_mod.Color'");
    }

    #[test]
    fn format_token_int_lit() {
        assert_eq!(format_token(&Token::IntLit(42)), "integer '42'");
    }

    #[test]
    fn format_token_keywords() {
        assert_eq!(format_token(&Token::Return), "'return'");
        assert_eq!(format_token(&Token::Jump), "'jump'");
        assert_eq!(format_token(&Token::Let), "'let'");
        assert_eq!(format_token(&Token::Label), "'label'");
    }

    #[test]
    fn format_expected_empty_gives_empty_string() {
        assert_eq!(format_expected(&[]), "");
    }

    #[test]
    fn format_expected_single_item() {
        let pats = vec![RichPattern::Label(std::borrow::Cow::Borrowed("expression"))];
        assert_eq!(format_expected(&pats), "expected expression");
    }

    #[test]
    fn format_expected_two_items() {
        let pats = vec![
            RichPattern::Token(chumsky::util::MaybeRef::Val(Token::Comma)),
            RichPattern::EndOfInput,
        ];
        let result = format_expected(&pats);
        assert!(
            result.contains("or"),
            "two-item expected list should use 'or': {result}"
        );
    }

    #[test]
    fn format_expected_deduplicates() {
        let pats = vec![
            RichPattern::Token(chumsky::util::MaybeRef::Val(Token::Comma)),
            RichPattern::Token(chumsky::util::MaybeRef::Val(Token::Comma)),
        ];
        let result = format_expected(&pats);
        // Should only appear once despite being listed twice.
        assert_eq!(result, "expected ','");
    }

    #[test]
    fn render_parse_errors_produces_nonempty_output_for_broken_source() {
        // A bare `=` at the top level is not a valid statement.
        let lines = parse_errors("= 5\n");
        let all = lines.join("\n");
        assert!(
            !all.is_empty(),
            "expected non-empty output for broken source"
        );
    }

    #[test]
    fn render_parse_errors_output_is_valid_utf8() {
        let mut buf: Vec<u8> = Vec::new();

        use crate::lexer::{Token, lex_src};
        use chumsky::input::Stream;
        use chumsky::prelude::*;

        let src = "= 5\n";
        let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        });
        let stream = Stream::from_iter(lexer)
            .map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

        let (_, errs) = script().parse(stream).into_output_errors();
        render_parse_errors(&errs, src, "test.urd", &mut buf).unwrap();

        assert!(
            std::str::from_utf8(&buf).is_ok(),
            "output must be valid UTF-8"
        );
    }

    #[test]
    fn render_parse_errors_mentions_found_token() {
        // A `}` with nothing preceding it is unexpected at top level.
        let lines = parse_errors("}\n");
        let all = strip_ansi(&lines.join("\n"));
        assert!(
            all.contains("'}'") || all.contains("unexpected"),
            "output should mention the unexpected token or 'unexpected': {all}"
        );
    }

    #[test]
    fn render_parse_errors_eof_caret_is_clamped_to_source_bounds() {
        // Missing closing brace forces an EOF parse error. Regression check:
        // rendering must not create an out-of-bounds caret span.
        use crate::lexer::{Token, lex_src};
        use chumsky::input::Stream;
        use chumsky::prelude::*;

        let src = "label start {\n";
        let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        });
        let stream = Stream::from_iter(lexer)
            .map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

        let (_, errs) = script().parse(stream).into_output_errors();
        assert!(
            !errs.is_empty(),
            "expected parse errors for unterminated label block"
        );

        let mut buf: Vec<u8> = Vec::new();
        let result = render_parse_errors(&errs, src, "test.urd", &mut buf);
        assert!(
            result.is_ok(),
            "rendering EOF error should not fail: {result:?}"
        );
        assert!(
            std::str::from_utf8(&buf).is_ok(),
            "output must be valid UTF-8"
        );
    }

    #[test]
    fn render_empty_error_list_writes_nothing() {
        let mut buf: Vec<u8> = Vec::new();
        let errs: Vec<Rich<'_, Token, SimpleSpan>> = vec![];
        render_parse_errors(&errs, "return\n", "test.urd", &mut buf).unwrap();
        assert!(buf.is_empty(), "empty error list must produce no output");
    }

    #[test]
    fn valid_source_produces_no_errors() {
        // A clean script must not trigger the renderer at all.
        let result = parse_test!(script(), "return\n");
        assert!(result.is_ok(), "clean source should parse without errors");
    }
}
