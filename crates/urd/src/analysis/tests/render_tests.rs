//! Integration tests for the ariadne-based [`crate::analysis::render_errors`] function.
//!
//! Unlike the other analysis tests that build AST nodes by hand (and therefore
//! produce zero spans), these tests drive the *real* lexer + parser so that
//! `Ast` nodes carry genuine byte-offset spans.  The rendered output can then
//! be inspected for source-location information.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]

use crate::analysis::{AnalysisError, analyze, render_errors};
use crate::parse_test;
use crate::parser::block::script;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Parse `src` with the `script` parser. Panics on parse error.
fn parse_script(src: &str) -> crate::parser::ast::Ast {
    parse_test!(script(), src).unwrap()
}

/// Run `render_errors` into a `Vec<u8>` and return it as a `String`.
fn render_to_string(errors: &[AnalysisError], src: &str) -> String {
    let mut buf: Vec<u8> = Vec::new();
    render_errors(errors, src, "test.urd", &mut buf).unwrap();
    // Strip ANSI escape sequences so assertions are colour-agnostic.
    strip_ansi(String::from_utf8_lossy(&buf).into_owned())
}

/// Very small ANSI-stripping helper: removes `ESC [ ... m` sequences.
fn strip_ansi(s: String) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // Consume until we see a letter that ends the escape sequence.
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

// ---------------------------------------------------------------------------
// Span population
// ---------------------------------------------------------------------------

#[test]
fn parsed_ast_nodes_have_nonzero_spans() {
    // A single `return` statement should produce an Ast with a real span.
    use chumsky::span::SimpleSpan;
    let src = "return\n";
    let ast = parse_script(src);
    // The root is a Block; the block itself spans the whole source.
    let span: SimpleSpan = ast.span();
    // start may be 0 but end must be > 0 for non-trivial source.
    assert!(
        span.end > 0,
        "expected a non-zero span end for parsed source, got {span:?}"
    );
}

// ---------------------------------------------------------------------------
// render_errors: zero-span fallback path
// ---------------------------------------------------------------------------

#[test]
fn render_zero_span_errors_falls_back_to_plain_text() {
    // Build errors the old-fashioned way (zero spans — no parser involved).
    // Flow must live inside a LabeledBlock; a bare root Block is a definitions
    // container and is never flagged as a dead end.
    use crate::parser::ast::Ast;
    use crate::runtime::value::RuntimeValue;
    let label = Ast::labeled_block(
        "scene".to_owned(),
        Ast::block(vec![Ast::value(RuntimeValue::Null)]),
    );
    let ast = Ast::block(vec![label]);
    let errors = analyze(&ast);
    // The labeled block is open → DeadEnd with zero span.
    assert!(
        !errors.is_empty(),
        "expected at least one error from an open labeled block"
    );

    let output = render_to_string(&errors, "");
    // Zero-span path must produce a plain `error: …` line, not an ariadne box.
    assert!(
        output.starts_with("error:"),
        "expected plain-text fallback starting with 'error:', got:\n{output}"
    );
    assert!(
        output.contains("Dead end") || output.contains("dead end") || output.contains("terminator"),
        "expected dead-end message in output, got:\n{output}"
    );
}

#[test]
fn render_empty_error_list_writes_nothing() {
    let mut buf: Vec<u8> = Vec::new();
    render_errors(&[], "return\n", "test.urd", &mut buf).unwrap();
    assert!(
        buf.is_empty(),
        "expected empty output for an empty error list"
    );
}

// ---------------------------------------------------------------------------
// TypeMismatch — real parser span
// ---------------------------------------------------------------------------

#[test]
fn render_type_mismatch_contains_variable_name() {
    // `my_var` is declared as `int` but assigned a string literal.
    let src = "let my_var: int = \"oops\"\nreturn\n";
    let ast = parse_script(src);
    let errors = analyze(&ast);

    let type_errors: Vec<AnalysisError> = errors
        .into_iter()
        .filter(|e| matches!(e, AnalysisError::TypeMismatch { .. }))
        .collect();

    assert!(
        !type_errors.is_empty(),
        "expected a TypeMismatch error; got: {type_errors:?}"
    );

    let output = render_to_string(&type_errors, src);

    // The ariadne output must contain the variable name.
    assert!(
        output.contains("my_var"),
        "rendered output should mention the variable 'my_var':\n{output}"
    );
}

#[test]
fn render_type_mismatch_has_nonzero_span() {
    let src = "let bad: bool = 42\nreturn\n";
    let ast = parse_script(src);
    let errors = analyze(&ast);

    let type_error = errors
        .iter()
        .find(|e| matches!(e, AnalysisError::TypeMismatch { .. }))
        .expect("expected a TypeMismatch error; got: {errors:?}");

    let span = type_error.span();
    assert!(
        span.end > 0,
        "TypeMismatch from parsed source should have a real span, got {span:?}"
    );
}

#[test]
fn render_type_mismatch_output_is_nonempty() {
    let src = "let x: float = true\nreturn\n";
    let ast = parse_script(src);
    let errors: Vec<_> = analyze(&ast)
        .into_iter()
        .filter(|e| matches!(e, AnalysisError::TypeMismatch { .. }))
        .collect();

    assert!(!errors.is_empty(), "expected a TypeMismatch error");

    let output = render_to_string(&errors, src);
    assert!(
        !output.is_empty(),
        "rendered output must not be empty for a real-span TypeMismatch"
    );
}

// ---------------------------------------------------------------------------
// DeadEnd — real parser span
// ---------------------------------------------------------------------------

#[test]
fn render_dead_end_has_nonzero_span_for_parsed_source() {
    // An empty labeled block has no terminator.
    let src = "label intro {\n}\n";
    let ast = parse_script(src);
    let errors = analyze(&ast);

    let dead_ends: Vec<AnalysisError> = errors
        .into_iter()
        .filter(|e| matches!(e, AnalysisError::DeadEnd { .. }))
        .collect();

    assert!(
        !dead_ends.is_empty(),
        "expected a DeadEnd error for an empty labeled block; got: {dead_ends:?}"
    );

    for err in &dead_ends {
        let span = err.span();
        assert!(
            span.end > 0,
            "DeadEnd from parsed source should have a real span, got {span:?}"
        );
    }
}

#[test]
fn render_dead_end_output_is_nonempty_for_real_span() {
    let src = "label intro {\n}\n";
    let ast = parse_script(src);
    let errors: Vec<_> = analyze(&ast)
        .into_iter()
        .filter(|e| matches!(e, AnalysisError::DeadEnd { .. }))
        .collect();

    assert!(!errors.is_empty(), "expected DeadEnd errors");

    let output = render_to_string(&errors, src);
    assert!(
        !output.is_empty(),
        "rendered output must not be empty for a real-span DeadEnd"
    );
}

// ---------------------------------------------------------------------------
// NonExhaustiveMatch — real parser span
// ---------------------------------------------------------------------------

#[test]
fn render_non_exhaustive_match_has_nonzero_span() {
    // An enum with three variants, but the match only covers two.
    let src = concat!(
        "enum Dir { North, South, East }\n",
        "let d: Dir = Dir.North\n",
        "match d {\n",
        "    Dir.North { return }\n",
        "    Dir.South { return }\n",
        "}\n",
        "return\n",
    );

    let ast = parse_script(src);
    let errors = analyze(&ast);

    let match_errors: Vec<AnalysisError> = errors
        .into_iter()
        .filter(|e| matches!(e, AnalysisError::NonExhaustiveMatch { .. }))
        .collect();

    assert!(
        !match_errors.is_empty(),
        "expected a NonExhaustiveMatch error; got: {match_errors:?}"
    );

    for err in &match_errors {
        let span = err.span();
        assert!(
            span.end > 0,
            "NonExhaustiveMatch from parsed source should have a real span, got {span:?}"
        );
    }
}

#[test]
fn render_non_exhaustive_match_mentions_missing_variant() {
    let src = concat!(
        "enum Color { Red, Green, Blue }\n",
        "let c: Color = Color.Red\n",
        "match c {\n",
        "    Color.Red   { return }\n",
        "    Color.Green { return }\n",
        "}\n",
        "return\n",
    );

    let ast = parse_script(src);
    let errors = analyze(&ast);

    let match_errors: Vec<_> = errors
        .into_iter()
        .filter(|e| matches!(e, AnalysisError::NonExhaustiveMatch { .. }))
        .collect();

    assert!(
        !match_errors.is_empty(),
        "expected NonExhaustiveMatch error"
    );

    let output = render_to_string(&match_errors, src);

    // The missing variant "Blue" must appear in the rendered output.
    assert!(
        output.contains("Blue"),
        "rendered output should mention the missing variant 'Blue':\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Multiple errors from a single source
// ---------------------------------------------------------------------------

#[test]
fn render_multiple_errors_all_appear_in_output() {
    // Two separate type mismatches in the same script.
    let src = "let a: int = \"bad\"\nlet b: bool = 99\nreturn\n";
    let ast = parse_script(src);
    let errors: Vec<_> = analyze(&ast)
        .into_iter()
        .filter(|e| matches!(e, AnalysisError::TypeMismatch { .. }))
        .collect();

    assert!(
        errors.len() >= 2,
        "expected at least 2 TypeMismatch errors; got: {errors:?}"
    );

    let output = render_to_string(&errors, src);

    // Both variable names must appear in the combined output.
    assert!(
        output.contains('a') && output.contains('b'),
        "rendered output should mention both variables:\n{output}"
    );
}

// ---------------------------------------------------------------------------
// render_errors round-trip: write to a buffer then re-parse the UTF-8
// ---------------------------------------------------------------------------

#[test]
fn render_errors_writes_valid_utf8() {
    let src = "let z: int = false\nreturn\n";
    let ast = parse_script(src);
    let errors = analyze(&ast);

    let mut buf: Vec<u8> = Vec::new();
    render_errors(&errors, src, "test.urd", &mut buf).unwrap();

    assert!(
        std::str::from_utf8(&buf).is_ok(),
        "rendered output must be valid UTF-8"
    );
}
