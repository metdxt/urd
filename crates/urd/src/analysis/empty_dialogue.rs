//! # Empty Dialogue Analysis Pass
//!
//! Checks that every `dialogue` node actually contains spoken content.
//!
//! [`AnalysisError::EmptyDialogue`] (warning) is emitted when:
//!
//! - The content is a `Value(Str(s))` where `s` is empty (no parts) or every
//!   part is a whitespace-only `Literal`.
//! - The content is a `Block([])` (a block with no statements at all).
//! - The content is a `Block` whose every child is `Value(Str(s))` with the
//!   above empty-or-whitespace condition.
//!
//! Strings that contain `EscapedChar` or `Interpolation` segments are never
//! considered empty — an interpolation like `"{name}"` is real content even if
//! it could theoretically resolve to the empty string at runtime.

use crate::analysis::AnalysisError;
use crate::lexer::strings::{ParsedString, StringPart};
use crate::parser::ast::{Ast, AstContent, walk_ast};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the empty-dialogue check over `ast` and return any diagnostics found.
///
/// Walks the entire AST in DFS order and emits one
/// [`AnalysisError::EmptyDialogue`] for every [`AstContent::Dialogue`] node
/// whose content is empty or whitespace-only as defined in the module docs.
pub fn check(ast: &Ast) -> Vec<AnalysisError> {
    let mut errors = Vec::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::Dialogue { speakers, content } = node.content()
            && is_empty_content(content)
        {
            let speaker = extract_speaker(speakers);
            errors.push(AnalysisError::EmptyDialogue {
                speaker,
                span: node.span(),
            });
        }
    });

    errors
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Extract a human-readable speaker name from the `speakers` sub-tree.
///
/// In parsed source this is an `ExprList([Value(IdentPath([name, ...]))])`.
/// In directly-constructed AST (tests) it may also be a bare
/// `Value(IdentPath([name, ...]))`.  The first path segment of the first
/// speaker is returned.  Falls back to `"(unknown)"` for anything else.
fn extract_speaker(speakers: &Ast) -> String {
    match speakers.content() {
        // Parsed form: ExprList of IdentPath values.
        AstContent::ExprList(items) => {
            if let Some(first) = items.first()
                && let AstContent::Value(RuntimeValue::IdentPath(path)) = first.content()
                && let Some(name) = path.first()
            {
                return name.clone();
            }
            "(unknown)".to_owned()
        }
        // Direct Value(IdentPath) — common in hand-built test ASTs.
        AstContent::Value(RuntimeValue::IdentPath(path)) => path
            .first()
            .cloned()
            .unwrap_or_else(|| "(unknown)".to_owned()),
        _ => "(unknown)".to_owned(),
    }
}

/// Returns `true` if `content` carries no meaningful spoken text.
///
/// Meaningful content is anything other than:
/// - A `Value(Str)` that is empty or has only whitespace `Literal` parts.
/// - An empty `Block`.
/// - A `Block` whose every child is an all-whitespace `Value(Str)`.
fn is_empty_content(content: &Ast) -> bool {
    match content.content() {
        AstContent::Value(RuntimeValue::Str(s)) => is_empty_string(s),

        AstContent::Block(stmts) => {
            if stmts.is_empty() {
                return true;
            }
            // All statements must be whitespace-only strings for the whole
            // block to be considered empty.  A non-string statement (e.g. a
            // call) is treated as content.
            stmts.iter().all(|stmt| {
                if let AstContent::Value(RuntimeValue::Str(s)) = stmt.content() {
                    is_empty_string(s)
                } else {
                    false
                }
            })
        }

        _ => false,
    }
}

/// Returns `true` if `s` contains no non-whitespace content.
///
/// Rules:
/// - Zero parts → empty.
/// - Any `EscapedChar` or `Interpolation` part → NOT empty (real content).
/// - `ExitString` parts are bookkeeping tokens; they don't count as content.
/// - A `Literal` part is empty iff its text is whitespace-only.
fn is_empty_string(s: &ParsedString) -> bool {
    if s.parts().is_empty() {
        return true;
    }
    s.parts().iter().all(|part| match part {
        StringPart::Literal(text) => text.trim().is_empty(),
        // Escaped chars and interpolations are real content.
        StringPart::EscapedChar(_) | StringPart::Interpolation(_) => false,
        // ExitString is a lexer bookkeeping token; no display content.
        StringPart::ExitString => true,
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;
    use crate::lexer::strings::{Interpolation, ParsedString, StringPart};
    use crate::parser::ast::Ast;
    use crate::runtime::value::RuntimeValue;

    // ── AST builder helpers ──────────────────────────────────────────────────

    fn make_speaker(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
    }

    fn make_dialogue_str(speaker: &str, text: &str) -> Ast {
        let s = make_speaker(speaker);
        let content = Ast::value(RuntimeValue::Str(ParsedString::new_plain(text)));
        Ast::dialogue(s, content)
    }

    fn make_dialogue_parsed_string(speaker: &str, s: ParsedString) -> Ast {
        let spk = make_speaker(speaker);
        let content = Ast::value(RuntimeValue::Str(s));
        Ast::dialogue(spk, content)
    }

    fn make_dialogue_block(speaker: &str, lines: Vec<&str>) -> Ast {
        let spk = make_speaker(speaker);
        let stmts: Vec<Ast> = lines
            .into_iter()
            .map(|t| Ast::value(RuntimeValue::Str(ParsedString::new_plain(t))))
            .collect();
        let content = Ast::block(stmts);
        Ast::dialogue(spk, content)
    }

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    // ── is_empty_string unit tests ───────────────────────────────────────────

    #[test]
    fn empty_parsed_string_is_empty() {
        assert!(is_empty_string(&ParsedString::new()));
    }

    #[test]
    fn blank_literal_is_empty() {
        assert!(is_empty_string(&ParsedString::new_plain("")));
    }

    #[test]
    fn whitespace_literal_is_empty() {
        assert!(is_empty_string(&ParsedString::new_plain("   ")));
    }

    #[test]
    fn whitespace_tab_literal_is_empty() {
        assert!(is_empty_string(&ParsedString::new_plain("\t\n  ")));
    }

    #[test]
    fn nonempty_literal_is_not_empty() {
        assert!(!is_empty_string(&ParsedString::new_plain("hello")));
    }

    #[test]
    fn escaped_char_makes_string_nonempty() {
        let s = ParsedString::new_from_parts(vec![StringPart::EscapedChar("\n".to_owned())]);
        assert!(!is_empty_string(&s));
    }

    #[test]
    fn interpolation_makes_string_nonempty() {
        let s = ParsedString::new_from_parts(vec![StringPart::Interpolation(Interpolation {
            path: "name".to_owned(),
            format: None,
        })]);
        assert!(!is_empty_string(&s));
    }

    #[test]
    fn mixed_whitespace_literal_and_escaped_char_is_nonempty() {
        let s = ParsedString::new_from_parts(vec![
            StringPart::Literal("  ".to_owned()),
            StringPart::EscapedChar("\t".to_owned()),
        ]);
        // EscapedChar counts as real content.
        assert!(!is_empty_string(&s));
    }

    // ── No-error cases (valid dialogues) ────────────────────────────────────

    #[test]
    fn dialogue_with_text_is_clean() {
        let ast = parse(
            r#"
label start {
    Narrator: "Hello, world!"
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn dialogue_with_interpolation_is_clean() {
        let ast = parse(
            r#"
label start {
    Narrator: "Your name is {player.name}."
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn no_dialogue_nodes_is_clean() {
        let ast = parse(
            r#"
label start {
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn block_dialogue_with_text_lines_is_clean() {
        // Multi-statement block where every child is a non-empty string.
        let node = make_dialogue_block("Narrator", vec!["Line one.", "Line two."]);
        let errors = check(&node);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── EmptyDialogue — single Value(Str) form ───────────────────────────────

    #[test]
    fn empty_string_content_emits_error() {
        let ast = parse(
            r#"
label start {
    Narrator: ""
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        assert!(
            matches!(&errors[0], AnalysisError::EmptyDialogue { .. }),
            "expected EmptyDialogue, got: {:?}",
            errors[0]
        );
    }

    #[test]
    fn whitespace_only_string_content_emits_error() {
        let ast = parse(
            r#"
label start {
    Narrator: "   "
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1, "expected 1 error, got: {errors:?}");
        assert!(
            matches!(&errors[0], AnalysisError::EmptyDialogue { .. }),
            "expected EmptyDialogue, got: {:?}",
            errors[0]
        );
    }

    #[test]
    fn empty_dialogue_ast_direct_blank_string() {
        let node = make_dialogue_str("Narrator", "");
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyDialogue { .. }));
    }

    #[test]
    fn empty_dialogue_ast_direct_whitespace_string() {
        let node = make_dialogue_str("Hero", "  \t  ");
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::EmptyDialogue { speaker, .. } => {
                assert_eq!(speaker, "Hero");
            }
            other => panic!("expected EmptyDialogue, got: {other:?}"),
        }
    }

    #[test]
    fn empty_parsed_string_no_parts_emits_error() {
        // ParsedString::new() has zero parts.
        let node = make_dialogue_parsed_string("Narrator", ParsedString::new());
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyDialogue { .. }));
    }

    // ── EmptyDialogue — Block form ───────────────────────────────────────────

    #[test]
    fn empty_block_content_emits_error() {
        let spk = make_speaker("Narrator");
        let content = Ast::block(vec![]);
        let node = Ast::dialogue(spk, content);
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyDialogue { .. }));
    }

    #[test]
    fn block_with_only_blank_strings_emits_error() {
        let node = make_dialogue_block("Narrator", vec!["", "   ", "\t"]);
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyDialogue { .. }));
    }

    #[test]
    fn block_with_one_blank_and_one_real_line_is_clean() {
        // One real line means the block has content — no error.
        let node = make_dialogue_block("Narrator", vec!["", "Hello there."]);
        let errors = check(&node);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn block_with_non_string_statement_is_clean() {
        // A call inside the dialogue block is considered real content.
        let spk = make_speaker("Narrator");
        let call_func = Ast::value(RuntimeValue::IdentPath(vec!["some_fn".to_owned()]));
        let call = Ast::call(call_func, Ast::expr_list(vec![]));
        let content = Ast::block(vec![call]);
        let node = Ast::dialogue(spk, content);
        let errors = check(&node);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── Speaker name extraction ──────────────────────────────────────────────

    #[test]
    fn speaker_name_is_captured_from_parsed_source() {
        let ast = parse(
            r#"
label start {
    Hero: ""
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::EmptyDialogue { speaker, .. } => {
                assert_eq!(speaker, "Hero");
            }
            other => panic!("expected EmptyDialogue, got: {other:?}"),
        }
    }

    #[test]
    fn speaker_name_is_captured_from_ast_ident_path() {
        let node = make_dialogue_str("Villain", "");
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::EmptyDialogue { speaker, .. } => {
                assert_eq!(speaker, "Villain");
            }
            other => panic!("expected EmptyDialogue, got: {other:?}"),
        }
    }

    #[test]
    fn speaker_from_expr_list_ast() {
        // Simulate parsed form: ExprList([Value(IdentPath(["Alice"]))])
        let ident = Ast::value(RuntimeValue::IdentPath(vec!["Alice".to_owned()]));
        let speakers = Ast::expr_list(vec![ident]);
        let content = Ast::value(RuntimeValue::Str(ParsedString::new_plain("")));
        let node = Ast::dialogue(speakers, content);
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::EmptyDialogue { speaker, .. } => {
                assert_eq!(speaker, "Alice");
            }
            other => panic!("expected EmptyDialogue, got: {other:?}"),
        }
    }

    #[test]
    fn unknown_speaker_fallback_for_unexpected_shape() {
        // Speakers node that is neither ExprList nor IdentPath.
        let speakers = Ast::value(RuntimeValue::Int(42));
        let content = Ast::value(RuntimeValue::Str(ParsedString::new_plain("")));
        let node = Ast::dialogue(speakers, content);
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            AnalysisError::EmptyDialogue { speaker, .. } => {
                assert_eq!(speaker, "(unknown)");
            }
            other => panic!("expected EmptyDialogue, got: {other:?}"),
        }
    }

    // ── Multiple errors ──────────────────────────────────────────────────────

    #[test]
    fn multiple_empty_dialogues_all_reported() {
        let ast = parse(
            r#"
label start {
    Narrator: ""
    Hero: "   "
    end!()
}
"#,
        );
        let errors = check(&ast);
        let empty_count = errors
            .iter()
            .filter(|e| matches!(e, AnalysisError::EmptyDialogue { .. }))
            .count();
        assert_eq!(
            empty_count, 2,
            "expected 2 EmptyDialogue errors, got: {errors:?}"
        );
    }

    #[test]
    fn one_empty_and_one_real_dialogue_reports_only_empty() {
        let ast = parse(
            r#"
label start {
    Narrator: ""
    Narrator: "This one is fine."
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyDialogue { .. }));
    }

    // ── Nested / deep structures ─────────────────────────────────────────────

    #[test]
    fn empty_dialogue_inside_if_block_is_found() {
        let ast = parse(
            r#"
label start {
    if true {
        Narrator: ""
    } else {
        end!()
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyDialogue { .. }));
    }

    #[test]
    fn empty_dialogue_inside_menu_option_is_found() {
        let ast = parse(
            r#"
label start {
    menu {
        "Choice A" {
            Narrator: ""
            end!()
        }
        "Choice B" { end!() }
    }
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], AnalysisError::EmptyDialogue { .. }));
    }

    // ── Span preservation ────────────────────────────────────────────────────

    #[test]
    fn empty_dialogue_span_is_nonzero_for_parsed_source() {
        let ast = parse(
            r#"
label start {
    Narrator: ""
    end!()
}
"#,
        );
        let errors = check(&ast);
        assert_eq!(errors.len(), 1);
        if let AnalysisError::EmptyDialogue { span, .. } = &errors[0] {
            assert!(
                span.start != 0 || span.end != 0,
                "expected a real source span, got zero span"
            );
        }
    }

    #[test]
    fn empty_dialogue_zero_span_for_ast_constructed_node() {
        let node = make_dialogue_str("Narrator", "");
        let errors = check(&node);
        assert_eq!(errors.len(), 1);
        if let AnalysisError::EmptyDialogue { span, .. } = &errors[0] {
            assert_eq!(span.start, 0, "expected zero span for AST-built node");
            assert_eq!(span.end, 0, "expected zero span for AST-built node");
        }
    }

    // ── Display / message smoke tests ────────────────────────────────────────

    #[test]
    fn empty_dialogue_error_message_contains_speaker_name() {
        use chumsky::span::{SimpleSpan, Span};
        let err = AnalysisError::EmptyDialogue {
            speaker: "Frida".to_owned(),
            span: SimpleSpan::new((), 0..0),
        };
        let msg = err.to_string();
        assert!(
            msg.contains("Frida"),
            "message should contain speaker name, got: {msg}"
        );
    }

    #[test]
    fn empty_dialogue_error_message_indicates_empty_content() {
        use chumsky::span::{SimpleSpan, Span};
        let err = AnalysisError::EmptyDialogue {
            speaker: "X".to_owned(),
            span: SimpleSpan::new((), 0..0),
        };
        let msg = err.to_string();
        assert!(
            msg.contains("empty") || msg.contains("blank") || msg.contains("nothing"),
            "message should indicate empty content, got: {msg}"
        );
    }
}
