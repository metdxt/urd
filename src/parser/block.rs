//! # Block Parsing Module
//!
//! This module handles parsing of code blocks and statements.
//! Blocks are collections of statements enclosed in curly braces.

use chumsky::prelude::*;

use crate::{
    lexer::Token,
    parser::{
        ast::Ast,
        expr::{comma_separated_exprs, declaration, expr},
    },
    runtime::value::RuntimeValue,
};

use super::aliases::{UrdInput, UrdParser};

/// Parser for a single statement.
pub fn statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    declaration()
        .or(if_statement())
        .or(labeled_block())
        .or(dialogue())
        .or(code_block())
}

/// Parser for dialogue lines
pub fn dialogue<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    let content = select! {
        Token::StrLit(s) => Ast::value(RuntimeValue::Str(s)),
    }
    .or(expr()
        .separated_by(
            just(Token::Comma)
                .ignored()
                .or(just(Token::Newline).repeated().at_least(1).ignored()),
        )
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
        .map(Ast::expr_list));

    comma_separated_exprs()
        .then_ignore(just(Token::Colon))
        .then(content)
        .map(|(speakers, content)| Ast::dialogue(speakers, content))
        .boxed()
}

/// Parser for if/elif/else statement
pub fn if_statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    if_parser(code_block())
}

/// Parser for labeled block
pub fn labeled_block<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    labeled_block_parser(code_block())
}

fn labeled_block_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let ident = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
    }
    .labelled("identifier");

    just(Token::Label)
        .ignore_then(ident)
        .then(block)
        .map(|(label, body)| Ast::labeled_block(label, body))
}

fn if_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let condition = expr();

    let else_block = just(Token::Else).ignore_then(block.clone());

    let elif = just(Token::Elif)
        .ignore_then(condition.clone())
        .then(block.clone());

    just(Token::If)
        .ignore_then(condition)
        .then(block)
        .then(elif.repeated().collect::<Vec<_>>())
        .then(else_block.or_not())
        .map(|(((cond, body), elifs), else_b)| {
            let else_part = elifs
                .into_iter()
                .rfold(else_b, |acc, (c, b)| Some(Ast::if_stmt(c, b, acc)));
            Ast::if_stmt(cond, body, else_part)
        })
}

/// Parser for a code block delimited by curly braces.
/// Contains a list of statements.
pub fn code_block<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    recursive(|block| {
        let stmt = declaration()
            .or(if_parser(block.clone()))
            .or(labeled_block_parser(block.clone()))
            .or(dialogue())
            .or(block);

        let separator = just(Token::Newline).or(just(Token::Semicolon));

        stmt.separated_by(separator.repeated().at_least(1))
            .allow_leading()
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
            .map(Ast::block)
    })
    .boxed()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_test, parser::ast::DeclKind, runtime::value::RuntimeValue};

    #[test]
    fn test_empty_block() {
        let src = "{}";
        let result = parse_test!(code_block(), src);
        assert_eq!(result, Ok(Ast::block(vec![])));
    }

    #[test]
    fn test_block_with_declarations() {
        let src = "{
            let x = 1
            const y = 2
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                ),
                Ast::decl(
                    DeclKind::Constant,
                    Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )
            ]))
        );
    }

    #[test]
    fn test_block_invalid_content() {
        let src = "{
            let x = 1
            1 + 1
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_err());
    }

    #[test]
    fn test_block_missing_closing_brace() {
        let src = "{
            let x = 1
        ";
        let result = parse_test!(code_block(), src);
        assert!(result.is_err());
    }

    #[test]
    fn test_block_missing_opening_brace() {
        let src = "
            let x = 1
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_err());
    }

    #[test]
    fn test_nested_block() {
        let src = "{
            let x = 1
            {
                let y = 2
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                ),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )])
            ]))
        );
    }

    #[test]
    fn test_block_semicolon_separator() {
        let src = "{
            let x = 1; let y = 2
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                ),
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )
            ]))
        );
    }

    #[test]
    fn test_if_statement() {
        let src = "{
            if true {
                let x = 1
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::if_stmt(
                Ast::value(RuntimeValue::Bool(true)),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                )]),
                None
            )]))
        );
    }

    #[test]
    fn test_if_else_statement() {
        let src = "{
            if true {
                let x = 1
            } else {
                let x = 2
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::if_stmt(
                Ast::value(RuntimeValue::Bool(true)),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                )]),
                Some(Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )]))
            )]))
        );
    }

    #[test]
    fn test_if_elif_statement() {
        let src = "{
            if true {
                let x = 1
            } elif false {
                let x = 2
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::if_stmt(
                Ast::value(RuntimeValue::Bool(true)),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                )]),
                Some(Ast::if_stmt(
                    Ast::value(RuntimeValue::Bool(false)),
                    Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                        Ast::value(RuntimeValue::Int(2))
                    )]),
                    None
                ))
            )]))
        );
    }

    #[test]
    fn test_if_elif_else_statement() {
        let src = "{
            if true {
                let x = 1
            } elif false {
                let x = 2
            } else {
                let x = 3
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::if_stmt(
                Ast::value(RuntimeValue::Bool(true)),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                )]),
                Some(Ast::if_stmt(
                    Ast::value(RuntimeValue::Bool(false)),
                    Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                        Ast::value(RuntimeValue::Int(2))
                    )]),
                    Some(Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                        Ast::value(RuntimeValue::Int(3))
                    )]))
                ))
            )]))
        );
    }

    #[test]
    fn test_labeled_block() {
        let src = "{
            label my_label {
                let x = 1
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::labeled_block(
                "my_label".to_string(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                )])
            )]))
        );
    }

    #[test]
    fn test_nested_labeled_blocks() {
        let src = "{
            label outer {
                label inner {
                    let x = 1
                }
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::labeled_block(
                "outer".to_string(),
                Ast::block(vec![Ast::labeled_block(
                    "inner".to_string(),
                    Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                        Ast::value(RuntimeValue::Int(1))
                    )])
                )])
            )]))
        );
    }

    #[test]
    fn test_dialogue_single_line() {
        let src = "{ Alice: \"Hello!\" }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_dialogue_monologue() {
        let src = "{ Alice: { \"Line 1\", \"Line 2\" } }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_dialogue_monologue_with_newlines() {
        let src = "{
            Alice: {
                \"Line 1\"
                \"Line 2\"
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_dialogue_multiple_speakers() {
        let src = "{ Alice, Bob: \"Hello both!\" }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }
}
