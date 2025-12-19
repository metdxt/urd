//! # Block Parsing Module
//!
//! This module handles parsing of code blocks and statements.
//! Blocks are collections of statements enclosed in curly braces.

use chumsky::prelude::*;

use crate::{
    lexer::Token,
    parser::{
        ast::Ast,
        expr::{declaration, expr},
    },
};

use super::aliases::{UrdInput, UrdParser};

/// Parser for a single statement.
pub fn statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    declaration().or(if_statement()).or(code_block())
}

/// Parser for if/elif/else statement
pub fn if_statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    if_parser(code_block())
}

fn if_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + Clone + 'tok,
) -> impl UrdParser<'tok, I> {
    let condition = expr();

    let else_block = just(Token::Else).ignore_then(block.clone()).map(Some);

    let elif_chain = recursive(|chain| {
        just(Token::Elif)
            .ignore_then(condition.clone())
            .then(block.clone())
            .then(chain.or(else_block.clone()).or(empty().to(None)))
            .map(|((cond, body), else_b)| Some(Ast::if_stmt(cond, body, else_b)))
    });

    just(Token::If)
        .ignore_then(condition)
        .then(block)
        .then(elif_chain.or(else_block).or(empty().to(None)))
        .map(|((cond, body), else_b)| Ast::if_stmt(cond, body, else_b))
}

/// Parser for a code block delimited by curly braces.
/// Contains a list of statements.
pub fn code_block<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    recursive(|block| {
        let stmt = declaration().or(if_parser(block.clone())).or(block);

        let separator = just(Token::Newline).or(just(Token::Semicolon));

        stmt.separated_by(separator.repeated().at_least(1))
            .allow_leading()
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
            .map(Ast::block)
    })
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
}
