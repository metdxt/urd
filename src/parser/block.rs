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
    statement_inner(code_block())
}

/// Parser for return statement (return expr)
pub fn return_statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    just(Token::Return)
        .ignore_then(expr().or_not())
        .map(Ast::return_stmt)
        .boxed()
}

/// Helper for statement parsing, allowing recursion injection.
fn statement_inner<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    assignment()
        .or(declaration())
        .or(if_parser(block.clone()))
        .or(labeled_block_parser(block.clone()))
        .or(menu_parser(block.clone()))
        .or(return_statement())
        .or(dialogue())
        .or(block)
}

/// Parser for assignment statements (ident = expr)
/// This allows mutating previously declared variables.
pub fn assignment<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    }
    .then_ignore(just(Token::Assign))
    .then(expr())
    .map(|(ident, value)| Ast::assign_op(ident, value))
    .boxed()
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
        .delimited_by(just(Token::LessThan), just(Token::GreaterThan))
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
        let separator = just(Token::Newline).or(just(Token::Semicolon));

        statement_inner(block)
            .separated_by(separator.repeated().at_least(1))
            .allow_leading()
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
            .map(Ast::block)
    })
    .boxed()
}

/// Parser for menu statement
pub fn menu<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    menu_parser(code_block())
}

fn menu_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let option_label = select! {
        Token::StrLit(s) => s,
    };

    let option = option_label.then(block).map(|(label, code_block)| {
        // Use Display trait to convert ParsedString to String
        (format!("{}", label), code_block)
    });

    let options = option
        .separated_by(just(Token::Newline).repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Menu)
        .ignore_then(options.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
        .map(Ast::menu)
        .boxed()
}

/// Parser for a bare script body (list of statements without braces).
pub fn script<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    let separator = just(Token::Newline).or(just(Token::Semicolon));

    statement()
        .separated_by(separator.repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(Ast::block)
        .boxed()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_test, parser::ast::DeclKind, runtime::value::RuntimeValue};

    #[test]
    fn test_script_bare() {
        let src = "
            let x = 1
            <Alice>: \"Hello\"
        ";
        let result = parse_test!(script(), src);
        assert_eq!(
            result,
            Ok(Ast::block(vec![
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                ),
                Ast::dialogue(
                    Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                        "Alice".to_string()
                    ]))]),
                    Ast::value(RuntimeValue::Str(
                        crate::lexer::strings::ParsedString::new_plain("Hello")
                    ))
                )
            ]))
        );
    }

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
        let src = "{ <Alice>: \"Hello!\" }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_dialogue_monologue() {
        let src = "{ <Alice>: { \"Line 1\", \"Line 2\" } }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_dialogue_monologue_with_newlines() {
        let src = "{
            <Alice>: {
                \"Line 1\"
                \"Line 2\"
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_dialogue_multiple_speakers() {
        let src = "{ <Alice, Bob>: \"Hello both!\" }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_empty() {
        let src = "{ menu { } }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_with_escaped_characters() {
        let src = "{
            menu {
                \"Option \\\"quoted\\\"\" {
                    let x = 1
                }
                \"Tab\\there\" {
                    let y = 2
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_complex_expressions() {
        let src = "{
            menu {
                \"Add\" {
                    let result = 5 + 3
                }
                \"Multiply\" {
                    let result = 5 * 3
                }
                \"Simple action\" {
                    let done = true
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_multiple_menus() {
        let src = "{
            menu {
                \"Choice 1\" {
                    let x = 1
                }
                \"Choice 2\" {
                    let x = 2
                }
            }
            menu {
                \"Next\" {
                    let y = 3
                }
                \"Back\" {
                    let y = 4
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_with_dialogue() {
        let src = "{
            menu {
                \"Talk to Alice\" {
                    <Alice>: \"Hello there!\"
                }
                \"Talk to Bob\" {
                    <Bob>: \"Hi!\"
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_with_if_else() {
        let src = "{
            menu {
                \"Check condition\" {
                    if true {
                        let x = 1
                    } else {
                        let x = 2
                    }
                }
                \"Nested menu\" {
                    menu {
                        \"Sub-option 1\" {
                            let y = 3
                        }
                    }
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_ast_structure() {
        let src = "{
            menu {
                \"Option 1\" {
                    let x = 1
                }
                \"Option 2\" {
                    let y = 2
                }
            }
        }";
        let result = parse_test!(code_block(), src);

        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::menu(vec![
                (
                    "Option 1".to_string(),
                    Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                        Ast::value(RuntimeValue::Int(1))
                    )])
                ),
                (
                    "Option 2".to_string(),
                    Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                        Ast::value(RuntimeValue::Int(2))
                    )])
                )
            ])]))
        );
    }

    #[test]
    fn test_menu_basic() {
        let src = "{
            menu {
                \"Option 1\" {
                    let x = 1
                }
                \"Option 2\" {
                    let y = 2
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_real_world_example() {
        let src = "{
            menu {
                \"Start Game\" {
                    let health = 100
                    <Narrator>: \"Your adventure begins!\"
                }
                \"Load Game\" {
                    <Narrator>: \"No save files found.\"
                }
                \"Exit\" {
                    <Narrator>: \"Goodbye!\"
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_single_option() {
        let src = "{ menu { \"Only option\" { let x = 1 } } }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_menu_nested_blocks() {
        let src = "{
            menu {
                \"Option 1\" {
                    {
                        let x = 1
                    }
                }
                \"Option 2\" {
                    if true {
                        let y = 2
                    }
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_assignment_basic() {
        let src = "{ x = 1 }";
        let result = parse_test!(code_block(), src);
        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::assign_op(
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            )]))
        );
    }

    #[test]
    fn test_assignment_with_expression() {
        let src = "{ x = 1 + 2 }";
        let result = parse_test!(code_block(), src);
        assert_eq!(
            result,
            Ok(Ast::block(vec![Ast::assign_op(
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::add_op(
                    Ast::value(RuntimeValue::Int(1)),
                    Ast::value(RuntimeValue::Int(2))
                )
            )]))
        );
    }

    #[test]
    fn test_assignment_after_declaration() {
        let src = "{
            let x = 1
            x = 2
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
                Ast::assign_op(
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )
            ]))
        );
    }

    #[test]
    fn test_multiple_assignments() {
        let src = "{
            let x = 0
            x = 1
            x = 2
            x = 3
        }";
        let result = parse_test!(code_block(), src);
        assert_eq!(
            result,
            Ok(Ast::block(vec![
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(0))
                ),
                Ast::assign_op(
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                ),
                Ast::assign_op(
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                ),
                Ast::assign_op(
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(3))
                )
            ]))
        );
    }

    #[test]
    fn test_assignment_in_nested_block() {
        let src = "{
            let x = 1
            {
                x = 2
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_assignment_in_if_statement() {
        let src = "{
            let x = 1
            if true {
                x = 2
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_assignment_in_menu_option() {
        let src = "{
            let x = 1
            menu {
                \"Option 1\" {
                    x = 2
                }
                \"Option 2\" {
                    x = 3
                }
            }
        }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_assignment_with_complex_expression() {
        let src = "{ x = (1 + 2) * 3 }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_assignment_with_variable() {
        let src = "{
            let y = 10
            let x = 5
            x = y
        }";
        let result = parse_test!(code_block(), src);
        assert_eq!(
            result,
            Ok(Ast::block(vec![
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                    Ast::value(RuntimeValue::Int(10))
                ),
                Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(5))
                ),
                Ast::assign_op(
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()]))
                )
            ]))
        );
    }

    #[test]
    fn test_assignment_in_script() {
        let src = "
            let x = 1
            x = 2
        ";
        let result = parse_test!(script(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_assignment_with_semicolon() {
        let src = "{ x = 1; y = 2 }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_basic() {
        let src = "return 42";
        let result = parse_test!(return_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_with_expression() {
        let src = "return x + 5";
        let result = parse_test!(return_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_without_value() {
        let src = "return";
        let result = parse_test!(return_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_in_block() {
        let src = "{ return 100 }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_in_if_statement() {
        let src = "if true { return 1 } else { return 2 }";
        let result = parse_test!(if_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_in_script() {
        let src = "
            let x = 10
            return x
        ";
        let result = parse_test!(script(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_with_function_call() {
        let src = "return foo(1, 2)";
        let result = parse_test!(return_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_complex_expression() {
        let src = "return (x + y) * 2";
        let result = parse_test!(return_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_return_in_menu_option() {
        let src = "menu { \"Option 1\" { return 1 } }";
        let result = parse_test!(menu(), src);
        assert!(result.is_ok());
    }
}
