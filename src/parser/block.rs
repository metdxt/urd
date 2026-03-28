//! # Block Parsing Module
//!
//! This module handles parsing of code blocks and statements.
//! Blocks are collections of statements enclosed in curly braces.

use chumsky::prelude::*;

use crate::{
    lexer::Token,
    parser::{
        ast::{Ast, Decorator, MatchArm, MatchPattern},
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

/// Parser for jump statement (jump ident)
pub fn jump_statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    just(Token::Jump)
        .ignore_then(select! {
            Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
        })
        .map(Ast::jump_stmt)
        .boxed()
}

/// Parser for a single decorator: `@name` or `@name(arg1, arg2, ...)`
fn decorator_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    Decorator,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    let name = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
    }
    .labelled("decorator name");

    let args =
        comma_separated_exprs().delimited_by(just(Token::LeftParen), just(Token::RightParen));

    just(Token::At)
        .ignore_then(name)
        .then(args.or_not())
        .map(|(name, maybe_args)| match maybe_args {
            Some(args) => Decorator::new(name, args),
            None => Decorator::bare(name),
        })
        .boxed()
}

/// Parser for a sequence of decorators, each on its own line, ending with at least one newline
/// before the decorated statement.
fn decorators_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    Vec<Decorator>,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    decorator_parser()
        .separated_by(just(Token::Newline).repeated().at_least(1))
        .at_least(1)
        .allow_leading()
        .collect::<Vec<_>>()
        .then_ignore(just(Token::Newline).repeated().at_least(1))
}

/// Helper for statement parsing, allowing recursion injection.
fn statement_inner<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    // Decoratable constructs: labeled_block, dialogue, menu, bare block
    let decoratable = labeled_block_parser(block.clone())
        .or(menu_parser(block.clone()))
        .or(dialogue())
        .or(block.clone());

    let decorated = decorators_parser()
        .then(decoratable.clone())
        .map(|(decorators, node)| node.with_decorators(decorators));

    assignment()
        .or(declaration())
        .or(if_parser(block.clone()))
        .or(return_statement())
        .or(jump_statement())
        .or(enum_decl_parser())
        .or(match_parser(block))
        .or(decorated)
        .or(decoratable)
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
        .boxed()
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
        .boxed()
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

/// Parser for enum declarations: `enum Foo { A, B, C }`
pub fn enum_decl<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    enum_decl_parser()
}

/// Parser for match statements: `match expr { pattern { ... } ... }`
pub fn match_statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    match_parser(code_block())
}

fn menu_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let option_label = select! {
        Token::StrLit(s) => s,
    };

    let option = option_label.then(block).map(|(label, code_block)| {
        // Use Display trait to convert ParsedString to String
        Ast::menu_option(format!("{}", label), code_block)
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

/// Parses an enum declaration: `enum Name { Variant1, Variant2, ... }`
///
/// Variants can be separated by commas, newlines, or a mix of both.
/// Trailing separators are allowed.
fn enum_decl_parser<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    // Separator: comma or newline, at least one, trailing allowed
    let sep = just(Token::Comma)
        .or(just(Token::Newline))
        .repeated()
        .at_least(1);

    let variant = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone()
    }
    .labelled("enum variant");

    let variants = variant
        .separated_by(sep)
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>();

    let name = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone()
    }
    .labelled("enum name");

    just(Token::Enum)
        .ignore_then(name)
        .then(variants.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
        .map(|(name, variants)| Ast::enum_decl(name, variants))
        .boxed()
}

/// Parses a match statement: `match expr { pattern { ... } ... }`
///
/// Each arm is `pattern block`. Patterns are separated by newlines.
/// Supported patterns: `_` wildcard, literals (bool/int/float/str/null), and identifier paths.
fn match_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let pattern = select! { Token::Wildcard => MatchPattern::Wildcard }
        .or(select! {
            Token::Null => MatchPattern::Value(Ast::value(RuntimeValue::Null)),
            Token::BoolLit(b) => MatchPattern::Value(Ast::value(RuntimeValue::Bool(b))),
            Token::IntLit(i) => MatchPattern::Value(Ast::value(RuntimeValue::Int(i))),
            Token::FloatLit(f) => MatchPattern::Value(Ast::value(RuntimeValue::Float(f))),
            Token::StrLit(s) => MatchPattern::Value(Ast::value(RuntimeValue::Str(s))),
            Token::IdentPath(p) => MatchPattern::Value(Ast::value(RuntimeValue::IdentPath(p))),
        })
        .labelled("match pattern");

    let arm = pattern
        .then(block)
        .map(|(pattern, body)| MatchArm::new(pattern, body));

    let arms = arm
        .separated_by(just(Token::Newline).repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Match)
        .ignore_then(expr())
        .then(arms.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
        .map(|(scrutinee, arms)| Ast::match_stmt(scrutinee, arms))
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
    use crate::{
        parse_test,
        parser::ast::{AstContent, DeclKind, Decorator, MatchPattern},
        runtime::value::RuntimeValue,
    };

    // ---- Decorator tests ----

    #[test]
    fn test_decorator_bare_on_labeled_block() {
        let src = "
            @on_enter
            label my_scene { let x = 1 }
        ";
        let result = parse_test!(script(), src);
        assert!(
            result.is_ok(),
            "bare decorator on labeled block should parse: {result:?}"
        );
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        assert_eq!(stmts.len(), 1);
        let decorated = &stmts[0];
        assert_eq!(decorated.decorators().len(), 1);
        assert_eq!(decorated.decorators()[0].name(), "on_enter");
        assert_eq!(
            decorated.decorators()[0].args().content(),
            &AstContent::ExprList(vec![])
        );
    }

    #[test]
    fn test_decorator_with_args_on_labeled_block() {
        let src = "
            @event(\"start\", 42)
            label my_scene { let x = 1 }
        ";
        let result = parse_test!(script(), src);
        assert!(
            result.is_ok(),
            "decorator with args on labeled block should parse: {result:?}"
        );
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        let decorated = &stmts[0];
        assert_eq!(decorated.decorators().len(), 1);
        assert_eq!(decorated.decorators()[0].name(), "event");
        let AstContent::ExprList(args) = decorated.decorators()[0].args().content() else {
            panic!("expected ExprList for decorator args");
        };
        assert_eq!(args.len(), 2);
    }

    #[test]
    fn test_multiple_decorators_on_labeled_block() {
        let src = "
            @foo
            @bar(1)
            label my_scene { let x = 1 }
        ";
        let result = parse_test!(script(), src);
        assert!(
            result.is_ok(),
            "multiple decorators should parse: {result:?}"
        );
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        let decorated = &stmts[0];
        assert_eq!(decorated.decorators().len(), 2);
        assert_eq!(decorated.decorators()[0].name(), "foo");
        assert_eq!(decorated.decorators()[1].name(), "bar");
    }

    #[test]
    fn test_decorator_on_dialogue() {
        let src = "
            @voiced
            <Alice>: \"Hello!\"
        ";
        let result = parse_test!(script(), src);
        assert!(
            result.is_ok(),
            "decorator on dialogue should parse: {result:?}"
        );
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        let decorated = &stmts[0];
        assert_eq!(decorated.decorators().len(), 1);
        assert_eq!(decorated.decorators()[0].name(), "voiced");
    }

    #[test]
    fn test_decorator_on_menu() {
        let src = "
            @important
            menu {
                \"Yes\" { jump yes }
                \"No\" { jump no }
            }
        ";
        let result = parse_test!(script(), src);
        assert!(result.is_ok(), "decorator on menu should parse: {result:?}");
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        let decorated = &stmts[0];
        assert_eq!(decorated.decorators().len(), 1);
        assert_eq!(decorated.decorators()[0].name(), "important");
    }

    #[test]
    fn test_undecorated_statement_still_has_no_decorators() {
        let src = "label plain { let x = 1 }";
        let result = parse_test!(script(), src);
        assert!(result.is_ok());
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        assert_eq!(stmts[0].decorators().len(), 0);
    }

    #[test]
    fn test_decorator_with_expression_arg() {
        let src = "
            @weight(1 + 2)
            label scene { let x = 1 }
        ";
        let result = parse_test!(script(), src);
        assert!(
            result.is_ok(),
            "decorator with expression arg should parse: {result:?}"
        );
    }

    #[test]
    fn test_decorator_inside_code_block() {
        let src = "{
            @on_enter
            label inner { let x = 1 }
        }";
        let result = parse_test!(code_block(), src);
        assert!(
            result.is_ok(),
            "decorator inside code block should parse: {result:?}"
        );
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        assert_eq!(stmts[0].decorators().len(), 1);
    }

    #[test]
    fn test_decorator_constructs() {
        let bare = Decorator::bare("test".to_string());
        assert_eq!(bare.name(), "test");
        assert_eq!(bare.args().content(), &AstContent::ExprList(vec![]));

        let with_args = Decorator::new(
            "event".to_string(),
            Ast::expr_list(vec![Ast::value(crate::runtime::value::RuntimeValue::Int(
                1,
            ))]),
        );
        assert_eq!(with_args.name(), "event");
        let AstContent::ExprList(args) = with_args.args().content() else {
            panic!("expected ExprList");
        };
        assert_eq!(args.len(), 1);
    }

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
                Ast::menu_option(
                    "Option 1".to_string(),
                    Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                        Ast::value(RuntimeValue::Int(1))
                    )])
                ),
                Ast::menu_option(
                    "Option 2".to_string(),
                    Ast::block(vec![Ast::decl(
                        DeclKind::Variable,
                        Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                        Ast::value(RuntimeValue::Int(2))
                    )])
                ),
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

    #[test]
    fn test_jump_basic() {
        let src = "jump my_label";
        let result = parse_test!(jump_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_jump_in_block() {
        let src = "{ jump my_label }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_jump_in_if_statement() {
        let src = "if x > 10 { jump target } else { jump other }";
        let result = parse_test!(if_statement(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_jump_in_menu_option() {
        let src = "menu { \"Option 1\" { jump scene_a }
            \"Option 2\" { jump scene_b } }";
        let result = parse_test!(menu(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_jump_in_script() {
        let src = "
            let x = 10
            jump start_label
        ";
        let result = parse_test!(script(), src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_jump_with_multiple_jumps() {
        let src = "{ jump a; jump b; jump c }";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok());
    }

    // ---- Enum declaration tests ----

    #[test]
    fn test_enum_single_line() {
        let src = "enum Direction { North, South, East, West }";
        let result = parse_test!(enum_decl(), src);
        assert!(result.is_ok(), "single-line enum should parse: {result:?}");
        let Ok(node) = result else { return };
        let AstContent::EnumDecl { name, variants } = node.content() else {
            panic!("expected EnumDecl, got {:?}", node.content());
        };
        assert_eq!(name, "Direction");
        assert_eq!(variants, &["North", "South", "East", "West"]);
    }

    #[test]
    fn test_enum_multiline() {
        let src = "enum Direction {
            North
            South
            East
            West
        }";
        let result = parse_test!(enum_decl(), src);
        assert!(result.is_ok(), "multiline enum should parse: {result:?}");
        let Ok(node) = result else { return };
        let AstContent::EnumDecl { name, variants } = node.content() else {
            panic!("expected EnumDecl, got {:?}", node.content());
        };
        assert_eq!(name, "Direction");
        assert_eq!(variants, &["North", "South", "East", "West"]);
    }

    #[test]
    fn test_enum_mixed_separators() {
        let src = "enum Direction {
            North, South
            East, West
        }";
        let result = parse_test!(enum_decl(), src);
        assert!(
            result.is_ok(),
            "enum with mixed separators should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::EnumDecl { variants, .. } = node.content() else {
            panic!("expected EnumDecl, got {:?}", node.content());
        };
        assert_eq!(variants, &["North", "South", "East", "West"]);
    }

    #[test]
    fn test_enum_single_variant() {
        let src = "enum Coin { Heads }";
        let result = parse_test!(enum_decl(), src);
        assert!(
            result.is_ok(),
            "single-variant enum should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::EnumDecl { name, variants } = node.content() else {
            panic!("expected EnumDecl, got {:?}", node.content());
        };
        assert_eq!(name, "Coin");
        assert_eq!(variants, &["Heads"]);
    }

    #[test]
    fn test_enum_trailing_comma() {
        let src = "enum X { A, B, }";
        let result = parse_test!(enum_decl(), src);
        assert!(
            result.is_ok(),
            "enum with trailing comma should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::EnumDecl { variants, .. } = node.content() else {
            panic!("expected EnumDecl, got {:?}", node.content());
        };
        assert_eq!(variants, &["A", "B"]);
    }

    #[test]
    fn test_enum_in_script() {
        let src = "
            enum Color { Red, Green, Blue }
            let c = Color.Red
        ";
        let result = parse_test!(script(), src);
        assert!(
            result.is_ok(),
            "enum in script should parse: {result:?}"
        );
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        assert_eq!(stmts.len(), 2);
        assert!(matches!(stmts[0].content(), AstContent::EnumDecl { .. }));
    }

    // ---- Match statement tests ----

    #[test]
    fn test_match_enum_variants() {
        let src = "match dir {
            Direction.North { let x = 1 }
            Direction.South { let x = 2 }
            _ { let x = 0 }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "match on enum variants should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::Match { scrutinee: _, arms } = node.content() else {
            panic!("expected Match, got {:?}", node.content());
        };
        assert_eq!(arms.len(), 3);
        assert!(matches!(arms[0].pattern, MatchPattern::Value(_)));
        assert!(matches!(arms[1].pattern, MatchPattern::Value(_)));
        assert!(matches!(arms[2].pattern, MatchPattern::Wildcard));
    }

    #[test]
    fn test_match_literal_int() {
        let src = "match score {
            1 { let grade = \"A\" }
            2 { let grade = \"B\" }
            _ { let grade = \"F\" }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "match on integer literals should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::Match { scrutinee: _, arms } = node.content() else {
            panic!("expected Match, got {:?}", node.content());
        };
        assert_eq!(arms.len(), 3);
        assert!(matches!(
            &arms[0].pattern,
            MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Int(1)))
        ));
        assert!(matches!(
            &arms[1].pattern,
            MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Int(2)))
        ));
        assert!(matches!(arms[2].pattern, MatchPattern::Wildcard));
    }

    #[test]
    fn test_match_literal_bool() {
        let src = "match flag {
            true { let x = 1 }
            false { let x = 0 }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "match on bool literals should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::Match { scrutinee: _, arms } = node.content() else {
            panic!("expected Match, got {:?}", node.content());
        };
        assert_eq!(arms.len(), 2);
        assert!(matches!(
            &arms[0].pattern,
            MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Bool(true)))
        ));
        assert!(matches!(
            &arms[1].pattern,
            MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Bool(false)))
        ));
    }

    #[test]
    fn test_match_wildcard_only() {
        let src = "match x {
            _ { let y = 42 }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "match with only wildcard arm should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::Match { arms, .. } = node.content() else {
            panic!("expected Match, got {:?}", node.content());
        };
        assert_eq!(arms.len(), 1);
        assert!(matches!(arms[0].pattern, MatchPattern::Wildcard));
    }

    #[test]
    fn test_match_no_wildcard() {
        let src = "match phase {
            1 { jump intro }
            2 { jump middle }
            3 { jump outro }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "match with no wildcard arm should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::Match { arms, .. } = node.content() else {
            panic!("expected Match, got {:?}", node.content());
        };
        assert_eq!(arms.len(), 3);
        for arm in arms {
            assert!(
                matches!(arm.pattern, MatchPattern::Value(_)),
                "expected Value pattern, got {:?}",
                arm.pattern
            );
        }
    }

    #[test]
    fn test_match_nested() {
        let src = "match outer {
            1 {
                match inner {
                    true { let z = 1 }
                    _ { let z = 0 }
                }
            }
            _ { let z = 99 }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "nested match should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::Match { arms, .. } = node.content() else {
            panic!("expected outer Match, got {:?}", node.content());
        };
        assert_eq!(arms.len(), 2);
        // The body of the first arm should be a Block containing a Match
        let AstContent::Block(inner_stmts) = arms[0].body.content() else {
            panic!("expected Block in first arm body");
        };
        assert_eq!(inner_stmts.len(), 1);
        assert!(
            matches!(inner_stmts[0].content(), AstContent::Match { .. }),
            "expected inner Match node"
        );
    }

    #[test]
    fn test_match_in_script() {
        let src = "
            enum Dir { North, South }
            match dir {
                Dir.North { jump north_scene }
                Dir.South { jump south_scene }
                _ { jump default_scene }
            }
        ";
        let result = parse_test!(script(), src);
        assert!(
            result.is_ok(),
            "enum + match in script should parse: {result:?}"
        );
        let Ok(block) = result else { return };
        let AstContent::Block(stmts) = block.content() else {
            panic!("expected Block");
        };
        assert_eq!(stmts.len(), 2);
        assert!(matches!(stmts[0].content(), AstContent::EnumDecl { .. }));
        assert!(matches!(stmts[1].content(), AstContent::Match { .. }));
    }

    #[test]
    fn test_match_null_pattern() {
        let src = "match val {
            null { let x = 0 }
            _ { let x = 1 }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "match on null pattern should parse: {result:?}"
        );
        let Ok(node) = result else { return };
        let AstContent::Match { arms, .. } = node.content() else {
            panic!("expected Match");
        };
        assert!(matches!(
            &arms[0].pattern,
            MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Null))
        ));
    }

    #[test]
    fn test_match_scrutinee_expression() {
        let src = "match x + 1 {
            2 { let r = \"two\" }
            _ { let r = \"other\" }
        }";
        let result = parse_test!(match_statement(), src);
        assert!(
            result.is_ok(),
            "match with expression scrutinee should parse: {result:?}"
        );
    }
}
