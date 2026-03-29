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

use super::aliases::{BoxedUrdParser, UrdInput, UrdParser};

/// Parser for a single statement.
pub fn statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    statement_inner(code_block()).boxed()
}

/// Parser for return statement (return expr)
pub fn return_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    just(Token::Return)
        .ignore_then(expr().or_not())
        .map(Ast::return_stmt)
        .boxed()
}

/// Parser for jump statement (jump ident)
pub fn jump_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
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
pub fn assignment<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    }
    .then_ignore(just(Token::Assign))
    .then(expr())
    .map(|(ident, value)| Ast::assign_op(ident, value))
    .boxed()
}

/// Parser for dialogue lines
pub fn dialogue<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
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
pub fn if_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    if_parser(code_block()).boxed()
}

/// Parser for labeled block
pub fn labeled_block<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    labeled_block_parser(code_block()).boxed()
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
pub fn code_block<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
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
pub fn menu<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    menu_parser(code_block()).boxed()
}

/// Parser for enum declarations: `enum Foo { A, B, C }`
pub fn enum_decl<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    enum_decl_parser().boxed()
}

/// Parser for match statements: `match expr { pattern { ... } ... }`
pub fn match_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    match_parser(code_block()).boxed()
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
pub fn script<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    let separator = just(Token::Newline).or(just(Token::Semicolon));

    statement()
        .separated_by(separator.repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(Ast::block)
        .boxed()
}
