//! # Expression Parser Module
//!
//! This module implements a Pratt parser for parsing expressions in the Urd language.
//! The Pratt parser technique is particularly well-suited for handling operator precedence
//! and associativity in expressions.
//!
//! The parser handles all operators defined in the Urd language, including:
//! - Arithmetic operators: +, -, *, /, //, %
//! - Comparison operators: ==, !=, >, <, >=, <=
//! - Bitwise operators: &, |, ^, !, <<, >>
//! - Logical operators: and/&&, or/||
//! - Assignment operator: =

use chumsky::pratt::*;
use chumsky::prelude::*;
use chumsky::{Parser, select};

use super::aliases::{Input, UrdParser};
use super::ast::Ast;
use crate::lexer::Token;
use crate::runtime::value::RuntimeValue;

/// Represents a value
pub fn atom<'tokens, I: Input<'tokens>>() -> impl UrdParser<'tokens, I> {
    select! {
        Token::Null => Ast::value(RuntimeValue::Null),
        Token::BoolLit(b) => Ast::value(RuntimeValue::Bool(b)),
        Token::IntLit(i) => Ast::value(RuntimeValue::Int(i)),
        Token::FloatLit(f) => Ast::value(RuntimeValue::Float(f)),
        Token::StrLit(s) => Ast::value(RuntimeValue::Str(s)),
        Token::Dice((count, sides)) => Ast::value(RuntimeValue::Dice(count, sides)),
        Token::Ident(name) => Ast::value(RuntimeValue::Ident(name)),
    }
    .labelled("value")
}

/// Creates a Pratt parser for parsing expressions in the Urd language.
///
/// The parser handles all literals, identifiers, unary operators, binary operators,
/// and parenthesized expressions. Operator precedence is defined by the Pratt
/// parser configuration with higher numbers indicating higher precedence.
pub fn expr<'tokens, I: Input<'tokens>>() -> impl UrdParser<'tokens, I> {
    recursive(|expr| {
        let term = atom().or(expr.delimited_by(just(Token::LeftParen), just(Token::RightParen)));

        term.pratt((
            // Unary operators (precedence 11)
            prefix(11, just(Token::BitwiseNot), |_, r, _| {
                Ast::bitwise_not_op(r)
            }),
            prefix(11, just(Token::Not), |_, r, _| Ast::not_op(r)),
            prefix(11, just(Token::Minus), |_, r, _| Ast::negate_op(r)),
            // Multiplication/division (precedence 10)
            infix(left(10), just(Token::Star), |l, _, r, _| {
                Ast::multiply_op(l, r)
            }),
            infix(left(10), just(Token::Slash), |l, _, r, _| {
                Ast::divide_op(l, r)
            }),
            infix(left(10), just(Token::DoubleSlash), |l, _, r, _| {
                Ast::floordiv_op(l, r)
            }),
            infix(left(10), just(Token::Percent), |l, _, r, _| {
                Ast::modulo_op(l, r)
            }),
            // Addition/subtraction (precedence 9)
            infix(left(9), just(Token::Plus), |l, _, r, _| Ast::add_op(l, r)),
            infix(left(9), just(Token::Minus), |l, _, r, _| {
                Ast::substract_op(l, r)
            }),
            // Bitwise shifts (precedence 8)
            infix(left(8), just(Token::LeftShift), |l, _, r, _| {
                Ast::left_shift_op(l, r)
            }),
            infix(left(8), just(Token::RightShift), |l, _, r, _| {
                Ast::right_shift_op(l, r)
            }),
            // Comparisons (precedence 7)
            infix(left(7), just(Token::GreaterThan), |l, _, r, _| {
                Ast::greater_than_op(l, r)
            }),
            infix(left(7), just(Token::LessThan), |l, _, r, _| {
                Ast::less_than_op(l, r)
            }),
            infix(left(7), just(Token::GreaterThanOrEquals), |l, _, r, _| {
                Ast::greater_than_or_equals_op(l, r)
            }),
            infix(left(7), just(Token::LessThanOrEquals), |l, _, r, _| {
                Ast::less_than_or_equals_op(l, r)
            }),
            // Equality (precedence 6)
            infix(left(6), just(Token::Equals), |l, _, r, _| {
                Ast::equals_op(l, r)
            }),
            infix(left(6), just(Token::NotEquals), |l, _, r, _| {
                Ast::not_equals_op(l, r)
            }),
            // Bitwise AND (precedence 5)
            infix(left(5), just(Token::BitwiseAnd), |l, _, r, _| {
                Ast::bitwise_and_op(l, r)
            }),
            // Bitwise XOR (precedence 4)
            infix(left(4), just(Token::BitwiseXor), |l, _, r, _| {
                Ast::bitwise_xor_op(l, r)
            }),
            // Bitwise OR (precedence 3)
            infix(left(3), just(Token::BitwiseOr), |l, _, r, _| {
                Ast::bitwise_or_op(l, r)
            }),
            // Logical AND (precedence 2)
            infix(left(2), just(Token::And), |l, _, r, _| Ast::and_op(l, r)),
            // Logical OR (precedence 1)
            infix(left(1), just(Token::Or), |l, _, r, _| Ast::or_op(l, r)),
            // Assignment (right-associative, lowest precedence 0)
            infix(right(0), just(Token::Assign), |l, _, r, _| {
                Ast::assign_op(l, r)
            }),
        ))
    })
    .boxed()
    .labelled("expression")
}
