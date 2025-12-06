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

use super::aliases::{UrdInput, UrdParser};
use super::ast::Ast;
use crate::lexer::Token;
use crate::runtime::value::RuntimeValue;

/// Represents a value
pub fn atom<'tokens, I: UrdInput<'tokens>>() -> impl UrdParser<'tokens, I> {
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
pub fn expr<'tokens, I: UrdInput<'tokens>>() -> impl UrdParser<'tokens, I> {
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

#[cfg(test)]
mod tests {
    use crate::{
        parse_test,
        parser::{
            ast::{Ast, Operator, UnaryOperator},
            expr::expr,
        },
        runtime::value::RuntimeValue,
    };

    // Test literals
    #[test]
    #[allow(clippy::approx_constant)]
    fn test_literals() {
        assert_eq!(
            parse_test!(expr(), "42"),
            Ok(Ast::value(RuntimeValue::Int(42)))
        );

        assert_eq!(
            parse_test!(expr(), "3.14"),
            Ok(Ast::value(RuntimeValue::Float(3.14)))
        );

        assert_eq!(
            parse_test!(expr(), "true"),
            Ok(Ast::value(RuntimeValue::Bool(true)))
        );

        assert_eq!(
            parse_test!(expr(), "false"),
            Ok(Ast::value(RuntimeValue::Bool(false)))
        );

        assert_eq!(
            parse_test!(expr(), "null"),
            Ok(Ast::value(RuntimeValue::Null))
        );

        assert_eq!(
            parse_test!(expr(), "\"hello\""),
            Ok(Ast::value(RuntimeValue::Str(
                crate::lexer::strings::ParsedString::new_plain("hello")
            )))
        );

        assert_eq!(
            parse_test!(expr(), "2d6"),
            Ok(Ast::value(RuntimeValue::Dice(2, 6)))
        );

        assert_eq!(
            parse_test!(expr(), "x"),
            Ok(Ast::value(RuntimeValue::Ident("x".to_string())))
        );
    }

    // Test parenthesized expressions
    #[test]
    fn test_parenthesized_expressions() {
        assert_eq!(
            parse_test!(expr(), "(1)"),
            Ok(Ast::value(RuntimeValue::Int(1)))
        );

        assert_eq!(
            parse_test!(expr(), "(1 + 2)"),
            Ok(Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );
    }

    // Test unary operators
    #[test]
    fn test_unary_operators() {
        assert_eq!(
            parse_test!(expr(), "-42"),
            Ok(Ast::unary(
                UnaryOperator::Negate,
                Ast::value(RuntimeValue::Int(42))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "not true"),
            Ok(Ast::unary(
                UnaryOperator::Not,
                Ast::value(RuntimeValue::Bool(true))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "!1"),
            Ok(Ast::unary(
                UnaryOperator::BitwiseNot,
                Ast::value(RuntimeValue::Int(1))
            ))
        );
    }

    // Test arithmetic operators
    #[test]
    fn test_arithmetic_operators() {
        assert_eq!(
            parse_test!(expr(), "1 + 2"),
            Ok(Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "3 - 1"),
            Ok(Ast::binop(
                Operator::Minus,
                Ast::value(RuntimeValue::Int(3)),
                Ast::value(RuntimeValue::Int(1))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "4 * 5"),
            Ok(Ast::binop(
                Operator::Multiply,
                Ast::value(RuntimeValue::Int(4)),
                Ast::value(RuntimeValue::Int(5))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "10 / 2"),
            Ok(Ast::binop(
                Operator::Divide,
                Ast::value(RuntimeValue::Int(10)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "10 // 3"),
            Ok(Ast::binop(
                Operator::DoubleSlash,
                Ast::value(RuntimeValue::Int(10)),
                Ast::value(RuntimeValue::Int(3))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "10 % 3"),
            Ok(Ast::binop(
                Operator::Percent,
                Ast::value(RuntimeValue::Int(10)),
                Ast::value(RuntimeValue::Int(3))
            ))
        );
    }

    // Test comparison operators
    #[test]
    fn test_comparison_operators() {
        assert_eq!(
            parse_test!(expr(), "1 == 2"),
            Ok(Ast::binop(
                Operator::Equals,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 != 2"),
            Ok(Ast::binop(
                Operator::NotEquals,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 > 2"),
            Ok(Ast::binop(
                Operator::GreaterThan,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 < 2"),
            Ok(Ast::binop(
                Operator::LessThan,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 >= 2"),
            Ok(Ast::binop(
                Operator::GreaterThanOrEquals,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 <= 2"),
            Ok(Ast::binop(
                Operator::LessThanOrEquals,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );
    }

    // Test bitwise operators
    #[test]
    fn test_bitwise_operators() {
        assert_eq!(
            parse_test!(expr(), "1 & 2"),
            Ok(Ast::binop(
                Operator::BitwiseAnd,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 | 2"),
            Ok(Ast::binop(
                Operator::BitwiseOr,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 ^ 2"),
            Ok(Ast::binop(
                Operator::BitwiseXor,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "1 << 2"),
            Ok(Ast::binop(
                Operator::LeftShift,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "8 >> 2"),
            Ok(Ast::binop(
                Operator::RightShift,
                Ast::value(RuntimeValue::Int(8)),
                Ast::value(RuntimeValue::Int(2))
            ))
        );
    }

    // Test logical operators
    #[test]
    fn test_logical_operators() {
        assert_eq!(
            parse_test!(expr(), "true and false"),
            Ok(Ast::binop(
                Operator::And,
                Ast::value(RuntimeValue::Bool(true)),
                Ast::value(RuntimeValue::Bool(false))
            ))
        );

        assert_eq!(
            parse_test!(expr(), "true or false"),
            Ok(Ast::binop(
                Operator::Or,
                Ast::value(RuntimeValue::Bool(true)),
                Ast::value(RuntimeValue::Bool(false))
            ))
        );
    }

    // Test assignment operator
    #[test]
    fn test_assignment_operator() {
        assert_eq!(
            parse_test!(expr(), "x = 42"),
            Ok(Ast::binop(
                Operator::Assign,
                Ast::value(RuntimeValue::Ident("x".to_string())),
                Ast::value(RuntimeValue::Int(42))
            ))
        );
    }

    // Test operator precedence
    #[test]
    fn test_operator_precedence() {
        // Multiplication should have higher precedence than addition
        assert_eq!(
            parse_test!(expr(), "1 + 2 * 3"),
            Ok(Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::binop(
                    Operator::Multiply,
                    Ast::value(RuntimeValue::Int(2)),
                    Ast::value(RuntimeValue::Int(3))
                )
            ))
        );

        // Equality should have lower precedence than addition
        assert_eq!(
            parse_test!(expr(), "1 + 2 == 3"),
            Ok(Ast::binop(
                Operator::Equals,
                Ast::binop(
                    Operator::Plus,
                    Ast::value(RuntimeValue::Int(1)),
                    Ast::value(RuntimeValue::Int(2))
                ),
                Ast::value(RuntimeValue::Int(3))
            ))
        );

        // Assignment should have the lowest precedence
        assert_eq!(
            parse_test!(expr(), "x = 1 + 2 * 3"),
            Ok(Ast::binop(
                Operator::Assign,
                Ast::value(RuntimeValue::Ident("x".to_string())),
                Ast::binop(
                    Operator::Plus,
                    Ast::value(RuntimeValue::Int(1)),
                    Ast::binop(
                        Operator::Multiply,
                        Ast::value(RuntimeValue::Int(2)),
                        Ast::value(RuntimeValue::Int(3))
                    )
                )
            ))
        );
    }

    // Test associativity
    #[test]
    fn test_associativity() {
        // Left associativity for addition
        assert_eq!(
            parse_test!(expr(), "1 - 2 - 3"),
            Ok(Ast::binop(
                Operator::Minus,
                Ast::binop(
                    Operator::Minus,
                    Ast::value(RuntimeValue::Int(1)),
                    Ast::value(RuntimeValue::Int(2))
                ),
                Ast::value(RuntimeValue::Int(3))
            ))
        );

        // Right associativity for assignment
        assert_eq!(
            parse_test!(expr(), "x = y = 1"),
            Ok(Ast::binop(
                Operator::Assign,
                Ast::value(RuntimeValue::Ident("x".to_string())),
                Ast::binop(
                    Operator::Assign,
                    Ast::value(RuntimeValue::Ident("y".to_string())),
                    Ast::value(RuntimeValue::Int(1))
                )
            ))
        );
    }

    // Test complex expressions
    #[test]
    fn test_complex_expressions() {
        // Nested expressions with mixed operators
        assert_eq!(
            parse_test!(expr(), "(1 + 2) * (3 - 4) / 5"),
            Ok(Ast::binop(
                Operator::Divide,
                Ast::binop(
                    Operator::Multiply,
                    Ast::binop(
                        Operator::Plus,
                        Ast::value(RuntimeValue::Int(1)),
                        Ast::value(RuntimeValue::Int(2))
                    ),
                    Ast::binop(
                        Operator::Minus,
                        Ast::value(RuntimeValue::Int(3)),
                        Ast::value(RuntimeValue::Int(4))
                    )
                ),
                Ast::value(RuntimeValue::Int(5))
            ))
        );

        // Mixed logical and comparison operators
        assert_eq!(
            parse_test!(expr(), "1 < 2 and 3 > 4"),
            Ok(Ast::binop(
                Operator::And,
                Ast::binop(
                    Operator::LessThan,
                    Ast::value(RuntimeValue::Int(1)),
                    Ast::value(RuntimeValue::Int(2))
                ),
                Ast::binop(
                    Operator::GreaterThan,
                    Ast::value(RuntimeValue::Int(3)),
                    Ast::value(RuntimeValue::Int(4))
                )
            ))
        );

        // Chained comparisons with logical operators
        assert_eq!(
            parse_test!(expr(), "1 < 2 == 2 < 3"),
            Ok(Ast::binop(
                Operator::Equals,
                Ast::binop(
                    Operator::LessThan,
                    Ast::value(RuntimeValue::Int(1)),
                    Ast::value(RuntimeValue::Int(2))
                ),
                Ast::binop(
                    Operator::LessThan,
                    Ast::value(RuntimeValue::Int(2)),
                    Ast::value(RuntimeValue::Int(3))
                )
            ))
        );
    }

    // Test error handling
    #[test]
    fn test_error_handling() {
        // Incomplete expression
        assert!(parse_test!(expr(), "1 +").is_err());

        // Unbalanced parentheses
        assert!(parse_test!(expr(), "(1 + 2").is_err());
        assert!(parse_test!(expr(), "1 + 2)").is_err());

        // Invalid operator sequence
        assert!(parse_test!(expr(), "1 + + 2").is_err());

        // Empty input
        assert!(parse_test!(expr(), "").is_err());
    }
}
