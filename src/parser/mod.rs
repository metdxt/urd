//! # Parser Module
//!
//! This module contains the parsing logic for the Urd language. It transforms a sequence
//! of tokens from the lexer into an Abstract Syntax Tree (AST) that represents the
//! structure of the source code.
//!
//! The parser is implemented using the Pratt parsing technique, which is particularly
//! well-suited for handling operator precedence and associativity in expressions.
//!
//! ## Submodules
//!
//! - [`ast`]: Abstract Syntax Tree definitions and node types
//! - [`expr`]: Expression parsing implementation using Pratt parser

pub mod aliases;
pub mod ast;
pub mod expr;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{input::Stream, prelude::*};

use crate::lexer::{Token, lex_src};

/// Parses a string of Urd source code and either returns a successful result or prints
/// detailed error information.
pub fn parse_src(src: &str) -> Result<(), ()> {
    let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(_e) => (Token::Error, span.into()),
    });

    let tok_stream =
        Stream::from_iter(lexer).map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

    match expr::expr().parse(tok_stream).into_result() {
        Ok(ast) => {
            println!("{:?}", ast);
            Ok(())
        }
        Err(errs) => {
            for err in errs {
                #[allow(clippy::unwrap_used)]
                Report::build(ReportKind::Error, ((), err.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(((), err.span().into_range()))
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(src))
                    .unwrap();
            }
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_src;

    #[test]
    fn test_parser() {
        // Test basic arithmetic operators
        assert!(parse_src("23+4-5*12").is_ok());
        assert!(parse_src("10 / 2").is_ok());
        assert!(parse_src("10 // 3").is_ok()); // Integer division
        assert!(parse_src("10 % 3").is_ok()); // Modulo

        // Test unary operators
        assert!(parse_src("-5").is_ok());
        assert!(parse_src("!true").is_ok());

        // Test comparison operators
        assert!(parse_src("5 > 3").is_ok());
        assert!(parse_src("5 < 3").is_ok());
        assert!(parse_src("5 >= 5").is_ok());
        assert!(parse_src("5 <= 5").is_ok());
        assert!(parse_src("5 == 5").is_ok());
        assert!(parse_src("5 != 3").is_ok());

        // Test bitwise operators
        assert!(parse_src("5 & 3").is_ok());
        assert!(parse_src("5 | 3").is_ok());
        assert!(parse_src("5 ^ 3").is_ok());
        assert!(parse_src("5 << 1").is_ok());
        assert!(parse_src("5 >> 1").is_ok());

        // Test logical operators
        assert!(parse_src("true and false").is_ok());
        assert!(parse_src("true && false").is_ok());
        assert!(parse_src("true or false").is_ok());
        assert!(parse_src("true || false").is_ok());

        // Test assignment operator
        assert!(parse_src("x = 5").is_ok());

        // Test operator precedence
        assert!(parse_src("1 + 2 * 3").is_ok()); // Should be 1 + (2 * 3)
        assert!(parse_src("1 * 2 + 3").is_ok()); // Should be (1 * 2) + 3
        assert!(parse_src("!a & b").is_ok()); // Should be (!a) & b
        assert!(parse_src("a & b | c").is_ok()); // Should be (a & b) | c
        assert!(parse_src("a == b and c != d").is_ok()); // Should be (a == b) and (c != d)
        assert!(parse_src("x = a + b * c").is_ok()); // Should be x = (a + (b * c))

        // Test complex expressions
        assert!(parse_src("(1 + 2) * 3").is_ok());
        assert!(parse_src("-a + b * c").is_ok());
        assert!(parse_src("a > b and c < d or e == f").is_ok());

        // Test all literal types
        assert!(parse_src("null").is_ok());
        assert!(parse_src("true").is_ok());
        assert!(parse_src("false").is_ok());
        assert!(parse_src("42").is_ok());
        assert!(parse_src("3.14").is_ok());
        assert!(parse_src("\"hello\"").is_ok());
        assert!(parse_src("2d6").is_ok());
        assert!(parse_src("identifier").is_ok());

        // Test string with interpolation (should parse but interpolation handled at runtime)
        assert!(parse_src("\"hello {name}\"").is_ok());
    }
}
