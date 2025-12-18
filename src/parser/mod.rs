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
pub mod block;
pub mod expr;

/// Helper macro to generate boilerplate for parsing text using Urd lexer
/// To use with any UrdParser.
#[cfg(test)]
#[macro_export]
macro_rules! parse_test {
    ($parser:expr, $src:expr) => {{
        use chumsky::input::Stream;
        use chumsky::prelude::*;
        use $crate::lexer::{Token, lex_src};

        let src = $src;
        let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        });

        let stream = Stream::from_iter(lexer)
            .map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

        $parser.parse(stream).into_result()
    }};
}
