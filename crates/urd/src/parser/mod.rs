//! # Parser Module
//!
//! This module contains the parsing logic for the Urd language. It transforms a sequence
//! of tokens from the lexer into an Abstract Syntax Tree (AST) that represents the
//! structure of the source code.
//!
//! The parser is built with chumsky, a parser-combinator library. Expression parsing
//! uses chumsky's built-in Pratt parser for operator precedence and associativity.
//!
//! ## Submodules
//!
//! - [`aliases`]: Type aliases used across parser sub-modules
//! - [`ast`]: Abstract Syntax Tree definitions and node types
//! - [`block`]: Block-level statement and top-level item parsers
//! - [`errors`]: Parser error types and diagnostics
//! - [`expr`]: Expression parsing implementation using Pratt parser
//! - [`test_util`]: Shared test helpers (available under `#[cfg(test)]` or `test-support` feature)

pub mod aliases;
pub mod ast;
pub mod block;
pub mod errors;
pub mod expr;
#[cfg(any(test, feature = "test-support"))]
pub mod test_util;

/// Helper macro to generate boilerplate for parsing text using Urd lexer.
///
/// Delegates to [`test_util::lex_to_vec`] and [`test_util::make_input`] so the
/// lexer-to-input bridging code is compiled exactly **once**, rather than being
/// monomorphised at each of the 200+ call sites.
///
/// Uses `into_output_errors` internally so that all borrowed data from the token
/// slice is consumed (and errors stringified) before the temporary `Vec` is dropped,
/// avoiding lifetime issues with the slice-backed input type.
#[cfg(any(test, feature = "test-support"))]
#[macro_export]
macro_rules! parse_test {
    ($parser:expr, $src:expr) => {{
        use chumsky::Parser as _;
        use $crate::parser::test_util::{lex_to_vec, make_input};

        let src = $src;
        let tokens = lex_to_vec(src);
        let input = make_input(&tokens, src.len());
        // `into_output_errors` lets us consume the borrowed Rich errors while
        // `tokens` is still alive, then convert them to owned Strings.
        let (output, errors) = $parser.parse(input).into_output_errors();
        if errors.is_empty() {
            Ok(output.expect("chumsky produced no output and no errors"))
        } else {
            Err(errors
                .into_iter()
                .map(|e| format!("{e:?}"))
                .collect::<Vec<String>>())
        }
    }};
}
