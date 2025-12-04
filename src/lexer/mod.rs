//! # Lexer Module
//!
//! This module is responsible for tokenizing Urd source code. It breaks down the input text
//! into a sequence of tokens that can be processed by the parser. The lexer handles string
//! literals, including interpolation, and various language tokens like operators, keywords,
//! and literals.
//!
//! ## Submodules
//!
//! - [`strings`]: String literal parsing and interpolation handling
//! - [`tokens`]: Token definitions and lexing logic

pub mod strings;
mod tokens;
pub use tokens::*;

use crate::erro::LexerError;

type Result<T> = std::result::Result<T, LexerError>;
