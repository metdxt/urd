use std::num::{ParseFloatError, ParseIntError};

/// Errors that can happen on lexing level
#[derive(thiserror::Error, Debug, PartialEq, Clone, Default)]
pub enum LexerError {
    /// Unknown error during lexing. Ideally wouldn't happen, but you never know.
    #[default]
    #[error("Unknown lexer error.")]
    Unknown,

    /// Error for invalid escape sequences in string literals
    #[error("Invalid escape sequence: {0}")]
    InvalidEscape(String),

    /// For all things wrong with interpolation in string literals
    #[error("Invalid interpolation")]
    Interpolation,

    /// Integer parsing failures
    #[error("Invalid integer literal. {0}")]
    Int(#[from] ParseIntError),

    /// Float parsing failures
    #[error("Invalid float literal. {0}")]
    Float(#[from] ParseFloatError),

    /// Errors with dice syntax
    #[error(
        "Invalid dice syntax. If you're wondering why you can't create 1 mil dice with 1 mil faces: we only support up to 255 sides/count"
    )]
    InvalidDice,
}
