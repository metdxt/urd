/// Errors that can happen on lexing level
#[derive(thiserror::Error, Debug, PartialEq, Clone, Default)]
pub enum LexerError {
    /// Unknown error during lexing. Ideally wouldn't happen, but you never know.
    #[default]
    #[error("Unknown lexer error.")]
    Unknown,
    #[error("Invalid escape sequence: {0}")]
    InvalidEscape(String),
    #[error("Invalid interpolation")]
    Interpolation,
}
