mod strings;
mod tokens;
pub use tokens::*;

use crate::erro::LexerError;

type Result<T> = std::result::Result<T, LexerError>;
