//! # Runtime Value Module
//!
//! This module defines the value types used at runtime when executing Urd scripts.
//! The `RuntimeValue` enum represents all possible values that can be produced
//! by the interpreter during script execution.

use crate::lexer::{Token, strings::ParsedString};

/// Represents a value in the Urd runtime environment.
///
/// Runtime values are the result of evaluating expressions and the operands
/// for operations during script execution.
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    /// The null value
    Null,
    /// Boolean value (true or false)
    Bool(bool),
    /// 64-bit signed integer
    Int(i64),
    /// Double-precision floating point number
    Float(f64),
    /// String value with support for interpolation
    Str(ParsedString),
    /// Dice roll value (count, sides)
    Dice(u8, u8),
    /// Identifier representing a variable or property
    Ident(String),
}

#[allow(missing_docs)]
impl TryFrom<Token> for RuntimeValue {
    type Error = ();
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Null => Ok(Self::Null),
            Token::BoolLit(b) => Ok(Self::Bool(b)),
            Token::FloatLit(f) => Ok(Self::Float(f)),
            Token::IntLit(i) => Ok(Self::Int(i)),
            Token::StrLit(s) => Ok(Self::Str(s)),
            Token::Dice((count, sides)) => Ok(Self::Dice(count, sides)),
            Token::Ident(name) => Ok(Self::Ident(name)),
            _ => Err(()),
        }
    }
}
