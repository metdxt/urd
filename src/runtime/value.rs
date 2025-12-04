use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    Null,
    Int(i64),
    Float(f64),
}

impl TryFrom<Token> for RuntimeValue {
    type Error = ();
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::FloatLit(f) => Ok(Self::Float(f)),
            Token::IntLit(i) => Ok(Self::Int(i)),
            _ => Err(()),
        }
    }
}
