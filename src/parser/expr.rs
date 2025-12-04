use chumsky::{Parser, error::Rich, extra, input::ValueInput, select, span::SimpleSpan};

use super::ast::Ast;
use crate::lexer::Token;
use crate::runtime::value::RuntimeValue;

pub fn expr<'tokens, I>() -> impl Parser<'tokens, I, Ast, extra::Err<Rich<'tokens, Token>>>
where
    I: ValueInput<'tokens, Token = Token, Span = SimpleSpan>,
{
    select! {
        Token::Null => Ast::value(RuntimeValue::Null),
        Token::IntLit(i) => Ast::value(RuntimeValue::Int(i)),
        Token::FloatLit(f) => Ast::value(RuntimeValue::Float(f))
    }
}
