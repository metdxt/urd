//! Useful type aliases for the module

use chumsky::{Boxed, Parser, error::Rich, extra, input::ValueInput, span::SimpleSpan};

use crate::{lexer::Token, parser::ast::Ast};

/// Sealed super-trait for chumsky inputs compatible with urd's token/span types.
pub trait UrdInput<'tokens>: ValueInput<'tokens, Token = Token, Span = SimpleSpan> {}
impl<'tokens, T> UrdInput<'tokens> for T where T: ValueInput<'tokens, Token = Token, Span = SimpleSpan> {}

/// Sealed super-trait for chumsky parsers that produce [`Ast`] nodes for urd.
pub trait UrdParser<'tokens, I>:
    Parser<'tokens, I, Ast, extra::Err<Rich<'tokens, Token, SimpleSpan>>> + Clone
where
    I: UrdInput<'tokens>,
{
}
impl<'tokens, I, T> UrdParser<'tokens, I> for T
where
    I: UrdInput<'tokens>,
    T: Parser<'tokens, I, Ast, extra::Err<Rich<'tokens, Token, SimpleSpan>>> + Clone,
{
}

/// A type-erased, boxed parser. Using this as the return type of public parser functions
/// prevents the compiler from monomorphizing deeply-nested combinator type trees at every
/// call site, which would otherwise exhaust RAM during test compilation.
pub type BoxedUrdParser<'tokens, I> =
    Boxed<'tokens, 'tokens, I, Ast, extra::Err<Rich<'tokens, Token, SimpleSpan>>>;
