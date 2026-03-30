//! Useful type aliases for the module

use chumsky::{Boxed, Parser, error::Rich, extra, input::ValueInput, span::SimpleSpan};

use crate::{lexer::Token, parser::ast::Ast};

/// Input trait alias for parser
pub trait UrdInput<'tokens> = ValueInput<'tokens, Token = Token, Span = SimpleSpan>;
/// Parser trait alias, this depends on `trait_aliases` nightly feature
pub trait UrdParser<'tokens, I> = Parser<'tokens, I, Ast, extra::Err<Rich<'tokens, Token, SimpleSpan>>>
    + Clone
where I: UrdInput<'tokens>;

/// A type-erased, boxed parser. Using this as the return type of public parser functions
/// prevents the compiler from monomorphizing deeply-nested combinator type trees at every
/// call site, which would otherwise exhaust RAM during test compilation.
pub type BoxedUrdParser<'tokens, I> =
    Boxed<'tokens, 'tokens, I, Ast, extra::Err<Rich<'tokens, Token, SimpleSpan>>>;

