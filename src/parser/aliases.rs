//! Useful type aliases for the module

use chumsky::{Parser, error::Rich, extra, input::ValueInput, span::SimpleSpan};

use crate::{lexer::Token, parser::ast::Ast};

/// Input trait alias for parser
pub trait Input<'tokens> = ValueInput<'tokens, Token = Token, Span = SimpleSpan>;
/// Parser trait alias, this depends on `trait_aliases` nightly feature
pub trait UrdParser<'tokens, I> = Parser<'tokens, I, Ast, extra::Err<Rich<'tokens, Token, SimpleSpan>>>
    + Clone
where I: Input<'tokens>;
