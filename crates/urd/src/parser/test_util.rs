//! Test utilities for parsing source code in tests.
//!
//! The [`lex_to_vec`] and [`make_input`] functions centralise the lexer-to-parser
//! bridging logic that was previously inlined by the `parse_test!` macro at every
//! call site (226+). By moving this work into real functions the compiler only
//! needs to codegen the lexer→token-collection→input-construction pipeline once,
//! dramatically reducing test compile times.

use chumsky::input::{Input as _, MappedInput};
use chumsky::span::SimpleSpan;

use crate::lexer::{Token, lex_src};

// ---------------------------------------------------------------------------
// Concrete input type
// ---------------------------------------------------------------------------

/// Named helper so we get a real `fn` pointer type (nameable) rather than an
/// anonymous closure type.
fn split_token_span(pair: &(Token, SimpleSpan)) -> (&Token, &SimpleSpan) {
    (&pair.0, &pair.1)
}

/// The concrete, fully-nameable chumsky input type produced by [`make_input`].
///
/// Using a concrete type (instead of the opaque `Stream` + closure chain the
/// old macro produced) means the compiler monomorphises the parser's `.parse()`
/// call exactly once for this input shape.
pub type TokenSliceInput<'tokens> = MappedInput<
    Token,
    SimpleSpan,
    &'tokens [(Token, SimpleSpan)],
    fn(&(Token, SimpleSpan)) -> (&Token, &SimpleSpan),
>;

// ---------------------------------------------------------------------------
// Public helpers
// ---------------------------------------------------------------------------

/// Lex source text into a collected `Vec` of `(Token, SimpleSpan)` pairs.
///
/// Lexer errors are converted to [`Token::Error`] so the parser can report
/// them uniformly.
pub fn lex_to_vec(src: &str) -> Vec<(Token, SimpleSpan)> {
    lex_src(src)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        })
        .collect()
}

/// Build a [`TokenSliceInput`] from a pre-collected token slice.
///
/// The `src_len` parameter is used to construct the *end-of-input* span,
/// which chumsky requires for error reporting on unexpected EOF.
pub fn make_input(tokens: &[(Token, SimpleSpan)], src_len: usize) -> TokenSliceInput<'_> {
    let eoi: SimpleSpan = (src_len..src_len).into();
    tokens.map(
        eoi,
        split_token_span as fn(&(Token, SimpleSpan)) -> (&Token, &SimpleSpan),
    )
}
