pub mod ast;
pub mod expr;

use std::error::Error;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};

use crate::lexer::{Token, lex_src};

pub fn parse_src(src: &str) -> Result<(), ()> {
    let lexer = lex_src(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(e) => (Token::Error, span.into()),
    });

    let tok_stream =
        Stream::from_iter(lexer).map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

    match expr::expr().parse(tok_stream).into_result() {
        Ok(ast) => {
            println!("{:?}", ast);
            Ok(())
        }
        Err(errs) => {
            for err in errs {
                #[allow(clippy::unwrap_used)]
                Report::build(ReportKind::Error, ((), err.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(((), err.span().into_range()))
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(src))
                    .unwrap();
            }
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_src;

    #[test]
    fn test_parser() {
        let a = parse_src("");
    }
}
