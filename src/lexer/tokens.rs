use logos::{Lexer, Logos};

use super::Result;
use super::strings::{ParsedString, string_parsing_callback};
use crate::erro::LexerError;

/// Main token enum for Urd
#[derive(Logos, Debug, Clone)]
#[logos(error = LexerError)]
pub enum Token {
    // ---- Basic datatypes ----
    /// String literal. Can be a static or interpolated string.
    /// In urd strings are multiline be default, no need to multiply enities.
    #[token("\"", string_parsing_callback)]
    StrLit(ParsedString),

    /// Boolean literal (true/false)
    #[regex(r#"true|false"#, |lex| lex.slice() == "true")]
    BoolLit(bool),

    /// Integer literal
    /// Accepts: decimal, hexidecimal, octodecimal and binary radix numbers.
    /// Separating numbers with _ is allowed.
    #[regex(
        r#"(0x[0-9a-fA-F_]+)|(0b[01_]+)|(0o[0-7_]+)|([0-9][0-9_]*)"#,
        integer_parsing_callback
    )]
    IntLit(i64),

    /// Floating point numbers. Represented with double precision.
    /// Requires a dot `.` or `e` char to disambiguate with ints.
    ///
    #[regex(
        r#"(?:[0-9][0-9_]*)?\.([0-9][0-9_]*)(?:[eE][+-]?[0-9][0-9_]*)?"#,
        float_parsing_callback
    )]
    #[regex(r#"[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*"#, float_parsing_callback)]
    FloatLit(f64),

    /// Dice syntax. Supports small d or capital D seprator and up to 255 count/sides
    #[regex(r#"\d{1,3}[dD]\d{1,3}"#, dice_parsing_callback)]
    Dice((u8, u8)),

    /// Identifier
    /// Identifier must start with ascii letter, optionally prefixed with any number of `_`.
    /// Then it can contain any number of letters, digits or `_`
    #[regex(r#"_*[a-zA-Z][_a-zA-Z\d]*"#, |lex| lex.slice().to_string())]
    Ident(String),

    // ---- Operators ----
    #[allow(missing_docs)]
    #[token("+")]
    Plus,

    #[allow(missing_docs)]
    #[token("-")]
    Minus,

    #[allow(missing_docs)]
    #[token("*")]
    Star,

    #[allow(missing_docs)]
    #[token("/")]
    Slash,

    #[allow(missing_docs)]
    #[token("//")]
    DoubleSlash,

    #[allow(missing_docs)]
    #[token("%")]
    Percent,
}

/// Parses ints, captured by `(0x[0-9a-fA-F_]+)|(0b[01_]+)|(0o[0-7_]+)|([0-9][0-9_]*)` regex
fn integer_parsing_callback<'a>(lex: &mut Lexer<'a, Token>) -> Result<i64> {
    let capture = lex.slice();
    let clean: String = capture.chars().filter(|c| *c != '_').collect();

    if let Some(hex) = clean.strip_prefix("0x") {
        Ok(i64::from_str_radix(hex, 16)?)
    } else if let Some(oct) = clean.strip_prefix("0o") {
        Ok(i64::from_str_radix(oct, 8)?)
    } else if let Some(bin) = clean.strip_prefix("0b") {
        Ok(i64::from_str_radix(bin, 2)?)
    } else {
        Ok(clean.parse()?)
    }
}

/// Parses floats captured by either of the expressions:
///
/// - `(?:[0-9][0-9_]*)?\.([0-9][0-9_]*)(?:[eE][+-]?[0-9][0-9_]*)?`
/// - `[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*`
fn float_parsing_callback<'a>(lex: &mut Lexer<'a, Token>) -> Result<f64> {
    let capture = lex.slice();
    let clean: String = capture.chars().filter(|c| *c != '_').collect();

    Ok(clean.parse()?)
}

/// Parses dice captured by `\d{1,3}[dD]\d{1,3}` regex
fn dice_parsing_callback<'a>(lex: &mut logos::Lexer<'a, Token>) -> Result<(u8, u8)> {
    let slice = lex.slice();
    let mid = slice.find(['d', 'D']).ok_or(LexerError::InvalidDice)?;

    let count = slice[..mid].parse()?;
    let sides = slice[mid + 1..]
        .parse()
        .map_err(|_| LexerError::InvalidDice)?;

    Ok((count, sides))
}

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)] // unwrap is still forbidden

    use super::*;

    fn lex(s: &str) -> Token {
        Token::lexer(s)
            .next()
            .expect("No token")
            .expect("Lexing error")
    }

    #[test]
    fn integers() {
        assert!(matches!(lex("0"), Token::IntLit(0)));
        assert!(matches!(lex("42"), Token::IntLit(42)));
        assert!(matches!(lex("1_000"), Token::IntLit(1000)));

        assert!(matches!(lex("0xFF"), Token::IntLit(255)));
        assert!(matches!(lex("0b1010"), Token::IntLit(10)));
        assert!(matches!(lex("0o77"), Token::IntLit(63)));
    }

    #[test]
    fn floats() {
        match lex("3.14") {
            #[allow(clippy::approx_constant)]
            Token::FloatLit(f) => assert!((f - 3.14).abs() < f64::EPSILON),
            _ => panic!("Expected FloatLit"),
        }

        match lex(".5") {
            Token::FloatLit(f) => assert!((f - 0.5).abs() < f64::EPSILON),
            _ => panic!("Expected FloatLit"),
        }

        match lex("2.5e2") {
            Token::FloatLit(f) => assert!((f - 250.0).abs() < f64::EPSILON),
            _ => panic!("Expected FloatLit"),
        }
    }

    #[test]
    fn bools() {
        assert!(matches!(lex("true"), Token::BoolLit(true)));
        assert!(matches!(lex("false"), Token::BoolLit(false)));
    }

    #[test]
    fn dice() {
        assert!(matches!(lex("1d6"), Token::Dice((1, 6))));
        assert!(matches!(lex("2D20"), Token::Dice((2, 20))));

        // Check overflow errors
        let mut lexer = Token::lexer("300d6");
        assert!(lexer.next().expect("No token?").is_err());

        let mut lexer = Token::lexer("1d300");
        assert!(lexer.next().expect("No token?").is_err());
    }

    #[test]
    fn strings() {
        assert!(matches!(lex(r#""hello""#), Token::StrLit(_)));
    }

    #[test]
    fn identifiers() {
        assert!(matches!(lex("foo"), Token::Ident(s) if s == "foo"));
        assert!(matches!(lex("_bar"), Token::Ident(s) if s == "_bar"));
        assert!(matches!(lex("baz_123"), Token::Ident(s) if s == "baz_123"));
    }

    #[test]
    fn operators() {
        assert!(matches!(lex("+"), Token::Plus));
        assert!(matches!(lex("-"), Token::Minus));
        assert!(matches!(lex("*"), Token::Star));
        assert!(matches!(lex("/"), Token::Slash));
        assert!(matches!(lex("//"), Token::DoubleSlash));
        assert!(matches!(lex("%"), Token::Percent));
    }
}
