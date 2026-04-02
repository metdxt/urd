//! # Token Definitions Module
//!
//! This module defines the tokens used in the Urd language. The `Token` enum represents
//! all possible lexical elements that can be recognized by the lexer, including literals,
//! identifiers, operators, and punctuation.
//!
//! The module also provides callback functions for parsing complex tokens like integers,
//! floats, and dice expressions.

use logos::{Lexer, Logos};
use logos_display::{Debug, Display};

use super::Result;
use super::strings::{ParsedString, string_parsing_callback};
use crate::erro::LexerError;

/// Main token enum for Urd
#[allow(missing_docs)] // most tokens are self-explanatory
#[derive(Logos, Display, Debug, Clone, PartialEq)]
#[logos(error = LexerError)]
#[logos(skip r"[ \t\r]+")] // Skip whitespace
#[logos(skip r"#[^\n]*")] // Skip # line comments (## doc comments are matched first as tokens)
pub enum Token {
    /// Documentation comment token (`## some doc text`).
    /// The captured string contains the trimmed comment text after the `##`.
    #[regex(r"##[^\n]*", |lex| lex.slice()[2..].trim().to_string())]
    DocComment(String),

    /// When error occurs on lexer level it is packed into Error kind token.
    /// We want parsing to be recoverable and not fail at the lexing stage.
    Error(LexerError),

    // ---- Basic datatypes ----
    /// The null value
    #[token("null")]
    Null,
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

    /// `end!` — unconditional script terminator.
    /// Must appear before `IdentPath` so Logos matches the longer `end!` literal
    /// rather than lexing `end` as an identifier followed by `!`.
    #[token("end!")]
    EndBang,

    /// `todo!` — placeholder terminator (logs a warning, then ends).
    /// Must appear before `IdentPath` for the same reason as `EndBang`.
    #[token("todo!")]
    TodoBang,

    /// Identifier
    /// Identifier must start with ascii letter, optionally prefixed with any number of `_`.
    /// Then it can contain any number of letters, digits or `_`
    #[regex(r#"(_*[a-zA-Z][_a-zA-Z\d]*)(\._*[a-zA-Z][_a-zA-Z\d]*)*"#, |lex| lex.slice().split(".").map(|s| s.to_string()).collect::<Vec<String>>())]
    IdentPath(Vec<String>),

    // ---- Operators ----
    #[token("+")]
    Plus,

    /// `->` arrow, separates function parameters from the return type.
    #[token("->")]
    Arrow,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("//")]
    DoubleSlash,

    #[token("%")]
    Percent,

    #[token("=")]
    Assign,

    // ---- Parentheses ----
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftCurly,

    #[token("}")]
    RightCurly,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token(":{")]
    DictStart,

    // ---- Comparison operators ----
    #[token("==")]
    Equals,

    #[token("!=")]
    NotEquals,

    #[token(">")]
    GreaterThan,

    #[token("<")]
    LessThan,

    #[token(">=")]
    GreaterThanOrEquals,

    #[token("<=")]
    LessThanOrEquals,

    // ---- Bitwise operators ----
    #[token("&")]
    BitwiseAnd,

    #[token("|")]
    BitwiseOr,

    #[token("^")]
    BitwiseXor,

    #[token("!")]
    BitwiseNot,

    #[token("<<")]
    LeftShift,

    #[token(">>")]
    RightShift,

    // ---- Logical operators ----
    #[token("and")]
    #[token("&&")]
    And,

    #[token("or")]
    #[token("||")]
    Or,

    #[token("not")]
    Not,

    // ---- Keywords ----
    #[token("const")]
    Const,
    #[token("let")]
    Let,
    #[token("global")]
    Global,
    #[token("extern")]
    Extern,

    /// The `fn` keyword, introduces a function definition.
    #[token("fn")]
    Fn,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("elif")]
    Elif,

    #[token("label")]
    Label,

    #[token("menu")]
    Menu,

    #[token("return")]
    Return,

    #[token("jump")]
    Jump,

    #[token("enum")]
    Enum,

    #[token("struct")]
    Struct,

    #[token("match")]
    Match,

    #[token("decorator")]
    DecoratorKw,

    #[token("import")]
    Import,

    #[token("from")]
    From,

    #[token("as")]
    As,

    #[token("_")]
    Wildcard,

    // ---- Other ----
    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token("@")]
    At,

    #[token("\n")]
    Newline,
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

/// Return a lexer for given source text
pub fn lex_src(src: &str) -> Lexer<'_, Token> {
    Token::lexer(src)
}

#[cfg(test)]
mod tests {

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
        assert!(matches!(lex("foo"), Token::IdentPath(s) if s == vec!["foo"]));
        assert!(matches!(lex("_bar"), Token::IdentPath(s) if s == vec!["_bar"]));
        assert!(matches!(lex("baz_123"), Token::IdentPath(s) if s == vec!["baz_123"]));
    }

    #[test]
    fn keywords() {
        assert!(matches!(lex("return"), Token::Return));
        assert!(matches!(lex("jump"), Token::Jump));
        assert!(matches!(lex("enum"), Token::Enum));
        assert!(matches!(lex("match"), Token::Match));
        assert!(matches!(lex("_"), Token::Wildcard));
    }

    #[test]
    fn end_bang_token() {
        assert!(matches!(lex("end!"), Token::EndBang));
        // Make sure "end" alone is still an IdentPath
        assert!(matches!(lex("end"), Token::IdentPath(_)));
    }

    #[test]
    fn todo_bang_token() {
        assert!(matches!(lex("todo!"), Token::TodoBang));
        assert!(matches!(lex("todo"), Token::IdentPath(_)));
    }

    #[test]
    fn decorator_keyword() {
        assert!(matches!(lex("decorator"), Token::DecoratorKw));
        // "decorators" (with extra chars) must still lex as IdentPath
        assert!(matches!(lex("decorators"), Token::IdentPath(_)));
    }

    #[test]
    fn extern_keyword() {
        assert!(matches!(lex("extern"), Token::Extern));
        assert!(matches!(lex("external"), Token::IdentPath(_)));
    }

    #[test]
    fn import_keyword() {
        assert!(matches!(lex("import"), Token::Import));
        // "imports" (with extra chars) must still lex as IdentPath
        assert!(matches!(lex("imports"), Token::IdentPath(_)));
    }

    #[test]
    fn as_keyword() {
        assert!(matches!(lex("as"), Token::As));
        // "asset" (with extra chars) must still lex as IdentPath
        assert!(matches!(lex("asset"), Token::IdentPath(_)));
    }

    #[test]
    fn from_keyword() {
        assert!(matches!(lex("from"), Token::From));
        // "fromage" (with extra chars) must still lex as IdentPath
        assert!(matches!(lex("fromage"), Token::IdentPath(_)));
    }

    #[test]
    fn struct_keyword() {
        assert!(matches!(lex("struct"), Token::Struct));
        // "structure" (with extra chars) must still lex as IdentPath
        assert!(matches!(lex("structure"), Token::IdentPath(_)));
    }

    #[test]
    fn operators() {
        assert!(matches!(lex("+"), Token::Plus));
        assert!(matches!(lex("-"), Token::Minus));
        assert!(matches!(lex("*"), Token::Star));
        assert!(matches!(lex("/"), Token::Slash));
        assert!(matches!(lex("//"), Token::DoubleSlash));
        assert!(matches!(lex("%"), Token::Percent));
        assert!(matches!(lex("="), Token::Assign));

        // Comparison operators
        assert!(matches!(lex("=="), Token::Equals));
        assert!(matches!(lex("!="), Token::NotEquals));
        assert!(matches!(lex(">"), Token::GreaterThan));
        assert!(matches!(lex("<"), Token::LessThan));
        assert!(matches!(lex(">="), Token::GreaterThanOrEquals));
        assert!(matches!(lex("<="), Token::LessThanOrEquals));

        // Bitwise operators
        assert!(matches!(lex("&"), Token::BitwiseAnd));
        assert!(matches!(lex("|"), Token::BitwiseOr));
        assert!(matches!(lex("^"), Token::BitwiseXor));
        assert!(matches!(lex("!"), Token::BitwiseNot));
        assert!(matches!(lex("<<"), Token::LeftShift));
        assert!(matches!(lex(">>"), Token::RightShift));

        // Logical operators
        assert!(matches!(lex("and"), Token::And));
        assert!(matches!(lex("&&"), Token::And));
        assert!(matches!(lex("or"), Token::Or));
        assert!(matches!(lex("||"), Token::Or));
    }

    #[test]
    fn line_comments() {
        // A standalone comment produces no tokens
        let tokens: Vec<_> = Token::lexer("# this is a comment").collect();
        assert!(tokens.is_empty());

        // A comment after code is ignored; only the token before it is emitted
        let tokens: Vec<_> = Token::lexer("42 # inline comment").collect::<Vec<_>>();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0], Ok(Token::IntLit(42))));

        // Code on the next line after a comment is still lexed correctly
        let tokens: Vec<_> = Token::lexer("# comment\n42")
            .filter(|t| !matches!(t, Ok(Token::Newline)))
            .collect();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0], Ok(Token::IntLit(42))));

        // A `#` inside a string literal is NOT treated as a comment
        let tok = lex(r#""hello # world""#);
        assert!(matches!(tok, Token::StrLit(_)));
    }

    #[test]
    fn doc_comments() {
        // A standalone ## comment produces a DocComment token with trimmed text
        let tok = lex("## hello world");
        assert!(matches!(tok, Token::DocComment(s) if s == "hello world"));

        // Leading/trailing whitespace after ## is trimmed
        let tok = lex("##   trimmed   ");
        assert!(matches!(tok, Token::DocComment(s) if s == "trimmed"));

        // An empty ## line yields an empty string
        let tok = lex("##");
        assert!(matches!(tok, Token::DocComment(s) if s.is_empty()));

        // A plain # is still skipped (not a doc comment)
        let tokens: Vec<_> = Token::lexer("# regular comment").collect();
        assert!(tokens.is_empty());

        // ## does NOT get swallowed by the # skip rule
        let tokens: Vec<_> = Token::lexer("## doc comment")
            .filter(|t| !matches!(t, Ok(Token::Newline)))
            .collect();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0], Ok(Token::DocComment(_))));

        // ## followed by code on the next line: DocComment then Newline then the code token
        let tokens: Vec<_> = Token::lexer("## doc\n42")
            .filter(|t| !matches!(t, Ok(Token::Newline)))
            .collect();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(&tokens[0], Ok(Token::DocComment(s)) if s == "doc"));
        assert!(matches!(&tokens[1], Ok(Token::IntLit(42))));

        // A # inside a string literal is still NOT treated as a comment or doc comment
        let tok = lex(r#""hello ## world""#);
        assert!(matches!(tok, Token::StrLit(_)));
    }
}
