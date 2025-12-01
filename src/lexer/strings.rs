use super::Result;
use crate::erro::LexerError;
use logos::{Lexer, Logos};

/// Interpolation details
#[derive(Debug, Clone, PartialEq)]
pub struct Interpolation {
    pub path: String,
    pub format: Option<String>,
}

/// Part of a string
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexerError)]
pub enum StringPart {
    #[token("\"")]
    ExitString,
    #[regex(
        r#"\\[ntr\\"{}]|\\x[0-9a-fA-F]{2}|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]{1,6}\}"#,
        escape_parsing_callback
    )]
    EscapedChar(String),

    #[regex(r#"([^\\{}"]+)"#, |lex| lex.slice().to_string())]
    Literal(String),

    /// Interpolated variable reference
    #[regex(r#"\{[^{}"]+\}"#, interpolation_parsing_callback)]
    Interpolation(Interpolation),
}

/// String parsed from code
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ParsedString(Vec<StringPart>);

impl ParsedString {
    /// New empty parsed string
    pub fn new() -> Self {
        ParsedString::default()
    }

    /// New string from it's parts
    pub fn new_from_parts(parts: Vec<StringPart>) -> Self {
        ParsedString(parts)
    }

    /// New plain parsed string
    pub fn new_plain(s: &str) -> Self {
        ParsedString::new_from_parts(vec![StringPart::Literal(s.into())])
    }
}

/// Callback for logos to parse strings.
/// Morphs lexer into string lexer, and then back after string parsing is finished.
pub fn string_parsing_callback<'a, T>(lex: &mut Lexer<'a, T>) -> Result<ParsedString>
where
    T: Logos<'a, Source = str, Extras = ()> + Clone,
    T::Extras: Clone,
{
    let mut string_lexer = lex.clone().morph::<StringPart>();
    let mut tokens = vec![];

    for token_or_error in string_lexer.by_ref() {
        match token_or_error {
            Ok(tok) => {
                use StringPart::*;
                match tok {
                    ExitString => break,
                    x => tokens.push(x),
                }
            }
            Err(e) => {
                return Err(e);
            }
        }
    }

    *lex = string_lexer.morph();
    Ok(ParsedString::new_from_parts(tokens))
}

/// Callback for logos to parse escaped characters.
/// Parses \n, \r, \t, \", \\, \{, \}, \xHH, \uHHHH and \u{HHHH}
fn escape_parsing_callback<'a>(lex: &mut Lexer<'a, StringPart>) -> Result<String> {
    let s = lex.slice();
    match s {
        "\\n" => Ok("\n".to_string()),
        "\\r" => Ok("\r".to_string()),
        "\\t" => Ok("\t".to_string()),
        "\\\"" => Ok("\"".to_string()),
        "\\{" => Ok("{".to_string()),
        "\\}" => Ok("}".to_string()),
        "\\\\" => Ok("\\".to_string()),
        _ if s.starts_with("\\x") => {
            let code_str = &s[2..];
            let code = u8::from_str_radix(code_str, 16)
                .map_err(|_| LexerError::InvalidEscape(code_str.to_string()))?;
            Ok((code as char).to_string())
        }
        _ if s.starts_with("\\u") => {
            let code_str = if s.starts_with("\\u{") {
                &s[3..s.len() - 1]
            } else {
                &s[2..]
            };
            let code = u32::from_str_radix(code_str, 16)
                .map_err(|_| LexerError::InvalidEscape(code_str.to_string()))?;
            char::from_u32(code)
                .map(|c| c.to_string())
                .ok_or(LexerError::InvalidEscape(code.to_string()))
        }
        _ => unreachable!("Regex for EscapedChar should cover all matched cases"),
    }
}

/// Parses string interpolation variables, e.g. {variable.path:format}
fn interpolation_parsing_callback<'a>(lex: &mut Lexer<'a, StringPart>) -> Result<Interpolation> {
    let slice = lex.slice();
    let inner_content = slice.trim_matches(|c| c == '{' || c == '}').trim();

    if inner_content.is_empty() {
        return Err(LexerError::Interpolation);
    }

    let (path_str, format) = if let Some((p, f)) = inner_content.split_once(':') {
        (p.trim(), Some(f.trim().to_string()))
    } else {
        (inner_content, None)
    };

    if !is_valid_path(path_str) {
        return Err(LexerError::Interpolation);
    }

    Ok(Interpolation {
        path: path_str.to_string(),
        format,
    })
}

fn is_valid_path(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    for part in s.split('.') {
        if part.is_empty() {
            return false;
        }
        let mut chars = part.chars();
        if let Some(first) = chars.next() {
            if !first.is_ascii_alphabetic() && first != '_' {
                return false;
            }
            for c in chars {
                if !c.is_ascii_alphanumeric() && c != '_' {
                    return false;
                }
            }
        } else {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::erro::LexerError;

    use super::ParsedString;

    #[derive(Logos, Debug, Clone)]
    #[logos(error = LexerError)]
    enum TestToken {
        #[token("\"", super::string_parsing_callback)]
        StringLit(ParsedString),
    }

    use super::{Interpolation, StringPart};

    fn parse(input: &str) -> ParsedString {
        let mut lex = TestToken::lexer(input);
        match lex.next() {
            Some(Ok(TestToken::StringLit(s))) => s,
            _ => panic!("Failed to parse string literal"),
        }
    }

    fn assert_parts(s: ParsedString, expected: &[StringPart]) {
        let expected_debug = ParsedString::new_from_parts(expected.to_vec());
        assert_eq!(s, expected_debug);
    }

    #[test]
    fn test_plain_string() {
        let s = parse(r#""hello world""#);
        assert_parts(s, &[StringPart::Literal("hello world".to_string())]);
    }

    #[test]
    fn test_escapes() {
        let s = parse(r#""\n\t\r\"\\\{\}""#);
        assert_parts(
            s,
            &[
                StringPart::EscapedChar("\n".to_string()),
                StringPart::EscapedChar("\t".to_string()),
                StringPart::EscapedChar("\r".to_string()),
                StringPart::EscapedChar("\"".to_string()),
                StringPart::EscapedChar("\\".to_string()),
                StringPart::EscapedChar("{".to_string()),
                StringPart::EscapedChar("}".to_string()),
            ],
        );
    }

    #[test]
    fn test_unicode_escapes() {
        let s = parse(r#""\x41\u0042\u{1F600}""#);
        assert_parts(
            s,
            &[
                StringPart::EscapedChar("A".to_string()),
                StringPart::EscapedChar("B".to_string()),
                StringPart::EscapedChar("😀".to_string()),
            ],
        );
    }

    #[test]
    fn test_interpolation() {
        let s = parse(r#""hello {name}!""#);
        assert_parts(
            s,
            &[
                StringPart::Literal("hello ".to_string()),
                StringPart::Interpolation(Interpolation {
                    path: "name".to_string(),
                    format: None,
                }),
                StringPart::Literal("!".to_string()),
            ],
        );
    }

    #[test]
    fn test_interpolation_spacing() {
        let s = parse(r#""{  foo  }""#);
        assert_parts(
            s,
            &[StringPart::Interpolation(Interpolation {
                path: "foo".to_string(),
                format: None,
            })],
        );
    }

    #[test]
    fn test_interpolation_path() {
        let s = parse(r#""{user.name}""#);
        assert_parts(
            s,
            &[StringPart::Interpolation(Interpolation {
                path: "user.name".to_string(),
                format: None,
            })],
        );
    }

    #[test]
    fn test_interpolation_format() {
        let s = parse(r#""{val:02}""#);
        assert_parts(
            s,
            &[StringPart::Interpolation(Interpolation {
                path: "val".to_string(),
                format: Some("02".to_string()),
            })],
        );
    }

    #[test]
    fn test_interpolation_path_and_format() {
        let s = parse(r#""{user.age : 03}""#);
        assert_parts(
            s,
            &[StringPart::Interpolation(Interpolation {
                path: "user.age".to_string(),
                format: Some("03".to_string()),
            })],
        );
    }

    #[test]
    fn test_complex_mix() {
        let s = parse(r#""a\nb{c}d""#);
        assert_parts(
            s,
            &[
                StringPart::Literal("a".to_string()),
                StringPart::EscapedChar("\n".to_string()),
                StringPart::Literal("b".to_string()),
                StringPart::Interpolation(Interpolation {
                    path: "c".to_string(),
                    format: None,
                }),
                StringPart::Literal("d".to_string()),
            ],
        );
    }

    #[test]
    fn test_empty_interpolation() {
        let mut lex = TestToken::lexer(r#""hello {}world""#);
        assert!(matches!(lex.next(), Some(Err(_))));

        let mut lex = TestToken::lexer(r#""hello {   }world""#);
        assert!(matches!(lex.next(), Some(Err(_))));
    }
}
