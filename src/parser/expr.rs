//! # Expression Parser Module
//!
//! This module implements a Pratt parser for parsing expressions in the Urd language.
//! The Pratt parser technique is particularly well-suited for handling operator precedence
//! and associativity in expressions.
//!
//! The parser handles all operators defined in the Urd language, including:
//! - Arithmetic operators: +, -, *, /, //, %
//! - Comparison operators: ==, !=, >, <, >=, <=
//! - Bitwise operators: &, |, ^, !, <<, >>
//! - Logical operators: and/&&, or/||
//! - Assignment operator: =

use chumsky::pratt::*;
use chumsky::prelude::*;
use chumsky::{Parser, select};

use super::aliases::{BoxedUrdParser, UrdInput, UrdParser};
use super::ast::Ast;
use crate::lexer::Token;
use crate::parser::ast::{DeclKind, TypeAnnotation};
use crate::runtime::value::RuntimeValue;

/// Represents a value
pub fn atom<'tokens, I: UrdInput<'tokens>>() -> BoxedUrdParser<'tokens, I> {
    atom_internal(expr()).boxed()
}

/// Internal helper for `atom` that accepts a recursive expression parser.
///
/// This is necessary to prevent infinite recursion during parser construction.
/// If `atom` called `expr()` directly, and `expr()` calls `atom()`, it would cause
/// a stack overflow when building the parser. By passing the recursive parser
/// reference, we defer the recursion until parsing time.
fn atom_internal<'tokens, I: UrdInput<'tokens>>(
    expr: impl UrdParser<'tokens, I> + 'tokens,
) -> impl UrdParser<'tokens, I> {
    let list = expr
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
        .map(Ast::list);

    let map = comma_separated_kv_pairs_internal(expr.clone())
        .delimited_by(just(Token::DictStart), just(Token::RightCurly));

    let base = select! {
        Token::Null => Ast::value(RuntimeValue::Null),
        Token::BoolLit(b) => Ast::value(RuntimeValue::Bool(b)),
        Token::IntLit(i) => Ast::value(RuntimeValue::Int(i)),
        Token::FloatLit(f) => Ast::value(RuntimeValue::Float(f)),
        Token::StrLit(s) => Ast::value(RuntimeValue::Str(s)),
        Token::Dice((count, sides)) => Ast::value(RuntimeValue::Dice(count, sides)),
    }
    .labelled("value")
    .or(list)
    .or(map)
    .or(select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    }
    .then(
        comma_separated_exprs_internal(expr.clone())
            .boxed()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .or_not(),
    )
    .map(|(ident, maybe_params)| match maybe_params {
        Some(params) => Ast::call(ident, params),
        None => ident,
    }));

    // Postfix subscript: zero or more `[key]` suffixes, left-associative.
    // e.g. `obj[a][b]` → Subscript(Subscript(obj, a), b)
    let subscript_suffix = expr.delimited_by(just(Token::LeftBracket), just(Token::RightBracket));

    base.foldl(subscript_suffix.repeated(), |obj, key| {
        Ast::subscript(obj, key)
    })
}

/// Creates a Pratt parser for parsing expressions in the Urd language.
///
/// The parser handles all literals, identifiers, unary operators, binary operators,
/// and parenthesized expressions. Operator precedence is defined by the Pratt
/// parser configuration with higher numbers indicating higher precedence.
pub fn expr<'tokens, I: UrdInput<'tokens>>() -> BoxedUrdParser<'tokens, I> {
    recursive(|expr| {
        let term = atom_internal(expr.clone())
            .or(expr.delimited_by(just(Token::LeftParen), just(Token::RightParen)));

        term.pratt((
            // Unary operators (precedence 11)
            prefix(11, just(Token::BitwiseNot), |_, r, _| {
                Ast::bitwise_not_op(r)
            }),
            prefix(11, just(Token::Not), |_, r, _| Ast::not_op(r)),
            prefix(11, just(Token::Minus), |_, r, _| Ast::negate_op(r)),
            // Multiplication/division (precedence 10)
            infix(left(10), just(Token::Star), |l, _, r, _| {
                Ast::multiply_op(l, r)
            }),
            infix(left(10), just(Token::Slash), |l, _, r, _| {
                Ast::divide_op(l, r)
            }),
            infix(left(10), just(Token::DoubleSlash), |l, _, r, _| {
                Ast::floordiv_op(l, r)
            }),
            infix(left(10), just(Token::Percent), |l, _, r, _| {
                Ast::modulo_op(l, r)
            }),
            // Addition/subtraction (precedence 9)
            infix(left(9), just(Token::Plus), |l, _, r, _| Ast::add_op(l, r)),
            infix(left(9), just(Token::Minus), |l, _, r, _| {
                Ast::substract_op(l, r)
            }),
            // Bitwise shifts (precedence 8)
            infix(left(8), just(Token::LeftShift), |l, _, r, _| {
                Ast::left_shift_op(l, r)
            }),
            infix(left(8), just(Token::RightShift), |l, _, r, _| {
                Ast::right_shift_op(l, r)
            }),
            // Comparisons (precedence 7)
            infix(left(7), just(Token::GreaterThan), |l, _, r, _| {
                Ast::greater_than_op(l, r)
            }),
            infix(left(7), just(Token::LessThan), |l, _, r, _| {
                Ast::less_than_op(l, r)
            }),
            infix(left(7), just(Token::GreaterThanOrEquals), |l, _, r, _| {
                Ast::greater_than_or_equals_op(l, r)
            }),
            infix(left(7), just(Token::LessThanOrEquals), |l, _, r, _| {
                Ast::less_than_or_equals_op(l, r)
            }),
            // Equality (precedence 6)
            infix(left(6), just(Token::Equals), |l, _, r, _| {
                Ast::equals_op(l, r)
            }),
            infix(left(6), just(Token::NotEquals), |l, _, r, _| {
                Ast::not_equals_op(l, r)
            }),
            // Bitwise AND (precedence 5)
            infix(left(5), just(Token::BitwiseAnd), |l, _, r, _| {
                Ast::bitwise_and_op(l, r)
            }),
            // Bitwise XOR (precedence 4)
            infix(left(4), just(Token::BitwiseXor), |l, _, r, _| {
                Ast::bitwise_xor_op(l, r)
            }),
            // Bitwise OR (precedence 3)
            infix(left(3), just(Token::BitwiseOr), |l, _, r, _| {
                Ast::bitwise_or_op(l, r)
            }),
            // Logical AND (precedence 2)
            infix(left(2), just(Token::And), |l, _, r, _| Ast::and_op(l, r)),
            // Logical OR (precedence 1)
            infix(left(1), just(Token::Or), |l, _, r, _| Ast::or_op(l, r)),
            // Assignment (right-associative, lowest precedence 0)
            infix(right(0), just(Token::Assign), |l, _, r, _| {
                Ast::assign_op(l, r)
            }),
        ))
    })
    .labelled("expression")
    .boxed()
}

/// Parser for comma-separated list of expressions. Typically used for list construction and
/// function calls.
pub fn comma_separated_exprs<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    comma_separated_exprs_internal(expr()).boxed()
}

/// Internal helper for `comma_separated_exprs` that accepts a recursive expression parser.
///
/// See `atom_internal` for details on why this is needed to prevent stack overflow.
fn comma_separated_exprs_internal<'tok, I: UrdInput<'tok>>(
    expr: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    expr.separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(Ast::expr_list)
}

fn comma_separated_kv_pairs_internal<'tok, I: UrdInput<'tok>>(
    expr: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    expr.clone()
        .then_ignore(just(Token::Colon))
        .then(expr)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(Ast::map)
}

/// Parser for an optional static type annotation: `: TypeName`.
///
/// Recognises the built-in primitive names (`int`, `float`, `bool`, `str`, `null`)
/// as well as any user-defined identifier path (e.g. `Direction`, `my_mod.Color`).
///
/// This function returns a raw [`Parser`] rather than the [`UrdParser`] alias because its
/// output type is [`TypeAnnotation`], not [`Ast`].
///
/// Visibility is `pub(crate)` so that `block.rs` can reuse this parser for decorator
/// parameter type annotations without duplicating the logic.
pub(crate) fn type_annotation<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    TypeAnnotation,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    just(Token::Colon).ignore_then(
        select! {
            // `null` is a keyword token in the lexer
            Token::Null => TypeAnnotation::Null,
            // Primitive type names arrive as plain IdentPath tokens
            Token::IdentPath(path) if path == ["int"]   => TypeAnnotation::Int,
            Token::IdentPath(path) if path == ["float"] => TypeAnnotation::Float,
            Token::IdentPath(path) if path == ["bool"]  => TypeAnnotation::Bool,
            Token::IdentPath(path) if path == ["str"]   => TypeAnnotation::Str,
            Token::IdentPath(path) if path == ["list"]  => TypeAnnotation::List,
            Token::IdentPath(path) if path == ["map"]   => TypeAnnotation::Map,
            Token::IdentPath(path) if path == ["dice"]  => TypeAnnotation::Dice,
            // `label` is a keyword token, not an IdentPath, so it needs its own arm
            Token::Label => TypeAnnotation::Label,
            // Any other identifier path is a user-defined (named) type
            Token::IdentPath(path) => TypeAnnotation::Named(path),
        }
        .labelled("type name"),
    )
}

/// Parser for variable/constant declarations. Parses:
///
/// - `const ident = expr`
/// - `let ident = expr`
/// - `global ident = expr`
///
/// An optional type annotation between the name and `=` is also accepted:
///
/// - `let x: int = 5`
/// - `const msg: str = "hello"`
/// - `global flag: bool = true`
/// - `let dir: Direction = Direction.North`
/// - `let coord: my_mod.Point = make_point(1, 2)`
pub fn declaration<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    let decl_word = select! {
        Token::Const => DeclKind::Constant,
        Token::Let => DeclKind::Variable,
        Token::Global => DeclKind::Global
    };

    let ident = select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    };

    decl_word
        .then(ident)
        .then(type_annotation().or_not())
        .then_ignore(just(Token::Assign))
        .then(expr())
        .map(|(((decl, name), annotation), def)| match annotation {
            Some(ann) => Ast::typed_decl(decl, name, ann, def),
            None => Ast::decl(decl, name, def),
        })
        .labelled("declaration")
        .boxed()
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)]

    use super::*;
    use crate::parse_test;

    /// Helper: parse a type annotation from source text (e.g. `": int"`).
    fn parse_type_annotation(src: &str) -> Result<TypeAnnotation, ()> {
        parse_test!(type_annotation(), src).map_err(|_| ())
    }

    #[test]
    fn test_type_annotation_int() {
        assert_eq!(parse_type_annotation(": int"), Ok(TypeAnnotation::Int));
    }

    #[test]
    fn test_type_annotation_float() {
        assert_eq!(parse_type_annotation(": float"), Ok(TypeAnnotation::Float));
    }

    #[test]
    fn test_type_annotation_bool() {
        assert_eq!(parse_type_annotation(": bool"), Ok(TypeAnnotation::Bool));
    }

    #[test]
    fn test_type_annotation_str() {
        assert_eq!(parse_type_annotation(": str"), Ok(TypeAnnotation::Str));
    }

    #[test]
    fn test_type_annotation_null() {
        assert_eq!(parse_type_annotation(": null"), Ok(TypeAnnotation::Null));
    }

    #[test]
    fn test_type_annotation_list() {
        assert_eq!(parse_type_annotation(": list"), Ok(TypeAnnotation::List));
    }

    #[test]
    fn test_type_annotation_map() {
        assert_eq!(parse_type_annotation(": map"), Ok(TypeAnnotation::Map));
    }

    #[test]
    fn test_type_annotation_dice() {
        assert_eq!(parse_type_annotation(": dice"), Ok(TypeAnnotation::Dice));
    }

    /// `label` is a keyword token (not an `IdentPath`), so it needs its own
    /// dedicated arm in the `select!` macro.  This test guards that arm.
    #[test]
    fn test_type_annotation_label() {
        assert_eq!(parse_type_annotation(": label"), Ok(TypeAnnotation::Label));
    }

    #[test]
    fn test_type_annotation_named_single() {
        assert_eq!(
            parse_type_annotation(": Direction"),
            Ok(TypeAnnotation::Named(vec!["Direction".to_string()]))
        );
    }

    #[test]
    fn test_type_annotation_named_path() {
        assert_eq!(
            parse_type_annotation(": my_mod.Color"),
            Ok(TypeAnnotation::Named(vec![
                "my_mod".to_string(),
                "Color".to_string()
            ]))
        );
    }

    #[test]
    fn test_type_annotation_missing_colon_fails() {
        assert!(parse_type_annotation("int").is_err());
    }

    #[test]
    fn test_type_annotation_bare_colon_fails() {
        assert!(parse_type_annotation(":").is_err());
    }
}
