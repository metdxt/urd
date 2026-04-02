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
use crate::parser::block::{code_block, fn_params_parser, return_type_parser};
use crate::runtime::value::RuntimeValue;

/// Represents a value, including anonymous function literals.
///
/// Anonymous function literals have the form
/// `fn(param: Type, ...) -> RetType { body }` and produce a
/// [`crate::parser::ast::AstContent::FnDef`] node with `name: None`.
///
/// # Construction safety
///
/// `code_block()` is called here (outside any `recursive` closure) so there
/// is no construction-time mutual-recursion cycle between this function and
/// the block parser.
pub fn atom<'tokens, I: UrdInput<'tokens>>() -> BoxedUrdParser<'tokens, I> {
    // Anonymous function literal: `fn(params) [-> RetType] { body }`
    // Must be tried before the generic atom so the `fn` keyword is not
    // consumed as an identifier.
    let anon_fn = just(Token::Fn)
        .ignore_then(fn_params_parser())
        .then(return_type_parser().or_not())
        .then(code_block())
        .map_with(|((params, ret_type), body), extra| {
            Ast::fn_def(None, params, ret_type, body).with_span(extra.span())
        });

    anon_fn.or(atom_internal(expr())).boxed()
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
        .map_with(|items, extra| Ast::list(items).with_span(extra.span()));

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
    .map_with(|ast, extra| ast.with_span(extra.span()))
    .labelled("value")
    .or(list)
    .or(map)
    .or(select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    }
    .map_with(|ast, extra| ast.with_span(extra.span()))
    .then(
        comma_separated_exprs_internal(expr.clone())
            .boxed()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .or_not(),
    )
    .map_with(|(ident, maybe_params), extra| {
        let span = extra.span();
        match maybe_params {
            Some(params) => Ast::call(ident, params).with_span(span),
            None => ident,
        }
    }))
    .or(
        // `end!` and `todo!` are keyword-like terminators that look like macro
        // calls.  The lexer emits them as single tokens (EndBang / TodoBang),
        // so we handle them here and produce a Call AST node directly.
        // Both bare form (`end!`) and parenthesised form (`end!()`) are accepted.
        select! {
            Token::EndBang  => "end!",
            Token::TodoBang => "todo!",
        }
        .then_ignore(
            just(Token::LeftParen)
                .then_ignore(just(Token::RightParen))
                .or_not(),
        )
        .map_with(|name, extra| {
            let span = extra.span();
            let func = Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]));
            Ast::call(func, Ast::expr_list(vec![])).with_span(span)
        }),
    );

    // Postfix subscript: zero or more `[key]` suffixes, left-associative.
    // e.g. `obj[a][b]` → Subscript(Subscript(obj, a), b)
    let subscript_suffix = expr.delimited_by(just(Token::LeftBracket), just(Token::RightBracket));

    base.foldl(subscript_suffix.repeated(), |obj, key| {
        Ast::subscript(obj, key)
    })
    .map_with(|ast, extra| ast.with_span(extra.span()))
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
                Ast::subtract_op(l, r)
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
        .map_with(|ast, extra| ast.with_span(extra.span()))
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
        .map_with(|exprs, extra| Ast::expr_list(exprs).with_span(extra.span()))
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
        .map_with(|items, extra| Ast::map(items).with_span(extra.span()))
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
        Token::IdentPath(path) if path.len() == 1 => Ast::value(RuntimeValue::IdentPath(path))
    }
    .labelled("variable name");

    decl_word
        .then(ident)
        .then(type_annotation().or_not())
        .then_ignore(just(Token::Assign))
        .then(expr())
        .map_with(|(((decl, name), annotation), def), extra| {
            let span = extra.span();
            match annotation {
                Some(ann) => Ast::typed_decl(decl, name, ann, def).with_span(span),
                None => Ast::decl(decl, name, def).with_span(span),
            }
        })
        .labelled("declaration")
        .boxed()
}

/// Parser for extern declarations. Parses:
///
/// - `extern const ident`
/// - `extern global ident`
/// - `extern const ident: Type`
/// - `extern global ident: Type`
///
/// No initialiser is parsed — the value is provided by the host runtime.
pub fn extern_declaration<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    let ident = select! {
        Token::IdentPath(path) if path.len() == 1 => Ast::value(RuntimeValue::IdentPath(path))
    }
    .labelled("variable name");

    just(Token::Extern)
        .ignore_then(ident)
        .then(type_annotation().or_not())
        .map_with(|(name, annotation), extra| {
            Ast::extern_decl(name, annotation).with_span(extra.span())
        })
        .labelled("extern declaration")
        .boxed()
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_test;
    use crate::parser::ast::AstContent;

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
    fn test_end_bang_parses() {
        use crate::parser::ast::AstContent;
        let result = parse_test!(expr(), "end!()");
        assert!(result.is_ok(), "end!() should parse: {:?}", result);
        let ast = result.unwrap();
        assert!(
            matches!(ast.content(), AstContent::Call { .. }),
            "expected Call node, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn test_end_bang_bare_parses() {
        use crate::parser::ast::AstContent;
        let result = parse_test!(expr(), "end!");
        assert!(result.is_ok(), "end! (bare) should parse: {:?}", result);
        let ast = result.unwrap();
        assert!(
            matches!(ast.content(), AstContent::Call { .. }),
            "expected Call node, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn test_todo_bang_parses() {
        use crate::parser::ast::AstContent;
        let result = parse_test!(expr(), "todo!()");
        assert!(result.is_ok(), "todo!() should parse: {:?}", result);
        let ast = result.unwrap();
        assert!(
            matches!(ast.content(), AstContent::Call { .. }),
            "expected Call node, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn test_todo_bang_bare_parses() {
        let result = parse_test!(expr(), "todo!");
        assert!(result.is_ok(), "todo! (bare) should parse: {:?}", result);
    }

    #[test]
    fn test_type_annotation_missing_colon_fails() {
        assert!(parse_type_annotation("int").is_err());
    }

    #[test]
    fn test_type_annotation_bare_colon_fails() {
        assert!(parse_type_annotation(":").is_err());
    }

    #[test]
    fn test_extern_no_annotation() {
        use crate::parser::ast::AstContent;
        let result = parse_test!(extern_declaration(), "extern narrator");
        assert!(
            result.is_ok(),
            "extern without type should parse: {:?}",
            result
        );
        let ast = result.unwrap();
        assert!(matches!(
            ast.content(),
            AstContent::ExternDeclaration {
                type_annotation: None,
                ..
            }
        ));
    }

    #[test]
    fn test_extern_with_annotation() {
        use crate::parser::ast::{AstContent, TypeAnnotation};
        let result = parse_test!(extern_declaration(), "extern score: int");
        assert!(
            result.is_ok(),
            "extern with type should parse: {:?}",
            result
        );
        let ast = result.unwrap();
        assert!(matches!(
            ast.content(),
            AstContent::ExternDeclaration {
                type_annotation: Some(TypeAnnotation::Int),
                ..
            }
        ));
    }

    #[test]
    fn test_extern_named_type() {
        use crate::parser::ast::{AstContent, TypeAnnotation};
        let result = parse_test!(extern_declaration(), "extern narrator: Character");
        assert!(
            result.is_ok(),
            "extern with named type should parse: {:?}",
            result
        );
        let ast = result.unwrap();
        assert!(matches!(
            ast.content(),
            AstContent::ExternDeclaration {
                type_annotation: Some(TypeAnnotation::Named(_)),
                ..
            }
        ));
    }

    #[test]
    fn test_extern_const_fails() {
        // `extern const` is no longer valid syntax — kind specifiers are gone
        let result = parse_test!(extern_declaration(), "extern const narrator");
        assert!(result.is_err(), "extern const should not parse");
    }

    #[test]
    fn test_extern_global_fails() {
        // `extern global` is no longer valid syntax — kind specifiers are gone
        let result = parse_test!(extern_declaration(), "extern global score");
        assert!(result.is_err(), "extern global should not parse");
    }

    /// PAR-1: anonymous function literal in expression (atom) position must
    /// produce a `FnDef` node with `name: None`.
    #[test]
    fn test_anon_fn_in_expression_position() {
        let result = parse_test!(atom(), "fn(x: int) -> int { return x }");
        assert!(
            result.is_ok(),
            "anonymous fn should parse in expression position: {result:?}"
        );
        let ast = result.unwrap();
        match ast.content() {
            AstContent::FnDef {
                name,
                params,
                ret_type,
                ..
            } => {
                assert_eq!(*name, None, "anonymous fn must have name=None");
                assert_eq!(params.len(), 1, "expected exactly 1 parameter");
                assert_eq!(params[0].name, "x");
                assert_eq!(
                    params[0].type_annotation,
                    Some(TypeAnnotation::Int),
                    "parameter type should be int"
                );
                assert_eq!(
                    *ret_type,
                    Some(TypeAnnotation::Int),
                    "return type should be int"
                );
            }
            other => panic!("expected AstContent::FnDef, got {:?}", other),
        }
    }
}
