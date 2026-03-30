#![allow(clippy::unwrap_used)]

use urd::{
    lexer::strings::{ParsedString, StringPart},
    parse_test,
    parser::{
        ast::{Ast, DeclKind, Operator, TypeAnnotation, UnaryOperator},
        expr::{comma_separated_exprs, declaration, expr},
    },
    runtime::value::RuntimeValue,
};

#[test]
#[allow(clippy::approx_constant)]
fn test_literals() {
    assert_eq!(
        parse_test!(expr(), "42"),
        Ok(Ast::value(RuntimeValue::Int(42)))
    );

    assert_eq!(
        parse_test!(expr(), "3.14"),
        Ok(Ast::value(RuntimeValue::Float(3.14)))
    );

    assert_eq!(
        parse_test!(expr(), "true"),
        Ok(Ast::value(RuntimeValue::Bool(true)))
    );

    assert_eq!(
        parse_test!(expr(), "false"),
        Ok(Ast::value(RuntimeValue::Bool(false)))
    );

    assert_eq!(
        parse_test!(expr(), "null"),
        Ok(Ast::value(RuntimeValue::Null))
    );

    assert_eq!(
        parse_test!(expr(), "\"hello\""),
        Ok(Ast::value(RuntimeValue::Str(ParsedString::new_plain(
            "hello"
        ))))
    );

    assert_eq!(
        parse_test!(expr(), "2d6"),
        Ok(Ast::value(RuntimeValue::Dice(2, 6)))
    );

    assert_eq!(
        parse_test!(expr(), "x"),
        Ok(Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])))
    );
}

#[test]
fn test_parenthesized_expressions() {
    assert_eq!(
        parse_test!(expr(), "(1)"),
        Ok(Ast::value(RuntimeValue::Int(1)))
    );

    assert_eq!(
        parse_test!(expr(), "(1 + 2)"),
        Ok(Ast::binop(
            Operator::Plus,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );
}

#[test]
fn test_unary_operators() {
    assert_eq!(
        parse_test!(expr(), "-42"),
        Ok(Ast::unary(
            UnaryOperator::Negate,
            Ast::value(RuntimeValue::Int(42))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "not true"),
        Ok(Ast::unary(
            UnaryOperator::Not,
            Ast::value(RuntimeValue::Bool(true))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "!1"),
        Ok(Ast::unary(
            UnaryOperator::BitwiseNot,
            Ast::value(RuntimeValue::Int(1))
        ))
    );
}

#[test]
fn test_arithmetic_operators() {
    assert_eq!(
        parse_test!(expr(), "1 + 2"),
        Ok(Ast::binop(
            Operator::Plus,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "3 - 1"),
        Ok(Ast::binop(
            Operator::Minus,
            Ast::value(RuntimeValue::Int(3)),
            Ast::value(RuntimeValue::Int(1))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "4 * 5"),
        Ok(Ast::binop(
            Operator::Multiply,
            Ast::value(RuntimeValue::Int(4)),
            Ast::value(RuntimeValue::Int(5))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "10 / 2"),
        Ok(Ast::binop(
            Operator::Divide,
            Ast::value(RuntimeValue::Int(10)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "10 // 3"),
        Ok(Ast::binop(
            Operator::DoubleSlash,
            Ast::value(RuntimeValue::Int(10)),
            Ast::value(RuntimeValue::Int(3))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "10 % 3"),
        Ok(Ast::binop(
            Operator::Percent,
            Ast::value(RuntimeValue::Int(10)),
            Ast::value(RuntimeValue::Int(3))
        ))
    );
}

#[test]
fn test_comparison_operators() {
    assert_eq!(
        parse_test!(expr(), "1 == 2"),
        Ok(Ast::binop(
            Operator::Equals,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 != 2"),
        Ok(Ast::binop(
            Operator::NotEquals,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 > 2"),
        Ok(Ast::binop(
            Operator::GreaterThan,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 < 2"),
        Ok(Ast::binop(
            Operator::LessThan,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 >= 2"),
        Ok(Ast::binop(
            Operator::GreaterThanOrEquals,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 <= 2"),
        Ok(Ast::binop(
            Operator::LessThanOrEquals,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );
}

#[test]
fn test_bitwise_operators() {
    assert_eq!(
        parse_test!(expr(), "1 & 2"),
        Ok(Ast::binop(
            Operator::BitwiseAnd,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 | 2"),
        Ok(Ast::binop(
            Operator::BitwiseOr,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 ^ 2"),
        Ok(Ast::binop(
            Operator::BitwiseXor,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 << 2"),
        Ok(Ast::binop(
            Operator::LeftShift,
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "8 >> 2"),
        Ok(Ast::binop(
            Operator::RightShift,
            Ast::value(RuntimeValue::Int(8)),
            Ast::value(RuntimeValue::Int(2))
        ))
    );
}

#[test]
fn test_logical_operators() {
    assert_eq!(
        parse_test!(expr(), "true and false"),
        Ok(Ast::binop(
            Operator::And,
            Ast::value(RuntimeValue::Bool(true)),
            Ast::value(RuntimeValue::Bool(false))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "true or false"),
        Ok(Ast::binop(
            Operator::Or,
            Ast::value(RuntimeValue::Bool(true)),
            Ast::value(RuntimeValue::Bool(false))
        ))
    );
}

#[test]
fn test_assignment_operator() {
    assert_eq!(
        parse_test!(expr(), "x = 42"),
        Ok(Ast::binop(
            Operator::Assign,
            Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
            Ast::value(RuntimeValue::Int(42))
        ))
    );
}

#[test]
fn test_operator_precedence() {
    assert_eq!(
        parse_test!(expr(), "1 + 2 * 3"),
        Ok(Ast::binop(
            Operator::Plus,
            Ast::value(RuntimeValue::Int(1)),
            Ast::binop(
                Operator::Multiply,
                Ast::value(RuntimeValue::Int(2)),
                Ast::value(RuntimeValue::Int(3))
            )
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 + 2 == 3"),
        Ok(Ast::binop(
            Operator::Equals,
            Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ),
            Ast::value(RuntimeValue::Int(3))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "x = 1 + 2 * 3"),
        Ok(Ast::binop(
            Operator::Assign,
            Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
            Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::binop(
                    Operator::Multiply,
                    Ast::value(RuntimeValue::Int(2)),
                    Ast::value(RuntimeValue::Int(3))
                )
            )
        ))
    );
}

#[test]
fn test_associativity() {
    assert_eq!(
        parse_test!(expr(), "1 - 2 - 3"),
        Ok(Ast::binop(
            Operator::Minus,
            Ast::binop(
                Operator::Minus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ),
            Ast::value(RuntimeValue::Int(3))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "x = y = 1"),
        Ok(Ast::binop(
            Operator::Assign,
            Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
            Ast::binop(
                Operator::Assign,
                Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            )
        ))
    );
}

#[test]
fn test_complex_expressions() {
    assert_eq!(
        parse_test!(expr(), "(1 + 2) * (3 - 4) / 5"),
        Ok(Ast::binop(
            Operator::Divide,
            Ast::binop(
                Operator::Multiply,
                Ast::binop(
                    Operator::Plus,
                    Ast::value(RuntimeValue::Int(1)),
                    Ast::value(RuntimeValue::Int(2))
                ),
                Ast::binop(
                    Operator::Minus,
                    Ast::value(RuntimeValue::Int(3)),
                    Ast::value(RuntimeValue::Int(4))
                )
            ),
            Ast::value(RuntimeValue::Int(5))
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 < 2 and 3 > 4"),
        Ok(Ast::binop(
            Operator::And,
            Ast::binop(
                Operator::LessThan,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ),
            Ast::binop(
                Operator::GreaterThan,
                Ast::value(RuntimeValue::Int(3)),
                Ast::value(RuntimeValue::Int(4))
            )
        ))
    );

    assert_eq!(
        parse_test!(expr(), "1 < 2 == 2 < 3"),
        Ok(Ast::binop(
            Operator::Equals,
            Ast::binop(
                Operator::LessThan,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ),
            Ast::binop(
                Operator::LessThan,
                Ast::value(RuntimeValue::Int(2)),
                Ast::value(RuntimeValue::Int(3))
            )
        ))
    );
}

#[test]
fn test_error_handling() {
    assert!(parse_test!(expr(), "1 +").is_err());
    assert!(parse_test!(expr(), "(1 + 2").is_err());
    assert!(parse_test!(expr(), "1 + 2)").is_err());
    assert!(parse_test!(expr(), "1 + + 2").is_err());
    assert!(parse_test!(expr(), "").is_err());
}

#[test]
fn test_const_declarations() {
    let expected = Ast::decl(
        DeclKind::Constant,
        Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
        Ast::value(RuntimeValue::Int(42)),
    );
    assert_eq!(
        parse_test!(declaration(), "const x = 42").unwrap(),
        expected
    );

    let expr_node = Ast::binop(
        Operator::Plus,
        Ast::value(RuntimeValue::Int(1)),
        Ast::binop(
            Operator::Multiply,
            Ast::value(RuntimeValue::Int(2)),
            Ast::value(RuntimeValue::Int(3)),
        ),
    );
    let expected = Ast::decl(
        DeclKind::Constant,
        Ast::value(RuntimeValue::IdentPath(vec!["result".to_string()])),
        expr_node,
    );
    assert_eq!(
        parse_test!(declaration(), "const result = 1 + 2 * 3").unwrap(),
        expected
    );
}

#[test]
fn test_let_declarations() {
    let expected = Ast::decl(
        DeclKind::Variable,
        Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
        Ast::value(RuntimeValue::Int(42)),
    );
    assert_eq!(parse_test!(declaration(), "let y = 42").unwrap(), expected);

    let expected = Ast::decl(
        DeclKind::Variable,
        Ast::value(RuntimeValue::IdentPath(vec!["name".to_string()])),
        Ast::value(RuntimeValue::Str(ParsedString::new_from_parts(vec![
            StringPart::Literal("test".to_string()),
        ]))),
    );
    assert_eq!(
        parse_test!(declaration(), "let name = \"test\"").unwrap(),
        expected
    );
}

#[test]
fn test_global_declarations() {
    let expected = Ast::decl(
        DeclKind::Global,
        Ast::value(RuntimeValue::IdentPath(vec!["counter".to_string()])),
        Ast::value(RuntimeValue::Int(0)),
    );
    assert_eq!(
        parse_test!(declaration(), "global counter = 0").unwrap(),
        expected
    );

    let expr_node = Ast::binop(
        Operator::Plus,
        Ast::value(RuntimeValue::Int(1)),
        Ast::value(RuntimeValue::Int(2)),
    );
    let expected = Ast::decl(
        DeclKind::Global,
        Ast::value(RuntimeValue::IdentPath(vec!["config".to_string()])),
        expr_node,
    );
    assert_eq!(
        parse_test!(declaration(), "global config = 1 + 2").unwrap(),
        expected
    );
}

#[test]
fn test_declaration_with_parenthesized_expression() {
    let inner_expr = Ast::binop(
        Operator::Plus,
        Ast::value(RuntimeValue::Int(1)),
        Ast::value(RuntimeValue::Int(2)),
    );
    let expr_node = Ast::binop(
        Operator::Multiply,
        inner_expr,
        Ast::value(RuntimeValue::Int(3)),
    );
    let expected = Ast::decl(
        DeclKind::Constant,
        Ast::value(RuntimeValue::IdentPath(vec!["value".to_string()])),
        expr_node,
    );
    assert_eq!(
        parse_test!(declaration(), "const value = (1 + 2) * 3").unwrap(),
        expected
    );
}

#[test]
fn test_declaration_error_handling() {
    assert!(parse_test!(declaration(), "const x 42").is_err());
    assert!(parse_test!(declaration(), "let x =").is_err());
    assert!(parse_test!(declaration(), "var x = 1").is_err());
    assert!(parse_test!(declaration(), "const = 1").is_err());
    assert!(parse_test!(declaration(), "").is_err());
}

#[test]
fn test_comma_separated_exprs() {
    assert_eq!(
        parse_test!(comma_separated_exprs(), "42"),
        Ok(Ast::expr_list(vec![Ast::value(RuntimeValue::Int(42))]))
    );

    assert_eq!(
        parse_test!(comma_separated_exprs(), "1, 2, 3"),
        Ok(Ast::expr_list(vec![
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2)),
            Ast::value(RuntimeValue::Int(3))
        ]))
    );

    assert_eq!(
        parse_test!(comma_separated_exprs(), "4, 5, 6,"),
        Ok(Ast::expr_list(vec![
            Ast::value(RuntimeValue::Int(4)),
            Ast::value(RuntimeValue::Int(5)),
            Ast::value(RuntimeValue::Int(6))
        ]))
    );

    assert_eq!(
        parse_test!(comma_separated_exprs(), "42, \"hello\", true"),
        Ok(Ast::expr_list(vec![
            Ast::value(RuntimeValue::Int(42)),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("hello"))),
            Ast::value(RuntimeValue::Bool(true))
        ]))
    );

    assert_eq!(
        parse_test!(comma_separated_exprs(), "1 + 2, x * y"),
        Ok(Ast::expr_list(vec![
            Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ),
            Ast::binop(
                Operator::Multiply,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()]))
            )
        ]))
    );

    assert_eq!(
        parse_test!(comma_separated_exprs(), "(1 + 2), (x * (y + z))"),
        Ok(Ast::expr_list(vec![
            Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ),
            Ast::binop(
                Operator::Multiply,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::binop(
                    Operator::Plus,
                    Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                    Ast::value(RuntimeValue::IdentPath(vec!["z".to_string()]))
                )
            )
        ]))
    );

    assert_eq!(
        parse_test!(comma_separated_exprs(), "-1, !true, !0b1010"),
        Ok(Ast::expr_list(vec![
            Ast::unary(UnaryOperator::Negate, Ast::value(RuntimeValue::Int(1))),
            Ast::unary(
                UnaryOperator::BitwiseNot,
                Ast::value(RuntimeValue::Bool(true))
            ),
            Ast::unary(UnaryOperator::BitwiseNot, Ast::value(RuntimeValue::Int(10)))
        ]))
    );

    assert_eq!(
        parse_test!(comma_separated_exprs(), ""),
        Ok(Ast::expr_list(vec![]))
    );
}

#[test]
fn test_function_calls() {
    assert_eq!(
        parse_test!(expr(), "foo()"),
        Ok(Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["foo".to_string()])),
            Ast::expr_list(vec![])
        ))
    );

    assert_eq!(
        parse_test!(expr(), "bar(1, 2)"),
        Ok(Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["bar".to_string()])),
            Ast::expr_list(vec![
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            ])
        ))
    );

    assert_eq!(
        parse_test!(expr(), "f(g(x))"),
        Ok(Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["f".to_string()])),
            Ast::expr_list(vec![Ast::call(
                Ast::value(RuntimeValue::IdentPath(vec!["g".to_string()])),
                Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                    "x".to_string()
                ]))])
            )])
        ))
    );
}

#[test]
fn test_collections() {
    assert_eq!(
        parse_test!(expr(), "[1, 2, 3]"),
        Ok(Ast::list(vec![
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2)),
            Ast::value(RuntimeValue::Int(3)),
        ]))
    );

    assert_eq!(
        parse_test!(expr(), ":{ \"a\": 1, \"b\": 2 }"),
        Ok(Ast::map(vec![
            (
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("a"))),
                Ast::value(RuntimeValue::Int(1))
            ),
            (
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("b"))),
                Ast::value(RuntimeValue::Int(2))
            ),
        ]))
    );

    assert_eq!(parse_test!(expr(), "[]"), Ok(Ast::list(vec![])));

    assert_eq!(parse_test!(expr(), ":{}"), Ok(Ast::map(vec![])));
}

// ---- Typed declaration tests ----

#[test]
fn test_typed_let_int() {
    let result = parse_test!(declaration(), "let x: int = 5").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
            TypeAnnotation::Int,
            Ast::value(RuntimeValue::Int(5)),
        )
    );
}

#[test]
#[allow(clippy::approx_constant)]
fn test_typed_let_float() {
    let result = parse_test!(declaration(), "let pi: float = 3.14").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["pi".to_string()])),
            TypeAnnotation::Float,
            Ast::value(RuntimeValue::Float(3.14)),
        )
    );
}

#[test]
fn test_typed_let_bool() {
    let result = parse_test!(declaration(), "let flag: bool = true").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["flag".to_string()])),
            TypeAnnotation::Bool,
            Ast::value(RuntimeValue::Bool(true)),
        )
    );
}

#[test]
fn test_typed_let_str() {
    let result = parse_test!(declaration(), "let msg: str = \"hello\"").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["msg".to_string()])),
            TypeAnnotation::Str,
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("hello"))),
        )
    );
}

#[test]
fn test_typed_const() {
    let result = parse_test!(declaration(), "const MAX: int = 100").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Constant,
            Ast::value(RuntimeValue::IdentPath(vec!["MAX".to_string()])),
            TypeAnnotation::Int,
            Ast::value(RuntimeValue::Int(100)),
        )
    );
}

#[test]
fn test_typed_global() {
    let result = parse_test!(declaration(), "global score: int = 0").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Global,
            Ast::value(RuntimeValue::IdentPath(vec!["score".to_string()])),
            TypeAnnotation::Int,
            Ast::value(RuntimeValue::Int(0)),
        )
    );
}

#[test]
fn test_typed_named_user_type() {
    // A user-defined type like an enum name
    let result = parse_test!(declaration(), "let dir: Direction = Direction.North").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["dir".to_string()])),
            TypeAnnotation::Named(vec!["Direction".to_string()]),
            Ast::value(RuntimeValue::IdentPath(vec![
                "Direction".to_string(),
                "North".to_string()
            ])),
        )
    );
}

#[test]
fn test_typed_named_path_type() {
    // A dotted module path type like `my_mod.Color`
    let result = parse_test!(declaration(), "let c: my_mod.Color = my_mod.Color.Red").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["c".to_string()])),
            TypeAnnotation::Named(vec!["my_mod".to_string(), "Color".to_string()]),
            Ast::value(RuntimeValue::IdentPath(vec![
                "my_mod".to_string(),
                "Color".to_string(),
                "Red".to_string()
            ])),
        )
    );
}

#[test]
fn test_typed_with_expression_rhs() {
    // Type annotation with a non-trivial RHS expression
    let result = parse_test!(declaration(), "let total: int = 1 + 2 * 3").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["total".to_string()])),
            TypeAnnotation::Int,
            Ast::binop(
                Operator::Plus,
                Ast::value(RuntimeValue::Int(1)),
                Ast::binop(
                    Operator::Multiply,
                    Ast::value(RuntimeValue::Int(2)),
                    Ast::value(RuntimeValue::Int(3)),
                )
            ),
        )
    );
}

#[test]
fn test_untyped_declaration_still_works() {
    // Ensure existing untyped declarations are unaffected
    let result = parse_test!(declaration(), "let x = 99").unwrap();
    assert_eq!(
        result,
        Ast::decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
            Ast::value(RuntimeValue::Int(99)),
        )
    );
}

#[test]
fn test_typed_null_annotation() {
    // `null` is a keyword token, make sure it works as a type name too
    let result = parse_test!(declaration(), "let nothing: null = null").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["nothing".to_string()])),
            TypeAnnotation::Null,
            Ast::value(RuntimeValue::Null),
        )
    );
}

#[test]
fn test_typed_declaration_error_missing_type_after_colon() {
    // A colon with no following type name must fail
    assert!(parse_test!(declaration(), "let x: = 5").is_err());
}

#[test]
fn test_typed_let_list() {
    let result = parse_test!(declaration(), "let items: list = [1, 2, 3]").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["items".to_string()])),
            TypeAnnotation::List,
            Ast::list(vec![
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2)),
                Ast::value(RuntimeValue::Int(3)),
            ]),
        )
    );
}

#[test]
fn test_typed_let_map() {
    let result = parse_test!(declaration(), "let data: map = :{\"a\": 1}").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["data".to_string()])),
            TypeAnnotation::Map,
            Ast::map(vec![(
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("a"))),
                Ast::value(RuntimeValue::Int(1)),
            )]),
        )
    );
}

#[test]
fn test_typed_let_dice() {
    let result = parse_test!(declaration(), "let roll: dice = 2d6").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Variable,
            Ast::value(RuntimeValue::IdentPath(vec!["roll".to_string()])),
            TypeAnnotation::Dice,
            Ast::value(RuntimeValue::Dice(2, 6)),
        )
    );
}

#[test]
fn test_typed_const_list() {
    let result = parse_test!(declaration(), "const EMPTY: list = []").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Constant,
            Ast::value(RuntimeValue::IdentPath(vec!["EMPTY".to_string()])),
            TypeAnnotation::List,
            Ast::list(vec![]),
        )
    );
}

#[test]
fn test_typed_global_map() {
    let result = parse_test!(declaration(), "global state: map = :{}").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Global,
            Ast::value(RuntimeValue::IdentPath(vec!["state".to_string()])),
            TypeAnnotation::Map,
            Ast::map(vec![]),
        )
    );
}

#[test]
fn test_typed_global_dice() {
    let result = parse_test!(declaration(), "global damage: dice = 1d20").unwrap();
    assert_eq!(
        result,
        Ast::typed_decl(
            DeclKind::Global,
            Ast::value(RuntimeValue::IdentPath(vec!["damage".to_string()])),
            TypeAnnotation::Dice,
            Ast::value(RuntimeValue::Dice(1, 20)),
        )
    );
}
