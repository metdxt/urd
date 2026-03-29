use urd::{
    lexer::strings::ParsedString,
    parse_test,
    parser::{
        ast::{Ast, AstContent, DeclKind, Decorator},
        block::{code_block, if_statement, labeled_block, script},
    },
    runtime::value::RuntimeValue,
};

// ---- Decorator tests ----

#[test]
fn test_decorator_bare_on_labeled_block() {
    let src = "
        @on_enter
        label my_scene { let x = 1 }
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "bare decorator on labeled block should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    let decorated = &stmts[0];
    assert_eq!(decorated.decorators().len(), 1);
    assert_eq!(decorated.decorators()[0].name(), "on_enter");
    assert_eq!(
        decorated.decorators()[0].args().content(),
        &AstContent::ExprList(vec![])
    );
}

#[test]
fn test_decorator_with_args_on_labeled_block() {
    let src = "
        @event(\"start\", 42)
        label my_scene { let x = 1 }
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "decorator with args on labeled block should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    let decorated = &stmts[0];
    assert_eq!(decorated.decorators().len(), 1);
    assert_eq!(decorated.decorators()[0].name(), "event");
    let AstContent::ExprList(args) = decorated.decorators()[0].args().content() else {
        panic!("expected ExprList for decorator args");
    };
    assert_eq!(args.len(), 2);
}

#[test]
fn test_multiple_decorators_on_labeled_block() {
    let src = "
        @foo
        @bar(1)
        label my_scene { let x = 1 }
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "multiple decorators should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    let decorated = &stmts[0];
    assert_eq!(decorated.decorators().len(), 2);
    assert_eq!(decorated.decorators()[0].name(), "foo");
    assert_eq!(decorated.decorators()[1].name(), "bar");
}

#[test]
fn test_decorator_on_dialogue() {
    let src = "
        @voiced
        <Alice>: \"Hello!\"
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "decorator on dialogue should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    let decorated = &stmts[0];
    assert_eq!(decorated.decorators().len(), 1);
    assert_eq!(decorated.decorators()[0].name(), "voiced");
}

#[test]
fn test_decorator_on_menu() {
    let src = "
        @important
        menu {
            \"Yes\" { jump yes }
            \"No\" { jump no }
        }
    ";
    let result = parse_test!(script(), src);
    assert!(result.is_ok(), "decorator on menu should parse: {result:?}");
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    let decorated = &stmts[0];
    assert_eq!(decorated.decorators().len(), 1);
    assert_eq!(decorated.decorators()[0].name(), "important");
}

#[test]
fn test_undecorated_statement_still_has_no_decorators() {
    let src = "label plain { let x = 1 }";
    let result = parse_test!(script(), src);
    assert!(result.is_ok());
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts[0].decorators().len(), 0);
}

#[test]
fn test_decorator_with_expression_arg() {
    let src = "
        @weight(1 + 2)
        label scene { let x = 1 }
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "decorator with expression arg should parse: {result:?}"
    );
}

#[test]
fn test_decorator_inside_code_block() {
    let src = "{
        @on_enter
        label inner { let x = 1 }
    }";
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "decorator inside code block should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts[0].decorators().len(), 1);
}

#[test]
fn test_decorator_constructs() {
    let bare = Decorator::bare("test".to_string());
    assert_eq!(bare.name(), "test");
    assert_eq!(bare.args().content(), &AstContent::ExprList(vec![]));

    let with_args = Decorator::new(
        "event".to_string(),
        Ast::expr_list(vec![Ast::value(RuntimeValue::Int(1))]),
    );
    assert_eq!(with_args.name(), "event");
    let AstContent::ExprList(args) = with_args.args().content() else {
        panic!("expected ExprList");
    };
    assert_eq!(args.len(), 1);
}

// ---- Script tests ----

#[test]
fn test_script_bare() {
    let src = "
        let x = 1
        <Alice>: \"Hello\"
    ";
    let result = parse_test!(script(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            ),
            Ast::dialogue(
                Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                    "Alice".to_string()
                ]))]),
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("Hello")))
            )
        ]))
    );
}

// ---- Basic block tests ----

#[test]
fn test_empty_block() {
    let src = "{}";
    let result = parse_test!(code_block(), src);
    assert_eq!(result, Ok(Ast::block(vec![])));
}

#[test]
fn test_block_with_declarations() {
    let src = "{
        let x = 1
        const y = 2
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            ),
            Ast::decl(
                DeclKind::Constant,
                Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                Ast::value(RuntimeValue::Int(2))
            )
        ]))
    );
}

#[test]
fn test_block_invalid_content() {
    let src = "{
        let x = 1
        1 + 1
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_err());
}

#[test]
fn test_block_missing_closing_brace() {
    let src = "{
        let x = 1
    ";
    let result = parse_test!(code_block(), src);
    assert!(result.is_err());
}

#[test]
fn test_block_missing_opening_brace() {
    let src = "
        let x = 1
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_err());
}

#[test]
fn test_nested_block() {
    let src = "{
        let x = 1
        {
            let y = 2
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            ),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                Ast::value(RuntimeValue::Int(2))
            )])
        ]))
    );
}

#[test]
fn test_block_semicolon_separator() {
    let src = "{
        let x = 1; let y = 2
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            ),
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                Ast::value(RuntimeValue::Int(2))
            )
        ]))
    );
}

// ---- If statement tests ----

#[test]
fn test_if_statement() {
    let src = "{
        if true {
            let x = 1
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            )]),
            None
        )]))
    );
}

#[test]
fn test_if_else_statement() {
    let src = "{
        if true {
            let x = 1
        } else {
            let x = 2
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            )]),
            Some(Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(2))
            )]))
        )]))
    );
}

#[test]
fn test_if_elif_statement() {
    let src = "{
        if true {
            let x = 1
        } elif false {
            let x = 2
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            )]),
            Some(Ast::if_stmt(
                Ast::value(RuntimeValue::Bool(false)),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )]),
                None
            ))
        )]))
    );
}

#[test]
fn test_if_elif_else_statement() {
    let src = "{
        if true {
            let x = 1
        } elif false {
            let x = 2
        } else {
            let x = 3
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::if_stmt(
            Ast::value(RuntimeValue::Bool(true)),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            )]),
            Some(Ast::if_stmt(
                Ast::value(RuntimeValue::Bool(false)),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )]),
                Some(Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(3))
                )]))
            ))
        )]))
    );
}

#[test]
fn test_return_in_if_statement() {
    let src = "if true { return 1 } else { return 2 }";
    let result = parse_test!(if_statement(), src);
    assert!(result.is_ok());
}

#[test]
fn test_jump_in_if_statement() {
    let src = "if x > 10 { jump target } else { jump other }";
    let result = parse_test!(if_statement(), src);
    assert!(result.is_ok());
}

// ---- Labeled block tests ----

#[test]
fn test_labeled_block() {
    let src = "{
        label my_label {
            let x = 1
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::labeled_block(
            "my_label".to_string(),
            Ast::block(vec![Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            )])
        )]))
    );
}

#[test]
fn test_nested_labeled_blocks() {
    let src = "{
        label outer {
            label inner {
                let x = 1
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::labeled_block(
            "outer".to_string(),
            Ast::block(vec![Ast::labeled_block(
                "inner".to_string(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                )])
            )])
        )]))
    );
}

#[test]
fn test_labeled_block_standalone() {
    let src = "label my_label { let x = 1 }";
    let result = parse_test!(labeled_block(), src);
    assert!(result.is_ok());
}
