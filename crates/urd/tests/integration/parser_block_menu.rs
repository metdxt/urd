use urd::{
    parse_test,
    parser::{
        ast::{Ast, AstContent, DeclKind},
        block::{code_block, jump_statement, menu, return_statement, script},
    },
    runtime::value::RuntimeValue,
};

// ---- Dialogue tests ----

#[test]
fn test_dialogue_single_line() {
    let src = "{ <Alice>: \"Hello!\" }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_dialogue_monologue() {
    let src = "{ <Alice>: { \"Line 1\", \"Line 2\" } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_dialogue_monologue_with_newlines() {
    let src = "{
        <Alice>: {
            \"Line 1\"
            \"Line 2\"
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_dialogue_multiple_speakers() {
    let src = "{ <Alice, Bob>: \"Hello both!\" }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

// ---- Menu tests ----

#[test]
fn test_menu_empty() {
    let src = "{ menu { } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_with_escaped_characters() {
    let src = "{
        menu {
            \"Option \\\"quoted\\\"\" {
                let x = 1
            }
            \"Tab\\there\" {
                let y = 2
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_complex_expressions() {
    let src = "{
        menu {
            \"Add\" {
                let result = 5 + 3
            }
            \"Multiply\" {
                let result = 5 * 3
            }
            \"Simple action\" {
                let done = true
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_multiple_menus() {
    let src = "{
        menu {
            \"Choice 1\" {
                let x = 1
            }
            \"Choice 2\" {
                let x = 2
            }
        }
        menu {
            \"Next\" {
                let y = 3
            }
            \"Back\" {
                let y = 4
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_with_dialogue() {
    let src = "{
        menu {
            \"Talk to Alice\" {
                <Alice>: \"Hello there!\"
            }
            \"Talk to Bob\" {
                <Bob>: \"Hi!\"
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_with_if_else() {
    let src = "{
        menu {
            \"Check condition\" {
                if true {
                    let x = 1
                } else {
                    let x = 2
                }
            }
            \"Nested menu\" {
                menu {
                    \"Sub-option 1\" {
                        let y = 3
                    }
                }
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_ast_structure() {
    let src = "{
        menu {
            \"Option 1\" {
                let x = 1
            }
            \"Option 2\" {
                let y = 2
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::menu(vec![
            Ast::menu_option(
                "Option 1".to_string(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                    Ast::value(RuntimeValue::Int(1))
                )])
            ),
            Ast::menu_option(
                "Option 2".to_string(),
                Ast::block(vec![Ast::decl(
                    DeclKind::Variable,
                    Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                    Ast::value(RuntimeValue::Int(2))
                )])
            ),
        ])]))
    );
}

#[test]
fn test_menu_basic() {
    let src = "{
        menu {
            \"Option 1\" {
                let x = 1
            }
            \"Option 2\" {
                let y = 2
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_real_world_example() {
    let src = "{
        menu {
            \"Start Game\" {
                let health = 100
                <Narrator>: \"Your adventure begins!\"
            }
            \"Load Game\" {
                <Narrator>: \"No save files found.\"
            }
            \"Exit\" {
                <Narrator>: \"Goodbye!\"
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_single_option() {
    let src = "{ menu { \"Only option\" { let x = 1 } } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_menu_nested_blocks() {
    let src = "{
        menu {
            \"Option 1\" {
                {
                    let x = 1
                }
            }
            \"Option 2\" {
                if true {
                    let y = 2
                }
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_return_in_menu_option() {
    let src = "menu { \"Option 1\" { return 1 } }";
    let result = parse_test!(menu(), src);
    assert!(result.is_ok());
}

#[test]
fn test_jump_in_menu_option() {
    let src = "menu { \"Option 1\" { jump scene_a }
        \"Option 2\" { jump scene_b } }";
    let result = parse_test!(menu(), src);
    assert!(result.is_ok());
}

// ---- Assignment tests ----

#[test]
fn test_assignment_basic() {
    let src = "{ x = 1 }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::assign_op(
            Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
            Ast::value(RuntimeValue::Int(1))
        )]))
    );
}

#[test]
fn test_assignment_with_expression() {
    let src = "{ x = 1 + 2 }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![Ast::assign_op(
            Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
            Ast::add_op(
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2))
            )
        )]))
    );
}

#[test]
fn test_assignment_after_declaration() {
    let src = "{
        let x = 1
        x = 2
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
            Ast::assign_op(
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(2))
            )
        ]))
    );
}

#[test]
fn test_multiple_assignments() {
    let src = "{
        let x = 0
        x = 1
        x = 2
        x = 3
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(0))
            ),
            Ast::assign_op(
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(1))
            ),
            Ast::assign_op(
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(2))
            ),
            Ast::assign_op(
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(3))
            )
        ]))
    );
}

#[test]
fn test_assignment_in_nested_block() {
    let src = "{
        let x = 1
        {
            x = 2
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_assignment_in_if_statement() {
    let src = "{
        let x = 1
        if true {
            x = 2
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_assignment_in_menu_option() {
    let src = "{
        let x = 1
        menu {
            \"Option 1\" {
                x = 2
            }
            \"Option 2\" {
                x = 3
            }
        }
    }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_assignment_with_complex_expression() {
    let src = "{ x = (1 + 2) * 3 }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_assignment_with_variable() {
    let src = "{
        let y = 10
        let x = 5
        x = y
    }";
    let result = parse_test!(code_block(), src);
    assert_eq!(
        result,
        Ok(Ast::block(vec![
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()])),
                Ast::value(RuntimeValue::Int(10))
            ),
            Ast::decl(
                DeclKind::Variable,
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::Int(5))
            ),
            Ast::assign_op(
                Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()])),
                Ast::value(RuntimeValue::IdentPath(vec!["y".to_string()]))
            )
        ]))
    );
}

#[test]
fn test_assignment_in_script() {
    let src = "
        let x = 1
        x = 2
    ";
    let result = parse_test!(script(), src);
    assert!(result.is_ok());
}

#[test]
fn test_assignment_with_semicolon() {
    let src = "{ x = 1; y = 2 }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

// ---- Return statement tests ----

#[test]
fn test_return_basic() {
    let src = "return 42";
    let result = parse_test!(return_statement(), src);
    assert!(result.is_ok());
}

#[test]
fn test_return_with_expression() {
    let src = "return x + 5";
    let result = parse_test!(return_statement(), src);
    assert!(result.is_ok());
}

#[test]
fn test_return_without_value() {
    let src = "return";
    let result = parse_test!(return_statement(), src);
    assert!(result.is_ok());
}

#[test]
fn test_return_in_block() {
    let src = "{ return 100 }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_return_in_script() {
    let src = "
        let x = 10
        return x
    ";
    let result = parse_test!(script(), src);
    assert!(result.is_ok());
}

#[test]
fn test_return_with_function_call() {
    let src = "return foo(1, 2)";
    let result = parse_test!(return_statement(), src);
    assert!(result.is_ok());
}

#[test]
fn test_return_complex_expression() {
    let src = "return (x + y) * 2";
    let result = parse_test!(return_statement(), src);
    assert!(result.is_ok());
}

// ---- Jump statement tests ----

#[test]
fn test_jump_basic() {
    let src = "jump my_label";
    let result = parse_test!(jump_statement(), src);
    assert!(result.is_ok());
}

#[test]
fn test_jump_in_block() {
    let src = "{ jump my_label }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

#[test]
fn test_jump_in_script() {
    let src = "
        let x = 10
        jump start_label
    ";
    let result = parse_test!(script(), src);
    assert!(result.is_ok());
}

#[test]
fn test_jump_with_multiple_jumps() {
    let src = "{ jump a; jump b; jump c }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
}

// ---- AstContent shape checks ----

#[test]
fn test_return_ast_shape() {
    let src = "{ return 42 }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    assert!(matches!(stmts[0].content(), AstContent::Return { .. }));
}

#[test]
fn test_jump_ast_shape() {
    let src = "{ jump somewhere }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok());
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    assert!(matches!(stmts[0].content(), AstContent::Jump { label, .. } if label == "somewhere"));
}
