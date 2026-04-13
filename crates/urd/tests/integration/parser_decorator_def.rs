use urd::{
    parse_test,
    parser::{
        ast::{AstContent, DecoratorParam, EventConstraint, TypeAnnotation},
        block::{code_block, decorator_def, script},
    },
    runtime::value::RuntimeValue,
};

// ---- Minimal decorator (no constraint, no params) ----

#[test]
fn test_decorator_def_minimal() {
    let src = "decorator shake() {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "minimal decorator def should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef {
        name,
        event_constraint,
        params,
        body,
    } = node.content()
    else {
        panic!("expected DecoratorDef, got {:?}", node.content());
    };
    assert_eq!(name, "shake");
    assert_eq!(*event_constraint, EventConstraint::Any);
    assert!(params.is_empty(), "expected no params, got {params:?}");
    assert!(
        matches!(body.content(), AstContent::Block(stmts) if stmts.is_empty()),
        "expected empty body block, got {:?}",
        body.content()
    );
}

// ---- Full decorator: constraint + typed params ----

#[test]
fn test_decorator_def_full_dialogue_typed_params() {
    let src = "decorator camera_shake<event: dialogue>(amount: float, duration: float) {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "full decorator def with dialogue constraint and typed params should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef {
        name,
        event_constraint,
        params,
        body: _,
    } = node.content()
    else {
        panic!("expected DecoratorDef, got {:?}", node.content());
    };
    assert_eq!(name, "camera_shake");
    assert_eq!(*event_constraint, EventConstraint::Dialogue);
    assert_eq!(params.len(), 2, "expected 2 params, got {params:?}");
    assert_eq!(
        params[0],
        DecoratorParam { span: (0..0).into(),
            name: "amount".to_string(),
            type_annotation: Some(TypeAnnotation::Float),
        }
    );
    assert_eq!(
        params[1],
        DecoratorParam { span: (0..0).into(),
            name: "duration".to_string(),
            type_annotation: Some(TypeAnnotation::Float),
        }
    );
}

// ---- Untyped params ----

#[test]
fn test_decorator_def_untyped_param() {
    let src = "decorator log(msg) {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with untyped param should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef { name, params, .. } = node.content() else {
        panic!("expected DecoratorDef, got {:?}", node.content());
    };
    assert_eq!(name, "log");
    assert_eq!(params.len(), 1, "expected 1 param, got {params:?}");
    assert_eq!(
        params[0],
        DecoratorParam { span: (0..0).into(),
            name: "msg".to_string(),
            type_annotation: None,
        }
    );
}

// ---- Mixed typed and untyped params ----

#[test]
fn test_decorator_def_mixed_params() {
    let src = "decorator notify(msg, priority: int) {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with mixed param types should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef { params, .. } = node.content() else {
        panic!("expected DecoratorDef, got {:?}", node.content());
    };
    assert_eq!(params.len(), 2);
    assert_eq!(
        params[0],
        DecoratorParam { span: (0..0).into(),
            name: "msg".to_string(),
            type_annotation: None,
        }
    );
    assert_eq!(
        params[1],
        DecoratorParam { span: (0..0).into(),
            name: "priority".to_string(),
            type_annotation: Some(TypeAnnotation::Int),
        }
    );
}

// ---- Choice constraint ----

#[test]
fn test_decorator_def_choice_constraint() {
    let src = "decorator highlight<event: choice>() {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with choice constraint should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef {
        name,
        event_constraint,
        params,
        ..
    } = node.content()
    else {
        panic!("expected DecoratorDef, got {:?}", node.content());
    };
    assert_eq!(name, "highlight");
    assert_eq!(*event_constraint, EventConstraint::Choice);
    assert!(params.is_empty());
}

// ---- Dialogue constraint ----

#[test]
fn test_decorator_def_dialogue_constraint_no_params() {
    let src = "decorator voiced<event: dialogue>() {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with dialogue constraint and no params should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef {
        event_constraint, ..
    } = node.content()
    else {
        panic!("expected DecoratorDef");
    };
    assert_eq!(*event_constraint, EventConstraint::Dialogue);
}

// ---- Decorator with body containing statements ----

#[test]
fn test_decorator_def_with_body() {
    let src = "decorator shake(amount: int) { let x = amount }";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with body statements should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef { body, .. } = node.content() else {
        panic!("expected DecoratorDef");
    };
    let AstContent::Block(stmts) = body.content() else {
        panic!("expected Block body, got {:?}", body.content());
    };
    assert_eq!(stmts.len(), 1, "body should have 1 statement");
}

// ---- Trailing comma in param list ----

#[test]
fn test_decorator_def_trailing_comma_params() {
    let src = "decorator multi(a: int, b: str,) {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with trailing comma in params should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef { params, .. } = node.content() else {
        panic!("expected DecoratorDef");
    };
    assert_eq!(params.len(), 2);
}

// ---- Decorator def inside script ----

#[test]
fn test_decorator_def_in_script() {
    let src = "
        decorator shake() {}
        let x = 1
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "decorator def in script should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 2);
    assert!(
        matches!(stmts[0].content(), AstContent::DecoratorDef { .. }),
        "first stmt should be DecoratorDef, got {:?}",
        stmts[0].content()
    );
}

// ---- Multiple decorator defs ----

#[test]
fn test_multiple_decorator_defs_in_script() {
    let src = "
        decorator shake(amount: float) {}
        decorator highlight<event: choice>() {}
        decorator log(msg) {}
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "multiple decorator defs in script should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 3);
    for (i, stmt) in stmts.iter().enumerate() {
        assert!(
            matches!(stmt.content(), AstContent::DecoratorDef { .. }),
            "stmt {i} should be DecoratorDef, got {:?}",
            stmt.content()
        );
    }
}

// ---- Subscript assignment statement ----

#[test]
fn test_subscript_assign_in_code_block() {
    let src = r#"{ event["camera_shake"] = 42 }"#;
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "subscript assignment in code block should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    let AstContent::SubscriptAssign { object, key, value } = stmts[0].content() else {
        panic!("expected SubscriptAssign, got {:?}", stmts[0].content());
    };
    assert!(
        matches!(
            object.content(),
            AstContent::Value(RuntimeValue::IdentPath(p)) if p == &["event"]
        ),
        "object should be IdentPath(event), got {:?}",
        object.content()
    );
    assert!(
        matches!(key.content(), AstContent::Value(RuntimeValue::Str(_))),
        "key should be a string literal, got {:?}",
        key.content()
    );
    assert!(
        matches!(value.content(), AstContent::Value(RuntimeValue::Int(42))),
        "value should be Int(42), got {:?}",
        value.content()
    );
}

// ---- Subscript assignment with integer key ----

#[test]
fn test_subscript_assign_integer_key() {
    let src = "{ arr[0] = 99 }";
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "subscript assignment with int key should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    assert!(
        matches!(stmts[0].content(), AstContent::SubscriptAssign { .. }),
        "expected SubscriptAssign, got {:?}",
        stmts[0].content()
    );
}

// ---- Subscript read in declaration rhs ----

#[test]
fn test_subscript_read_in_declaration() {
    let src = r#"{ let x = m["key"] }"#;
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "subscript read in let declaration should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    let AstContent::Declaration { decl_defs, .. } = stmts[0].content() else {
        panic!("expected Declaration, got {:?}", stmts[0].content());
    };
    // decl_defs is directly the rhs expression — the Subscript node
    assert!(
        matches!(decl_defs.content(), AstContent::Subscript { .. }),
        "expected Subscript node as decl_defs, got {:?}",
        decl_defs.content()
    );
}

// ---- Chained subscript read ----

#[test]
fn test_subscript_chained_read() {
    let src = "{ let v = m[a][b] }";
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "chained subscript read should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    let AstContent::Declaration { decl_defs, .. } = stmts[0].content() else {
        panic!("expected Declaration");
    };
    // decl_defs is directly the rhs expression — the outer Subscript node
    // outer is Subscript(inner, b), inner is Subscript(m, a)
    let AstContent::Subscript {
        object: outer_obj, ..
    } = decl_defs.content()
    else {
        panic!("expected outer Subscript, got {:?}", decl_defs.content());
    };
    assert!(
        matches!(outer_obj.content(), AstContent::Subscript { .. }),
        "inner object should also be a Subscript (chained), got {:?}",
        outer_obj.content()
    );
}

// ---- Subscript in expression context (inside if condition) ----

#[test]
fn test_subscript_in_if_condition() {
    let src = "{ if flags[0] { let x = 1 } }";
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "subscript in if condition should parse: {result:?}"
    );
}

// ---- Decorator def does not conflict with @decorator usage ----

#[test]
fn test_decorator_def_and_usage_coexist_in_script() {
    let src = "
        decorator shake() {}
        @shake
        label my_scene { let x = 1 }
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "decorator def followed by @decorator usage should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 2, "expected 2 top-level statements");
    assert!(matches!(
        stmts[0].content(),
        AstContent::DecoratorDef { .. }
    ));
    // Second statement is a labeled block with 1 decorator applied
    assert_eq!(
        stmts[1].decorators().len(),
        1,
        "labeled block should have 1 decorator"
    );
    assert_eq!(stmts[1].decorators()[0].name(), "shake");
}

// ---- All built-in type annotations in decorator params ----

#[test]
fn test_decorator_def_all_builtin_type_annotations() {
    let src = "decorator full(a: int, b: float, c: bool, d: str, e: list, f: map, g: dice) {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with all builtin type annotations should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef { params, .. } = node.content() else {
        panic!("expected DecoratorDef");
    };
    assert_eq!(params.len(), 7);
    assert_eq!(params[0].type_annotation, Some(TypeAnnotation::Int));
    assert_eq!(params[1].type_annotation, Some(TypeAnnotation::Float));
    assert_eq!(params[2].type_annotation, Some(TypeAnnotation::Bool));
    assert_eq!(params[3].type_annotation, Some(TypeAnnotation::Str));
    assert_eq!(params[4].type_annotation, Some(TypeAnnotation::List));
    assert_eq!(params[5].type_annotation, Some(TypeAnnotation::Map));
    assert_eq!(params[6].type_annotation, Some(TypeAnnotation::Dice));
}

// ---- Named (user-defined) type annotation in decorator params ----

#[test]
fn test_decorator_def_named_type_annotation() {
    let src = "decorator position(loc: Vec2) {}";
    let result = parse_test!(decorator_def(), src);
    assert!(
        result.is_ok(),
        "decorator with named type annotation should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::DecoratorDef { params, .. } = node.content() else {
        panic!("expected DecoratorDef");
    };
    assert_eq!(params.len(), 1);
    assert!(
        matches!(
            &params[0].type_annotation,
            Some(TypeAnnotation::Named(path)) if path == &["Vec2"]
        ),
        "expected Named(Vec2), got {:?}",
        params[0].type_annotation
    );
}
