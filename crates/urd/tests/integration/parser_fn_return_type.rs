//! Regression tests for function return-type annotations (`-> Type`).
//!
//! These tests verify that `return_type_parser` (which now delegates to the
//! shared `type_name` recogniser) correctly handles all built-in types —
//! including `range`, which was previously missing from the duplicated
//! select block in `block.rs`.

use urd::{
    parse_test,
    parser::{
        ast::{AstContent, TypeAnnotation},
        block::{code_block, statement},
    },
};

// ---------------------------------------------------------------------------
// Fix 1 regression: `-> range` was missing from the old duplicated select block
// ---------------------------------------------------------------------------

/// Named function with `-> range` return type parses correctly.
#[test]
fn fn_def_return_type_range() {
    let src = "{ fn f() -> range { return 1..10 } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> range should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block, got {:?}", ast.content());
    };
    assert_eq!(stmts.len(), 1);

    let AstContent::FnDef { name, ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef, got {:?}", stmts[0].content());
    };
    assert_eq!(name.as_deref(), Some("f"));
    assert_eq!(
        *ret_type,
        Some(TypeAnnotation::Range),
        "return type should be Range"
    );
}

/// Anonymous function assigned via `let` with `-> range` return type.
#[test]
fn anon_fn_return_type_range() {
    let src = "let f = fn() -> range { return 1..10 }";
    let result = parse_test!(statement(), src);
    assert!(
        result.is_ok(),
        "anon fn with -> range should parse: {result:?}"
    );

    let ast = result.unwrap();
    let AstContent::Declaration { decl_defs, .. } = ast.content() else {
        panic!("expected Declaration, got {:?}", ast.content());
    };

    let AstContent::FnDef { name, ret_type, .. } = decl_defs.content() else {
        panic!("expected FnDef in decl_defs, got {:?}", decl_defs.content());
    };
    assert_eq!(*name, None, "anonymous fn should have name=None");
    assert_eq!(
        *ret_type,
        Some(TypeAnnotation::Range),
        "return type should be Range"
    );
}

// ---------------------------------------------------------------------------
// Regression: existing built-in return types must still work
// ---------------------------------------------------------------------------

/// `-> int` still parses after the refactor.
#[test]
fn fn_def_return_type_int() {
    let src = "{ fn f() -> int { return 1 } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> int should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::Int));
}

/// `-> float` still parses.
#[test]
fn fn_def_return_type_float() {
    let src = "{ fn f() -> float { return 1.0 } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> float should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::Float));
}

/// `-> bool` still parses.
#[test]
fn fn_def_return_type_bool() {
    let src = "{ fn f() -> bool { return true } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> bool should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::Bool));
}

/// `-> str` still parses.
#[test]
fn fn_def_return_type_str() {
    let src = "{ fn f() -> str { return \"hi\" } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> str should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::Str));
}

// ---------------------------------------------------------------------------
// User-defined / named return types
// ---------------------------------------------------------------------------

/// `-> SomeType` parses as `TypeAnnotation::Named`.
#[test]
fn fn_def_return_type_named() {
    let src = "{ fn f() -> SomeType { return 1 } }";
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "fn with -> SomeType should parse: {result:?}"
    );

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(
        *ret_type,
        Some(TypeAnnotation::Named(vec!["SomeType".to_string()])),
        "user-defined type should parse as Named"
    );
}

/// `-> null` still parses (the `null` keyword is a distinct token).
#[test]
fn fn_def_return_type_null() {
    let src = "{ fn f() -> null { return null } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> null should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::Null));
}

/// No return type annotation produces `ret_type = None`.
#[test]
fn fn_def_no_return_type() {
    let src = "{ fn f() { return 1 } }";
    let result = parse_test!(code_block(), src);
    assert!(
        result.is_ok(),
        "fn without return type should parse: {result:?}"
    );

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, None, "missing return type should be None");
}

/// `-> list` return type.
#[test]
fn fn_def_return_type_list() {
    let src = "{ fn f() -> list { return [1, 2] } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> list should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::List));
}

/// `-> map` return type.
#[test]
fn fn_def_return_type_map() {
    let src = "{ fn f() -> map { return m } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> map should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::Map));
}

/// `-> dice` return type.
#[test]
fn fn_def_return_type_dice() {
    let src = "{ fn f() -> dice { return 2d6 } }";
    let result = parse_test!(code_block(), src);
    assert!(result.is_ok(), "fn with -> dice should parse: {result:?}");

    let ast = result.unwrap();
    let AstContent::Block(stmts) = ast.content() else {
        panic!("expected Block");
    };
    let AstContent::FnDef { ret_type, .. } = stmts[0].content() else {
        panic!("expected FnDef");
    };
    assert_eq!(*ret_type, Some(TypeAnnotation::Dice));
}
