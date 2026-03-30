use urd::{
    parse_test,
    parser::{
        ast::{AstContent, MatchPattern},
        block::{enum_decl, match_statement, script},
    },
    runtime::value::RuntimeValue,
};

// ---- Enum declaration tests ----

#[test]
fn test_enum_single_line() {
    let src = "enum Direction { North, South, East, West }";
    let result = parse_test!(enum_decl(), src);
    assert!(result.is_ok(), "single-line enum should parse: {result:?}");
    let Ok(node) = result else { return };
    let AstContent::EnumDecl { name, variants } = node.content() else {
        panic!("expected EnumDecl, got {:?}", node.content());
    };
    assert_eq!(name, "Direction");
    assert_eq!(variants, &["North", "South", "East", "West"]);
}

#[test]
fn test_enum_multiline() {
    let src = "enum Direction {
        North
        South
        East
        West
    }";
    let result = parse_test!(enum_decl(), src);
    assert!(result.is_ok(), "multiline enum should parse: {result:?}");
    let Ok(node) = result else { return };
    let AstContent::EnumDecl { name, variants } = node.content() else {
        panic!("expected EnumDecl, got {:?}", node.content());
    };
    assert_eq!(name, "Direction");
    assert_eq!(variants, &["North", "South", "East", "West"]);
}

#[test]
fn test_enum_mixed_separators() {
    let src = "enum Direction {
        North, South
        East, West
    }";
    let result = parse_test!(enum_decl(), src);
    assert!(
        result.is_ok(),
        "enum with mixed separators should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::EnumDecl { variants, .. } = node.content() else {
        panic!("expected EnumDecl, got {:?}", node.content());
    };
    assert_eq!(variants, &["North", "South", "East", "West"]);
}

#[test]
fn test_enum_single_variant() {
    let src = "enum Coin { Heads }";
    let result = parse_test!(enum_decl(), src);
    assert!(
        result.is_ok(),
        "single-variant enum should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::EnumDecl { name, variants } = node.content() else {
        panic!("expected EnumDecl, got {:?}", node.content());
    };
    assert_eq!(name, "Coin");
    assert_eq!(variants, &["Heads"]);
}

#[test]
fn test_enum_trailing_comma() {
    let src = "enum X { A, B, }";
    let result = parse_test!(enum_decl(), src);
    assert!(
        result.is_ok(),
        "enum with trailing comma should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::EnumDecl { variants, .. } = node.content() else {
        panic!("expected EnumDecl, got {:?}", node.content());
    };
    assert_eq!(variants, &["A", "B"]);
}

#[test]
fn test_enum_in_script() {
    let src = "
        enum Color { Red, Green, Blue }
        let c = Color.Red
    ";
    let result = parse_test!(script(), src);
    assert!(result.is_ok(), "enum in script should parse: {result:?}");
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 2);
    assert!(matches!(stmts[0].content(), AstContent::EnumDecl { .. }));
}

// ---- Match statement tests ----

#[test]
fn test_match_enum_variants() {
    let src = "match dir {
        Direction.North { let x = 1 }
        Direction.South { let x = 2 }
        _ { let x = 0 }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(
        result.is_ok(),
        "match on enum variants should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::Match { scrutinee: _, arms } = node.content() else {
        panic!("expected Match, got {:?}", node.content());
    };
    assert_eq!(arms.len(), 3);
    assert!(matches!(arms[0].pattern, MatchPattern::Value(_)));
    assert!(matches!(arms[1].pattern, MatchPattern::Value(_)));
    assert!(matches!(arms[2].pattern, MatchPattern::Wildcard));
}

#[test]
fn test_match_literal_int() {
    let src = "match score {
        1 { let grade = \"A\" }
        2 { let grade = \"B\" }
        _ { let grade = \"F\" }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(
        result.is_ok(),
        "match on integer literals should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::Match { scrutinee: _, arms } = node.content() else {
        panic!("expected Match, got {:?}", node.content());
    };
    assert_eq!(arms.len(), 3);
    assert!(matches!(
        &arms[0].pattern,
        MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Int(1)))
    ));
    assert!(matches!(
        &arms[1].pattern,
        MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Int(2)))
    ));
    assert!(matches!(arms[2].pattern, MatchPattern::Wildcard));
}

#[test]
fn test_match_literal_bool() {
    let src = "match flag {
        true { let x = 1 }
        false { let x = 0 }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(
        result.is_ok(),
        "match on bool literals should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::Match { scrutinee: _, arms } = node.content() else {
        panic!("expected Match, got {:?}", node.content());
    };
    assert_eq!(arms.len(), 2);
    assert!(matches!(
        &arms[0].pattern,
        MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Bool(true)))
    ));
    assert!(matches!(
        &arms[1].pattern,
        MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Bool(false)))
    ));
}

#[test]
fn test_match_wildcard_only() {
    let src = "match x {
        _ { let y = 42 }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(
        result.is_ok(),
        "match with only wildcard arm should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::Match { arms, .. } = node.content() else {
        panic!("expected Match, got {:?}", node.content());
    };
    assert_eq!(arms.len(), 1);
    assert!(matches!(arms[0].pattern, MatchPattern::Wildcard));
}

#[test]
fn test_match_no_wildcard() {
    let src = "match phase {
        1 { jump intro }
        2 { jump middle }
        3 { jump outro }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(
        result.is_ok(),
        "match with no wildcard arm should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::Match { arms, .. } = node.content() else {
        panic!("expected Match, got {:?}", node.content());
    };
    assert_eq!(arms.len(), 3);
    for arm in arms {
        assert!(
            matches!(arm.pattern, MatchPattern::Value(_)),
            "expected Value pattern, got {:?}",
            arm.pattern
        );
    }
}

#[test]
fn test_match_nested() {
    let src = "match outer {
        1 {
            match inner {
                true { let z = 1 }
                _ { let z = 0 }
            }
        }
        _ { let z = 99 }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(result.is_ok(), "nested match should parse: {result:?}");
    let Ok(node) = result else { return };
    let AstContent::Match { arms, .. } = node.content() else {
        panic!("expected outer Match, got {:?}", node.content());
    };
    assert_eq!(arms.len(), 2);
    let AstContent::Block(inner_stmts) = arms[0].body.content() else {
        panic!("expected Block in first arm body");
    };
    assert_eq!(inner_stmts.len(), 1);
    assert!(
        matches!(inner_stmts[0].content(), AstContent::Match { .. }),
        "expected inner Match node"
    );
}

#[test]
fn test_match_in_script() {
    let src = "
        enum Dir { North, South }
        match dir {
            Dir.North { jump north_scene }
            Dir.South { jump south_scene }
            _ { jump default_scene }
        }
    ";
    let result = parse_test!(script(), src);
    assert!(
        result.is_ok(),
        "enum + match in script should parse: {result:?}"
    );
    let Ok(block) = result else { return };
    let AstContent::Block(stmts) = block.content() else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 2);
    assert!(matches!(stmts[0].content(), AstContent::EnumDecl { .. }));
    assert!(matches!(stmts[1].content(), AstContent::Match { .. }));
}

#[test]
fn test_match_null_pattern() {
    let src = "match val {
        null { let x = 0 }
        _ { let x = 1 }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(
        result.is_ok(),
        "match on null pattern should parse: {result:?}"
    );
    let Ok(node) = result else { return };
    let AstContent::Match { arms, .. } = node.content() else {
        panic!("expected Match");
    };
    assert!(matches!(
        &arms[0].pattern,
        MatchPattern::Value(ast) if matches!(ast.content(), AstContent::Value(RuntimeValue::Null))
    ));
}

#[test]
fn test_match_scrutinee_expression() {
    let src = "match x + 1 {
        2 { let r = \"two\" }
        _ { let r = \"other\" }
    }";
    let result = parse_test!(match_statement(), src);
    assert!(
        result.is_ok(),
        "match with expression scrutinee should parse: {result:?}"
    );
}
