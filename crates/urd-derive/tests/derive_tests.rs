//! # Integration tests for `urd-derive`'s `ExternObject` derive macro
//!
//! These tests exercise every attribute combination the macro supports and
//! verify the generated trait implementation behaves correctly at runtime.

#![allow(missing_docs, dead_code)]

use urd::{ExternObject, RuntimeValue};

// ═══════════════════════════════════════════════════════════════════════════════
// Test structs — one per attribute combination
// ═══════════════════════════════════════════════════════════════════════════════

/// Bare struct with no container or field attributes.
/// `type_name()` should default to the Rust struct name.
#[derive(ExternObject)]
struct Bare {
    x: f64,
    name: String,
}

/// Struct with a custom `type_name` container attribute.
#[derive(ExternObject)]
#[extern_object(type_name = "CustomName")]
struct WithTypeName {
    value: i32,
}

/// Struct with a `skip`ped field.
#[derive(ExternObject)]
struct WithSkip {
    visible: i32,

    #[extern_object(skip)]
    hidden: String,
}

/// Struct with a `readonly` field.
#[derive(ExternObject)]
struct WithReadonly {
    mutable_field: i32,

    #[extern_object(readonly)]
    locked: String,
}

/// Struct with a `rename`d field.
#[derive(ExternObject)]
struct WithRename {
    #[extern_object(rename = "public_name")]
    internal_name: f64,
}

/// Struct exercising every supported field type: f64, String, bool, i32, u64,
/// Vec<i32>, and Vec<String>.
#[derive(ExternObject)]
#[extern_object(type_name = "Kitchen")]
struct KitchenSink {
    float_field: f64,
    string_field: String,
    bool_field: bool,
    int_field: i32,
    uint_field: u64,
    list_ints: Vec<i32>,
    list_strings: Vec<String>,
}

/// Struct combining every attribute flavour at once: custom type_name, skip,
/// readonly, rename, and plain fields.
#[derive(ExternObject)]
#[extern_object(type_name = "Player")]
struct FullCombo {
    hp: i32,
    name: String,

    #[extern_object(readonly)]
    id: u64,

    #[extern_object(skip)]
    internal_counter: u32,

    #[extern_object(rename = "pos_x")]
    position_x: f64,
}

/// Struct with no exposed fields (all fields skipped).
#[derive(ExternObject)]
#[extern_object(type_name = "Ghost")]
struct AllSkipped {
    #[extern_object(skip)]
    secret: i32,
}

/// Struct with multiple readonly fields.
#[derive(ExternObject)]
struct MultiReadonly {
    #[extern_object(readonly)]
    a: i32,

    #[extern_object(readonly)]
    b: String,

    writable: f64,
}

/// Struct combining readonly + rename on the same field.
#[derive(ExternObject)]
struct ReadonlyRenamed {
    #[extern_object(readonly, rename = "alias")]
    original: i32,
}

// ═══════════════════════════════════════════════════════════════════════════════
// type_name() tests
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn bare_type_name_defaults_to_struct_name() {
    let b = Bare {
        x: 1.0,
        name: "hi".into(),
    };
    assert_eq!(ExternObject::type_name(&b), "Bare");
}

#[test]
fn custom_type_name_overrides_struct_name() {
    let w = WithTypeName { value: 42 };
    assert_eq!(ExternObject::type_name(&w), "CustomName");
}

#[test]
fn full_combo_type_name() {
    let p = FullCombo {
        hp: 100,
        name: "Hero".into(),
        id: 1,
        internal_counter: 0,
        position_x: 0.0,
    };
    assert_eq!(ExternObject::type_name(&p), "Player");
}

#[test]
fn all_skipped_type_name() {
    let g = AllSkipped { secret: 42 };
    assert_eq!(ExternObject::type_name(&g), "Ghost");
}

// ═══════════════════════════════════════════════════════════════════════════════
// fields() tests
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn bare_fields_lists_all() {
    let b = Bare {
        x: 1.0,
        name: "hi".into(),
    };
    let fields = ExternObject::fields(&b);
    assert_eq!(fields.len(), 2);
    assert!(fields.contains(&"x".to_string()));
    assert!(fields.contains(&"name".to_string()));
}

#[test]
fn skip_field_absent_from_fields() {
    let w = WithSkip {
        visible: 10,
        hidden: "secret".into(),
    };
    let fields = ExternObject::fields(&w);
    assert_eq!(fields.len(), 1);
    assert!(fields.contains(&"visible".to_string()));
    assert!(!fields.contains(&"hidden".to_string()));
}

#[test]
fn readonly_field_present_in_fields() {
    let w = WithReadonly {
        mutable_field: 5,
        locked: "read-only".into(),
    };
    let fields = ExternObject::fields(&w);
    assert_eq!(fields.len(), 2);
    assert!(fields.contains(&"mutable_field".to_string()));
    assert!(fields.contains(&"locked".to_string()));
}

#[test]
fn renamed_field_uses_script_name_in_fields() {
    let w = WithRename {
        internal_name: 3.15,
    };
    let fields = ExternObject::fields(&w);
    assert_eq!(fields.len(), 1);
    assert!(fields.contains(&"public_name".to_string()));
    assert!(
        !fields.contains(&"internal_name".to_string()),
        "Rust ident must not leak into fields()"
    );
}

#[test]
fn full_combo_fields_skips_internal_and_renames() {
    let p = FullCombo {
        hp: 1,
        name: "x".into(),
        id: 2,
        internal_counter: 0,
        position_x: 0.0,
    };
    let fields = ExternObject::fields(&p);
    assert!(fields.contains(&"hp".to_string()));
    assert!(fields.contains(&"name".to_string()));
    assert!(fields.contains(&"id".to_string()));
    assert!(fields.contains(&"pos_x".to_string()));
    assert!(!fields.contains(&"internal_counter".to_string()));
    assert!(!fields.contains(&"position_x".to_string()));
    assert_eq!(fields.len(), 4);
}

#[test]
fn all_skipped_fields_is_empty() {
    let g = AllSkipped { secret: 1 };
    assert!(ExternObject::fields(&g).is_empty());
}

#[test]
fn kitchen_sink_fields_lists_all_seven() {
    let ks = KitchenSink {
        float_field: 0.0,
        string_field: String::new(),
        bool_field: false,
        int_field: 0,
        uint_field: 0,
        list_ints: vec![],
        list_strings: vec![],
    };
    let fields = ExternObject::fields(&ks);
    assert_eq!(fields.len(), 7);
    for name in &[
        "float_field",
        "string_field",
        "bool_field",
        "int_field",
        "uint_field",
        "list_ints",
        "list_strings",
    ] {
        assert!(
            fields.contains(&name.to_string()),
            "missing field: {name}"
        );
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// get() tests
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn get_bare_f64() {
    let b = Bare {
        x: 2.5,
        name: "test".into(),
    };
    assert_eq!(ExternObject::get(&b, "x").unwrap(), RuntimeValue::Float(2.5));
}

#[test]
fn get_bare_string() {
    let b = Bare {
        x: 0.0,
        name: "hello".into(),
    };
    match ExternObject::get(&b, "name").unwrap() {
        RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "hello"),
        other => panic!("expected Str, got {other:?}"),
    }
}

#[test]
fn get_readonly_field_succeeds() {
    let w = WithReadonly {
        mutable_field: 5,
        locked: "readonly-val".into(),
    };
    match ExternObject::get(&w, "locked").unwrap() {
        RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "readonly-val"),
        other => panic!("expected Str, got {other:?}"),
    }
}

#[test]
fn get_skipped_field_errors() {
    let w = WithSkip {
        visible: 1,
        hidden: "nope".into(),
    };
    let err = ExternObject::get(&w, "hidden").unwrap_err();
    assert!(
        err.contains("hidden"),
        "error should mention the field name: {err}"
    );
}

#[test]
fn get_unknown_field_errors() {
    let b = Bare {
        x: 0.0,
        name: String::new(),
    };
    let err = ExternObject::get(&b, "nonexistent").unwrap_err();
    assert!(
        err.contains("nonexistent"),
        "error should mention the field name: {err}"
    );
}

#[test]
fn get_renamed_field_uses_script_name() {
    let w = WithRename {
        internal_name: 7.7,
    };
    assert_eq!(
        ExternObject::get(&w, "public_name").unwrap(),
        RuntimeValue::Float(7.7)
    );
}

#[test]
fn get_renamed_field_rejects_rust_name() {
    let w = WithRename {
        internal_name: 1.0,
    };
    assert!(
        ExternObject::get(&w, "internal_name").is_err(),
        "Rust ident must not be usable as a script field name"
    );
}

#[test]
fn get_kitchen_sink_all_types() {
    let ks = KitchenSink {
        float_field: 1.5,
        string_field: "abc".into(),
        bool_field: true,
        int_field: -42,
        uint_field: 99,
        list_ints: vec![1, 2, 3],
        list_strings: vec!["a".into(), "b".into()],
    };

    assert_eq!(
        ExternObject::get(&ks, "float_field").unwrap(),
        RuntimeValue::Float(1.5)
    );
    match ExternObject::get(&ks, "string_field").unwrap() {
        RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "abc"),
        other => panic!("expected Str, got {other:?}"),
    }
    assert_eq!(
        ExternObject::get(&ks, "bool_field").unwrap(),
        RuntimeValue::Bool(true)
    );
    assert_eq!(
        ExternObject::get(&ks, "int_field").unwrap(),
        RuntimeValue::Int(-42)
    );
    assert_eq!(
        ExternObject::get(&ks, "uint_field").unwrap(),
        RuntimeValue::Int(99)
    );
    assert_eq!(
        ExternObject::get(&ks, "list_ints").unwrap(),
        RuntimeValue::list(vec![
            RuntimeValue::Int(1),
            RuntimeValue::Int(2),
            RuntimeValue::Int(3),
        ])
    );
    // List of strings
    match ExternObject::get(&ks, "list_strings").unwrap() {
        RuntimeValue::List(items) => {
            assert_eq!(items.borrow().len(), 2);
        }
        other => panic!("expected List, got {other:?}"),
    }
}

#[test]
fn get_readonly_renamed_field() {
    let rr = ReadonlyRenamed { original: 123 };
    assert_eq!(
        ExternObject::get(&rr, "alias").unwrap(),
        RuntimeValue::Int(123)
    );
    assert!(
        ExternObject::get(&rr, "original").is_err(),
        "Rust ident must not be usable as a script field name"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
// set() tests
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn set_bare_field() {
    let mut b = Bare {
        x: 0.0,
        name: "old".into(),
    };
    ExternObject::set(&mut b, "x", RuntimeValue::Float(9.9)).unwrap();
    assert_eq!(ExternObject::get(&b, "x").unwrap(), RuntimeValue::Float(9.9));
}

#[test]
fn set_string_field() {
    let mut b = Bare {
        x: 0.0,
        name: "old".into(),
    };
    ExternObject::set(
        &mut b,
        "name",
        RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("new")),
    )
    .unwrap();
    match ExternObject::get(&b, "name").unwrap() {
        RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "new"),
        other => panic!("expected Str, got {other:?}"),
    }
}

#[test]
fn set_readonly_field_errors() {
    let mut w = WithReadonly {
        mutable_field: 5,
        locked: "nope".into(),
    };
    let err = ExternObject::set(
        &mut w,
        "locked",
        RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("attempt")),
    )
    .unwrap_err();
    assert!(
        err.contains("read-only"),
        "error should mention read-only: {err}"
    );
}

#[test]
fn set_readonly_field_does_not_mutate() {
    let mut w = WithReadonly {
        mutable_field: 5,
        locked: "original".into(),
    };
    let _ = ExternObject::set(
        &mut w,
        "locked",
        RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("hacked")),
    );
    match ExternObject::get(&w, "locked").unwrap() {
        RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "original"),
        other => panic!("expected Str, got {other:?}"),
    }
}

#[test]
fn set_mutable_field_alongside_readonly() {
    let mut w = WithReadonly {
        mutable_field: 5,
        locked: "safe".into(),
    };
    ExternObject::set(&mut w, "mutable_field", RuntimeValue::Int(99)).unwrap();
    assert_eq!(
        ExternObject::get(&w, "mutable_field").unwrap(),
        RuntimeValue::Int(99)
    );
}

#[test]
fn set_skipped_field_errors() {
    let mut w = WithSkip {
        visible: 1,
        hidden: "secret".into(),
    };
    let err = ExternObject::set(
        &mut w,
        "hidden",
        RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("attempt")),
    )
    .unwrap_err();
    assert!(
        err.contains("hidden"),
        "error should mention the field name: {err}"
    );
}

#[test]
fn set_unknown_field_errors() {
    let mut b = Bare {
        x: 0.0,
        name: String::new(),
    };
    let err = ExternObject::set(&mut b, "bogus", RuntimeValue::Int(1)).unwrap_err();
    assert!(
        err.contains("bogus"),
        "error should mention the field name: {err}"
    );
}

#[test]
fn set_wrong_type_errors() {
    let mut w = WithTypeName { value: 10 };
    // `value` is i32, passing a Bool should fail.
    let err = ExternObject::set(&mut w, "value", RuntimeValue::Bool(true)).unwrap_err();
    assert!(!err.is_empty(), "error message should not be empty");
}

#[test]
fn set_renamed_field_uses_script_name() {
    let mut w = WithRename {
        internal_name: 0.0,
    };
    ExternObject::set(&mut w, "public_name", RuntimeValue::Float(42.0)).unwrap();
    assert_eq!(
        ExternObject::get(&w, "public_name").unwrap(),
        RuntimeValue::Float(42.0)
    );
}

#[test]
fn set_renamed_field_rejects_rust_name() {
    let mut w = WithRename {
        internal_name: 0.0,
    };
    assert!(
        ExternObject::set(&mut w, "internal_name", RuntimeValue::Float(1.0)).is_err(),
        "Rust ident must not be usable as a script field name"
    );
}

#[test]
fn set_readonly_renamed_field_errors() {
    let mut rr = ReadonlyRenamed { original: 1 };
    let err = ExternObject::set(&mut rr, "alias", RuntimeValue::Int(999)).unwrap_err();
    assert!(
        err.contains("read-only"),
        "error should mention read-only: {err}"
    );
}

#[test]
fn set_kitchen_sink_round_trips() {
    let mut ks = KitchenSink {
        float_field: 0.0,
        string_field: String::new(),
        bool_field: false,
        int_field: 0,
        uint_field: 0,
        list_ints: vec![],
        list_strings: vec![],
    };

    ExternObject::set(&mut ks, "float_field", RuntimeValue::Float(2.5)).unwrap();
    assert_eq!(
        ExternObject::get(&ks, "float_field").unwrap(),
        RuntimeValue::Float(2.5)
    );

    ExternObject::set(&mut ks, "bool_field", RuntimeValue::Bool(true)).unwrap();
    assert_eq!(
        ExternObject::get(&ks, "bool_field").unwrap(),
        RuntimeValue::Bool(true)
    );

    ExternObject::set(&mut ks, "int_field", RuntimeValue::Int(7)).unwrap();
    assert_eq!(
        ExternObject::get(&ks, "int_field").unwrap(),
        RuntimeValue::Int(7)
    );

    ExternObject::set(&mut ks, "uint_field", RuntimeValue::Int(255)).unwrap();
    assert_eq!(
        ExternObject::get(&ks, "uint_field").unwrap(),
        RuntimeValue::Int(255)
    );

    let new_list = RuntimeValue::list(vec![RuntimeValue::Int(10), RuntimeValue::Int(20)]);
    ExternObject::set(&mut ks, "list_ints", new_list).unwrap();
    assert_eq!(
        ExternObject::get(&ks, "list_ints").unwrap(),
        RuntimeValue::list(vec![RuntimeValue::Int(10), RuntimeValue::Int(20)])
    );
}

#[test]
fn set_all_skipped_unknown_field_errors() {
    let mut g = AllSkipped { secret: 1 };
    assert!(ExternObject::set(&mut g, "secret", RuntimeValue::Int(2)).is_err());
    assert!(ExternObject::set(&mut g, "anything", RuntimeValue::Int(2)).is_err());
}

#[test]
fn set_multi_readonly_rejects_all_readonly() {
    let mut mr = MultiReadonly {
        a: 1,
        b: "x".into(),
        writable: 0.0,
    };
    assert!(ExternObject::set(&mut mr, "a", RuntimeValue::Int(2))
        .unwrap_err()
        .contains("read-only"));
    assert!(ExternObject::set(
        &mut mr,
        "b",
        RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("y"))
    )
    .unwrap_err()
    .contains("read-only"));
    // The writable field should succeed.
    ExternObject::set(&mut mr, "writable", RuntimeValue::Float(3.15)).unwrap();
    assert_eq!(
        ExternObject::get(&mr, "writable").unwrap(),
        RuntimeValue::Float(3.15)
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
// display() tests
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn display_bare_contains_type_and_fields() {
    let b = Bare {
        x: 1.5,
        name: "world".into(),
    };
    let d = ExternObject::display(&b);
    assert!(d.contains("Bare"), "display should contain type name: {d}");
    assert!(d.contains("x: 1.5"), "display should contain x field: {d}");
    assert!(
        d.contains("name: \"world\""),
        "display should contain name field: {d}"
    );
}

#[test]
fn display_custom_type_name() {
    let w = WithTypeName { value: 7 };
    let d = ExternObject::display(&w);
    assert!(
        d.contains("CustomName"),
        "display should use overridden type name: {d}"
    );
    assert!(
        !d.contains("WithTypeName"),
        "display should not use Rust struct name: {d}"
    );
}

#[test]
fn display_skipped_field_absent() {
    let w = WithSkip {
        visible: 10,
        hidden: "secret".into(),
    };
    let d = ExternObject::display(&w);
    assert!(
        !d.contains("hidden"),
        "display must not contain skipped field: {d}"
    );
    assert!(
        !d.contains("secret"),
        "display must not contain skipped field value: {d}"
    );
    assert!(
        d.contains("visible: 10"),
        "display should contain visible field: {d}"
    );
}

#[test]
fn display_renamed_field_uses_script_name() {
    let w = WithRename {
        internal_name: 3.15,
    };
    let d = ExternObject::display(&w);
    assert!(
        d.contains("public_name: 3.15"),
        "display should use renamed field name: {d}"
    );
    assert!(
        !d.contains("internal_name"),
        "display must not contain Rust field name: {d}"
    );
}

#[test]
fn display_all_skipped_is_minimal() {
    let g = AllSkipped { secret: 42 };
    let d = ExternObject::display(&g);
    assert!(
        d.contains("Ghost"),
        "display should contain type name: {d}"
    );
    assert!(
        !d.contains("secret"),
        "display must not contain skipped field: {d}"
    );
}

#[test]
fn display_full_combo() {
    let p = FullCombo {
        hp: 100,
        name: "Hero".into(),
        id: 1,
        internal_counter: 999,
        position_x: 3.5,
    };
    let d = ExternObject::display(&p);
    assert!(d.contains("Player"), "display should contain type name: {d}");
    assert!(d.contains("hp: 100"), "display should contain hp: {d}");
    assert!(
        d.contains("name: \"Hero\""),
        "display should contain name: {d}"
    );
    assert!(d.contains("id: 1"), "display should contain id: {d}");
    assert!(
        d.contains("pos_x: 3.5"),
        "display should contain renamed field: {d}"
    );
    assert!(
        !d.contains("internal_counter"),
        "display must not contain skipped field: {d}"
    );
    assert!(
        !d.contains("999"),
        "display must not contain skipped field value: {d}"
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
// Edge cases / regression guards
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn get_error_message_includes_type_name() {
    let w = WithTypeName { value: 1 };
    let err = ExternObject::get(&w, "no_such").unwrap_err();
    assert!(
        err.contains("CustomName"),
        "error should mention type name: {err}"
    );
}

#[test]
fn set_error_message_includes_type_name() {
    let mut w = WithTypeName { value: 1 };
    let err = ExternObject::set(&mut w, "no_such", RuntimeValue::Int(0)).unwrap_err();
    assert!(
        err.contains("CustomName"),
        "error should mention type name: {err}"
    );
}

#[test]
fn readonly_error_message_includes_field_and_type() {
    let mut w = WithReadonly {
        mutable_field: 0,
        locked: "x".into(),
    };
    let err = ExternObject::set(
        &mut w,
        "locked",
        RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("y")),
    )
    .unwrap_err();
    assert!(
        err.contains("locked"),
        "error should mention field name: {err}"
    );
    assert!(
        err.contains("WithReadonly"),
        "error should mention type name: {err}"
    );
}

#[test]
fn f64_field_accepts_int_via_from_runtime_value() {
    // FromRuntimeValue for f64 accepts RuntimeValue::Int — verify the derive
    // macro's generated set() passes through correctly.
    let mut b = Bare {
        x: 0.0,
        name: String::new(),
    };
    ExternObject::set(&mut b, "x", RuntimeValue::Int(5)).unwrap();
    assert_eq!(
        ExternObject::get(&b, "x").unwrap(),
        RuntimeValue::Float(5.0)
    );
}

#[test]
fn fields_order_matches_struct_definition() {
    let p = FullCombo {
        hp: 1,
        name: "x".into(),
        id: 2,
        internal_counter: 0,
        position_x: 0.0,
    };
    let fields = ExternObject::fields(&p);
    // internal_counter is skipped, so expected order is: hp, name, id, pos_x
    assert_eq!(fields, vec!["hp", "name", "id", "pos_x"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// trybuild compile-fail tests
// ═══════════════════════════════════════════════════════════════════════════════

#[test]
fn compile_fail_tests() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
