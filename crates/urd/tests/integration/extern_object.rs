//! # Integration tests: `ExternObject` derive macro & extern handle field access
//!
//! Exercises the full **parse → compile → VM** pipeline for the `ExternObject`
//! trait, the `#[derive(ExternObject)]` macro, and the `RuntimeValue::Extern`
//! variant — verifying that scripts can read fields, write fields, call
//! built-in methods, and use extern objects in string interpolation.

#![allow(missing_docs)]

use std::sync::{Arc, RwLock};

use urd::{
    Event, ExternHandle, ExternObject, FromRuntimeValue, IntoRuntimeValue, RuntimeValue, VmError,
    VmStep,
    compiler::{Compiler, loader::parse_source},
    vm::{Vm, registry::DecoratorRegistry},
};

// ── Test objects ──────────────────────────────────────────────────────────────

/// A simple player object using the derive macro with all attribute flavours.
#[derive(ExternObject)]
#[extern_object(type_name = "Player")]
struct Player {
    hp: i32,
    name: String,

    #[extern_object(readonly)]
    id: u64,

    #[extern_object(skip)]
    #[allow(dead_code)]
    internal_counter: u32,

    #[extern_object(rename = "pos_x")]
    position_x: f64,
}

impl Player {
    fn new(id: u64, name: &str, hp: i32, pos_x: f64) -> Self {
        Self {
            hp,
            name: name.to_string(),
            id,
            internal_counter: 0,
            position_x: pos_x,
        }
    }
}

/// A minimal manually-implemented extern object for comparison / edge cases.
struct ManualExtern {
    value: i64,
}

impl ExternObject for ManualExtern {
    fn type_name(&self) -> &str {
        "ManualExtern"
    }

    fn display(&self) -> String {
        format!("ManualExtern({})", self.value)
    }

    fn get(&self, field: &str) -> Result<RuntimeValue, String> {
        match field {
            "value" => Ok(RuntimeValue::Int(self.value)),
            other => Err(format!("no field '{other}' on ManualExtern")),
        }
    }

    fn set(&mut self, field: &str, value: RuntimeValue) -> Result<(), String> {
        match field {
            "value" => {
                self.value = i64::from_runtime_value(&value)?;
                Ok(())
            }
            other => Err(format!("no field '{other}' on ManualExtern")),
        }
    }

    fn fields(&self) -> Vec<String> {
        vec!["value".to_string()]
    }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Parse, compile, inject externs, and drive the VM to completion.
#[allow(clippy::expect_used)]
fn run_script_with_externs(src: &str, setup: impl FnOnce(&mut Vm)) -> Vec<VmStep> {
    let ast = parse_source(src).expect("script should parse");
    let graph = Compiler::compile(&ast).expect("script should compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm should initialise");
    setup(&mut vm);
    drive_vm(&mut vm)
}

fn drive_vm(vm: &mut Vm) -> Vec<VmStep> {
    let mut steps = Vec::new();
    for _ in 0..1024 {
        let step = vm.next(None);
        let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
        steps.push(step);
        if terminal {
            break;
        }
    }
    steps
}

/// Collect every dialogue line (as a plain `String`) from a step sequence.
fn dialogue_texts(steps: &[VmStep]) -> Vec<String> {
    let mut out = Vec::new();
    for step in steps {
        if let VmStep::Event(Event::Dialogue { lines, .. }) = step {
            for val in lines {
                if let RuntimeValue::Str(ps) = val {
                    out.push(ps.to_string());
                }
            }
        }
    }
    out
}

/// Return the first [`VmError`] in the step sequence, if any.
fn first_error(steps: &[VmStep]) -> Option<&VmError> {
    steps.iter().find_map(|s| {
        if let VmStep::Error(e) = s {
            Some(e)
        } else {
            None
        }
    })
}

// ── Derive macro: basic trait generation ──────────────────────────────────────

#[test]
fn derive_type_name() {
    let p = Player::new(1, "Hero", 100, 3.5);
    assert_eq!(ExternObject::type_name(&p), "Player");
}

#[test]
fn derive_display_contains_fields() {
    let p = Player::new(1, "Hero", 100, 3.5);
    let d = ExternObject::display(&p);
    assert!(
        d.contains("Player"),
        "display should contain type name: {d}"
    );
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
    // Skipped field must NOT appear.
    assert!(
        !d.contains("internal_counter"),
        "display must not contain skipped field: {d}"
    );
}

#[test]
fn derive_get_field() {
    let p = Player::new(42, "Zara", 75, 1.5);
    assert_eq!(ExternObject::get(&p, "hp").unwrap(), RuntimeValue::Int(75));
    assert_eq!(ExternObject::get(&p, "id").unwrap(), RuntimeValue::Int(42));
    assert_eq!(
        ExternObject::get(&p, "pos_x").unwrap(),
        RuntimeValue::Float(1.5)
    );
    match ExternObject::get(&p, "name").unwrap() {
        RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "Zara"),
        other => panic!("expected Str, got {other:?}"),
    }
}

#[test]
fn derive_get_skipped_field_errors() {
    let p = Player::new(1, "x", 10, 0.0);
    assert!(ExternObject::get(&p, "internal_counter").is_err());
}

#[test]
fn derive_get_unknown_field_errors() {
    let p = Player::new(1, "x", 10, 0.0);
    assert!(ExternObject::get(&p, "nonexistent").is_err());
}

#[test]
fn derive_set_field() {
    let mut p = Player::new(1, "Hero", 100, 0.0);
    ExternObject::set(&mut p, "hp", RuntimeValue::Int(50)).unwrap();
    assert_eq!(ExternObject::get(&p, "hp").unwrap(), RuntimeValue::Int(50));
}

#[test]
fn derive_set_readonly_field_errors() {
    let mut p = Player::new(1, "Hero", 100, 0.0);
    let result = ExternObject::set(&mut p, "id", RuntimeValue::Int(999));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("read-only"));
}

#[test]
fn derive_set_skipped_field_errors() {
    let mut p = Player::new(1, "Hero", 100, 0.0);
    assert!(ExternObject::set(&mut p, "internal_counter", RuntimeValue::Int(5)).is_err());
}

#[test]
fn derive_set_wrong_type_errors() {
    let mut p = Player::new(1, "Hero", 100, 0.0);
    // hp is i32 — setting a Str should fail.
    assert!(ExternObject::set(&mut p, "hp", RuntimeValue::Bool(true)).is_err());
}

#[test]
fn derive_set_renamed_field() {
    let mut p = Player::new(1, "Hero", 100, 0.0);
    ExternObject::set(&mut p, "pos_x", RuntimeValue::Float(9.9)).unwrap();
    assert_eq!(
        ExternObject::get(&p, "pos_x").unwrap(),
        RuntimeValue::Float(9.9)
    );
}

#[test]
fn derive_fields_list() {
    let p = Player::new(1, "x", 10, 0.0);
    let fields = ExternObject::fields(&p);
    assert!(fields.contains(&"hp".to_string()));
    assert!(fields.contains(&"name".to_string()));
    assert!(fields.contains(&"id".to_string()));
    assert!(fields.contains(&"pos_x".to_string()));
    assert!(!fields.contains(&"internal_counter".to_string()));
    assert!(!fields.contains(&"position_x".to_string()));
}

// ── ExternHandle: shared identity and mutation ────────────────────────────────

#[test]
fn handle_mutation_visible_through_clones() {
    let h1 = ExternHandle::new(Player::new(1, "Hero", 100, 0.0));
    let h2 = h1.clone();
    h1.set("hp", RuntimeValue::Int(50)).unwrap();
    assert_eq!(h2.get("hp").unwrap(), RuntimeValue::Int(50));
}

#[test]
fn handle_from_arc_preserves_identity() {
    let arc: Arc<RwLock<dyn ExternObject>> = Arc::new(RwLock::new(Player::new(1, "x", 10, 0.0)));
    let h1 = ExternHandle::from_arc(Arc::clone(&arc));
    let h2 = ExternHandle::from_arc(arc);
    assert_eq!(h1, h2);
}

// ── VM integration: field reads via dot notation ──────────────────────────────

#[test]
fn extern_object_field_read_in_dialogue() {
    let src = r#"
extern player

@entry
label start {
    narrator: "HP is {player.hp}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["HP is 100"]);
}

#[test]
fn extern_object_multiple_field_reads() {
    let src = r#"
extern player

@entry
label start {
    narrator: "{player.name} has {player.hp} HP"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Zara", 75, 0.0))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Zara has 75 HP"]);
}

#[test]
fn extern_object_renamed_field_read() {
    let src = r#"
extern player

@entry
label start {
    narrator: "X: {player.pos_x}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 2.72))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["X: 2.72"]);
}

// ── VM integration: field reads via subscript ─────────────────────────────────

#[test]
fn extern_object_subscript_read() {
    let src = r#"
extern player

@entry
label start {
    let h = player["hp"]
    narrator: "HP is {h}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 42, 0.0))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["HP is 42"]);
}

// ── VM integration: field writes via subscript assign ─────────────────────────

#[test]
fn extern_object_subscript_write() {
    let src = r#"
extern player

@entry
label start {
    player["hp"] = 50
    narrator: "HP is {player.hp}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["HP is 50"]);
}

#[test]
fn extern_object_write_readonly_field_errors() {
    let src = r#"
extern player

@entry
label start {
    player["id"] = 999
    narrator: "should not reach"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    let err = first_error(&steps);
    assert!(err.is_some(), "expected an error for readonly write");
    let msg = format!("{}", err.unwrap());
    assert!(
        msg.contains("read-only"),
        "error should mention read-only: {msg}"
    );
}

#[test]
fn extern_object_write_unknown_field_errors() {
    let src = r#"
extern player

@entry
label start {
    player["nonexistent"] = 1
    narrator: "should not reach"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    let err = first_error(&steps);
    assert!(err.is_some(), "expected an error for unknown field write");
}

// ── VM integration: mutations visible across references ───────────────────────

#[test]
fn extern_object_mutation_visible_through_shared_handle() {
    let src = r#"
extern player

@entry
label start {
    player["hp"] = player.hp - 10
    narrator: "After hit: {player.hp}"
    player["hp"] = player.hp - 25
    narrator: "After crit: {player.hp}"
    end!
}
"#;
    let handle = ExternHandle::new(Player::new(1, "Hero", 100, 0.0));
    let handle_clone = handle.clone();

    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("player", RuntimeValue::Extern(handle_clone));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["After hit: 90", "After crit: 65"]);

    // The host's handle sees the mutations too.
    assert_eq!(handle.get("hp").unwrap(), RuntimeValue::Int(65));
}

// ── VM integration: string interpolation / display ────────────────────────────

#[test]
fn extern_object_in_string_interpolation() {
    let src = r#"
extern obj

@entry
label start {
    narrator: "Object: {obj}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "obj",
            RuntimeValue::Extern(ExternHandle::new(ManualExtern { value: 42 })),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Object: ManualExtern(42)"]);
}

// ── VM integration: method calls ──────────────────────────────────────────────

#[test]
fn extern_object_to_string_method() {
    let src = r#"
extern player

@entry
label start {
    let s = player.to_string()
    narrator: "{s}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 3.5))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts.len(), 1);
    assert!(
        texts[0].contains("Player"),
        "to_string should contain type name"
    );
    assert!(
        texts[0].contains("hp: 100"),
        "to_string should contain fields"
    );
}

#[test]
fn extern_object_type_name_method() {
    let src = r#"
extern player

@entry
label start {
    let t = player.type_name()
    narrator: "{t}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Player"]);
}

#[test]
fn extern_object_fields_method() {
    let src = r#"
extern player

@entry
label start {
    let f = player.fields()
    narrator: "{f}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts.len(), 1);
    assert!(
        texts[0].contains("hp"),
        "fields list should contain hp: {}",
        texts[0]
    );
    assert!(
        texts[0].contains("name"),
        "fields list should contain name: {}",
        texts[0]
    );
    assert!(
        texts[0].contains("pos_x"),
        "fields list should contain pos_x: {}",
        texts[0]
    );
}

#[test]
fn extern_object_unknown_method_errors() {
    let src = r#"
extern player

@entry
label start {
    let x = player.nonexistent_method()
    narrator: "should not reach"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    let err = first_error(&steps);
    assert!(err.is_some(), "unknown method should error");
}

// ── VM integration: extern object used in conditional ─────────────────────────

#[test]
fn extern_object_field_in_conditional() {
    let src = r#"
extern player

@entry
label start {
    if player.hp > 50 {
        narrator: "Healthy"
    } else {
        narrator: "Wounded"
    }
    end!
}
"#;
    let steps_high = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    assert_eq!(dialogue_texts(&steps_high), vec!["Healthy"]);

    let steps_low = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 30, 0.0))),
        );
    });
    assert_eq!(dialogue_texts(&steps_low), vec!["Wounded"]);
}

// ── VM integration: extern object is truthy ───────────────────────────────────

#[test]
fn extern_object_is_truthy() {
    let src = r#"
extern player

@entry
label start {
    if player {
        narrator: "exists"
    } else {
        narrator: "null"
    }
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    assert_eq!(dialogue_texts(&steps), vec!["exists"]);
}

// ── VM integration: extern object equality is by identity ─────────────────────

#[test]
fn extern_object_equality_is_identity() {
    let src = r#"
extern a
extern b

@entry
label start {
    if a == b {
        narrator: "same"
    } else {
        narrator: "different"
    }
    end!
}
"#;
    let handle = ExternHandle::new(Player::new(1, "Hero", 100, 0.0));

    // Same handle → equal.
    let steps_same = run_script_with_externs(src, |vm| {
        vm.provide_extern("a", RuntimeValue::Extern(handle.clone()));
        vm.provide_extern("b", RuntimeValue::Extern(handle.clone()));
    });
    assert_eq!(dialogue_texts(&steps_same), vec!["same"]);

    // Different handles (even with identical data) → not equal.
    let steps_diff = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "a",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
        vm.provide_extern(
            "b",
            RuntimeValue::Extern(ExternHandle::new(Player::new(1, "Hero", 100, 0.0))),
        );
    });
    assert_eq!(dialogue_texts(&steps_diff), vec!["different"]);
}

// ── VM integration: manual ExternObject impl ──────────────────────────────────

#[test]
fn manual_extern_object_read_write() {
    let src = r#"
extern counter

@entry
label start {
    narrator: "Before: {counter.value}"
    counter["value"] = counter.value + 1
    narrator: "After: {counter.value}"
    end!
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "counter",
            RuntimeValue::Extern(ExternHandle::new(ManualExtern { value: 10 })),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Before: 10", "After: 11"]);
}

// ── Host-side mutation between VM steps ───────────────────────────────────────

#[test]
fn host_mutates_extern_between_steps() {
    let src = r#"
extern counter

@entry
label start {
    narrator: "Value: {counter.value}"
    narrator: "Value: {counter.value}"
    end!
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init");

    let handle = ExternHandle::new(ManualExtern { value: 1 });
    vm.provide_extern("counter", RuntimeValue::Extern(handle.clone()));

    // First step: should produce "Value: 1".
    let step1 = vm.next(None);
    if let VmStep::Event(Event::Dialogue { lines, .. }) = &step1 {
        if let RuntimeValue::Str(ps) = &lines[0] {
            assert_eq!(ps.to_string(), "Value: 1");
        } else {
            panic!("expected Str line");
        }
    } else {
        panic!("expected Dialogue event, got {step1:?}");
    }

    // Host mutates the extern between steps.
    handle.set("value", RuntimeValue::Int(42)).unwrap();

    // Second step: should see the updated value.
    let step2 = vm.next(None);
    if let VmStep::Event(Event::Dialogue { lines, .. }) = &step2 {
        if let RuntimeValue::Str(ps) = &lines[0] {
            assert_eq!(ps.to_string(), "Value: 42");
        } else {
            panic!("expected Str line");
        }
    } else {
        panic!("expected Dialogue event, got {step2:?}");
    }
}

// ── Conversion traits: round-trip property ────────────────────────────────────

#[test]
fn conversion_round_trip_i32() {
    let val: i32 = 42;
    let rv = val.to_runtime_value();
    assert_eq!(i32::from_runtime_value(&rv).unwrap(), 42);
}

#[test]
fn conversion_round_trip_string() {
    let val = "hello".to_string();
    let rv = val.to_runtime_value();
    assert_eq!(String::from_runtime_value(&rv).unwrap(), "hello");
}

#[test]
fn conversion_round_trip_bool() {
    let rv = true.to_runtime_value();
    assert!(bool::from_runtime_value(&rv).unwrap());
}

#[test]
fn conversion_round_trip_f64() {
    let val: f64 = 2.72;
    let rv = val.to_runtime_value();
    let back = f64::from_runtime_value(&rv).unwrap();
    assert!((back - 2.72).abs() < f64::EPSILON);
}

#[test]
fn conversion_round_trip_option_some() {
    let val: Option<i64> = Some(7);
    let rv = val.to_runtime_value();
    assert_eq!(Option::<i64>::from_runtime_value(&rv).unwrap(), Some(7));
}

#[test]
fn conversion_round_trip_option_none() {
    let val: Option<i64> = None;
    let rv = val.to_runtime_value();
    assert_eq!(Option::<i64>::from_runtime_value(&rv).unwrap(), None);
}

#[test]
fn conversion_round_trip_vec() {
    let val: Vec<i64> = vec![1, 2, 3];
    let rv = val.to_runtime_value();
    assert_eq!(Vec::<i64>::from_runtime_value(&rv).unwrap(), vec![1, 2, 3]);
}

#[test]
fn conversion_i32_overflow_errors() {
    let rv = RuntimeValue::Int(i64::MAX);
    assert!(i32::from_runtime_value(&rv).is_err());
}

#[test]
fn conversion_u64_negative_errors() {
    let rv = RuntimeValue::Int(-1);
    assert!(u64::from_runtime_value(&rv).is_err());
}

#[test]
fn conversion_f64_accepts_int() {
    // f64::from_runtime_value should coerce Int to Float.
    let rv = RuntimeValue::Int(5);
    assert_eq!(f64::from_runtime_value(&rv).unwrap(), 5.0);
}
