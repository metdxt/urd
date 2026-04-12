//! # Integration tests: string format specifiers & extern values
//!
//! Exercises the full **parse → compile → VM** pipeline for two features:
//!
//! 1. **String format specifiers** — `{expr:.2}`, `{expr:04}`, etc.
//! 2. **Extern declarations** — `extern name: type`, host-provided values
//!    injected via [`Vm::provide_extern`].

#![allow(missing_docs)]

use urd::{
    Event, RuntimeValue, VmError, VmStep,
    compiler::{Compiler, loader::parse_source},
    lexer::strings::ParsedString,
    vm::{Vm, registry::DecoratorRegistry},
};

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion (or the first terminal step),
/// returning every [`VmStep`] observed.  Capped at 1024 steps to prevent
/// infinite loops in broken scripts.
#[allow(clippy::expect_used)]
fn run_script(src: &str) -> Vec<VmStep> {
    let ast = parse_source(src).expect("script should parse");
    let graph = Compiler::compile(&ast).expect("script should compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm should initialise");
    drive_vm(&mut vm)
}

/// Like [`run_script`] but accepts a callback that can call
/// [`Vm::provide_extern`] before the first step.
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

// ═══════════════════════════════════════════════════════════════════════════════
// §1  String format specifiers
// ═══════════════════════════════════════════════════════════════════════════════

// ── Float formatting ──────────────────────────────────────────────────────────

#[test]
fn fmt_float_two_decimal_places() {
    let src = r#"
@entry
label start {
    let price = 3.14159
    Narrator: "Price: {price:.2}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["Price: 3.14"]);
}

#[test]
fn fmt_float_zero_decimal_places() {
    let src = r#"
@entry
label start {
    let val = 9.87654
    Narrator: "{val:.0}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["10"]);
}

#[test]
fn fmt_float_four_decimal_places_with_trailing_zeros() {
    let src = r#"
@entry
label start {
    let x = 1.5
    Narrator: "{x:.4}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["1.5000"]);
}

#[test]
fn fmt_float_one_decimal_rounds_up() {
    let src = r#"
@entry
label start {
    let val = 2.95
    Narrator: "{val:.1}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    // 2.95 rounds to 3.0 at one decimal place
    assert_eq!(texts, vec!["3.0"]);
}

// ── Integer zero-padding ─────────────────────────────────────────────────────

#[test]
fn fmt_int_zero_pad_four_digits() {
    let src = r#"
@entry
label start {
    let count = 42
    Narrator: "ID: {count:04}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["ID: 0042"]);
}

#[test]
fn fmt_int_zero_pad_already_wider_than_spec() {
    let src = r#"
@entry
label start {
    let big = 123456
    Narrator: "{big:04}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    // Value already exceeds width — no truncation.
    assert_eq!(texts, vec!["123456"]);
}

#[test]
fn fmt_int_zero_pad_single_digit_width() {
    let src = r#"
@entry
label start {
    let n = 7
    Narrator: "{n:01}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["7"]);
}

#[test]
fn fmt_int_zero_pad_six_digits() {
    let src = r##"
@entry
label start {
    let id = 1
    Narrator: "#{id:06}"
    end!()
}
"##;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["#000001"]);
}

// ── Multiple format specs in one string ──────────────────────────────────────

#[test]
fn fmt_multiple_specs_in_single_string() {
    let src = r#"
@entry
label start {
    let price = 9.99
    let qty = 3
    Narrator: "{qty:03}x items @ {price:.2} each"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["003x items @ 9.99 each"]);
}

#[test]
fn fmt_mixed_formatted_and_plain_interpolations() {
    let src = r#"
@entry
label start {
    let name = "sword"
    let cost = 49.9
    let stock = 5
    Narrator: "{name} - {cost:.1} gold ({stock:03} left)"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["sword - 49.9 gold (005 left)"]);
}

// ── No format spec (baseline) ────────────────────────────────────────────────

#[test]
fn fmt_no_spec_float_uses_default_display() {
    let src = r#"
@entry
label start {
    let val = 3.14
    Narrator: "pi is {val}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["pi is 3.14"]);
}

#[test]
fn fmt_no_spec_int_uses_default_display() {
    let src = r#"
@entry
label start {
    let n = 42
    Narrator: "answer: {n}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["answer: 42"]);
}

#[test]
fn fmt_no_spec_bool_uses_default_display() {
    let src = r#"
@entry
label start {
    let flag = true
    Narrator: "flag is {flag}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["flag is true"]);
}

// ── Format spec edge cases ───────────────────────────────────────────────────

#[test]
fn fmt_float_high_precision() {
    let src = r#"
@entry
label start {
    let e = 2.718281828
    Narrator: "{e:.6}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["2.718282"]);
}

#[test]
fn fmt_int_zero_value_padded() {
    let src = r#"
@entry
label start {
    let z = 0
    Narrator: "{z:04}"
    end!()
}
"#;
    let texts = dialogue_texts(&run_script(src));
    assert_eq!(texts, vec!["0000"]);
}

// ═══════════════════════════════════════════════════════════════════════════════
// §2  Extern values
// ═══════════════════════════════════════════════════════════════════════════════

// ── Extern string value ──────────────────────────────────────────────────────

#[test]
fn extern_string_used_in_dialogue() {
    let src = r#"
extern player_name: str

@entry
label start {
    Narrator: "Hello, {player_name}!"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "player_name",
            RuntimeValue::Str(ParsedString::new_plain("Alice")),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Hello, Alice!"]);
    assert!(first_error(&steps).is_none(), "should not error");
}

#[test]
fn extern_string_as_speaker() {
    let src = r#"
extern narrator: str

@entry
label start {
    narrator: "Greetings, traveler."
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "narrator",
            RuntimeValue::Str(ParsedString::new_plain("Guide")),
        );
    });
    // The script should run without errors; the extern is used as a speaker name.
    assert!(first_error(&steps).is_none(), "should not error");

    // Verify the extern value was actually used as the speaker name.
    let dialogue = steps.iter().find_map(|s| {
        if let VmStep::Event(Event::Dialogue {
            speakers, lines, ..
        }) = s
        {
            Some((speakers, lines))
        } else {
            None
        }
    });
    let (speakers, _lines) = dialogue.expect("expected at least one Dialogue event");
    assert_eq!(speakers.len(), 1, "expected exactly one speaker");
    match &speakers[0] {
        RuntimeValue::Str(ps) => assert_eq!(
            ps.to_string(),
            "Guide",
            "speaker should be the extern value 'Guide', got '{}'",
            ps
        ),
        other => panic!("expected speaker to be Str(\"Guide\"), got {:?}", other),
    }
}

// ── Extern int value ─────────────────────────────────────────────────────────

#[test]
fn extern_int_used_in_arithmetic() {
    let src = r#"
extern base_hp: int

@entry
label start {
    let total = base_hp + 50
    Narrator: "Total HP: {total}"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("base_hp", RuntimeValue::Int(100));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Total HP: 150"]);
    assert!(first_error(&steps).is_none());
}

#[test]
fn extern_int_with_format_spec() {
    let src = r#"
extern score: int

@entry
label start {
    Narrator: "Score: {score:06}"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("score", RuntimeValue::Int(999));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Score: 000999"]);
}

// ── Extern bool value ────────────────────────────────────────────────────────

#[test]
fn extern_bool_in_conditional_true_branch() {
    let src = r#"
extern is_vip: bool

@entry
label start {
    if is_vip {
        Narrator: "Welcome, VIP!"
    } else {
        Narrator: "Welcome."
    }
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("is_vip", RuntimeValue::Bool(true));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Welcome, VIP!"]);
}

#[test]
fn extern_bool_in_conditional_false_branch() {
    let src = r#"
extern is_vip: bool

@entry
label start {
    if is_vip {
        Narrator: "Welcome, VIP!"
    } else {
        Narrator: "Welcome."
    }
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("is_vip", RuntimeValue::Bool(false));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Welcome."]);
}

// ── Missing extern → error ───────────────────────────────────────────────────

#[test]
fn extern_not_provided_yields_error() {
    let src = r#"
extern missing_val: int

@entry
label start {
    Narrator: "unreachable"
    end!()
}
"#;
    // Deliberately do NOT provide the extern.
    let steps = run_script(src);
    let err = first_error(&steps).expect("expected ExternNotProvided error");
    match err {
        VmError::ExternNotProvided(name) => {
            assert_eq!(name, "missing_val");
        }
        other => panic!("expected ExternNotProvided, got: {other:?}"),
    }
}

#[test]
fn extern_one_of_two_missing_yields_error() {
    let src = r#"
extern provided_val: int
extern missing_val: str

@entry
label start {
    Narrator: "unreachable"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("provided_val", RuntimeValue::Int(10));
        // missing_val deliberately NOT provided
    });
    let err = first_error(&steps).expect("expected ExternNotProvided error");
    match err {
        VmError::ExternNotProvided(name) => {
            assert_eq!(name, "missing_val");
        }
        other => panic!("expected ExternNotProvided, got: {other:?}"),
    }
}

// ── Multiple extern values ───────────────────────────────────────────────────

#[test]
fn multiple_externs_all_provided() {
    let src = r#"
extern hero: str
extern level: int
extern hp_ratio: float

@entry
label start {
    Narrator: "{hero} (Lv.{level}) — HP {hp_ratio:.0}%"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("hero", RuntimeValue::Str(ParsedString::new_plain("Kael")));
        vm.provide_extern("level", RuntimeValue::Int(12));
        vm.provide_extern("hp_ratio", RuntimeValue::Float(87.5));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Kael (Lv.12) — HP 88%"]);
    assert!(first_error(&steps).is_none());
}

#[test]
fn multiple_externs_used_across_dialogue_lines() {
    let src = r#"
extern greeting: str
extern farewell: str

@entry
label start {
    Narrator: "{greeting}"
    Narrator: "{farewell}"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern(
            "greeting",
            RuntimeValue::Str(ParsedString::new_plain("Hello")),
        );
        vm.provide_extern(
            "farewell",
            RuntimeValue::Str(ParsedString::new_plain("Goodbye")),
        );
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Hello", "Goodbye"]);
}

// ── Extern with format specifier ─────────────────────────────────────────────

#[test]
fn extern_float_with_format_specifier() {
    let src = r#"
extern temperature: float

@entry
label start {
    Narrator: "Current temp: {temperature:.1}°C"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("temperature", RuntimeValue::Float(36.667));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Current temp: 36.7°C"]);
}

// ── Extern cannot be reassigned by script ────────────────────────────────────

#[test]
fn extern_reassignment_by_script_errors() {
    let src = r#"
extern locked: int

@entry
label start {
    locked = 999
    Narrator: "unreachable"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("locked", RuntimeValue::Int(0));
    });
    let err = first_error(&steps).expect("expected error on extern reassignment");
    let msg = err.to_string();
    assert!(
        msg.contains("extern"),
        "error should mention 'extern': {msg}"
    );
}

// ── Extern combined with local variables ─────────────────────────────────────

#[test]
fn extern_and_local_coexist() {
    let src = r#"
extern base_damage: int

@entry
label start {
    let bonus = 15
    let total = base_damage + bonus
    Narrator: "Damage: {total}"
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("base_damage", RuntimeValue::Int(35));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["Damage: 50"]);
}

#[test]
fn extern_in_comparison_with_local() {
    let src = r#"
extern threshold: int

@entry
label start {
    let score = 80
    if score >= threshold {
        Narrator: "pass"
    } else {
        Narrator: "fail"
    }
    end!()
}
"#;
    let steps = run_script_with_externs(src, |vm| {
        vm.provide_extern("threshold", RuntimeValue::Int(70));
    });
    let texts = dialogue_texts(&steps);
    assert_eq!(texts, vec!["pass"]);
}

// ── Extern updated between steps ─────────────────────────────────────────────

#[test]
fn extern_updated_mid_execution() {
    let src = r#"
extern counter: int

@entry
label start {
    Narrator: "before: {counter}"
    Narrator: "after: {counter}"
    end!()
}
"#;
    let ast = parse_source(src).expect("script should parse");
    let graph = Compiler::compile(&ast).expect("script should compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm should initialise");
    vm.provide_extern("counter", RuntimeValue::Int(1));

    let mut texts = Vec::new();
    for _ in 0..1024 {
        let step = vm.next(None);
        match &step {
            VmStep::Event(Event::Dialogue { lines, .. }) => {
                for val in lines {
                    if let RuntimeValue::Str(ps) = val {
                        texts.push(ps.to_string());
                    }
                }
                // After the first dialogue, update the extern for the next one.
                if texts.len() == 1 {
                    vm.provide_extern("counter", RuntimeValue::Int(2));
                }
            }
            VmStep::Ended | VmStep::Error(_) => break,
            _ => {}
        }
    }
    assert_eq!(texts, vec!["before: 1", "after: 2"]);
}
