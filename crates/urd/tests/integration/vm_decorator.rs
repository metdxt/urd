#![allow(missing_docs)]

//! Integration tests for VM error propagation in script-defined decorator bodies.
//!
//! Verifies that an expression inside a decorator body that fails at runtime
//! (e.g. a plain assignment whose RHS references an undefined variable, which
//! hits the `_ =>` arm of `exec_stmt_sync`) propagates `Err(VmError::…)` to
//! the caller rather than being silently swallowed with a `log::warn`.
//!
//! ## Why `x = ghost_var` and not bare `ghost_var`?
//!
//! The Urd statement grammar does **not** accept a standalone identifier as a
//! statement — it always expects an assignment (`=`), subscript (`[`), or
//! declaration keyword after an identifier.  A plain assignment of the form
//! `x = ghost_var` therefore:
//!
//! 1. **Parses correctly** as `BinOp { op: Assign, left: ident("x"),
//!    right: ident("ghost_var") }`.
//! 2. **Is not** a `Declaration` or `SubscriptAssign`, so it falls through to
//!    the `_ =>` arm of `exec_stmt_sync` — precisely the arm that was
//!    previously swallowing errors.
//! 3. **Fails at runtime** because `eval_binop` for `Assign` evaluates the
//!    RHS, and `ghost_var` is not in the environment.

use urd::{Event, RuntimeValue, VmError, VmStep};

// ── Helper ────────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion (or the first error),
/// returning every [`VmStep`] observed.  Caps at 64 steps to prevent infinite
/// loops in broken tests.
fn run_script(src: &str) -> Vec<VmStep> {
    super::fixtures::run_script(src, 64)
}

// ── Tests ─────────────────────────────────────────────────────────────────────

/// A decorator body with a plain assignment whose RHS is an undefined variable
/// (`x = ghost_var`) must cause the VM to emit
/// `VmStep::Error(VmError::UndefinedVariable("ghost_var"))`.
///
/// Before the fix, the `_ =>` arm of `exec_stmt_sync` swallowed the error with
/// `log::warn!` and returned `Ok(())`, hiding the bug.
#[test]
fn test_decorator_body_undefined_rhs_propagates_error() {
    // `x = ghost_var` parses as BinOp(Assign, …) — hits the `_ =>` arm of
    // exec_stmt_sync — and fails at runtime because `ghost_var` is undefined.
    let src = r#"
decorator crash() {
    x = ghost_var
}

@crash
Narrator: "This line must never be emitted."
"#;
    let steps = run_script(src);

    let error_step = steps.iter().find(|s| matches!(s, VmStep::Error(_)));

    match error_step {
        Some(VmStep::Error(VmError::UndefinedVariable(name))) => {
            assert_eq!(
                name, "ghost_var",
                "error should name the offending (undefined) variable"
            );
        }
        Some(other) => panic!(
            "expected VmStep::Error(VmError::UndefinedVariable(\"ghost_var\")), got {:?}",
            other
        ),
        None => panic!(
            "expected a VmStep::Error but the VM succeeded silently; \
             steps: {steps:?}"
        ),
    }
}

/// Execution must be aborted **before** the `Dialogue` event is yielded — the
/// broken decorator body must prevent the event from ever reaching the caller.
#[test]
fn test_decorator_body_error_prevents_dialogue_emission() {
    let src = r#"
decorator crash() {
    x = no_such_var
}

@crash
Alice: "You should never see this."
"#;
    let steps = run_script(src);

    for step in &steps {
        assert!(
            !matches!(step, VmStep::Event(Event::Dialogue { .. })),
            "Dialogue must not be emitted when the decorator body errors; \
             got steps: {steps:?}"
        );
    }

    assert!(
        steps
            .iter()
            .any(|s| matches!(s, VmStep::Error(VmError::UndefinedVariable(_)))),
        "expected at least one UndefinedVariable error step; got: {steps:?}"
    );
}

/// A decorator body that performs a valid `event` subscript assignment must
/// still work correctly — the error-propagation change must not regress the
/// happy path.
#[test]
fn test_decorator_body_valid_assignment_still_works() {
    let src = r#"
decorator tag() {
    event["tagged"] = 1
}

@tag
Bob: "All good."
"#;
    let steps = run_script(src);

    let dialogue_step = steps
        .iter()
        .find(|s| matches!(s, VmStep::Event(Event::Dialogue { .. })));

    assert!(
        dialogue_step.is_some(),
        "expected a Dialogue event from a valid decorator body; got: {steps:?}"
    );

    match dialogue_step.unwrap() {
        VmStep::Event(Event::Dialogue { fields, .. }) => {
            assert!(
                fields.contains_key("tagged"),
                "decorator should have written 'tagged' into event fields; \
                 got: {fields:?}"
            );
            assert_eq!(
                fields.get("tagged"),
                Some(&RuntimeValue::Int(1)),
                "'tagged' field should be Int(1)"
            );
        }
        _ => unreachable!(),
    }
}

/// When two decorators are applied to a single node and the **second** one
/// contains an error, the error must still propagate — it must not be masked
/// by the successful first decorator.
#[test]
fn test_second_decorator_body_error_propagates() {
    let src = r#"
decorator ok() {
    event["ok"] = 1
}

decorator bad() {
    x = missing_var
}

@ok
@bad
Carol: "Two decorators — second one broken."
"#;
    let steps = run_script(src);

    assert!(
        steps
            .iter()
            .any(|s| matches!(s, VmStep::Error(VmError::UndefinedVariable(_)))),
        "expected UndefinedVariable error from the second decorator body; \
         got: {steps:?}"
    );
}

/// A decorator with a body that uses a declared local variable must succeed —
/// verifying that the `_ =>` arm does not erroneously reject valid BinOp nodes
/// whose operands resolve correctly.
#[test]
fn test_decorator_body_assignment_with_declared_variable_succeeds() {
    // `amount` is bound as a decorator parameter, so `x = amount` must succeed.
    let src = r#"
decorator with_param(amount: int) {
    event["level"] = amount
}

@with_param(7)
Dave: "Parameterised decorator."
"#;
    let steps = run_script(src);

    assert!(
        !steps.iter().any(|s| matches!(s, VmStep::Error(_))),
        "parameterised decorator with valid body should not error; got: {steps:?}"
    );

    let dialogue_step = steps
        .iter()
        .find(|s| matches!(s, VmStep::Event(Event::Dialogue { .. })));

    assert!(
        dialogue_step.is_some(),
        "expected a Dialogue event; got: {steps:?}"
    );

    match dialogue_step.unwrap() {
        VmStep::Event(Event::Dialogue { fields, .. }) => {
            assert_eq!(
                fields.get("level"),
                Some(&RuntimeValue::Int(7)),
                "'level' should be Int(7)"
            );
        }
        _ => unreachable!(),
    }
}

// ── Phase 2: Stale-snapshot & arity regression tests ──────────────────────────

/// A decorator body that writes to `event` and then reads back the same key
/// in a subsequent statement must see the updated value — not `Null`.
///
/// Before the fix, `inner_env["event"]` held a stale snapshot taken before body
/// execution, so the read would return `Null` even though the write succeeded.
#[test]
fn test_decorator_write_then_read_in_same_body() {
    let src = r#"
decorator stamp() {
    event["x"] = 42
    event["y"] = event["x"]
}

@stamp
Alice: "write-then-read"
"#;
    let steps = run_script(src);

    assert!(
        !steps.iter().any(|s| matches!(s, VmStep::Error(_))),
        "decorator body should not error; got: {steps:?}"
    );

    let dialogue_step = steps
        .iter()
        .find(|s| matches!(s, VmStep::Event(Event::Dialogue { .. })));

    assert!(
        dialogue_step.is_some(),
        "expected a Dialogue event; got: {steps:?}"
    );

    match dialogue_step.unwrap() {
        VmStep::Event(Event::Dialogue { fields, .. }) => {
            assert_eq!(
                fields.get("x"),
                Some(&RuntimeValue::Int(42)),
                "'x' should be Int(42)"
            );
            assert_eq!(
                fields.get("y"),
                Some(&RuntimeValue::Int(42)),
                "'y' should equal 'x' (Int(42)), not Null — stale snapshot bug"
            );
        }
        _ => unreachable!(),
    }
}

/// Two decorators applied to a single node — the second decorator reads a
/// field written by the first. The final event must contain both fields.
#[test]
fn test_two_decorators_second_reads_firsts_mutation() {
    let src = r#"
decorator first_dec() {
    event["marker"] = "first"
}

decorator second_dec() {
    event["saw_marker"] = event["marker"]
}

@first_dec
@second_dec
Bob: "chained decorators"
"#;
    let steps = run_script(src);

    assert!(
        !steps.iter().any(|s| matches!(s, VmStep::Error(_))),
        "chained decorators should not error; got: {steps:?}"
    );

    let dialogue_step = steps
        .iter()
        .find(|s| matches!(s, VmStep::Event(Event::Dialogue { .. })));

    assert!(
        dialogue_step.is_some(),
        "expected a Dialogue event; got: {steps:?}"
    );

    match dialogue_step.unwrap() {
        VmStep::Event(Event::Dialogue { fields, .. }) => {
            assert_eq!(
                fields.get("marker"),
                Some(&RuntimeValue::Str(
                    urd::lexer::strings::ParsedString::new_plain("first")
                )),
                "'marker' should be Str(\"first\")"
            );
            assert_eq!(
                fields.get("saw_marker"),
                Some(&RuntimeValue::Str(
                    urd::lexer::strings::ParsedString::new_plain("first")
                )),
                "'saw_marker' should equal 'marker' — second decorator must see first's writes"
            );
        }
        _ => unreachable!(),
    }
}

/// Calling a decorator with fewer arguments than it declares parameters must
/// produce an error — `zip` must not silently truncate.
#[test]
fn test_under_arity_decorator_call_fails() {
    let src = r#"
decorator need_two(a: int, b: int) {
    event["sum"] = a
}

@need_two(1)
Carol: "under-arity"
"#;
    let steps = run_script(src);

    let error_step = steps.iter().find(|s| matches!(s, VmStep::Error(_)));
    assert!(
        error_step.is_some(),
        "calling a 2-param decorator with 1 arg must error; got: {steps:?}"
    );

    match error_step.unwrap() {
        VmStep::Error(VmError::TypeError(msg)) => {
            assert!(
                msg.contains("expects 2 argument(s), got 1"),
                "error message should mention the arity mismatch; got: {msg}"
            );
        }
        other => panic!("expected TypeError about arity mismatch, got: {other:?}"),
    }
}

/// Calling a decorator with more arguments than it declares parameters must
/// produce an error — `zip` must not silently truncate.
#[test]
fn test_over_arity_decorator_call_fails() {
    let src = r#"
decorator need_one(a: int) {
    event["val"] = a
}

@need_one(1, 2)
Dave: "over-arity"
"#;
    let steps = run_script(src);

    let error_step = steps.iter().find(|s| matches!(s, VmStep::Error(_)));
    assert!(
        error_step.is_some(),
        "calling a 1-param decorator with 2 args must error; got: {steps:?}"
    );

    match error_step.unwrap() {
        VmStep::Error(VmError::TypeError(msg)) => {
            assert!(
                msg.contains("expects 1 argument(s), got 2"),
                "error message should mention the arity mismatch; got: {msg}"
            );
        }
        other => panic!("expected TypeError about arity mismatch, got: {other:?}"),
    }
}
