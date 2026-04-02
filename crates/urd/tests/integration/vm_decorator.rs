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

use urd::{
    Event, RuntimeValue, VmError, VmStep,
    compiler::{Compiler, loader::parse_source},
    vm::{Vm, registry::DecoratorRegistry},
};

// ── Helper ────────────────────────────────────────────────────────────────────

/// Parse, compile, and drive the VM to completion (or the first error),
/// returning every [`VmStep`] observed.  Caps at 64 steps to prevent infinite
/// loops in broken tests.
fn run_script(src: &str) -> Vec<VmStep> {
    let ast = parse_source(src).expect("script should parse without errors");
    let graph = Compiler::compile(&ast).expect("script should compile without errors");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm should initialise");

    let mut steps = Vec::new();
    for _ in 0..64 {
        let step = vm.next(None);
        let terminal = matches!(step, VmStep::Ended | VmStep::Error(_));
        steps.push(step);
        if terminal {
            break;
        }
    }
    steps
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
