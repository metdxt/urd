use super::*;
use crate::{
    compiler::Compiler,
    lexer::strings::ParsedString,
    parser::ast::{Ast, AstContent, DeclKind, Decorator, MatchArm, MatchPattern, TokSpan},
    runtime::value::RuntimeValue,
};

// ── Shared helpers ────────────────────────────────────────────────────────

fn ident(name: &str) -> Ast {
    Ast::value(RuntimeValue::IdentPath(vec![name.to_string()]))
}

fn int(n: i64) -> Ast {
    Ast::value(RuntimeValue::Int(n))
}

fn str_lit(s: &str) -> Ast {
    Ast::value(RuntimeValue::Str(ParsedString::new_plain(s)))
}

fn decl(name: &str, val: Ast) -> Ast {
    Ast::decl(DeclKind::Variable, ident(name), val)
}

fn empty_registry() -> DecoratorRegistry {
    DecoratorRegistry::new()
}

fn build_vm(ast: Ast) -> Vm {
    let graph = Compiler::compile(&ast).expect("compile failed");
    Vm::new(graph, empty_registry()).expect("vm construction failed")
}

fn build_vm_named(ast: Ast, file_stem: &str) -> Vm {
    let graph =
        crate::compiler::Compiler::compile_named(&ast, file_stem).expect("compile_named failed");
    Vm::new(graph, empty_registry()).expect("vm construction failed")
}

// ── Tests ─────────────────────────────────────────────────────────────────

/// Dialogue compiled with `compile_named` carries the generated `loc_id`.
#[test]
fn test_dialogue_event_carries_loc_id() {
    let speakers = Ast::expr_list(vec![str_lit("narrator")]);
    let lines = Ast::expr_list(vec![str_lit("Hello!")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let labeled = Ast::labeled_block("start".to_string(), dialogue);
    let ast = Ast::block(vec![labeled]);

    let mut vm = build_vm_named(ast, "intro");

    match vm.next(None) {
        VmStep::Event(Event::Dialogue { loc_id, .. }) => {
            assert_eq!(
                loc_id.as_deref(),
                Some("intro-start-line_1"),
                "expected intro-start-line_1, got {:?}",
                loc_id
            );
        }
        other => panic!("expected Dialogue event, got {:?}", other),
    }
}

/// `compile` (no stem) produces `loc_id: None` on emitted Dialogue events.
#[test]
fn test_dialogue_event_no_loc_id_without_file_stem() {
    let speakers = Ast::expr_list(vec![str_lit("narrator")]);
    let lines = Ast::expr_list(vec![str_lit("Hello!")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let ast = Ast::block(vec![dialogue]);

    let mut vm = build_vm(ast);

    match vm.next(None) {
        VmStep::Event(Event::Dialogue { loc_id, .. }) => {
            assert!(
                loc_id.is_none(),
                "compile() should yield loc_id=None, got {:?}",
                loc_id
            );
        }
        other => panic!("expected Dialogue event, got {:?}", other),
    }
}

/// Choice event and each option carry their generated `loc_id` values.
#[test]
fn test_choice_event_carries_loc_ids() {
    let opt1 = Ast::menu_option("yes".to_string(), Ast::block(vec![]), false);
    let opt2 = Ast::menu_option("no".to_string(), Ast::block(vec![]), false);
    let menu = Ast::menu(vec![opt1, opt2]);
    let labeled = Ast::labeled_block("start".to_string(), menu);
    let ast = Ast::block(vec![labeled]);

    let mut vm = build_vm_named(ast, "test");

    match vm.next(None) {
        VmStep::Event(Event::Choice {
            loc_id, options, ..
        }) => {
            assert_eq!(
                loc_id.as_deref(),
                Some("test-start-menu_1"),
                "Choice event loc_id mismatch: got {:?}",
                loc_id
            );
            assert_eq!(options.len(), 2);
            assert_eq!(
                options[0].loc_id.as_deref(),
                Some("test-start-menu_1-yes"),
                "option[0] loc_id mismatch: got {:?}",
                options[0].loc_id
            );
            assert_eq!(
                options[1].loc_id.as_deref(),
                Some("test-start-menu_1-no"),
                "option[1] loc_id mismatch: got {:?}",
                options[1].loc_id
            );
        }
        other => panic!("expected Choice event, got {:?}", other),
    }
}

/// A script `let x = 1` followed by a Dialogue emits exactly one
/// `Event::Dialogue` and then ends.
#[test]
fn test_let_then_dialogue_emits_event() {
    let speakers = Ast::expr_list(vec![str_lit("Alice")]);
    let lines = Ast::expr_list(vec![str_lit("Hello!")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let ast = Ast::block(vec![decl("x", int(1)), dialogue]);

    let mut vm = build_vm(ast);

    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue {
            speakers, lines, ..
        } => {
            assert_eq!(speakers.len(), 1);
            assert_eq!(lines.len(), 1);
            match &lines[0] {
                RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "Hello!"),
                other => panic!("expected Str, got {:?}", other),
            }
        }
        other => panic!("expected Dialogue, got {:?}", other),
    }

    assert!(
        matches!(vm.next(None), VmStep::Ended),
        "script should end after dialogue"
    );
}

/// A `Branch` on `true` follows `then_node`; the dialogue reads the
/// variable set in the then-block.
#[test]
fn test_branch_true_follows_then() {
    let condition = Ast::value(RuntimeValue::Bool(true));
    // Declare x before the if so it survives block-scope pop.
    let pre_decl = decl("x", int(0));
    let then_block = Ast::block(vec![Ast::assign_op(ident("x"), int(1))]);
    let else_block = Ast::block(vec![Ast::assign_op(ident("x"), int(2))]);
    let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

    let speakers = Ast::expr_list(vec![str_lit("Bob")]);
    let lines = Ast::expr_list(vec![ident("x")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let ast = Ast::block(vec![pre_decl, if_ast, dialogue]);

    let mut vm = build_vm(ast);
    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };

    match ev {
        Event::Dialogue { lines, .. } => {
            assert_eq!(
                lines,
                vec![RuntimeValue::Int(1)],
                "then-branch should set x=1"
            );
        }
        other => panic!("expected Dialogue, got {:?}", other),
    }
}

/// A `Branch` on `false` follows `else_node`.
#[test]
fn test_branch_false_follows_else() {
    let condition = Ast::value(RuntimeValue::Bool(false));
    // Declare result before the if so it survives block-scope pop.
    let pre_decl = decl("result", int(0));
    let then_block = Ast::block(vec![Ast::assign_op(ident("result"), int(1))]);
    let else_block = Ast::block(vec![Ast::assign_op(ident("result"), int(2))]);
    let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

    let speakers = Ast::expr_list(vec![str_lit("Narrator")]);
    let lines = Ast::expr_list(vec![ident("result")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let ast = Ast::block(vec![pre_decl, if_ast, dialogue]);

    let mut vm = build_vm(ast);
    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue { lines, .. } => {
            assert_eq!(
                lines,
                vec![RuntimeValue::Int(2)],
                "else-branch should set result=2"
            );
        }
        other => panic!("expected Dialogue, got {:?}", other),
    }
}

/// `next(None)` on a Menu emits `Event::Choice`; calling `next(None)` again
/// re-emits it (with a log warning); `next(Some(0))` clears the pending
/// choice and advances into the option body.
#[test]
fn test_choice_flow() {
    let opt_a = Ast::menu_option(
        "Option A".to_string(),
        Ast::block(vec![decl("picked", int(1))]),
        false,
    );
    let opt_b = Ast::menu_option(
        "Option B".to_string(),
        Ast::block(vec![decl("picked", int(2))]),
        false,
    );
    let ast = Ast::menu(vec![opt_a, opt_b]);

    let mut vm = build_vm(ast);

    // First call — emits Choice and sets pending_choice.
    let ev1 = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("first next(None) should emit Event, got {:?}", other),
    };
    assert!(
        matches!(ev1, Event::Choice { .. }),
        "first next(None) should emit Choice, got {:?}",
        ev1
    );
    // cursor and pending_choice are both Option<NodeIndex>.
    let choice_cursor = vm.state.cursor;
    assert_eq!(
        vm.state.pending_choice, choice_cursor,
        "pending_choice must equal cursor after first next(None)"
    );

    // Second call with None — re-emits Choice.
    let ev2 = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("second next(None) should emit Event, got {:?}", other),
    };
    assert!(
        matches!(ev2, Event::Choice { ref options, .. } if options.len() == 2),
        "second next(None) should re-emit same Choice"
    );

    // Provide a valid choice (option 0).
    // Option 0's body has `let picked = 1` — internal, so the loop ends.
    let ev3 = vm.next(Some(0));
    assert!(
        vm.state.pending_choice.is_none(),
        "pending_choice must be cleared after valid choice"
    );
    // Option body has no dialogue → script ends → VmStep::Ended.
    assert!(
        matches!(ev3, VmStep::Ended),
        "expected script end after choosing option 0, got {:?}",
        ev3
    );
}

/// An out-of-bounds choice index re-emits the Choice event with a warning
/// and does NOT advance the cursor.
#[test]
fn test_choice_out_of_bounds_reemits() {
    let opt = Ast::menu_option("Only".to_string(), Ast::block(vec![]), false);
    let ast = Ast::menu(vec![opt]);
    let mut vm = build_vm(ast);

    // Consume the initial None-choice emission.
    match vm.next(None) {
        VmStep::Event(_) => {}
        other => panic!("expected Event, got {:?}", other),
    };

    // Provide an out-of-bounds index.
    let ev = match vm.next(Some(99)) {
        VmStep::Event(e) => e,
        other => panic!("out-of-bounds should re-emit Event, got {:?}", other),
    };
    assert!(
        matches!(ev, Event::Choice { .. }),
        "out-of-bounds choice should re-emit Choice, got {:?}",
        ev
    );
}

/// A `Jump` to a known label advances the cursor and the dialogue inside
/// the label is reached.
#[test]
fn test_jump_advances_to_label() {
    let speakers = Ast::expr_list(vec![str_lit("Alice")]);
    let lines = Ast::expr_list(vec![str_lit("Jumped here!")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let labeled = Ast::labeled_block("scene1".to_string(), Ast::block(vec![dialogue]));
    let jump = Ast::jump_stmt("scene1".to_string(), false);
    let ast = Ast::block(vec![jump, labeled]);

    let mut vm = build_vm(ast);

    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue { lines, .. } => match &lines[0] {
            RuntimeValue::Str(ps) => {
                assert_eq!(ps.to_string(), "Jumped here!");
            }
            other => panic!("expected Str line, got {:?}", other),
        },
        other => panic!("expected Dialogue, got {:?}", other),
    }
}

/// `Vm::new` returns `VmError::UnknownDecorator` when the compiled script
/// uses a decorator name that is not registered.
#[test]
fn test_unknown_decorator_fails_validation() {
    let speakers = Ast::expr_list(vec![str_lit("Alice")]);
    let lines = Ast::expr_list(vec![str_lit("Hi")]);
    let deco = Decorator::bare("mystery_deco".to_string());
    let dialogue = Ast::new_decorated(
        AstContent::Dialogue {
            speakers: Box::new(speakers),
            content: Box::new(lines),
        },
        vec![deco],
    );

    let graph = Compiler::compile(&dialogue).expect("compile failed");
    let result = Vm::new(graph, empty_registry());

    assert!(
        matches!(
            result,
            Err(VmError::UnknownDecorator { ref name, .. }) if name == "mystery_deco"
        ),
        "expected UnknownDecorator, got {:?}",
        result
    );
}

/// A `Vm::new` call succeeds when all decorators used in the script are
/// registered.
#[test]
fn test_known_decorator_passes_validation() {
    let speakers = Ast::expr_list(vec![str_lit("Alice")]);
    let lines = Ast::expr_list(vec![str_lit("Hi")]);
    let deco = Decorator::bare("mood".to_string());
    let dialogue = Ast::new_decorated(
        AstContent::Dialogue {
            speakers: Box::new(speakers),
            content: Box::new(lines),
        },
        vec![deco],
    );

    let graph = Compiler::compile(&dialogue).expect("compile failed");
    let mut registry = DecoratorRegistry::new();
    registry.register("mood", |_args| HashMap::new());

    assert!(
        Vm::new(graph, registry).is_ok(),
        "registered decorator should pass validation"
    );
}

/// `Return` inside a labeled block unwinds back to the continuation after
/// the block (the dialogue that follows it in the script).
#[test]
fn test_return_exits_to_continuation() {
    let labeled = Ast::labeled_block(
        "myblock".to_string(),
        Ast::block(vec![Ast::return_stmt(None)]),
    );
    let speakers = Ast::expr_list(vec![str_lit("Alice")]);
    let lines = Ast::expr_list(vec![str_lit("After block")]);
    let after_dialogue = Ast::dialogue(speakers, lines);
    let ast = Ast::block(vec![labeled, after_dialogue]);

    let mut vm = build_vm(ast);

    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue { lines, .. } => match &lines[0] {
            RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "After block"),
            other => panic!("expected Str, got {:?}", other),
        },
        other => panic!("expected Dialogue after return, got {:?}", other),
    }
    assert!(
        matches!(vm.next(None), VmStep::Ended),
        "script should end after dialogue"
    );
}

/// A `Return` with no call frame on the stack ends the script.
#[test]
fn test_return_with_empty_call_stack_ends_script() {
    let ast = Ast::return_stmt(None);
    let mut vm = build_vm(ast);
    assert!(
        matches!(vm.next(None), VmStep::Ended),
        "Return with empty call stack should end script"
    );
}

// ── Expression evaluator unit tests ───────────────────────────────────────

/// Basic arithmetic: Int+Int, Float+Int coercion, negation.
#[test]
fn test_eval_arithmetic() {
    let env = Environment::new();

    let add = Ast::add_op(int(3), int(4));
    assert_eq!(eval_expr(&add, &env).expect("3+4"), RuntimeValue::Int(7));

    let mixed = Ast::add_op(Ast::value(RuntimeValue::Float(2.5)), int(1));
    assert_eq!(
        eval_expr(&mixed, &env).expect("2.5+1"),
        RuntimeValue::Float(3.5)
    );

    let neg = Ast::negate_op(int(5));
    assert_eq!(eval_expr(&neg, &env).expect("-5"), RuntimeValue::Int(-5));

    let not_true = Ast::not_op(Ast::value(RuntimeValue::Bool(true)));
    assert_eq!(
        eval_expr(&not_true, &env).expect("not true"),
        RuntimeValue::Bool(false)
    );
}

/// Comparison operators produce Bool results.
#[test]
fn test_eval_comparison() {
    let env = Environment::new();

    let gt = Ast::greater_than_op(int(5), int(3));
    assert_eq!(eval_expr(&gt, &env).expect("5>3"), RuntimeValue::Bool(true));

    let eq = Ast::equals_op(int(4), int(4));
    assert_eq!(
        eval_expr(&eq, &env).expect("4==4"),
        RuntimeValue::Bool(true)
    );

    let neq = Ast::not_equals_op(int(4), int(5));
    assert_eq!(
        eval_expr(&neq, &env).expect("4!=5"),
        RuntimeValue::Bool(true)
    );
}

/// Shift with a negative count must return a VM error (never panic).
#[test]
fn test_eval_left_shift_negative_count_errors() {
    let env = Environment::new();
    let expr = Ast::left_shift_op(int(8), int(-1));

    match eval_expr(&expr, &env) {
        Err(VmError::TypeError(msg)) => {
            assert!(
                msg.contains("invalid shift count -1"),
                "unexpected error message: {msg}"
            );
        }
        other => panic!(
            "expected TypeError for negative shift count, got {:?}",
            other
        ),
    }
}

/// Shift with a count larger than 63 must return a VM error (never panic).
#[test]
fn test_eval_right_shift_too_large_count_errors() {
    let env = Environment::new();
    let expr = Ast::right_shift_op(int(8), int(64));

    match eval_expr(&expr, &env) {
        Err(VmError::TypeError(msg)) => {
            assert!(
                msg.contains("invalid shift count 64"),
                "unexpected error message: {msg}"
            );
        }
        other => panic!(
            "expected TypeError for too-large shift count, got {:?}",
            other
        ),
    }
}

/// Boundary shift counts (0 and 63) remain valid.
#[test]
fn test_eval_shift_boundary_counts_are_valid() {
    let env = Environment::new();

    let left_zero = Ast::left_shift_op(int(1), int(0));
    match eval_expr(&left_zero, &env) {
        Ok(RuntimeValue::Int(1)) => {}
        Ok(other) => panic!("1 << 0 should be Int(1), got {:?}", other),
        Err(err) => panic!("1 << 0 should be valid, got error: {err}"),
    }

    let right_max = Ast::right_shift_op(int(i64::MAX), int(63));
    match eval_expr(&right_max, &env) {
        Ok(RuntimeValue::Int(0)) => {}
        Ok(other) => panic!("i64::MAX >> 63 should be Int(0), got {:?}", other),
        Err(err) => panic!("i64::MAX >> 63 should be valid, got error: {err}"),
    }
}

/// Short-circuit `And` and `Or`.
#[test]
fn test_eval_logical_short_circuit() {
    let env = Environment::new();

    let and_short = Ast::and_op(
        Ast::value(RuntimeValue::Bool(false)),
        ident("undefined_var"),
    );
    assert_eq!(
        eval_expr(&and_short, &env).expect("false and x"),
        RuntimeValue::Bool(false)
    );

    let or_short = Ast::or_op(
        Ast::value(RuntimeValue::Bool(true)),
        ident("also_undefined"),
    );
    assert_eq!(
        eval_expr(&or_short, &env).expect("true or x"),
        RuntimeValue::Bool(true)
    );
}

/// Variable lookup succeeds when defined; returns `UndefinedVariable`
/// otherwise.
#[test]
fn test_variable_lookup() {
    let mut env = Environment::new();
    env.set("hp", RuntimeValue::Int(100), &DeclKind::Variable)
        .expect("set failed");

    assert_eq!(
        eval_expr(&ident("hp"), &env).expect("hp"),
        RuntimeValue::Int(100)
    );
    assert!(
        matches!(
            eval_expr(&ident("missing"), &env),
            Err(VmError::UndefinedVariable(_))
        ),
        "undefined variable should error"
    );
}

/// Decorator registry: registered decorator fields are applied; unknown
/// decorator returns an error.
#[test]
fn test_decorator_registry_apply() {
    let mut registry = DecoratorRegistry::new();
    registry.register("mood", |args| {
        let mut m = HashMap::new();
        if let Some(RuntimeValue::Str(s)) = args.first() {
            m.insert("mood".to_string(), RuntimeValue::Str(s.clone()));
        }
        m
    });

    let env = Environment::new();
    let deco = Decorator::new("mood".to_string(), Ast::expr_list(vec![str_lit("happy")]));

    let fields = registry.apply(&deco, &env).expect("apply should succeed");
    assert!(fields.contains_key("mood"), "fields should contain 'mood'");

    let unknown_deco = Decorator::bare("ghost".to_string());
    assert!(
        registry.apply(&unknown_deco, &env).is_err(),
        "unknown decorator should error"
    );
}

/// Environment: `const` cannot be reassigned.
#[test]
fn test_const_immutability() {
    let mut env = Environment::new();
    env.set("MAX", RuntimeValue::Int(100), &DeclKind::Constant)
        .expect("first const set");
    let result = env.set("MAX", RuntimeValue::Int(200), &DeclKind::Constant);
    assert!(
        matches!(result, Err(VmError::TypeError(_))),
        "reassigning a const should be a TypeError"
    );
}

/// VM execution: plain assignment (`x = 2`) to a constant must error.
#[test]
fn test_plain_assignment_to_const_errors_at_runtime() {
    let const_decl = Ast::decl(DeclKind::Constant, ident("x"), int(1));
    let plain_assign = Ast::assign_op(ident("x"), int(2));
    let ast = Ast::block(vec![const_decl, plain_assign]);

    let mut vm = build_vm(ast);

    match vm.next(None) {
        VmStep::Error(VmError::TypeError(msg)) => {
            assert!(
                msg.contains("cannot assign to constant 'x'"),
                "expected const-assignment runtime error, got: {msg}"
            );
        }
        other => panic!(
            "expected VmStep::Error(TypeError) for const reassignment, got {:?}",
            other
        ),
    }
}

/// Environment: globals are accessible from nested scopes.
#[test]
fn test_globals_visible_in_nested_scope() {
    let mut env = Environment::new();
    env.set("score", RuntimeValue::Int(0), &DeclKind::Global)
        .expect("set global");
    env.push_scope();
    assert_eq!(
        env.get("score").expect("global visible in inner scope"),
        RuntimeValue::Int(0)
    );
    let _ = env.pop_scope();
}

/// A complete script with `DefineEnum` + `Switch` reaches the right arm.
#[test]
fn test_switch_on_enum_variant() {
    let enum_decl = Ast::enum_decl(
        "Direction".to_string(),
        vec![
            ("North".to_string(), TokSpan::default()),
            ("South".to_string(), TokSpan::default()),
        ],
    );

    let north_path = Ast::value(RuntimeValue::IdentPath(vec![
        "Direction".to_string(),
        "North".to_string(),
    ]));
    let dir_decl = Ast::decl(DeclKind::Variable, ident("dir"), north_path);

    let north_arm = MatchArm::new(
        MatchPattern::Value(Ast::value(RuntimeValue::Str(ParsedString::new_plain(
            "North",
        )))),
        Ast::block(vec![Ast::dialogue(
            Ast::expr_list(vec![str_lit("Alice")]),
            Ast::expr_list(vec![str_lit("going north")]),
        )]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![Ast::dialogue(
            Ast::expr_list(vec![str_lit("Alice")]),
            Ast::expr_list(vec![str_lit("other")]),
        )]),
    );
    let match_stmt = Ast::match_stmt(ident("dir"), vec![north_arm, wild_arm]);

    let ast = Ast::block(vec![enum_decl, dir_decl, match_stmt]);
    let mut vm = build_vm(ast);

    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue { lines, .. } => match &lines[0] {
            RuntimeValue::Str(ps) => {
                assert_eq!(ps.to_string(), "going north");
            }
            other => panic!("expected 'going north', got {:?}", other),
        },
        other => panic!("expected Dialogue, got {:?}", other),
    }
}

/// `eval_expr_list` returns all elements of an ExprList.
#[test]
fn test_eval_expr_list_all_elements() {
    let env = Environment::new();
    let list = Ast::expr_list(vec![int(1), int(2), int(3)]);
    let result = eval_expr_list(&list, &env).expect("eval");
    assert_eq!(
        result,
        vec![
            RuntimeValue::Int(1),
            RuntimeValue::Int(2),
            RuntimeValue::Int(3)
        ]
    );
}

/// A Dialogue with multiple speakers correctly emits all of them.
#[test]
fn test_dialogue_multiple_speakers() {
    let speakers = Ast::expr_list(vec![str_lit("Alice"), str_lit("Bob")]);
    let lines = Ast::expr_list(vec![str_lit("Together!")]);
    let ast = Ast::dialogue(speakers, lines);

    let mut vm = build_vm(ast);
    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue { speakers, .. } => {
            assert_eq!(speakers.len(), 2, "expected 2 speakers");
        }
        other => panic!("expected Dialogue, got {:?}", other),
    }
}

// ── Script-decorator integration tests ────────────────────────────────────

/// A script-defined decorator that writes `event["camera_shake"] = amount`
/// should produce a Dialogue event with that field set.
#[test]
fn test_script_decorator_mutates_event_fields() {
    use crate::parser::ast::{DecoratorParam, EventConstraint};

    let event_ident = Ast::value(RuntimeValue::IdentPath(vec!["event".to_string()]));
    let key_ast = Ast::value(RuntimeValue::Str(
        crate::lexer::strings::ParsedString::new_plain("camera_shake"),
    ));
    let amount_ident = Ast::value(RuntimeValue::IdentPath(vec!["amount".to_string()]));
    let subscript_assign = Ast::subscript_assign(event_ident, key_ast, amount_ident);
    let body = Ast::block(vec![subscript_assign]);

    let decorator_def = Ast::decorator_def(
        "shake".to_string(),
        EventConstraint::Any,
        vec![DecoratorParam {
            name: "amount".to_string(),
            type_annotation: None,
        }],
        body,
    );

    let speakers = Ast::expr_list(vec![str_lit("Alice")]);
    let lines = Ast::expr_list(vec![str_lit("Watch out!")]);
    let deco = Decorator::new(
        "shake".to_string(),
        Ast::expr_list(vec![Ast::value(RuntimeValue::Float(0.5))]),
    );
    let dialogue = Ast::new_decorated(
        AstContent::Dialogue {
            speakers: Box::new(speakers),
            content: Box::new(lines),
        },
        vec![deco],
    );

    let ast = Ast::block(vec![decorator_def, dialogue]);
    let mut vm = build_vm(ast);

    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue { fields, .. } => {
            assert!(
                fields.contains_key("camera_shake"),
                "expected 'camera_shake' in fields, got {:?}",
                fields
            );
            assert_eq!(
                fields.get("camera_shake"),
                Some(&RuntimeValue::Float(0.5)),
                "camera_shake should be Float(0.5)"
            );
        }
        other => panic!("expected Dialogue, got {:?}", other),
    }
}

/// A VM built with an empty Rust registry but a script-defined decorator
/// must NOT return `VmError::UnknownDecorator`.
#[test]
fn test_script_decorator_does_not_require_rust_registration() {
    use crate::parser::ast::EventConstraint;

    let decorator_def = Ast::decorator_def(
        "noop".to_string(),
        EventConstraint::Any,
        vec![],
        Ast::block(vec![]),
    );

    let speakers = Ast::expr_list(vec![str_lit("Alice")]);
    let lines = Ast::expr_list(vec![str_lit("Hi")]);
    let deco = Decorator::new("noop".to_string(), Ast::expr_list(vec![]));
    let dialogue = Ast::new_decorated(
        AstContent::Dialogue {
            speakers: Box::new(speakers),
            content: Box::new(lines),
        },
        vec![deco],
    );

    let ast = Ast::block(vec![decorator_def, dialogue]);
    let graph = Compiler::compile(&ast).expect("compile ok");

    let result = Vm::new(graph, empty_registry());
    assert!(
        result.is_ok(),
        "script-defined decorator should not require Rust registration, got {:?}",
        result
    );
}

/// Evaluating a Map literal `:{\"key\": 42}` returns a `RuntimeValue::Map`.
#[test]
fn test_map_literal_eval() {
    let env = Environment::new();
    let key_ast = Ast::value(RuntimeValue::Str(
        crate::lexer::strings::ParsedString::new_plain("key"),
    ));
    let val_ast = int(42);
    let map_ast = Ast::map(vec![(key_ast, val_ast)]);

    let result = eval_expr(&map_ast, &env).expect("map eval");
    match result {
        RuntimeValue::Map(m) => {
            assert_eq!(m.len(), 1);
            assert_eq!(*m["key"], RuntimeValue::Int(42));
        }
        other => panic!("expected Map, got {:?}", other),
    }
}

/// `Map[key]` subscript read returns the correct value.
#[test]
fn test_subscript_read() {
    let mut env = Environment::new();

    let key_ast = Ast::value(RuntimeValue::Str(
        crate::lexer::strings::ParsedString::new_plain("a"),
    ));
    let val_ast = int(99);
    let map_ast = Ast::map(vec![(key_ast, val_ast)]);
    env.set("m", eval_expr(&map_ast, &env).unwrap(), &DeclKind::Variable)
        .unwrap();

    let subscript = Ast::subscript(
        Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()])),
        Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        )),
    );

    let result = eval_expr(&subscript, &env).expect("subscript read");
    assert_eq!(result, RuntimeValue::Int(99));
}

/// Subscript assignment `m["a"] = 99` mutates the map stored in the env.
#[test]
fn test_subscript_assign_mutates_map() {
    use super::exec::eval_subscript_assign;

    let mut env = Environment::new();

    let key_ast = Ast::value(RuntimeValue::Str(
        crate::lexer::strings::ParsedString::new_plain("a"),
    ));
    let val_ast = int(1);
    let map_ast = Ast::map(vec![(key_ast, val_ast)]);
    env.set("m", eval_expr(&map_ast, &env).unwrap(), &DeclKind::Variable)
        .unwrap();

    let obj = Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()]));
    let key = Ast::value(RuntimeValue::Str(
        crate::lexer::strings::ParsedString::new_plain("a"),
    ));
    let val = int(99);
    eval_subscript_assign(&obj, &key, &val, &mut env).expect("subscript assign");

    let subscript = Ast::subscript(
        Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()])),
        Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        )),
    );
    let result = eval_expr(&subscript, &env).expect("read back");
    assert_eq!(
        result,
        RuntimeValue::Int(99),
        "m[\"a\"] should be 99 after assign"
    );
}

#[test]
fn test_list_literal_eval_via_vm() {
    // Verify that list literals evaluate to RuntimeValue::List.
    let env = Environment::default();
    let ast = Ast::list(vec![
        Ast::value(RuntimeValue::Int(10)),
        Ast::value(RuntimeValue::Int(20)),
        Ast::value(RuntimeValue::Int(30)),
    ]);
    let result = eval_expr(&ast, &env).expect("list eval");
    assert_eq!(
        result,
        RuntimeValue::List(vec![
            RuntimeValue::Int(10),
            RuntimeValue::Int(20),
            RuntimeValue::Int(30),
        ])
    );
}

#[test]
fn test_subscript_assign_mutates_list() {
    use super::exec::eval_subscript_assign;

    let mut env = Environment::new();
    // Declare: let xs: list = [1, 2, 3]
    env.set(
        "xs",
        RuntimeValue::List(vec![
            RuntimeValue::Int(1),
            RuntimeValue::Int(2),
            RuntimeValue::Int(3),
        ]),
        &DeclKind::Variable,
    )
    .unwrap();

    // xs[1] = 99
    let obj = Ast::value(RuntimeValue::IdentPath(vec!["xs".into()]));
    let key = Ast::value(RuntimeValue::Int(1));
    let val = Ast::value(RuntimeValue::Int(99));
    eval_subscript_assign(&obj, &key, &val, &mut env).expect("subscript assign");

    // Read back xs[1]
    let subscript = Ast::subscript(
        Ast::value(RuntimeValue::IdentPath(vec!["xs".into()])),
        Ast::value(RuntimeValue::Int(1)),
    );
    let result = eval_expr(&subscript, &env).expect("subscript read");
    assert_eq!(result, RuntimeValue::Int(99));
}

#[test]
fn test_subscript_assign_list_negative_index() {
    use super::exec::eval_subscript_assign;

    let mut env = Environment::new();
    env.set(
        "xs",
        RuntimeValue::List(vec![
            RuntimeValue::Int(1),
            RuntimeValue::Int(2),
            RuntimeValue::Int(3),
        ]),
        &DeclKind::Variable,
    )
    .unwrap();

    // xs[-1] = 42  — should update the last element
    let obj = Ast::value(RuntimeValue::IdentPath(vec!["xs".into()]));
    let key = Ast::value(RuntimeValue::Int(-1));
    let val = Ast::value(RuntimeValue::Int(42));
    eval_subscript_assign(&obj, &key, &val, &mut env).expect("negative index assign");

    let subscript = Ast::subscript(
        Ast::value(RuntimeValue::IdentPath(vec!["xs".into()])),
        Ast::value(RuntimeValue::Int(-1)),
    );
    let result = eval_expr(&subscript, &env).expect("negative subscript read");
    assert_eq!(result, RuntimeValue::Int(42));
}

#[test]
fn test_subscript_assign_list_out_of_bounds() {
    use super::exec::eval_subscript_assign;

    let mut env = Environment::new();
    env.set(
        "xs",
        RuntimeValue::List(vec![RuntimeValue::Int(1)]),
        &DeclKind::Variable,
    )
    .unwrap();

    let obj = Ast::value(RuntimeValue::IdentPath(vec!["xs".into()]));
    let key = Ast::value(RuntimeValue::Int(5));
    let val = Ast::value(RuntimeValue::Int(0));
    let err =
        eval_subscript_assign(&obj, &key, &val, &mut env).expect_err("should be out of bounds");
    assert!(matches!(
        err,
        VmError::IndexOutOfBounds { index: 5, len: 1 }
    ));
}

/// `jump label and return` (compiled to LetCall) pushes a call frame,
/// executes the label body, and after `return` resumes at the continuation.
#[test]
fn test_subroutine_call_and_return() {
    let speakers1 = Ast::expr_list(vec![str_lit("Alice")]);
    let lines1 = Ast::expr_list(vec![str_lit("Hello from greet")]);
    let greet_dialogue = Ast::dialogue(speakers1, lines1);
    let greet_body = Ast::block(vec![greet_dialogue, Ast::return_stmt(None)]);
    let greet_label = Ast::labeled_block("greet".to_string(), greet_body);

    let call_jump = Ast::jump_stmt("greet".to_string(), true);

    let speakers2 = Ast::expr_list(vec![str_lit("Alice")]);
    let lines2 = Ast::expr_list(vec![str_lit("After call")]);
    let after_dialogue = Ast::dialogue(speakers2, lines2);

    let main_return = Ast::return_stmt(None);

    let ast = Ast::block(vec![call_jump, after_dialogue, main_return, greet_label]);
    let mut vm = build_vm(ast);

    let ev1 = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected first Event, got {:?}", other),
    };
    match ev1 {
        Event::Dialogue { lines, .. } => match &lines[0] {
            RuntimeValue::Str(ps) => assert_eq!(
                ps.to_string(),
                "Hello from greet",
                "first event should be the greet dialogue"
            ),
            other => panic!("expected Str, got {:?}", other),
        },
        other => panic!("expected Dialogue for greet, got {:?}", other),
    }

    let ev2 = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected second Event, got {:?}", other),
    };
    match ev2 {
        Event::Dialogue { lines, .. } => match &lines[0] {
            RuntimeValue::Str(ps) => assert_eq!(
                ps.to_string(),
                "After call",
                "second event should be the post-call dialogue"
            ),
            other => panic!("expected Str, got {:?}", other),
        },
        other => panic!("expected Dialogue after return, got {:?}", other),
    }

    assert!(
        matches!(vm.next(None), VmStep::Ended),
        "script should end after second dialogue"
    );
}

/// `let result = jump double and return` binds the subroutine's return
/// value to `result` and execution continues after the call site.
#[test]
fn test_let_call_captures_return_value() {
    let double_body = Ast::block(vec![Ast::return_stmt(Some(int(42)))]);
    let double_label = Ast::labeled_block("double".to_string(), double_body);

    let let_call = Ast::let_call("result".to_string(), "double".to_string());

    let speakers = Ast::expr_list(vec![str_lit("Bot")]);
    let lines = Ast::expr_list(vec![ident("result")]);
    let dialogue = Ast::dialogue(speakers, lines);

    let ast = Ast::block(vec![let_call, dialogue, double_label]);
    let mut vm = build_vm(ast);

    let ev = match vm.next(None) {
        VmStep::Event(e) => e,
        other => panic!("expected Event, got {:?}", other),
    };
    match ev {
        Event::Dialogue { lines, .. } => {
            assert_eq!(
                lines,
                vec![RuntimeValue::Int(42)],
                "result should be the return value 42"
            );
        }
        other => panic!("expected Dialogue, got {:?}", other),
    }

    assert!(
        matches!(vm.next(None), VmStep::Ended),
        "script should end after dialogue"
    );
}

#[test]
fn test_todo_bang_ends_script() {
    use crate::ir::{IrEdge, IrGraph, IrNodeKind};
    use petgraph::stable_graph::StableGraph;
    use std::collections::HashSet;

    let mut g: StableGraph<IrNodeKind, IrEdge> = StableGraph::new();
    let todo_id = g.add_node(IrNodeKind::Todo);
    let graph = IrGraph {
        graph: g,
        entry: Some(todo_id),
        labels: HashMap::new(),
        entry_labels: HashSet::new(),
        cluster_names: HashMap::new(),
        label_sources: HashMap::new(),
    };
    let registry = DecoratorRegistry::new();
    let mut vm = Vm::new(graph, registry).unwrap();
    let result = vm.next(None);
    assert!(
        matches!(result, VmStep::Ended),
        "todo!() should end the script, got: {result:?}"
    );
}

#[test]
fn test_end_bang_ends_script() {
    use crate::ir::{IrEdge, IrGraph, IrNodeKind};
    use petgraph::stable_graph::StableGraph;
    use std::collections::HashSet;

    let mut g: StableGraph<IrNodeKind, IrEdge> = StableGraph::new();
    let end_id = g.add_node(IrNodeKind::End);
    let graph = IrGraph {
        graph: g,
        entry: Some(end_id),
        labels: HashMap::new(),
        entry_labels: HashSet::new(),
        cluster_names: HashMap::new(),
        label_sources: HashMap::new(),
    };
    let registry = DecoratorRegistry::new();
    let mut vm = Vm::new(graph, registry).unwrap();
    let result = vm.next(None);
    assert!(
        matches!(result, VmStep::Ended),
        "end!() should end the script, got: {result:?}"
    );
}

#[test]
fn test_jump_without_return_does_not_push_frame() {
    let dest_label = Ast::labeled_block("dest".to_string(), Ast::block(vec![]));
    let jump = Ast::jump_stmt("dest".to_string(), false);
    let ast = Ast::block(vec![dest_label, jump]);

    let graph = Compiler::compile(&ast).expect("compile failed");

    // Plain jump must emit IrNodeKind::Jump, never LetCall.
    let has_let_call = graph
        .graph
        .node_weights()
        .any(|k| matches!(k, IrNodeKind::LetCall { .. }));
    assert!(
        !has_let_call,
        "plain `jump label` must not emit a LetCall node; graph = {graph:?}"
    );

    let has_jump = graph
        .graph
        .node_weights()
        .any(|k| matches!(k, IrNodeKind::Jump));
    assert!(
        has_jump,
        "plain `jump label` must emit a Jump node; graph = {graph:?}"
    );
}

#[test]
fn test_extern_provided_runs_ok() {
    use crate::compiler::Compiler;
    use crate::compiler::loader::parse_source;
    use crate::runtime::value::RuntimeValue;
    use crate::vm::Vm;
    use crate::vm::registry::DecoratorRegistry;

    let src = r#"
extern narrator: str
label start {
    narrator: "Hello"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");
    vm.provide_extern(
        "narrator",
        RuntimeValue::Str(ParsedString::new_plain("Narrator")),
    );
    // Should run without error
    let step = vm.next(None);
    assert!(
        !matches!(step, VmStep::Error(_)),
        "unexpected error: {:?}",
        step
    );
}

#[test]
fn test_extern_not_provided_returns_error() {
    use crate::compiler::Compiler;
    use crate::compiler::loader::parse_source;
    use crate::vm::Vm;
    use crate::vm::VmError;
    use crate::vm::registry::DecoratorRegistry;

    let src = r#"
extern narrator: str
label start {
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");
    // Do NOT call provide_extern — should error

    // Step until we hit an error or end
    let mut found_error = false;
    for _ in 0..10 {
        match vm.next(None) {
            VmStep::Error(VmError::ExternNotProvided(name)) => {
                assert_eq!(name, "narrator");
                found_error = true;
                break;
            }
            VmStep::Error(e) => panic!("unexpected error: {:?}", e),
            VmStep::Ended => break,
            VmStep::Event(_) => {}
        }
    }
    assert!(found_error, "expected ExternNotProvided error");
}

// ── extract_label_interp_vars ────────────────────────────────────────────

#[test]
fn extract_label_interp_vars_empty_string() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("");
    assert!(vars.is_empty());
}

#[test]
fn extract_label_interp_vars_no_placeholders() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("Buy the potion");
    assert!(vars.is_empty());
}

#[test]
fn extract_label_interp_vars_single_placeholder() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("Buy it for {price} gold");
    assert_eq!(vars, vec![("price".to_string(), None)]);
}

#[test]
fn extract_label_interp_vars_multiple_placeholders() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("Pay {price} from your {gold} gold");
    assert!(vars.contains(&("price".to_string(), None)));
    assert!(vars.contains(&("gold".to_string(), None)));
}

#[test]
fn extract_label_interp_vars_dotted_path() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("Hello {player.name}!");
    assert_eq!(vars, vec![("player.name".to_string(), None)]);
}

#[test]
fn extract_label_interp_vars_ignores_empty_braces() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("broken {} placeholder");
    assert!(vars.is_empty(), "empty braces must not produce a var");
}

#[test]
fn extract_label_interp_vars_with_float_format() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("Cost: {price:.2} gold");
    assert_eq!(vars, vec![("price".to_string(), Some(".2".to_string()))]);
}

#[test]
fn extract_label_interp_vars_with_zero_pad_format() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("Turn {turns:03} of 100");
    assert_eq!(vars, vec![("turns".to_string(), Some("03".to_string()))]);
}

#[test]
fn extract_label_interp_vars_mixed_format_and_plain() {
    use super::fluent::extract_label_interp_vars;
    let vars = extract_label_interp_vars("{count:02} items for {price} gold");
    assert!(vars.contains(&("count".to_string(), Some("02".to_string()))));
    assert!(vars.contains(&("price".to_string(), None)));
}

// ── stale @fluent binding fixes ─────────────────────────────────────────

/// Verify that after `@fluent global gold = 50` followed by `gold = 20`,
/// the Fluent context passed to the localizer contains the CURRENT value
/// (20), not the stale initial value (50) cached from the declaration.
#[test]
fn fluent_var_uses_current_value_after_global_mutation() {
    use crate::compiler::Compiler;
    use crate::compiler::loader::parse_source;
    use crate::vm::Vm;
    use crate::vm::registry::DecoratorRegistry;
    use std::sync::Arc;

    let src = r#"
const n = :{ name: "N", name_color: "white" }
@fluent
global gold = 50
@entry
label start {
    gold = 20
    n: "You have {gold} gold."
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile_named(&ast, "test").expect("compile");
    let vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");

    // Mock localizer: captures the fluent vars map passed for the known
    // message ID and returns them serialised as "key=value" pairs so the
    // test can assert on the exact value without needing fluent_bundle.
    struct CapturingLocalizer;
    impl crate::Localizer for CapturingLocalizer {
        fn localize(
            &self,
            id: &str,
            vars: &std::collections::HashMap<String, RuntimeValue>,
        ) -> Option<String> {
            if id == "test-start-line_1" {
                let gold = vars.get("gold")?;
                Some(format!("gold={gold:?}"))
            } else {
                None
            }
        }
    }

    let mut vm = vm.with_localizer(Arc::new(CapturingLocalizer));

    // Advance until the Dialogue event.
    let mut localized: Option<String> = None;
    for _ in 0..20 {
        match vm.next(None) {
            VmStep::Event(crate::ir::Event::Dialogue { localized_text, .. }) => {
                localized = localized_text;
                break;
            }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {:?}", e),
            _ => {}
        }
    }

    // The localizer must receive gold=20 (current), not gold=50 (stale
    // @fluent initial binding).
    assert_eq!(
        localized.as_deref(),
        Some("gold=Int(20)"),
        "FTL context must carry current gold=20, not stale gold=50"
    );
}

/// Verify that when a string interpolation carries a format specifier
/// (e.g. `{price:.2}`), `collect_fluent_vars` passes the **pre-formatted**
/// string (`"30.00"`) to the Fluent localizer rather than the raw
/// `Float(30.0)`.
#[test]
fn fluent_var_pre_formatted_with_specifier() {
    use crate::compiler::Compiler;
    use crate::compiler::loader::parse_source;
    use crate::vm::Vm;
    use crate::vm::registry::DecoratorRegistry;
    use std::sync::Arc;

    let src = r#"
const n = :{ name: "N", name_color: "white" }
@fluent
global price = 30.0
@entry
label start {
    n: "Total: {price:.2} gold."
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile_named(&ast, "test").expect("compile");
    let vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");

    struct CapturingLocalizer;
    impl crate::Localizer for CapturingLocalizer {
        fn localize(
            &self,
            _id: &str,
            vars: &std::collections::HashMap<String, RuntimeValue>,
        ) -> Option<String> {
            // Return a tagged string so the test can distinguish a
            // pre-formatted Str from a raw Float.
            match vars.get("price")? {
                RuntimeValue::Str(ps) => Some(format!("str:{}", ps)),
                other => Some(format!("other:{other:?}")),
            }
        }
    }

    let mut vm = vm.with_localizer(Arc::new(CapturingLocalizer));

    let mut localized: Option<String> = None;
    for _ in 0..20 {
        match vm.next(None) {
            VmStep::Event(crate::ir::Event::Dialogue { localized_text, .. }) => {
                localized = localized_text;
                break;
            }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {:?}", e),
            _ => {}
        }
    }

    // The Fluent context must contain the pre-formatted "30.00" string, not
    // the raw Float(30.0) that would be formatted independently by Fluent.
    assert_eq!(
        localized.as_deref(),
        Some("str:30.00"),
        "FTL context must carry pre-formatted price=\"30.00\", not raw Float(30.0)"
    );
}

/// Verify that a dotted-path interpolation (`{inv.gold}`) produces a Fluent
/// key using `-` as the separator (`inv-gold`) and resolves the value via
/// the module-namespace lookup (`inv::gold` in the environment).
#[test]
fn fluent_vars_dotted_path_uses_hyphen_key() {
    use crate::compiler::Compiler;
    use crate::compiler::loader::parse_source;
    use crate::vm::Vm;
    use crate::vm::registry::DecoratorRegistry;
    use std::sync::Arc;

    let src = r#"
const n = :{ name: "N", name_color: "white" }
@entry
label start {
    n: "You have {inv.gold} coins."
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile_named(&ast, "test").expect("compile");
    let mut vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");
    // Inject the namespaced variable so that resolve_interp_path("inv.gold")
    // finds "inv::gold" in the externs slot.
    vm.provide_extern("inv::gold", RuntimeValue::Int(77));

    struct CapturingLocalizer;
    impl crate::Localizer for CapturingLocalizer {
        fn localize(
            &self,
            id: &str,
            vars: &std::collections::HashMap<String, RuntimeValue>,
        ) -> Option<String> {
            if id == "test-start-line_1" {
                // Key must be "inv-gold" (hyphen), not "inv_gold" (underscore)
                // and not "gold" (last segment only).
                let val = vars.get("inv-gold")?;
                Some(format!("inv-gold={val:?}"))
            } else {
                None
            }
        }
    }

    let mut vm = vm.with_localizer(Arc::new(CapturingLocalizer));

    let mut localized: Option<String> = None;
    for _ in 0..20 {
        match vm.next(None) {
            VmStep::Event(crate::ir::Event::Dialogue { localized_text, .. }) => {
                localized = localized_text;
                break;
            }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {:?}", e),
            _ => {}
        }
    }

    assert_eq!(
        localized.as_deref(),
        Some("inv-gold=Int(77)"),
        "Fluent vars must use hyphen separator and resolve namespaced env key"
    );
}

/// Unconditional jump loops between two labeled blocks must NOT accumulate
/// [`CallFrame`]s on the call stack.
///
/// **Known bug**: [`IrNodeKind::EnterScope`] always pushes a [`CallFrame`]
/// regardless of whether the label was entered via [`IrNodeKind::LetCall`]
/// (subroutine) or a plain [`IrNodeKind::Jump`] (tail transfer).  Because
/// an unconditional `jump` never reaches [`IrNodeKind::ExitScope`] of the
/// *previous* label, the pushed frame is never popped.
///
/// After 20 dialogue events from the A → B → A loop the call stack has ≈ 20
/// leaked frames; this test asserts 0 and therefore **FAILS** against the
/// current implementation.
#[test]
fn test_unconditional_jump_loop_leaks_call_frames() {
    use crate::compiler::loader::parse_source;

    // A simple two-label ping-pong loop.  Each label emits one Dialogue event
    // before jumping to the other label so that vm.next() returns control to
    // the test after each observable step.
    let src = r#"
@entry
label alpha {
    Narrator: "in alpha"
    jump beta
}

label beta {
    Narrator: "in beta"
    jump alpha
}
"#;

    let ast = parse_source(src).expect("parse must succeed");
    let graph = Compiler::compile(&ast).expect("compile must succeed");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm construction must succeed");

    // Drive 20 observable events (10 full alpha→beta cycles).
    for i in 0..20 {
        match vm.next(None) {
            VmStep::Error(e) => panic!("unexpected VM error at step {i}: {e}"),
            VmStep::Ended => break,
            VmStep::Event(_) => {}
        }
    }

    // A correct implementation uses tail-call semantics for unconditional
    // jumps: no CallFrame should be pushed when entering a label via `jump`.
    // The current implementation leaks one frame per EnterScope hit.
    assert_eq!(
        vm.state.call_stack.len(),
        0,
        "BUG: jump loop leaked {} CallFrame(s) after 20 steps. \
         Plain `jump` must not accumulate frames on the call stack. \
         Fix: only push a CallFrame for LetCall (subroutine), not for Jump.",
        vm.state.call_stack.len()
    );
    assert!(
        vm.state.env.depth() <= 2,
        "BUG: jump loop grew scope stack to depth {} after 20 steps. \
         Plain `jump` must not accumulate scopes (base 1 + current label = 2 max).",
        vm.state.env.depth()
    );
}

// ── Dice-match helpers ────────────────────────────────────────────────────

/// A deterministic dice roller that returns a fixed sequence of values,
/// truncated to the requested `count`.  Used to make dice-evaluation
/// tests reproducible.
struct StubRoller(Vec<i64>);

impl DiceRoller for StubRoller {
    fn roll_individual(&self, count: u32, _sides: u32) -> Vec<i64> {
        self.0[..count as usize].to_vec()
    }
}

/// Build a VM with a deterministic [`StubRoller`] injected into the
/// environment before the first step.
fn build_vm_with_roller(ast: Ast, roller: impl DiceRoller + 'static) -> Vm {
    let graph = Compiler::compile(&ast).expect("compile failed");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm init failed");
    vm.state.env.set_dice_roller(Box::new(roller));
    vm
}

/// Construct a minimal two-part `Dialogue` AST node for use in match arm bodies.
fn dialogue_node(speaker: &str, line: &str) -> Ast {
    Ast::dialogue(
        Ast::expr_list(vec![str_lit(speaker)]),
        Ast::expr_list(vec![str_lit(line)]),
    )
}

/// Advance the VM one step and assert it emits a `Dialogue` event whose
/// first line matches `expected`.
fn expect_dialogue_line(vm: &mut Vm, expected: &str) {
    match vm.next(None) {
        VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
            RuntimeValue::Str(ps) => {
                assert_eq!(ps.to_string(), expected, "dialogue line text mismatch")
            }
            other => panic!("expected Str line, got {:?}", other),
        },
        other => panic!("expected Dialogue event, got {:?}", other),
    }
}

// ── VM dice-match integration tests ──────────────────────────────────────

/// A `Value` arm is compared against the **sum** of the roll.
///
/// `1d6` rolls `[5]` → sum=5 → arm `5` matches → `"hit 5"` dialogue fires.
#[test]
fn test_match_value_coerces_roll_to_sum() {
    let scrutinee = Ast::value(RuntimeValue::Dice(1, 6));
    let hit_arm = MatchArm::new(
        MatchPattern::Value(Ast::value(RuntimeValue::Int(5))),
        Ast::block(vec![dialogue_node("narrator", "hit 5")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "no match")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![hit_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![5]));

    expect_dialogue_line(&mut vm, "hit 5");
}

/// An exclusive `Range` arm matches when the sum falls strictly inside the bounds.
///
/// `1d20` rolls `[15]`, arm `2..18` → 2 ≤ 15 < 18 → matches → `"mid"` fires.
#[test]
fn test_match_range_exclusive_matches() {
    let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
    let range_arm = MatchArm::new(
        MatchPattern::Range {
            start: Ast::value(RuntimeValue::Int(2)),
            end: Ast::value(RuntimeValue::Int(18)),
            inclusive: false,
            binding: None,
        },
        Ast::block(vec![dialogue_node("narrator", "mid")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "out")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![range_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![15]));

    expect_dialogue_line(&mut vm, "mid");
}

/// The exclusive upper bound is itself excluded from the range.
///
/// `1d20` rolls `[18]`, arm `2..18` → 18 is NOT < 18 → no match →
/// wildcard fires → `"out of range"`.
#[test]
fn test_match_range_exclusive_boundary_excluded() {
    let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
    let range_arm = MatchArm::new(
        MatchPattern::Range {
            start: Ast::value(RuntimeValue::Int(2)),
            end: Ast::value(RuntimeValue::Int(18)),
            inclusive: false,
            binding: None,
        },
        Ast::block(vec![dialogue_node("narrator", "in range")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "out of range")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![range_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![18]));

    expect_dialogue_line(&mut vm, "out of range");
}

/// The inclusive upper bound is itself included in the range.
///
/// `1d20` rolls `[18]`, arm `2..=18` → 18 ≤ 18 → matches → `"in range"` fires.
#[test]
fn test_match_range_inclusive_boundary_included() {
    let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
    let range_arm = MatchArm::new(
        MatchPattern::Range {
            start: Ast::value(RuntimeValue::Int(2)),
            end: Ast::value(RuntimeValue::Int(18)),
            inclusive: true,
            binding: None,
        },
        Ast::block(vec![dialogue_node("narrator", "in range")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "out of range")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![range_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![18]));

    expect_dialogue_line(&mut vm, "in range");
}

/// An `as name` binding injects the matched scalar into the arm body scope.
///
/// `1d20` rolls `[7]`, arm `1..=20 as roll_val` → `roll_val` is bound to
/// `Int(7)` inside the arm body.  The dialogue emits `roll_val` as a line,
/// which must evaluate to `RuntimeValue::Int(7)`.
#[test]
fn test_match_range_binding_injects_variable() {
    let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
    let arm = MatchArm::new(
        MatchPattern::Range {
            start: Ast::value(RuntimeValue::Int(1)),
            end: Ast::value(RuntimeValue::Int(20)),
            inclusive: true,
            binding: Some("roll_val".to_string()),
        },
        // Emit the bound variable as the dialogue line to verify its value.
        Ast::block(vec![Ast::dialogue(
            Ast::expr_list(vec![str_lit("narrator")]),
            Ast::expr_list(vec![ident("roll_val")]),
        )]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![7]));

    match vm.next(None) {
        VmStep::Event(Event::Dialogue { lines, .. }) => {
            assert_eq!(
                lines[0],
                RuntimeValue::Int(7),
                "binding `roll_val` must resolve to Int(7) inside the arm body"
            );
        }
        other => panic!("expected Dialogue event, got {:?}", other),
    }
}

/// An `Array` pattern matches element-by-element against individual die results.
///
/// `1d6` rolls `[6]`, arm `[6]` → exact element match → `"max"` fires.
#[test]
fn test_match_array_single_die_matches() {
    let scrutinee = Ast::value(RuntimeValue::Dice(1, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(6)))]),
        Ast::block(vec![dialogue_node("narrator", "max")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "other")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![6]));

    expect_dialogue_line(&mut vm, "max");
}

/// An `Array` pattern fails when the die value differs from the pattern element.
///
/// `1d6` rolls `[5]`, arm `[6]` → 5 ≠ 6 → no match → wildcard → `"not max"`.
#[test]
fn test_match_array_single_die_no_match() {
    let scrutinee = Ast::value(RuntimeValue::Dice(1, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(6)))]),
        Ast::block(vec![dialogue_node("narrator", "max")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "not max")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![5]));

    expect_dialogue_line(&mut vm, "not max");
}

/// A multi-element `Array` pattern matches each die result in order.
///
/// `2d6` rolls `[1, 6]`, arm `[1, 6]` → both elements match → `"snake+six"` fires.
#[test]
fn test_match_array_multi_die_matches() {
    let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![
            Some(Ast::value(RuntimeValue::Int(1))),
            Some(Ast::value(RuntimeValue::Int(6))),
        ]),
        Ast::block(vec![dialogue_node("narrator", "snake+six")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "other")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![1, 6]));

    expect_dialogue_line(&mut vm, "snake+six");
}

/// `Array` pattern comparison is strictly order-sensitive.
///
/// `2d6` rolls `[6, 1]`, arm `[1, 6]` → die[0]=6 ≠ pat[0]=1 → no match →
/// wildcard fires → `"wrong order"`.
#[test]
fn test_match_array_multi_die_wrong_order() {
    let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![
            Some(Ast::value(RuntimeValue::Int(1))),
            Some(Ast::value(RuntimeValue::Int(6))),
        ]),
        Ast::block(vec![dialogue_node("narrator", "one then six")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "wrong order")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![6, 1]));

    expect_dialogue_line(&mut vm, "wrong order");
}

/// A wildcard element in an `Array` pattern matches any die value at that position.
///
/// `2d6` rolls `[1, 6]`, arm `[1, _]` → first matches, second is wildcard → fires.
#[test]
fn test_match_array_wildcard_element_matches() {
    let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(1))), None]),
        Ast::block(vec![dialogue_node("narrator", "one then any")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "other")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![1, 6]));

    expect_dialogue_line(&mut vm, "one then any");
}

/// Wildcard element still matches when the concrete die value differs.
///
/// `2d6` rolls `[1, 3]`, arm `[1, _]` → first matches, second is wildcard → fires.
#[test]
fn test_match_array_wildcard_element_matches_any_value() {
    let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(1))), None]),
        Ast::block(vec![dialogue_node("narrator", "one then any")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "other")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![1, 3]));

    expect_dialogue_line(&mut vm, "one then any");
}

/// A concrete element still rejects a mismatch even when wildcards are present.
///
/// `2d6` rolls `[2, 6]`, arm `[1, _]` → die[0]=2 ≠ pat[0]=1 → no match → wildcard.
#[test]
fn test_match_array_wildcard_element_rejects_mismatch() {
    let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(1))), None]),
        Ast::block(vec![dialogue_node("narrator", "one then any")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "not one")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![2, 6]));

    expect_dialogue_line(&mut vm, "not one");
}

/// An all-wildcard `Array` pattern `[_, _]` matches any 2-die roll.
///
/// `2d6` rolls `[4, 2]`, arm `[_, _]` → both wildcards → fires.
#[test]
fn test_match_array_all_wildcards_matches() {
    let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
    let array_arm = MatchArm::new(
        MatchPattern::Array(vec![None, None]),
        Ast::block(vec![dialogue_node("narrator", "any pair")]),
    );
    let wild_arm = MatchArm::new(
        MatchPattern::Wildcard,
        Ast::block(vec![dialogue_node("narrator", "fallback")]),
    );
    let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
    let ast = Ast::block(vec![match_stmt]);
    let mut vm = build_vm_with_roller(ast, StubRoller(vec![4, 2]));

    expect_dialogue_line(&mut vm, "any pair");
}

// ── Wildcard / default menu option tests ─────────────────────────────────

/// A menu with 2 real options and 1 default emits an `Event::Choice` with
/// only 2 visible options, and `has_default == true`.
#[test]
fn test_menu_default_option_not_in_event() {
    let opt_a = Ast::menu_option(
        "Option A".to_string(),
        Ast::block(vec![dialogue_node("narrator", "picked A")]),
        false,
    );
    let opt_b = Ast::menu_option(
        "Option B".to_string(),
        Ast::block(vec![dialogue_node("narrator", "picked B")]),
        false,
    );
    let opt_default =
        Ast::menu_default_option(Ast::block(vec![dialogue_node("narrator", "default fired")]));
    let ast = Ast::menu(vec![opt_a, opt_b, opt_default]);
    let mut vm = build_vm(ast);

    match vm.next(None) {
        VmStep::Event(Event::Choice {
            options,
            has_default,
            ..
        }) => {
            assert_eq!(options.len(), 2, "default option must be excluded");
            assert!(has_default, "has_default must be true");
            assert_eq!(options[0].label, "Option A");
            assert_eq!(options[1].label, "Option B");
        }
        other => panic!("expected Choice event, got {:?}", other),
    }
}

/// When a menu has a default option and is already pending, calling
/// `vm.next(None)` follows the default branch instead of re-emitting.
#[test]
fn test_menu_default_triggered_by_none() {
    let opt_a = Ast::menu_option(
        "Option A".to_string(),
        Ast::block(vec![dialogue_node("narrator", "picked A")]),
        false,
    );
    let opt_default =
        Ast::menu_default_option(Ast::block(vec![dialogue_node("narrator", "default fired")]));
    let ast = Ast::menu(vec![opt_a, opt_default]);
    let mut vm = build_vm(ast);

    // First next(None) → emits Choice and sets pending.
    match vm.next(None) {
        VmStep::Event(Event::Choice { has_default, .. }) => {
            assert!(has_default);
        }
        other => panic!("expected Choice event, got {:?}", other),
    }

    // Second next(None) → follows default option, emits its dialogue.
    match vm.next(None) {
        VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
            RuntimeValue::Str(ps) => {
                assert_eq!(
                    ps.to_string(),
                    "default fired",
                    "expected default branch dialogue"
                );
            }
            other => panic!("expected Str line, got {:?}", other),
        },
        other => panic!("expected Dialogue from default branch, got {:?}", other),
    }
}

/// A menu WITHOUT a default option re-emits the Choice event when
/// `vm.next(None)` is called while already pending (existing behavior).
#[test]
fn test_menu_without_default_reemits_on_none() {
    let opt_a = Ast::menu_option(
        "Option A".to_string(),
        Ast::block(vec![dialogue_node("narrator", "picked A")]),
        false,
    );
    let opt_b = Ast::menu_option(
        "Option B".to_string(),
        Ast::block(vec![dialogue_node("narrator", "picked B")]),
        false,
    );
    let ast = Ast::menu(vec![opt_a, opt_b]);
    let mut vm = build_vm(ast);

    // First next(None) → emits Choice.
    match vm.next(None) {
        VmStep::Event(Event::Choice { has_default, .. }) => {
            assert!(!has_default, "no default option present");
        }
        other => panic!("expected Choice event, got {:?}", other),
    }

    // Second next(None) → re-emits Choice (no default to follow).
    match vm.next(None) {
        VmStep::Event(Event::Choice { has_default, .. }) => {
            assert!(!has_default, "still no default");
        }
        other => panic!("expected re-emitted Choice event, got {:?}", other),
    }
}

/// Host-provided choice indices map correctly when a default option sits
/// between real options: menu ["A", _ , "B"] → host sees indices 0 ("A")
/// and 1 ("B"); the default is skipped in the index mapping.
#[test]
fn test_menu_default_choice_index_mapping() {
    let opt_a = Ast::menu_option(
        "Option A".to_string(),
        Ast::block(vec![dialogue_node("narrator", "picked A")]),
        false,
    );
    let opt_default =
        Ast::menu_default_option(Ast::block(vec![dialogue_node("narrator", "default fired")]));
    let opt_b = Ast::menu_option(
        "Option B".to_string(),
        Ast::block(vec![dialogue_node("narrator", "picked B")]),
        false,
    );
    // Order: A (idx 0), default (idx 1), B (idx 2)
    let ast = Ast::menu(vec![opt_a, opt_default, opt_b]);

    // ── Test host index 0 → "A" ─────────────────────────────────────
    {
        let mut vm = build_vm(ast.clone());
        match vm.next(None) {
            VmStep::Event(Event::Choice { options, .. }) => {
                assert_eq!(options.len(), 2);
                assert_eq!(options[0].label, "Option A");
                assert_eq!(options[1].label, "Option B");
            }
            other => panic!("expected Choice, got {:?}", other),
        }
        // Choose index 0 → should follow "Option A"
        match vm.next(Some(0)) {
            VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(ps.to_string(), "picked A", "index 0 should map to Option A");
                }
                other => panic!("expected Str line, got {:?}", other),
            },
            other => panic!("expected Dialogue from Option A, got {:?}", other),
        }
    }

    // ── Test host index 1 → "B" (not default!) ──────────────────────
    {
        let mut vm = build_vm(ast.clone());
        match vm.next(None) {
            VmStep::Event(Event::Choice { .. }) => {}
            other => panic!("expected Choice, got {:?}", other),
        }
        // Choose index 1 → should follow "Option B", NOT default
        match vm.next(Some(1)) {
            VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(ps.to_string(), "picked B", "index 1 should map to Option B");
                }
                other => panic!("expected Str line, got {:?}", other),
            },
            other => panic!("expected Dialogue from Option B, got {:?}", other),
        }
    }
}

// ── Block-scope / shadowing tests ─────────────────────────────────────────

/// `let` inside an `if` block creates a new binding that shadows the outer
/// one without mutating it. After the block ends the outer value is restored.
#[test]
fn test_let_shadows_outer_variable() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label start {
    let x = 10
    if true {
        let x = 42
        narrator: "inner x is {x}"
    }
    narrator: "outer x is {x}"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm");

    expect_dialogue_line(&mut vm, "inner x is 42");
    expect_dialogue_line(&mut vm, "outer x is 10");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

/// Bare assignment (`x = 42`) without `let` searches outward and mutates
/// the binding found in the enclosing scope.
#[test]
fn test_bare_assignment_mutates_outer_variable() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label start {
    let x = 10
    if true {
        x = 42
    }
    narrator: "x is {x}"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm");

    expect_dialogue_line(&mut vm, "x is 42");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

/// Variables declared inside an `if` block don't leak into the outer scope.
/// The script continues normally after the block ends.
#[test]
fn test_block_scope_variables_dont_leak() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label start {
    if true {
        let temp = 99
    }
    narrator: "done"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm");

    expect_dialogue_line(&mut vm, "done");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

/// `let` can shadow a `const` — the constant check is bypassed for
/// `DeclKind::Variable`, so the local binding wins.
#[test]
fn test_let_can_shadow_constant() {
    use crate::compiler::loader::parse_source;

    let src = r#"
const MAX = 100
@entry
label start {
    let MAX = 999
    narrator: "MAX is {MAX}"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm");

    expect_dialogue_line(&mut vm, "MAX is 999");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

/// Variables declared inside a `match` arm body don't leak to the
/// continuation after the match statement.
#[test]
fn test_match_arm_block_scoping() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label start {
    let val = 1
    match val {
        1 {
            let result = "one"
            narrator: "matched {result}"
        }
        _ {
            let result = "other"
            narrator: "matched {result}"
        }
    }
    narrator: "done"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm");

    expect_dialogue_line(&mut vm, "matched one");
    expect_dialogue_line(&mut vm, "done");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

/// A `const`-declared speaker variable must resolve to its runtime value,
/// not fall back to the identifier name string.
///
/// Regression for: `const zara = :{ name: "Zara" }` followed by
/// `zara: "Hello"` — the speaker should be the Map value, never the
/// string `"zara"`.
#[test]
fn test_const_speaker_resolves_to_value_not_identifier() {
    use crate::compiler::Compiler;
    use crate::compiler::loader::parse_source;

    let src = r#"
const zara = :{ name: "Zara" }

label start {
    zara: "Hello"
    end!
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile_named(&ast, "test").expect("compile");
    let registry = empty_registry();
    let mut vm = Vm::new(graph, registry).expect("vm");

    let step = vm.next(None);
    match step {
        VmStep::Event(Event::Dialogue { speakers, .. }) => {
            assert_eq!(speakers.len(), 1, "expected exactly one speaker");
            // Must NOT be the plain string "zara" (identifier fallback).
            assert!(
                !matches!(&speakers[0], RuntimeValue::Str(ps) if ps.to_string() == "zara"),
                "speaker must not be the raw identifier string 'zara'; got {:?}",
                speakers[0]
            );
            // Must be the Map value defined by `const zara = :{{ name: "Zara" }}`.
            assert!(
                matches!(&speakers[0], RuntimeValue::Map(_)),
                "expected speaker to be RuntimeValue::Map (the const value), got {:?}",
                speakers[0]
            );
        }
        other => panic!("expected Dialogue event, got {:?}", other),
    }
}

/// `jump` from inside nested `if` blocks correctly unwinds all block
/// scopes (PushScope levels) in addition to the label scope (EnterScope).
#[test]
fn test_jump_cleans_up_block_scopes() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label start {
    if true {
        let temp = 1
        if true {
            let temp2 = 2
            jump finish
        }
    }
    end!()
}

label finish {
    narrator: "arrived"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, empty_registry()).expect("vm");

    expect_dialogue_line(&mut vm, "arrived");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

// ── entry_labels / Vm::new_at tests ──────────────────────────────────────

/// Compiling a script with two `@entry` labels populates `entry_labels`
/// with both bare names.
#[test]
fn test_entry_labels_populated_single_file() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label a {
    narrator: "label a"
    end!()
}

@entry
label b {
    narrator: "label b"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");

    let entries = graph.entry_labels();
    assert!(
        entries.contains("a"),
        "entry_labels should contain 'a': {entries:?}"
    );
    assert!(
        entries.contains("b"),
        "entry_labels should contain 'b': {entries:?}"
    );
    assert_eq!(
        entries.len(),
        2,
        "should have exactly 2 entry labels: {entries:?}"
    );
}

/// `Vm::new_at` starts execution at the specified `@entry` label.
#[test]
fn test_new_at_valid_entry_label() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label a {
    narrator: "label a"
    end!()
}

@entry
label b {
    narrator: "label b"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new_at(graph, empty_registry(), "b").expect("new_at should succeed");

    expect_dialogue_line(&mut vm, "label b");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

/// `Vm::new_at` rejects a label that exists but is not `@entry`-decorated.
#[test]
fn test_new_at_non_entry_label_errors() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label a {
    narrator: "label a"
    end!()
}

label b {
    narrator: "label b"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let result = Vm::new_at(graph, empty_registry(), "b");

    assert!(
        matches!(result, Err(VmError::UnknownLabel(ref msg)) if msg.contains("b")),
        "expected UnknownLabel for non-@entry label 'b', got: {result:?}"
    );
}

/// `Vm::new_at` rejects a label name that does not exist at all.
#[test]
fn test_new_at_nonexistent_label_errors() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label a {
    narrator: "label a"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let result = Vm::new_at(graph, empty_registry(), "nonexistent");

    assert!(
        matches!(result, Err(VmError::UnknownLabel(ref msg)) if msg.contains("nonexistent")),
        "expected UnknownLabel for 'nonexistent', got: {result:?}"
    );
}

/// `Vm::new` still works exactly as before — starts at the first `@entry`
/// label (backward compatibility).
#[test]
fn test_new_still_works_as_before() {
    use crate::compiler::loader::parse_source;

    let src = r#"
@entry
label start {
    narrator: "hello from start"
    end!()
}
"#;
    let ast = parse_source(src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    let mut vm = Vm::new(graph, empty_registry()).expect("new should succeed");

    expect_dialogue_line(&mut vm, "hello from start");
    assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
}

// ── Bug-fix regression tests ──────────────────────────────────────────────

/// `list.contains()` must use `values_equal` so that cross-type numeric
/// comparisons work (e.g. `[1.0, 2.0].contains(1)` → `true`).
#[test]
fn test_list_contains_cross_type_numeric_equality() {
    let list = vec![
        RuntimeValue::Float(1.0),
        RuntimeValue::Float(2.0),
        RuntimeValue::Float(3.0),
    ];

    // Int 1 should match Float 1.0
    let env = Environment::new();
    let result =
        list_methods::dispatch(list.clone(), "contains", &[RuntimeValue::Int(1)], &env).unwrap();
    assert_eq!(result, RuntimeValue::Bool(true), "[1.0,2.0,3.0].contains(1)");

    // Int 4 should not match anything
    let result =
        list_methods::dispatch(list.clone(), "contains", &[RuntimeValue::Int(4)], &env).unwrap();
    assert_eq!(result, RuntimeValue::Bool(false), "[1.0,2.0,3.0].contains(4)");

    // Symmetric: list of ints, searching with a float
    let int_list = vec![
        RuntimeValue::Int(10),
        RuntimeValue::Int(20),
    ];
    let result =
        list_methods::dispatch(int_list, "contains", &[RuntimeValue::Float(20.0)], &env).unwrap();
    assert_eq!(result, RuntimeValue::Bool(true), "[10,20].contains(20.0)");
}

/// `is_truthy` must return `false` for empty strings and `true` for non-empty.
#[test]
fn test_is_truthy_empty_string_is_falsy() {
    use super::eval::is_truthy;

    let empty = RuntimeValue::Str(ParsedString::new_plain(""));
    assert!(!is_truthy(&empty), "empty string should be falsy");

    let non_empty = RuntimeValue::Str(ParsedString::new_plain("hello"));
    assert!(is_truthy(&non_empty), "non-empty string should be truthy");
}

/// `is_truthy` must return `false` for empty maps and `true` for non-empty.
#[test]
fn test_is_truthy_empty_map_is_falsy() {
    use super::eval::is_truthy;

    let empty_map = RuntimeValue::Map(std::collections::HashMap::new());
    assert!(!is_truthy(&empty_map), "empty map should be falsy");

    let mut m = std::collections::HashMap::new();
    m.insert("k".to_string(), Box::new(RuntimeValue::Int(1)));
    let non_empty_map = RuntimeValue::Map(m);
    assert!(is_truthy(&non_empty_map), "non-empty map should be truthy");
}

/// `is_truthy` must return `false` for structs with no fields and `true` otherwise.
#[test]
fn test_is_truthy_empty_struct_is_falsy() {
    use super::eval::is_truthy;

    let empty_struct = RuntimeValue::Struct {
        name: "Empty".to_string(),
        fields: std::collections::HashMap::new(),
    };
    assert!(!is_truthy(&empty_struct), "empty struct should be falsy");

    let mut fields = std::collections::HashMap::new();
    fields.insert("x".to_string(), RuntimeValue::Int(42));
    let non_empty_struct = RuntimeValue::Struct {
        name: "Point".to_string(),
        fields,
    };
    assert!(is_truthy(&non_empty_struct), "non-empty struct should be truthy");
}
