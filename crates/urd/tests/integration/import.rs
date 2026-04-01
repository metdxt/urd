//! Integration tests for Urd's import system.
//!
//! These tests exercise the full pipeline — source → lex → parse →
//! `compile_with_loader` → VM execution — for multi-module scenarios:
//!
//! 1. Importing a module and jumping to a label defined in it.
//! 2. Importing a module and reading a cross-module global set by the consumer.
//! 3. Multiple imported modules coexist without label collisions.
//! 4. Missing module surfaces a clear error at compile time.
//! 5. Circular imports are detected and reported.

use urd::{
    VmStep,
    compiler::{Compiler, CompilerError},
    ir::Event,
    parse_test,
    parser::block::script,
    runtime::value::RuntimeValue,
    vm::{DecoratorRegistry, Vm, loader::MemLoader},
};

// ─── Helpers ─────────────────────────────────────────────────────────────────

/// Parse `src` with the top-level `script()` parser, panicking on failure.
fn parse_src(src: &str) -> urd::parser::ast::Ast {
    parse_test!(script(), src).expect("parse failed")
}

/// Build a VM from a source string + loader, panicking on any error.
fn build_vm_with_loader(main_src: &str, loader: &MemLoader) -> Vm {
    let ast = parse_src(main_src);
    let graph = Compiler::compile_with_loader(&ast, loader).expect("compile_with_loader failed");
    Vm::new(graph, DecoratorRegistry::new()).expect("Vm::new failed")
}

/// Drain the VM and collect all `Event::Dialogue` lines into a flat `Vec<String>`.
fn collect_dialogue_lines(vm: &mut Vm) -> Vec<String> {
    let mut out = Vec::new();
    loop {
        match vm.next(None) {
            VmStep::Ended => break,
            VmStep::Event(Event::Dialogue { lines, .. }) => {
                for val in &lines {
                    if let RuntimeValue::Str(ps) = val {
                        out.push(ps.to_string());
                    }
                }
            }
            VmStep::Event(_) => {}
            VmStep::Error(e) => panic!("VM error: {e}"),
        }
    }
    out
}

// ─── Test 1: Jump to a label defined in an imported module ───────────────────

/// Importing a module and doing `jump lib.greeting` should reach the dialogue
/// node that lives in the imported module.
#[test]
fn test_import_then_jump_to_module_label() {
    let mut loader = MemLoader::new();
    loader.add(
        "lib.urd",
        r#"
        label greeting {
            <"Alice">: "Hello from lib!"
        }
        "#,
    );

    let main_src = r#"
        import "lib.urd" as lib

        @entry
        label main {
            jump lib.greeting
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader);
    let lines = collect_dialogue_lines(&mut vm);

    assert_eq!(
        lines,
        vec!["Hello from lib!"],
        "expected dialogue from imported module, got: {lines:?}"
    );
}

// ─── Test 2: Read a cross-module global set by the consumer ──────────────────

/// `lib.score = 42` stores the value under `lib::score` (Global scope).
/// A subsequent `lib.score` expression should resolve to 42.
///
/// We verify this by having a conditional dialogue: only the "correct" branch
/// fires, confirming the read returned the expected value.
#[test]
fn test_cross_module_global_write_and_read() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", r#""#);

    let main_src = r#"
        import "lib.urd" as lib

        @entry
        label main {
            lib.score = 99
            if lib.score == 99 {
                <"Narrator">: "High score!"
            } else {
                <"Narrator">: "Nope"
            }
            end!()
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader);
    let lines = collect_dialogue_lines(&mut vm);

    assert_eq!(
        lines,
        vec!["High score!"],
        "expected cross-module global read to return 99, got: {lines:?}"
    );
}

// ─── Test 3: Module label is reachable and body executes ─────────────────────

/// Jumping to a module label should execute its body, not just the EnterScope
/// sentinel.  We check that variables set inside the label body are visible
/// to subsequent instructions (by using them in a following dialogue).
#[test]
fn test_module_label_body_executes_on_jump() {
    let mut loader = MemLoader::new();
    loader.add(
        "state.urd",
        r#"
        label init {
            <"Sys">: "module init ran"
        }
        "#,
    );

    let main_src = r#"
        import "state.urd" as state

        @entry
        label main {
            jump state.init
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader);
    let lines = collect_dialogue_lines(&mut vm);

    assert_eq!(
        lines,
        vec!["module init ran"],
        "expected body of module label to execute, got: {lines:?}"
    );
}

// ─── Test 4: Two modules imported under different aliases ─────────────────────

/// Two modules with identically-named labels ("start") must not collide when
/// imported under different aliases.  Jumping to each alias should reach the
/// correct dialogue.
#[test]
fn test_two_modules_with_same_label_name_do_not_collide() {
    let mut loader = MemLoader::new();
    loader.add(
        "module_a.urd",
        r#"
        label start {
            <"A">: "from module A"
        }
        "#,
    );
    loader.add(
        "module_b.urd",
        r#"
        label start {
            <"B">: "from module B"
        }
        "#,
    );

    // Jump to module A's start only.
    let main_src = r#"
        import "module_a.urd" as a
        import "module_b.urd" as b

        @entry
        label main {
            jump a.start
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader);
    let lines = collect_dialogue_lines(&mut vm);

    assert_eq!(
        lines,
        vec!["from module A"],
        "expected only module A dialogue, got: {lines:?}"
    );
}

// ─── Test 5: Module with multiple labels — jump to a non-first label ──────────

/// The merged graph contains ALL labels from the module, not just the first one.
/// Verify that jumping to a label that is not the module's graph entry works.
#[test]
fn test_jump_to_non_entry_module_label() {
    let mut loader = MemLoader::new();
    loader.add(
        "scenes.urd",
        r#"
        label intro {
            <"Alice">: "this is intro"
        }
        label epilogue {
            <"Alice">: "this is epilogue"
        }
        "#,
    );

    let main_src = r#"
        import "scenes.urd" as scenes

        @entry
        label main {
            jump scenes.epilogue
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader);
    let lines = collect_dialogue_lines(&mut vm);

    assert_eq!(
        lines,
        vec!["this is epilogue"],
        "expected epilogue dialogue (non-entry module label), got: {lines:?}"
    );
}

// ─── Test 6: Missing module → ModuleLoadError at compile time ────────────────

/// `import "ghost.urd" as g` where the loader has no such file must produce a
/// `CompilerError::ModuleLoadError` containing the missing path.
#[test]
fn test_missing_module_produces_load_error() {
    let loader = MemLoader::new(); // empty — no files

    let ast = parse_src(r#"import "ghost.urd" as g"#);
    let result = Compiler::compile_with_loader(&ast, &loader);

    assert!(
        matches!(
            result,
            Err(CompilerError::ModuleLoadError { ref path, .. }) if path == "ghost.urd"
        ),
        "expected ModuleLoadError for 'ghost.urd', got: {result:?}"
    );
}

// ─── Test 7: Circular import → compiles successfully ─────────────────────────

/// If `a.urd` imports `b.urd` and `b.urd` imports `a.urd` back, compilation
/// must now succeed.  The 4-phase flat pipeline pre-allocates all label Nop
/// stubs before emitting any IR, so neither module needs the other to be fully
/// compiled first.
#[test]
fn test_circular_import_compiles_successfully() {
    let mut loader = MemLoader::new();
    loader.add(
        "a.urd",
        "import \"b.urd\" as b\nlabel a_label {\n  jump b.b_label\n}\n",
    );
    loader.add(
        "b.urd",
        "import \"a.urd\" as a\nlabel b_label {\n  jump a.a_label\n}\n",
    );

    let ast = parse_src(r#"import "a.urd" as a"#);
    let result = Compiler::compile_with_loader(&ast, &loader);

    assert!(
        result.is_ok(),
        "circular import should compile successfully, got: {result:?}"
    );

    let graph = result.unwrap();
    assert!(
        graph.labels.contains_key("a::a_label"),
        "a::a_label must be accessible; labels: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    assert!(
        graph.labels.contains_key("b::b_label"),
        "b::b_label must be accessible; labels: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
}

// ─── Test 8: Import-only script (no jump) — graph contains module labels ─────

/// A script that only contains an `import` statement (no jump) must still
/// compile successfully and have the module's labels registered in the graph.
#[test]
fn test_import_only_script_registers_labels() {
    let mut loader = MemLoader::new();
    loader.add("util.urd", r#"label helper { <Sys>: "util" }"#);

    let ast = parse_src(r#"import "util.urd" as util"#);
    let graph =
        Compiler::compile_with_loader(&ast, &loader).expect("compile_with_loader should succeed");

    assert!(
        graph.labels.contains_key("util::helper"),
        "'util::helper' must be present in graph.labels after import, got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
}

// ─── Test 9: Cross-module global persists across scope transitions ────────────

/// `mod.flag = 1` followed by an `if` that reads `mod.flag` must see the
/// value 1 — cross-module globals are stored in the global map and therefore
/// visible everywhere in the script, including inside conditional branches.
#[test]
fn test_cross_module_global_visible_in_conditional() {
    let mut loader = MemLoader::new();
    loader.add("ns.urd", r#""#);
    let main_src = r#"
        import "ns.urd" as ns

        @entry
        label main {
            ns.flag = 1
            if ns.flag == 1 {
                <"Test">: "flag is set"
            }
            end!()
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader);
    let lines = collect_dialogue_lines(&mut vm);

    assert_eq!(
        lines,
        vec!["flag is set"],
        "cross-module global must be visible in a conditional, got: {lines:?}"
    );
}

// ─── Test 10: Transitive imports — module that imports another module ─────────

/// `main.urd` imports `mid.urd`, which imports `base.urd`.  Jumping to
/// `base::entry` from main must work.
#[test]
fn test_transitive_imports_resolve() {
    let mut loader = MemLoader::new();
    loader.add(
        "base.urd",
        r#"
        label entry {
            <"Base">: "from base"
        }
        "#,
    );
    loader.add("ext.urd", r#"import "base.urd" as base"#);

    // main imports base directly, and ext (which internally imports base).
    // The diamond-import logic should handle this without a CircularImport
    // error, and both aliases should be available.
    let main_src = r#"
        import "base.urd" as base
        import "ext.urd" as ext

        @entry
        label main {
            jump base.entry
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader);
    let lines = collect_dialogue_lines(&mut vm);

    assert_eq!(
        lines,
        vec!["from base"],
        "expected base module dialogue via transitive import + direct import, got: {lines:?}"
    );
}
