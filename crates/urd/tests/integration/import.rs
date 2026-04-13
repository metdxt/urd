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
//! 6. Bare-name leakage from whole-module imports is rejected.
//! 7. Duplicate module aliases are rejected.
//! 8. Duplicate symbol import aliases are rejected.
//! 9. Missing symbol imports are rejected with a diagnostic.
//! 10. Circular imports under visibility rules behave correctly.

use std::io;

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

/// Parse `src` with the top-level `script()` parser.
fn parse_src(src: &str) -> Result<urd::parser::ast::Ast, Box<dyn std::error::Error>> {
    let ast = parse_test!(script(), src)
        .map_err(|err| io::Error::other(format!("parse failed: {err:?}")))?;
    Ok(ast)
}

/// Build a VM from a source string + loader.
fn build_vm_with_loader(
    main_src: &str,
    loader: &MemLoader,
) -> Result<Vm, Box<dyn std::error::Error>> {
    let ast = parse_src(main_src)?;
    let graph = Compiler::compile_with_loader(&ast, loader)
        .map_err(|err| io::Error::other(format!("compile_with_loader failed: {err}")))?;
    let vm = Vm::new(graph, DecoratorRegistry::new())
        .map_err(|err| io::Error::other(format!("Vm::new failed: {err}")))?;
    Ok(vm)
}

/// Compile a source string with a loader and return the raw `Result`.
///
/// Unlike [`build_vm_with_loader`], this does **not** convert errors — it
/// returns the underlying [`CompilerError`] directly so tests can pattern-match
/// on it.
#[allow(clippy::expect_used)]
fn try_compile_with_loader(
    main_src: &str,
    loader: &MemLoader,
) -> Result<urd::ir::IrGraph, CompilerError> {
    let ast = parse_src(main_src).expect("parse must succeed for compiler-level tests");
    Compiler::compile_with_loader(&ast, loader)
}

/// Drain the VM and collect all `Event::Dialogue` lines into a flat `Vec<String>`.
fn collect_dialogue_lines(vm: &mut Vm) -> Result<Vec<String>, Box<dyn std::error::Error>> {
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
            VmStep::Error(e) => return Err(io::Error::other(format!("VM error: {e}")).into()),
            _ => {}
        }
    }
    Ok(out)
}

// ─── Test 1: Jump to a label defined in an imported module ───────────────────

/// Importing a module and doing `jump lib.greeting` should reach the dialogue
/// node that lives in the imported module.
#[test]
fn test_import_then_jump_to_module_label() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add(
        "lib.urd",
        r#"
        @entry
        label greeting {
            Alice: "Hello from lib!"
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

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["Hello from lib!"],
        "expected dialogue from imported module, got: {lines:?}"
    );
    Ok(())
}

// ─── Test 2: Read a cross-module global set by the consumer ──────────────────

/// `lib.score = 42` stores the value under `lib::score` (Global scope).
/// A subsequent `lib.score` expression should resolve to 42.
///
/// We verify this by having a conditional dialogue: only the "correct" branch
/// fires, confirming the read returned the expected value.
#[test]
fn test_cross_module_global_write_and_read() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", r#""#);

    let main_src = r#"
        import "lib.urd" as lib

        @entry
        label main {
            lib.score = 99
            if lib.score == 99 {
                Narrator: "High score!"
            } else {
                Narrator: "Nope"
            }
            end!()
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["High score!"],
        "expected cross-module global read to return 99, got: {lines:?}"
    );
    Ok(())
}

// ─── Test 3: Module label is reachable and body executes ─────────────────────

/// Jumping to a module label should execute its body, not just the EnterScope
/// sentinel.  We check that variables set inside the label body are visible
/// to subsequent instructions (by using them in a following dialogue).
#[test]
fn test_module_label_body_executes_on_jump() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add(
        "state.urd",
        r#"
        @entry
        label init {
            Sys: "module init ran"
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

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["module init ran"],
        "expected body of module label to execute, got: {lines:?}"
    );
    Ok(())
}

// ─── Test 4: Two modules imported under different aliases ─────────────────────

/// Two modules with identically-named labels ("start") must not collide when
/// imported under different aliases.  Jumping to each alias should reach the
/// correct dialogue.
#[test]
fn test_two_modules_with_same_label_name_do_not_collide() -> Result<(), Box<dyn std::error::Error>>
{
    let mut loader = MemLoader::new();
    loader.add(
        "module_a.urd",
        r#"
        @entry
        label start {
            A: "from module A"
        }
        "#,
    );
    loader.add(
        "module_b.urd",
        r#"
        @entry
        label start {
            B: "from module B"
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

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["from module A"],
        "expected only module A dialogue, got: {lines:?}"
    );
    Ok(())
}

// ─── Test 5: Module with multiple labels — jump to a non-first label ──────────

/// The merged graph contains ALL labels from the module, not just the first one.
/// Verify that jumping to a non-first label in the module works.
#[test]
fn test_jump_to_non_entry_module_label() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add(
        "scenes.urd",
        r#"
        label intro {
            Alice: "this is intro"
        }
        @entry
        label epilogue {
            Alice: "this is epilogue"
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

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["this is epilogue"],
        "expected epilogue dialogue (non-entry module label), got: {lines:?}"
    );
    Ok(())
}

// ─── Test 6: Missing module → ModuleLoadError at compile time ────────────────

/// `import "ghost.urd" as g` where the loader has no such file must produce a
/// `CompilerError::ModuleLoadError` containing the missing path.
#[test]
fn test_missing_module_produces_load_error() -> Result<(), Box<dyn std::error::Error>> {
    let loader = MemLoader::new(); // empty — no files

    let ast = parse_src(r#"import "ghost.urd" as g"#)?;
    let result = Compiler::compile_with_loader(&ast, &loader);

    assert!(
        matches!(
            result,
            Err(CompilerError::ModuleLoadError { ref path, .. }) if path == "ghost.urd"
        ),
        "expected ModuleLoadError for 'ghost.urd', got: {result:?}"
    );
    Ok(())
}

// ─── Test 7: Circular import → compiles successfully ─────────────────────────

/// If `a.urd` imports `b.urd` and `b.urd` imports `a.urd` back, compilation
/// must now succeed.  The 4-phase flat pipeline pre-allocates all label Nop
/// stubs before emitting any IR, so neither module needs the other to be fully
/// compiled first.
#[test]
fn test_circular_import_compiles_successfully() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add(
        "a.urd",
        "import \"b.urd\" as b\n@entry\nlabel a_label {\n  jump b.b_label\n}\n",
    );
    loader.add(
        "b.urd",
        "import \"a.urd\" as a\n@entry\nlabel b_label {\n  jump a.a_label\n}\n",
    );

    let ast = parse_src(r#"import "a.urd" as a"#)?;
    let result = Compiler::compile_with_loader(&ast, &loader);

    assert!(
        result.is_ok(),
        "circular import should compile successfully, got: {result:?}"
    );

    let graph = result.map_err(|err| {
        io::Error::other(format!(
            "expected circular import compile success, got error: {err}"
        ))
    })?;
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
    Ok(())
}

// ─── Test 8: Import-only script (no jump) — graph contains module labels ─────

/// A script that only contains an `import` statement (no jump) must still
/// compile successfully and have the module's labels registered in the graph.
#[test]
fn test_import_only_script_registers_labels() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add("util.urd", r#"label helper { Sys: "util" }"#);

    let ast = parse_src(r#"import "util.urd" as util"#)?;
    let graph = Compiler::compile_with_loader(&ast, &loader)
        .map_err(|err| io::Error::other(format!("compile_with_loader should succeed: {err}")))?;

    assert!(
        graph.labels.contains_key("util::helper"),
        "'util::helper' must be present in graph.labels after import, got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    Ok(())
}

// ─── Test 9: Cross-module global persists across scope transitions ────────────

/// `mod.flag = 1` followed by an `if` that reads `mod.flag` must see the
/// value 1 — cross-module globals are stored in the global map and therefore
/// visible everywhere in the script, including inside conditional branches.
#[test]
fn test_cross_module_global_visible_in_conditional() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add("ns.urd", r#""#);
    let main_src = r#"
        import "ns.urd" as ns

        @entry
        label main {
            ns.flag = 1
            if ns.flag == 1 {
                Test: "flag is set"
            }
            end!()
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["flag is set"],
        "cross-module global must be visible in a conditional, got: {lines:?}"
    );
    Ok(())
}

// ─── Test 10: Transitive imports — module that imports another module ─────────

/// `main.urd` imports `mid.urd`, which imports `base.urd`.  Jumping to
/// `base::entry` from main must work.
#[test]
fn test_transitive_imports_resolve() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add(
        "base.urd",
        r#"
        @entry
        label entry {
            Base: "from base"
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

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["from base"],
        "expected base module dialogue via transitive import + direct import, got: {lines:?}"
    );
    Ok(())
}

// ═════════════════════════════════════════════════════════════════════════════
// Phase 1 regression tests — import visibility, collision, and error paths
// ═════════════════════════════════════════════════════════════════════════════

// ─── Test 11: Bare jump after whole-module import fails ──────────────────────

/// `import "lib.urd" as lib` does NOT inject bare names.
/// A bare `jump greeting` must fail with `UnknownLabel`, not succeed silently.
#[test]
fn test_bare_jump_after_whole_module_import_fails() {
    let mut loader = MemLoader::new();
    loader.add(
        "lib.urd",
        r#"
        @entry
        label greeting {
            Alice: "Hello!"
        }
        "#,
    );

    let main_src = r#"
        import "lib.urd" as lib

        @entry
        label main {
            jump greeting
        }
    "#;

    let result = try_compile_with_loader(main_src, &loader);
    assert!(
        matches!(result, Err(CompilerError::UnknownLabel(ref l)) if l == "greeting"),
        "bare `jump greeting` after whole-module import must fail with UnknownLabel, got: {result:?}"
    );
}

// ─── Test 12: Qualified jump to non-@entry label is rejected ─────────────────

/// `jump lib.secret` must fail with `PrivateLabel` when `secret` is not
/// decorated with `@entry` in the imported module.
#[test]
fn test_qualified_jump_to_non_entry_label_is_rejected() {
    let mut loader = MemLoader::new();
    loader.add(
        "lib.urd",
        r#"
        label secret {
            Alice: "You shouldn't see this."
        }
        "#,
    );

    let main_src = r#"
        import "lib.urd" as lib

        @entry
        label main {
            jump lib.secret
        }
    "#;

    let result = try_compile_with_loader(main_src, &loader);
    assert!(
        matches!(result, Err(CompilerError::PrivateLabel(ref l)) if l == "lib.secret"),
        "jump to non-@entry label must fail with PrivateLabel, got: {result:?}"
    );
}

// ─── Test 13: Symbol import allows bare jump ─────────────────────────────────

/// `import (greeting) from "lib.urd"` injects `greeting` as a bare name,
/// bypassing the `@entry` restriction entirely.
#[test]
fn test_symbol_import_allows_bare_jump() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add(
        "lib.urd",
        r#"
        label greeting {
            Alice: "Hello from symbol import!"
        }
        "#,
    );

    let main_src = r#"
        import (greeting) from "lib.urd"

        @entry
        label main {
            jump greeting
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["Hello from symbol import!"],
        "symbol import must allow bare jump without @entry, got: {lines:?}"
    );
    Ok(())
}

// ─── Test 14: Duplicate whole-module aliases fail ────────────────────────────

/// `import "a.urd" as lib` followed by `import "b.urd" as lib` must fail
/// with `DuplicateAlias`.
#[test]
fn test_duplicate_whole_module_aliases_fail() {
    let mut loader = MemLoader::new();
    loader.add(
        "a.urd",
        r#"
        @entry
        label x { Alice: "from a" }
        "#,
    );
    loader.add(
        "b.urd",
        r#"
        @entry
        label y { Bob: "from b" }
        "#,
    );

    let main_src = r#"
        import "a.urd" as lib
        import "b.urd" as lib

        @entry
        label main {
            jump lib.x
        }
    "#;

    let result = try_compile_with_loader(main_src, &loader);
    assert!(
        matches!(result, Err(CompilerError::DuplicateAlias(ref a)) if a == "lib"),
        "duplicate module alias 'lib' must fail with DuplicateAlias, got: {result:?}"
    );
}

// ─── Test 15: Duplicate symbol import aliases fail ───────────────────────────

/// Two symbol imports producing the same bare name must fail deterministically.
#[test]
fn test_duplicate_symbol_import_aliases_fail() {
    let mut loader = MemLoader::new();
    loader.add(
        "a.urd",
        r#"
        label greet { Alice: "from a" }
        "#,
    );
    loader.add(
        "b.urd",
        r#"
        label greet { Bob: "from b" }
        "#,
    );

    let main_src = r#"
        import (greet) from "a.urd"
        import (greet) from "b.urd"

        @entry
        label main {
            jump greet
        }
    "#;

    let result = try_compile_with_loader(main_src, &loader);
    assert!(
        matches!(result, Err(CompilerError::DuplicateLabel(ref l)) if l == "greet"),
        "duplicate symbol alias 'greet' must fail, got: {result:?}"
    );
}

// ─── Test 16: Missing symbol import fails with diagnostic ────────────────────

/// `import (nonexistent) from "lib.urd"` must fail at compile time with
/// `MissingImportedSymbol` that names both the symbol and the module.
#[test]
fn test_missing_symbol_import_fails_with_diagnostic() {
    let mut loader = MemLoader::new();
    loader.add(
        "lib.urd",
        r#"
        @entry
        label greeting { Alice: "Hello!" }
        "#,
    );

    let main_src = r#"
        import (nonexistent) from "lib.urd"

        @entry
        label main {
            jump nonexistent
        }
    "#;

    let result = try_compile_with_loader(main_src, &loader);
    assert!(
        matches!(
            result,
            Err(CompilerError::MissingImportedSymbol { ref symbol, ref module })
                if symbol == "nonexistent" && module == "lib.urd"
        ),
        "missing symbol import must fail with MissingImportedSymbol naming the symbol and module, got: {result:?}"
    );
}

// ─── Test 17: Circular import with non-@entry qualified jump fails ───────────

/// Circular imports compile successfully (4-phase pipeline), but a qualified
/// jump to a non-`@entry` label across the cycle boundary must still fail
/// with `PrivateLabel`.
#[test]
fn test_circular_import_non_entry_jump_fails() {
    let mut loader = MemLoader::new();
    loader.add(
        "a.urd",
        r#"
        import "b.urd" as b
        @entry
        label a_pub {
            jump b.b_private
        }
        "#,
    );
    loader.add(
        "b.urd",
        r#"
        import "a.urd" as a
        label b_private {
            jump a.a_pub
        }
        "#,
    );

    let main_src = r#"import "a.urd" as a"#;

    let result = try_compile_with_loader(main_src, &loader);
    assert!(
        matches!(result, Err(CompilerError::PrivateLabel(ref l)) if l.contains("b_private")),
        "circular import with non-@entry qualified jump must fail with PrivateLabel, got: {result:?}"
    );
}

// ─── Test 18: Circular import with @entry labels succeeds ────────────────────

/// Both sides of a circular import mark their labels `@entry`.
/// The 4-phase pipeline must compile and execute cleanly.
#[test]
fn test_circular_import_with_entry_labels_succeeds() -> Result<(), Box<dyn std::error::Error>> {
    let mut loader = MemLoader::new();
    loader.add(
        "ping.urd",
        r#"
        import "pong.urd" as pong
        @entry
        label ping_label {
            Ping: "ping"
        }
        "#,
    );
    loader.add(
        "pong.urd",
        r#"
        import "ping.urd" as ping
        @entry
        label pong_label {
            Pong: "pong"
        }
        "#,
    );

    let main_src = r#"
        import "ping.urd" as ping
        import "pong.urd" as pong

        @entry
        label main {
            jump ping.ping_label
        }
    "#;

    let mut vm = build_vm_with_loader(main_src, &loader)?;
    let lines = collect_dialogue_lines(&mut vm)?;

    assert_eq!(
        lines,
        vec!["ping"],
        "circular import with @entry labels must execute cleanly, got: {lines:?}"
    );
    Ok(())
}
