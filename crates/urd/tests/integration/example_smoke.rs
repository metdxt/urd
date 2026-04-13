//! Smoke-tests for the example `.urd` files shipped with the repository.
//!
//! Each test loads an example source file, parses it, runs all analysis passes,
//! and asserts that **zero hard errors** are produced.  Warnings are tolerated.
//!
//! ## Multi-file examples
//!
//! The current flat compiler's symbol-import resolution (`import (X) from "y"`)
//! only resolves **labels** — not consts, structs, or enums.  All three
//! multi-file example directories (`circular/`, `localization/`, `multifile/`)
//! use symbol imports for non-label items and therefore **cannot be fully
//! compiled yet**.  The tests below document this limitation explicitly:
//!
//! - Each individual `.urd` file is tested for successful **parsing**.
//! - Multi-file **compilation** is expected to fail with
//!   [`CompilerError::MissingImportedSymbol`] — tests assert the correct error.
//! - When/if the compiler gains full symbol import support, these tests should
//!   be upgraded to assert successful compilation.

#![allow(clippy::expect_used)]

use urd::{
    analysis,
    compiler::{
        loader::{compile_recursive_with_root_path, parse_source},
        Compiler, CompilerError,
    },
    loc::ftl::generate_ftl,
    vm::loader::FsLoader,
};

// ─── Helpers ─────────────────────────────────────────────────────────────────

/// Parse an Urd source string and run all analysis passes.
/// Returns only the hard errors (warnings are filtered out).
fn analyze_errors(src: &str) -> Vec<analysis::AnalysisError> {
    let ast = parse_source(src).expect("example should parse without syntax errors");
    analysis::analyze(&ast)
        .into_iter()
        .filter(|e| !e.is_warning())
        .collect()
}

/// Parse an Urd source string and run all analysis passes.
/// Returns *all* diagnostics — both errors and warnings.
fn analyze_all(src: &str) -> Vec<analysis::AnalysisError> {
    let ast = parse_source(src).expect("example should parse without syntax errors");
    analysis::analyze(&ast)
}

/// Helper: build an absolute path to an example file relative to the workspace
/// `examples/` directory.  `CARGO_MANIFEST_DIR` points at `crates/urd`, so the
/// workspace root is two levels up.
fn example_path(relative: &str) -> std::path::PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    std::path::Path::new(manifest)
        .join("../../examples")
        .join(relative)
}

/// Read a single example file and return its contents.
fn read_example(relative: &str) -> String {
    let path = example_path(relative);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read examples/{relative}: {e}"))
}

/// Attempt to compile a multi-file example rooted at `dir` with entry-point
/// `filename`.  Returns the raw `Result` so callers can assert on both success
/// and expected failure.
///
/// `dir` is relative to the workspace `examples/` directory
/// (e.g. `"circular"`).  `filename` is the entry-point file within that
/// directory (e.g. `"main.urd"`).
fn try_compile_multifile(
    dir: &str,
    filename: &str,
) -> Result<urd::IrGraph, CompilerError> {
    let base = example_path(dir);
    let loader = FsLoader::new(&base);
    let src = std::fs::read_to_string(base.join(filename))
        .unwrap_or_else(|e| panic!("failed to read examples/{dir}/{filename}: {e}"));
    let ast = parse_source(&src).unwrap_or_else(|e| {
        panic!("examples/{dir}/{filename} should parse without syntax errors: {e}")
    });
    // Pass the full filename (e.g. "main.urd") as root_path — this matches
    // what the `quest` CLI does and is required so that back-imports like
    // `import "main.urd" as main` can deduplicate against the root module.
    compile_recursive_with_root_path(&ast, filename, &loader)
}

// ═════════════════════════════════════════════════════════════════════════════
// SINGLE-FILE EXAMPLES — full analysis (parse + analyse, zero hard errors)
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn functions_urd_has_no_errors() {
    let src = read_example("functions.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "examples/functions.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

#[test]
fn collections_urd_has_no_errors() {
    let src = read_example("collections.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "examples/collections.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

#[test]
fn quest_cave_urd_has_no_errors() {
    let src = read_example("quest/cave.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "examples/quest/cave.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

// ═════════════════════════════════════════════════════════════════════════════
// SINGLE-FILE — parse-only (files that intentionally skip full analysis)
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn dice_urd_parses_successfully() {
    let src = read_example("dice.urd");
    parse_source(&src).expect("examples/dice.urd should parse without syntax errors");
}

#[test]
fn extern_demo_urd_parses_successfully() {
    let src = read_example("extern_demo.urd");
    parse_source(&src).expect("examples/extern_demo.urd should parse without syntax errors");
}

// ═════════════════════════════════════════════════════════════════════════════
// SINGLE-FILE — compile to IrGraph
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn functions_urd_compiles_to_non_empty_graph() {
    let src = read_example("functions.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    assert!(
        graph.graph().node_count() > 0,
        "functions.urd compiled to an empty graph"
    );
    assert!(
        !graph.labels.is_empty(),
        "functions.urd should define at least one label"
    );
}

#[test]
fn collections_urd_compiles_to_non_empty_graph() {
    let src = read_example("collections.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    assert!(
        graph.graph().node_count() > 0,
        "collections.urd compiled to an empty graph"
    );
}

#[test]
fn quest_cave_urd_compiles_to_non_empty_graph() {
    let src = read_example("quest/cave.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    assert!(
        graph.graph().node_count() > 0,
        "quest/cave.urd compiled to an empty graph"
    );
    assert!(graph.entry.is_some(), "cave.urd should have an entry point");
}

#[test]
fn dice_urd_compiles_to_non_empty_graph() {
    let src = read_example("dice.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    assert!(
        graph.graph().node_count() > 0,
        "dice.urd compiled to an empty graph"
    );
}

#[test]
fn extern_demo_urd_compiles_to_non_empty_graph() {
    let src = read_example("extern_demo.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile(&ast).expect("compile");
    assert!(
        graph.graph().node_count() > 0,
        "extern_demo.urd compiled to an empty graph"
    );
}

// ═════════════════════════════════════════════════════════════════════════════
// SINGLE-FILE — FTL generation from single-file compile
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn quest_cave_urd_generates_non_empty_ftl() {
    let src = read_example("quest/cave.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile_named(&ast, "cave").expect("compile_named");
    let ftl = generate_ftl(&graph, "cave");
    assert!(
        ftl.contains("cave.ftl"),
        "FTL output should reference cave.ftl in the header; got:\n{ftl}"
    );
    assert!(
        ftl.contains("generated by urd"),
        "FTL output should contain the 'generated by urd' marker"
    );
    // cave.urd defines labels with dialogue; the FTL should contain entries.
    assert!(
        ftl.contains("label:"),
        "FTL output should contain at least one label section; got:\n{ftl}"
    );
}

#[test]
fn functions_urd_generates_non_empty_ftl() {
    let src = read_example("functions.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile_named(&ast, "functions").expect("compile_named");
    let ftl = generate_ftl(&graph, "functions");
    assert!(
        ftl.contains("functions.ftl"),
        "FTL header should name the file slug"
    );
}

// ═════════════════════════════════════════════════════════════════════════════
// errors.urd — intentional diagnostics showcase
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn errors_urd_parses_successfully() {
    let src = read_example("errors.urd");
    parse_source(&src).expect("examples/errors.urd should parse without syntax errors");
}

#[test]
fn errors_urd_compile_fails_with_unknown_label() {
    // errors.urd contains a deliberate jump to a misspelled label
    // ("confrontaion" instead of "confrontation").  The compiler should
    // reject this with an `UnknownLabel` error rather than silently produce
    // a broken graph.
    let src = read_example("errors.urd");
    let ast = parse_source(&src).expect("parse");
    let result = Compiler::compile(&ast);
    assert!(
        result.is_err(),
        "errors.urd should fail to compile (deliberate unknown-label typo)"
    );
    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("confrontaion"),
        "compile error should mention the misspelled label 'confrontaion'; got: {err_msg}"
    );
}

#[test]
fn errors_urd_produces_warnings() {
    let src = read_example("errors.urd");
    let all = analyze_all(&src);
    let warnings: Vec<_> = all.iter().filter(|e| e.is_warning()).collect();
    // The file is designed to trigger many warnings — at least several should fire.
    assert!(
        !warnings.is_empty(),
        "errors.urd should produce analysis warnings (it is a diagnostics showcase)"
    );
}

#[test]
fn errors_urd_produces_hard_errors() {
    let src = read_example("errors.urd");
    let all = analyze_all(&src);
    let hard_errors: Vec<_> = all.iter().filter(|e| !e.is_warning()).collect();
    // The file is designed to trigger deliberate hard errors (TypeMismatch,
    // UndefinedLabel, etc.) — verify the analyser actually catches them.
    assert!(
        !hard_errors.is_empty(),
        "errors.urd should produce analysis hard errors (it is a diagnostics showcase)"
    );
}

// ═════════════════════════════════════════════════════════════════════════════
// circular/ — two files with circular imports
//
// NOTE: tavern.urd uses `import (narrator, hero, Character) from "main.urd"`
// which imports non-label symbols.  The current flat compiler only resolves
// labels in symbol imports, so full compilation fails with
// `MissingImportedSymbol`.  Tests below assert the expected error and verify
// that each individual file parses correctly.
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn circular_main_parses_successfully() {
    let src = read_example("circular/main.urd");
    parse_source(&src).expect("circular/main.urd should parse without syntax errors");
}

#[test]
fn circular_tavern_parses_successfully() {
    let src = read_example("circular/tavern.urd");
    parse_source(&src).expect("circular/tavern.urd should parse without syntax errors");
}

#[test]
fn circular_main_analysis_runs_without_panic() {
    let src = read_example("circular/main.urd");
    // Will produce errors for unresolved cross-module labels, but must not panic.
    let _all = analyze_all(&src);
}

#[test]
fn circular_tavern_analysis_runs_without_panic() {
    let src = read_example("circular/tavern.urd");
    let _all = analyze_all(&src);
}

#[test]
fn circular_compile_fails_with_missing_symbol_import() {
    // tavern.urd does `import (narrator, hero, Character) from "main.urd"`.
    // The flat compiler only resolves labels as importable symbols, so this
    // fails because `narrator` is a const, not a label.
    let result = try_compile_multifile("circular", "main.urd");
    assert!(
        result.is_err(),
        "circular/ should fail to compile until symbol imports support non-label items"
    );
    let err = result.unwrap_err();
    match &err {
        CompilerError::MissingImportedSymbol { symbol, module } => {
            assert_eq!(module, "main.urd", "error should reference main.urd");
            // narrator, hero, or Character — any of these could be the first
            // one the compiler trips over depending on iteration order.
            assert!(
                symbol == "narrator" || symbol == "hero" || symbol == "Character",
                "expected MissingImportedSymbol for narrator/hero/Character, got: {symbol}"
            );
        }
        other => panic!(
            "expected CompilerError::MissingImportedSymbol, got: {other:?}"
        ),
    }
}

// ═════════════════════════════════════════════════════════════════════════════
// localization/ — bazaar + merchant with @fluent and circular imports
//
// NOTE: merchant.urd uses `import (narrator, elara, ...) from "bazaar.urd"`
// which imports consts/globals.  Same limitation as circular/ above.
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn localization_bazaar_parses_successfully() {
    let src = read_example("localization/bazaar.urd");
    parse_source(&src).expect("localization/bazaar.urd should parse without syntax errors");
}

#[test]
fn localization_merchant_parses_successfully() {
    let src = read_example("localization/merchant.urd");
    parse_source(&src).expect("localization/merchant.urd should parse without syntax errors");
}

#[test]
fn localization_bazaar_analysis_runs_without_panic() {
    let src = read_example("localization/bazaar.urd");
    let _all = analyze_all(&src);
}

#[test]
fn localization_merchant_analysis_runs_without_panic() {
    let src = read_example("localization/merchant.urd");
    let _all = analyze_all(&src);
}

#[test]
fn localization_compile_fails_with_missing_symbol_import() {
    // merchant.urd does `import (narrator, elara, ...) from "bazaar.urd"`.
    let result = try_compile_multifile("localization", "bazaar.urd");
    assert!(
        result.is_err(),
        "localization/ should fail to compile until symbol imports support non-label items"
    );
    let err = result.unwrap_err();
    match &err {
        CompilerError::MissingImportedSymbol { symbol, module } => {
            assert_eq!(module, "bazaar.urd", "error should reference bazaar.urd");
            assert!(
                symbol == "narrator" || symbol == "elara"
                    || symbol == "gold" || symbol == "price"
                    || symbol == "has_potion" || symbol == "haggled",
                "expected MissingImportedSymbol for a bazaar.urd const/global, got: {symbol}"
            );
        }
        other => panic!(
            "expected CompilerError::MissingImportedSymbol, got: {other:?}"
        ),
    }
}

#[test]
fn localization_bazaar_standalone_compile_fails_on_cross_module_jump() {
    // bazaar.urd contains `jump merchant.browse` — a cross-module jump that
    // requires a loader.  Standalone compilation (no loader) correctly rejects
    // this with `UnknownLabel`.
    let src = read_example("localization/bazaar.urd");
    let ast = parse_source(&src).expect("parse");
    let result = Compiler::compile(&ast);
    assert!(
        result.is_err(),
        "bazaar.urd standalone compile should fail (cross-module jumps)"
    );
    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("merchant.browse") || err_msg.contains("UnknownLabel"),
        "error should mention the unresolved cross-module jump; got: {err_msg}"
    );
}

#[test]
fn localization_merchant_standalone_compile_fails_on_cross_module_jump() {
    // merchant.urd contains `jump bazaar.farewell` — same issue.
    let src = read_example("localization/merchant.urd");
    let ast = parse_source(&src).expect("parse");
    let result = Compiler::compile(&ast);
    assert!(
        result.is_err(),
        "merchant.urd standalone compile should fail (cross-module jumps)"
    );
    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("bazaar.farewell") || err_msg.contains("UnknownLabel"),
        "error should mention the unresolved cross-module jump; got: {err_msg}"
    );
}

// ═════════════════════════════════════════════════════════════════════════════
// multifile/ — 5-file project (main, characters, items, village, dungeon)
//
// NOTE: All non-root files use `import (X) from "y.urd"` for structs, enums,
// consts, and globals.  Same symbol-import limitation as above.
// ═════════════════════════════════════════════════════════════════════════════

#[test]
fn multifile_main_parses_successfully() {
    let src = read_example("multifile/main.urd");
    parse_source(&src).expect("multifile/main.urd should parse without syntax errors");
}

#[test]
fn multifile_characters_parses_successfully() {
    let src = read_example("multifile/characters.urd");
    parse_source(&src).expect("multifile/characters.urd should parse without syntax errors");
}

#[test]
fn multifile_items_parses_successfully() {
    let src = read_example("multifile/items.urd");
    parse_source(&src).expect("multifile/items.urd should parse without syntax errors");
}

#[test]
fn multifile_village_parses_successfully() {
    let src = read_example("multifile/village.urd");
    parse_source(&src).expect("multifile/village.urd should parse without syntax errors");
}

#[test]
fn multifile_dungeon_parses_successfully() {
    let src = read_example("multifile/dungeon.urd");
    parse_source(&src).expect("multifile/dungeon.urd should parse without syntax errors");
}

#[test]
fn multifile_compile_fails_with_missing_symbol_import() {
    // main.urd does `import (Faction, narrator, ...) from "characters.urd"`
    // which imports struct/enum/const items the flat compiler cannot resolve.
    let result = try_compile_multifile("multifile", "main.urd");
    assert!(
        result.is_err(),
        "multifile/ should fail to compile until symbol imports support non-label items"
    );
    let err = result.unwrap_err();
    match &err {
        CompilerError::MissingImportedSymbol { symbol, module } => {
            assert_eq!(module, "characters.urd", "error should reference characters.urd");
            assert!(
                symbol == "Character" || symbol == "Faction"
                    || symbol == "narrator" || symbol == "hero"
                    || symbol == "merchant" || symbol == "villain",
                "expected MissingImportedSymbol for a characters.urd item, got: {symbol}"
            );
        }
        other => panic!(
            "expected CompilerError::MissingImportedSymbol, got: {other:?}"
        ),
    }
}

#[test]
fn multifile_characters_has_no_analysis_errors() {
    // characters.urd has no imports and no labels — only struct, enum, and
    // const definitions.  It should analyse cleanly on its own.
    let src = read_example("multifile/characters.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "multifile/characters.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

#[test]
fn multifile_items_has_no_analysis_errors() {
    // items.urd has no imports — should analyse cleanly.
    let src = read_example("multifile/items.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "multifile/items.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

#[test]
fn multifile_characters_standalone_compiles() {
    let src = read_example("multifile/characters.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile(&ast).expect("standalone compile");
    // characters.urd has only type/const definitions, no labels.  The graph
    // exists but may have zero or very few nodes — the important thing is
    // that compilation doesn't fail.
    let _ = graph;
}

#[test]
fn multifile_items_standalone_compiles_to_non_empty_graph() {
    let src = read_example("multifile/items.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile(&ast).expect("standalone compile");
    assert!(
        graph.graph().node_count() > 0,
        "multifile/items.urd standalone compiled to an empty graph"
    );
    assert!(
        graph.labels.contains_key("show_inventory"),
        "items.urd should define the 'show_inventory' label"
    );
    assert!(
        graph.labels.contains_key("buy_item"),
        "items.urd should define the 'buy_item' label"
    );
}

#[test]
fn multifile_items_generates_ftl() {
    let src = read_example("multifile/items.urd");
    let ast = parse_source(&src).expect("parse");
    let graph = Compiler::compile_named(&ast, "items").expect("compile_named");
    let ftl = generate_ftl(&graph, "items");
    assert!(
        ftl.contains("items.ftl"),
        "FTL header should name items.ftl; got:\n{ftl}"
    );
}

// ── Analysis-doesn't-panic tests for files with unresolvable imports ─────────

#[test]
fn multifile_village_analysis_runs_without_panic() {
    let src = read_example("multifile/village.urd");
    let _all = analyze_all(&src);
}

#[test]
fn multifile_dungeon_analysis_runs_without_panic() {
    let src = read_example("multifile/dungeon.urd");
    let _all = analyze_all(&src);
}

#[test]
fn multifile_main_analysis_runs_without_panic() {
    let src = read_example("multifile/main.urd");
    let _all = analyze_all(&src);
}
