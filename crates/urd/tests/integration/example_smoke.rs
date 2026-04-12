//! Smoke-tests for the example `.urd` files shipped with the repository.
//!
//! Each test loads an example source file, parses it, runs all analysis passes,
//! and asserts that **zero hard errors** are produced. Warnings are tolerated.

#![allow(clippy::expect_used)]

use urd::{analysis, compiler::loader::parse_source};

/// Parse an Urd source string and run all analysis passes.
/// Returns only the hard errors (warnings are filtered out).
fn analyze_errors(src: &str) -> Vec<analysis::AnalysisError> {
    let ast = parse_source(src).expect("example should parse without syntax errors");
    analysis::analyze(&ast)
        .into_iter()
        .filter(|e| !e.is_warning())
        .collect()
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

// ── Individual example smoke tests ───────────────────────────────────────────

#[test]
fn functions_urd_has_no_errors() {
    let src = std::fs::read_to_string(example_path("functions.urd"))
        .expect("failed to read examples/functions.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "examples/functions.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

#[test]
fn collections_urd_has_no_errors() {
    let src = std::fs::read_to_string(example_path("collections.urd"))
        .expect("failed to read examples/collections.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "examples/collections.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

#[test]
fn quest_cave_urd_has_no_errors() {
    let src = std::fs::read_to_string(example_path("quest/cave.urd"))
        .expect("failed to read examples/quest/cave.urd");
    let errors = analyze_errors(&src);
    assert!(
        errors.is_empty(),
        "examples/quest/cave.urd produced {} unexpected error(s):\n{errors:#?}",
        errors.len(),
    );
}

// ── Parse-only tests for files we intentionally skip from full analysis ──────

#[test]
fn dice_urd_parses_successfully() {
    let src = std::fs::read_to_string(example_path("dice.urd"))
        .expect("failed to read examples/dice.urd");
    parse_source(&src).expect("examples/dice.urd should parse without syntax errors");
}

#[test]
fn extern_demo_urd_parses_successfully() {
    let src = std::fs::read_to_string(example_path("extern_demo.urd"))
        .expect("failed to read examples/extern_demo.urd");
    parse_source(&src).expect("examples/extern_demo.urd should parse without syntax errors");
}
