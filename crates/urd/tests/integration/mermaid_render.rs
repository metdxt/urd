#![allow(missing_docs)]

//! Integration tests for the Mermaid flowchart renderer.
//!
//! Uses the same `EXAMPLE_SCRIPT` as `dot_render.rs` to exercise every major
//! language feature, then makes structural assertions on the rendered Mermaid
//! string.  No external tooling is required — Mermaid is pure text.

use std::io;

use urd::{compiler::Compiler, parse_test, parser::block::script};

use super::fixtures::EXAMPLE_SCRIPT;

// ── Helpers ─────────────────────────────────────────────────────────────────

fn compile_example() -> Result<urd::ir::IrGraph, Box<dyn std::error::Error>> {
    compile_fixture(EXAMPLE_SCRIPT)
}

fn compile_fixture(src: &str) -> Result<urd::ir::IrGraph, Box<dyn std::error::Error>> {
    let ast = parse_test!(script(), src)
        .map_err(|err| io::Error::other(format!("fixture should parse without errors: {err:?}")))?;
    let graph = Compiler::compile(&ast)
        .map_err(|err| io::Error::other(format!("fixture should compile without errors: {err}")))?;
    Ok(graph)
}

// ── Parse + compile ──────────────────────────────────────────────────────────

#[test]
fn example_script_parses() -> Result<(), Box<dyn std::error::Error>> {
    parse_test!(script(), EXAMPLE_SCRIPT)
        .map_err(|err| io::Error::other(format!("example script must parse cleanly: {err:?}")))?;
    Ok(())
}

#[test]
fn example_script_compiles() -> Result<(), Box<dyn std::error::Error>> {
    compile_example()?;
    Ok(())
}

#[test]
fn compiled_graph_has_expected_label_count() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_example()?;
    // start, talk, notice_board, compute_rank, log_visit
    assert_eq!(
        graph.labels.len(),
        5,
        "expected 5 named labels, got {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    for name in ["start", "talk", "notice_board", "compute_rank", "log_visit"] {
        assert!(
            graph.labels.contains_key(name),
            "label '{name}' should be present in the compiled graph"
        );
    }
    Ok(())
}

// ── Mermaid structural assertions ────────────────────────────────────────────

#[test]
fn mermaid_has_flowchart_header() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    assert!(
        mmd.contains("%%{init:"),
        "Mermaid output must contain %%{{init:}} directive for edge colour, got:\n{}",
        &mmd[..mmd.len().min(120)]
    );
    assert!(
        mmd.contains("lineColor"),
        "init directive must set lineColor, got:\n{}",
        &mmd[..mmd.len().min(120)]
    );
    assert!(
        mmd.contains("flowchart LR"),
        "Mermaid output must contain 'flowchart LR', got:\n{}",
        &mmd[..mmd.len().min(120)]
    );
    // init directive must come before the flowchart declaration
    let init_pos = mmd
        .find("%%{init:")
        .ok_or_else(|| io::Error::other("expected init directive marker in Mermaid output"))?;
    let flowchart_pos = mmd
        .find("flowchart LR")
        .ok_or_else(|| io::Error::other("expected flowchart header in Mermaid output"))?;
    assert!(
        init_pos < flowchart_pos,
        "%%{{init:}} directive must appear before 'flowchart LR'"
    );
    Ok(())
}

#[test]
fn mermaid_has_classdefs() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    for class in [
        "classDef dialogue",
        "classDef branch",
        "classDef choice",
        "classDef assign",
        "classDef jump",
        "classDef letcall",
        "classDef ret",
        "classDef scope",
        "classDef enumDef",
        "classDef endNode",
        "classDef startNode",
    ] {
        assert!(
            mmd.contains(class),
            "Mermaid output must contain '{class}' classDef declaration"
        );
    }
    Ok(())
}

#[test]
fn mermaid_has_start_node() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    assert!(
        mmd.contains("__start__"),
        "Mermaid output must contain the __start__ synthetic entry node"
    );
    assert!(
        mmd.contains("▶ START"),
        "Mermaid output must contain the ▶ START label on the start node"
    );
    Ok(())
}

#[test]
fn mermaid_has_subgraph_for_every_label() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    for label in ["start", "talk", "notice_board", "compute_rank", "log_visit"] {
        assert!(
            mmd.contains(&format!("subgraph cluster_{label}")),
            "Mermaid output must contain a subgraph for label '{label}'"
        );
    }
    Ok(())
}

#[test]
fn mermaid_has_enter_scope_markers() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    for label in ["start", "talk", "notice_board", "compute_rank", "log_visit"] {
        assert!(
            mmd.contains(&format!("▶ {label}")),
            "Mermaid output must contain an EnterScope (▶) marker for label '{label}'"
        );
    }
    Ok(())
}

#[test]
fn mermaid_has_dialogue_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    assert!(
        mmd.contains(":::dialogue"),
        "Mermaid output must contain at least one dialogue node (:::dialogue class)"
    );
    Ok(())
}

#[test]
fn mermaid_has_choice_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    assert!(
        mmd.contains(":::choice"),
        "Mermaid output must contain at least one choice node (:::choice class)"
    );
    Ok(())
}

#[test]
fn mermaid_has_branch_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    assert!(
        mmd.contains(":::branch"),
        "Mermaid output must contain at least one branch node (:::branch class)"
    );
    assert!(
        mmd.contains("then"),
        "branch edges must include a 'then' label"
    );
    assert!(
        mmd.contains("else"),
        "branch edges must include an 'else' label"
    );
    Ok(())
}

#[test]
fn mermaid_has_jump_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    assert!(
        mmd.contains(":::jump"),
        "Mermaid output must contain at least one jump node (:::jump class)"
    );
    assert!(mmd.contains("-.->"), "jump edges must be dashed (-.->)");
    Ok(())
}

#[test]
fn mermaid_has_let_call_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    // LetCall nodes use the ⤑ prefix in their label.
    assert!(
        mmd.contains("⤑"),
        "Mermaid output must contain at least one LetCall node with a '⤑' label prefix"
    );
    assert!(
        mmd.contains(":::letcall"),
        "LetCall nodes must carry the :::letcall class"
    );
    // Both subroutine call sites must reference their callees.
    assert!(
        mmd.contains("compute_rank"),
        "Mermaid output must reference compute_rank in a LetCall node"
    );
    assert!(
        mmd.contains("log_visit"),
        "Mermaid output must reference log_visit in a LetCall node"
    );
    Ok(())
}

#[test]
fn mermaid_has_subroutine_return_edges() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    // Subroutine Returns must emit dashed ↩ back-edges to the caller continuation,
    // not a bare edge to __end__.
    assert!(
        mmd.contains("↩"),
        "Mermaid output must contain ↩ back-edges for subroutine Return nodes \
         (compute_rank and log_visit both return to the start cluster)"
    );
    Ok(())
}

#[test]
fn mermaid_has_end_sink() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    assert!(
        mmd.contains("__end__"),
        "Mermaid output must contain the __end__ sink node for terminal edges"
    );
    assert!(
        mmd.contains(":::endNode"),
        "__end__ sink must carry the :::endNode class"
    );
    Ok(())
}

#[test]
fn mermaid_has_assign_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    // global reputation = 0
    assert!(
        mmd.contains("global reputation"),
        "Mermaid output must contain an Assign node for 'global reputation'"
    );
    // const max_health = 100
    assert!(
        mmd.contains("const max_health"),
        "Mermaid output must contain an Assign node for 'const max_health'"
    );
    Ok(())
}

#[test]
fn mermaid_has_define_enum_node() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    // Preamble definitions (globals, consts, enums, decorators) are collapsed
    // into a single __preamble__ summary node.
    assert!(
        mmd.contains("__preamble__"),
        "Mermaid output must contain a __preamble__ summary node for top-level definitions"
    );
    assert!(
        mmd.contains("enum Action"),
        "Mermaid preamble must mention 'enum Action'"
    );
    Ok(())
}

#[test]
fn mermaid_has_return_node() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    // The script has several Return nodes (in compute_rank, log_visit, and the
    // "Leave the tavern" menu option).
    assert!(
        mmd.contains(":::ret"),
        "Mermaid output must contain at least one Return node (:::ret class)"
    );
    Ok(())
}

/// Test 19 — the `LetCall.next` cluster fix.
///
/// Verifies that after the `compute_clusters` fix, the continuation node
/// (`next`) of a `LetCall` is placed inside the **same** subgraph cluster as
/// the call site, rather than being orphaned outside it.
///
/// In the example script the `start` label contains:
/// ```text
/// let rank = jump compute_rank and return   ← LetCall
/// jump log_visit and return                 ← also LetCall, next = the @important menu
/// @important menu { … }                     ← must be in cluster_start
/// ```
/// Before the fix, `compute_clusters` treated `LetCall` as a terminal and
/// never enqueued `next`, so the choice menu and everything after it fell
/// outside `cluster_start`.
#[test]
fn mermaid_nodes_after_letcall_in_cluster() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();

    // Find the subgraph cluster_start block.
    let start_pos = mmd
        .find("subgraph cluster_start")
        .ok_or_else(|| io::Error::other("subgraph cluster_start must exist in Mermaid output"))?;

    // Find the "end" line that closes this subgraph (first `\n    end` after open).
    let end_rel = mmd[start_pos..]
        .find("\n    end")
        .ok_or_else(|| io::Error::other("subgraph cluster_start must have a closing 'end'"))?;
    let end_pos = start_pos + end_rel;

    let cluster_block = &mmd[start_pos..end_pos];

    // The LetCall nodes (⤑ compute_rank and ⤑ log_visit) must be inside the block.
    assert!(
        cluster_block.contains(":::letcall"),
        "LetCall node(s) must appear inside subgraph cluster_start.\n\
         Block content:\n{cluster_block}"
    );

    // The @important choice menu that follows the two LetCalls must also be
    // inside cluster_start.  This is the critical regression check.
    assert!(
        cluster_block.contains(":::choice"),
        "The choice menu (:::choice) must appear inside subgraph cluster_start \
         — this proves that LetCall.next is followed into the same cluster.\n\
         Block content:\n{cluster_block}"
    );
    Ok(())
}

#[test]
fn mermaid_is_non_empty() -> Result<(), Box<dyn std::error::Error>> {
    assert!(
        !compile_example()?.to_mermaid().is_empty(),
        "Mermaid output must not be empty"
    );
    Ok(())
}

/// Writes `target/mermaid_example.mmd` for manual inspection.
///
/// Always passes — purely a developer convenience artifact.
/// Paste the file contents into <https://mermaid.live> to view the diagram.
#[test]
fn mermaid_write_artifact_for_inspection() -> Result<(), Box<dyn std::error::Error>> {
    let mmd = compile_example()?.to_mermaid();
    let out_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("target");
    std::fs::create_dir_all(&out_dir)?;
    let path = out_dir.join("mermaid_example.mmd");
    std::fs::write(&path, &mmd)?;
    println!("Mermaid artifact: {}", path.display());
    Ok(())
}

// ── Focused fixture tests ─────────────────────────────────────────────────

/// Minimal dialogue fixture compiles and renders non-empty Mermaid output.
#[test]
fn test_mermaid_minimal_dialogue() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_DIALOGUE)?;
    let mmd = graph.to_mermaid();
    assert!(
        !mmd.is_empty(),
        "minimal dialogue fixture must produce Mermaid output"
    );
    assert!(
        mmd.contains(":::dialogue"),
        "dialogue fixture must contain a :::dialogue node"
    );
    Ok(())
}

/// Minimal branching fixture renders branch edges.
#[test]
fn test_mermaid_minimal_branching() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_BRANCHING)?;
    let mmd = graph.to_mermaid();
    assert!(
        mmd.contains(":::branch"),
        "branching fixture must contain :::branch node"
    );
    Ok(())
}

/// Minimal menu fixture renders choice nodes.
#[test]
fn test_mermaid_minimal_menu() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_MENU)?;
    let mmd = graph.to_mermaid();
    assert!(
        mmd.contains(":::choice"),
        "menu fixture must contain :::choice node"
    );
    Ok(())
}
