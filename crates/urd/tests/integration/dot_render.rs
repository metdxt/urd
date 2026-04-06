#![allow(missing_docs)]

//! Integration tests for the DOT graph renderer.
//!
//! Includes a rich example Urd script that exercises every major language
//! feature, structural assertions on the rendered DOT string, and a live
//! Graphviz validation test that pipes the output through `dot -Tsvg`.

use std::io::{self, Write as _};
use std::process::{Command, Stdio};

use urd::{compiler::Compiler, parse_test, parser::block::script};

use super::fixtures::EXAMPLE_SCRIPT;

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Compile an arbitrary fixture string into an [`IrGraph`].
fn compile_fixture(src: &str) -> Result<urd::ir::IrGraph, Box<dyn std::error::Error>> {
    let ast = parse_test!(script(), src)
        .map_err(|err| io::Error::other(format!("fixture should parse without errors: {err:?}")))?;
    let graph = Compiler::compile(&ast)
        .map_err(|err| io::Error::other(format!("fixture should compile without errors: {err}")))?;
    Ok(graph)
}

/// Compile the full [`EXAMPLE_SCRIPT`] showcase fixture.
fn compile_example() -> Result<urd::ir::IrGraph, Box<dyn std::error::Error>> {
    compile_fixture(EXAMPLE_SCRIPT)
}

// ── Parse + compile ───────────────────────────────────────────────────────────

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

// ── DOT structural assertions (no Graphviz required) ─────────────────────────

#[test]
fn dot_is_non_empty() -> Result<(), Box<dyn std::error::Error>> {
    assert!(!compile_example()?.to_dot().is_empty());
    Ok(())
}

#[test]
fn dot_has_digraph_wrapper() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(
        dot.starts_with("digraph urd_script {"),
        "DOT must open with digraph header"
    );
    assert!(dot.ends_with("}\n"), "DOT must close with }}");
    assert!(
        dot.contains("splines=ortho"),
        "DOT must set splines=ortho for right-angle edge routing"
    );
    Ok(())
}

#[test]
fn dot_has_entry_arrow() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(
        dot.contains("__start__"),
        "DOT must contain an entry-point arrow"
    );
    Ok(())
}

#[test]
fn dot_has_scope_nodes_for_every_label() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    for label in ["start", "talk", "notice_board"] {
        // Each label must produce a cluster subgraph block.
        assert!(
            dot.contains(&format!("subgraph cluster_{label}")),
            "DOT must contain a cluster subgraph for label '{label}'"
        );
        assert!(
            dot.contains(&format!("label=\"{label}\"")),
            "cluster for '{label}' must display the label name"
        );
        // EnterScope (▶) is always reachable — it is the jump target.
        assert!(
            dot.contains(&format!("▶ {label}")),
            "DOT must contain compact EnterScope marker for '{label}'"
        );
        // ExitScope (◀) is NOT checked here: in the example script every label
        // terminates via `jump` or `return`, so ExitScope nodes are dead code
        // and are correctly filtered out by the reachability pass.
        // See test_dead_exit_scope_not_rendered in the unit tests for the
        // explicit assertion of that behaviour.
    }
    // Cluster border and scope marker nodes are darkgreen / palegreen.
    assert!(
        dot.contains("darkgreen"),
        "clusters must use darkgreen border"
    );
    assert!(
        dot.contains("#66bb6a"),
        "scope marker nodes must use new green fill"
    );
    Ok(())
}

#[test]
fn dot_has_dialogue_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(
        dot.contains("#b8c0cc"),
        "DOT must contain at least one dialogue node (muted slate fill)"
    );
    Ok(())
}

#[test]
fn dot_has_choice_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(
        dot.contains("hexagon"),
        "DOT must contain at least one choice node (hexagon shape)"
    );
    // Choice edges must be purple
    assert!(
        dot.contains("color=purple"),
        "choice option edges must be purple"
    );
    Ok(())
}

#[test]
fn dot_choice_option_labels_are_present() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // A sample of option labels from the script
    for label in [
        "Talk to the innkeeper",
        "Check the notice board",
        "Ask about local rumours",
        "Buy a drink",
    ] {
        assert!(
            dot.contains(label),
            "choice option label '{label}' should appear in DOT"
        );
    }
    Ok(())
}

#[test]
fn dot_has_branch_nodes_for_if_elif_else() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(
        dot.contains("diamond"),
        "DOT must contain branch/switch nodes (diamond shape)"
    );
    assert!(dot.contains("then"), "branch edges must be labelled 'then'");
    assert!(dot.contains("else"), "branch edges must be labelled 'else'");
    Ok(())
}

#[test]
fn dot_has_switch_node_for_match() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // match compiles to a Switch/match node which uses diamond shape.
    // The example script's match has 2 value arms + 1 wildcard default = 3 total.
    assert!(
        dot.contains("match"),
        "DOT must label the match node as 'match', not 'switch'"
    );
    assert!(
        dot.contains("match (3 arms)"),
        "DOT must count all arms including the wildcard default (2 value + 1 default = 3)"
    );
    Ok(())
}

#[test]
fn dot_has_decorator_annotations_on_emitting_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // @voiced is on Dialogue nodes → appears in the node label
    assert!(
        dot.contains("@voiced"),
        "DOT must show @voiced decorator on dialogue nodes"
    );
    // @important is on the Menu/Choice node → appears in the node label
    assert!(
        dot.contains("@important"),
        "DOT must show @important decorator on the choice node"
    );
    Ok(())
}

#[test]
fn dot_has_jump_nodes_with_dashed_edges() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(
        dot.contains("jump"),
        "DOT must contain at least one jump node"
    );
    assert!(dot.contains("rarrow"), "jump nodes must use rarrow shape");
    assert!(dot.contains("style=dashed"), "jump edges must be dashed");
    // compound=true/lhead are disabled so splines=ortho works everywhere.
    assert!(
        !dot.contains("lhead="),
        "jump edges must NOT use lhead= (compound=true is disabled for splines=ortho)"
    );
    assert!(
        dot.contains("constraint=false"),
        "jump edges must carry constraint=false to avoid distorting layout rank"
    );
    Ok(())
}

#[test]
fn dot_has_assign_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // global reputation = 0  →  Assign node labelled "global reputation = ⟨expr⟩"
    assert!(
        dot.contains("global reputation"),
        "DOT must contain an Assign node for 'global reputation'"
    );
    // const max_health = 100
    assert!(
        dot.contains("const max_health"),
        "DOT must contain an Assign node for 'const max_health'"
    );
    // let cost = 2 (inside menu option)
    assert!(
        dot.contains("let cost"),
        "DOT must contain an Assign node for 'let cost'"
    );
    Ok(())
}

#[test]
fn dot_has_define_enum_node() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(dot.contains("enum"), "DOT must contain a DefineEnum node");
    assert!(
        dot.contains("lavender"),
        "DefineEnum node must use lavender fill"
    );
    Ok(())
}

#[test]
fn dot_has_define_script_decorator_node() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // Preamble definitions (globals, consts, enums, decorators) are collapsed
    // into a single __preamble__ summary node.
    assert!(
        dot.contains("__preamble__"),
        "DOT must contain a __preamble__ summary node for top-level definitions"
    );
    assert!(
        dot.contains("decorator timed"),
        "DOT preamble must mention the script-defined `timed` decorator"
    );
    Ok(())
}

#[test]
fn dot_has_decorator_and_label_block() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // The dialogue node that carries @timed should mention @timed in its label.
    assert!(
        dot.contains("@timed"),
        "DOT must show @timed annotation on the decorated dialogue node"
    );
    // notice_board is a known label in the script — it must be present as a
    // cluster subgraph.
    assert!(
        dot.contains("subgraph cluster_notice_board"),
        "notice_board label must compile as a real label block"
    );
    Ok(())
}

#[test]
fn dot_has_end_sentinel() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    assert!(
        dot.contains("__end__"),
        "DOT must contain the __end__ sentinel for NODE_END edges"
    );
    assert!(
        dot.contains("tomato"),
        "__end__ sentinel must use tomato fill"
    );
    Ok(())
}

#[test]
fn dot_has_return_node() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // The "Leave the tavern" option contains a return statement
    assert!(dot.contains("return"), "DOT must contain a Return node");
    assert!(
        dot.contains("#e8c4c4"),
        "Return node must use muted rose fill"
    );
    Ok(())
}

#[test]
fn dot_has_let_call_nodes() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // `let rank = jump compute_rank and return` compiles to an IrNodeKind::LetCall
    // node whose label in the DOT graph reads "⤑ compute_rank\n→ rank".
    assert!(
        dot.contains("⤑ "),
        "DOT must contain at least one LetCall node with a '⤑ ' label prefix"
    );
    assert!(
        dot.contains("→ rank"),
        "DOT must show the bound variable name 'rank' in the LetCall node label"
    );
    // Both the binding and the discard-result form must reference their callees.
    assert!(
        dot.contains("compute_rank"),
        "DOT must reference the compute_rank subroutine in a LetCall node"
    );
    assert!(
        dot.contains("log_visit"),
        "DOT must reference the log_visit subroutine (discard-result call)"
    );
    Ok(())
}

#[test]
fn dot_has_subroutine_call_edges() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // LetCall emits two directed edges per call site:
    //   • a dashed 'call' edge  → the callee's EnterScope
    //   • a solid  'ret'  edge  → the continuation node after the call returns
    assert!(
        dot.contains(r#"label="call""#),
        "DOT must contain a 'call' labelled edge from each LetCall node to its callee"
    );
    assert!(
        dot.contains(r#"label="ret""#),
        "DOT must contain a 'ret' labelled edge from each LetCall node to its continuation"
    );
    Ok(())
}

#[test]
fn dot_subroutine_labels_produce_clusters() -> Result<(), Box<dyn std::error::Error>> {
    let dot = compile_example()?.to_dot();
    // Subroutine labels compile to the same LabeledBlock IR as ordinary labels,
    // so they must also appear as named cluster subgraphs with EnterScope markers.
    for label in ["compute_rank", "log_visit"] {
        assert!(
            dot.contains(&format!("subgraph cluster_{label}")),
            "DOT must contain a cluster subgraph for subroutine label '{label}'"
        );
        assert!(
            dot.contains(&format!("▶ {label}")),
            "DOT must contain an EnterScope (▶) marker node for subroutine label '{label}'"
        );
    }
    Ok(())
}

// ── Graphviz live validation ──────────────────────────────────────────────────

/// Pipes the rendered DOT through `dot -Tsvg` and asserts Graphviz accepts it.
///
/// This proves the output is syntactically valid DOT, not just a string that
/// looks plausible.
#[test]
fn dot_output_is_valid_graphviz() -> Result<(), Box<dyn std::error::Error>> {
    let dot_src = compile_example()?.to_dot();

    let mut child = match Command::new("dot")
        .arg("-Tsvg")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(err) if err.kind() == io::ErrorKind::NotFound => return Ok(()),
        Err(err) => return Err(err.into()),
    };

    let stdin = child
        .stdin
        .as_mut()
        .ok_or_else(|| io::Error::other("stdin must be piped"))?;
    stdin.write_all(dot_src.as_bytes())?;

    let output = child.wait_with_output()?;

    assert!(
        output.status.success(),
        "`dot -Tsvg` rejected the rendered DOT.\n\nSTDERR:\n{}\n\nDOT source:\n{}",
        String::from_utf8_lossy(&output.stderr),
        dot_src,
    );

    let svg = String::from_utf8_lossy(&output.stdout);
    assert!(
        svg.contains("<svg"),
        "dot stdout should contain an <svg> element, got:\n{}",
        &svg[..svg.len().min(500)],
    );
    Ok(())
}

/// Renders and writes `target/dot_render_example.dot` and
/// `target/dot_render_example.svg` for manual inspection in a browser or
/// Graphviz viewer.
///
/// Always passes — purely a developer convenience artifact.
#[test]
fn dot_write_artifacts_for_inspection() -> Result<(), Box<dyn std::error::Error>> {
    let dot_src = compile_example()?.to_dot();
    let out_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("target");
    std::fs::create_dir_all(&out_dir)?;

    let dot_path = out_dir.join("dot_render_example.dot");
    std::fs::write(&dot_path, &dot_src)?;

    // Best-effort SVG render — silently skipped if `dot` is unavailable.
    let svg_path = out_dir.join("dot_render_example.svg");
    if let Ok(mut child) = Command::new("dot")
        .arg("-Tsvg")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        if let Some(stdin) = child.stdin.as_mut() {
            let _ = stdin.write_all(dot_src.as_bytes());
        }
        if let Ok(out) = child.wait_with_output()
            && out.status.success()
        {
            let _ = std::fs::write(&svg_path, &out.stdout);
            println!("SVG artifact : {}", svg_path.display());
        }
    }

    println!("DOT artifact : {}", dot_path.display());
    Ok(())
}

// ── Focused fixture tests ─────────────────────────────────────────────────────

/// Minimal dialogue fixture compiles and renders non-empty DOT output.
#[test]
fn test_dot_minimal_dialogue() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_DIALOGUE)?;
    let dot = graph.to_dot();
    assert!(
        !dot.is_empty(),
        "minimal dialogue fixture must produce DOT output"
    );
    assert!(
        dot.contains("#b8c0cc"),
        "dialogue fixture must contain a dialogue node (muted slate fill)"
    );
    Ok(())
}

/// Minimal branching fixture renders branch edges.
#[test]
fn test_dot_minimal_branching() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_BRANCHING)?;
    let dot = graph.to_dot();
    assert!(
        dot.contains("diamond"),
        "branching fixture must contain a branch node (diamond shape)"
    );
    Ok(())
}

/// Minimal menu fixture renders choice nodes.
#[test]
fn test_dot_minimal_menu() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_MENU)?;
    let dot = graph.to_dot();
    assert!(
        dot.contains("hexagon"),
        "menu fixture must contain a choice node (hexagon shape)"
    );
    Ok(())
}

/// Minimal match fixture renders switch/match node.
#[test]
fn test_dot_minimal_match() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_MATCH)?;
    let dot = graph.to_dot();
    assert!(
        dot.contains("match"),
        "match fixture must contain a match node"
    );
    Ok(())
}

/// Minimal jump fixture renders jump edges.
#[test]
fn test_dot_minimal_jump() -> Result<(), Box<dyn std::error::Error>> {
    let graph = compile_fixture(super::fixtures::FIXTURE_JUMP)?;
    let dot = graph.to_dot();
    assert!(
        dot.contains("jump"),
        "jump fixture must contain a jump node"
    );
    assert!(
        dot.contains("style=dashed"),
        "jump fixture must have dashed edges"
    );
    Ok(())
}
