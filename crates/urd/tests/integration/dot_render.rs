#![allow(missing_docs)]
#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

//! Integration tests for the DOT graph renderer.
//!
//! Includes a rich example Urd script that exercises every major language
//! feature, structural assertions on the rendered DOT string, and a live
//! Graphviz validation test that pipes the output through `dot -Tsvg`.

use std::io::Write as _;
use std::process::{Command, Stdio};

use urd::{compiler::Compiler, parse_test, parser::block::script};

// ── Example script ────────────────────────────────────────────────────────────

/// A showcase Urd script that exercises the full range of language features:
///
/// - `global` and `const` declarations
/// - `enum` declaration
/// - `label` blocks with decorators (`@scene`)
/// - `dialogue` lines with decorators (`@voiced`) — single-line and multi-line
/// - `if` / `elif` / `else` branching
/// - `menu` with three options and a decorator (`@important`)
/// - `match` with value patterns and a wildcard arm
/// - `let` declarations and `=` assignments
/// - `jump` between labels (fire-and-forget)
/// - `jump … and return` — subroutine call, discard result
/// - `let x = jump … and return` — subroutine call, capture return value
/// - `return` to exit early or yield a value back to the caller
const EXAMPLE_SCRIPT: &str = r#"
global reputation = 0
const max_health = 100

enum Action {
    Attack
    Flee
    Talk
}

decorator timed(duration: float, fallback: label) {
    event["timed"] = :{duration: duration, next: fallback}
}

@scene("tavern")
label start {
    @voiced("narrator")
    @timed(3.0, notice_board)
    <Narrator>: "You push open the heavy oak door and step inside."

    if reputation > 10 {
        <Innkeeper>: "Ah, a friendly face! Welcome back."
    } elif reputation < 0 {
        <Innkeeper>: "I'm watching you, stranger."
    } else {
        <Innkeeper>: "What do you want?"
    }

    let rank = jump compute_rank and return
    jump log_visit and return

    @important
    menu {
        "Talk to the innkeeper" {
            jump talk
        }
        "Check the notice board" {
            jump notice_board
        }
        "Leave the tavern" {
            @voiced("narrator")
            <Narrator>: "You step back out into the cold night air."
            return
        }
    }
}

label talk {
    @voiced("innkeeper")
    <Innkeeper>: {
        "What can I do for you?"
        "Don't keep me waiting."
    }

    menu {
        "Ask about local rumours" {
            reputation = reputation + 1
            <Innkeeper>: "They say the old mill is haunted now."
            jump start
        }
        "Buy a drink" {
            let cost = 2
            <Innkeeper>: "Two gold. Pay up."
            jump start
        }
        "Never mind" {
            jump start
        }
    }
}

label notice_board {
    let action = Action.Talk

    match action {
        Action.Attack {
            <Narrator>: "You tear the notices from the board."
            reputation = reputation - 5
        }
        Action.Talk {
            <Narrator>: "You study the bounty posters carefully."
            reputation = reputation + 1
        }
        _ {
            <Narrator>: "You glance at the board and move on."
        }
    }

    jump start
}

label compute_rank {
    if reputation > 10 {
        return "hero"
    }
    if reputation < 0 {
        return "villain"
    }
    return "neutral"
}

label log_visit {
    <Narrator>: "Your visit has been recorded in the ledger."
    return
}
"#;

// ── Helper ────────────────────────────────────────────────────────────────────

fn compile_example() -> urd::ir::IrGraph {
    let ast =
        parse_test!(script(), EXAMPLE_SCRIPT).expect("example script should parse without errors");
    Compiler::compile(&ast).expect("example script should compile without errors")
}

// ── Parse + compile ───────────────────────────────────────────────────────────

#[test]
fn example_script_parses() {
    parse_test!(script(), EXAMPLE_SCRIPT).expect("example script must parse cleanly");
}

#[test]
fn example_script_compiles() {
    compile_example();
}

#[test]
fn compiled_graph_has_expected_label_count() {
    let graph = compile_example();
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
}

// ── DOT structural assertions (no Graphviz required) ─────────────────────────

#[test]
fn dot_is_non_empty() {
    assert!(!compile_example().to_dot().is_empty());
}

#[test]
fn dot_has_digraph_wrapper() {
    let dot = compile_example().to_dot();
    assert!(
        dot.starts_with("digraph urd_script {"),
        "DOT must open with digraph header"
    );
    assert!(dot.ends_with("}\n"), "DOT must close with }}");
    assert!(
        dot.contains("splines=ortho"),
        "DOT must set splines=ortho for right-angle edge routing"
    );
}

#[test]
fn dot_has_entry_arrow() {
    let dot = compile_example().to_dot();
    assert!(
        dot.contains("__start__"),
        "DOT must contain an entry-point arrow"
    );
}

#[test]
fn dot_has_scope_nodes_for_every_label() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_dialogue_nodes() {
    let dot = compile_example().to_dot();
    assert!(
        dot.contains("#b8c0cc"),
        "DOT must contain at least one dialogue node (muted slate fill)"
    );
}

#[test]
fn dot_has_choice_nodes() {
    let dot = compile_example().to_dot();
    assert!(
        dot.contains("hexagon"),
        "DOT must contain at least one choice node (hexagon shape)"
    );
    // Choice edges must be purple
    assert!(
        dot.contains("color=purple"),
        "choice option edges must be purple"
    );
}

#[test]
fn dot_choice_option_labels_are_present() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_branch_nodes_for_if_elif_else() {
    let dot = compile_example().to_dot();
    assert!(
        dot.contains("diamond"),
        "DOT must contain branch/switch nodes (diamond shape)"
    );
    assert!(dot.contains("then"), "branch edges must be labelled 'then'");
    assert!(dot.contains("else"), "branch edges must be labelled 'else'");
}

#[test]
fn dot_has_switch_node_for_match() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_decorator_annotations_on_emitting_nodes() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_jump_nodes_with_dashed_edges() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_assign_nodes() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_define_enum_node() {
    let dot = compile_example().to_dot();
    assert!(dot.contains("enum"), "DOT must contain a DefineEnum node");
    assert!(
        dot.contains("lavender"),
        "DefineEnum node must use lavender fill"
    );
}

#[test]
fn dot_has_define_script_decorator_node() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_label_value_in_decorator_node() {
    let dot = compile_example().to_dot();
    // @timed(3.0, notice_board) — the second argument resolves to
    // RuntimeValue::Label("notice_board") and is stored in the event map.
    // The dialogue node that carries @timed should mention @timed in its label.
    assert!(
        dot.contains("@timed"),
        "DOT must show @timed annotation on the decorated dialogue node"
    );
    // notice_board is a known label in the script — it must be present as a
    // cluster subgraph (verifying the label itself compiled correctly alongside
    // being used as a first-class value in the decorator argument).
    assert!(
        dot.contains("subgraph cluster_notice_board"),
        "notice_board label (used as a value in @timed) must still compile as a real label block"
    );
}

#[test]
fn dot_has_end_sentinel() {
    let dot = compile_example().to_dot();
    assert!(
        dot.contains("__end__"),
        "DOT must contain the __end__ sentinel for NODE_END edges"
    );
    assert!(
        dot.contains("tomato"),
        "__end__ sentinel must use tomato fill"
    );
}

#[test]
fn dot_has_return_node() {
    let dot = compile_example().to_dot();
    // The "Leave the tavern" option contains a return statement
    assert!(dot.contains("return"), "DOT must contain a Return node");
    assert!(
        dot.contains("#e8c4c4"),
        "Return node must use muted rose fill"
    );
}

#[test]
fn dot_has_let_call_nodes() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_has_subroutine_call_edges() {
    let dot = compile_example().to_dot();
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
}

#[test]
fn dot_subroutine_labels_produce_clusters() {
    let dot = compile_example().to_dot();
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
}

// ── Graphviz live validation ──────────────────────────────────────────────────

/// Pipes the rendered DOT through `dot -Tsvg` and asserts Graphviz accepts it.
///
/// This proves the output is syntactically valid DOT, not just a string that
/// looks plausible.
#[test]
fn dot_output_is_valid_graphviz() {
    let dot_src = compile_example().to_dot();

    let mut child = Command::new("dot")
        .arg("-Tsvg")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn `dot` — is Graphviz installed?");

    child
        .stdin
        .as_mut()
        .expect("stdin must be piped")
        .write_all(dot_src.as_bytes())
        .expect("failed to write DOT to stdin");

    let output = child
        .wait_with_output()
        .expect("failed to wait for dot process");

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
}

/// Renders and writes `target/dot_render_example.dot` and
/// `target/dot_render_example.svg` for manual inspection in a browser or
/// Graphviz viewer.
///
/// Always passes — purely a developer convenience artifact.
#[test]
fn dot_write_artifacts_for_inspection() {
    let dot_src = compile_example().to_dot();
    let out_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("target");
    std::fs::create_dir_all(&out_dir).expect("failed to create artifact output dir");

    let dot_path = out_dir.join("dot_render_example.dot");
    std::fs::write(&dot_path, &dot_src).expect("failed to write DOT artifact");

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
}
