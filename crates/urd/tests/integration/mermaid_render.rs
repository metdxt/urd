#![allow(missing_docs)]
#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

//! Integration tests for the Mermaid flowchart renderer.
//!
//! Uses the same `EXAMPLE_SCRIPT` as `dot_render.rs` to exercise every major
//! language feature, then makes structural assertions on the rendered Mermaid
//! string.  No external tooling is required — Mermaid is pure text.

use urd::{compiler::Compiler, parse_test, parser::block::script};

// ── Example script ─────────────────────────────────────────────────────────

/// The same showcase Urd script used in the DOT integration tests.
///
/// Exercises:
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

// ── Helper ──────────────────────────────────────────────────────────────────

fn compile_example() -> urd::ir::IrGraph {
    let ast =
        parse_test!(script(), EXAMPLE_SCRIPT).expect("example script should parse without errors");
    Compiler::compile(&ast).expect("example script should compile without errors")
}

// ── Parse + compile ──────────────────────────────────────────────────────────

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

// ── Mermaid structural assertions ────────────────────────────────────────────

#[test]
fn mermaid_has_flowchart_header() {
    let mmd = compile_example().to_mermaid();
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
        mmd.contains("flowchart TD"),
        "Mermaid output must contain 'flowchart TD', got:\n{}",
        &mmd[..mmd.len().min(120)]
    );
    // init directive must come before the flowchart declaration
    let init_pos = mmd.find("%%{init:").unwrap();
    let flowchart_pos = mmd.find("flowchart TD").unwrap();
    assert!(
        init_pos < flowchart_pos,
        "%%{{init:}} directive must appear before 'flowchart TD'"
    );
}

#[test]
fn mermaid_has_classdefs() {
    let mmd = compile_example().to_mermaid();
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
}

#[test]
fn mermaid_has_start_node() {
    let mmd = compile_example().to_mermaid();
    assert!(
        mmd.contains("__start__"),
        "Mermaid output must contain the __start__ synthetic entry node"
    );
    assert!(
        mmd.contains("▶ START"),
        "Mermaid output must contain the ▶ START label on the start node"
    );
}

#[test]
fn mermaid_has_subgraph_for_every_label() {
    let mmd = compile_example().to_mermaid();
    for label in ["start", "talk", "notice_board", "compute_rank", "log_visit"] {
        assert!(
            mmd.contains(&format!("subgraph cluster_{label}")),
            "Mermaid output must contain a subgraph for label '{label}'"
        );
    }
}

#[test]
fn mermaid_has_enter_scope_markers() {
    let mmd = compile_example().to_mermaid();
    for label in ["start", "talk", "notice_board", "compute_rank", "log_visit"] {
        assert!(
            mmd.contains(&format!("▶ {label}")),
            "Mermaid output must contain an EnterScope (▶) marker for label '{label}'"
        );
    }
}

#[test]
fn mermaid_has_dialogue_nodes() {
    let mmd = compile_example().to_mermaid();
    assert!(
        mmd.contains(":::dialogue"),
        "Mermaid output must contain at least one dialogue node (:::dialogue class)"
    );
}

#[test]
fn mermaid_has_choice_nodes() {
    let mmd = compile_example().to_mermaid();
    assert!(
        mmd.contains(":::choice"),
        "Mermaid output must contain at least one choice node (:::choice class)"
    );
}

#[test]
fn mermaid_has_branch_nodes() {
    let mmd = compile_example().to_mermaid();
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
}

#[test]
fn mermaid_has_jump_nodes() {
    let mmd = compile_example().to_mermaid();
    assert!(
        mmd.contains(":::jump"),
        "Mermaid output must contain at least one jump node (:::jump class)"
    );
    assert!(
        mmd.contains("-.->"),
        "jump edges must be dashed (-.->)"
    );
}

#[test]
fn mermaid_has_let_call_nodes() {
    let mmd = compile_example().to_mermaid();
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
}

#[test]
fn mermaid_has_subroutine_return_edges() {
    let mmd = compile_example().to_mermaid();
    // Subroutine Returns must emit dashed ↩ back-edges to the caller continuation,
    // not a bare edge to __end__.
    assert!(
        mmd.contains("↩"),
        "Mermaid output must contain ↩ back-edges for subroutine Return nodes \
         (compute_rank and log_visit both return to the start cluster)"
    );
}

#[test]
fn mermaid_has_end_sink() {
    let mmd = compile_example().to_mermaid();
    assert!(
        mmd.contains("__end__"),
        "Mermaid output must contain the __end__ sink node for terminal edges"
    );
    assert!(
        mmd.contains(":::endNode"),
        "__end__ sink must carry the :::endNode class"
    );
}

#[test]
fn mermaid_has_assign_nodes() {
    let mmd = compile_example().to_mermaid();
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
}

#[test]
fn mermaid_has_define_enum_node() {
    let mmd = compile_example().to_mermaid();
    assert!(
        mmd.contains("enum Action"),
        "Mermaid output must contain a DefineEnum node for 'enum Action'"
    );
    assert!(
        mmd.contains(":::enumDef"),
        "DefineEnum node must carry the :::enumDef class"
    );
}

#[test]
fn mermaid_has_return_node() {
    let mmd = compile_example().to_mermaid();
    // The script has several Return nodes (in compute_rank, log_visit, and the
    // "Leave the tavern" menu option).
    assert!(
        mmd.contains(":::ret"),
        "Mermaid output must contain at least one Return node (:::ret class)"
    );
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
fn mermaid_nodes_after_letcall_in_cluster() {
    let mmd = compile_example().to_mermaid();

    // Find the subgraph cluster_start block.
    let start_pos = mmd
        .find("subgraph cluster_start")
        .expect("subgraph cluster_start must exist in Mermaid output");

    // Find the "end" line that closes this subgraph (first `\n    end` after open).
    let end_rel = mmd[start_pos..]
        .find("\n    end")
        .expect("subgraph cluster_start must have a closing 'end'");
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
}

#[test]
fn mermaid_is_non_empty() {
    assert!(
        !compile_example().to_mermaid().is_empty(),
        "Mermaid output must not be empty"
    );
}

/// Writes `target/mermaid_example.mmd` for manual inspection.
///
/// Always passes — purely a developer convenience artifact.
/// Paste the file contents into <https://mermaid.live> to view the diagram.
#[test]
fn mermaid_write_artifact_for_inspection() {
    let mmd = compile_example().to_mermaid();
    let out_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("target");
    let path = out_dir.join("mermaid_example.mmd");
    std::fs::write(&path, &mmd).expect("failed to write Mermaid artifact");
    println!("Mermaid artifact: {}", path.display());
}
