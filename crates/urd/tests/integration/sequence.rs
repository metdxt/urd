#![allow(missing_docs)]

//! Integration tests for the Mermaid sequence diagram renderer.
//!
//! Uses the same rich example script as `dot_render.rs` to exercise the full
//! range of language features and verify the sequence diagram output.

use urd::{compiler::Compiler, parse_test, parser::block::script};

// ── Example script (mirrors dot_render.rs) ────────────────────────────────────

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
    Narrator: "You push open the heavy oak door and step inside."

    if reputation > 10 {
        Innkeeper: "Ah, a friendly face! Welcome back."
    } elif reputation < 0 {
        Innkeeper: "I'm watching you, stranger."
    } else {
        Innkeeper: "What do you want?"
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
            Narrator: "You step back out into the cold night air."
            return
        }
    }
}

label talk {
    @voiced("innkeeper")
    Innkeeper: {
        "What can I do for you?"
        "Don't keep me waiting."
    }

    menu {
        "Ask about local rumours" {
            reputation = reputation + 1
            Innkeeper: "They say the old mill is haunted now."
            jump start
        }
        "Buy a drink" {
            let cost = 2
            Innkeeper: "Two gold. Pay up."
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
            Narrator: "You tear the notices from the board."
            reputation = reputation - 5
        }
        Action.Talk {
            Narrator: "You study the bounty posters carefully."
            reputation = reputation + 1
        }
        _ {
            Narrator: "You glance at the board and move on."
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
    Narrator: "Your visit has been recorded in the ledger."
    return
}
"#;

// ── Helper ────────────────────────────────────────────────────────────────────

fn compile_example() -> urd::ir::IrGraph {
    let ast =
        parse_test!(script(), EXAMPLE_SCRIPT).expect("example script should parse without errors");
    Compiler::compile(&ast).expect("example script should compile without errors")
}

// ── Tests ─────────────────────────────────────────────────────────────────────

/// The renderer must not panic on the full example script.
#[test]
fn sequence_compiles_without_panic() {
    let _ = compile_example().to_sequence_mermaid();
}

/// Every label in the example script must appear as a `participant` line.
#[test]
fn sequence_has_all_participants() {
    let out = compile_example().to_sequence_mermaid();
    for label in &["start", "talk", "notice_board", "compute_rank", "log_visit"] {
        assert!(
            out.contains(&format!("participant {label}")),
            "participant `{label}` missing from diagram;\ngot:\n{out}"
        );
    }
}

/// `start` must call into `compute_rank` with a solid arrow.
#[test]
fn sequence_shows_compute_rank_call() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("start->>compute_rank"),
        "call arrow start->>compute_rank missing;\ngot:\n{out}"
    );
}

/// `compute_rank` must return to `start` with a dashed arrow.
#[test]
fn sequence_shows_compute_rank_return() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("compute_rank-->>start"),
        "return arrow compute_rank-->>start missing;\ngot:\n{out}"
    );
}

/// `start` must call into `log_visit` with a solid arrow.
#[test]
fn sequence_shows_log_visit_call() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("start->>log_visit"),
        "call arrow start->>log_visit missing;\ngot:\n{out}"
    );
}

/// `log_visit` must return to `start` with a dashed arrow.
#[test]
fn sequence_shows_log_visit_return() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("log_visit-->>start"),
        "return arrow log_visit-->>start missing;\ngot:\n{out}"
    );
}

/// The `let rank = jump compute_rank and return` call must show the variable
/// binding in both the call label and the return label.
#[test]
fn sequence_shows_rank_binding() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("let rank = ⤑"),
        "call label `let rank = ⤑` missing;\ngot:\n{out}"
    );
    assert!(
        out.contains("↩ rank"),
        "return label `↩ rank` missing;\ngot:\n{out}"
    );
}

/// The `menu` node must produce an `alt` / `else` / `end` block.
#[test]
fn sequence_shows_menu_as_alt_block() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("alt "),
        "`alt` keyword missing from diagram;\ngot:\n{out}"
    );
    assert!(
        out.contains("\n    else "),
        "`else` keyword missing from diagram;\ngot:\n{out}"
    );
    assert!(
        out.contains("\n    end"),
        "`end` keyword missing from diagram;\ngot:\n{out}"
    );
}

/// One of the menu options jumps to `talk` — that jump arrow must be present.
#[test]
fn sequence_shows_jump_to_talk() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("start->>talk") || out.contains("->>talk: jump"),
        "jump arrow to `talk` missing;\ngot:\n{out}"
    );
}

/// At least one `Note over` must be present (from Dialogue nodes).
#[test]
fn sequence_shows_dialogue_notes() {
    let out = compile_example().to_sequence_mermaid();
    assert!(
        out.contains("Note over"),
        "no `Note over` annotations found;\ngot:\n{out}"
    );
}

/// Writes `target/sequence_example.mmd` for manual inspection in Mermaid Live
/// (<https://mermaid.live>).  Always passes — purely a developer convenience.
#[test]
fn sequence_write_artifact_for_inspection() {
    let out = compile_example().to_sequence_mermaid();
    let out_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("target");
    std::fs::create_dir_all(&out_dir).expect("failed to create artifact output dir");
    let path = out_dir.join("sequence_example.mmd");
    std::fs::write(&path, &out).expect("failed to write Mermaid artifact");
    println!("Mermaid artifact: {}", path.display());
    println!("\n{out}");
}
