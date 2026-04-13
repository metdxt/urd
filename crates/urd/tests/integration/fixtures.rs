//! Shared test fixtures for integration tests.
//!
//! Centralises example Urd scripts so render (and other) tests can
//! import them from one place.  Changes to language syntax only need
//! to be applied once.
//!
// TODO: extract a shared `run_script(src, step_cap) -> Vec<VmStep>` helper
// into this module.  The same function is copy-pasted across adversarial.rs,
// adversarial_new.rs, adversarial_round2.rs, builtin_methods.rs,
// in_operator_and_functions.rs, string_format_and_extern.rs, and
// vm_decorator.rs — each with a different step cap (64 / 128 / 256 / 1024).
// A single parametric version here would eliminate the duplication.

/// Full showcase Urd script exercising every major language feature.
/// Used as a smoke-test input for both DOT and Mermaid renderers.
///
/// Exercises:
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
pub const EXAMPLE_SCRIPT: &str = r#"
global reputation = 0
const max_health = 100

enum Action {
    Attack
    Flee
    Talk
}

decorator timed(duration: float, fallback: str) {
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

/// Minimal fixture: dialogue with a decorator.
pub const FIXTURE_DIALOGUE: &str = r#"
@entry
label scene {
    @voiced("narrator")
    Narrator: "Welcome to the world."
}
"#;

/// Minimal fixture: if/elif/else branching.
pub const FIXTURE_BRANCHING: &str = r#"
global score = 0

@entry
label test {
    if score > 10 {
        Alice: "High score!"
    } elif score > 0 {
        Alice: "Some points."
    } else {
        Alice: "Nothing yet."
    }
}
"#;

/// Minimal fixture: menu with choices.
pub const FIXTURE_MENU: &str = r#"
@entry
label test {
    menu {
        "Option A" {
            Alice: "Chose A"
        }
        "Option B" {
            Alice: "Chose B"
        }
    }
}
"#;

/// Minimal fixture: match with patterns.
pub const FIXTURE_MATCH: &str = r#"
enum Color {
    Red
    Blue
}

@entry
label test {
    let c = Color.Red
    match c {
        Color.Red { Alice: "Red!" }
        Color.Blue { Alice: "Blue!" }
    }
}
"#;

/// Minimal fixture: jump and subroutine calls.
pub const FIXTURE_JUMP: &str = r#"
@entry
label main {
    jump helper
}

label helper {
    Alice: "Jumped here."
}
"#;
