#![allow(missing_docs)]

//! # quest — interactive Urd script runner
//!
//! Loads a `.urd` file from the path given as the first CLI argument (or
//! `examples/quest/cave.urd` relative to the current directory as a fallback),
//! compiles it, and runs the resulting VM in an interactive terminal loop.

use std::io::{self, BufRead, IsTerminal, Write};
use std::path::{Path, PathBuf};

use crossterm::{
    cursor,
    event::{self, Event as CtEvent, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{self, ClearType},
};

use urd::analysis;
use urd::compiler::Compiler;
use urd::ir::Event;
use urd::parse_test;
use urd::parser::block::script;
use urd::runtime::value::RuntimeValue;
use urd::vm::loader::FsLoader;
use urd::vm::{DecoratorRegistry, Vm};

// ─── ANSI color helpers ───────────────────────────────────────────────────────

const RESET: &str = "\x1b[0m";
const DIM: &str = "\x1b[2m";

/// Map a color name string to an ANSI foreground escape code.
fn ansi_color(name: &str) -> &'static str {
    match name {
        "white"         => "\x1b[97m",
        "cyan"          => "\x1b[96m",
        "yellow"        => "\x1b[93m",
        "green"         => "\x1b[92m",
        "red"           => "\x1b[91m",
        "blue"          => "\x1b[94m",
        "magenta"       => "\x1b[95m",
        "gray" | "grey" => "\x1b[90m",
        _               => "\x1b[0m",
    }
}

// ─── Character display ────────────────────────────────────────────────────────

/// A speaker resolved to a printable name + ANSI color.
struct CharacterDisplay {
    name:  String,
    color: &'static str,
}

/// Extract display info from a speaker [`RuntimeValue`].
///
/// If the value is a `Map` with `name` / `name_color` keys those are used;
/// otherwise the value is formatted with [`display_value`] and color is reset.
fn extract_character(val: &RuntimeValue) -> CharacterDisplay {
    if let RuntimeValue::Map(map) = val {
        let name = map
            .get("name")
            .map(|v| display_value(v))
            .unwrap_or_else(|| "???".to_string());
        let color = map
            .get("name_color")
            .map(|v| ansi_color(&display_value(v)))
            .unwrap_or(RESET);
        CharacterDisplay { name, color }
    } else {
        CharacterDisplay {
            name:  display_value(val),
            color: RESET,
        }
    }
}

/// Build a [`CharacterDisplay`] from a list of speaker values.
///
/// Returns `None` when the list is empty (pure narration with no speaker label).
fn format_speakers(speakers: &[RuntimeValue]) -> Option<CharacterDisplay> {
    match speakers {
        [] => None,
        [single] => Some(extract_character(single)),
        multiple => {
            let first = extract_character(&multiple[0]);
            let name = multiple
                .iter()
                .map(|v| extract_character(v).name)
                .collect::<Vec<_>>()
                .join(" & ");
            Some(CharacterDisplay { name, color: first.color })
        }
    }
}

// ─── Value rendering ──────────────────────────────────────────────────────────

/// Convert a [`RuntimeValue`] to a human-readable display string.
fn display_value(val: &RuntimeValue) -> String {
    match val {
        RuntimeValue::Str(s)             => s.to_string(),
        RuntimeValue::IdentPath(parts)   => parts.join("."),
        RuntimeValue::Int(n)             => n.to_string(),
        RuntimeValue::Bool(b)            => b.to_string(),
        RuntimeValue::Float(f)           => f.to_string(),
        RuntimeValue::Null               => "null".to_string(),
        RuntimeValue::Dice(count, sides) => format!("{}d{}", count, sides),
        RuntimeValue::Label { name, .. } => name.clone(),
        other                            => format!("{:?}", other),
    }
}

// ─── Terminal utilities ───────────────────────────────────────────────────────

/// Print a separator line.
fn print_separator() {
    println!("{DIM}{}{RESET}", "─".repeat(60));
}

/// Flush stdout so prompts appear before blocking reads.
fn flush_stdout() {
    let _ = io::stdout().flush();
}

/// Read one line from `stdin`, trimming the trailing newline.
/// Returns `None` on EOF or I/O error.
fn read_line(stdin: &mut impl BufRead) -> Option<String> {
    let mut buf = String::new();
    match stdin.read_line(&mut buf) {
        Ok(0) | Err(_) => None,
        Ok(_) => Some(buf.trim_end_matches(['\n', '\r']).to_string()),
    }
}

/// Block until the player presses Enter.
///
/// When `is_tty` is `false` (piped / non-interactive input) the prompt is
/// skipped entirely so that choice numbers are not interleaved with dialogue
/// dismissal reads.
fn wait_for_enter(stdin: &mut impl BufRead, is_tty: bool) {
    if is_tty {
        print!("  {DIM}[press Enter to continue]{RESET} ");
        flush_stdout();
        let _ = read_line(stdin);
    }
}

// ─── Core I/O loops ───────────────────────────────────────────────────────────

/// Display a `Dialogue` event and wait for Enter (only when running on a TTY).
fn handle_dialogue(
    speakers: &[RuntimeValue],
    lines: &[RuntimeValue],
    stdin: &mut impl BufRead,
    is_tty: bool,
) {
    println!();

    if let Some(ch) = format_speakers(speakers) {
        println!("  {}{}{RESET}:", ch.color, ch.name);
    }

    for line_val in lines {
        println!("    {}", display_value(line_val));
    }

    wait_for_enter(stdin, is_tty);
}

// ─── Interactive choice menu (TTY) ────────────────────────────────────────────

/// Render all options with `selected` highlighted using a bold-yellow arrow.
/// Each line is cleared before printing to prevent stale text from bleeding
/// through when option labels differ in length across redraws.
fn render_menu(options: &[urd::ir::ChoiceEvent], selected: usize) {
    let mut out = std::io::stdout();
    for (i, opt) in options.iter().enumerate() {
        // Clear the current line and move to column 0 before writing, so no
        // leftover characters from a previously longer label remain visible.
        execute!(
            out,
            cursor::MoveToColumn(0),
            terminal::Clear(ClearType::CurrentLine)
        )
        .ok();
        if i == selected {
            println!("  \x1b[1;93m▶  {}\x1b[0m", opt.label);
        } else {
            println!("   \x1b[2m  {}\x1b[0m", opt.label);
        }
    }
}

/// Full arrow-key interactive menu. Returns the 0-based index of the confirmed
/// selection. Designed for use only when stdin is a real TTY.
fn handle_choice_tty(options: &[urd::ir::ChoiceEvent]) -> usize {
    let n = options.len();
    let mut selected = 0usize;

    println!();
    println!("  \x1b[93mWhat do you do?\x1b[0m  \x1b[2m(↑↓ navigate, Enter confirm)\x1b[0m");
    println!();
    render_menu(options, selected);

    // Enter raw mode AFTER printing the initial state so the header isn't
    // swallowed before the user sees it.
    terminal::enable_raw_mode().ok();

    loop {
        match event::read() {
            Ok(CtEvent::Key(KeyEvent { code, modifiers, .. })) => {
                // Ctrl-C / Ctrl-D → exit cleanly
                if matches!(code, KeyCode::Char('c'))
                    && modifiers.contains(KeyModifiers::CONTROL)
                {
                    terminal::disable_raw_mode().ok();
                    std::process::exit(0);
                }
                if matches!(code, KeyCode::Char('d'))
                    && modifiers.contains(KeyModifiers::CONTROL)
                {
                    terminal::disable_raw_mode().ok();
                    std::process::exit(0);
                }

                match code {
                    KeyCode::Up | KeyCode::Char('k') => {
                        selected = if selected == 0 { n - 1 } else { selected - 1 };
                    }
                    KeyCode::Down | KeyCode::Char('j') => {
                        selected = (selected + 1) % n;
                    }
                    KeyCode::Enter | KeyCode::Char(' ') => {
                        break;
                    }
                    _ => {}
                }
            }
            Err(_) => break,
            _ => {}
        }

        // Move cursor back to the first option line, clear everything below,
        // then redraw so no stale characters remain.
        execute!(
            std::io::stdout(),
            cursor::MoveUp(n as u16),
            terminal::Clear(ClearType::FromCursorDown)
        )
        .ok();
        render_menu(options, selected);
    }

    terminal::disable_raw_mode().ok();

    // Redraw in "confirmed" style: tick on selected, dim on the rest.
    execute!(
        std::io::stdout(),
        cursor::MoveUp(n as u16),
        terminal::Clear(ClearType::FromCursorDown)
    )
    .ok();
    let mut out = std::io::stdout();
    for (i, opt) in options.iter().enumerate() {
        execute!(
            out,
            cursor::MoveToColumn(0),
            terminal::Clear(ClearType::CurrentLine)
        )
        .ok();
        if i == selected {
            println!("  \x1b[92m✓  {}\x1b[0m", opt.label);
        } else {
            println!("     \x1b[2m{}\x1b[0m", opt.label);
        }
    }

    selected
}

// ─── Plain-text choice fallback (non-TTY / pipe) ─────────────────────────────

/// Number-based choice prompt used when stdin is not a terminal (e.g. piped
/// input in tests or scripted runs).
fn handle_choice_pipe(
    options: &[urd::ir::ChoiceEvent],
    stdin: &mut impl BufRead,
) -> usize {
    let n = options.len();
    println!();
    println!("  What do you do?");
    for (i, opt) in options.iter().enumerate() {
        println!("  {}. {}", i + 1, opt.label);
    }

    let input = match read_line(stdin) {
        Some(l) => l,
        None => {
            eprintln!("Unexpected EOF waiting for choice.");
            std::process::exit(1);
        }
    };

    match input.trim().parse::<usize>() {
        Ok(i) if i >= 1 && i <= n => i - 1,
        _ => {
            eprintln!(
                "Invalid choice '{}', expected 1\u{2013}{}",
                input.trim(),
                n
            );
            std::process::exit(1);
        }
    }
}

// ─── Setup ────────────────────────────────────────────────────────────────────

/// Resolve the script path: first CLI arg, or the built-in demo.
fn resolve_script_path() -> PathBuf {
    let mut args = std::env::args();
    args.next(); // skip argv[0]
    args.next()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("examples/quest/cave.urd"))
}

/// Run all static-analysis passes over `ast` and print rich ariadne diagnostics
/// to stderr.  Analysis is non-fatal: issues are reported but execution continues.
fn run_analysis(ast: &urd::parser::ast::Ast, src: &str, filename: &str) {
    let errors = analysis::analyze(ast);
    if !errors.is_empty() {
        analysis::render_errors_stderr(&errors, src, filename);
        eprintln!("[analysis] {} issue(s) found in '{}'", errors.len(), filename);
    }
}

/// Load, parse, compile the script at `path` and return a ready [`Vm`].
fn build_vm(path: &Path) -> Result<Vm, String> {
    let src = std::fs::read_to_string(path)
        .map_err(|e| format!("Could not read '{}': {}", path.display(), e))?;

    let display_name = path.display().to_string();

    let ast = parse_test!(script(), src.as_str())
        .map_err(|errs| format!("Parse errors in '{}':\n{:?}", path.display(), errs))?;

    run_analysis(&ast, &src, &display_name);

    let parent = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let loader = FsLoader::new(parent);

    let graph = Compiler::compile_with_loader(&ast, &loader)
        .map_err(|e| format!("Compile error in '{}':\n{:?}", path.display(), e))?;

    let registry = DecoratorRegistry::new();
    Vm::new(graph, registry).map_err(|e| format!("VM initialisation error:\n{:?}", e))
}

// ─── Entry point ──────────────────────────────────────────────────────────────

fn main() {
    env_logger::init();

    let is_tty = io::stdin().is_terminal();
    let script_path = resolve_script_path();

    print_separator();
    println!("  \x1b[1mUrd Quest Runner\x1b[0m");
    println!("  Loading: {}", script_path.display());
    print_separator();

    let mut vm = match build_vm(&script_path) {
        Ok(vm) => vm,
        Err(msg) => {
            eprintln!("\n\x1b[91mError:\x1b[0m {}\n", msg);
            std::process::exit(1);
        }
    };

    let stdin = io::stdin();
    let mut stdin_lock = stdin.lock();

    let mut choice: Option<usize> = None;

    loop {
        match vm.next(choice) {
            None => {
                println!();
                print_separator();
                println!("  \x1b[1m--- The End ---\x1b[0m");
                print_separator();
                break;
            }
            Some(Err(e)) => {
                eprintln!("\n\x1b[91mRuntime error:\x1b[0m {:?}\n", e);
                std::process::exit(1);
            }
            Some(Ok(Event::Dialogue { speakers, lines, .. })) => {
                handle_dialogue(&speakers, &lines, &mut stdin_lock, is_tty);
                choice = None;
            }
            Some(Ok(Event::Choice { options, .. })) => {
                let idx = if is_tty {
                    handle_choice_tty(&options)
                } else {
                    handle_choice_pipe(&options, &mut stdin_lock)
                };
                choice = Some(idx);
            }
        }
    }
}
