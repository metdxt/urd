#![allow(missing_docs)]

//! # quest — interactive Urd script runner & graph exporter
//!
//! ## Subcommands
//!
//! - **`run`** (default) — loads a `.urd` file and runs it interactively.
//! - **`export`** — compiles a `.urd` file and exports its IR graph as
//!   Graphviz DOT, Mermaid flowchart, or Mermaid sequence diagram.

use std::io::{self, BufRead, IsTerminal, Write};
use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand, ValueEnum};
use crossterm::{
    cursor,
    event::{self, Event as CtEvent, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{self, ClearType},
};

use urd::analysis;
use urd::compiler::Compiler;
use urd::ir::{Event, IrGraph};
use urd::parser::block::script;
use urd::parser::errors::render_parse_errors_stderr;
use urd::runtime::value::RuntimeValue;
use urd::vm::loader::FsLoader;
use urd::vm::{DecoratorRegistry, Vm};

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  CLI
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Interactive terminal runner and tooling for Urd dialogue scripts.
#[derive(Parser, Debug)]
#[command(name = "quest", version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Run an Urd script interactively in the terminal (default).
    Run {
        /// Path to the `.urd` script file.
        #[arg(default_value = "examples/quest/cave.urd")]
        script: PathBuf,
    },

    /// Export the compiled IR graph in a visual format.
    Export {
        /// Path to the `.urd` script file.
        script: PathBuf,

        /// Output format.
        #[arg(short, long, default_value = "mermaid")]
        format: ExportFormat,

        /// Write output to a file instead of stdout.
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

/// Supported graph export formats.
#[derive(Debug, Clone, Copy, ValueEnum)]
enum ExportFormat {
    /// Graphviz DOT
    Dot,
    /// Mermaid flowchart
    Mermaid,
    /// Mermaid sequence diagram
    Sequence,
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  ANSI color helpers
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

const RESET: &str = "\x1b[0m";
const DIM: &str = "\x1b[2m";

/// Map a color name string to an ANSI foreground escape code.
fn ansi_color(name: &str) -> &'static str {
    match name {
        "white" => "\x1b[97m",
        "cyan" => "\x1b[96m",
        "yellow" => "\x1b[93m",
        "green" => "\x1b[92m",
        "red" => "\x1b[91m",
        "blue" => "\x1b[94m",
        "magenta" => "\x1b[95m",
        "gray" | "grey" => "\x1b[90m",
        _ => "\x1b[0m",
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Character display
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// A speaker resolved to a printable name + ANSI color.
struct CharacterDisplay {
    name: String,
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
            name: display_value(val),
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
            Some(CharacterDisplay {
                name,
                color: first.color,
            })
        }
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Value rendering
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Convert a [`RuntimeValue`] to a human-readable display string.
fn display_value(val: &RuntimeValue) -> String {
    match val {
        RuntimeValue::Str(s) => s.to_string(),
        RuntimeValue::IdentPath(parts) => parts.join("."),
        RuntimeValue::Int(n) => n.to_string(),
        RuntimeValue::Bool(b) => b.to_string(),
        RuntimeValue::Float(f) => f.to_string(),
        RuntimeValue::Null => "null".to_string(),
        RuntimeValue::Dice(count, sides) => format!("{}d{}", count, sides),
        RuntimeValue::Label { name, .. } => name.clone(),
        other => format!("{:?}", other),
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Terminal utilities
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Core I/O loops
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Interactive choice menu (TTY)
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
            Ok(CtEvent::Key(KeyEvent {
                code, modifiers, ..
            })) => {
                // Ctrl-C / Ctrl-D → exit cleanly
                if matches!(code, KeyCode::Char('c')) && modifiers.contains(KeyModifiers::CONTROL) {
                    terminal::disable_raw_mode().ok();
                    std::process::exit(0);
                }
                if matches!(code, KeyCode::Char('d')) && modifiers.contains(KeyModifiers::CONTROL) {
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

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Plain-text choice fallback (non-TTY / pipe)
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Number-based choice prompt used when stdin is not a terminal (e.g. piped
/// input in tests or scripted runs).
fn handle_choice_pipe(options: &[urd::ir::ChoiceEvent], stdin: &mut impl BufRead) -> usize {
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
            eprintln!("Invalid choice '{}', expected 1\u{2013}{}", input.trim(), n);
            std::process::exit(1);
        }
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Compilation pipeline
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Run all static-analysis passes over `ast` and print rich ariadne diagnostics
/// to stderr.  Analysis is non-fatal: issues are reported but execution continues.
fn run_analysis(ast: &urd::parser::ast::Ast, src: &str, filename: &str, loader: &FsLoader) {
    use std::collections::{HashMap, HashSet};

    let mut imported_structs: HashMap<String, Vec<urd::parser::ast::StructField>> = HashMap::new();
    let mut imported_enums: HashMap<String, Vec<String>> = HashMap::new();
    let mut imported_labels: HashSet<String> = HashSet::new();

    collect_analysis_imports(
        ast,
        loader,
        &mut imported_structs,
        &mut imported_enums,
        &mut imported_labels,
    );

    let errors =
        analysis::analyze_with_imports(ast, imported_structs, imported_enums, imported_labels);
    if !errors.is_empty() {
        analysis::render_errors_stderr(&errors, src, filename);
        eprintln!(
            "[analysis] {} issue(s) found in '{}'",
            errors.len(),
            filename
        );
    }
}

fn collect_analysis_imports(
    ast: &urd::parser::ast::Ast,
    loader: &FsLoader,
    structs: &mut std::collections::HashMap<String, Vec<urd::parser::ast::StructField>>,
    enums: &mut std::collections::HashMap<String, Vec<String>>,
    labels: &mut std::collections::HashSet<String>,
) {
    use urd::parser::ast::AstContent;
    use urd::vm::loader::FileLoader;

    match ast.content() {
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_analysis_imports(stmt, loader, structs, enums, labels);
            }
        }
        AstContent::Import { path, symbols } => {
            let src = match loader.load(path) {
                Ok(s) => s,
                Err(_) => return,
            };
            let module_ast = match urd::compiler::loader::parse_source(&src) {
                Ok(a) => a,
                Err(_) => return,
            };
            // A whole-module import has a single symbol entry with `original: None`.
            let is_whole_module = symbols.first().map_or(false, |s| s.original.is_none());
            let alias = if is_whole_module {
                symbols[0].alias.as_str()
            } else {
                ""
            };
            collect_type_defs_from_ast(&module_ast, alias, structs, enums);
            // For symbol imports, register each directly-imported name as a known label.
            if !is_whole_module {
                for sym in symbols {
                    if sym.original.is_some() {
                        labels.insert(sym.alias.clone());
                    }
                }
            }
        }
        _ => {}
    }
}

fn collect_type_defs_from_ast(
    ast: &urd::parser::ast::Ast,
    alias: &str,
    structs: &mut std::collections::HashMap<String, Vec<urd::parser::ast::StructField>>,
    enums: &mut std::collections::HashMap<String, Vec<String>>,
) {
    use urd::parser::ast::AstContent;

    match ast.content() {
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_type_defs_from_ast(stmt, alias, structs, enums);
            }
        }
        AstContent::StructDecl { name, fields } => {
            if !alias.is_empty() {
                structs
                    .entry(format!("{alias}.{name}"))
                    .or_insert_with(|| fields.clone());
            }
            structs
                .entry(name.clone())
                .or_insert_with(|| fields.clone());
        }
        AstContent::EnumDecl { name, variants } => {
            if !alias.is_empty() {
                enums
                    .entry(format!("{alias}.{name}"))
                    .or_insert_with(|| variants.clone());
            }
            enums
                .entry(name.clone())
                .or_insert_with(|| variants.clone());
        }
        AstContent::LabeledBlock { block, .. } => {
            collect_type_defs_from_ast(block, alias, structs, enums);
        }
        _ => {}
    }
}

/// Load, lex, parse, analyse and compile the script at `path` into an [`IrGraph`].
///
/// This is the shared front-end used by both `run` and `export`.
fn build_graph(path: &Path) -> Result<IrGraph, String> {
    use chumsky::input::Stream;
    use chumsky::prelude::*;
    use urd::lexer::{Token, lex_src};

    let src = std::fs::read_to_string(path)
        .map_err(|e| format!("Could not read '{}': {}", path.display(), e))?;

    let display_name = path.display().to_string();

    // ── Loader (created early so analysis can resolve imports) ────────────
    let parent = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let loader = FsLoader::new(parent);

    // ── Lex ───────────────────────────────────────────────────────────────
    let lexer = lex_src(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(e) => (Token::Error(e), span.into()),
    });
    let stream =
        Stream::from_iter(lexer).map((0..src.len()).into(), |(t, s): (Token, SimpleSpan)| (t, s));

    // ── Parse ─────────────────────────────────────────────────────────────
    let (maybe_ast, parse_errs) = script().parse(stream).into_output_errors();

    if !parse_errs.is_empty() {
        render_parse_errors_stderr(&parse_errs, &src, &display_name);
        return Err(format!(
            "{} parse error(s) in '{display_name}'",
            parse_errs.len()
        ));
    }

    let ast = maybe_ast.ok_or_else(|| format!("failed to parse '{display_name}'"))?;

    run_analysis(&ast, &src, &display_name, &loader);

    Compiler::compile_with_loader(&ast, &loader)
        .map_err(|e| format!("Compile error in '{}':\n{:?}", path.display(), e))
}

/// Wrap a compiled [`IrGraph`] into a ready-to-run [`Vm`].
fn build_vm(graph: IrGraph) -> Result<Vm, String> {
    let registry = DecoratorRegistry::new();
    Vm::new(graph, registry).map_err(|e| format!("VM initialisation error:\n{:?}", e))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Subcommand: run
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

fn cmd_run(script_path: &Path) {
    let is_tty = io::stdin().is_terminal();

    print_separator();
    println!("  \x1b[1mUrd Quest Runner\x1b[0m");
    println!("  Loading: {}", script_path.display());
    print_separator();

    let graph = match build_graph(script_path) {
        Ok(g) => g,
        Err(msg) => {
            eprintln!("\n\x1b[91mError:\x1b[0m {}\n", msg);
            std::process::exit(1);
        }
    };

    let mut vm = match build_vm(graph) {
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
            Some(Ok(Event::Dialogue {
                speakers, lines, ..
            })) => {
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

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Subcommand: export
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

fn cmd_export(script_path: &Path, format: ExportFormat, output: Option<&Path>) {
    let graph = match build_graph(script_path) {
        Ok(g) => g,
        Err(msg) => {
            eprintln!("\x1b[91mError:\x1b[0m {}", msg);
            std::process::exit(1);
        }
    };

    let rendered = match format {
        ExportFormat::Dot => graph.to_dot(),
        ExportFormat::Mermaid => graph.to_mermaid(),
        ExportFormat::Sequence => graph.to_sequence_mermaid(),
    };

    match output {
        Some(path) => {
            if let Err(e) = std::fs::write(path, &rendered) {
                eprintln!(
                    "\x1b[91mError:\x1b[0m could not write '{}': {}",
                    path.display(),
                    e
                );
                std::process::exit(1);
            }

            let fmt_label = match format {
                ExportFormat::Dot => "DOT",
                ExportFormat::Mermaid => "Mermaid flowchart",
                ExportFormat::Sequence => "Mermaid sequence diagram",
            };
            eprintln!("Exported {} graph to '{}'", fmt_label, path.display());
        }
        None => {
            print!("{rendered}");
            flush_stdout();
        }
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Entry point
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

fn main() {
    env_logger::init();

    let cli = Cli::parse();

    match cli.command {
        None => {
            // No subcommand — default to running the demo script.
            cmd_run(Path::new("examples/quest/cave.urd"));
        }
        Some(Command::Run { script }) => {
            cmd_run(&script);
        }
        Some(Command::Export {
            script,
            format,
            output,
        }) => {
            cmd_export(&script, format, output.as_deref());
        }
    }
}
