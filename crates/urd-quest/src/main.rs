//! # quest — interactive Urd script runner & graph exporter
//!
//! ## Subcommands
//!
//! - **`run`** (default) — loads a `.urd` file and runs it interactively.
//! - **`export`** — compiles a `.urd` file and exports its IR graph as
//!   Graphviz DOT or Mermaid flowchart.

mod localizer;

use unic_langid::LanguageIdentifier;

use std::io::{self, BufRead, IsTerminal, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::{Parser, Subcommand, ValueEnum};
use crossterm::{
    cursor,
    event::{self, Event as CtEvent, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{self, ClearType},
};

use urd::analysis;
use urd::analysis::imports::{collect_label_names_from_ast, collect_type_defs_from_ast};

use urd::VmStep;
use urd::ir::{Event, IrGraph};
use urd::parser::block::script;
use urd::parser::errors::render_parse_errors_stderr;
use urd::runtime::value::RuntimeValue;
use urd::vm::loader::FsLoader;
use urd::vm::{DecoratorRegistry, Vm};

use localizer::FsLocalizer;

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

        /// Force a specific locale tag (e.g. `en-US`, `pl-PL`).
        ///
        /// When omitted and an `i18n/` directory exists next to the script,
        /// quest will offer an interactive picker if multiple locales are
        /// available, or silently load the single available locale.
        #[arg(short, long, value_name = "TAG")]
        locale: Option<String>,
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

    /// Generate a Fluent `.ftl` stub file from a compiled Urd script.
    #[command(name = "gen-l10n")]
    GenL10n {
        /// Path to the `.urd` script to process.
        script: std::path::PathBuf,

        /// Output directory where `.ftl` file(s) are written.
        /// Defaults to `i18n/en-US/` relative to the script's parent directory.
        #[arg(short, long, value_name = "DIR")]
        output: Option<std::path::PathBuf>,
    },
}

/// Supported graph export formats.
#[derive(Debug, Clone, Copy, ValueEnum)]
enum ExportFormat {
    /// Graphviz DOT
    Dot,
    /// Mermaid flowchart
    Mermaid,
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
        let name = map.borrow()
            .get("name")
            .map(|v| display_value(v))
            .unwrap_or_else(|| "???".to_string());
        let color = map.borrow()
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

        RuntimeValue::Roll(rolls) => {
            let sum: i64 = rolls
                .iter()
                .try_fold(0i64, |acc, &x| acc.checked_add(x))
                .unwrap_or(i64::MAX);
            let parts = rolls
                .iter()
                .map(|r| r.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("roll({sum}) [{parts}]")
        }
        other => format!("{:?}", other),
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Terminal utilities
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// Print a separator line.
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Locale discovery & interactive picker
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Scan `i18n_dir` for valid locale subdirectories.
///
/// Returns a sorted list of BCP-47 locale tags found as immediate
/// subdirectories whose names parse as valid [`LanguageIdentifier`]s.
/// Returns an empty vec if the directory does not exist or cannot be read.
fn discover_locales(i18n_dir: &Path) -> Vec<String> {
    if !i18n_dir.exists() {
        return Vec::new();
    }
    let Ok(entries) = std::fs::read_dir(i18n_dir) else {
        return Vec::new();
    };
    let mut locales: Vec<String> = entries
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().map(|t| t.is_dir()).unwrap_or(false))
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            // Accept only names that parse as valid BCP-47 identifiers.
            name.parse::<LanguageIdentifier>().ok().map(|_| name)
        })
        .collect();
    locales.sort();
    locales
}

/// Returns a human-readable display name for common BCP-47 locale tags.
///
/// Falls back to the raw tag string for any tag not in the static table.
fn locale_display_name(tag: &str) -> &str {
    match tag {
        "de-DE" => "Deutsch (Deutschland)",
        "en-GB" => "English (United Kingdom)",
        "en-US" => "English (United States)",
        "es-ES" => "Español (España)",
        "fr-FR" => "Français (France)",
        "it-IT" => "Italiano (Italia)",
        "ja-JP" => "日本語 (日本)",
        "ko-KR" => "한국어 (대한민국)",
        "pl-PL" => "Polski (Polska)",
        "pt-BR" => "Português (Brasil)",
        "ru-RU" => "Русский (Россия)",
        "uk-UA" => "Українська (Україна)",
        "zh-CN" => "中文 (中国)",
        "zh-TW" => "中文 (台灣)",
        other => other,
    }
}

/// Render the locale picker menu with `selected` highlighted.
fn render_locale_menu(locales: &[String], selected: usize) {
    let mut out = std::io::stdout();
    for (i, tag) in locales.iter().enumerate() {
        execute!(
            out,
            cursor::MoveToColumn(0),
            terminal::Clear(ClearType::CurrentLine)
        )
        .ok();
        let name = locale_display_name(tag);
        let has_name = name != tag;
        if i == selected {
            if has_name {
                println!("  \x1b[1;93m▶  {tag}\x1b[0m  \x1b[93m{name}\x1b[0m");
            } else {
                println!("  \x1b[1;93m▶  {tag}\x1b[0m");
            }
        } else if has_name {
            println!("     \x1b[2m{tag}  {name}\x1b[0m");
        } else {
            println!("     \x1b[2m{tag}\x1b[0m");
        }
    }
}

/// Interactive arrow-key locale picker. Returns the index of the chosen locale.
///
/// Follows the same raw-mode pattern as [`handle_choice_tty`]. Only call this
/// when stdin is a real TTY.
fn pick_locale_tty(locales: &[String]) -> usize {
    let n = locales.len();
    let mut selected = 0usize;

    println!();
    println!("  \x1b[93m🌐 Select language\x1b[0m  \x1b[2m(↑↓ navigate, Enter confirm)\x1b[0m");
    println!();
    render_locale_menu(locales, selected);

    terminal::enable_raw_mode().ok();

    loop {
        match event::read() {
            Ok(CtEvent::Key(KeyEvent {
                code, modifiers, ..
            })) => {
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
                    KeyCode::Enter | KeyCode::Char(' ') => break,
                    _ => {}
                }
            }
            Err(_) => break,
            _ => {}
        }

        execute!(
            std::io::stdout(),
            cursor::MoveUp(n as u16),
            terminal::Clear(ClearType::FromCursorDown)
        )
        .ok();
        render_locale_menu(locales, selected);
    }

    terminal::disable_raw_mode().ok();

    // Redraw in confirmed style.
    execute!(
        std::io::stdout(),
        cursor::MoveUp(n as u16),
        terminal::Clear(ClearType::FromCursorDown)
    )
    .ok();
    let mut out = std::io::stdout();
    for (i, tag) in locales.iter().enumerate() {
        execute!(
            out,
            cursor::MoveToColumn(0),
            terminal::Clear(ClearType::CurrentLine)
        )
        .ok();
        let name = locale_display_name(tag);
        let has_name = name != tag;
        if i == selected {
            if has_name {
                println!("  \x1b[92m✓  {tag}\x1b[0m  \x1b[2m{name}\x1b[0m");
            } else {
                println!("  \x1b[92m✓  {tag}\x1b[0m");
            }
        } else if has_name {
            println!("     \x1b[2m{tag}  {name}\x1b[0m");
        } else {
            println!("     \x1b[2m{tag}\x1b[0m");
        }
    }

    selected
}

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
///
/// If `localized_text` is `Some`, it is shown as a single pre-formatted block;
/// otherwise each entry in `lines` is rendered individually via [`display_value`].
fn handle_dialogue(
    speakers: &[RuntimeValue],
    lines: &[RuntimeValue],
    localized_text: Option<&str>,
    stdin: &mut impl BufRead,
    is_tty: bool,
) {
    println!();

    if let Some(ch) = format_speakers(speakers) {
        println!("  {}{}{RESET}:", ch.color, ch.name);
    }

    if let Some(text) = localized_text {
        // Show localized text, indenting every line uniformly.
        for line in text.lines() {
            println!("    {}", line);
        }
    } else {
        // Fall back to raw lines.
        for line_val in lines {
            println!("    {}", display_value(line_val));
        }
    }

    wait_for_enter(stdin, is_tty);
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Interactive choice menu (TTY)
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Render all options with `selected` highlighted using a bold-yellow arrow.
/// Each line is cleared before printing to prevent stale text from bleeding
/// through when option labels differ in length across redraws.
///
/// Shows `localized_label` when available, falling back to `label`.
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
        let display = opt.localized_label.as_deref().unwrap_or(&opt.label);
        if i == selected {
            println!("  \x1b[1;93m▶  {}\x1b[0m", display);
        } else {
            println!("   \x1b[2m  {}\x1b[0m", display);
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
        let display = opt.localized_label.as_deref().unwrap_or(&opt.label);
        if i == selected {
            println!("  \x1b[92m✓  {}\x1b[0m", display);
        } else {
            println!("     \x1b[2m{}\x1b[0m", display);
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
        let display = opt.localized_label.as_deref().unwrap_or(&opt.label);
        println!("  {}. {}", i + 1, display);
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
fn run_analysis(
    ast: &urd::parser::ast::Ast,
    src: &str,
    filename: &str,
    loader: &dyn urd::vm::loader::FileLoader,
) {
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
    loader: &dyn urd::vm::loader::FileLoader,
    structs: &mut std::collections::HashMap<String, Vec<urd::parser::ast::StructField>>,
    enums: &mut std::collections::HashMap<String, Vec<String>>,
    labels: &mut std::collections::HashSet<String>,
) {
    use urd::parser::ast::AstContent;

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
            let is_whole_module = symbols.first().is_some_and(|s| s.original.is_none());
            let alias = if is_whole_module {
                symbols[0].alias.as_str()
            } else {
                ""
            };
            collect_type_defs_from_ast(&module_ast, alias, structs, enums);
            // For symbol imports, only register aliases whose imported original
            // resolves to a label declaration in the imported module.
            if !is_whole_module {
                let imported_label_names = collect_label_names_from_ast(&module_ast);
                for sym in symbols {
                    if let Some(original) = sym.original.as_ref()
                        && imported_label_names.contains(original)
                    {
                        labels.insert(sym.alias.clone());
                    }
                }
            }
        }
        // Defensively recurse into compound nodes in case imports appear
        // inside conditional, menu, or match blocks (the compiler permits it).
        AstContent::If {
            then_block,
            else_block,
            ..
        } => {
            collect_analysis_imports(then_block, loader, structs, enums, labels);
            if let Some(eb) = else_block {
                collect_analysis_imports(eb, loader, structs, enums, labels);
            }
        }
        AstContent::LabeledBlock { block, .. } => {
            collect_analysis_imports(block, loader, structs, enums, labels);
        }
        AstContent::Menu { options } => {
            for opt in options {
                collect_analysis_imports(opt, loader, structs, enums, labels);
            }
        }
        AstContent::MenuOption { content, .. } => {
            collect_analysis_imports(content, loader, structs, enums, labels);
        }
        AstContent::Match { arms, .. } => {
            for arm in arms {
                collect_analysis_imports(&arm.body, loader, structs, enums, labels);
            }
        }
        AstContent::DecoratorDef { body, .. } => {
            collect_analysis_imports(body, loader, structs, enums, labels);
        }
        // All other node types cannot contain import statements.
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

    // Pass the root filename (e.g. "main.urd") so the flat pipeline can
    // deduplicate any back-import of the root against the already-loaded
    // root entry, preventing its preamble from executing twice.
    let root_filename = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    urd::compiler::loader::compile_recursive_with_root_path(&ast, root_filename, &loader)
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

fn cmd_run(script_path: &Path, forced_locale: Option<&str>) -> Result<(), String> {
    let is_tty = io::stdin().is_terminal();

    print_separator();
    println!("  \x1b[1mUrd Quest Runner\x1b[0m");
    println!("  Loading: {}", script_path.display());
    print_separator();

    let graph = build_graph(script_path)?;

    let vm = build_vm(graph)?;

    // ── Locale discovery & selection ─────────────────────────────────────────
    //
    // 1. If --locale was passed on the CLI, use it (hard error if not found).
    // 2. Otherwise scan i18n/ for available locale directories.
    //    • 0 locales → run without translation.
    //    • 1 locale  → load it silently, show a status line.
    //    • 2+ locales → show an interactive picker (TTY) or pick the first
    //                   one with a stderr notice (pipe / non-TTY).
    let i18n_dir = script_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .join("i18n");

    let chosen_locale: Option<String> = if let Some(tag) = forced_locale {
        Some(tag.to_string())
    } else {
        let locales = discover_locales(&i18n_dir);
        match locales.len() {
            0 => None,
            1 => locales.into_iter().next(),
            _ => {
                let idx = if is_tty {
                    pick_locale_tty(&locales)
                } else {
                    eprintln!(
                        "[l10n] multiple locales available; using '{}' (pass --locale to override)",
                        locales[0]
                    );
                    0
                };
                locales.into_iter().nth(idx)
            }
        }
    };

    let mut vm = match chosen_locale {
        Some(ref tag) => match FsLocalizer::load(&i18n_dir, tag) {
            Ok(localizer) => {
                let name = locale_display_name(tag);
                if name != tag {
                    println!("  \x1b[2m🌐 {tag}  {name}\x1b[0m");
                } else {
                    println!("  \x1b[2m🌐 {tag}\x1b[0m");
                }
                print_separator();
                vm.with_localizer(Arc::new(localizer))
            }
            Err(e) => {
                return Err(format!("could not load locale '{}': {}", tag, e));
            }
        },
        None => vm,
    };

    let stdin = io::stdin();
    let mut stdin_lock = stdin.lock();

    let mut choice: Option<usize> = None;

    loop {
        match vm.next(choice) {
            VmStep::Ended => {
                println!();
                print_separator();
                println!("  \x1b[1m--- The End ---\x1b[0m");
                print_separator();
                break;
            }
            VmStep::Error(e) => {
                return Err(format!("Runtime error: {:?}", e));
            }
            VmStep::Event(Event::Dialogue {
                speakers,
                lines,
                localized_text,
                ..
            }) => {
                handle_dialogue(
                    &speakers,
                    &lines,
                    localized_text.as_deref(),
                    &mut stdin_lock,
                    is_tty,
                );
                choice = None;
            }
            VmStep::Event(Event::Choice { options, .. }) => {
                let idx = if is_tty {
                    handle_choice_tty(&options)
                } else {
                    handle_choice_pipe(&options, &mut stdin_lock)
                };
                choice = Some(idx);
            }
            // Forward-compatibility: `Event` and `VmStep` are `#[non_exhaustive]`.
            _ => {
                eprintln!("\n\x1b[93mWarning:\x1b[0m unhandled VM step, skipping.\n");
                choice = None;
            }
        }
    }

    Ok(())
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Subcommand: gen-l10n
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Compile `script_path` and write Fluent `.ftl` stubs to `output_dir`.
///
/// For single-file scripts, one `<script_stem>.ftl` is produced.  For
/// multi-file scripts (with imports), one `.ftl` is produced **per source
/// module** — the root module and each imported file get their own file,
/// so shared imports are never duplicated across `.ftl` files.
///
/// Output defaults to `i18n/en-US/` next to the script.
fn cmd_gen_l10n(script_path: &Path, output_dir: Option<&Path>) -> Result<(), String> {
    let graph = build_graph(script_path)?;

    // Determine the file slug from the script name.
    let file_slug = script_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("script");

    // Determine output directory.
    let out_dir = output_dir.map(|p| p.to_path_buf()).unwrap_or_else(|| {
        let parent = script_path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf();
        parent.join("i18n").join("en-US")
    });

    std::fs::create_dir_all(&out_dir).map_err(|e| {
        format!("could not create '{}': {}", out_dir.display(), e)
    })?;

    // Collect the distinct source modules.  For multi-file scripts this
    // returns one entry per source file ("" = root, "common.urd" = import);
    // for single-file scripts the vec is empty and we fall back to emitting
    // one monolithic .ftl.
    let modules = urd::loc::ftl::source_modules(&graph);

    if modules.is_empty() {
        // Single-file script: emit everything into one .ftl.
        let ftl_content = urd::loc::ftl::generate_ftl(&graph, file_slug);
        write_ftl(&out_dir, file_slug, &ftl_content)?;
    } else {
        // Multi-file script: one .ftl per source module — no duplicates.
        for source in &modules {
            let slug = if source.is_empty() {
                // Root module → use the script's own stem.
                file_slug.to_string()
            } else {
                // Imported module: "common.urd" → "common".
                Path::new(source.as_str())
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(source)
                    .to_string()
            };
            let ftl_content = urd::loc::ftl::generate_ftl_for_file(&graph, &slug, source);
            write_ftl(&out_dir, &slug, &ftl_content)?;
        }
    }

    Ok(())
}

/// Write `content` to `<dir>/<slug>.ftl`, printing a success message or
/// returning an error.
fn write_ftl(dir: &Path, slug: &str, content: &str) -> Result<(), String> {
    let out_file = dir.join(format!("{slug}.ftl"));
    std::fs::write(&out_file, content).map_err(|e| {
        format!("could not write '{}': {}", out_file.display(), e)
    })?;
    eprintln!(
        "Generated Fluent file: \x1b[32m{}\x1b[0m",
        out_file.display()
    );
    Ok(())
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Subcommand: export
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

fn cmd_export(script_path: &Path, format: ExportFormat, output: Option<&Path>) -> Result<(), String> {
    let graph = build_graph(script_path)?;

    let rendered = match format {
        ExportFormat::Dot => graph.to_dot(),
        ExportFormat::Mermaid => graph.to_mermaid(),
    };

    match output {
        Some(path) => {
            std::fs::write(path, &rendered).map_err(|e| {
                format!("could not write '{}': {}", path.display(), e)
            })?;

            let fmt_label = match format {
                ExportFormat::Dot => "DOT",
                ExportFormat::Mermaid => "Mermaid flowchart",
            };
            eprintln!("Exported {} graph to '{}'", fmt_label, path.display());
        }
        None => {
            print!("{rendered}");
            flush_stdout();
        }
    }

    Ok(())
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//  Entry point
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

fn main() {
    env_logger::init();

    let cli = Cli::parse();

    let result = match cli.command {
        None => {
            // No subcommand — default to running the demo script.
            cmd_run(Path::new("examples/quest/cave.urd"), None)
        }
        Some(Command::Run { script, locale }) => {
            cmd_run(&script, locale.as_deref())
        }
        Some(Command::Export {
            script,
            format,
            output,
        }) => {
            cmd_export(&script, format, output.as_deref())
        }
        Some(Command::GenL10n { script, output }) => {
            cmd_gen_l10n(&script, output.as_deref())
        }
    };

    if let Err(msg) = result {
        eprintln!("\n\x1b[91mError:\x1b[0m {msg}\n");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::collect_analysis_imports;
    use urd::compiler::loader::parse_source;
    use urd::vm::loader::MemLoader;

    fn parse(src: &str) -> urd::parser::ast::Ast {
        parse_source(src).expect("test source should parse")
    }

    #[test]
    fn collect_analysis_imports_only_marks_symbols_that_are_labels() {
        let mut loader = MemLoader::new();
        loader.add(
            "lib.urd",
            r#"
label greet {
  end!()
}

const answer = 42
struct Character { name: str }
enum Faction { Guild, Empire }
"#,
        );

        let ast = parse(
            r#"
import (greet, answer, Character, Faction) from "lib.urd"

label start {
  jump greet
}
"#,
        );

        let mut structs: HashMap<String, Vec<urd::parser::ast::StructField>> = HashMap::new();
        let mut enums: HashMap<String, Vec<String>> = HashMap::new();
        let mut labels: HashSet<String> = HashSet::new();

        collect_analysis_imports(&ast, &loader, &mut structs, &mut enums, &mut labels);

        assert!(
            labels.contains("greet"),
            "expected imported label to be tracked"
        );
        assert!(
            !labels.contains("answer"),
            "const import must not be classified as label"
        );
        assert!(
            !labels.contains("Character"),
            "struct import must not be classified as label"
        );
        assert!(
            !labels.contains("Faction"),
            "enum import must not be classified as label"
        );
    }

    #[test]
    fn collect_analysis_imports_only_marks_alias_when_original_is_label() {
        let mut loader = MemLoader::new();
        loader.add(
            "lib.urd",
            r#"
label greet { end!() }
const answer = 42
"#,
        );

        let ast = parse(
            r#"
import (greet as hello, answer as life) from "lib.urd"
label start { jump hello }
"#,
        );

        let mut structs: HashMap<String, Vec<urd::parser::ast::StructField>> = HashMap::new();
        let mut enums: HashMap<String, Vec<String>> = HashMap::new();
        let mut labels: HashSet<String> = HashSet::new();

        collect_analysis_imports(&ast, &loader, &mut structs, &mut enums, &mut labels);

        assert!(
            labels.contains("hello"),
            "label alias should be tracked when original symbol is a label"
        );
        assert!(
            !labels.contains("life"),
            "non-label alias must not be tracked as label"
        );
    }
}
