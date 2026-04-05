//! # Fluent Translation List (FTL) Generator
//!
//! This module walks a compiled [`IrGraph`] and produces a `.ftl`
//! ([Project Fluent](https://projectfluent.org/)) stub file.  Each
//! [`IrNodeKind::Dialogue`] line and each [`IrNodeKind::Choice`] option that
//! carries a `loc_id` gets one message entry pre-filled with its
//! source-language text.  Translators copy the file and replace the values.
//!
//! ## Entry points
//!
//! - [`generate_ftl`] — compile a graph into a `.ftl` string (all labels).
//! - [`generate_ftl_for_file`] — like `generate_ftl`, but only entries from
//!   one source module.
//! - [`source_modules`] — list the distinct source-module paths in a graph.
//!
//! ## Lower-level helpers (also public for testing / tooling)
//!
//! - [`collect_interpolation_paths`] — find every `{var}` path in an AST.
//! - [`render_parsed_string_as_ftl`] — turn a [`ParsedString`] into a Fluent
//!   message fragment.
//! - [`render_lines_ast_as_ftl`] — render a dialogue-lines AST node.

use std::collections::HashMap;

use petgraph::stable_graph::NodeIndex;

use crate::ir::analysis;
use crate::ir::{IrGraph, IrNodeKind};
use crate::lexer::strings::{ParsedString, StringPart};
use crate::parser::ast::{Ast, AstContent, DeclKind};
use crate::runtime::value::RuntimeValue;

// ─── Public helpers ───────────────────────────────────────────────────────────

/// Recursively walks `ast` and collects every string-interpolation variable
/// path found inside [`RuntimeValue::Str`] leaves.
///
/// Returned paths are de-duplicated and sorted alphabetically.  Paths keep
/// their original dot-separated form (e.g. `"user.name"`); callers that need a
/// Fluent-safe identifier should replace `.` with `-` themselves.
///
/// # Example
///
/// ```ignore
/// let ps = ParsedString::new_from_parts(vec![
///     StringPart::Interpolation(Interpolation { path: "gold".into(), format: None }),
/// ]);
/// let ast = Ast::value(RuntimeValue::Str(ps));
/// assert_eq!(collect_interpolation_paths(&ast), vec!["gold".to_string()]);
/// ```
pub fn collect_interpolation_paths(ast: &Ast) -> Vec<String> {
    let mut paths: Vec<String> = Vec::new();
    collect_paths_inner(ast, &mut paths);
    paths.sort();
    paths.dedup();
    paths
}

fn collect_paths_inner(ast: &Ast, paths: &mut Vec<String>) {
    match ast.content() {
        // Leaf: collect every interpolation in this string.
        AstContent::Value(RuntimeValue::Str(ps)) => {
            for part in ps.parts() {
                if let StringPart::Interpolation(interp) = part {
                    paths.push(interp.path.clone());
                }
            }
        }
        // Non-leaf: recurse into children (ExprList, Block, BinOp, etc.).
        _ => {
            for child in ast.children() {
                collect_paths_inner(child, paths);
            }
        }
    }
}

/// Renders a [`ParsedString`] as a Fluent message value fragment.
///
/// | Part | Output |
/// |---|---|
/// | [`StringPart::Literal`]`(s)` | `s` verbatim |
/// | [`StringPart::EscapedChar`]`(s)` | `s` verbatim (already decoded) |
/// | [`StringPart::Interpolation`] | `{ $var }` with `.` replaced by `-` |
/// | [`StringPart::ExitString`] | *(empty)* |
///
/// # Example
///
/// ```ignore
/// let ps = ParsedString::new_from_parts(vec![
///     StringPart::Literal("hello ".into()),
///     StringPart::Interpolation(Interpolation { path: "user.name".into(), format: None }),
/// ]);
/// assert_eq!(render_parsed_string_as_ftl(&ps), "hello { $user-name }");
/// ```
/// Converts a menu option label string (already rendered via [`ParsedString`]'s
/// `Display` impl, so interpolations appear as `{var}`) into a Fluent message
/// value where those interpolations become `{ $var }`.
///
/// Dots in variable paths are replaced with hyphens to match Fluent
/// identifier rules (mirrors what [`render_parsed_string_as_ftl`] does).
///
/// Format specifiers (`{var:fmt}`) are stripped — Fluent handles formatting
/// through its own selector syntax.
fn label_to_ftl_value(label: &str) -> String {
    let mut out = String::with_capacity(label.len());
    let mut chars = label.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '{' {
            let mut inner = String::new();
            let mut closed = false;
            for ic in chars.by_ref() {
                if ic == '}' {
                    closed = true;
                    break;
                }
                inner.push(ic);
            }
            if closed && !inner.is_empty() {
                // Strip any `:format` specifier before converting.
                let var_path = inner.split(':').next().unwrap_or(&inner);
                let ftl_var = var_path.replace('.', "-");
                out.push_str(&format!("{{ ${ftl_var} }}"));
            } else {
                // Malformed brace expression — pass through unchanged.
                out.push('{');
                out.push_str(&inner);
                if closed {
                    out.push('}');
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Renders a [`ParsedString`] as a Fluent (FTL) string value.
///
/// Literal and escaped-character parts are emitted verbatim; interpolation
/// parts are converted to Fluent variable references (`{ $var_name }`, with
/// any `.` separators replaced by `-` to satisfy FTL identifier rules).
/// [`StringPart::ExitString`] sentinels are silently skipped.
pub fn render_parsed_string_as_ftl(ps: &ParsedString) -> String {
    let mut out = String::new();
    for part in ps.parts() {
        match part {
            StringPart::Literal(s) | StringPart::EscapedChar(s) => out.push_str(s),
            StringPart::Interpolation(interp) => {
                let ftl_var = interp.path.replace('.', "-");
                out.push_str(&format!("{{ ${ftl_var} }}"));
            }
            StringPart::ExitString => {}
        }
    }
    out
}

/// Renders the `lines` [`Ast`] node from an [`IrNodeKind::Dialogue`] as a
/// Fluent message value.
///
/// - [`AstContent::Value`]`(`[`RuntimeValue::Str`]`)` — rendered directly.
/// - [`AstContent::ExprList`] — every string-valued child is rendered and the
///   results are joined with `"\n    "` (the FTL multi-line indent convention).
///   Non-string children are silently skipped.
/// - Anything else — `"???"`.
///
/// # Example
///
/// ```ignore
/// let ast = Ast::value(RuntimeValue::Str(ParsedString::new_plain("Hello")));
/// assert_eq!(render_lines_ast_as_ftl(&ast), "Hello");
/// ```
pub fn render_lines_ast_as_ftl(ast: &Ast) -> String {
    match ast.content() {
        AstContent::Value(RuntimeValue::Str(ps)) => render_parsed_string_as_ftl(ps),
        AstContent::ExprList(items) => {
            let rendered: Vec<String> = items
                .iter()
                .filter_map(|item| match item.content() {
                    AstContent::Value(RuntimeValue::Str(ps)) => {
                        Some(render_parsed_string_as_ftl(ps))
                    }
                    _ => None,
                })
                .collect();
            if rendered.is_empty() {
                "???".to_string()
            } else {
                rendered.join("\n    ")
            }
        }
        _ => "???".to_string(),
    }
}

// ─── FTL generator ────────────────────────────────────────────────────────────

/// Generate a Fluent `.ftl` stub file from a compiled [`IrGraph`].
///
/// `file_slug` is the source file stem (e.g. `"cave"` for `cave.urd`). It
/// should match the slug used to compile the graph via
/// [`crate::compiler::Compiler::compile_named`].
///
/// The returned string is a valid `.ftl` file containing one message entry per
/// `Dialogue` line and `Choice` option. Each entry is initialised with the
/// source-language text extracted from the IR. Translators copy the file and
/// replace the values.
///
/// Sections are grouped by label and annotated with:
/// - `@fluent`-tagged variables in scope at that label.
/// - String-interpolation variables used in each message.
///
/// # Example output
///
/// ```text
/// ### cave.ftl — generated by urd
/// ### Run `quest gen-l10n cave.urd` to regenerate.
/// ### Source: cave.urd
///
/// ## label: start
/// # @fluent variables: none
///
/// cave-start-line_1 = The wind howls across the barren moor.
///
/// # menu: cave-start-menu_1
/// cave-start-menu_1-enter_the_cave = Enter the cave
/// cave-start-menu_1-walk_away = Walk away
/// ```
/// Generate a `.ftl` stub containing all entries from the compiled graph.
///
/// This is the original entry point — it emits every label regardless of
/// source module.  See [`generate_ftl_for_file`] for per-module filtering.
pub fn generate_ftl(graph: &IrGraph, file_slug: &str) -> String {
    generate_ftl_inner(graph, file_slug, None)
}

/// Generate a `.ftl` stub containing only the entries that belong to the
/// specified source module.
///
/// `source_filter` selects which labels to include based on
/// [`IrGraph::label_sources`]:
/// - `""` (empty string) → only the root module's labels.
/// - `"tavern.urd"` → only labels from that imported module.
///
/// When `label_sources` is empty (single-file compilation), *all* labels are
/// included — the filter is a no-op.
///
/// `file_slug` is used for the FTL header comment (e.g. `"merchant"`).
pub fn generate_ftl_for_file(graph: &IrGraph, file_slug: &str, source_filter: &str) -> String {
    generate_ftl_inner(graph, file_slug, Some(source_filter))
}

/// Returns the distinct source-module paths from [`IrGraph::label_sources`],
/// sorted alphabetically and deduplicated.
///
/// When `label_sources` is empty (single-file compilation), returns an empty
/// vec — callers should fall back to [`generate_ftl`] in that case.
pub fn source_modules(graph: &IrGraph) -> Vec<String> {
    if graph.label_sources.is_empty() {
        return Vec::new();
    }
    let mut modules: Vec<String> = graph.label_sources.values().cloned().collect();
    modules.sort();
    modules.dedup();
    modules
}

// ─── Inner implementation ─────────────────────────────────────────────────────

fn generate_ftl_inner(graph: &IrGraph, file_slug: &str, source_filter: Option<&str>) -> String {
    // ── Reachability & cluster analysis ───────────────────────────────────────
    //
    // For FTL generation every label's subgraph must be considered reachable —
    // not just nodes reachable from graph.entry.  Imported-module labels may
    // not be called from the root module yet, but their dialogue / choice
    // entries still need localisation stubs.
    //
    // We start with the standard entry-point reachable set, then extend it
    // with a DFS from every label entry point that wasn't already covered.
    let reachable = {
        let mut set = analysis::reachable_nodes(graph);
        let label_entry_nodes: Vec<NodeIndex> = if graph.cluster_names.is_empty() {
            graph.labels.values().copied().collect()
        } else {
            graph.cluster_names.keys().copied().collect()
        };
        for entry_idx in label_entry_nodes {
            if !set.contains(&entry_idx) {
                let mut dfs = petgraph::visit::Dfs::new(&graph.graph, entry_idx);
                while let Some(idx) = dfs.next(&graph.graph) {
                    set.insert(idx);
                }
            }
        }
        set
    };
    let clusters = analysis::compute_clusters(graph, &reachable);
    // node_to_label borrows &str from the keys of `clusters`; clusters must
    // outlive node_to_label (guaranteed by declaration order — Rust drops in
    // reverse order).
    let node_to_label: HashMap<NodeIndex, &str> = analysis::node_to_cluster(&clusters);

    // ── Step 1: collect @fluent-tagged variable bindings ─────────────────────
    //
    // Global / constant @fluent vars are surfaced in every label's comment.
    // Variable-scoped @fluent vars are local to the label that contains them.
    let mut global_fluent_vars: Vec<(String, String)> = Vec::new(); // (alias, var_name)
    let mut label_fluent_vars: HashMap<String, Vec<(String, String)>> = HashMap::new();

    for idx in graph.graph.node_indices() {
        if let Some(IrNodeKind::Assign {
            var,
            scope,
            fluent_alias: Some(alias),
            ..
        }) = graph.graph.node_weight(idx)
        {
            match scope {
                DeclKind::Global | DeclKind::Constant => {
                    global_fluent_vars.push((alias.clone(), var.clone()));
                }
                DeclKind::Variable | DeclKind::Assignment => {
                    if let Some(&label_name) = node_to_label.get(&idx) {
                        label_fluent_vars
                            .entry(label_name.to_string())
                            .or_default()
                            .push((alias.clone(), var.clone()));
                    }
                }
            }
        }
    }

    // ── Step 2: render Dialogue / Choice events per label ────────────────────
    //
    // We render each event to a String immediately so we never hold live
    // references into the graph across the sorting / rendering steps.
    // Each entry is tagged with its NodeIndex for sorting.
    let mut label_event_blocks: HashMap<String, Vec<(NodeIndex, String)>> = HashMap::new();

    for idx in graph.graph.node_indices() {
        match graph.graph.node_weight(idx) {
            // ── Dialogue ──
            Some(IrNodeKind::Dialogue {
                loc_id: Some(id),
                lines,
                ..
            }) => {
                if let Some(&label_name) = node_to_label.get(&idx) {
                    let mut block = String::new();

                    // Emit interpolation comment when the message uses variables.
                    let paths = collect_interpolation_paths(lines);
                    if !paths.is_empty() {
                        let path_list = paths
                            .iter()
                            .map(|p| format!("${}", p.replace('.', "-")))
                            .collect::<Vec<_>>()
                            .join(", ");
                        block.push_str(&format!("# interpolation: {path_list}\n"));
                    }

                    let text = render_lines_ast_as_ftl(lines);
                    block.push_str(&format!("{id} = {text}\n"));
                    block.push('\n');

                    label_event_blocks
                        .entry(label_name.to_string())
                        .or_default()
                        .push((idx, block));
                }
            }

            // ── Choice ──
            Some(IrNodeKind::Choice {
                loc_id: Some(id),
                options,
                ..
            }) => {
                if let Some(&label_name) = node_to_label.get(&idx) {
                    let mut block = String::new();
                    block.push_str(&format!("# menu: {id}\n"));
                    for option in options {
                        // Default options (`_`) are not player-visible text
                        // and should not appear in the localisation file.
                        if option.is_default {
                            continue;
                        }
                        if let Some(opt_id) = &option.loc_id {
                            block.push_str(&format!(
                                "{opt_id} = {}\n",
                                label_to_ftl_value(&option.label)
                            ));
                        }
                    }
                    block.push('\n');

                    label_event_blocks
                        .entry(label_name.to_string())
                        .or_default()
                        .push((idx, block));
                }
            }

            _ => {}
        }
    }

    // Sort events within each label by NodeIndex (ascending = compilation /
    // source order).
    for blocks in label_event_blocks.values_mut() {
        blocks.sort_by_key(|(idx, _)| idx.index());
    }

    // ── Step 3: sort labels by their entry NodeIndex ──────────────────────────
    //
    // Mirrors the strategy used by the DOT renderer: prefer cluster_names
    // (canonical, one entry per label, populated by multi-file compilation);
    // fall back to labels (single-file compilation).
    let mut sorted_labels: Vec<String> = clusters.keys().cloned().collect();
    sorted_labels.sort_by_key(|name| {
        graph
            .cluster_names
            .iter()
            .find_map(|(&idx, n)| if n == name { Some(idx) } else { None })
            .or_else(|| graph.labels.get(name).copied())
            .map(|idx| idx.index())
            .unwrap_or(usize::MAX)
    });

    // ── Step 3b: optional source-file filtering ──────────────────────────────
    //
    // When a source_filter is provided *and* label_sources is populated
    // (multi-file compilation), keep only labels whose entry node maps to
    // the requested source module.
    if let Some(filter) = source_filter
        && !graph.label_sources.is_empty()
    {
        sorted_labels.retain(|label_name| {
            let entry_idx = graph
                .cluster_names
                .iter()
                .find_map(|(&idx, n)| if n == label_name { Some(idx) } else { None })
                .or_else(|| graph.labels.get(label_name).copied());

            match entry_idx {
                Some(idx) => graph
                    .label_sources
                    .get(&idx)
                    .map(|s| s.as_str() == filter)
                    .unwrap_or(false),
                None => false,
            }
        });
    }

    // ── Step 4: render ────────────────────────────────────────────────────────
    let mut out = String::new();

    out.push_str(&format!("### {file_slug}.ftl \u{2014} generated by urd\n"));
    out.push_str(&format!(
        "### Run `quest gen-l10n {file_slug}.urd` to regenerate.\n"
    ));
    out.push_str(&format!("### Source: {file_slug}.urd\n"));
    out.push('\n');

    for label_name in &sorted_labels {
        // Skip clusters that have no reachable members — they would produce an
        // empty section with no messages, which is noise for translators.
        let is_empty = clusters
            .get(label_name.as_str())
            .map(|m| m.is_empty())
            .unwrap_or(true);
        if is_empty {
            continue;
        }

        // ── Label heading ──
        out.push_str(&format!("## label: {label_name}\n"));

        // ── @fluent variables comment ──
        // Globals are listed first, then label-local vars.
        let mut vars_in_scope: Vec<(String, String)> = global_fluent_vars.clone();
        if let Some(local_vars) = label_fluent_vars.get(label_name.as_str()) {
            vars_in_scope.extend_from_slice(local_vars);
        }

        if vars_in_scope.is_empty() {
            out.push_str("# @fluent variables: none\n");
        } else {
            let var_list = vars_in_scope
                .iter()
                .map(|(alias, var)| format!("${alias} (urd: {var})"))
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("# @fluent variables: {var_list}\n"));
        }

        // Blank line between the variables comment and the first message.
        out.push('\n');

        // ── Event blocks (dialogue lines + choice menus), source-ordered ──
        if let Some(blocks) = label_event_blocks.get(label_name.as_str()) {
            for (_, block) in blocks {
                out.push_str(block);
            }
        }
    }

    out
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Compiler;
    use crate::compiler::loader::{compile_recursive_with_root_path, parse_source};
    use crate::lexer::strings::{Interpolation, ParsedString, StringPart};
    use crate::parser::ast::{Ast, DeclKind, Decorator};
    use crate::runtime::value::RuntimeValue;
    use crate::vm::loader::MemLoader;

    // ── Test helpers ─────────────────────────────────────────────────────────

    /// A single-line dialogue content as an `ExprList` (mirrors the parser).
    fn str_line(s: &str) -> Ast {
        Ast::expr_list(vec![Ast::value(RuntimeValue::Str(
            ParsedString::new_plain(s),
        ))])
    }

    /// Single-element narrator speaker list.
    fn narrator() -> Ast {
        Ast::expr_list(vec![Ast::value(RuntimeValue::Str(
            ParsedString::new_plain("narrator"),
        ))])
    }

    // ── Unit tests: render helpers ────────────────────────────────────────────

    #[test]
    fn render_plain_string_unchanged() {
        let ps = ParsedString::new_plain("hello world");
        assert_eq!(render_parsed_string_as_ftl(&ps), "hello world");
    }

    #[test]
    fn render_string_with_simple_interpolation() {
        let ps = ParsedString::new_from_parts(vec![
            StringPart::Literal("coins: ".to_string()),
            StringPart::Interpolation(Interpolation {
                path: "gold".to_string(),
                format: None,
            }),
        ]);
        assert_eq!(render_parsed_string_as_ftl(&ps), "coins: { $gold }");
    }

    #[test]
    fn render_dotted_interpolation_replaces_dots_with_hyphens() {
        let ps = ParsedString::new_from_parts(vec![StringPart::Interpolation(Interpolation {
            path: "user.name".to_string(),
            format: None,
        })]);
        assert_eq!(render_parsed_string_as_ftl(&ps), "{ $user-name }");
    }

    #[test]
    fn collect_paths_deduplicates_and_sorts_alphabetically() {
        let ps = ParsedString::new_from_parts(vec![
            StringPart::Interpolation(Interpolation {
                path: "gold".to_string(),
                format: None,
            }),
            StringPart::Literal(" and ".to_string()),
            StringPart::Interpolation(Interpolation {
                path: "alpha".to_string(),
                format: None,
            }),
            StringPart::Interpolation(Interpolation {
                path: "gold".to_string(), // duplicate — must be removed
                format: None,
            }),
        ]);
        let ast = Ast::value(RuntimeValue::Str(ps));
        let paths = collect_interpolation_paths(&ast);
        assert_eq!(
            paths,
            vec!["alpha".to_string(), "gold".to_string()],
            "paths must be sorted and deduplicated"
        );
    }

    #[test]
    fn render_lines_multiline_joins_with_ftl_indent() {
        let ast = Ast::expr_list(vec![
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("Line one."))),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("Line two."))),
        ]);
        assert_eq!(render_lines_ast_as_ftl(&ast), "Line one.\n    Line two.");
    }

    #[test]
    fn render_lines_non_string_content_returns_question_marks() {
        let ast = Ast::value(RuntimeValue::Int(42));
        assert_eq!(render_lines_ast_as_ftl(&ast), "???");
    }

    // ── Integration tests: generate_ftl ───────────────────────────────────────

    #[test]
    fn empty_graph_produces_header_only() {
        let graph = IrGraph::new();
        let output = generate_ftl(&graph, "empty");

        assert!(
            output.contains("### empty.ftl"),
            "header must name the file slug"
        );
        assert!(
            output.contains("generated by urd"),
            "header must say 'generated by urd'"
        );
        assert!(
            output.contains("quest gen-l10n empty.urd"),
            "header must contain regeneration hint"
        );
        assert!(
            output.contains("### Source: empty.urd"),
            "header must contain source annotation"
        );
        assert!(
            !output.contains("## label:"),
            "empty graph must not produce any label section"
        );
    }

    #[test]
    fn single_dialogue_produces_message_entry() {
        let dialogue = Ast::dialogue(narrator(), str_line("The wind howls."));
        let ast = Ast::block(vec![Ast::labeled_block(
            "start".to_string(),
            Ast::block(vec![dialogue]),
        )]);

        let graph = Compiler::compile_named(&ast, "cave").expect("compile failed");
        let output = generate_ftl(&graph, "cave");

        assert!(
            output.contains("## label: start"),
            "label section header must be present"
        );
        assert!(
            output.contains("# @fluent variables: none"),
            "label with no fluent vars must say 'none'"
        );
        assert!(
            output.contains("cave-start-line_1 = The wind howls."),
            "dialogue line must produce a message entry"
        );
    }

    #[test]
    fn multiline_dialogue_with_interpolation() {
        let ps_interp = ParsedString::new_from_parts(vec![
            StringPart::Literal("You found ".to_string()),
            StringPart::Interpolation(Interpolation {
                path: "gold".to_string(),
                format: None,
            }),
            StringPart::Literal(" gold.".to_string()),
        ]);
        let lines = Ast::expr_list(vec![
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("Line one."))),
            Ast::value(RuntimeValue::Str(ps_interp)),
        ]);
        let dialogue = Ast::dialogue(narrator(), lines);
        let ast = Ast::block(vec![Ast::labeled_block(
            "start".to_string(),
            Ast::block(vec![dialogue]),
        )]);

        let graph = Compiler::compile_named(&ast, "cave").expect("compile failed");
        let output = generate_ftl(&graph, "cave");

        assert!(
            output.contains("# interpolation: $gold"),
            "interpolation comment must list every used variable"
        );
        assert!(
            output.contains("cave-start-line_1 = Line one.\n    You found { $gold } gold."),
            "multi-line FTL value must use the continuation-indent convention"
        );
    }

    #[test]
    fn choice_with_options_renders_menu_block() {
        let menu = Ast::menu(vec![
            Ast::menu_option("Enter the cave".to_string(), Ast::block(vec![]), false),
            Ast::menu_option("Walk away".to_string(), Ast::block(vec![]), false),
        ]);
        let ast = Ast::block(vec![Ast::labeled_block(
            "start".to_string(),
            Ast::block(vec![menu]),
        )]);

        let graph = Compiler::compile_named(&ast, "cave").expect("compile failed");
        let output = generate_ftl(&graph, "cave");

        assert!(
            output.contains("# menu: cave-start-menu_1"),
            "choice node must emit a menu comment"
        );
        assert!(
            output.contains("cave-start-menu_1-enter_the_cave = Enter the cave"),
            "first option must have its own message entry"
        );
        assert!(
            output.contains("cave-start-menu_1-walk_away = Walk away"),
            "second option must have its own message entry"
        );
    }

    #[test]
    fn global_fluent_var_appears_in_label_comment() {
        // A global @fluent-tagged variable must show up in every label's
        // "# @fluent variables:" annotation.
        let gold_decl = Ast::decl(
            DeclKind::Global,
            Ast::value(RuntimeValue::IdentPath(vec!["gold".into()])),
            Ast::value(RuntimeValue::Int(0)),
        )
        .with_decorators(vec![Decorator::bare("fluent".to_string())]);

        let dialogue = Ast::dialogue(narrator(), str_line("Hello."));
        let ast = Ast::block(vec![
            gold_decl,
            Ast::labeled_block("start".to_string(), Ast::block(vec![dialogue])),
        ]);

        let graph = Compiler::compile_named(&ast, "cave").expect("compile failed");
        let output = generate_ftl(&graph, "cave");

        assert!(
            output.contains("# @fluent variables: $gold (urd: gold)"),
            "global @fluent var must appear in the label variable comment"
        );
    }

    #[test]
    fn multi_label_ordering_matches_source_order() {
        // scene_a compiles first → lower NodeIndex → must appear before scene_b
        // in the FTL output regardless of HashMap iteration order.
        let dialogue_a = Ast::dialogue(narrator(), str_line("Scene A."));
        let dialogue_b = Ast::dialogue(narrator(), str_line("Scene B."));

        let ast = Ast::block(vec![
            Ast::labeled_block(
                "scene_a".to_string(),
                Ast::block(vec![
                    dialogue_a,
                    Ast::jump_stmt("scene_b".to_string(), false),
                ]),
            ),
            Ast::labeled_block("scene_b".to_string(), Ast::block(vec![dialogue_b])),
        ]);

        let graph = Compiler::compile_named(&ast, "story").expect("compile failed");
        let output = generate_ftl(&graph, "story");

        let pos_a = output
            .find("## label: scene_a")
            .expect("scene_a section not found");
        let pos_b = output
            .find("## label: scene_b")
            .expect("scene_b section not found");
        assert!(
            pos_a < pos_b,
            "scene_a (compiled first) must precede scene_b in the output"
        );
        assert!(
            output.contains("story-scene_a-line_1 = Scene A."),
            "scene_a dialogue entry must be present"
        );
        assert!(
            output.contains("story-scene_b-line_1 = Scene B."),
            "scene_b dialogue entry must be present"
        );
    }

    #[test]
    fn menu_option_interpolation_converted_to_fluent_syntax() {
        // "Buy it for {price} gold" must become "Buy it for { $price } gold"
        // in the FTL output — the raw urd {var} brace form is not valid Fluent.
        let opt_buy = Ast::menu_option(
            "Buy it for {price} gold".to_string(),
            Ast::block(vec![]),
            false,
        );
        let opt_haggle = Ast::menu_option("Try to haggle".to_string(), Ast::block(vec![]), false);
        let menu = Ast::menu(vec![opt_buy, opt_haggle]);
        let ast = Ast::block(vec![Ast::labeled_block(
            "browse".to_string(),
            Ast::block(vec![menu]),
        )]);

        let graph = Compiler::compile_named(&ast, "shop").expect("compile failed");
        let output = generate_ftl(&graph, "shop");

        assert!(
            output.contains("{ $price }"),
            "interpolation in option label must be converted to Fluent {{ $var }} syntax; got:\n{output}"
        );
        assert!(
            !output.contains("{price}"),
            "raw urd-style {{price}} must not appear in FTL output; got:\n{output}"
        );
        assert!(
            output.contains("shop-browse-menu_1-try_to_haggle = Try to haggle"),
            "plain option label must be passed through unchanged"
        );
    }

    // ── Multi-file FTL generation tests ──────────────────────────────────────

    /// Helper: builds a two-module graph where `main.urd` imports `common.urd`.
    fn build_main_imports_common_graph() -> IrGraph {
        let mut loader = MemLoader::new();
        loader.add(
            "common.urd",
            r#"
@entry
label greet {
    narrator: "Hello from common!"
}
"#,
        );

        let main_src = r#"
import "common.urd" as common

label start {
    narrator: "Welcome."
    jump common.greet
}
"#;
        let ast = parse_source(main_src).expect("parse main.urd");
        compile_recursive_with_root_path(&ast, "main.urd", &loader).expect("compile")
    }

    #[test]
    fn multi_file_generate_ftl_includes_all_modules() {
        let graph = build_main_imports_common_graph();
        let output = generate_ftl(&graph, "main");

        assert!(
            output.contains("main-start-line_1"),
            "unfiltered output must include root module entry; got:\n{output}"
        );
        assert!(
            output.contains("common-greet-line_1"),
            "unfiltered output must include imported module entry; got:\n{output}"
        );
    }

    #[test]
    fn multi_file_generate_ftl_for_file_filters_root_only() {
        let graph = build_main_imports_common_graph();
        let output = generate_ftl_for_file(&graph, "main", "");

        assert!(
            output.contains("## label: start"),
            "root-filtered output must contain root label header; got:\n{output}"
        );
        assert!(
            output.contains("main-start-line_1"),
            "root-filtered output must contain root dialogue entry; got:\n{output}"
        );
        assert!(
            !output.contains("common-greet-line_1"),
            "root-filtered output must NOT contain imported module entry; got:\n{output}"
        );
        assert!(
            !output.contains("## label: common::greet"),
            "root-filtered output must NOT contain imported label header; got:\n{output}"
        );
    }

    #[test]
    fn multi_file_generate_ftl_for_file_filters_import_only() {
        let graph = build_main_imports_common_graph();
        let output = generate_ftl_for_file(&graph, "common", "common.urd");

        assert!(
            output.contains("common-greet-line_1"),
            "import-filtered output must contain imported module entry; got:\n{output}"
        );
        assert!(
            !output.contains("main-start-line_1"),
            "import-filtered output must NOT contain root module entry; got:\n{output}"
        );
    }

    #[test]
    fn multi_file_source_modules_returns_distinct_paths() {
        let graph = build_main_imports_common_graph();
        let modules = source_modules(&graph);

        assert_eq!(
            modules,
            vec!["".to_string(), "common.urd".to_string()],
            "source_modules must return sorted distinct paths; got: {:?}",
            modules
        );
    }

    #[test]
    fn single_file_generate_ftl_for_file_includes_all() {
        let dialogue_a = Ast::dialogue(narrator(), str_line("Scene A."));
        let dialogue_b = Ast::dialogue(narrator(), str_line("Scene B."));
        let ast = Ast::block(vec![
            Ast::labeled_block("scene_a".to_string(), Ast::block(vec![dialogue_a])),
            Ast::labeled_block("scene_b".to_string(), Ast::block(vec![dialogue_b])),
        ]);

        let graph = Compiler::compile_named(&ast, "test").expect("compile failed");
        let output = generate_ftl_for_file(&graph, "test", "");

        assert!(
            output.contains("test-scene_a-line_1 = Scene A."),
            "single-file filter must include all labels (scene_a); got:\n{output}"
        );
        assert!(
            output.contains("test-scene_b-line_1 = Scene B."),
            "single-file filter must include all labels (scene_b); got:\n{output}"
        );
    }

    #[test]
    fn diamond_import_no_duplicate_entries() {
        let mut loader = MemLoader::new();
        loader.add(
            "common.urd",
            r#"
@entry
label shared {
    narrator: "Shared content."
}
"#,
        );
        loader.add(
            "a.urd",
            r#"
import "common.urd" as common

@entry
label branch_a {
    narrator: "Branch A."
    jump common.shared
}
"#,
        );
        loader.add(
            "b.urd",
            r#"
import "common.urd" as common

@entry
label branch_b {
    narrator: "Branch B."
    jump common.shared
}
"#,
        );

        let root_src = r#"
import "a.urd" as a
import "b.urd" as b

label hub {
    narrator: "Hub."
    jump a.branch_a
}
"#;
        let ast = parse_source(root_src).expect("parse root");
        let graph = compile_recursive_with_root_path(&ast, "root.urd", &loader).expect("compile");

        // Root-only output must NOT contain common::shared entries.
        let root_output = generate_ftl_for_file(&graph, "root", "");
        assert!(
            root_output.contains("root-hub-line_1"),
            "root output must contain its own label; got:\n{root_output}"
        );
        assert!(
            !root_output.contains("common-shared"),
            "root output must NOT contain common module entries; got:\n{root_output}"
        );

        // Common-only output must contain the shared entry exactly once.
        let common_output = generate_ftl_for_file(&graph, "common", "common.urd");
        assert!(
            common_output.contains("common-shared-line_1"),
            "common output must contain its shared entry; got:\n{common_output}"
        );
        // Verify no duplication: count occurrences of the entry key.
        let count = common_output.matches("common-shared-line_1").count();
        assert_eq!(
            count, 1,
            "common-shared-line_1 must appear exactly once (no diamond duplication); found {count}"
        );
    }
}
