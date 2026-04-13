//! # Cross-file import helpers
//!
//! Shared AST-walking utilities for collecting type definitions and label names
//! from imported modules.  Both `urd-quest` and `urd-lsp` need these when
//! resolving cross-file imports for static analysis; having a single
//! implementation in the core crate avoids duplication.

use std::collections::{HashMap, HashSet};

use crate::parser::ast::{Ast, AstContent, StructField};

// ---------------------------------------------------------------------------
// collect_type_defs_from_ast
// ---------------------------------------------------------------------------

/// Recursively walk `ast` and collect every [`AstContent::StructDecl`] and
/// [`AstContent::EnumDecl`] into the provided maps.
///
/// Each name is inserted under:
/// - a qualified key `"alias.Name"` — for references written as `chars.Character`
///   (only when `alias` is non-empty)
/// - an unqualified key `"Name"` — for bare references
///
/// Existing entries are never overwritten (first-write-wins), so caller order
/// determines precedence when multiple modules define a same-named type.
pub fn collect_type_defs_from_ast(
    ast: &Ast,
    alias: &str,
    structs: &mut HashMap<String, Vec<StructField>>,
    enums: &mut HashMap<String, Vec<String>>,
) {
    match ast.content() {
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_type_defs_from_ast(stmt, alias, structs, enums);
            }
        }
        AstContent::StructDecl { name, fields } => {
            // Store under qualified ("chars.Character") and unqualified
            // ("Character") keys, but never generate malformed ".Character".
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
            // Extract just the variant names (dropping source spans).
            // Avoid malformed ".Faction" when alias is empty.
            let variant_names: Vec<String> = variants.iter().map(|(n, _)| n.clone()).collect();
            if !alias.is_empty() {
                enums
                    .entry(format!("{alias}.{name}"))
                    .or_insert_with(|| variant_names.clone());
            }
            enums.entry(name.clone()).or_insert_with(|| variant_names);
        }
        AstContent::LabeledBlock { block, .. } => {
            collect_type_defs_from_ast(block, alias, structs, enums);
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// collect_label_names_from_ast
// ---------------------------------------------------------------------------

/// Collect every label name declared anywhere in `ast`.
///
/// Returns a set of plain label names (e.g. `"greet"`, `"shop"`).  This is
/// used by import resolution to decide whether a symbol imported from another
/// module refers to a label (as opposed to a variable, struct, etc.).
pub fn collect_label_names_from_ast(ast: &Ast) -> HashSet<String> {
    let mut labels = HashSet::new();
    collect_label_names_from_node(ast, &mut labels);
    labels
}

fn collect_label_names_from_node(ast: &Ast, labels: &mut HashSet<String>) {
    match ast.content() {
        AstContent::LabeledBlock { label, block, .. } => {
            labels.insert(label.clone());
            collect_label_names_from_node(block, labels);
        }
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_label_names_from_node(stmt, labels);
            }
        }
        AstContent::If {
            then_block,
            else_block,
            ..
        } => {
            collect_label_names_from_node(then_block, labels);
            if let Some(else_block) = else_block {
                collect_label_names_from_node(else_block, labels);
            }
        }
        AstContent::Menu { options } => {
            for opt in options {
                if let AstContent::MenuOption { content, .. } = opt.content() {
                    collect_label_names_from_node(content, labels);
                }
            }
        }
        AstContent::Match { arms, .. } => {
            for arm in arms {
                collect_label_names_from_node(&arm.body, labels);
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    // ── collect_type_defs_from_ast ────────────────────────────────────────

    #[test]
    fn collects_top_level_struct() {
        let ast = parse("struct Character { name: str, level: num }");
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        collect_type_defs_from_ast(&ast, "", &mut structs, &mut enums);

        assert!(structs.contains_key("Character"));
        assert_eq!(structs.len(), 1, "no qualified key when alias is empty");
    }

    #[test]
    fn collects_qualified_and_unqualified_struct() {
        let ast = parse("struct Character { name: str }");
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        collect_type_defs_from_ast(&ast, "chars", &mut structs, &mut enums);

        assert!(structs.contains_key("Character"));
        assert!(structs.contains_key("chars.Character"));
    }

    #[test]
    fn collects_top_level_enum() {
        let ast = parse("enum Faction { Guild, Empire, Rebels }");
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        collect_type_defs_from_ast(&ast, "", &mut structs, &mut enums);

        assert!(enums.contains_key("Faction"));
        let variants = &enums["Faction"];
        assert_eq!(variants, &["Guild", "Empire", "Rebels"]);
    }

    #[test]
    fn collects_qualified_and_unqualified_enum() {
        let ast = parse("enum Faction { Guild, Empire }");
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        collect_type_defs_from_ast(&ast, "world", &mut structs, &mut enums);

        assert!(enums.contains_key("Faction"));
        assert!(enums.contains_key("world.Faction"));
    }

    #[test]
    fn collects_types_inside_labeled_blocks() {
        let ast = parse(
            r#"
label setup {
    struct Weapon { damage: num }
    enum Element { Fire, Ice }
}
"#,
        );
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        collect_type_defs_from_ast(&ast, "gear", &mut structs, &mut enums);

        assert!(structs.contains_key("Weapon"));
        assert!(structs.contains_key("gear.Weapon"));
        assert!(enums.contains_key("Element"));
        assert!(enums.contains_key("gear.Element"));
    }

    #[test]
    fn first_write_wins() {
        let ast1 = parse("struct Item { a: str }");
        let ast2 = parse("struct Item { b: num }");
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        collect_type_defs_from_ast(&ast1, "", &mut structs, &mut enums);
        collect_type_defs_from_ast(&ast2, "", &mut structs, &mut enums);

        let fields = &structs["Item"];
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, "a", "first definition should win");
    }

    // ── collect_label_names_from_ast ──────────────────────────────────────

    #[test]
    fn collects_top_level_labels() {
        let ast = parse(
            r#"
label greet {
    end!()
}
label farewell {
    end!()
}
"#,
        );
        let labels = collect_label_names_from_ast(&ast);
        assert!(labels.contains("greet"));
        assert!(labels.contains("farewell"));
    }

    #[test]
    fn does_not_collect_non_label_names() {
        let ast = parse(
            r#"
const answer = 42
struct Hero { name: str }
enum Side { Good, Evil }
"#,
        );
        let labels = collect_label_names_from_ast(&ast);
        assert!(labels.is_empty());
    }

    #[test]
    fn collects_nested_labels() {
        let ast = parse(
            r#"
label outer {
    label inner {
        end!()
    }
    end!()
}
"#,
        );
        let labels = collect_label_names_from_ast(&ast);
        assert!(labels.contains("outer"));
        assert!(labels.contains("inner"));
    }
}
