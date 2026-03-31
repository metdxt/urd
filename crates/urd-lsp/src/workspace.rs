//! Workspace-level multi-file index for the LSP.
//!
//! When a `.urd` file is opened or changed, [`WorkspaceIndex`] scans its AST
//! for `import` statements, loads each imported file from disk, parses it, and
//! caches the resulting symbols so that cross-file LSP features
//! (go-to-definition, hover, completion, find-references) work across the whole
//! workspace.
//!
//! Only **direct** imports (one level deep) are resolved per file. That is
//! sufficient to cover qualified references like `alias.label` that appear in
//! the importing file.  Transitive imports (A → B → C, used from A) can be
//! layered on top later by walking the index recursively.

use std::path::PathBuf;

use chumsky::span::{SimpleSpan, Span as _};
use dashmap::DashMap;
use tower_lsp::lsp_types::{Location, Url};

use urd::compiler::loader::parse_source;
use urd::parser::ast::{Ast, AstContent};

use crate::document::byte_span_to_lsp_range;
use crate::semantic::{Symbol, collect_symbols};

// ── ImportedModule ────────────────────────────────────────────────────────────

/// Everything the LSP knows about a single module that was imported by one file.
#[derive(Debug, Clone)]
pub struct ImportedModule {
    /// The local alias written in source (`import "foo.urd" as alias` → `"alias"`).
    pub alias: String,
    /// The raw import path string as written in source (e.g. `"helpers.urd"`).
    pub _path: String,
    /// Resolved `file://` URI of the imported file (`None` if the file could
    /// not be found or if the importer itself has no file URI).
    pub uri: Option<Url>,
    /// Full source text of the imported file (empty on load failure).
    pub source: String,
    /// Symbols defined in the imported file, with their original (unaliased) names.
    pub symbols: Vec<Symbol>,
    /// Parsed AST of the imported file (`None` on load / parse failure).
    pub ast: Option<Ast>,
    /// For symbol imports (`import sym as alias from "path"`): the original name
    /// of the single symbol to expose.  `None` means whole-module import (expose
    /// all symbols prefixed with `alias`).
    pub symbol_filter: Option<String>,
}

impl ImportedModule {
    /// Return symbols visible to the importing file.
    ///
    /// - **Whole-module import** (`symbol_filter == None`): every symbol is
    ///   exposed with its name prefixed by the module alias, e.g. `helpers.greet`.
    /// - **Symbol import** (`symbol_filter == Some(orig_name)`): only the one
    ///   matching symbol is exposed, under `self.alias` directly (no prefix),
    ///   e.g. `import greet as hello from "helpers.urd"` → visible as `hello`.
    pub fn aliased_symbols(&self) -> Vec<Symbol> {
        match &self.symbol_filter {
            None => {
                // Whole-module import: prefix every symbol with the alias.
                self.symbols
                    .iter()
                    .map(|s| Symbol {
                        name: format!("{}.{}", self.alias, s.name),
                        span: chumsky::span::SimpleSpan::new((), 0..0),
                        ..s.clone()
                    })
                    .collect()
            }
            Some(orig_name) => {
                // Symbol import: expose only the matching symbol under its alias.
                self.symbols
                    .iter()
                    .filter(|s| &s.name == orig_name)
                    .map(|s| Symbol {
                        name: self.alias.clone(),
                        span: chumsky::span::SimpleSpan::new((), 0..0),
                        ..s.clone()
                    })
                    .collect()
            }
        }
    }
}

// ── WorkspaceIndex ────────────────────────────────────────────────────────────

/// A concurrent, per-file cache of imported modules.
///
/// The map is keyed by the *importer*'s [`Url`]. Each entry is the list of
/// [`ImportedModule`]s that file directly imports.
///
/// All public methods take `&self` (not `&mut self`) because [`DashMap`]
/// provides interior mutability, making the index safe to share across async
/// tasks via `Arc`.
#[derive(Debug, Default)]
pub struct WorkspaceIndex {
    /// `importer_uri` → list of modules it directly imports.
    imports: DashMap<Url, Vec<ImportedModule>>,
}

impl WorkspaceIndex {
    /// Create an empty index.
    pub fn new() -> Self {
        WorkspaceIndex::default()
    }

    /// Re-index the imports of the file at `uri`.
    ///
    /// Walks `ast` for `Import` nodes, resolves each path relative to `uri`'s
    /// directory, reads the file from disk, parses it, and caches the result.
    /// Imports that fail to load or parse are stored with empty symbol lists so
    /// we don't panic or retry loudly on every keystroke.
    pub fn update(&self, uri: &Url, ast: &Ast) {
        let base_dir = uri_to_dir(uri);
        let mut modules = Vec::new();
        collect_imports_from_ast(ast, &base_dir, &mut modules);
        self.imports.insert(uri.clone(), modules);
    }

    /// Remove all index entries for `uri` (call this on `did_close`).
    pub fn remove(&self, uri: &Url) {
        self.imports.remove(uri);
    }

    /// Return all alias-prefixed symbols visible from `uri` through its direct
    /// imports (e.g. `helpers.greet`, `lib.Player`).
    ///
    /// Suitable for injecting into completion lists and for cross-file hover
    /// fallback.
    pub fn imported_symbols(&self, uri: &Url) -> Vec<Symbol> {
        match self.imports.get(uri) {
            Some(modules) => modules.iter().flat_map(|m| m.aliased_symbols()).collect(),
            None => Vec::new(),
        }
    }

    /// Try to locate the *definition* of `name` in any module directly imported
    /// by `uri`.
    ///
    /// `name` can be:
    /// - **Qualified** (`"alias.label_name"`) — only the matching alias is searched.
    /// - **Unqualified** (`"label_name"`) — every imported module is searched in
    ///   order; the first match wins.
    ///
    /// Returns `(target_uri, span_in_that_file, source_of_that_file)` on success.
    pub fn find_definition(&self, uri: &Url, name: &str) -> Option<(Url, SimpleSpan, String)> {
        let modules = self.imports.get(uri)?;

        if let Some(dot) = name.find('.') {
            // ── Qualified lookup: "alias.local_name" ─────────────────────
            let alias = &name[..dot];
            let local_name = &name[dot + 1..];

            for module in modules.iter() {
                if module.alias != alias {
                    continue;
                }
                if let Some(ast) = &module.ast
                    && let Some(span) = crate::semantic::find_definition(ast, local_name)
                {
                    let target_uri = module.uri.clone().unwrap_or_else(|| uri.clone());
                    return Some((target_uri, span, module.source.clone()));
                }
            }
        } else {
            // ── Unqualified lookup: search every imported module ──────────
            for module in modules.iter() {
                if let Some(ast) = &module.ast
                    && let Some(span) = crate::semantic::find_definition(ast, name)
                {
                    let target_uri = module.uri.clone().unwrap_or_else(|| uri.clone());
                    return Some((target_uri, span, module.source.clone()));
                }
            }
        }

        None
    }

    /// Find all references to `name` that exist in modules directly imported by
    /// `uri`, returning them as LSP [`Location`]s.
    ///
    /// If `name` is qualified (`"alias.label"`), only the matching module is
    /// searched and the bare local name is used for the reference query.
    pub fn find_references(&self, uri: &Url, name: &str) -> Vec<Location> {
        let modules = match self.imports.get(uri) {
            Some(m) => m,
            None => return Vec::new(),
        };

        let (target_alias, local_name) = if let Some(dot) = name.find('.') {
            (Some(&name[..dot]), &name[dot + 1..])
        } else {
            (None, name)
        };

        let mut locations = Vec::new();

        for module in modules.iter() {
            if let Some(alias) = target_alias
                && module.alias != alias
            {
                continue;
            }

            let ast = match &module.ast {
                Some(a) => a,
                None => continue,
            };
            let target_uri = match &module.uri {
                Some(u) => u.clone(),
                None => continue,
            };

            for span in crate::semantic::find_references(ast, local_name) {
                let range = byte_span_to_lsp_range(&module.source, span);
                locations.push(Location::new(target_uri.clone(), range));
            }
        }

        locations
    }

    /// Look up the hover text for a qualified reference `alias.name` in the
    /// modules imported by `uri`.
    ///
    /// Returns the markdown hover string produced by the standard
    /// [`crate::semantic::hover_info`] function, run against the imported
    /// module's AST, or `None` if nothing is found.
    pub fn hover_info(&self, uri: &Url, name: &str) -> Option<String> {
        let modules = self.imports.get(uri)?;

        let (alias_opt, local_name) = if let Some(dot) = name.find('.') {
            (Some(&name[..dot]), &name[dot + 1..])
        } else {
            (None, name)
        };

        for module in modules.iter() {
            if let Some(alias) = alias_opt
                && module.alias != alias
            {
                continue;
            }
            if let Some(ast) = &module.ast {
                let symbols = collect_symbols(ast);
                // We don't have a byte offset here, so use find_definition to
                // get the span and then locate the *name* inside it (to avoid
                // landing on a keyword like `label` which hover_info now
                // suppresses).
                if let Some(span) = crate::semantic::find_definition(ast, local_name) {
                    let name_offset = module.source[span.start..span.end]
                        .find(local_name)
                        .map(|rel| span.start + rel)
                        .unwrap_or(span.start);
                    if let Some(md) =
                        crate::semantic::hover_info(ast, &symbols, &module.source, name_offset)
                    {
                        return Some(md);
                    }
                }
            }
        }

        None
    }

    /// Return the list of directly imported modules for `uri`, if the index
    /// has an entry for it.
    pub fn _modules_for(
        &self,
        uri: &Url,
    ) -> Option<dashmap::mapref::one::Ref<'_, Url, Vec<ImportedModule>>> {
        self.imports.get(uri)
    }

    /// Return imported struct and enum definitions for use in cross-file analysis.
    ///
    /// Returns two maps:
    /// - `imported_structs`: `"alias.StructName"` → field list
    /// - `imported_enums`: `"alias.EnumName"` → variant list
    ///
    /// Both qualified (`"chars.Character"`) and unqualified (`"Character"`) forms
    /// are included so the type checker can resolve either spelling.
    /// Return imported struct, enum, and label definitions for use in cross-file
    /// analysis.
    ///
    /// Returns three values:
    /// - `imported_structs`: `"alias.StructName"` → field list
    /// - `imported_enums`: `"alias.EnumName"` → variant list
    /// - `imported_labels`: set of label names directly imported into scope
    ///   without a qualifier (e.g. `"show_inventory"` from
    ///   `import (show_inventory) from "items.urd"`)
    ///
    /// Both qualified (`"chars.Character"`) and unqualified (`"Character"`) forms
    /// are included in the struct/enum maps so the type checker can resolve
    /// either spelling.
    pub fn imported_type_context(
        &self,
        uri: &Url,
    ) -> (
        std::collections::HashMap<String, Vec<urd::parser::ast::StructField>>,
        std::collections::HashMap<String, Vec<String>>,
        std::collections::HashSet<String>,
    ) {
        let mut structs = std::collections::HashMap::new();
        let mut enums = std::collections::HashMap::new();
        let mut labels = std::collections::HashSet::new();

        let modules = match self.imports.get(uri) {
            Some(m) => m,
            None => return (structs, enums, labels),
        };

        for module in modules.iter() {
            let alias = &module.alias;
            if let Some(ast) = &module.ast {
                collect_type_defs_from_ast(ast, alias, &mut structs, &mut enums);
            }

            // Collect directly-imported label names (symbol imports without a
            // module-qualifier prefix, e.g. `import (show_inventory) from "items.urd"`).
            // `symbol_filter == Some(_)` means this entry is a single-symbol import;
            // the alias is what the importing file actually uses as the label name.
            if module.symbol_filter.is_some() {
                for sym in module.aliased_symbols() {
                    if sym.kind == crate::semantic::SymbolKind::Label {
                        labels.insert(sym.name.clone());
                    }
                }
            }
        }

        (structs, enums, labels)
    }
}

// ── Type-definition collection (cross-file analysis) ─────────────────────────

/// Recursively walk `ast` and collect every `StructDecl` and `EnumDecl` into
/// the provided maps.
///
/// Each name is inserted under two keys:
/// - a qualified key `"alias.Name"` — for references written as `chars.Character`
/// - an unqualified key `"Name"` — for bare references
///
/// Existing entries are never overwritten (first-write-wins), so caller order
/// determines precedence when multiple modules define a same-named type.
fn collect_type_defs_from_ast(
    ast: &Ast,
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
            // Store under both qualified ("chars.Character") and unqualified ("Character") keys.
            let qualified = format!("{alias}.{name}");
            structs.entry(qualified).or_insert_with(|| fields.clone());
            structs
                .entry(name.clone())
                .or_insert_with(|| fields.clone());
        }
        AstContent::EnumDecl { name, variants } => {
            // `variants` is already a `Vec<String>` — just clone it directly.
            let qualified = format!("{alias}.{name}");
            enums.entry(qualified).or_insert_with(|| variants.clone());
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

// ── Import collection (AST walk) ─────────────────────────────────────────────

/// Recursively walk `ast` collecting every `Import` node and loading the
/// referenced file.  Results are appended to `out`.
fn collect_imports_from_ast(ast: &Ast, base_dir: &Option<PathBuf>, out: &mut Vec<ImportedModule>) {
    match ast.content() {
        // Top-level block — walk every statement.
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_imports_from_ast(stmt, base_dir, out);
            }
        }

        // The import statement itself — load and cache the module.
        AstContent::Import { path, symbols } => {
            for entry in symbols {
                // `entry.original == None`  →  whole-module import
                // `entry.original == Some(name)` →  single-symbol import
                out.push(load_module(
                    path,
                    &entry.alias,
                    entry.original.as_deref(),
                    base_dir,
                ));
            }
        }

        // Defensively recurse into compound nodes in case imports appear
        // inside conditional or menu blocks (the compiler permits it).
        AstContent::If {
            then_block,
            else_block,
            ..
        } => {
            collect_imports_from_ast(then_block, base_dir, out);
            if let Some(eb) = else_block {
                collect_imports_from_ast(eb, base_dir, out);
            }
        }
        AstContent::LabeledBlock { block, .. } => {
            collect_imports_from_ast(block, base_dir, out);
        }
        AstContent::Menu { options } => {
            for opt in options {
                collect_imports_from_ast(opt, base_dir, out);
            }
        }
        AstContent::MenuOption { content, .. } => {
            collect_imports_from_ast(content, base_dir, out);
        }
        AstContent::Match { arms, .. } => {
            for arm in arms {
                collect_imports_from_ast(&arm.body, base_dir, out);
            }
        }
        AstContent::DecoratorDef { body, .. } => {
            collect_imports_from_ast(body, base_dir, out);
        }

        // All other node types cannot contain import statements.
        _ => {}
    }
}

/// Load and parse a single module at `path` relative to `base_dir`.
///
/// Never panics — a load or parse failure produces an `ImportedModule` with
/// empty `symbols` and `ast: None`.
fn load_module(
    path: &str,
    alias: &str,
    symbol_filter: Option<&str>,
    base_dir: &Option<PathBuf>,
) -> ImportedModule {
    let full_path = match base_dir {
        Some(dir) => dir.join(path),
        None => PathBuf::from(path),
    };

    let uri = path_to_uri(&full_path);

    let source = match std::fs::read_to_string(&full_path) {
        Ok(s) => s,
        Err(_) => {
            // File not found or unreadable — return an empty shell so the
            // rest of the index stays consistent.
            return ImportedModule {
                alias: alias.to_owned(),
                _path: path.to_owned(),
                uri,
                source: String::new(),
                symbols: Vec::new(),
                ast: None,
                symbol_filter: symbol_filter.map(str::to_owned),
            };
        }
    };

    let ast = parse_source(&source).ok();
    let symbols = ast.as_ref().map(collect_symbols).unwrap_or_default();

    ImportedModule {
        alias: alias.to_owned(),
        _path: path.to_owned(),
        uri,
        source,
        symbols,
        ast,
        symbol_filter: symbol_filter.map(str::to_owned),
    }
}

// ── URI / path helpers ────────────────────────────────────────────────────────

/// Extract the parent directory from a `file://` URI.
///
/// Returns `None` for non-`file://` URIs (e.g. `untitled:`) or for URIs whose
/// path has no parent component.
fn uri_to_dir(uri: &Url) -> Option<PathBuf> {
    uri.to_file_path()
        .ok()
        .and_then(|p| p.parent().map(|d| d.to_path_buf()))
}

/// Convert a filesystem path to a `file://` [`Url`].
///
/// Returns `None` if the path is not absolute (which would produce an invalid
/// file URI).
fn path_to_uri(path: &PathBuf) -> Option<Url> {
    Url::from_file_path(path).ok()
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use std::io::Write;

    // ── Helpers ───────────────────────────────────────────────────────────

    /// Write `content` to a temporary file inside `dir` and return its URI.
    fn write_tmp(dir: &std::path::Path, filename: &str, content: &str) -> Url {
        let path = dir.join(filename);
        let mut f = std::fs::File::create(&path).unwrap();
        write!(f, "{content}").unwrap();
        Url::from_file_path(&path).unwrap()
    }

    /// Create a fresh temporary directory that is automatically cleaned up.
    /// Returns the path; callers must keep it alive for the duration of the test.
    fn tmp_dir() -> PathBuf {
        let base = std::env::temp_dir().join(format!(
            "urd_lsp_workspace_test_{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        std::fs::create_dir_all(&base).unwrap();
        base
    }

    // ── update / imported_symbols ─────────────────────────────────────────

    #[test]
    fn update_indexes_imported_symbols() {
        let dir = tmp_dir();

        write_tmp(&dir, "helpers.urd", "label greet {\n  end!()\n}\n");
        let main_uri = write_tmp(
            &dir,
            "main.urd",
            "import \"helpers.urd\" as helpers\nlabel start {\n  jump helpers.greet\n}\n",
        );

        let main_src = std::fs::read_to_string(main_uri.to_file_path().unwrap()).unwrap();
        let main_ast = parse_source(&main_src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&main_uri, &main_ast);

        let imported = index.imported_symbols(&main_uri);
        assert!(
            imported.iter().any(|s| s.name == "helpers.greet"),
            "expected 'helpers.greet', got: {:?}",
            imported.iter().map(|s| &s.name).collect::<Vec<_>>()
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn update_with_no_imports_produces_empty_list() {
        let dir = tmp_dir();

        let uri = write_tmp(&dir, "solo.urd", "label start {\n  end!()\n}\n");
        let src = std::fs::read_to_string(uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&uri, &ast);

        assert!(index.imported_symbols(&uri).is_empty());

        let _ = std::fs::remove_dir_all(&dir);
    }

    // ── find_definition ───────────────────────────────────────────────────

    #[test]
    fn find_definition_label_in_imported_module() {
        let dir = tmp_dir();

        // items.urd defines a label `show_inventory` (with some globals before it,
        // mirroring the real items.urd structure).
        write_tmp(
            &dir,
            "items.urd",
            "global gold: int = 100\nlabel show_inventory {\n  end!()\n}\n",
        );

        // village.urd imports items.urd as inv and jumps to inv.show_inventory.
        let village_uri = write_tmp(
            &dir,
            "village.urd",
            "import \"items.urd\" as inv\nlabel village_farewell {\n  jump inv.show_inventory\n}\n",
        );

        let village_src = std::fs::read_to_string(village_uri.to_file_path().unwrap()).unwrap();
        let village_ast = parse_source(&village_src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&village_uri, &village_ast);

        let result = index.find_definition(&village_uri, "inv.show_inventory");
        assert!(
            result.is_some(),
            "expected to resolve 'inv.show_inventory' to items.urd, got None"
        );

        let (target_uri, _span, _src) = result.unwrap();
        assert!(
            target_uri.to_file_path().unwrap().ends_with("items.urd"),
            "expected target URI to point at items.urd, got: {target_uri}"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_definition_qualified_resolves_to_correct_file() {
        let dir = tmp_dir();

        write_tmp(&dir, "lib.urd", "label intro {\n  end!()\n}\n");
        let main_uri = write_tmp(
            &dir,
            "main.urd",
            "import \"lib.urd\" as lib\nlabel start {\n  jump lib.intro\n}\n",
        );

        let src = std::fs::read_to_string(main_uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&main_uri, &ast);

        let result = index.find_definition(&main_uri, "lib.intro");
        assert!(result.is_some(), "expected to find 'lib.intro'");

        let (target_uri, _span, _src) = result.unwrap();
        let expected = Url::from_file_path(dir.join("lib.urd")).unwrap();
        assert_eq!(target_uri, expected);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_definition_unqualified_searches_all_modules() {
        let dir = tmp_dir();

        write_tmp(&dir, "util.urd", "label helper {\n  end!()\n}\n");
        let main_uri = write_tmp(
            &dir,
            "main.urd",
            "import \"util.urd\" as util\nlabel start {\n  end!()\n}\n",
        );

        let src = std::fs::read_to_string(main_uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&main_uri, &ast);

        let result = index.find_definition(&main_uri, "helper");
        assert!(result.is_some(), "expected to find unqualified 'helper'");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_definition_missing_returns_none() {
        let dir = tmp_dir();

        let uri = write_tmp(&dir, "main.urd", "label start {\n  end!()\n}\n");
        let src = std::fs::read_to_string(uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&uri, &ast);

        assert!(index.find_definition(&uri, "ghost").is_none());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_definition_wrong_alias_returns_none() {
        let dir = tmp_dir();

        write_tmp(&dir, "lib.urd", "label foo {\n  end!()\n}\n");
        let main_uri = write_tmp(
            &dir,
            "main.urd",
            "import \"lib.urd\" as lib\nlabel start {\n  end!()\n}\n",
        );

        let src = std::fs::read_to_string(main_uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&main_uri, &ast);

        // "wrong.foo" uses a different alias — should not match "lib".
        assert!(index.find_definition(&main_uri, "wrong.foo").is_none());

        let _ = std::fs::remove_dir_all(&dir);
    }

    // ── find_references ───────────────────────────────────────────────────

    #[test]
    fn find_references_qualified_returns_locations_in_module() {
        let dir = tmp_dir();

        write_tmp(
            &dir,
            "chars.urd",
            "label greet {\n  end!()\n}\nlabel farewell {\n  jump greet\n}\n",
        );
        let main_uri = write_tmp(
            &dir,
            "main.urd",
            "import \"chars.urd\" as chars\nlabel start {\n  end!()\n}\n",
        );

        let src = std::fs::read_to_string(main_uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&main_uri, &ast);

        let refs = index.find_references(&main_uri, "chars.greet");
        assert!(
            !refs.is_empty(),
            "expected at least one reference to 'greet' in chars module"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    // ── remove ────────────────────────────────────────────────────────────

    #[test]
    fn remove_clears_cached_imports() {
        let dir = tmp_dir();

        write_tmp(&dir, "mod.urd", "label x {\n  end!()\n}\n");
        let main_uri = write_tmp(
            &dir,
            "main.urd",
            "import \"mod.urd\" as m\nlabel start {\n  end!()\n}\n",
        );

        let src = std::fs::read_to_string(main_uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&main_uri, &ast);

        assert!(!index.imported_symbols(&main_uri).is_empty());

        index.remove(&main_uri);

        assert!(index.imported_symbols(&main_uri).is_empty());

        let _ = std::fs::remove_dir_all(&dir);
    }

    // ── load failure ──────────────────────────────────────────────────────

    #[test]
    fn failed_import_load_produces_empty_module_without_panic() {
        let dir = tmp_dir();

        let uri = write_tmp(
            &dir,
            "main.urd",
            "import \"does_not_exist.urd\" as ghost\nlabel start {\n  end!()\n}\n",
        );

        let src = std::fs::read_to_string(uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&uri, &ast); // must not panic

        assert!(
            index.imported_symbols(&uri).is_empty(),
            "no symbols expected from a failed import"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    // ── hover_info ────────────────────────────────────────────────────────

    #[test]
    fn hover_info_qualified_returns_some() {
        let dir = tmp_dir();

        write_tmp(&dir, "npc.urd", "label talk {\n  end!()\n}\n");
        let main_uri = write_tmp(
            &dir,
            "main.urd",
            "import \"npc.urd\" as npc\nlabel start {\n  jump npc.talk\n}\n",
        );

        let src = std::fs::read_to_string(main_uri.to_file_path().unwrap()).unwrap();
        let ast = parse_source(&src).unwrap();

        let index = WorkspaceIndex::new();
        index.update(&main_uri, &ast);

        let hover = index.hover_info(&main_uri, "npc.talk");
        assert!(hover.is_some(), "expected hover info for 'npc.talk'");

        let _ = std::fs::remove_dir_all(&dir);
    }
}
