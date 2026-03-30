//! Semantic queries over Urd ASTs for the LSP.
//!
//! Provides symbol collection, hover info, goto-definition, completions,
//! find-references, and semantic token generation.

use chumsky::span::{SimpleSpan, Span as _};

use urd::parser::ast::{Ast, AstContent, DeclKind, EventConstraint, MatchPattern, TypeAnnotation};
use urd::runtime::value::RuntimeValue;

// ── Symbol types ─────────────────────────────────────────────────────────────

/// The kind of a named symbol found in an Urd AST.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    /// A `label name { }` block.
    Label,
    /// A `let` variable.
    Variable,
    /// A `const` binding.
    Constant,
    /// A `global` variable.
    Global,
    /// An `enum Name { ... }` declaration.
    Enum,
    /// A variant inside an enum.
    EnumVariant,
    /// A `struct Name { ... }` declaration.
    Struct,
    /// A `decorator name(...) { ... }` definition.
    Decorator,
    /// An `import "..." as name` statement.
    Import,
}

impl std::fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolKind::Label => write!(f, "label"),
            SymbolKind::Variable => write!(f, "variable"),
            SymbolKind::Constant => write!(f, "constant"),
            SymbolKind::Global => write!(f, "global"),
            SymbolKind::Enum => write!(f, "enum"),
            SymbolKind::EnumVariant => write!(f, "enum variant"),
            SymbolKind::Struct => write!(f, "struct"),
            SymbolKind::Decorator => write!(f, "decorator"),
            SymbolKind::Import => write!(f, "import"),
        }
    }
}

/// A named symbol extracted from the AST.
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// The symbol's name (e.g. variable name, label name, enum name).
    pub name: String,
    /// What kind of symbol this is.
    pub kind: SymbolKind,
    /// Byte-offset span of the defining node in the source.
    pub span: SimpleSpan,
    /// Optional type annotation from the declaration.
    pub type_annotation: Option<TypeAnnotation>,
    /// Extra detail string for hover display (e.g. enum variants list).
    pub detail: Option<String>,
}

// ── Semantic token types ─────────────────────────────────────────────────────

/// The type assigned to a semantic token range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticTokenType {
    /// A label name.
    Label,
    /// A variable / identifier reference.
    Variable,
    /// A string literal.
    String,
    /// A numeric literal (int or float).
    Number,
    /// An operator (`+`, `-`, `==`, etc.).
    Operator,
    /// A keyword-like construct (`if`, `match`, `jump`, `return`, etc.).
    Keyword,
    /// An enum name or variant.
    EnumMember,
    /// A struct name.
    Struct,
    /// A decorator name or usage.
    #[allow(dead_code)]
    Decorator,
    /// A function / built-in call name.
    Function,
}

/// A single semantic-token range produced by [`semantic_tokens`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticTokenInfo {
    /// Byte offset of the token start.
    pub start: usize,
    /// Length of the token in bytes.
    pub length: usize,
    /// The semantic type to assign.
    pub token_type: SemanticTokenType,
}

// ── Helpers ──────────────────────────────────────────────────────────────────

/// Format a [`TypeAnnotation`] as a human-readable string.
fn format_type_annotation(ta: &TypeAnnotation) -> String {
    match ta {
        TypeAnnotation::Int => "int".into(),
        TypeAnnotation::Float => "float".into(),
        TypeAnnotation::Bool => "bool".into(),
        TypeAnnotation::Str => "str".into(),
        TypeAnnotation::Null => "null".into(),
        TypeAnnotation::List => "list".into(),
        TypeAnnotation::Map => "map".into(),
        TypeAnnotation::Dice => "dice".into(),
        TypeAnnotation::Label => "label".into(),
        TypeAnnotation::Named(parts) => parts.join("."),
    }
}

/// Format a [`DeclKind`] as a keyword string.
fn format_decl_kind(kind: &DeclKind) -> &'static str {
    match kind {
        DeclKind::Variable => "let",
        DeclKind::Constant => "const",
        DeclKind::Global => "global",
    }
}

/// Extract the identifier name from a `Value(IdentPath(...))` AST node,
/// returning the **last** segment (the leaf name).
fn ident_name_from_ast(ast: &Ast) -> Option<String> {
    if let AstContent::Value(RuntimeValue::IdentPath(parts)) = ast.content() {
        parts.last().cloned()
    } else {
        None
    }
}

/// Return the full ident path joined with `.` if the AST is an IdentPath value.
#[allow(dead_code)]
fn ident_path_from_ast(ast: &Ast) -> Option<String> {
    if let AstContent::Value(RuntimeValue::IdentPath(parts)) = ast.content() {
        if parts.is_empty() {
            None
        } else {
            Some(parts.join("."))
        }
    } else {
        None
    }
}

/// Check whether a byte offset falls inside a [`SimpleSpan`].
fn span_contains(span: SimpleSpan, offset: usize) -> bool {
    offset >= span.start && offset < span.end
}

// ── 1. collect_symbols ───────────────────────────────────────────────────────

/// Recursively walk the AST collecting all named definitions.
///
/// This powers document symbols, completion, and goto-definition. The
/// returned list contains labels, variables, constants, globals, enums (with
/// their variants), structs, decorator defs, and imports.
pub fn collect_symbols(ast: &Ast) -> Vec<Symbol> {
    let mut symbols = Vec::new();
    collect_symbols_recursive(ast, &mut symbols);
    symbols
}

fn collect_symbols_recursive(ast: &Ast, out: &mut Vec<Symbol>) {
    match ast.content() {
        // ── Declarations (let / const / global) ──────────────────────────
        AstContent::Declaration {
            kind,
            decl_name,
            type_annotation,
            decl_defs: _,
        } => {
            if let Some(name) = ident_name_from_ast(decl_name) {
                let sym_kind = match kind {
                    DeclKind::Variable => SymbolKind::Variable,
                    DeclKind::Constant => SymbolKind::Constant,
                    DeclKind::Global => SymbolKind::Global,
                };
                let detail = type_annotation.as_ref().map(|ta| {
                    format!(
                        "{} {}: {}",
                        format_decl_kind(kind),
                        name,
                        format_type_annotation(ta)
                    )
                });
                out.push(Symbol {
                    name,
                    kind: sym_kind,
                    span: ast.span(),
                    type_annotation: type_annotation.clone(),
                    detail,
                });
            }
            // Walk the definition expression (it may contain nested blocks).
            collect_symbols_recursive(decl_name, out);
            // decl_defs may contain nested blocks, lambdas, etc.
            walk_ast_content(ast.content(), out);
        }

        // ── Labeled blocks ───────────────────────────────────────────────
        AstContent::LabeledBlock { label, block } => {
            out.push(Symbol {
                name: label.clone(),
                kind: SymbolKind::Label,
                span: ast.span(),
                type_annotation: None,
                detail: Some(format!("label {label}")),
            });
            collect_symbols_recursive(block, out);
        }

        // ── Enum declarations ────────────────────────────────────────────
        AstContent::EnumDecl { name, variants } => {
            let variant_list = variants.join(", ");
            out.push(Symbol {
                name: name.clone(),
                kind: SymbolKind::Enum,
                span: ast.span(),
                type_annotation: None,
                detail: Some(format!("enum {name} {{ {variant_list} }}")),
            });
            // Each variant is also a symbol (for completion / references).
            for variant in variants {
                out.push(Symbol {
                    name: variant.clone(),
                    kind: SymbolKind::EnumVariant,
                    span: ast.span(),
                    type_annotation: None,
                    detail: Some(format!("{name}.{variant}")),
                });
            }
        }

        // ── Struct declarations ──────────────────────────────────────────
        AstContent::StructDecl { name, fields } => {
            let field_list: Vec<String> = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, format_type_annotation(&f.type_annotation)))
                .collect();
            out.push(Symbol {
                name: name.clone(),
                kind: SymbolKind::Struct,
                span: ast.span(),
                type_annotation: None,
                detail: Some(format!("struct {name} {{ {} }}", field_list.join(", "))),
            });
        }

        // ── Decorator definitions ────────────────────────────────────────
        AstContent::DecoratorDef {
            name,
            event_constraint,
            params,
            body,
        } => {
            let constraint_str = match event_constraint {
                EventConstraint::Dialogue => "<event: dialogue>",
                EventConstraint::Choice => "<event: choice>",
                EventConstraint::Any => "",
            };
            let param_list: Vec<String> = params
                .iter()
                .map(|p| {
                    if let Some(ta) = &p.type_annotation {
                        format!("{}: {}", p.name, format_type_annotation(ta))
                    } else {
                        p.name.clone()
                    }
                })
                .collect();
            out.push(Symbol {
                name: name.clone(),
                kind: SymbolKind::Decorator,
                span: ast.span(),
                type_annotation: None,
                detail: Some(format!(
                    "decorator {name}{constraint_str}({})",
                    param_list.join(", ")
                )),
            });
            collect_symbols_recursive(body, out);
        }

        // ── Imports ──────────────────────────────────────────────────────
        AstContent::Import { path, alias } => {
            out.push(Symbol {
                name: alias.clone(),
                kind: SymbolKind::Import,
                span: ast.span(),
                type_annotation: None,
                detail: Some(format!("import \"{path}\" as {alias}")),
            });
        }

        // ── LetCall (let name = jump label and return) ───────────────────
        AstContent::LetCall { name, target: _ } => {
            out.push(Symbol {
                name: name.clone(),
                kind: SymbolKind::Variable,
                span: ast.span(),
                type_annotation: None,
                detail: Some(format!("let {name} = jump ... and return")),
            });
        }

        // ── Everything else: just recurse into children ──────────────────
        _ => {
            walk_ast_content(ast.content(), out);
        }
    }
}

/// Walk the *children* of an [`AstContent`] node, without handling the node
/// itself (that's done in [`collect_symbols_recursive`]).
fn walk_ast_content(content: &AstContent, out: &mut Vec<Symbol>) {
    match content {
        AstContent::BinOp { left, right, .. } => {
            collect_symbols_recursive(left, out);
            collect_symbols_recursive(right, out);
        }
        AstContent::UnaryOp { expr, .. } => {
            collect_symbols_recursive(expr, out);
        }
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for s in stmts {
                collect_symbols_recursive(s, out);
            }
        }
        AstContent::Declaration { decl_defs, .. } => {
            collect_symbols_recursive(decl_defs, out);
        }
        AstContent::Call { func_path, params } => {
            collect_symbols_recursive(func_path, out);
            collect_symbols_recursive(params, out);
        }
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_symbols_recursive(condition, out);
            collect_symbols_recursive(then_block, out);
            if let Some(eb) = else_block {
                collect_symbols_recursive(eb, out);
            }
        }
        AstContent::Match { scrutinee, arms } => {
            collect_symbols_recursive(scrutinee, out);
            for arm in arms {
                if let MatchPattern::Value(v) = &arm.pattern {
                    collect_symbols_recursive(v, out);
                }
                collect_symbols_recursive(&arm.body, out);
            }
        }
        AstContent::Dialogue { speakers, content } => {
            collect_symbols_recursive(speakers, out);
            collect_symbols_recursive(content, out);
        }
        AstContent::Menu { options } => {
            for opt in options {
                collect_symbols_recursive(opt, out);
            }
        }
        AstContent::MenuOption { content, .. } => {
            collect_symbols_recursive(content, out);
        }
        AstContent::Return { value } => {
            if let Some(v) = value {
                collect_symbols_recursive(v, out);
            }
        }
        AstContent::Subscript { object, key } => {
            collect_symbols_recursive(object, out);
            collect_symbols_recursive(key, out);
        }
        AstContent::SubscriptAssign { object, key, value } => {
            collect_symbols_recursive(object, out);
            collect_symbols_recursive(key, out);
            collect_symbols_recursive(value, out);
        }
        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                collect_symbols_recursive(k, out);
                collect_symbols_recursive(v, out);
            }
        }
        // Leaves and nodes already fully handled in collect_symbols_recursive.
        AstContent::Value(_)
        | AstContent::Jump { .. }
        | AstContent::LabeledBlock { .. }
        | AstContent::EnumDecl { .. }
        | AstContent::StructDecl { .. }
        | AstContent::DecoratorDef { .. }
        | AstContent::Import { .. }
        | AstContent::LetCall { .. } => {}
    }
}

// ── 2. find_symbol_at_offset ─────────────────────────────────────────────────

/// Find the first symbol whose span contains the given byte offset.
pub fn find_symbol_at_offset(symbols: &[Symbol], byte_offset: usize) -> Option<&Symbol> {
    // Prefer narrower spans (more specific) when multiple symbols overlap.
    symbols
        .iter()
        .filter(|s| span_contains(s.span, byte_offset))
        .min_by_key(|s| s.span.end - s.span.start)
}

// ── 3. find_definition ───────────────────────────────────────────────────────

/// Search the AST for the *defining* occurrence of `name` and return its span.
///
/// Checks `Declaration`, `LabeledBlock`, `EnumDecl`, `StructDecl`,
/// `DecoratorDef`, `Import`, and `LetCall` nodes.
pub fn find_definition(ast: &Ast, name: &str) -> Option<SimpleSpan> {
    find_definition_recursive(ast, name)
}

fn find_definition_recursive(ast: &Ast, name: &str) -> Option<SimpleSpan> {
    match ast.content() {
        AstContent::Declaration { decl_name, .. } => {
            if ident_name_from_ast(decl_name).as_deref() == Some(name) {
                return Some(ast.span());
            }
            find_definition_in_children(ast.content(), name)
        }

        AstContent::LabeledBlock { label, block } => {
            if label == name {
                return Some(ast.span());
            }
            find_definition_recursive(block, name)
        }

        AstContent::EnumDecl {
            name: enum_name, ..
        } => {
            if enum_name == name {
                return Some(ast.span());
            }
            None
        }

        AstContent::StructDecl {
            name: struct_name, ..
        } => {
            if struct_name == name {
                return Some(ast.span());
            }
            None
        }

        AstContent::DecoratorDef {
            name: dec_name,
            body,
            ..
        } => {
            if dec_name == name {
                return Some(ast.span());
            }
            find_definition_recursive(body, name)
        }

        AstContent::Import { alias, .. } => {
            if alias == name {
                return Some(ast.span());
            }
            None
        }

        AstContent::LetCall { name: let_name, .. } => {
            if let_name == name {
                return Some(ast.span());
            }
            None
        }

        _ => find_definition_in_children(ast.content(), name),
    }
}

/// Walk children of a node looking for a definition of `name`.
fn find_definition_in_children(content: &AstContent, name: &str) -> Option<SimpleSpan> {
    match content {
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for s in stmts {
                if let Some(sp) = find_definition_recursive(s, name) {
                    return Some(sp);
                }
            }
            None
        }
        AstContent::BinOp { left, right, .. } => {
            find_definition_recursive(left, name).or_else(|| find_definition_recursive(right, name))
        }
        AstContent::UnaryOp { expr, .. } => find_definition_recursive(expr, name),
        AstContent::Declaration { decl_defs, .. } => find_definition_recursive(decl_defs, name),
        AstContent::Call { func_path, params } => find_definition_recursive(func_path, name)
            .or_else(|| find_definition_recursive(params, name)),
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => find_definition_recursive(condition, name)
            .or_else(|| find_definition_recursive(then_block, name))
            .or_else(|| {
                else_block
                    .as_ref()
                    .and_then(|eb| find_definition_recursive(eb, name))
            }),
        AstContent::Match { scrutinee, arms } => {
            if let Some(sp) = find_definition_recursive(scrutinee, name) {
                return Some(sp);
            }
            for arm in arms {
                if let MatchPattern::Value(v) = &arm.pattern
                    && let Some(sp) = find_definition_recursive(v, name)
                {
                    return Some(sp);
                }
                if let Some(sp) = find_definition_recursive(&arm.body, name) {
                    return Some(sp);
                }
            }
            None
        }
        AstContent::Dialogue { speakers, content } => find_definition_recursive(speakers, name)
            .or_else(|| find_definition_recursive(content, name)),
        AstContent::Menu { options } => {
            for opt in options {
                if let Some(sp) = find_definition_recursive(opt, name) {
                    return Some(sp);
                }
            }
            None
        }
        AstContent::MenuOption { content, .. } => find_definition_recursive(content, name),
        AstContent::Return { value } => value
            .as_ref()
            .and_then(|v| find_definition_recursive(v, name)),
        AstContent::Subscript { object, key } => {
            find_definition_recursive(object, name).or_else(|| find_definition_recursive(key, name))
        }
        AstContent::SubscriptAssign { object, key, value } => {
            find_definition_recursive(object, name)
                .or_else(|| find_definition_recursive(key, name))
                .or_else(|| find_definition_recursive(value, name))
        }
        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                if let Some(sp) = find_definition_recursive(k, name) {
                    return Some(sp);
                }
                if let Some(sp) = find_definition_recursive(v, name) {
                    return Some(sp);
                }
            }
            None
        }
        AstContent::LabeledBlock { label, block } => {
            if label == name {
                // This shouldn't normally be reached (handled in parent), but
                // for completeness…
                return Some(SimpleSpan::new((), 0..0));
            }
            find_definition_recursive(block, name)
        }
        AstContent::DecoratorDef { name: dn, body, .. } => {
            if dn == name {
                return Some(SimpleSpan::new((), 0..0));
            }
            find_definition_recursive(body, name)
        }
        // Leaves / nodes with no children that could define names.
        _ => None,
    }
}

// ── 4. hover_info ────────────────────────────────────────────────────────────

/// Produce markdown hover text for the symbol at the given byte offset.
///
/// Returns `None` when there is nothing meaningful at that position.
pub fn hover_info(ast: &Ast, symbols: &[Symbol], byte_offset: usize) -> Option<String> {
    // First try matching against collected symbols.
    if let Some(sym) = find_symbol_at_offset(symbols, byte_offset) {
        return Some(hover_for_symbol(sym, ast));
    }

    // Fall back: try to find an AST node at offset and describe it.
    hover_from_ast(ast, byte_offset)
}

/// Build hover markdown for a known [`Symbol`].
fn hover_for_symbol(sym: &Symbol, _ast: &Ast) -> String {
    match sym.kind {
        SymbolKind::Label => {
            format!("**label** `{}`", sym.name)
        }
        SymbolKind::Variable | SymbolKind::Constant | SymbolKind::Global => {
            let keyword = match sym.kind {
                SymbolKind::Variable => "let",
                SymbolKind::Constant => "const",
                SymbolKind::Global => "global",
                _ => unreachable!(),
            };
            if let Some(ta) = &sym.type_annotation {
                format!(
                    "**{keyword}** `{}`: `{}`",
                    sym.name,
                    format_type_annotation(ta)
                )
            } else {
                format!("**{keyword}** `{}`", sym.name)
            }
        }
        SymbolKind::Enum => {
            // Try to include the variants.
            if let Some(detail) = &sym.detail {
                format!("**enum** `{}`\n\n```\n{detail}\n```", sym.name)
            } else {
                format!("**enum** `{}`", sym.name)
            }
        }
        SymbolKind::EnumVariant => {
            if let Some(detail) = &sym.detail {
                format!("**variant** `{detail}`")
            } else {
                format!("**variant** `{}`", sym.name)
            }
        }
        SymbolKind::Struct => {
            if let Some(detail) = &sym.detail {
                format!("**struct** `{}`\n\n```\n{detail}\n```", sym.name)
            } else {
                format!("**struct** `{}`", sym.name)
            }
        }
        SymbolKind::Decorator => {
            if let Some(detail) = &sym.detail {
                format!("**decorator** `{}`\n\n```\n{detail}\n```", sym.name)
            } else {
                format!("**decorator** `{}`", sym.name)
            }
        }
        SymbolKind::Import => {
            if let Some(detail) = &sym.detail {
                format!("```\n{detail}\n```")
            } else {
                format!("**import** `{}`", sym.name)
            }
        }
    }
}

/// Walk the AST to find the innermost node at `byte_offset` and produce hover
/// text for it (used as a fallback when no collected symbol matches).
fn hover_from_ast(ast: &Ast, byte_offset: usize) -> Option<String> {
    if !span_contains(ast.span(), byte_offset) && ast.span().start != ast.span().end {
        return None;
    }

    match ast.content() {
        AstContent::Value(RuntimeValue::IdentPath(parts)) => {
            Some(format!("**identifier** `{}`", parts.join(".")))
        }
        AstContent::Value(RuntimeValue::Int(n)) => Some(format!("**int** `{n}`")),
        AstContent::Value(RuntimeValue::Float(f)) => Some(format!("**float** `{f}`")),
        AstContent::Value(RuntimeValue::Bool(b)) => Some(format!("**bool** `{b}`")),
        AstContent::Value(RuntimeValue::Null) => Some("**null**".into()),
        AstContent::Value(RuntimeValue::Dice(count, sides)) => {
            Some(format!("**dice** `{count}d{sides}`"))
        }
        AstContent::Jump {
            label,
            expects_return,
        } => {
            if *expects_return {
                Some(format!("**jump** `{label}` and return"))
            } else {
                Some(format!("**jump** `{label}`"))
            }
        }
        // Recurse into children to find the innermost match.
        _ => hover_from_ast_children(ast.content(), byte_offset),
    }
}

fn hover_from_ast_children(content: &AstContent, byte_offset: usize) -> Option<String> {
    match content {
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for s in stmts {
                if let Some(h) = hover_from_ast(s, byte_offset) {
                    return Some(h);
                }
            }
            None
        }
        AstContent::BinOp { left, right, .. } => {
            hover_from_ast(left, byte_offset).or_else(|| hover_from_ast(right, byte_offset))
        }
        AstContent::UnaryOp { expr, .. } => hover_from_ast(expr, byte_offset),
        AstContent::Declaration {
            decl_name,
            decl_defs,
            ..
        } => hover_from_ast(decl_name, byte_offset)
            .or_else(|| hover_from_ast(decl_defs, byte_offset)),
        AstContent::Call { func_path, params } => {
            hover_from_ast(func_path, byte_offset).or_else(|| hover_from_ast(params, byte_offset))
        }
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => hover_from_ast(condition, byte_offset)
            .or_else(|| hover_from_ast(then_block, byte_offset))
            .or_else(|| {
                else_block
                    .as_ref()
                    .and_then(|eb| hover_from_ast(eb, byte_offset))
            }),
        AstContent::LabeledBlock { block, .. } => hover_from_ast(block, byte_offset),
        AstContent::Match { scrutinee, arms } => {
            if let Some(h) = hover_from_ast(scrutinee, byte_offset) {
                return Some(h);
            }
            for arm in arms {
                if let MatchPattern::Value(v) = &arm.pattern
                    && let Some(h) = hover_from_ast(v, byte_offset)
                {
                    return Some(h);
                }
                if let Some(h) = hover_from_ast(&arm.body, byte_offset) {
                    return Some(h);
                }
            }
            None
        }
        AstContent::Dialogue { speakers, content } => {
            hover_from_ast(speakers, byte_offset).or_else(|| hover_from_ast(content, byte_offset))
        }
        AstContent::Menu { options } => {
            for opt in options {
                if let Some(h) = hover_from_ast(opt, byte_offset) {
                    return Some(h);
                }
            }
            None
        }
        AstContent::MenuOption { content, .. } => hover_from_ast(content, byte_offset),
        AstContent::Return { value } => value.as_ref().and_then(|v| hover_from_ast(v, byte_offset)),
        AstContent::Subscript { object, key } => {
            hover_from_ast(object, byte_offset).or_else(|| hover_from_ast(key, byte_offset))
        }
        AstContent::SubscriptAssign { object, key, value } => hover_from_ast(object, byte_offset)
            .or_else(|| hover_from_ast(key, byte_offset))
            .or_else(|| hover_from_ast(value, byte_offset)),
        AstContent::DecoratorDef { body, .. } => hover_from_ast(body, byte_offset),
        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                if let Some(h) = hover_from_ast(k, byte_offset) {
                    return Some(h);
                }
                if let Some(h) = hover_from_ast(v, byte_offset) {
                    return Some(h);
                }
            }
            None
        }
        _ => None,
    }
}

// ── 5. completion_items ──────────────────────────────────────────────────────

/// URD language keywords offered as completion candidates.
const KEYWORDS: &[&str] = &[
    "label",
    "let",
    "const",
    "global",
    "if",
    "else",
    "match",
    "jump",
    "return",
    "enum",
    "struct",
    "decorator",
    "import",
    "menu",
    "end!",
    "true",
    "false",
    "null",
    "and",
    "or",
    "not",
];

/// Produce a list of completion candidates relevant at `byte_offset`.
///
/// The returned tuples are `(label, kind)` pairs.  The caller maps them to
/// LSP `CompletionItem`s.
pub fn completion_items(
    _ast: &Ast,
    symbols: &[Symbol],
    _byte_offset: usize,
) -> Vec<(String, SymbolKind)> {
    let mut items: Vec<(String, SymbolKind)> = Vec::new();

    // All symbols are potential completions (labels for jumps, vars, enums…).
    for sym in symbols {
        // Avoid duplicates (enum variants may share names).
        if !items.iter().any(|(n, k)| n == &sym.name && *k == sym.kind) {
            items.push((sym.name.clone(), sym.kind));
        }
    }

    // Add keywords as Label kind (closest semantic match; the caller can
    // remap to `CompletionItemKind::Keyword`).
    for kw in KEYWORDS {
        items.push(((*kw).to_string(), SymbolKind::Variable));
    }

    items
}

// ── 6. find_references ───────────────────────────────────────────────────────

/// Find all spans in the AST that reference `name`, including both definitions
/// and usages (IdentPath values, jump labels, let-call targets, etc.).
pub fn find_references(ast: &Ast, name: &str) -> Vec<SimpleSpan> {
    let mut refs = Vec::new();
    find_references_recursive(ast, name, &mut refs);
    refs
}

fn find_references_recursive(ast: &Ast, name: &str, out: &mut Vec<SimpleSpan>) {
    match ast.content() {
        // Identifier references.
        AstContent::Value(RuntimeValue::IdentPath(parts)) => {
            if parts.iter().any(|p| p == name) {
                out.push(ast.span());
            }
        }

        // Jump to a label.
        AstContent::Jump { label, .. } => {
            if label == name {
                out.push(ast.span());
            }
        }

        // let x = jump label and return
        AstContent::LetCall {
            name: let_name,
            target,
        } => {
            if let_name == name || target == name {
                out.push(ast.span());
            }
        }

        // Labeled blocks: the label itself is a definition-reference.
        AstContent::LabeledBlock { label, block } => {
            if label == name {
                out.push(ast.span());
            }
            find_references_recursive(block, name, out);
        }

        // Declarations: the declared name is a definition-reference.
        AstContent::Declaration {
            decl_name,
            decl_defs,
            ..
        } => {
            if ident_name_from_ast(decl_name).as_deref() == Some(name) {
                out.push(ast.span());
            }
            find_references_recursive(decl_name, name, out);
            find_references_recursive(decl_defs, name, out);
        }

        // Enums: the enum name and each variant.
        AstContent::EnumDecl {
            name: enum_name,
            variants,
        } => {
            if enum_name == name || variants.iter().any(|v| v == name) {
                out.push(ast.span());
            }
        }

        // Structs.
        AstContent::StructDecl {
            name: struct_name, ..
        } => {
            if struct_name == name {
                out.push(ast.span());
            }
        }

        // Decorator definitions.
        AstContent::DecoratorDef {
            name: dec_name,
            body,
            ..
        } => {
            if dec_name == name {
                out.push(ast.span());
            }
            find_references_recursive(body, name, out);
        }

        // Imports.
        AstContent::Import { alias, .. } => {
            if alias == name {
                out.push(ast.span());
            }
        }

        // Everything else: recurse children.
        _ => {
            find_references_in_children(ast.content(), name, out);
        }
    }
}

fn find_references_in_children(content: &AstContent, name: &str, out: &mut Vec<SimpleSpan>) {
    match content {
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for s in stmts {
                find_references_recursive(s, name, out);
            }
        }
        AstContent::BinOp { left, right, .. } => {
            find_references_recursive(left, name, out);
            find_references_recursive(right, name, out);
        }
        AstContent::UnaryOp { expr, .. } => {
            find_references_recursive(expr, name, out);
        }
        AstContent::Call { func_path, params } => {
            find_references_recursive(func_path, name, out);
            find_references_recursive(params, name, out);
        }
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            find_references_recursive(condition, name, out);
            find_references_recursive(then_block, name, out);
            if let Some(eb) = else_block {
                find_references_recursive(eb, name, out);
            }
        }
        AstContent::Match { scrutinee, arms } => {
            find_references_recursive(scrutinee, name, out);
            for arm in arms {
                if let MatchPattern::Value(v) = &arm.pattern {
                    find_references_recursive(v, name, out);
                }
                find_references_recursive(&arm.body, name, out);
            }
        }
        AstContent::Dialogue { speakers, content } => {
            find_references_recursive(speakers, name, out);
            find_references_recursive(content, name, out);
        }
        AstContent::Menu { options } => {
            for opt in options {
                find_references_recursive(opt, name, out);
            }
        }
        AstContent::MenuOption { content, .. } => {
            find_references_recursive(content, name, out);
        }
        AstContent::Return { value: Some(v) } => {
            find_references_recursive(v, name, out);
        }
        AstContent::Return { value: None } => {}
        AstContent::Subscript { object, key } => {
            find_references_recursive(object, name, out);
            find_references_recursive(key, name, out);
        }
        AstContent::SubscriptAssign { object, key, value } => {
            find_references_recursive(object, name, out);
            find_references_recursive(key, name, out);
            find_references_recursive(value, name, out);
        }
        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                find_references_recursive(k, name, out);
                find_references_recursive(v, name, out);
            }
        }
        // Nodes already handled in find_references_recursive or leaves.
        _ => {}
    }
}

// ── 7. semantic_tokens ───────────────────────────────────────────────────────

/// Walk the AST and produce a flat list of [`SemanticTokenInfo`] ranges.
///
/// Note: comments are stripped by the Urd lexer before parsing, so they do
/// **not** appear in the AST and cannot be tagged here.
pub fn semantic_tokens(ast: &Ast) -> Vec<SemanticTokenInfo> {
    let mut tokens = Vec::new();
    emit_semantic_tokens(ast, &mut tokens);
    // Sort by start offset so the consumer can compute deltas easily.
    tokens.sort_by_key(|t| t.start);
    tokens
}

fn emit_semantic_tokens(ast: &Ast, out: &mut Vec<SemanticTokenInfo>) {
    let span = ast.span();
    let len = span.end.saturating_sub(span.start);

    // Tag decorators applied to this node.
    for dec in ast.decorators() {
        let ds = dec.name();
        // We don't have the decorator's own span directly, but we know the
        // parent node span covers it. Best-effort: skip sub-token tagging
        // since we can't reliably recover decorator byte positions from AST
        // alone. (Full implementation would need token-level spans.)
        let _ = ds;
    }

    match ast.content() {
        AstContent::Value(rv) => {
            if len > 0 {
                let tt = match rv {
                    RuntimeValue::IdentPath(_) => SemanticTokenType::Variable,
                    RuntimeValue::Int(_) | RuntimeValue::Float(_) => SemanticTokenType::Number,
                    RuntimeValue::Str(_) => SemanticTokenType::String,
                    RuntimeValue::Bool(_) | RuntimeValue::Null => SemanticTokenType::Keyword,
                    RuntimeValue::Dice(_, _) => SemanticTokenType::Number,
                    _ => return,
                };
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: len,
                    token_type: tt,
                });
            }
        }

        AstContent::BinOp { op: _, left, right } => {
            emit_semantic_tokens(left, out);
            emit_semantic_tokens(right, out);
            // The operator token itself sits between left and right spans.
            let op_start = left.span().end;
            let op_end = right.span().start;
            if op_end > op_start {
                out.push(SemanticTokenInfo {
                    start: op_start,
                    length: op_end - op_start,
                    token_type: SemanticTokenType::Operator,
                });
            }
        }

        AstContent::UnaryOp { op: _, expr } => {
            // Operator token is the bytes before the operand.
            let op_end = expr.span().start;
            if op_end > span.start {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: op_end - span.start,
                    token_type: SemanticTokenType::Operator,
                });
            }
            emit_semantic_tokens(expr, out);
        }

        AstContent::Declaration {
            decl_name,
            decl_defs,
            ..
        } => {
            // The keyword (let/const/global) spans from node start to the
            // name's start.
            let name_start = decl_name.span().start;
            if name_start > span.start {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: name_start - span.start,
                    token_type: SemanticTokenType::Keyword,
                });
            }
            emit_semantic_tokens(decl_name, out);
            emit_semantic_tokens(decl_defs, out);
        }

        AstContent::LabeledBlock { block, .. } => {
            // `label` keyword at start of span.
            // Label name is tagged as Label.
            if len > 0 {
                // Approximate: first 5 bytes are the keyword "label".
                let kw_len = "label".len().min(len);
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
                // The label name follows after "label ".
                let name_start = span.start + kw_len;
                let block_start = block.span().start;
                if block_start > name_start {
                    out.push(SemanticTokenInfo {
                        start: name_start,
                        length: block_start - name_start,
                        token_type: SemanticTokenType::Label,
                    });
                }
            }
            emit_semantic_tokens(block, out);
        }

        AstContent::Jump { .. } => {
            // Whole span is keyword-ish.
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
        }

        AstContent::Return { value } => {
            if len > 0 {
                // "return" keyword.
                let kw_len = "return".len().min(len);
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
            if let Some(v) = value {
                emit_semantic_tokens(v, out);
            }
        }

        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            // "if" keyword.
            let kw_len = "if".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
            emit_semantic_tokens(condition, out);
            emit_semantic_tokens(then_block, out);
            if let Some(eb) = else_block {
                emit_semantic_tokens(eb, out);
            }
        }

        AstContent::Match { scrutinee, arms } => {
            let kw_len = "match".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
            emit_semantic_tokens(scrutinee, out);
            for arm in arms {
                if let MatchPattern::Value(v) = &arm.pattern {
                    emit_semantic_tokens(v, out);
                }
                emit_semantic_tokens(&arm.body, out);
            }
        }

        AstContent::EnumDecl { .. } => {
            let kw_len = "enum".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
                // Rest of the span is the enum body.
                if len > kw_len {
                    out.push(SemanticTokenInfo {
                        start: span.start + kw_len,
                        length: len - kw_len,
                        token_type: SemanticTokenType::EnumMember,
                    });
                }
            }
        }

        AstContent::StructDecl { .. } => {
            let kw_len = "struct".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
                if len > kw_len {
                    out.push(SemanticTokenInfo {
                        start: span.start + kw_len,
                        length: len - kw_len,
                        token_type: SemanticTokenType::Struct,
                    });
                }
            }
        }

        AstContent::DecoratorDef { body, .. } => {
            let kw_len = "decorator".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
            emit_semantic_tokens(body, out);
        }

        AstContent::Import { .. } => {
            let kw_len = "import".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
        }

        AstContent::Call { func_path, params } => {
            // Tag the function name as Function.
            let fp_span = func_path.span();
            let fp_len = fp_span.end.saturating_sub(fp_span.start);
            if fp_len > 0 {
                out.push(SemanticTokenInfo {
                    start: fp_span.start,
                    length: fp_len,
                    token_type: SemanticTokenType::Function,
                });
            }
            emit_semantic_tokens(params, out);
        }

        AstContent::Dialogue { speakers, content } => {
            emit_semantic_tokens(speakers, out);
            emit_semantic_tokens(content, out);
        }

        AstContent::Menu { options } => {
            let kw_len = "menu".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
            for opt in options {
                emit_semantic_tokens(opt, out);
            }
        }

        AstContent::MenuOption { content, .. } => {
            emit_semantic_tokens(content, out);
        }

        AstContent::LetCall { .. } => {
            // "let" keyword.
            let kw_len = "let".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
        }

        // Containers: recurse.
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for s in stmts {
                emit_semantic_tokens(s, out);
            }
        }

        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                emit_semantic_tokens(k, out);
                emit_semantic_tokens(v, out);
            }
        }

        AstContent::Subscript { object, key } => {
            emit_semantic_tokens(object, out);
            emit_semantic_tokens(key, out);
        }

        AstContent::SubscriptAssign { object, key, value } => {
            emit_semantic_tokens(object, out);
            emit_semantic_tokens(key, out);
            emit_semantic_tokens(value, out);
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]

    use super::*;
    use urd::compiler::loader::parse_source;

    /// Helper: parse Urd source and return the AST, panicking on failure.
    fn parse(src: &str) -> Ast {
        parse_source(src).unwrap_or_else(|e| panic!("parse failed: {e}"))
    }

    // ── collect_symbols ──────────────────────────────────────────────────

    #[test]
    fn collect_symbols_finds_labels() {
        let ast = parse("label foo {\n  end!()\n}\nlabel bar {\n  end!()\n}\n");
        let syms = collect_symbols(&ast);
        let labels: Vec<&str> = syms
            .iter()
            .filter(|s| s.kind == SymbolKind::Label)
            .map(|s| s.name.as_str())
            .collect();
        assert!(
            labels.contains(&"foo"),
            "expected 'foo' label, got {labels:?}"
        );
        assert!(
            labels.contains(&"bar"),
            "expected 'bar' label, got {labels:?}"
        );
    }

    #[test]
    fn collect_symbols_finds_variables() {
        let ast = parse("let x = 1\nconst y = 2\nglobal z = 3\n");
        let syms = collect_symbols(&ast);

        let var = syms.iter().find(|s| s.name == "x");
        assert!(var.is_some(), "expected variable 'x'");
        assert_eq!(var.unwrap().kind, SymbolKind::Variable);

        let cst = syms.iter().find(|s| s.name == "y");
        assert!(cst.is_some(), "expected constant 'y'");
        assert_eq!(cst.unwrap().kind, SymbolKind::Constant);

        let glb = syms.iter().find(|s| s.name == "z");
        assert!(glb.is_some(), "expected global 'z'");
        assert_eq!(glb.unwrap().kind, SymbolKind::Global);
    }

    #[test]
    fn collect_symbols_finds_enums_and_variants() {
        let ast = parse("enum Color { Red, Green, Blue }\n");
        let syms = collect_symbols(&ast);

        let enum_sym = syms.iter().find(|s| s.kind == SymbolKind::Enum);
        assert!(enum_sym.is_some(), "expected Enum symbol");
        assert_eq!(enum_sym.unwrap().name, "Color");

        let variant_names: Vec<&str> = syms
            .iter()
            .filter(|s| s.kind == SymbolKind::EnumVariant)
            .map(|s| s.name.as_str())
            .collect();
        assert!(variant_names.contains(&"Red"), "missing variant Red");
        assert!(variant_names.contains(&"Green"), "missing variant Green");
        assert!(variant_names.contains(&"Blue"), "missing variant Blue");
    }

    #[test]
    fn collect_symbols_finds_structs() {
        let ast = parse("struct Player { name: str, hp: int }\n");
        let syms = collect_symbols(&ast);

        let struct_sym = syms.iter().find(|s| s.kind == SymbolKind::Struct);
        assert!(struct_sym.is_some(), "expected Struct symbol");
        assert_eq!(struct_sym.unwrap().name, "Player");
    }

    #[test]
    fn collect_symbols_nested_in_labels() {
        let ast = parse("label start {\n  let hp = 100\n  end!()\n}\n");
        let syms = collect_symbols(&ast);

        let label = syms.iter().find(|s| s.kind == SymbolKind::Label);
        assert!(label.is_some(), "expected label 'start'");

        let var = syms.iter().find(|s| s.name == "hp");
        assert!(var.is_some(), "expected nested variable 'hp'");
    }

    // ── find_definition ──────────────────────────────────────────────────

    #[test]
    fn find_definition_label() {
        let ast = parse("label intro {\n  end!()\n}\n");
        let span = find_definition(&ast, "intro");
        assert!(span.is_some(), "expected definition span for 'intro'");
    }

    #[test]
    fn find_definition_variable() {
        let ast = parse("let score = 0\n");
        let span = find_definition(&ast, "score");
        assert!(span.is_some(), "expected definition span for 'score'");
    }

    #[test]
    fn find_definition_enum() {
        let ast = parse("enum Dir { N, S, E, W }\n");
        let span = find_definition(&ast, "Dir");
        assert!(span.is_some(), "expected definition span for 'Dir'");
    }

    #[test]
    fn find_definition_struct() {
        let ast = parse("struct Item { name: str }\n");
        let span = find_definition(&ast, "Item");
        assert!(span.is_some(), "expected definition span for 'Item'");
    }

    #[test]
    fn find_definition_not_found() {
        let ast = parse("let x = 1\n");
        assert!(find_definition(&ast, "nonexistent").is_none());
    }

    // ── find_symbol_at_offset ────────────────────────────────────────────

    #[test]
    fn find_symbol_at_offset_within_span() {
        let ast = parse("label hello {\n  end!()\n}\n");
        let syms = collect_symbols(&ast);
        // The label node's span should cover "label hello { ... }".
        let label_sym = syms.iter().find(|s| s.kind == SymbolKind::Label).unwrap();
        let mid = label_sym.span.start + 1;
        let found = find_symbol_at_offset(&syms, mid);
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "hello");
    }

    #[test]
    fn find_symbol_at_offset_outside_all_spans() {
        let syms = vec![Symbol {
            name: "x".into(),
            kind: SymbolKind::Variable,
            span: SimpleSpan::new((), 10..20),
            type_annotation: None,
            detail: None,
        }];
        assert!(find_symbol_at_offset(&syms, 5).is_none());
        assert!(find_symbol_at_offset(&syms, 25).is_none());
    }

    // ── hover_info ───────────────────────────────────────────────────────

    #[test]
    fn hover_info_for_label() {
        let ast = parse("label greet {\n  end!()\n}\n");
        let syms = collect_symbols(&ast);
        let label_sym = syms.iter().find(|s| s.kind == SymbolKind::Label).unwrap();
        let mid = label_sym.span.start + 1;
        let info = hover_info(&ast, &syms, mid);
        assert!(info.is_some());
        let text = info.unwrap();
        assert!(
            text.contains("label") && text.contains("greet"),
            "hover should mention 'label' and 'greet', got: {text}"
        );
    }

    // ── completion_items ─────────────────────────────────────────────────

    #[test]
    fn completion_includes_symbols_and_keywords() {
        let ast = parse("let x = 1\nlabel a {\n  end!()\n}\n");
        let syms = collect_symbols(&ast);
        let items = completion_items(&ast, &syms, 0);
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(
            names.contains(&"x"),
            "completion should include variable 'x'"
        );
        assert!(names.contains(&"a"), "completion should include label 'a'");
        assert!(
            names.contains(&"if"),
            "completion should include keyword 'if'"
        );
    }

    // ── find_references ──────────────────────────────────────────────────

    #[test]
    fn find_references_includes_definition_and_usage() {
        let src = "label start {\n  end!()\n}\njump start\n";
        let ast = parse(src);
        let refs = find_references(&ast, "start");
        // At minimum, the LabeledBlock definition and the Jump usage.
        assert!(
            refs.len() >= 2,
            "expected at least 2 references to 'start', got {}",
            refs.len()
        );
    }

    // ── semantic_tokens ──────────────────────────────────────────────────

    #[test]
    fn semantic_tokens_sorted_by_offset() {
        let ast = parse("let x = 1\nlabel a {\n  end!()\n}\n");
        let toks = semantic_tokens(&ast);
        for window in toks.windows(2) {
            assert!(
                window[0].start <= window[1].start,
                "tokens must be sorted by start offset"
            );
        }
    }

    #[test]
    fn semantic_tokens_includes_number() {
        let ast = parse("let x = 42\n");
        let toks = semantic_tokens(&ast);
        let has_number = toks
            .iter()
            .any(|t| t.token_type == SemanticTokenType::Number);
        assert!(
            has_number,
            "expected at least one Number token, got {toks:?}"
        );
    }
}
