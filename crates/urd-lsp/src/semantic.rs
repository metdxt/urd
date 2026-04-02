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
    /// Optional documentation comment attached to this symbol via `##` syntax.
    pub doc_comment: Option<String>,
    /// `true` when this symbol was declared with the `extern` keyword, meaning
    /// its value is provided by the host runtime rather than the script itself.
    pub is_extern: bool,
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

/// Pretty-print an AST expression node as a compact value string suitable for
/// hover display (e.g. `42`, `"Narrator"`, `:{ name: "Narrator", name_color: "#a0a0b0" }`).
///
/// Complex sub-expressions that cannot be rendered concisely are shown as `…`.
fn format_ast_value(ast: &Ast) -> String {
    match ast.content() {
        AstContent::Value(RuntimeValue::Null) => "null".into(),
        AstContent::Value(RuntimeValue::Bool(b)) => b.to_string(),
        AstContent::Value(RuntimeValue::Int(n)) => n.to_string(),
        AstContent::Value(RuntimeValue::Float(f)) => f.to_string(),
        AstContent::Value(RuntimeValue::Str(s)) => format!("\"{s}\""),
        AstContent::Value(RuntimeValue::Dice(count, sides)) => format!("{count}d{sides}"),
        AstContent::Value(RuntimeValue::IdentPath(parts)) => parts.join("."),
        AstContent::Map(pairs) => {
            let fields: Vec<String> = pairs
                .iter()
                .map(|(k, v)| {
                    let key = if let AstContent::Value(RuntimeValue::IdentPath(parts)) = k.content()
                    {
                        parts.last().cloned().unwrap_or_default()
                    } else {
                        format_ast_value(k)
                    };
                    format!("{key}: {}", format_ast_value(v))
                })
                .collect();
            format!(":{{ {} }}", fields.join(", "))
        }
        AstContent::List(items) => {
            let parts: Vec<String> = items.iter().map(format_ast_value).collect();
            format!("[{}]", parts.join(", "))
        }
        AstContent::UnaryOp { op, expr } => {
            use urd::parser::ast::UnaryOperator;
            let op_str = match op {
                UnaryOperator::Negate => "-",
                UnaryOperator::Not => "not ",
                UnaryOperator::BitwiseNot => "~",
            };
            format!("{op_str}{}", format_ast_value(expr))
        }
        AstContent::BinOp { op, left, right } => {
            use urd::parser::ast::Operator;
            let op_str = match op {
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Multiply => "*",
                Operator::Divide => "/",
                Operator::DoubleSlash => "//",
                Operator::Percent => "%",
                Operator::Equals => "==",
                Operator::NotEquals => "!=",
                Operator::And => "and",
                Operator::Or => "or",
                _ => return format!("({} … {})", format_ast_value(left), format_ast_value(right)),
            };
            format!(
                "({} {op_str} {})",
                format_ast_value(left),
                format_ast_value(right)
            )
        }
        // Calls, blocks, and other complex nodes are rendered as an ellipsis.
        _ => "…".into(),
    }
}

/// Walk `ast` to find the right-hand side of the declaration whose name is
/// `name`, returning a reference to its `decl_defs` node.
///
/// Searches the outermost block and all label-wrapped blocks (matching the
/// same scope as [`collect_symbols`]).
fn find_decl_value<'a>(ast: &'a Ast, name: &str) -> Option<&'a Ast> {
    match ast.content() {
        AstContent::Block(stmts) => {
            for stmt in stmts {
                if let Some(v) = find_decl_value(stmt, name) {
                    return Some(v);
                }
            }
            None
        }
        AstContent::LabeledBlock { block, .. } => find_decl_value(block, name),
        AstContent::Declaration {
            decl_name,
            decl_defs,
            ..
        } => {
            if ident_name_from_ast(decl_name).as_deref() == Some(name) {
                Some(decl_defs)
            } else {
                None
            }
        }
        _ => None,
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

/// Extract the `[_a-zA-Z0-9]+` identifier word that the cursor sits inside.
///
/// Returns an empty string when `byte_offset` is not on an identifier character.
/// Unlike [`word_at_offset`](crate::main) this helper does **not** include dots,
/// so it returns only a single identifier segment — suitable for comparing
/// against `Jump.label` and `LetCall.target` which are plain names.
fn ident_at_offset(src: &str, byte_offset: usize) -> &str {
    let bytes = src.as_bytes();
    let offset = byte_offset.min(bytes.len());

    fn is_ident(b: u8) -> bool {
        b.is_ascii_alphanumeric() || b == b'_'
    }

    let probe = if offset < bytes.len() && is_ident(bytes[offset]) {
        offset
    } else if offset > 0 && is_ident(bytes[offset - 1]) {
        offset - 1
    } else {
        return "";
    };

    let mut start = probe;
    while start > 0 && is_ident(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = probe + 1;
    while end < bytes.len() && is_ident(bytes[end]) {
        end += 1;
    }

    &src[start..end]
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

fn make_symbol(
    name: String,
    kind: SymbolKind,
    span: SimpleSpan,
    type_annotation: Option<TypeAnnotation>,
    detail: Option<String>,
    doc_comment: Option<String>,
) -> Symbol {
    Symbol {
        name,
        kind,
        span,
        type_annotation,
        detail,
        doc_comment,
        is_extern: false,
    }
}

fn collect_symbols_recursive(ast: &Ast, out: &mut Vec<Symbol>) {
    match ast.content() {
        // ── Declarations (let / const / global) ──────────────────────────
        AstContent::Declaration {
            kind,
            decl_name,
            type_annotation,
            decl_defs,
        } => {
            if let Some(name) = ident_name_from_ast(decl_name) {
                let sym_kind = match kind {
                    DeclKind::Variable => SymbolKind::Variable,
                    DeclKind::Constant => SymbolKind::Constant,
                    DeclKind::Global => SymbolKind::Global,
                };
                // Render the value and embed it in `detail` so that when this
                // symbol is imported into another file, hover_for_symbol can
                // still display the value without needing the defining AST.
                let val_str = {
                    let s = format_ast_value(decl_defs);
                    if s == "…" { None } else { Some(s) }
                };
                let detail = Some(match (type_annotation.as_ref(), val_str.as_deref()) {
                    (Some(ta), Some(vs)) => format!(
                        "{} {}: {} = {}",
                        format_decl_kind(kind),
                        name,
                        format_type_annotation(ta),
                        vs
                    ),
                    (Some(ta), None) => format!(
                        "{} {}: {}",
                        format_decl_kind(kind),
                        name,
                        format_type_annotation(ta)
                    ),
                    (None, Some(vs)) => {
                        format!("{} {} = {}", format_decl_kind(kind), name, vs)
                    }
                    (None, None) => format!("{} {}", format_decl_kind(kind), name),
                });
                out.push(make_symbol(
                    name,
                    sym_kind,
                    ast.span(),
                    type_annotation.clone(),
                    detail,
                    ast.doc_comment.clone(),
                ));
            }
            // Walk the definition expression (it may contain nested blocks).
            collect_symbols_recursive(decl_name, out);
            // decl_defs may contain nested blocks, lambdas, etc.
            walk_ast_content(ast.content(), out);
        }

        // ── Labeled blocks ───────────────────────────────────────────────
        AstContent::LabeledBlock { label, block } => {
            out.push(make_symbol(
                label.clone(),
                SymbolKind::Label,
                ast.span(),
                None,
                Some(format!("label {label}")),
                ast.doc_comment.clone(),
            ));
            collect_symbols_recursive(block, out);
        }

        // ── Enum declarations ────────────────────────────────────────────
        AstContent::EnumDecl { name, variants } => {
            let variant_list = variants.join(", ");
            out.push(make_symbol(
                name.clone(),
                SymbolKind::Enum,
                ast.span(),
                None,
                Some(format!("enum {name} {{ {variant_list} }}")),
                ast.doc_comment.clone(),
            ));
            // Each variant is also a symbol (for completion / references).
            for variant in variants {
                out.push(make_symbol(
                    variant.clone(),
                    SymbolKind::EnumVariant,
                    ast.span(),
                    None,
                    Some(format!("{name}.{variant}")),
                    None,
                ));
            }
        }

        // ── Struct declarations ──────────────────────────────────────
        AstContent::StructDecl { name, fields } => {
            let field_list: Vec<String> = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, format_type_annotation(&f.type_annotation)))
                .collect();
            out.push(make_symbol(
                name.clone(),
                SymbolKind::Struct,
                ast.span(),
                None,
                Some(format!("struct {name} {{ {} }}", field_list.join(", "))),
                ast.doc_comment.clone(),
            ));
            // Expose each field as a discoverable symbol so that rename can
            // recognise field names.  The zero-length span keeps these
            // invisible to offset-based lookups (hover, find-symbol-at).
            for field in fields {
                out.push(make_symbol(
                    field.name.clone(),
                    SymbolKind::Variable,
                    SimpleSpan::new((), 0..0),
                    Some(field.type_annotation.clone()),
                    Some(format!("{}.{}", name, field.name)),
                    None,
                ));
            }
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
            out.push(make_symbol(
                name.clone(),
                SymbolKind::Decorator,
                ast.span(),
                None,
                Some(format!(
                    "decorator {name}{constraint_str}({})",
                    param_list.join(", ")
                )),
                ast.doc_comment.clone(),
            ));
            collect_symbols_recursive(body, out);
        }

        // ── Imports ──────────────────────────────────────────────────────
        AstContent::Import { path, symbols } => {
            for entry in symbols {
                let detail = match &entry.original {
                    // Whole-module import: `import "path" as alias`
                    None => Some(format!("import \"{path}\" as {}", entry.alias)),
                    // Symbol import: `import sym as alias from "path"`
                    Some(orig) if orig == &entry.alias => {
                        Some(format!("import {} from \"{path}\"", entry.alias))
                    }
                    Some(orig) => Some(format!("import {orig} as {} from \"{path}\"", entry.alias)),
                };
                out.push(make_symbol(
                    entry.alias.clone(),
                    SymbolKind::Import,
                    ast.span(),
                    None,
                    detail,
                    ast.doc_comment.clone(),
                ));
            }
        }

        // ── LetCall (let name = jump label and return) ───────────────────
        AstContent::LetCall { name, target: _ } => {
            out.push(make_symbol(
                name.clone(),
                SymbolKind::Variable,
                ast.span(),
                None,
                Some(format!("let {name} = jump ... and return")),
                ast.doc_comment.clone(),
            ));
        }

        // ── Extern declarations (`extern name: Type`) ────────────────────
        AstContent::ExternDeclaration {
            name,
            type_annotation,
        } => {
            if let Some(var_name) = ident_name_from_ast(name) {
                let detail = Some(match type_annotation {
                    Some(ta) => format!("extern {}: {}", var_name, format_type_annotation(ta)),
                    None => format!("extern {}", var_name),
                });
                out.push(Symbol {
                    name: var_name,
                    kind: SymbolKind::Variable,
                    span: ast.span(),
                    type_annotation: type_annotation.clone(),
                    detail,
                    doc_comment: ast.doc_comment.clone(),
                    is_extern: true,
                });
            }
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
        | AstContent::FnDef { .. }
        | AstContent::Import { .. }
        | AstContent::LetCall { .. }
        | AstContent::ExternDeclaration { .. } => {}
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

        AstContent::Import { symbols, .. } => {
            if symbols.iter().any(|s| s.alias == name) {
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

        AstContent::ExternDeclaration {
            name: decl_name, ..
        } => {
            if ident_name_from_ast(decl_name).as_deref() == Some(name) {
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
pub fn hover_info(ast: &Ast, symbols: &[Symbol], src: &str, byte_offset: usize) -> Option<String> {
    // First try matching against collected symbols.
    if let Some(sym) = find_symbol_at_offset(symbols, byte_offset) {
        // Container symbols (labels, decorators) span their entire body.
        // When the cursor is inside the body but not on the name itself,
        // prefer a more specific AST-level hover before falling back to
        // the container's hover.
        let is_container = matches!(sym.kind, SymbolKind::Label | SymbolKind::Decorator);
        if !is_container {
            return Some(hover_for_symbol(sym, ast, symbols));
        }

        // For containers, try the AST walk first for a more specific hit.
        // An empty string means "position claimed but nothing to display".
        if let Some(h) = hover_from_ast(ast, symbols, src, byte_offset, ast) {
            if !h.is_empty() {
                return Some(h);
            }
            return None;
        }

        // If the cursor is on a keyword, operator, or punctuation there's
        // nothing useful to show — suppress the container hover.
        if is_keyword_or_syntax(src, byte_offset) {
            return None;
        }

        // Nothing more specific — show the container symbol itself.
        return Some(hover_for_symbol(sym, ast, symbols));
    }

    // Fall back: try to find an AST node at offset and describe it.
    // Filter out empty strings (meaning "position claimed, nothing to show").
    hover_from_ast(ast, symbols, src, byte_offset, ast).filter(|h| !h.is_empty())
}

/// Build hover markdown for a known [`Symbol`].
///
/// All code snippets are wrapped in ```` ```urd ```` fenced blocks so that the
/// editor applies Urd syntax highlighting (via the registered Tree-sitter
/// grammar) the same way Rust's rust-analyzer highlights Rust snippets.
/// Append a doc comment block to a hover string, if one is present.
fn append_doc_comment(hover: String, sym: &Symbol) -> String {
    if let Some(doc) = &sym.doc_comment {
        format!("{hover}\n\n---\n{doc}")
    } else {
        hover
    }
}

fn hover_for_symbol(sym: &Symbol, root: &Ast, symbols: &[Symbol]) -> String {
    match sym.kind {
        SymbolKind::Label => {
            let hover = format!("```urd\nlabel {}\n```", sym.name);
            append_doc_comment(hover, sym)
        }

        SymbolKind::Variable | SymbolKind::Constant | SymbolKind::Global => {
            let keyword = match sym.kind {
                SymbolKind::Variable => "let",
                SymbolKind::Constant => "const",
                SymbolKind::Global => "global",
                _ => unreachable!(),
            };
            let prefix = if sym.is_extern {
                format!("extern {keyword}")
            } else {
                keyword.to_owned()
            };
            // Build the full declaration: `[extern] keyword name: Type = value`
            let sig = if let Some(ta) = &sym.type_annotation {
                format!("{prefix} {}: {}", sym.name, format_type_annotation(ta))
            } else {
                format!("{prefix} {}", sym.name)
            };
            let full_decl = if sym.is_extern {
                // Extern declarations have no initialiser — show only the signature.
                sig
            } else if let Some(val_ast) = find_decl_value(root, &sym.name) {
                // Local declaration — render value directly from the AST.
                let val_str = format_ast_value(val_ast);
                if val_str != "…" {
                    format!("{sig} = {val_str}")
                } else {
                    sig
                }
            } else if let Some(val) = sym
                .detail
                .as_deref()
                .and_then(|d| d.split_once(" = ").map(|x| x.1))
            {
                // Imported symbol — value was pre-rendered into `detail` at
                // collect_symbols_recursive time.
                format!("{sig} = {val}")
            } else {
                sig
            };
            // If the type is a Named type, append its struct/enum definition
            // as a second highlighted code block.
            if let Some(TypeAnnotation::Named(_)) = &sym.type_annotation {
                let type_name = sym
                    .type_annotation
                    .as_ref()
                    .map(format_type_annotation)
                    .unwrap_or_default();
                if let Some(type_sym) = symbols.iter().find(|s| {
                    s.name == type_name && matches!(s.kind, SymbolKind::Struct | SymbolKind::Enum)
                }) && let Some(detail) = &type_sym.detail
                {
                    let hover = format!("```urd\n{full_decl}\n```\n\n```urd\n{detail}\n```");
                    return append_doc_comment(hover, sym);
                }
            }
            append_doc_comment(format!("```urd\n{full_decl}\n```"), sym)
        }

        SymbolKind::Enum => {
            let hover = if let Some(detail) = &sym.detail {
                format!("```urd\n{detail}\n```")
            } else {
                format!("```urd\nenum {}\n```", sym.name)
            };
            append_doc_comment(hover, sym)
        }

        SymbolKind::EnumVariant => {
            let hover = if let Some(detail) = &sym.detail {
                format!("```urd\n{detail}\n```")
            } else {
                format!("```urd\n{}\n```", sym.name)
            };
            append_doc_comment(hover, sym)
        }

        SymbolKind::Struct => {
            let hover = if let Some(detail) = &sym.detail {
                format!("```urd\n{detail}\n```")
            } else {
                format!("```urd\nstruct {}\n```", sym.name)
            };
            append_doc_comment(hover, sym)
        }

        SymbolKind::Decorator => {
            let hover = if let Some(detail) = &sym.detail {
                format!("```urd\n{detail}\n```")
            } else {
                format!("```urd\ndecorator {}\n```", sym.name)
            };
            append_doc_comment(hover, sym)
        }

        SymbolKind::Import => {
            let hover = if let Some(detail) = &sym.detail {
                format!("```urd\n{detail}\n```")
            } else {
                format!("```urd\nimport {}\n```", sym.name)
            };
            append_doc_comment(hover, sym)
        }
    }
}

/// Walk the AST to find the innermost node at `byte_offset` and produce hover
/// text for it (used as a fallback when no collected symbol matches).
fn hover_from_ast(
    ast: &Ast,
    symbols: &[Symbol],
    src: &str,
    byte_offset: usize,
    root: &Ast,
) -> Option<String> {
    if !span_contains(ast.span(), byte_offset) && ast.span().start != ast.span().end {
        return None;
    }

    match ast.content() {
        AstContent::Value(RuntimeValue::IdentPath(parts)) => {
            let name = parts.join(".");
            // Look up the identifier in the symbol table to show its
            // definition / type info instead of a bare "identifier" hover.
            //
            // Prefer value-kinded symbols (Constant / Variable / Global) over
            // Import-kinded declaration nodes.  A directly-imported symbol
            // like `import (hero) from "chars.urd"` produces BOTH an Import
            // sentinel in the local symbol list AND a Constant symbol in the
            // imported list; we want the richer Constant.
            let sym = symbols
                .iter()
                .find(|s| s.name == name && !matches!(s.kind, SymbolKind::Import))
                .or_else(|| symbols.iter().find(|s| s.name == name));
            if let Some(sym) = sym {
                return Some(hover_for_symbol(sym, root, symbols));
            }
            Some(format!("**identifier** `{name}`"))
        }
        AstContent::Value(RuntimeValue::Str(_)) => {
            // No hover for plain string text, but if the cursor is on an
            // interpolation variable like {hull_integrity}, resolve its type.
            //
            // String AST nodes may carry zero-length spans, so we search the
            // source text around `byte_offset` directly: scan backwards for
            // an unescaped `{` and forwards for `}`.  If both are found
            // before hitting a `"` boundary we're inside an interpolation.
            if let Some(info) = interpolation_hover_at(src, byte_offset, symbols, ast, root) {
                return Some(info);
            }
            // Inside a string but not on an interpolation — return an empty
            // string to claim this position and prevent the container
            // (label/decorator) fallback from firing.
            Some(String::new())
        }
        AstContent::Value(RuntimeValue::Int(n)) => Some(format!("**int** `{n}`")),
        AstContent::Value(RuntimeValue::Float(f)) => Some(format!("**float** `{f}`")),
        AstContent::Value(RuntimeValue::Bool(b)) => Some(format!("**bool** `{b}`")),
        AstContent::Value(RuntimeValue::Null) => Some("**null**".into()),
        AstContent::Value(RuntimeValue::Dice(count, sides)) => {
            Some(format!("**dice** `{count}d{sides}`"))
        }

        // Jump / LetCall — the target label is stored as a plain String, not
        // as an AST child, so hover_from_ast_children can never find it.
        // We detect whether the cursor is over the target name by comparing
        // the word under the cursor to the stored label string; if it matches
        // we resolve the label symbol directly.
        AstContent::Jump { label, .. } => {
            let word = ident_at_offset(src, byte_offset);
            if word == label.as_str() {
                // Cursor is on the target label name — show its hover card.
                if let Some(sym) = symbols
                    .iter()
                    .find(|s| s.name == *label && s.kind == SymbolKind::Label)
                {
                    return Some(hover_for_symbol(sym, root, symbols));
                }
                // Label not (yet) in the symbol table — show a plain snippet.
                return Some(format!("```urd\nlabel {}\n```", label));
            }
            // Cursor is on the `jump` keyword — return None so that
            // is_keyword_or_syntax suppresses the container-label fallback.
            None
        }

        AstContent::LetCall { name, target } => {
            let word = ident_at_offset(src, byte_offset);
            if word == target.as_str() {
                if let Some(sym) = symbols
                    .iter()
                    .find(|s| s.name == *target && s.kind == SymbolKind::Label)
                {
                    return Some(hover_for_symbol(sym, root, symbols));
                }
                return Some(format!("```urd\nlabel {}\n```", target));
            }
            if word == name.as_str()
                && let Some(sym) = symbols.iter().find(|s| s.name == *name)
            {
                return Some(hover_for_symbol(sym, root, symbols));
            }
            None
        }

        // Recurse into children to find the innermost match.
        // Note: Menu, MenuOption, and Jump are intentionally NOT suppressed here
        // so that hover works on identifiers inside menu bodies, if-blocks, etc.
        // The `jump` / `menu` keywords are already handled by `is_keyword_or_syntax`
        // in the container fallback path, so suppressing these nodes would
        // incorrectly prevent hover from reaching nested content like
        // `boss_defeated = 1` inside a menu option body.
        _ => hover_from_ast_children(ast.content(), symbols, src, byte_offset, root),
    }
}

/// Language keywords used by [`is_keyword_or_syntax`] to suppress hover tooltips.
const KEYWORDS: &[&str] = &[
    "label",
    "let",
    "const",
    "global",
    "if",
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

/// Return `true` when `byte_offset` sits on a language keyword, operator, or
/// punctuation — anything that doesn't benefit from a hover tooltip.
fn is_keyword_or_syntax(src: &str, byte_offset: usize) -> bool {
    let bytes = src.as_bytes();
    if byte_offset >= bytes.len() {
        return false;
    }

    // Non-identifier byte → operator / punctuation / whitespace.
    let b = bytes[byte_offset];
    let is_ident = b.is_ascii_alphanumeric() || b == b'_';
    if !is_ident {
        return true;
    }

    // Extract the full word surrounding the offset.
    let mut start = byte_offset;
    while start > 0 && {
        let p = bytes[start - 1];
        p.is_ascii_alphanumeric() || p == b'_'
    } {
        start -= 1;
    }
    let mut end = byte_offset + 1;
    while end < bytes.len() && {
        let p = bytes[end];
        p.is_ascii_alphanumeric() || p == b'_'
    } {
        end += 1;
    }
    let word = &src[start..end];

    // Check against known keywords (including `end!` / `todo!`).
    KEYWORDS.contains(&word)
        || word == "elif"
        || word == "else"
        || word == "event"
        || word == "as"
        || word == "dialogue"
        || word == "choice"
        || word == "end"
        || word == "todo"
}

/// Check whether `byte_offset` falls inside a string interpolation `{…}` and,
/// if so, resolve the interpolation path against `symbols`.
///
/// Works by scanning the raw source text instead of relying on AST spans
/// (which may be zero-length for string literal nodes).
fn interpolation_hover_at(
    src: &str,
    byte_offset: usize,
    symbols: &[Symbol],
    _ast: &Ast,
    root: &Ast,
) -> Option<String> {
    let bytes = src.as_bytes();
    if byte_offset >= bytes.len() {
        return None;
    }

    // Scan backwards from byte_offset looking for `{`.
    // Stop if we hit `"` or `}` first (means we're not inside an interpolation).
    let mut open = byte_offset;
    loop {
        if open == 0 {
            return None;
        }
        open -= 1;
        let b = bytes[open];
        if b == b'{' {
            // Make sure it's not escaped: check the byte before.
            if open > 0 && bytes[open - 1] == b'\\' {
                return None;
            }
            break;
        }
        if b == b'"' || b == b'}' {
            return None;
        }
    }

    // Scan forwards from byte_offset looking for `}`.
    // Stop if we hit `"` or `{` first.
    let mut close = byte_offset;
    while close < bytes.len() {
        let b = bytes[close];
        if b == b'}' {
            break;
        }
        if b == b'"' || b == b'{' {
            return None;
        }
        close += 1;
    }
    if close >= bytes.len() {
        return None;
    }

    // Extract the content between { and } — may contain a format spec after `:`
    let inner = &src[open + 1..close];
    let path = inner.split(':').next().unwrap_or(inner).trim();
    if path.is_empty() {
        return None;
    }

    // Same priority as the IdentPath branch: prefer non-Import symbols.
    let sym = symbols
        .iter()
        .find(|s| s.name == path && !matches!(s.kind, SymbolKind::Import))
        .or_else(|| symbols.iter().find(|s| s.name == path));
    if let Some(sym) = sym {
        return Some(hover_for_symbol(sym, root, symbols));
    }
    Some(format!("**identifier** `{path}`"))
}

fn hover_from_ast_children(
    content: &AstContent,
    symbols: &[Symbol],
    src: &str,
    byte_offset: usize,
    root: &Ast,
) -> Option<String> {
    match content {
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for s in stmts {
                if let Some(h) = hover_from_ast(s, symbols, src, byte_offset, root) {
                    return Some(h);
                }
            }
            None
        }
        AstContent::BinOp { left, right, .. } => {
            hover_from_ast(left, symbols, src, byte_offset, root)
                .or_else(|| hover_from_ast(right, symbols, src, byte_offset, root))
        }
        AstContent::UnaryOp { expr, .. } => hover_from_ast(expr, symbols, src, byte_offset, root),
        AstContent::Declaration {
            decl_name,
            decl_defs,
            ..
        } => hover_from_ast(decl_name, symbols, src, byte_offset, root)
            .or_else(|| hover_from_ast(decl_defs, symbols, src, byte_offset, root)),
        AstContent::Call { func_path, params } => {
            hover_from_ast(func_path, symbols, src, byte_offset, root)
                .or_else(|| hover_from_ast(params, symbols, src, byte_offset, root))
        }
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => hover_from_ast(condition, symbols, src, byte_offset, root)
            .or_else(|| hover_from_ast(then_block, symbols, src, byte_offset, root))
            .or_else(|| {
                else_block
                    .as_ref()
                    .and_then(|eb| hover_from_ast(eb, symbols, src, byte_offset, root))
            }),
        AstContent::LabeledBlock { block, .. } => {
            hover_from_ast(block, symbols, src, byte_offset, root)
        }
        AstContent::Match { scrutinee, arms } => {
            if let Some(h) = hover_from_ast(scrutinee, symbols, src, byte_offset, root) {
                return Some(h);
            }
            for arm in arms {
                if let MatchPattern::Value(v) = &arm.pattern
                    && let Some(h) = hover_from_ast(v, symbols, src, byte_offset, root)
                {
                    return Some(h);
                }
                if let Some(h) = hover_from_ast(&arm.body, symbols, src, byte_offset, root) {
                    return Some(h);
                }
            }
            None
        }
        AstContent::Dialogue { speakers, content } => {
            hover_from_ast(speakers, symbols, src, byte_offset, root)
                .or_else(|| hover_from_ast(content, symbols, src, byte_offset, root))
        }
        AstContent::Menu { options } => {
            for opt in options {
                if let Some(h) = hover_from_ast(opt, symbols, src, byte_offset, root) {
                    return Some(h);
                }
            }
            None
        }
        AstContent::MenuOption { content, .. } => {
            hover_from_ast(content, symbols, src, byte_offset, root)
        }
        AstContent::Return { value } => value
            .as_ref()
            .and_then(|v| hover_from_ast(v, symbols, src, byte_offset, root)),
        AstContent::Subscript { object, key } => {
            hover_from_ast(object, symbols, src, byte_offset, root)
                .or_else(|| hover_from_ast(key, symbols, src, byte_offset, root))
        }
        AstContent::SubscriptAssign { object, key, value } => {
            hover_from_ast(object, symbols, src, byte_offset, root)
                .or_else(|| hover_from_ast(key, symbols, src, byte_offset, root))
                .or_else(|| hover_from_ast(value, symbols, src, byte_offset, root))
        }
        AstContent::DecoratorDef { body, .. } => {
            hover_from_ast(body, symbols, src, byte_offset, root)
        }
        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                if let Some(h) = hover_from_ast(k, symbols, src, byte_offset, root) {
                    return Some(h);
                }
                if let Some(h) = hover_from_ast(v, symbols, src, byte_offset, root) {
                    return Some(h);
                }
            }
            None
        }
        AstContent::ExternDeclaration { name, .. } => {
            hover_from_ast(name, symbols, src, byte_offset, root)
        }
        _ => None,
    }
}

// ── 5. find_references ───────────────────────────────────────────────────────

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
            let full_path = parts.join(".");
            let leaf_matches = parts.last().is_some_and(|p| p == name);
            if full_path == name || leaf_matches {
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
            type_annotation,
            decl_defs,
            ..
        } => {
            if ident_name_from_ast(decl_name).as_deref() == Some(name) {
                out.push(ast.span());
            }
            // Named type annotations reference struct / enum names.
            if let Some(TypeAnnotation::Named(path)) = type_annotation
                && path.iter().any(|p| p == name)
            {
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
        AstContent::Import { symbols, .. } => {
            if symbols.iter().any(|s| s.alias == name) {
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

// ── 6b. find_rename_spans ────────────────────────────────────────────────────

/// Search for `name` as a whole-word match within `src[span.start..span.end]`.
///
/// Returns the span of the first occurrence, or `None`.
fn find_name_in_span(src: &str, span: SimpleSpan, name: &str) -> Option<SimpleSpan> {
    let slice = src.get(span.start..span.end)?;
    let mut search_from = 0;
    while let Some(rel) = slice[search_from..].find(name) {
        let abs_start = span.start + search_from + rel;
        let abs_end = abs_start + name.len();
        // Check whole-word boundaries.
        let before_ok = abs_start == 0 || {
            let b = src.as_bytes()[abs_start - 1];
            !b.is_ascii_alphanumeric() && b != b'_'
        };
        let after_ok = abs_end >= src.len() || {
            let b = src.as_bytes()[abs_end];
            !b.is_ascii_alphanumeric() && b != b'_'
        };
        if before_ok && after_ok {
            return Some(SimpleSpan::new((), abs_start..abs_end));
        }
        search_from += rel + 1;
    }
    None
}

/// Find all spans of *just the name text* for rename operations.
///
/// Unlike [`find_references`] which returns whole-node spans (e.g. the entire
/// `label foo { … }` block), this returns only the byte range of the name
/// itself within each reference site.
pub fn find_rename_spans(ast: &Ast, src: &str, name: &str) -> Vec<SimpleSpan> {
    // If `name` is a struct field, rename across the struct definition and all
    // struct-typed map literals instead of doing a plain symbol rename.
    if let Some(struct_name) = find_struct_for_field(ast, name) {
        let mut field_spans = Vec::new();
        collect_struct_field_spans(ast, src, &struct_name, name, &mut field_spans);
        if !field_spans.is_empty() {
            return field_spans;
        }
    }

    let ref_spans = find_references(ast, name);
    let mut out = Vec::new();
    for span in ref_spans {
        if let Some(precise) = find_name_in_span(src, span, name) {
            // Avoid duplicates from overlapping reference spans.
            if !out.iter().any(|s: &SimpleSpan| s.start == precise.start) {
                out.push(precise);
            }
        }
    }
    out
}

// ── 6c. struct field rename helpers ──────────────────────────────────────────

/// Return the struct name that declares a field called `field_name`, if any.
fn find_struct_for_field(ast: &Ast, field_name: &str) -> Option<String> {
    match ast.content() {
        AstContent::StructDecl { name, fields } => {
            if fields.iter().any(|f| f.name == field_name) {
                Some(name.clone())
            } else {
                None
            }
        }
        AstContent::Block(stmts) | AstContent::ExprList(stmts) => stmts
            .iter()
            .find_map(|s| find_struct_for_field(s, field_name)),
        AstContent::LabeledBlock { block, .. } => find_struct_for_field(block, field_name),
        AstContent::If {
            then_block,
            else_block,
            ..
        } => find_struct_for_field(then_block, field_name).or_else(|| {
            else_block
                .as_ref()
                .and_then(|eb| find_struct_for_field(eb, field_name))
        }),
        AstContent::DecoratorDef { body, .. } => find_struct_for_field(body, field_name),
        AstContent::Match { arms, .. } => arms
            .iter()
            .find_map(|arm| find_struct_for_field(&arm.body, field_name)),
        _ => None,
    }
}

/// Walk the AST collecting precise spans of `field_name` inside the definition
/// of `struct_name` and inside map-literal keys of declarations typed with that
/// struct.
fn collect_struct_field_spans(
    ast: &Ast,
    src: &str,
    struct_name: &str,
    field_name: &str,
    out: &mut Vec<SimpleSpan>,
) {
    match ast.content() {
        // The struct definition itself — locate the field name text.
        AstContent::StructDecl { name, fields }
            if name == struct_name && fields.iter().any(|f| f.name == field_name) =>
        {
            if let Some(sp) = find_name_in_span(src, ast.span(), field_name)
                && !out.iter().any(|s| s.start == sp.start)
            {
                out.push(sp);
            }
        }
        // Typed declaration — if it carries the struct type, look for
        // matching map keys in its value expression.
        AstContent::Declaration {
            type_annotation,
            decl_defs,
            ..
        } => {
            if let Some(TypeAnnotation::Named(path)) = type_annotation
                && path.first().map(String::as_str) == Some(struct_name)
            {
                collect_field_in_map_keys(decl_defs, field_name, out);
            }
            collect_struct_field_spans(decl_defs, src, struct_name, field_name, out);
        }
        // ── Generic recursion for the remaining node types ───────────
        AstContent::Block(stmts) | AstContent::ExprList(stmts) | AstContent::List(stmts) => {
            for s in stmts {
                collect_struct_field_spans(s, src, struct_name, field_name, out);
            }
        }
        AstContent::LabeledBlock { block, .. } => {
            collect_struct_field_spans(block, src, struct_name, field_name, out);
        }
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_struct_field_spans(condition, src, struct_name, field_name, out);
            collect_struct_field_spans(then_block, src, struct_name, field_name, out);
            if let Some(eb) = else_block {
                collect_struct_field_spans(eb, src, struct_name, field_name, out);
            }
        }
        AstContent::Match { scrutinee, arms } => {
            collect_struct_field_spans(scrutinee, src, struct_name, field_name, out);
            for arm in arms {
                collect_struct_field_spans(&arm.body, src, struct_name, field_name, out);
            }
        }
        AstContent::Menu { options } => {
            for opt in options {
                collect_struct_field_spans(opt, src, struct_name, field_name, out);
            }
        }
        AstContent::MenuOption { content, .. } => {
            collect_struct_field_spans(content, src, struct_name, field_name, out);
        }
        AstContent::DecoratorDef { body, .. } => {
            collect_struct_field_spans(body, src, struct_name, field_name, out);
        }
        AstContent::Dialogue { speakers, content } => {
            collect_struct_field_spans(speakers, src, struct_name, field_name, out);
            collect_struct_field_spans(content, src, struct_name, field_name, out);
        }
        AstContent::BinOp { left, right, .. } => {
            collect_struct_field_spans(left, src, struct_name, field_name, out);
            collect_struct_field_spans(right, src, struct_name, field_name, out);
        }
        AstContent::UnaryOp { expr, .. } => {
            collect_struct_field_spans(expr, src, struct_name, field_name, out);
        }
        AstContent::Call { func_path, params } => {
            collect_struct_field_spans(func_path, src, struct_name, field_name, out);
            collect_struct_field_spans(params, src, struct_name, field_name, out);
        }
        AstContent::Return { value: Some(v) } => {
            collect_struct_field_spans(v, src, struct_name, field_name, out);
        }
        AstContent::Subscript { object, key } => {
            collect_struct_field_spans(object, src, struct_name, field_name, out);
            collect_struct_field_spans(key, src, struct_name, field_name, out);
        }
        AstContent::SubscriptAssign { object, key, value } => {
            collect_struct_field_spans(object, src, struct_name, field_name, out);
            collect_struct_field_spans(key, src, struct_name, field_name, out);
            collect_struct_field_spans(value, src, struct_name, field_name, out);
        }
        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                collect_struct_field_spans(k, src, struct_name, field_name, out);
                collect_struct_field_spans(v, src, struct_name, field_name, out);
            }
        }
        _ => {}
    }
}

/// Collect spans of map keys whose name matches `field_name`.
fn collect_field_in_map_keys(ast: &Ast, field_name: &str, out: &mut Vec<SimpleSpan>) {
    if let AstContent::Map(pairs) = ast.content() {
        for (key, _) in pairs {
            if let AstContent::Value(RuntimeValue::IdentPath(parts)) = key.content()
                && parts.last().map(String::as_str) == Some(field_name)
            {
                out.push(key.span());
            }
        }
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

        AstContent::FnDef { body, .. } => {
            let kw_len = "fn".len().min(len);
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

        AstContent::ExternDeclaration { name, .. } => {
            // "extern" keyword at start, then the variable name.
            let kw_len = "extern".len().min(len);
            if len > 0 {
                out.push(SemanticTokenInfo {
                    start: span.start,
                    length: kw_len,
                    token_type: SemanticTokenType::Keyword,
                });
            }
            emit_semantic_tokens(name, out);
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
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
            doc_comment: None,
            is_extern: false,
        }];
        assert!(find_symbol_at_offset(&syms, 5).is_none());
        assert!(find_symbol_at_offset(&syms, 25).is_none());
    }

    // ── hover_info ───────────────────────────────────────────────────────

    #[test]
    fn hover_info_for_label() {
        let src = "label greet {\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let _label_sym = syms.iter().find(|s| s.kind == SymbolKind::Label).unwrap();
        // Point at the label *name* ("greet"), not the "label" keyword.
        let mid = src.find("greet").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, mid);
        assert!(info.is_some());
        let text = info.unwrap();
        assert!(
            text.contains("label") && text.contains("greet"),
            "hover should mention 'label' and 'greet', got: {text}"
        );
    }

    #[test]
    fn hover_info_string_in_label_shows_nothing() {
        let src = "label greet {\n  aria: \"Hello world\"\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        // Offset pointing at 'H' in "Hello world" (inside the string).
        let h_offset = src.find("Hello").unwrap();
        let info = hover_info(&ast, &syms, src, h_offset);
        assert!(
            info.is_none(),
            "hovering on plain string text should show nothing, got: {:?}",
            info
        );
    }

    #[test]
    fn hover_info_interpolation_in_label_shows_variable() {
        let src = "label greet {\n  let name = \"Ada\"\n  aria: \"Hello {name}!\"\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        // Offset pointing inside `name` within the interpolation {name}.
        let interp_offset = src.rfind("name").unwrap() + 1; // 'a' in the second 'name'
        let info = hover_info(&ast, &syms, src, interp_offset);
        assert!(
            info.is_some(),
            "hovering on interpolation variable should show info, src[{}..] = {:?}",
            interp_offset,
            &src[interp_offset..interp_offset + 4]
        );
        let text = info.unwrap();
        assert!(
            text.contains("let") && text.contains("name"),
            "hover should mention 'let' and 'name', got: {text}"
        );
    }

    // ── format_ast_value ─────────────────────────────────────────────────

    #[test]
    fn format_ast_value_primitives() {
        use urd::lexer::strings::ParsedString;
        assert_eq!(format_ast_value(&Ast::value(RuntimeValue::Null)), "null");
        assert_eq!(
            format_ast_value(&Ast::value(RuntimeValue::Bool(true))),
            "true"
        );
        assert_eq!(
            format_ast_value(&Ast::value(RuntimeValue::Bool(false))),
            "false"
        );
        assert_eq!(format_ast_value(&Ast::value(RuntimeValue::Int(42))), "42");
        assert_eq!(
            format_ast_value(&Ast::value(RuntimeValue::Dice(2, 6))),
            "2d6"
        );
        assert_eq!(
            format_ast_value(&Ast::value(RuntimeValue::Str(ParsedString::new_plain(
                "hello"
            )))),
            "\"hello\""
        );
        assert_eq!(
            format_ast_value(&Ast::value(RuntimeValue::IdentPath(vec![
                "Faction".to_owned(),
                "Guild".to_owned()
            ]))),
            "Faction.Guild"
        );
    }

    #[test]
    fn format_ast_value_list() {
        let list_ast = Ast::list(vec![
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2)),
            Ast::value(RuntimeValue::Int(3)),
        ]);
        assert_eq!(format_ast_value(&list_ast), "[1, 2, 3]");
    }

    #[test]
    fn format_ast_value_unary_negate() {
        let neg = Ast::negate_op(Ast::value(RuntimeValue::Int(5)));
        assert_eq!(format_ast_value(&neg), "-5");
    }

    #[test]
    fn format_ast_value_binary_add() {
        let add = Ast::add_op(
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2)),
        );
        assert_eq!(format_ast_value(&add), "(1 + 2)");
    }

    #[test]
    fn format_ast_value_complex_yields_ellipsis() {
        let block = Ast::block(vec![]);
        assert_eq!(format_ast_value(&block), "…");
    }

    // ── find_decl_value ───────────────────────────────────────────────────

    #[test]
    fn find_decl_value_top_level_const() {
        let src = "const score = 42\n";
        let ast = parse(src);
        let val = find_decl_value(&ast, "score");
        assert!(val.is_some(), "must find value for top-level const 'score'");
        assert_eq!(format_ast_value(val.unwrap()), "42");
    }

    #[test]
    fn find_decl_value_string_literal() {
        let src = "const greeting = \"hello\"\n";
        let ast = parse(src);
        let val = find_decl_value(&ast, "greeting");
        assert!(val.is_some());
        let rendered = format_ast_value(val.unwrap());
        assert!(
            rendered.contains("hello"),
            "expected rendered value to contain 'hello', got: {rendered}"
        );
    }

    #[test]
    fn find_decl_value_inside_label() {
        let src = "label intro {\n  let hp = 100\n  end!()\n}\n";
        let ast = parse(src);
        let val = find_decl_value(&ast, "hp");
        assert!(val.is_some(), "must find 'hp' inside a label block");
        assert_eq!(format_ast_value(val.unwrap()), "100");
    }

    #[test]
    fn find_decl_value_unknown_returns_none() {
        let src = "const x = 1\n";
        let ast = parse(src);
        assert!(
            find_decl_value(&ast, "nonexistent").is_none(),
            "must return None for unknown declaration name"
        );
    }

    // ── hover with values ─────────────────────────────────────────────────

    #[test]
    fn hover_shows_int_value_for_const() {
        let src = "const score = 42\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("score").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("42"),
            "hover must show the value 42; got: {info}"
        );
    }

    #[test]
    fn hover_shows_string_value_for_let() {
        let src = "label a {\n  let title = \"Wanderer\"\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("title").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("Wanderer"),
            "hover must show the string value 'Wanderer'; got: {info}"
        );
    }

    #[test]
    fn hover_shows_map_value_for_typed_const() {
        let src = "struct Character { name: str }\n\
                   const narrator: Character = :{ name: \"Narrator\" }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("narrator").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("Character"),
            "hover must mention the type 'Character'; got: {info}"
        );
        assert!(
            info.contains("Narrator"),
            "hover must show the struct value with 'Narrator'; got: {info}"
        );
    }

    #[test]
    fn hover_value_does_not_show_ellipsis_for_simple_expr() {
        let src = "label a {\n  let hp = 100\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("hp").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            !info.contains('…'),
            "hover must not show ellipsis for a simple integer literal; got: {info}"
        );
        assert!(
            info.contains("100"),
            "hover must show the value 100; got: {info}"
        );
    }

    #[test]
    fn hover_shows_value_when_variable_used_as_dialogue_speaker() {
        // This is the real-world case from the screenshot: `elder` is declared
        // at the top of the file and then used as a dialogue speaker further
        // down.  Hovering on the speaker name must show the declaration value,
        // not just the type signature — which requires `find_decl_value` to
        // search from the document ROOT, not from the leaf IdentPath node that
        // `hover_from_ast` lands on.
        let src = "struct Character { name: str }\n\
                   const elder: Character = :{ name: \"Elder Maren\" }\n\
                   label village_square {\n\
                   \telder: { \"Stranger! Thank the old gods you've come.\" }\n\
                   \tend!()\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Hover at the `elder` on the dialogue speaker line (line 4, after the
        // two declaration lines).
        let speaker_offset = src.rfind("elder").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, speaker_offset);
        assert!(
            info.is_some(),
            "hovering on dialogue speaker 'elder' must produce hover text"
        );
        let text = info.unwrap();
        assert!(
            text.contains("Elder Maren"),
            "hover on dialogue speaker must show the actual value; got: {text}"
        );
        assert!(
            text.contains("Character"),
            "hover on dialogue speaker must mention the type; got: {text}"
        );
    }

    #[test]
    fn hover_shows_value_for_variable_at_declaration_site() {
        // Hovering on a variable at its own declaration site shows the value.
        let src = "const base_hp = 100\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        let decl_offset = src.find("base_hp").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, decl_offset).unwrap();
        assert!(
            info.contains("100"),
            "hover on const declaration must show value 100; got: {info}"
        );
    }

    #[test]
    fn detail_includes_value_for_typed_const() {
        let src = "struct Character { name: str }\n\
                   const narrator: Character = :{ name: \"Narrator\", name_color: \"#a0a0b0\" }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let narrator = syms.iter().find(|s| s.name == "narrator").unwrap();
        let detail = narrator.detail.as_deref().unwrap();
        assert!(
            detail.contains("= :{"),
            "detail must embed the map value; got: {detail}"
        );
        assert!(
            detail.contains("Narrator"),
            "detail must contain the name value; got: {detail}"
        );
    }

    #[test]
    fn detail_includes_value_for_int_const() {
        let src = "const score = 42\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "score").unwrap();
        let detail = sym.detail.as_deref().unwrap();
        assert!(
            detail.contains("= 42"),
            "detail must embed the integer value 42; got: {detail}"
        );
    }

    #[test]
    fn detail_omits_value_for_complex_rhs() {
        // A declaration whose RHS is a call expression renders as "…" — the
        // detail must NOT include " = …" (the ellipsis is suppressed).
        let src = "label a {\n  let x = 1\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "x").unwrap();
        let detail = sym.detail.as_deref().unwrap();
        assert!(
            !detail.contains('…'),
            "detail must not contain ellipsis; got: {detail}"
        );
    }

    #[test]
    fn hover_imported_symbol_shows_value_via_detail_fallback() {
        // Simulate what happens when `hero` is a directly-imported symbol:
        // its Symbol was produced by collect_symbols on the imported module,
        // then aliased (with span=0..0) into the local symbol list.
        // find_decl_value on the LOCAL root will return None (hero is not
        // declared locally), so hover_for_symbol must fall back to the
        // pre-rendered value in sym.detail.
        use chumsky::span::SimpleSpan;

        // Build an imported symbol that looks like what aliased_symbols() returns
        // for `const hero: Character = :{ name: "Hero" }` in a remote file.
        let imported_sym = Symbol {
            is_extern: false,
            name: "hero".to_string(),
            kind: SymbolKind::Constant,
            span: SimpleSpan::new((), 0..0),
            type_annotation: Some(TypeAnnotation::Named(vec!["Character".to_string()])),
            // detail was produced by collect_symbols on the IMPORTED file,
            // so it includes the rendered value.
            detail: Some(
                r##"const hero: Character = :{ name: "Hero", name_color: "#f5c542" }"##.to_string(),
            ),
            doc_comment: None,
        };

        // Local source: hero is used as a dialogue speaker but NOT declared here.
        let src = "label scene {\n\thero: { \"Hello.\" }\n\tend!()\n}\n";
        let ast = parse(src);
        // Combine: local symbols + the imported symbol.
        let mut syms = collect_symbols(&ast);
        // Also add a local Import sentinel (as collect_symbols would add for
        // `import (hero) from "chars.urd"`).
        syms.push(Symbol {
            is_extern: false,
            name: "hero".to_string(),
            kind: SymbolKind::Import,
            span: SimpleSpan::new((), 0..5),
            type_annotation: None,
            detail: Some("import hero from \"chars.urd\"".to_string()),
            doc_comment: None,
        });
        syms.push(imported_sym);

        // Hover on `hero` in the dialogue speaker position.
        let speaker_offset = src.find("hero").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, speaker_offset).unwrap();

        assert!(
            info.contains("Hero"),
            "hover on imported speaker must show value from detail fallback; got: {info}"
        );
        assert!(
            !info.contains("import hero"),
            "hover must not show the raw import declaration; got: {info}"
        );
    }

    #[test]
    fn hover_on_ident_reference_in_speaker_position_shows_value() {
        // When an IdentPath is the *speaker* in a dialogue node, no symbol
        // spans that position, so hover_from_ast walks down to the IdentPath
        // leaf and calls hover_for_symbol.  The root-threading fix ensures
        // find_decl_value is called with the document root, not the leaf node.
        // This is a regression test for the bug: previously hover showed
        // `const elder: Character` without the value; now it must include the
        // map literal.
        let src = "struct Character { name: str }\n\
                   const elder: Character = :{ name: \"Elder Maren\" }\n\
                   label scene {\n\
                   \telder: { \"Hello.\" }\n\
                   \tend!()\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Find the `elder` used as the dialogue speaker (last occurrence).
        let speaker_offset = src.rfind("elder").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, speaker_offset).unwrap();
        assert!(
            info.contains("Elder Maren"),
            "hover on dialogue speaker must show the actual map value; got: {info}"
        );
    }

    #[test]
    fn hover_ident_ref_value_is_rendered_when_it_is_a_simple_literal() {
        // `let x = base_hp` — hovering on `base_hp` inside the declaration
        // RHS.  The declaration symbol for `x` spans the whole expression, so
        // find_symbol_at_offset returns `x`; hover shows x's value `base_hp`
        // (an IdentPath reference, not a literal — rendered as the identifier
        // name, not as the numeric value 100).  This verifies the value
        // rendering path doesn't crash and produces meaningful text.
        let src = "const base_hp = 100\n\
                   label a {\n\
                   \tlet x = base_hp\n\
                   \tend!()\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Hover on `x` (the declaration name).
        let x_offset = src.find("let x").unwrap() + 4;
        let info = hover_info(&ast, &syms, src, x_offset).unwrap();
        // The value of x is `base_hp` (an identifier reference) which should
        // be rendered as the identifier name.
        assert!(info.contains('x'), "hover must mention 'x'; got: {info}");
        assert!(
            !info.contains('…'),
            "hover must not show ellipsis for an ident reference value; got: {info}"
        );
    }

    // ── hover inside nested blocks ────────────────────────────────────────

    #[test]
    fn hover_works_inside_menu_option_body() {
        // Regression test: `Menu` and `MenuOption` nodes were previously in
        // the `hover_from_ast` suppress arm, which returned `Some("")` without
        // recursing into children.  Any identifier inside a menu option body
        // (e.g. `boss_defeated = 1`) was unreachable for hover.
        let src = "global boss_defeated: int = 0\n\
                   label dungeon {\n\
                   \tmenu {\n\
                   \t\t\"Fight\" {\n\
                   \t\t\tboss_defeated = 1\n\
                   \t\t\tend!()\n\
                   \t\t}\n\
                   \t}\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Cursor on `boss_defeated` on the assignment line (last occurrence).
        let offset = src.rfind("boss_defeated").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset);
        assert!(
            info.is_some(),
            "hover must work for an identifier inside a menu option body; got None\n\
             (check that Menu/MenuOption are not in the suppress arm of hover_from_ast)"
        );
        let text = info.unwrap();
        assert!(
            text.contains("boss_defeated"),
            "hover must mention the symbol name; got: {text}"
        );
    }

    #[test]
    fn hover_works_inside_if_block() {
        let src = "global score: int = 0\n\
                   label play {\n\
                   \tif score == 0 {\n\
                   \t\tscore = 1\n\
                   \t}\n\
                   \tend!()\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Cursor on `score` in the assignment inside the if body.
        let offset = src.rfind("score").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset);
        assert!(
            info.is_some(),
            "hover must work for an identifier inside an if block; got None"
        );
        let text = info.unwrap();
        assert!(
            text.contains("score"),
            "hover must mention the symbol name; got: {text}"
        );
    }

    #[test]
    fn hover_on_jump_target_shows_target_label_card() {
        // When the cursor is on `dungeon_epilogue` in `jump dungeon_epilogue`,
        // the hover must show the target label's card, not the enclosing label.
        let src = "label dungeon_epilogue {\n\
                   \tend!()\n\
                   }\n\
                   label boss_fight {\n\
                   \tjump dungeon_epilogue\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Cursor on `dungeon_epilogue` in the jump statement (last occurrence).
        let offset = src.rfind("dungeon_epilogue").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset);
        assert!(
            info.is_some(),
            "hover on jump target must produce hover text; got None"
        );
        let text = info.unwrap();
        assert!(
            text.contains("dungeon_epilogue"),
            "hover must show the TARGET label, not the enclosing one; got: {text}"
        );
        assert!(
            !text.contains("boss_fight"),
            "hover must NOT show the enclosing label; got: {text}"
        );
    }

    #[test]
    fn hover_on_jump_keyword_does_not_crash() {
        // Hovering on the `jump` keyword itself (not the target name) should
        // not crash and may return None (keyword suppression) or the enclosing
        // label's hover card — both are acceptable.
        let src = "label dungeon_epilogue {\n\
                   \tend!()\n\
                   }\n\
                   label dungeon {\n\
                   \tjump dungeon_epilogue\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Cursor on the `jump` keyword.
        let jump_offset = src.rfind("jump").unwrap() + 1;
        // Must not panic — result can be None or Some(...).
        let _ = hover_info(&ast, &syms, src, jump_offset);
    }

    #[test]
    fn hover_on_jump_shows_enclosing_label_or_target() {
        // Hovering on `dungeon_epilogue` after `jump` — since Jump nodes store
        // the target as a String (not an AST child), hover_from_ast_children
        // returns None for Jump, so the enclosing label's hover card appears.
        // The important thing is no panic and some meaningful output.
        let src = "label dungeon_epilogue {\n\
                   \tend!()\n\
                   }\n\
                   label dungeon {\n\
                   \tjump dungeon_epilogue\n\
                   }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);

        // Cursor on `dungeon_epilogue` in the jump statement (last occurrence).
        let offset = src.rfind("dungeon_epilogue").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset);
        // Some hover is expected (the enclosing `dungeon` label's card).
        assert!(
            info.is_some(),
            "hovering near a jump target must produce some hover text; got None"
        );
        // The result shows either the target or the enclosing label — either
        // is a `label` urd code block.
        let text = info.unwrap();
        assert!(
            text.contains("label"),
            "hover near jump must show a label hover card; got: {text}"
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

    #[test]
    fn find_references_ident_path_does_not_match_middle_segment() {
        let src = "label start {\n  let value = alpha.beta.gamma\n  end!()\n}\n";
        let ast = parse(src);

        let refs = find_references(&ast, "beta");
        assert!(
            refs.is_empty(),
            "middle segment 'beta' must not match IdentPath references; got {refs:?}"
        );
    }

    // ── find_rename_spans ────────────────────────────────────────────────

    #[test]
    fn rename_spans_covers_definition_and_usage() {
        let src = "label start {\n  end!()\n}\njump start\n";
        let ast = parse(src);
        let spans = find_rename_spans(&ast, src, "start");
        assert!(
            spans.len() >= 2,
            "expected at least 2 rename spans for 'start', got {}",
            spans.len()
        );
        // Every span should contain exactly the text "start".
        for sp in &spans {
            assert_eq!(&src[sp.start..sp.end], "start");
        }
    }

    #[test]
    fn rename_spans_for_variable() {
        let src = "label a {\n  let score = 10\n  let doubled = score\n  end!()\n}\n";
        let ast = parse(src);
        let spans = find_rename_spans(&ast, src, "score");
        assert!(
            spans.len() >= 2,
            "expected at least 2 rename spans for 'score', got {}",
            spans.len()
        );
        for sp in &spans {
            assert_eq!(&src[sp.start..sp.end], "score");
        }
    }

    #[test]
    fn rename_spans_no_duplicates() {
        let src = "label start {\n  end!()\n}\njump start\n";
        let ast = parse(src);
        let spans = find_rename_spans(&ast, src, "start");
        // Check no two spans have the same start offset.
        for (i, a) in spans.iter().enumerate() {
            for b in spans.iter().skip(i + 1) {
                assert_ne!(a.start, b.start, "duplicate span at offset {}", a.start);
            }
        }
    }

    #[test]
    fn rename_spans_unknown_name_is_empty() {
        let src = "label a {\n  end!()\n}\n";
        let ast = parse(src);
        let spans = find_rename_spans(&ast, src, "nonexistent");
        assert!(spans.is_empty());
    }

    #[test]
    fn rename_spans_covers_struct_type_annotation() {
        let src = "struct Player {\n  name: str\n}\nlabel start {\n  let hero: Player = :{ name: \"Ada\" }\n  end!()\n}\n";
        let ast = parse(src);
        let spans = find_rename_spans(&ast, src, "Player");
        assert!(
            spans.len() >= 2,
            "expected at least 2 rename spans for 'Player', got {}",
            spans.len()
        );
        for sp in &spans {
            assert_eq!(&src[sp.start..sp.end], "Player");
        }
    }

    #[test]
    fn rename_spans_struct_field() {
        let src = "struct Player {\n  name: str\n  health: int\n}\nlabel start {\n  let hero: Player = :{ name: \"Ada\", health: 100 }\n  end!()\n}\n";
        let ast = parse(src);
        let spans = find_rename_spans(&ast, src, "health");
        assert!(
            spans.len() >= 2,
            "expected at least 2 rename spans for 'health', got {}",
            spans.len()
        );
        for sp in &spans {
            assert_eq!(&src[sp.start..sp.end], "health");
        }
    }

    #[test]
    fn find_struct_for_field_returns_struct_name() {
        let src = "struct Vec2 {\n  x: float\n  y: float\n}\n";
        let ast = parse(src);
        assert_eq!(find_struct_for_field(&ast, "x"), Some("Vec2".into()));
        assert_eq!(find_struct_for_field(&ast, "z"), None);
    }

    // ── semantic_tokens ──────────────────────────────────────────────

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

    // ── doc comment tests ────────────────────────────────────────────────

    #[test]
    fn doc_comment_attached_to_label() {
        let src = "## The intro scene\nlabel intro {\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let intro = syms.iter().find(|s| s.name == "intro").unwrap();
        assert_eq!(
            intro.doc_comment.as_deref(),
            Some("The intro scene"),
            "doc comment should be attached to the label symbol"
        );
    }

    #[test]
    fn doc_comment_attached_to_variable() {
        let src = "## The player's health\nlet health = 100\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "health").unwrap();
        assert_eq!(sym.doc_comment.as_deref(), Some("The player's health"),);
    }

    #[test]
    fn doc_comment_multiline_joined_with_newline() {
        let src = "## First line\n## Second line\nconst MAX = 10\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "MAX").unwrap();
        assert_eq!(
            sym.doc_comment.as_deref(),
            Some("First line\nSecond line"),
            "multiple ## lines should be joined with newline"
        );
    }

    #[test]
    fn doc_comment_not_attached_when_absent() {
        let src = "let x = 1\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "x").unwrap();
        assert!(
            sym.doc_comment.is_none(),
            "no doc comment should be attached when none is written"
        );
    }

    #[test]
    fn doc_comment_attached_to_enum() {
        let src = "## The player's faction\nenum Faction { Guild, Empire }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "Faction").unwrap();
        assert_eq!(sym.doc_comment.as_deref(), Some("The player's faction"));
    }

    #[test]
    fn doc_comment_attached_to_struct() {
        let src = "## Represents a character\nstruct Character { name: str }\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "Character").unwrap();
        assert_eq!(sym.doc_comment.as_deref(), Some("Represents a character"));
    }

    #[test]
    fn doc_comment_attached_to_decorator() {
        let src = "## Plays a sound effect\ndecorator sfx(sound: str) {\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "sfx").unwrap();
        assert_eq!(sym.doc_comment.as_deref(), Some("Plays a sound effect"));
    }

    #[test]
    fn hover_shows_doc_comment_for_label() {
        let src = "## The intro scene\nlabel intro {\n  end!()\n}\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        // Find "intro" after "label " to avoid matching "intro" inside the doc comment text.
        let offset = src.find("label intro").unwrap() + "label ".len() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("The intro scene"),
            "hover should include the doc comment; got: {info}"
        );
        assert!(
            info.contains("label intro"),
            "hover should still include the label signature; got: {info}"
        );
    }

    #[test]
    fn hover_shows_doc_comment_for_variable() {
        let src = "## The player's health\nlet health = 100\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        // Find "health" after "let " to avoid matching it inside the doc comment text.
        let offset = src.find("let health").unwrap() + "let ".len() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("The player's health"),
            "hover should contain doc comment; got: {info}"
        );
    }

    #[test]
    fn hover_doc_comment_separated_by_horizontal_rule() {
        let src = "## Some docs\nconst LIMIT = 42\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("LIMIT").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        // The separator "---" should appear between the code block and the doc text.
        assert!(
            info.contains("---"),
            "hover should contain a horizontal rule separator; got: {info}"
        );
        assert!(
            info.contains("Some docs"),
            "hover should contain the doc comment text; got: {info}"
        );
    }

    #[test]
    fn collect_symbols_finds_extern_const() {
        let src = "extern narrator: Character\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "narrator").unwrap();
        assert_eq!(sym.kind, SymbolKind::Variable);
        assert_eq!(
            sym.type_annotation,
            Some(TypeAnnotation::Named(vec!["Character".to_owned()]))
        );
        assert!(
            sym.detail
                .as_deref()
                .unwrap_or("")
                .contains("extern narrator"),
            "detail should show extern keyword; got: {:?}",
            sym.detail
        );
    }

    #[test]
    fn collect_symbols_finds_extern_global() {
        let src = "extern score: int\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "score").unwrap();
        assert_eq!(sym.kind, SymbolKind::Variable);
        assert_eq!(sym.type_annotation, Some(TypeAnnotation::Int));
        assert!(
            sym.detail.as_deref().unwrap_or("").contains("extern score"),
            "detail should show extern; got: {:?}",
            sym.detail
        );
    }

    #[test]
    fn collect_symbols_finds_extern_without_annotation() {
        let src = "extern narrator\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "narrator").unwrap();
        assert_eq!(sym.kind, SymbolKind::Variable);
        assert!(sym.type_annotation.is_none());
        assert!(
            sym.detail
                .as_deref()
                .unwrap_or("")
                .contains("extern narrator"),
            "detail should still show name; got: {:?}",
            sym.detail
        );
    }

    #[test]
    fn find_definition_extern_const() {
        let src = "extern narrator: Character\n";
        let ast = parse(src);
        let span = find_definition(&ast, "narrator");
        assert!(span.is_some(), "should find definition for extern");
    }

    #[test]
    fn find_definition_extern_global() {
        let src = "extern score: int\n";
        let ast = parse(src);
        let span = find_definition(&ast, "score");
        assert!(span.is_some(), "should find definition for extern");
    }

    #[test]
    fn hover_extern_const_shows_type() {
        let src = "extern narrator: Character\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("narrator").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("narrator"),
            "hover should show name; got: {info}"
        );
        assert!(
            info.contains("Character"),
            "hover should show type annotation; got: {info}"
        );
        assert!(
            info.contains("extern"),
            "hover should show extern keyword; got: {info}"
        );
    }

    #[test]
    fn hover_extern_global_shows_type() {
        let src = "extern score: int\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("score").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("score"),
            "hover should show name; got: {info}"
        );
        assert!(
            info.contains("int"),
            "hover should show type annotation; got: {info}"
        );
    }

    #[test]
    fn hover_extern_with_doc_comment() {
        let src = "## The active narrator character\nextern narrator: Character\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let offset = src.find("narrator: Character").unwrap() + 1;
        let info = hover_info(&ast, &syms, src, offset).unwrap();
        assert!(
            info.contains("The active narrator character"),
            "hover should include doc comment; got: {info}"
        );
    }

    #[test]
    fn regular_comment_does_not_become_doc_comment() {
        // A single-# comment must NOT be attached as a doc comment.
        let src = "# not a doc comment\nlet x = 1\n";
        let ast = parse(src);
        let syms = collect_symbols(&ast);
        let sym = syms.iter().find(|s| s.name == "x").unwrap();
        assert!(
            sym.doc_comment.is_none(),
            "single-# comment must not be treated as a doc comment"
        );
    }
}
