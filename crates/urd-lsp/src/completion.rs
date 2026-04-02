//! Context-sensitive completion for the Urd LSP.
//!
//! This module provides:
//!
//! - [`CompletionContext`] — an enum describing what kind of completion is
//!   appropriate at a given cursor position.
//! - [`TypeCompatibility`] — a enum that constrains which variable types are
//!   admissible on the right-hand side of a binary operator.
//! - [`detect_completion_context`] — a purely text-based heuristic that
//!   classifies the cursor position into a [`CompletionContext`] by scanning
//!   backward through the source bytes.
//! - [`completion_items`] — the LSP-facing entry point; replaces the previous
//!   flat symbol dump with context-aware filtering.
//!
//! # Detection strategy
//!
//! Completion is triggered **before** the source is re-parsed, so we cannot
//! rely on a complete, valid AST at the cursor position.  The algorithm scans
//! backward from `byte_offset` in the raw source string, looking for syntactic
//! clues.  Cases are tried in priority order (first match wins):
//!
//! 1. `@` immediately before cursor (possibly with a partial ident tail) →
//!    [`CompletionContext::AfterAt`]
//! 2. `.` before cursor (possibly with a partial ident tail) →
//!    [`CompletionContext::ModuleAccess`] /
//!    [`CompletionContext::StructFieldAccess`] /
//!    [`CompletionContext::EnumVariantAccess`]
//! 3. `jump` keyword before cursor → [`CompletionContext::JumpTarget`]
//! 4. Binary operator before cursor → [`CompletionContext::OperatorRhs`]
//! 5. Fallback → [`CompletionContext::General`]
//!
//! # Symbol storage conventions used by filters
//!
//! Several item generators rely on the following storage invariants that are
//! established by [`crate::semantic::collect_symbols`]:
//!
//! | Symbol | `kind` | `span` | `detail` |
//! |--------|--------|--------|---------|
//! | Struct field | `Variable` | `0..0` | `"StructName.field"` |
//! | Enum variant | `EnumVariant` | any | `"EnumName.Variant"` |
//! | Imported symbol (whole-module) | any | `0..0` | original detail |
//!
//! Whole-module imported symbols are prefixed by the module alias
//! (e.g. `chars.Faction`), but their `detail` retains the local form
//! (e.g. `"enum Faction { … }"`).

use std::collections::HashMap;

use urd::parser::ast::{Ast, StructField, TypeAnnotation};

use crate::semantic::{Symbol, SymbolKind};

// ── TypeContext ───────────────────────────────────────────────────────────────

/// Cross-file type information threaded into completion so that struct fields
/// and enum variants resolve even when the defining module was only partially
/// imported (e.g. `import (narrator) from "characters.urd"` rather than a
/// whole-module `import "characters.urd" as chars`).
///
/// The maps mirror the output of
/// [`WorkspaceIndex::imported_type_context`](crate::workspace::WorkspaceIndex::imported_type_context):
///
/// - `structs`: `"StructName"` / `"alias.StructName"` → field list
/// - `enums`:   `"EnumName"`  / `"alias.EnumName"`  → variant list
#[derive(Debug, Default)]
pub struct TypeContext {
    pub structs: HashMap<String, Vec<StructField>>,
    pub enums: HashMap<String, Vec<String>>,
}

// ── Keywords ──────────────────────────────────────────────────────────────────

/// Keywords offered in general (unspecialised) completion.
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

/// All opt-in lint names accepted by the `@lint(…)` built-in decorator.
///
/// Each entry corresponds to an analysis pass that is gated behind an explicit
/// opt-in on a label (e.g. `@lint(check_loops)`).  This list must be kept in
/// sync with the analysis passes in `urd::analysis`.
const BUILTIN_LINTS: &[&str] = &["check_loops"];

// ── TypeCompatibility ─────────────────────────────────────────────────────────

/// Describes which symbol types are admissible on the right-hand side of a
/// binary operator.
///
/// Produced by [`operator_compatible_type`] and consumed by
/// [`items_for_context`] to filter candidates for
/// [`CompletionContext::OperatorRhs`].
#[derive(Debug, Clone, PartialEq)]
pub enum TypeCompatibility {
    /// `+`, `-`, `*`, `/`, `//`, `%` — accepts `int` or `float` symbols.
    Numeric,
    /// `and`, `&&`, `or`, `||` — accepts `bool` symbols only.
    Bool,
    /// `&`, `|`, `^`, `<<`, `>>` — accepts `int` symbols only.
    Integer,
    /// `==`, `!=`, `>`, `<`, `>=`, `<=` — accepts symbols whose declared type
    /// matches the LHS type.
    SameAs(TypeAnnotation),
    /// `=` (assignment) — accepts symbols whose declared type matches the LHS
    /// variable's declared type.
    AssignTo(TypeAnnotation),
    /// Fallback when the LHS type cannot be resolved: offer every symbol that
    /// carries *any* declared type annotation.
    AnyTyped,
}

// ── CompletionContext ─────────────────────────────────────────────────────────

/// The syntactic context at the cursor position, used to narrow completion
/// candidates to a relevant subset.
///
/// Produced by [`detect_completion_context`] and consumed by
/// [`items_for_context`].
#[derive(Debug, Clone, PartialEq)]
pub enum CompletionContext {
    /// Cursor follows `jump ` (or `let name = jump `).
    ///
    /// Only [`SymbolKind::Label`] symbols should be offered.
    ///
    /// Patterns:
    /// ```text
    /// jump |
    /// jump fo|
    /// let result = jump |
    /// let x = jump bar_|
    /// ```
    JumpTarget,

    /// Cursor follows `alias.` where `alias` resolves to an
    /// [`SymbolKind::Import`] symbol.
    ///
    /// All symbols whose name starts with `"{alias}."` are returned with the
    /// `"{alias}."` prefix stripped, so the editor inserts only the member
    /// name.
    ///
    /// Patterns:
    /// ```text
    /// chars.|
    /// village.sq|
    /// ```
    ModuleAccess {
        /// The module alias as written in source (the text before the dot).
        alias: String,
    },

    /// Cursor follows `varname.` (or `varname.partial`) where `varname` is a
    /// variable / constant / global whose declared type is
    /// `Named([…, struct_name])`.
    ///
    /// Only the fields of the named struct are offered.
    ///
    /// Patterns:
    /// ```text
    /// hero.|
    /// hero.na|
    /// chars.narrator.|   (qualified cross-module struct variable)
    /// ```
    StructFieldAccess {
        /// The symbol name that carries the struct value (may be qualified,
        /// e.g. `"chars.narrator"`).
        var_name: String,
        /// The resolved struct type name (leaf segment only, e.g.
        /// `"Character"`).
        struct_name: String,
    },

    /// Cursor follows `EnumName.` (or `EnumName.partial`) where `EnumName`
    /// resolves to an [`SymbolKind::Enum`] symbol.
    ///
    /// Only the variants of that enum are offered.
    ///
    /// Patterns:
    /// ```text
    /// Faction.|
    /// Direction.No|
    /// chars.Faction.|   (qualified cross-module enum)
    /// ```
    EnumVariantAccess {
        /// The full symbol name of the enum (may be qualified, e.g.
        /// `"chars.Faction"` or plain `"Faction"`).
        enum_name: String,
    },

    /// Cursor is inside the argument list of a `@lint(…)` decorator (possibly
    /// with a partial lint name already typed).
    ///
    /// Only the known built-in lint names are offered (e.g. `check_loops`).
    ///
    /// Patterns:
    /// ```text
    /// @lint(|
    /// @lint(check_|
    /// ```
    AfterLintParen,

    /// Cursor is immediately after `@` (possibly with a partial decorator name
    /// already typed).
    ///
    /// Only [`SymbolKind::Decorator`] symbols are offered.
    ///
    /// Patterns:
    /// ```text
    /// @|
    /// @ent|
    /// ```
    AfterAt,

    /// Cursor is on the right-hand side of a binary operator.
    ///
    /// [`TypeCompatibility`] constrains which symbols are relevant.  `lhs_name`
    /// carries the resolved left-hand identifier (if detectable) so the caller
    /// can display it in documentation; `compatibility` is what drives
    /// filtering.
    ///
    /// Patterns:
    /// ```text
    /// score + |
    /// hp == |
    /// active and |
    /// x = |
    /// flags & |
    /// ```
    OperatorRhs {
        /// The operator string as it appears in source (e.g. `"+"`, `"=="`,
        /// `"and"`, `">>"`).
        operator: String,
        /// The left-hand identifier, if one could be detected.
        lhs_name: Option<String>,
        /// Type constraint that filters RHS candidates.
        compatibility: TypeCompatibility,
    },

    /// No special context detected.
    ///
    /// All symbols plus the keyword list are offered.
    General,
}

// ── Public API ────────────────────────────────────────────────────────────────

/// Classify the cursor position in `src` at `byte_offset` into a
/// [`CompletionContext`].
///
/// `symbols` must be the **full** combined symbol list already assembled by the
/// caller (local symbols + aliased imported symbols), because the classifier
/// resolves identifier kinds and type annotations.
///
/// When `byte_offset` is past the end of `src` it is silently clamped to
/// `src.len()`.
pub fn detect_completion_context(
    src: &str,
    byte_offset: usize,
    symbols: &[Symbol],
) -> CompletionContext {
    let offset = byte_offset.min(src.len());
    let before = &src[..offset];

    tracing::debug!(
        "detect_completion_context: offset={offset} before_tail={:?}",
        before
            .get(before.len().saturating_sub(30)..)
            .unwrap_or(before)
    );

    // 1. `@lint(` argument — must be checked before AfterAt so that
    //    `@lint(ch|` is not swallowed by the bare `@` detector.
    if detect_after_lint_paren(before) {
        tracing::debug!("detect_completion_context: matched AfterLintParen");
        return CompletionContext::AfterLintParen;
    }

    // 2. `@` immediately before cursor.
    if detect_after_at(before) {
        tracing::debug!("detect_completion_context: matched AfterAt");
        return CompletionContext::AfterAt;
    }

    // 3. Dot-triggered contexts (module access / struct fields / enum variants).
    if let Some(ctx) = detect_dot_context(before, symbols) {
        tracing::debug!("detect_completion_context: matched dot context → {ctx:?}");
        return ctx;
    }

    // 4. `jump` keyword before cursor.
    if detect_jump_keyword(before) {
        tracing::debug!("detect_completion_context: matched JumpTarget");
        return CompletionContext::JumpTarget;
    }

    // 5. Binary operator RHS.
    if let Some(ctx) = detect_operator_rhs(before, symbols) {
        tracing::debug!("detect_completion_context: matched OperatorRhs → {ctx:?}");
        return ctx;
    }

    // 6. Fallback.
    tracing::debug!("detect_completion_context: fallback → General");
    CompletionContext::General
}

/// Produce completion candidates relevant at `byte_offset` in `src`.
///
/// This is the LSP-facing entry point.  It delegates to
/// [`detect_completion_context`] and then to [`items_for_context`].
///
/// The returned tuples are `(label, kind)` pairs; the LSP handler maps them to
/// `CompletionItem` values.
pub fn completion_items(
    _ast: &Ast,
    symbols: &[Symbol],
    byte_offset: usize,
    src: &str,
    type_ctx: &TypeContext,
) -> Vec<(String, SymbolKind)> {
    // Log a window of source around the cursor so we can see what the
    // detector is actually working with.
    let window_start = byte_offset.saturating_sub(40);
    let window_end = (byte_offset + 10).min(src.len());
    let window = src.get(window_start..window_end).unwrap_or("<oob>");
    tracing::debug!(
        "completion_items: byte_offset={byte_offset} src_len={} \
         symbols={} structs={} enums={} window={window:?}",
        src.len(),
        symbols.len(),
        type_ctx.structs.len(),
        type_ctx.enums.len(),
    );
    tracing::debug!(
        "completion_items: struct keys={:?}",
        type_ctx.structs.keys().collect::<Vec<_>>()
    );

    let ctx = detect_completion_context(src, byte_offset, symbols);
    tracing::debug!("completion_items: resolved context → {ctx:?}");

    let items = items_for_context(&ctx, symbols, type_ctx);
    tracing::debug!("completion_items: returning {} candidates", items.len());
    items
}

/// Produce the completion item list for a resolved [`CompletionContext`].
///
/// This is intentionally separate from [`completion_items`] so that callers
/// with an already-classified context (e.g. tests, IDE tooling) can skip the
/// detection step.
///
/// `type_ctx` supplies cross-file struct/enum definitions used as a fallback
/// when the symbol list does not contain field/variant sentinel entries — which
/// happens when a struct or enum type was only partially imported (symbol
/// import rather than whole-module import).
pub fn items_for_context(
    ctx: &CompletionContext,
    symbols: &[Symbol],
    type_ctx: &TypeContext,
) -> Vec<(String, SymbolKind)> {
    match ctx {
        CompletionContext::JumpTarget => items_jump_targets(symbols),
        CompletionContext::AfterLintParen => items_lint_args(),
        CompletionContext::AfterAt => items_decorators(symbols),
        CompletionContext::ModuleAccess { alias } => items_module_members(alias, symbols),
        CompletionContext::StructFieldAccess { struct_name, .. } => {
            items_struct_fields(struct_name, symbols, type_ctx)
        }
        CompletionContext::EnumVariantAccess { enum_name } => {
            items_enum_variants(enum_name, symbols, type_ctx)
        }
        CompletionContext::OperatorRhs { compatibility, .. } => {
            items_operator_rhs(compatibility, symbols)
        }
        CompletionContext::General => items_general(symbols),
    }
}

// ── Type-compatibility helpers ────────────────────────────────────────────────

/// Map an operator string (as it appears in source) to the
/// [`TypeCompatibility`] that applies to its right-hand operand.
///
/// `lhs_type` is the declared type of the left-hand identifier.  When
/// provided, comparison and assignment operators are narrowed to
/// [`TypeCompatibility::SameAs`] / [`TypeCompatibility::AssignTo`];
/// otherwise they fall back to [`TypeCompatibility::AnyTyped`].
///
/// Returns `None` when `op_str` is not a recognised binary operator (which
/// prevents triggering operator-RHS completion at all).
pub fn operator_compatible_type(
    op_str: &str,
    lhs_type: Option<&TypeAnnotation>,
) -> Option<TypeCompatibility> {
    let compat = match op_str {
        // ── Arithmetic ────────────────────────────────────────────────────
        "+" | "-" | "*" | "/" | "//" | "%" => TypeCompatibility::Numeric,

        // ── Logical ──────────────────────────────────────────────────────
        "and" | "&&" | "or" | "||" => TypeCompatibility::Bool,

        // ── Bitwise ──────────────────────────────────────────────────────
        "&" | "|" | "^" | "<<" | ">>" => TypeCompatibility::Integer,

        // ── Comparison — narrow to LHS type when known ───────────────────
        "==" | "!=" | ">" | "<" | ">=" | "<=" => match lhs_type {
            Some(t) => TypeCompatibility::SameAs(t.clone()),
            None => TypeCompatibility::AnyTyped,
        },

        // ── Assignment — must match the declared type of the LHS variable ─
        "=" => match lhs_type {
            Some(t) => TypeCompatibility::AssignTo(t.clone()),
            None => TypeCompatibility::AnyTyped,
        },

        _ => return None,
    };
    Some(compat)
}

// ── Item generators ───────────────────────────────────────────────────────────

/// Offer only [`SymbolKind::Label`] symbols (for `jump` completion).
fn items_jump_targets(symbols: &[Symbol]) -> Vec<(String, SymbolKind)> {
    deduplicated(
        symbols
            .iter()
            .filter(|s| s.kind == SymbolKind::Label)
            .map(|s| (s.name.clone(), s.kind)),
    )
}

/// Offer only [`SymbolKind::Decorator`] symbols (for `@` completion).
fn items_decorators(symbols: &[Symbol]) -> Vec<(String, SymbolKind)> {
    deduplicated(
        symbols
            .iter()
            .filter(|s| s.kind == SymbolKind::Decorator)
            .map(|s| (s.name.clone(), s.kind)),
    )
}

/// Offer the known built-in lint names (for `@lint(` completion).
///
/// These are returned as [`SymbolKind::Constant`] so the editor renders them
/// with a distinct icon from decorator/label symbols.
fn items_lint_args() -> Vec<(String, SymbolKind)> {
    BUILTIN_LINTS
        .iter()
        .map(|name| ((*name).to_string(), SymbolKind::Constant))
        .collect()
}

/// Offer all symbols from a module, stripping the `"alias."` prefix.
///
/// Whole-module imported symbols are stored as `"alias.original_name"` in the
/// combined symbol list.  We strip the prefix so the editor inserts only the
/// member name.
fn items_module_members(alias: &str, symbols: &[Symbol]) -> Vec<(String, SymbolKind)> {
    let qualified_prefix = format!("{alias}.");
    deduplicated(symbols.iter().filter_map(|s| {
        s.name
            .strip_prefix(&qualified_prefix)
            .map(|local| (local.to_string(), s.kind))
    }))
}

/// Offer the fields of a named struct.
///
/// Struct-field symbols are stored in the combined symbol list with:
/// - `kind == Variable`
/// - `span == 0..0` (zero-length, distinguishing them from real variables)
/// - `detail == Some("{struct_name}.{field_name}")`
///
/// The detail-prefix match ties field symbols to their owning struct so that
/// homonymous fields across different structs don't bleed through.
fn items_struct_fields(
    struct_name: &str,
    symbols: &[Symbol],
    type_ctx: &TypeContext,
) -> Vec<(String, SymbolKind)> {
    // Primary: field sentinel symbols injected by collect_symbols (present when
    // the struct's StructDecl node was visited — either locally defined or
    // brought in via a whole-module import).
    let detail_prefix = format!("{struct_name}.");
    tracing::debug!(
        "items_struct_fields: looking for sentinels with detail prefix {detail_prefix:?} \
         in {} symbols",
        symbols.len()
    );

    let from_symbols: Vec<(String, SymbolKind)> = symbols
        .iter()
        .filter(|s| {
            let is_sentinel = s.kind == SymbolKind::Variable
                && s.span.start == 0
                && s.span.end == 0
                && s.detail
                    .as_deref()
                    .map(|d| d.starts_with(&detail_prefix))
                    .unwrap_or(false);
            if s.span.start == 0 && s.span.end == 0 && s.kind == SymbolKind::Variable {
                tracing::debug!(
                    "items_struct_fields: zero-span Variable name={:?} detail={:?} → sentinel={}",
                    s.name,
                    s.detail,
                    is_sentinel
                );
            }
            is_sentinel
        })
        .map(|s| (s.name.clone(), s.kind))
        .collect();

    tracing::debug!(
        "items_struct_fields: found {} sentinels from symbol list",
        from_symbols.len()
    );

    if !from_symbols.is_empty() {
        return deduplicated(from_symbols.into_iter());
    }

    // Fallback: look up the struct directly in the cross-file type context.
    // This covers the case where the struct was only partially imported
    // (e.g. `import (narrator) from "characters.urd"` — the `Character` struct
    // itself was never added to the symbol list as sentinel fields).
    tracing::debug!(
        "items_struct_fields: falling back to type_ctx for {struct_name:?} \
         (type_ctx has {} struct entries: {:?})",
        type_ctx.structs.len(),
        type_ctx.structs.keys().collect::<Vec<_>>()
    );
    if let Some(fields) = type_ctx.structs.get(struct_name) {
        tracing::debug!(
            "items_struct_fields: type_ctx fallback resolved {struct_name} → {} fields",
            fields.len()
        );
        return deduplicated(
            fields
                .iter()
                .map(|f| (f.name.clone(), SymbolKind::Variable)),
        );
    }

    tracing::debug!(
        "items_struct_fields: {struct_name:?} not found in type_ctx either — returning empty"
    );
    Vec::new()
}

/// Offer the variants of a named enum.
///
/// Handles both plain (`"Faction"`) and qualified (`"chars.Faction"`) enum
/// names uniformly:
///
/// - **Plain**: looks for `EnumVariant` symbols whose `detail` starts with
///   `"Faction."`, returning their unqualified `name`.
/// - **Qualified**: uses the leaf segment (`"Faction"`) as the detail prefix,
///   and additionally filters by the module prefix (`"chars."`) in the symbol
///   name before stripping it from the result.
///
/// Enum-variant symbols are stored as:
/// - `kind == EnumVariant`
/// - `name`: unqualified variant name (`"Guild"`) or module-prefixed
///   (`"chars.Guild"`) for whole-module imports
/// - `detail`: `"EnumName.Variant"` using the *local* enum name (e.g.
///   `"Faction.Guild"` even if the module is `chars`)
fn items_enum_variants(
    enum_name: &str,
    symbols: &[Symbol],
    type_ctx: &TypeContext,
) -> Vec<(String, SymbolKind)> {
    if let Some(dot_pos) = enum_name.rfind('.') {
        // Qualified name, e.g. "chars.Faction".
        let module_prefix = &enum_name[..dot_pos + 1]; // "chars."
        let leaf = &enum_name[dot_pos + 1..]; // "Faction"
        let detail_prefix = format!("{leaf}.");

        let from_symbols: Vec<(String, SymbolKind)> = symbols
            .iter()
            .filter(|s| {
                s.kind == SymbolKind::EnumVariant
                    && s.name.starts_with(module_prefix)
                    && s.detail
                        .as_deref()
                        .map(|d| d.starts_with(&detail_prefix))
                        .unwrap_or(false)
            })
            .map(|s| {
                // Strip the module prefix so the editor inserts just
                // "Guild" rather than "chars.Guild".
                let local = s.name.strip_prefix(module_prefix).unwrap_or(&s.name);
                (local.to_string(), s.kind)
            })
            .collect();

        if !from_symbols.is_empty() {
            return deduplicated(from_symbols.into_iter());
        }

        // Fallback: try both the qualified and unqualified name in the type context.
        for key in &[enum_name, leaf] {
            if let Some(variants) = type_ctx.enums.get(*key) {
                tracing::debug!(
                    "enum variant fallback (qualified): resolved {enum_name} via key '{key}' \
                     from type_ctx ({} variants)",
                    variants.len()
                );
                return deduplicated(
                    variants
                        .iter()
                        .map(|v| (v.clone(), SymbolKind::EnumVariant)),
                );
            }
        }

        return Vec::new();
    }

    // Plain name, e.g. "Faction".
    let detail_prefix = format!("{enum_name}.");
    let from_symbols: Vec<(String, SymbolKind)> = symbols
        .iter()
        .filter(|s| {
            s.kind == SymbolKind::EnumVariant
                && s.detail
                    .as_deref()
                    .map(|d| d.starts_with(&detail_prefix))
                    .unwrap_or(false)
        })
        .map(|s| (s.name.clone(), s.kind))
        .collect();

    if !from_symbols.is_empty() {
        return deduplicated(from_symbols.into_iter());
    }

    // Fallback: look up the enum in the cross-file type context.
    if let Some(variants) = type_ctx.enums.get(enum_name) {
        tracing::debug!(
            "enum variant fallback (plain): resolved {enum_name} from type_ctx ({} variants)",
            variants.len()
        );
        return deduplicated(
            variants
                .iter()
                .map(|v| (v.clone(), SymbolKind::EnumVariant)),
        );
    }

    Vec::new()
}

/// Offer value symbols compatible with the right-hand side of a binary
/// operator.
///
/// Only value-bearing symbol kinds (variable, constant, global, enum variant)
/// are considered; declaration-only kinds (label, struct, decorator, import)
/// are always excluded.
fn items_operator_rhs(
    compatibility: &TypeCompatibility,
    symbols: &[Symbol],
) -> Vec<(String, SymbolKind)> {
    deduplicated(
        symbols
            .iter()
            .filter(|s| is_value_symbol(s) && type_matches(s, compatibility))
            .map(|s| (s.name.clone(), s.kind)),
    )
}

/// Offer all symbols plus the keyword list (fallback context).
fn items_general(symbols: &[Symbol]) -> Vec<(String, SymbolKind)> {
    let mut items: Vec<(String, SymbolKind)> =
        deduplicated(symbols.iter().map(|s| (s.name.clone(), s.kind)));
    for kw in KEYWORDS {
        let entry = ((*kw).to_string(), SymbolKind::Variable);
        if !items.contains(&entry) {
            items.push(entry);
        }
    }
    items
}

// ── Type filtering ────────────────────────────────────────────────────────────

/// Return `true` iff `sym` is a value-bearing symbol kind.
///
/// Labels, structs, decorators, and imports are excluded: they are never
/// admissible as operator operands.
fn is_value_symbol(sym: &Symbol) -> bool {
    matches!(
        sym.kind,
        SymbolKind::Variable | SymbolKind::Constant | SymbolKind::Global | SymbolKind::EnumVariant
    )
}

/// Return `true` iff `sym`'s declared type is compatible with `compat`.
///
/// For `Numeric`, `Bool`, and `Integer`, the symbol must carry the
/// *exact* required annotation.  For `SameAs` / `AssignTo`, types are
/// compared via [`types_compatible`], which also accepts cross-module
/// qualified/unqualified name matches on the leaf segment.  `AnyTyped` passes
/// any symbol that declares *some* type.
fn type_matches(sym: &Symbol, compat: &TypeCompatibility) -> bool {
    match compat {
        TypeCompatibility::Numeric => matches!(
            sym.type_annotation,
            Some(TypeAnnotation::Int) | Some(TypeAnnotation::Float)
        ),
        TypeCompatibility::Bool => {
            matches!(sym.type_annotation, Some(TypeAnnotation::Bool))
        }
        TypeCompatibility::Integer => {
            matches!(sym.type_annotation, Some(TypeAnnotation::Int))
        }
        TypeCompatibility::SameAs(lhs_type) | TypeCompatibility::AssignTo(lhs_type) => sym
            .type_annotation
            .as_ref()
            .map(|t| types_compatible(t, lhs_type))
            .unwrap_or(false),
        TypeCompatibility::AnyTyped => sym.type_annotation.is_some(),
    }
}

/// Structural type equality with a cross-module leaf-name heuristic.
///
/// Two `Named` types are considered compatible if either:
/// - their path segments are identical (`["Faction"] == ["Faction"]`), or
/// - their *last* segment matches (`["Faction"] ≈ ["chars", "Faction"]`).
///
/// This avoids false negatives when one side of a comparison uses a qualified
/// type path and the other uses the unqualified form.
fn types_compatible(a: &TypeAnnotation, b: &TypeAnnotation) -> bool {
    match (a, b) {
        (TypeAnnotation::Named(pa), TypeAnnotation::Named(pb)) => {
            pa == pb || pa.last() == pb.last()
        }
        _ => a == b,
    }
}

// ── Detectors ─────────────────────────────────────────────────────────────────

// ── Detector 1: AfterAt ───────────────────────────────────────────────────────

/// Return `true` if the cursor is inside the argument list of a `@lint(`
/// decorator, optionally with a partial lint name already typed.
///
/// Examples:
/// - `"@lint("` → `true`
/// - `"@lint(check_"` → `true`  (partial name)
/// - `"@lint(check_loops"` → `true`  (full name still completing)
/// - `"@entry"` → `false`
/// - `"@lint"` → `false`  (paren not yet opened)
fn detect_after_lint_paren(before: &str) -> bool {
    // Strip any partial ident the user has already typed inside the parens.
    let stripped = before.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_');
    stripped.ends_with("@lint(")
}

/// Return `true` if the text immediately before the cursor (discarding any
/// partial decorator name the user is typing) ends with `@`.
///
/// Examples:
/// - `"@"` → `true`
/// - `"@ent"` → `true` (cursor inside a decorator name)
/// - `"label foo"` → `false`
/// - `"foo@bar"` → `true` (valid position, `@` in urd is always a decorator)
fn detect_after_at(before: &str) -> bool {
    let stripped = before.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_');
    stripped.ends_with('@')
}

// ── Detector 2: Dot contexts ──────────────────────────────────────────────────

/// Attempt to classify a dot-triggered completion by scanning backward to find
/// the identifier before the dot and resolving it against the symbol list.
///
/// Returns `None` when no dot pattern is found.
///
/// The function first strips any partial identifier the user has already typed
/// after the dot (e.g. `"hero.na"` → strip `"na"` → `"hero."`).  It then
/// extracts the full text before the dot and attempts to resolve it in two
/// passes:
///
/// 1. **Full path**: try `before_dot` verbatim (e.g. `"chars.Faction"` for a
///    qualified cross-module enum).
/// 2. **Leaf segment**: fall back to just the last identifier segment of
///    `before_dot` (e.g. `"Faction"` from `"chars.Faction"` when the fully
///    qualified symbol isn't in the list — handles direct imports).
fn detect_dot_context(before: &str, symbols: &[Symbol]) -> Option<CompletionContext> {
    // Strip any partial field / variant name the user has started typing.
    let before_partial = before.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_');

    if !before_partial.ends_with('.') {
        tracing::debug!("detect_dot_context: no trailing dot — skipping");
        return None;
    }

    // Drop the dot itself.
    let before_dot = &before_partial[..before_partial.len() - 1];

    // Log the last 40 chars of before_dot so we can see what was stripped.
    tracing::debug!(
        "detect_dot_context: before_dot tail={:?}",
        before_dot
            .get(before_dot.len().saturating_sub(40)..)
            .unwrap_or(before_dot)
    );

    // Pass 1: try the full text before the dot.
    if let Some(ctx) = try_resolve_dot_prefix(before_dot, symbols) {
        tracing::debug!("detect_dot_context: pass-1 hit → {ctx:?}");
        return Some(ctx);
    }
    tracing::debug!("detect_dot_context: pass-1 miss (full prefix too long or not a symbol)");

    // Pass 2: try just the last identifier segment (stops at any dot or
    // non-ident character).
    let leaf = scan_back_ident(before_dot);
    tracing::debug!("detect_dot_context: pass-2 leaf={leaf:?}");
    if let Some(leaf) = leaf
        && leaf != before_dot
    {
        // Only retry when the leaf differs from the full path (avoids
        // redundant re-resolution).
        let result = try_resolve_dot_prefix(&leaf, symbols);
        tracing::debug!("detect_dot_context: pass-2 result={result:?}");
        return result;
    }

    tracing::debug!("detect_dot_context: both passes missed — returning None");
    None
}

/// Resolve a dot-prefix string against `symbols` and return the appropriate
/// [`CompletionContext`].
///
/// Priority:
/// 1. [`SymbolKind::Import`] → [`CompletionContext::ModuleAccess`]
/// 2. Variable / Constant / Global with `Named` type annotation →
///    [`CompletionContext::StructFieldAccess`]
/// 3. [`SymbolKind::Enum`] → [`CompletionContext::EnumVariantAccess`]
///
/// Returns `None` when `prefix` is not found in `symbols` or its kind does
/// not support dot-access.
///
/// # Symbol priority
///
/// Multiple symbols can share the same `name`.  The most important case is a
/// *directly-imported symbol* such as `import (narrator) from "chars.urd"`:
/// `collect_symbols` on the local AST emits an `Import`-kinded sentinel for
/// the declaration node itself (`type_annotation: None`), while
/// `workspace.imported_symbols` adds the real `Constant`-kinded symbol with
/// `type_annotation: Some(Named(["Character"]))`.  If we returned on the
/// first match (`Import`), we would produce `ModuleAccess` and find no
/// members — the completion list would be empty.
///
/// Resolution order (first match wins):
/// 1. `Variable | Constant | Global` with a `Named` type annotation → `StructFieldAccess`
/// 2. `Enum` → `EnumVariantAccess`
/// 3. `Import` that has prefixed members in the symbol list (i.e. a real
///    module alias) → `ModuleAccess`
/// 4. Anything else → `None`
fn try_resolve_dot_prefix(prefix: &str, symbols: &[Symbol]) -> Option<CompletionContext> {
    // Collect every symbol that carries this exact name so we can pick the
    // best-suited one rather than blindly taking the first.
    let candidates: Vec<&Symbol> = symbols.iter().filter(|s| s.name == prefix).collect();

    if candidates.is_empty() {
        tracing::debug!("try_resolve_dot_prefix: prefix={prefix:?} — no symbols found");
        return None;
    }

    tracing::debug!(
        "try_resolve_dot_prefix: prefix={prefix:?} — {} candidate(s): {:?}",
        candidates.len(),
        candidates
            .iter()
            .map(|s| (&s.kind, &s.type_annotation))
            .collect::<Vec<_>>()
    );

    // Priority 1: value symbol with a Named type annotation → struct field access.
    for sym in &candidates {
        if matches!(
            sym.kind,
            SymbolKind::Variable | SymbolKind::Constant | SymbolKind::Global
        ) && let Some(TypeAnnotation::Named(parts)) = &sym.type_annotation
            && let Some(struct_name) = parts.last().cloned()
        {
            tracing::debug!(
                "try_resolve_dot_prefix: {prefix:?} → StructFieldAccess(struct_name={struct_name:?})"
            );
            return Some(CompletionContext::StructFieldAccess {
                var_name: prefix.to_string(),
                struct_name,
            });
        }
    }

    // Priority 2: enum symbol → enum variant access.
    for sym in &candidates {
        if sym.kind == SymbolKind::Enum {
            tracing::debug!("try_resolve_dot_prefix: {prefix:?} → EnumVariantAccess");
            return Some(CompletionContext::EnumVariantAccess {
                enum_name: prefix.to_string(),
            });
        }
    }

    // Priority 3: Import symbol — but only when there are actually prefixed
    // members in the symbol list (i.e. it is a *module alias*, not a
    // directly-imported value like `import (narrator) from "chars.urd"`).
    for sym in &candidates {
        if sym.kind == SymbolKind::Import {
            let qualified_prefix = format!("{prefix}.");
            let has_members = symbols
                .iter()
                .any(|s| s.name.starts_with(&qualified_prefix));
            if has_members {
                tracing::debug!(
                    "try_resolve_dot_prefix: {prefix:?} → ModuleAccess (has prefixed members)"
                );
                return Some(CompletionContext::ModuleAccess {
                    alias: prefix.to_string(),
                });
            }
            tracing::debug!(
                "try_resolve_dot_prefix: {prefix:?} is Import but has no prefixed members \
                 — skipping (direct symbol import, not a module alias)"
            );
        }
    }

    tracing::debug!(
        "try_resolve_dot_prefix: {prefix:?} — no dot-accessible kind found among candidates"
    );
    None
}

// ── Detector 3: Jump keyword ──────────────────────────────────────────────────

/// Return `true` if the text before the cursor indicates we are in a jump
/// target position.
///
/// Recognised patterns (reading left-to-right):
/// ```text
/// jump |           → bare jump
/// jump label_pre|  → partial label name after jump
/// let x = jump |   → let-call form
/// ```
///
/// Algorithm (scanning *right-to-left* from cursor):
/// 1. Strip any partial identifier the user has started typing.
/// 2. There must be at least one whitespace character after `jump`.
/// 3. After trimming trailing whitespace, the text must end with the exact
///    word `jump`.
/// 4. The character immediately before `jump` must be a word boundary (space,
///    `=`, newline, or start of text) to avoid matching `nojump`.
fn detect_jump_keyword(before: &str) -> bool {
    // Strip partial label name being typed.
    let after_partial = before.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_');

    // Require at least one space between `jump` and the cursor / partial name.
    if !after_partial.ends_with(|c: char| c.is_ascii_whitespace()) {
        return false;
    }

    let trimmed = after_partial.trim_end();

    if !trimmed.ends_with("jump") {
        return false;
    }

    // Verify word boundary: the character before `jump` must not be an ident char.
    let before_jump = &trimmed[..trimmed.len() - 4];
    before_jump.is_empty()
        || before_jump
            .chars()
            .next_back()
            .map(|c| !c.is_alphanumeric() && c != '_')
            .unwrap_or(true)
}

// ── Detector 4: Operator RHS ──────────────────────────────────────────────────

/// Attempt to detect an operator-RHS context.
///
/// The expected right-to-left scan pattern is:
/// ```text
/// [partial-rhs-ident?] [≥1 space] [operator] [≥0 space] [lhs-ident?] …
/// ```
///
/// Steps:
/// 1. Strip any partial right-hand identifier already typed by the user.
/// 2. Require at least one whitespace character after the operator (guards
///    against triggering mid-token, e.g. `x==y` with no spaces).
/// 3. Trim trailing whitespace to reach the operator end.
/// 4. Consume the operator via [`consume_operator_backward`] (longest-match
///    first to avoid `==` being read as `=` + `=`).
/// 5. Trim whitespace to reach the left-hand identifier.
/// 6. Consume the left-hand identifier via [`scan_back_ident`].
/// 7. Look up the LHS symbol's type and call [`operator_compatible_type`].
fn detect_operator_rhs(before: &str, symbols: &[Symbol]) -> Option<CompletionContext> {
    // 1. Strip any partial RHS identifier.
    let after_rhs = before.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_');

    // 2. Require at least one space after the operator.
    if !after_rhs.ends_with(|c: char| c.is_ascii_whitespace()) {
        return None;
    }

    // 3. Trim trailing whitespace.
    let after_op = after_rhs.trim_end();

    // 4. Consume the operator (longest-match first).
    let (op_str, before_op) = consume_operator_backward(after_op)?;

    // 5. Trim whitespace between LHS and operator.
    let before_op_trimmed = before_op.trim_end();

    // 6. Consume LHS identifier.
    let lhs_name = scan_back_ident(before_op_trimmed);

    // 7. Look up LHS type and compute compatibility.
    let lhs_type = lhs_name
        .as_deref()
        .and_then(|name| find_symbol_type(name, symbols));

    let compatibility = operator_compatible_type(op_str, lhs_type.as_ref())?;

    Some(CompletionContext::OperatorRhs {
        operator: op_str.to_string(),
        lhs_name,
        compatibility,
    })
}

/// Consume a binary operator from the *end* of `s` using longest-match-first.
///
/// Returns `(operator_str, text_before_operator)` on the first match, or
/// `None` if no known operator is found.
///
/// Multi-character operators are tried before single-character ones to prevent
/// `"=="` being misread as `"="`.  Keyword operators (`and`, `or`, `&&`, `||`)
/// additionally require a word boundary immediately to their left so that an
/// identifier ending in `or` (e.g. `color`) is not misidentified.
fn consume_operator_backward(s: &str) -> Option<(&str, &str)> {
    // ── Multi-character operators (tried before single-char) ─────────────
    const MULTI: &[&str] = &[
        "==", "!=", ">=", "<=", "//", "<<", ">>", "&&", "||", "and", "or",
    ];

    for &op in MULTI {
        let Some(rest) = s.strip_suffix(op) else {
            continue;
        };

        // Keyword operators require a word boundary on the left.
        if op.chars().all(|c| c.is_alphabetic()) {
            let boundary = rest.is_empty()
                || rest
                    .chars()
                    .next_back()
                    .map(|c| !c.is_alphanumeric() && c != '_')
                    .unwrap_or(true);
            if !boundary {
                continue;
            }
        }

        return Some((op, rest));
    }

    // ── Single-character operators ────────────────────────────────────────
    const SINGLE: &[&str] = &["+", "-", "*", "/", "%", "=", "&", "|", "^", "<", ">"];

    for &op in SINGLE {
        if let Some(rest) = s.strip_suffix(op) {
            return Some((op, rest));
        }
    }

    None
}

// ── Shared text-scan utilities ────────────────────────────────────────────────

/// Scan backward from the end of `s` and return the identifier-like token at
/// the tail (`[_a-zA-Z0-9]+`).
///
/// Dots are treated as separators (not included), so `"chars.Faction"` yields
/// `"Faction"`.  Returns `None` when the tail of `s` is not an identifier
/// character or `s` is empty.
fn scan_back_ident(s: &str) -> Option<String> {
    if s.is_empty() {
        return None;
    }

    // Walk backward collecting identifier characters.
    let mut start = s.len();
    for (i, c) in s.char_indices().rev() {
        if c.is_alphanumeric() || c == '_' {
            start = i;
        } else {
            break;
        }
    }

    if start == s.len() {
        // No identifier characters at the tail.
        return None;
    }

    let word = &s[start..];
    if word.is_empty() {
        None
    } else {
        Some(word.to_string())
    }
}

/// Look up `name` in `symbols` and return a clone of its declared
/// [`TypeAnnotation`], if any.
///
/// Only value-bearing kinds (variable, constant, global) are considered.
fn find_symbol_type(name: &str, symbols: &[Symbol]) -> Option<TypeAnnotation> {
    symbols
        .iter()
        .find(|s| {
            s.name == name
                && matches!(
                    s.kind,
                    SymbolKind::Variable | SymbolKind::Constant | SymbolKind::Global
                )
        })
        .and_then(|s| s.type_annotation.clone())
}

/// Collect an iterator of `(String, SymbolKind)` pairs into a `Vec`, removing
/// exact duplicates (same name **and** same kind) while preserving insertion
/// order.
fn deduplicated(iter: impl Iterator<Item = (String, SymbolKind)>) -> Vec<(String, SymbolKind)> {
    let mut seen = std::collections::HashSet::new();
    iter.filter(|item| seen.insert(item.clone())).collect()
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use chumsky::span::{SimpleSpan, Span as _};

    use super::*;

    // ── Fixture helpers ───────────────────────────────────────────────────

    /// Create a symbol with a non-zero span (a "real" symbol, not a struct
    /// field placeholder).
    fn sym(name: &str, kind: SymbolKind, ann: Option<TypeAnnotation>) -> Symbol {
        Symbol {
            name: name.to_string(),
            kind,
            span: SimpleSpan::new((), 4..20),
            type_annotation: ann,
            detail: None,
            doc_comment: None,
            is_extern: false,
        }
    }

    /// Create a struct-field placeholder symbol (zero span, detail =
    /// `"StructName.field"`).
    fn field(struct_name: &str, field_name: &str, ann: TypeAnnotation) -> Symbol {
        Symbol {
            name: field_name.to_string(),
            kind: SymbolKind::Variable,
            span: SimpleSpan::new((), 0..0),
            type_annotation: Some(ann),
            detail: Some(format!("{struct_name}.{field_name}")),
            doc_comment: None,
            is_extern: false,
        }
    }

    /// Create an enum-variant symbol with `detail = "EnumName.Variant"`.
    fn variant(enum_name: &str, variant_name: &str) -> Symbol {
        Symbol {
            name: variant_name.to_string(),
            kind: SymbolKind::EnumVariant,
            span: SimpleSpan::new((), 4..20),
            type_annotation: None,
            detail: Some(format!("{enum_name}.{variant_name}")),
            doc_comment: None,
            is_extern: false,
        }
    }

    /// Helper: run `detect_completion_context` and return it.
    fn ctx(src: &str, symbols: &[Symbol]) -> CompletionContext {
        detect_completion_context(src, src.len(), symbols)
    }

    fn empty_type_ctx() -> TypeContext {
        TypeContext::default()
    }

    // ── detect_after_at ───────────────────────────────────────────────────

    #[test]
    fn at_bare_triggers_decorator() {
        assert_eq!(ctx("@", &[]), CompletionContext::AfterAt);
    }

    #[test]
    fn at_with_partial_name_triggers_decorator() {
        // `@ent|` — cursor partway through a decorator name.
        assert_eq!(ctx("@ent", &[]), CompletionContext::AfterAt);
    }

    #[test]
    fn at_with_full_name_still_triggers() {
        assert_eq!(ctx("@entry", &[]), CompletionContext::AfterAt);
    }

    #[test]
    fn no_at_does_not_trigger_decorator() {
        // Plain text with no `@`.
        assert_ne!(ctx("label foo", &[]), CompletionContext::AfterAt);
    }

    #[test]
    fn at_in_string_context_still_triggers_lexically() {
        // The completion engine has no semantic context here; `@` is `@`.
        assert_eq!(ctx("narrator: \"hello @", &[]), CompletionContext::AfterAt);
    }

    // ── detect_after_lint_paren ───────────────────────────────────────────

    #[test]
    fn lint_paren_bare_triggers() {
        assert_eq!(ctx("@lint(", &[]), CompletionContext::AfterLintParen);
    }

    #[test]
    fn lint_paren_with_partial_name_triggers() {
        assert_eq!(ctx("@lint(check_", &[]), CompletionContext::AfterLintParen);
    }

    #[test]
    fn lint_paren_with_full_name_triggers() {
        assert_eq!(
            ctx("@lint(check_loops", &[]),
            CompletionContext::AfterLintParen
        );
    }

    #[test]
    fn lint_no_paren_does_not_trigger_lint_paren() {
        // `@lint` without `(` should fall through to AfterAt.
        assert_eq!(ctx("@lint", &[]), CompletionContext::AfterAt);
    }

    #[test]
    fn lint_paren_takes_priority_over_after_at() {
        // The `@lint(` pattern must win before the bare `@` detector fires.
        assert_eq!(ctx("@lint(ch", &[]), CompletionContext::AfterLintParen);
    }

    #[test]
    fn after_lint_paren_returns_only_lint_names() {
        let items = items_for_context(&CompletionContext::AfterLintParen, &[], &empty_type_ctx());
        assert!(!items.is_empty(), "expected at least one lint name");
        assert!(
            items.iter().any(|(n, _)| n == "check_loops"),
            "check_loops must be present"
        );
        assert!(
            items.iter().all(|(_, k)| *k == SymbolKind::Constant),
            "lint args should be Constant kind"
        );
    }

    // ── detect_jump_keyword ───────────────────────────────────────────────

    #[test]
    fn jump_space_triggers() {
        assert_eq!(ctx("jump ", &[]), CompletionContext::JumpTarget);
    }

    #[test]
    fn jump_with_partial_label_triggers() {
        // Cursor in the middle of the label name.
        assert_eq!(ctx("jump foo", &[]), CompletionContext::JumpTarget);
    }

    #[test]
    fn jump_with_underscored_partial_triggers() {
        assert_eq!(ctx("jump my_lab", &[]), CompletionContext::JumpTarget);
    }

    #[test]
    fn let_call_pattern_triggers() {
        assert_eq!(
            ctx("let result = jump ", &[]),
            CompletionContext::JumpTarget
        );
    }

    #[test]
    fn let_call_with_partial_label_triggers() {
        assert_eq!(
            ctx("let x = jump start_", &[]),
            CompletionContext::JumpTarget
        );
    }

    #[test]
    fn jump_without_trailing_space_does_not_trigger() {
        // Cursor immediately after `jump` with no space: user still typing the keyword.
        assert_ne!(ctx("jump", &[]), CompletionContext::JumpTarget);
    }

    #[test]
    fn nojump_does_not_trigger() {
        // `nojump` contains `jump` but is not the keyword — word boundary check
        // must prevent a false match.
        assert_ne!(ctx("nojump ", &[]), CompletionContext::JumpTarget);
    }

    #[test]
    fn jumpstart_does_not_trigger() {
        // Identifier that starts with jump.
        assert_ne!(ctx("jumpstart ", &[]), CompletionContext::JumpTarget);
    }

    // ── detect_dot_context: ModuleAccess ──────────────────────────────────

    #[test]
    fn dot_after_import_alias_triggers_module_access() {
        // A whole-module import produces both the alias symbol AND prefixed
        // member symbols (e.g. `chars.narrator`).  Both must be present for
        // the `has_members` check to classify this as ModuleAccess rather than
        // assuming it is a directly-imported value (symbol import).
        let syms = vec![
            sym("chars", SymbolKind::Import, None),
            sym("chars.narrator", SymbolKind::Constant, None),
        ];
        assert_eq!(
            ctx("chars.", &syms),
            CompletionContext::ModuleAccess {
                alias: "chars".to_string()
            }
        );
    }

    #[test]
    fn dot_after_import_alias_with_partial_member_triggers_module_access() {
        let syms = vec![
            sym("chars", SymbolKind::Import, None),
            sym("chars.Faction", SymbolKind::Enum, None),
        ];
        assert_eq!(
            ctx("chars.Fac", &syms),
            CompletionContext::ModuleAccess {
                alias: "chars".to_string()
            }
        );
    }

    // ── detect_dot_context: StructFieldAccess ─────────────────────────────

    #[test]
    fn dot_after_typed_const_triggers_struct_field_access() {
        let syms = vec![sym(
            "hero",
            SymbolKind::Constant,
            Some(TypeAnnotation::Named(vec!["Character".to_string()])),
        )];
        assert_eq!(
            ctx("hero.", &syms),
            CompletionContext::StructFieldAccess {
                var_name: "hero".to_string(),
                struct_name: "Character".to_string(),
            }
        );
    }

    #[test]
    fn dot_after_typed_variable_triggers_struct_field_access() {
        let syms = vec![sym(
            "player",
            SymbolKind::Variable,
            Some(TypeAnnotation::Named(vec!["Stats".to_string()])),
        )];
        assert_eq!(
            ctx("player.", &syms),
            CompletionContext::StructFieldAccess {
                var_name: "player".to_string(),
                struct_name: "Stats".to_string(),
            }
        );
    }

    #[test]
    fn dot_with_partial_field_triggers_struct_field_access() {
        let syms = vec![sym(
            "hero",
            SymbolKind::Constant,
            Some(TypeAnnotation::Named(vec!["Character".to_string()])),
        )];
        assert_eq!(
            ctx("hero.na", &syms),
            CompletionContext::StructFieldAccess {
                var_name: "hero".to_string(),
                struct_name: "Character".to_string(),
            }
        );
    }

    #[test]
    fn dot_after_global_with_named_type_triggers_struct_field_access() {
        let syms = vec![sym(
            "g",
            SymbolKind::Global,
            Some(TypeAnnotation::Named(vec!["Config".to_string()])),
        )];
        assert_eq!(
            ctx("g.", &syms),
            CompletionContext::StructFieldAccess {
                var_name: "g".to_string(),
                struct_name: "Config".to_string(),
            }
        );
    }

    // ── detect_dot_context: EnumVariantAccess ─────────────────────────────

    #[test]
    fn dot_after_enum_triggers_enum_variant_access() {
        let syms = vec![sym("Faction", SymbolKind::Enum, None)];
        assert_eq!(
            ctx("Faction.", &syms),
            CompletionContext::EnumVariantAccess {
                enum_name: "Faction".to_string()
            }
        );
    }

    #[test]
    fn dot_with_partial_variant_triggers_enum_variant_access() {
        let syms = vec![sym("Direction", SymbolKind::Enum, None)];
        assert_eq!(
            ctx("Direction.No", &syms),
            CompletionContext::EnumVariantAccess {
                enum_name: "Direction".to_string()
            }
        );
    }

    #[test]
    fn dot_after_qualified_enum_triggers_enum_variant_access() {
        // `chars.Faction.` → pass 1 finds "chars.Faction" as Enum directly.
        let syms = vec![sym("chars.Faction", SymbolKind::Enum, None)];
        assert_eq!(
            ctx("chars.Faction.", &syms),
            CompletionContext::EnumVariantAccess {
                enum_name: "chars.Faction".to_string()
            }
        );
    }

    #[test]
    fn dot_after_unknown_prefix_falls_through_to_general() {
        assert_eq!(ctx("unknown.", &[]), CompletionContext::General);
    }

    #[test]
    fn dot_after_label_falls_through_labels_are_not_dot_accessible() {
        let syms = vec![sym("intro", SymbolKind::Label, None)];
        // Labels don't support `.` access.
        assert_eq!(ctx("intro.", &syms), CompletionContext::General);
    }

    // ── detect_operator_rhs ───────────────────────────────────────────────

    #[test]
    fn arithmetic_plus_triggers_numeric() {
        let syms = vec![sym(
            "score",
            SymbolKind::Variable,
            Some(TypeAnnotation::Int),
        )];
        assert_eq!(
            ctx("score + ", &syms),
            CompletionContext::OperatorRhs {
                operator: "+".to_string(),
                lhs_name: Some("score".to_string()),
                compatibility: TypeCompatibility::Numeric,
            }
        );
    }

    #[test]
    fn arithmetic_minus_triggers_numeric() {
        let syms = vec![sym("hp", SymbolKind::Variable, Some(TypeAnnotation::Float))];
        assert_eq!(
            ctx("hp - ", &syms),
            CompletionContext::OperatorRhs {
                operator: "-".to_string(),
                lhs_name: Some("hp".to_string()),
                compatibility: TypeCompatibility::Numeric,
            }
        );
    }

    #[test]
    fn floor_div_triggers_numeric() {
        let syms = vec![sym("n", SymbolKind::Variable, Some(TypeAnnotation::Int))];
        assert_eq!(
            ctx("n // ", &syms),
            CompletionContext::OperatorRhs {
                operator: "//".to_string(),
                lhs_name: Some("n".to_string()),
                compatibility: TypeCompatibility::Numeric,
            }
        );
    }

    #[test]
    fn logical_and_triggers_bool() {
        let syms = vec![sym(
            "active",
            SymbolKind::Variable,
            Some(TypeAnnotation::Bool),
        )];
        assert_eq!(
            ctx("active and ", &syms),
            CompletionContext::OperatorRhs {
                operator: "and".to_string(),
                lhs_name: Some("active".to_string()),
                compatibility: TypeCompatibility::Bool,
            }
        );
    }

    #[test]
    fn logical_or_triggers_bool() {
        // `or` is a logical operator: RHS compatibility is always Bool,
        // regardless of whether the LHS symbol is resolvable. The LHS type
        // does not influence the fixed-type group (`Numeric` / `Bool` / `Integer`).
        assert_eq!(
            ctx("flag or ", &[]),
            CompletionContext::OperatorRhs {
                operator: "or".to_string(),
                lhs_name: Some("flag".to_string()),
                compatibility: TypeCompatibility::Bool,
            }
        );
    }

    #[test]
    fn comparison_with_known_lhs_gives_same_as() {
        let syms = vec![sym("hp", SymbolKind::Variable, Some(TypeAnnotation::Int))];
        assert_eq!(
            ctx("hp == ", &syms),
            CompletionContext::OperatorRhs {
                operator: "==".to_string(),
                lhs_name: Some("hp".to_string()),
                compatibility: TypeCompatibility::SameAs(TypeAnnotation::Int),
            }
        );
    }

    #[test]
    fn comparison_with_unknown_lhs_gives_any_typed() {
        assert_eq!(
            ctx("mystery_var != ", &[]),
            CompletionContext::OperatorRhs {
                operator: "!=".to_string(),
                lhs_name: Some("mystery_var".to_string()),
                compatibility: TypeCompatibility::AnyTyped,
            }
        );
    }

    #[test]
    fn assignment_with_known_lhs_gives_assign_to() {
        let syms = vec![sym("x", SymbolKind::Variable, Some(TypeAnnotation::Float))];
        assert_eq!(
            ctx("x = ", &syms),
            CompletionContext::OperatorRhs {
                operator: "=".to_string(),
                lhs_name: Some("x".to_string()),
                compatibility: TypeCompatibility::AssignTo(TypeAnnotation::Float),
            }
        );
    }

    #[test]
    fn bitwise_and_triggers_integer() {
        let syms = vec![sym(
            "flags",
            SymbolKind::Variable,
            Some(TypeAnnotation::Int),
        )];
        assert_eq!(
            ctx("flags & ", &syms),
            CompletionContext::OperatorRhs {
                operator: "&".to_string(),
                lhs_name: Some("flags".to_string()),
                compatibility: TypeCompatibility::Integer,
            }
        );
    }

    #[test]
    fn bitwise_left_shift_triggers_integer() {
        let syms = vec![sym("mask", SymbolKind::Variable, Some(TypeAnnotation::Int))];
        assert_eq!(
            ctx("mask << ", &syms),
            CompletionContext::OperatorRhs {
                operator: "<<".to_string(),
                lhs_name: Some("mask".to_string()),
                compatibility: TypeCompatibility::Integer,
            }
        );
    }

    #[test]
    fn double_equals_not_confused_with_single_assign() {
        // `==` must be consumed as one unit, not `=` + `=`.
        let syms = vec![sym("hp", SymbolKind::Variable, Some(TypeAnnotation::Int))];
        match ctx("hp == ", &syms) {
            CompletionContext::OperatorRhs { operator, .. } => {
                assert_eq!(operator, "==", "`==` should be consumed as a single token");
            }
            other => panic!("expected OperatorRhs, got {other:?}"),
        }
    }

    #[test]
    fn op_without_trailing_space_does_not_trigger() {
        // No space after `+`: cursor is touching the operator.
        let syms = vec![sym("x", SymbolKind::Variable, Some(TypeAnnotation::Int))];
        // "x +" — cursor immediately after `+` with no space.
        assert_eq!(
            detect_completion_context("x +", 3, &syms),
            CompletionContext::General
        );
    }

    // ── items_for_context ─────────────────────────────────────────────────

    #[test]
    fn jump_target_returns_only_labels() {
        let syms = vec![
            sym("intro", SymbolKind::Label, None),
            sym("score", SymbolKind::Variable, Some(TypeAnnotation::Int)),
            sym("Faction", SymbolKind::Enum, None),
        ];
        let items = items_for_context(&CompletionContext::JumpTarget, &syms, &empty_type_ctx());
        assert!(items.iter().all(|(_, k)| *k == SymbolKind::Label));
        assert!(items.iter().any(|(n, _)| n == "intro"));
        assert!(items.iter().all(|(n, _)| n != "score"));
        assert!(items.iter().all(|(n, _)| n != "Faction"));
    }

    #[test]
    fn after_at_returns_only_decorators() {
        let syms = vec![
            sym("entry", SymbolKind::Decorator, None),
            sym("intro", SymbolKind::Label, None),
            sym("x", SymbolKind::Variable, Some(TypeAnnotation::Int)),
        ];
        let items = items_for_context(&CompletionContext::AfterAt, &syms, &empty_type_ctx());
        assert!(items.iter().all(|(_, k)| *k == SymbolKind::Decorator));
        assert!(items.iter().any(|(n, _)| n == "entry"));
        assert!(items.iter().all(|(n, _)| n != "intro"));
    }

    #[test]
    fn module_access_strips_prefix_and_returns_members() {
        let syms = vec![
            Symbol {
                name: "chars.narrator".to_string(),
                kind: SymbolKind::Constant,
                span: SimpleSpan::new((), 0..0),
                type_annotation: None,
                detail: None,
                doc_comment: None,
                is_extern: false,
            },
            Symbol {
                name: "chars.Faction".to_string(),
                kind: SymbolKind::Enum,
                span: SimpleSpan::new((), 0..0),
                type_annotation: None,
                detail: None,
                doc_comment: None,
                is_extern: false,
            },
            sym("local_var", SymbolKind::Variable, None),
        ];
        let items = items_for_context(
            &CompletionContext::ModuleAccess {
                alias: "chars".to_string(),
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"narrator"), "must contain 'narrator'");
        assert!(names.contains(&"Faction"), "must contain 'Faction'");
        assert!(
            !names.contains(&"local_var"),
            "must not contain local symbols"
        );
        assert!(
            !names.contains(&"chars.narrator"),
            "must not contain fully-qualified name"
        );
    }

    #[test]
    fn struct_field_access_returns_correct_fields_only() {
        let syms = vec![
            field("Character", "name", TypeAnnotation::Str),
            field("Character", "name_color", TypeAnnotation::Str),
            // Field from a different struct — must NOT appear.
            field("Item", "value", TypeAnnotation::Int),
            // Real variable with same name as a field — must NOT appear
            // (its span is non-zero).
            sym("name", SymbolKind::Variable, Some(TypeAnnotation::Str)),
        ];
        let items = items_for_context(
            &CompletionContext::StructFieldAccess {
                var_name: "hero".to_string(),
                struct_name: "Character".to_string(),
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"name"), "must contain 'name'");
        assert!(names.contains(&"name_color"), "must contain 'name_color'");
        assert!(!names.contains(&"value"), "must not contain Item.value");
    }

    #[test]
    fn enum_variant_access_plain_name() {
        let syms = vec![
            variant("Faction", "Guild"),
            variant("Faction", "Empire"),
            variant("Faction", "Rebel"),
            // Variant from a different enum — must NOT appear.
            variant("Direction", "North"),
            sym("Faction", SymbolKind::Enum, None),
        ];
        let items = items_for_context(
            &CompletionContext::EnumVariantAccess {
                enum_name: "Faction".to_string(),
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"Guild"));
        assert!(names.contains(&"Empire"));
        assert!(names.contains(&"Rebel"));
        assert!(!names.contains(&"North"), "Direction.North must not appear");
    }

    #[test]
    fn enum_variant_access_qualified_name_strips_module_prefix() {
        // Whole-module import: `chars.Guild` / `chars.Empire` / `chars.Rebel`,
        // but their detail retains the local form `"Faction.Guild"` etc.
        let syms = vec![
            Symbol {
                name: "chars.Guild".to_string(),
                kind: SymbolKind::EnumVariant,
                span: SimpleSpan::new((), 0..0),
                type_annotation: None,
                detail: Some("Faction.Guild".to_string()),
                doc_comment: None,
                is_extern: false,
            },
            Symbol {
                name: "chars.Empire".to_string(),
                kind: SymbolKind::EnumVariant,
                span: SimpleSpan::new((), 0..0),
                type_annotation: None,
                detail: Some("Faction.Empire".to_string()),
                doc_comment: None,
                is_extern: false,
            },
            Symbol {
                name: "chars.Rebel".to_string(),
                kind: SymbolKind::EnumVariant,
                span: SimpleSpan::new((), 0..0),
                type_annotation: None,
                detail: Some("Faction.Rebel".to_string()),
                doc_comment: None,
                is_extern: false,
            },
        ];
        let items = items_for_context(
            &CompletionContext::EnumVariantAccess {
                enum_name: "chars.Faction".to_string(),
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        // Module prefix must be stripped from the insert label.
        assert!(names.contains(&"Guild"), "must contain 'Guild' (stripped)");
        assert!(
            names.contains(&"Empire"),
            "must contain 'Empire' (stripped)"
        );
        assert!(names.contains(&"Rebel"), "must contain 'Rebel' (stripped)");
        assert!(
            !names.contains(&"chars.Guild"),
            "must not contain qualified name"
        );
    }

    #[test]
    fn operator_rhs_numeric_includes_ints_and_floats_only() {
        let syms = vec![
            sym("i", SymbolKind::Variable, Some(TypeAnnotation::Int)),
            sym("f", SymbolKind::Variable, Some(TypeAnnotation::Float)),
            sym("b", SymbolKind::Variable, Some(TypeAnnotation::Bool)),
            sym("s", SymbolKind::Variable, Some(TypeAnnotation::Str)),
            sym("intro", SymbolKind::Label, None),
        ];
        let items = items_for_context(
            &CompletionContext::OperatorRhs {
                operator: "+".to_string(),
                lhs_name: None,
                compatibility: TypeCompatibility::Numeric,
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"i"), "int must be included");
        assert!(names.contains(&"f"), "float must be included");
        assert!(!names.contains(&"b"), "bool must be excluded");
        assert!(!names.contains(&"s"), "str must be excluded");
        assert!(!names.contains(&"intro"), "labels must be excluded");
    }

    #[test]
    fn operator_rhs_bool_includes_bools_only() {
        let syms = vec![
            sym("flag", SymbolKind::Variable, Some(TypeAnnotation::Bool)),
            sym("score", SymbolKind::Variable, Some(TypeAnnotation::Int)),
        ];
        let items = items_for_context(
            &CompletionContext::OperatorRhs {
                operator: "and".to_string(),
                lhs_name: None,
                compatibility: TypeCompatibility::Bool,
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"flag"));
        assert!(!names.contains(&"score"));
    }

    #[test]
    fn operator_rhs_integer_includes_ints_not_floats() {
        let syms = vec![
            sym("a", SymbolKind::Variable, Some(TypeAnnotation::Int)),
            sym("b", SymbolKind::Variable, Some(TypeAnnotation::Float)),
        ];
        let items = items_for_context(
            &CompletionContext::OperatorRhs {
                operator: "&".to_string(),
                lhs_name: None,
                compatibility: TypeCompatibility::Integer,
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"a"), "int must be included");
        assert!(!names.contains(&"b"), "float must be excluded for bitwise");
    }

    #[test]
    fn operator_rhs_same_as_filters_matching_type() {
        let syms = vec![
            sym("a", SymbolKind::Variable, Some(TypeAnnotation::Int)),
            sym("b", SymbolKind::Variable, Some(TypeAnnotation::Int)),
            sym("c", SymbolKind::Variable, Some(TypeAnnotation::Float)),
        ];
        let items = items_for_context(
            &CompletionContext::OperatorRhs {
                operator: "==".to_string(),
                lhs_name: Some("a".to_string()),
                compatibility: TypeCompatibility::SameAs(TypeAnnotation::Int),
            },
            &syms,
            &empty_type_ctx(),
        );
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"a"));
        assert!(names.contains(&"b"));
        assert!(!names.contains(&"c"), "float must not match int");
    }

    #[test]
    fn general_includes_symbols_and_keywords() {
        let syms = vec![
            sym("hp", SymbolKind::Variable, Some(TypeAnnotation::Int)),
            sym("intro", SymbolKind::Label, None),
        ];
        let items = items_for_context(&CompletionContext::General, &syms, &empty_type_ctx());
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"hp"));
        assert!(names.contains(&"intro"));
        assert!(names.contains(&"if"), "keywords must appear in general");
        assert!(names.contains(&"jump"), "keywords must appear in general");
        assert!(names.contains(&"match"), "keywords must appear in general");
    }

    // ── operator_compatible_type ──────────────────────────────────────────

    #[test]
    fn arithmetic_operators_are_numeric() {
        for op in ["+", "-", "*", "/", "//", "%"] {
            assert_eq!(
                operator_compatible_type(op, None),
                Some(TypeCompatibility::Numeric),
                "expected Numeric for op={op}"
            );
        }
    }

    #[test]
    fn logical_operators_are_bool() {
        for op in ["and", "&&", "or", "||"] {
            assert_eq!(
                operator_compatible_type(op, None),
                Some(TypeCompatibility::Bool),
                "expected Bool for op={op}"
            );
        }
    }

    #[test]
    fn bitwise_operators_are_integer() {
        for op in ["&", "|", "^", "<<", ">>"] {
            assert_eq!(
                operator_compatible_type(op, None),
                Some(TypeCompatibility::Integer),
                "expected Integer for op={op}"
            );
        }
    }

    #[test]
    fn comparison_with_lhs_type_gives_same_as() {
        let ann = TypeAnnotation::Str;
        assert_eq!(
            operator_compatible_type("==", Some(&ann)),
            Some(TypeCompatibility::SameAs(TypeAnnotation::Str))
        );
    }

    #[test]
    fn comparison_without_lhs_type_gives_any_typed() {
        assert_eq!(
            operator_compatible_type("!=", None),
            Some(TypeCompatibility::AnyTyped)
        );
    }

    #[test]
    fn assignment_with_lhs_type_gives_assign_to() {
        let ann = TypeAnnotation::Int;
        assert_eq!(
            operator_compatible_type("=", Some(&ann)),
            Some(TypeCompatibility::AssignTo(TypeAnnotation::Int))
        );
    }

    #[test]
    fn unrecognised_operator_returns_none() {
        assert_eq!(operator_compatible_type("?", None), None);
        // `not` is a unary operator — must not be treated as binary.
        assert_eq!(operator_compatible_type("not", None), None);
        assert_eq!(operator_compatible_type("!", None), None);
    }

    // ── scan_back_ident ───────────────────────────────────────────────────

    #[test]
    fn scan_back_simple_word() {
        assert_eq!(scan_back_ident("hello"), Some("hello".to_string()));
    }

    #[test]
    fn scan_back_after_space() {
        assert_eq!(scan_back_ident("foo bar"), Some("bar".to_string()));
    }

    #[test]
    fn scan_back_stops_at_dot() {
        assert_eq!(
            scan_back_ident("chars.Faction"),
            Some("Faction".to_string())
        );
    }

    #[test]
    fn scan_back_underscore_allowed() {
        assert_eq!(scan_back_ident("some_var"), Some("some_var".to_string()));
    }

    #[test]
    fn scan_back_empty_string_is_none() {
        assert_eq!(scan_back_ident(""), None);
    }

    #[test]
    fn scan_back_trailing_space_is_none() {
        assert_eq!(scan_back_ident("foo "), None);
    }

    #[test]
    fn scan_back_trailing_dot_is_none() {
        // The dot itself is not an ident char.
        assert_eq!(scan_back_ident("foo."), None);
    }

    // ── types_compatible ─────────────────────────────────────────────────

    #[test]
    fn same_primitive_types_are_compatible() {
        assert!(types_compatible(&TypeAnnotation::Int, &TypeAnnotation::Int));
        assert!(types_compatible(
            &TypeAnnotation::Bool,
            &TypeAnnotation::Bool
        ));
        assert!(types_compatible(&TypeAnnotation::Str, &TypeAnnotation::Str));
    }

    #[test]
    fn different_primitives_are_not_compatible() {
        assert!(!types_compatible(
            &TypeAnnotation::Int,
            &TypeAnnotation::Float
        ));
        assert!(!types_compatible(
            &TypeAnnotation::Bool,
            &TypeAnnotation::Str
        ));
    }

    #[test]
    fn named_types_with_identical_paths_are_compatible() {
        let a = TypeAnnotation::Named(vec!["Faction".to_string()]);
        let b = TypeAnnotation::Named(vec!["Faction".to_string()]);
        assert!(types_compatible(&a, &b));
    }

    #[test]
    fn named_types_with_same_leaf_are_compatible_cross_module() {
        let unqualified = TypeAnnotation::Named(vec!["Faction".to_string()]);
        let qualified = TypeAnnotation::Named(vec!["chars".to_string(), "Faction".to_string()]);
        assert!(
            types_compatible(&unqualified, &qualified),
            "leaf-name match should unify qualified and unqualified"
        );
        assert!(
            types_compatible(&qualified, &unqualified),
            "symmetry: qualified vs unqualified"
        );
    }

    #[test]
    fn named_types_with_different_names_are_not_compatible() {
        let a = TypeAnnotation::Named(vec!["Faction".to_string()]);
        let b = TypeAnnotation::Named(vec!["Direction".to_string()]);
        assert!(!types_compatible(&a, &b));
    }

    // ── deduplicated ──────────────────────────────────────────────────────

    #[test]
    fn dedup_removes_exact_duplicates_preserving_order() {
        let input = vec![
            ("foo".to_string(), SymbolKind::Variable),
            ("bar".to_string(), SymbolKind::Label),
            ("foo".to_string(), SymbolKind::Variable), // duplicate
        ];
        let result = deduplicated(input.into_iter());
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].0, "foo");
        assert_eq!(result[1].0, "bar");
    }

    #[test]
    fn dedup_keeps_same_name_with_different_kind() {
        // "foo" as Variable and "foo" as Label are distinct entries.
        let input = vec![
            ("foo".to_string(), SymbolKind::Variable),
            ("foo".to_string(), SymbolKind::Label),
        ];
        let result = deduplicated(input.into_iter());
        assert_eq!(result.len(), 2);
    }

    // ── consume_operator_backward ─────────────────────────────────────────

    #[test]
    fn consume_eq_not_assign() {
        // "==" must be consumed as "==", leaving "hp ".
        let (op, rest) = consume_operator_backward("hp ==").unwrap();
        assert_eq!(op, "==");
        assert_eq!(rest, "hp ");
    }

    #[test]
    fn consume_single_assign() {
        let (op, rest) = consume_operator_backward("x =").unwrap();
        assert_eq!(op, "=");
        assert_eq!(rest, "x ");
    }

    #[test]
    fn consume_and_keyword_with_boundary() {
        // "active and" — 'e' in "active" is a word boundary.
        let (op, rest) = consume_operator_backward("active and").unwrap();
        assert_eq!(op, "and");
        assert_eq!(rest, "active ");
    }

    #[test]
    fn consume_and_no_match_inside_identifier() {
        // "expand" ends with... let's see. "expand" — no op suffix matches
        // except 'd' at the end for nothing useful. Make sure "and" inside
        // "expand" is not matched by requiring a word boundary.
        // "expand" ends with 'd', not with "and" — so it shouldn't match at all.
        // Let's use a trickier case: "colorand" — ends with "and" but no boundary.
        let result = consume_operator_backward("colorand");
        // "and" requires a word boundary before it; "r" is alphanumeric → no match.
        // Single char ops: "d" not in SINGLE → None overall for this suffix.
        // Actually, it would match "d" — no, "d" is not in SINGLE.
        // The single-char list: +,-,*,/,%,=,&,|,^,<,>  — no "d".
        // So the result should be None.
        assert!(
            result.is_none(),
            "must not match 'and' inside an identifier: got {result:?}"
        );
    }

    #[test]
    fn consume_double_slash() {
        let (op, rest) = consume_operator_backward("n //").unwrap();
        assert_eq!(op, "//");
        assert_eq!(rest, "n ");
    }

    #[test]
    fn consume_left_shift() {
        let (op, rest) = consume_operator_backward("mask <<").unwrap();
        assert_eq!(op, "<<");
        assert_eq!(rest, "mask ");
    }

    // ── TypeContext fallback — cross-file partial imports ─────────────────
    //
    // These tests cover the real-world case that triggered the bug:
    //
    //   import (narrator) from "characters.urd"
    //
    // Because this is a single-symbol import, the `Character` struct's field
    // sentinel symbols (`Variable` with `span == 0..0` and `detail =
    // "Character.field"`) are never added to the local symbol list.  The
    // TypeContext fallback must kick in and return the fields directly from the
    // workspace type map.

    #[test]
    fn struct_fields_via_type_ctx_fallback_when_no_sentinels_in_symbols() {
        // Symbol list has `narrator: Character` but NO Character field sentinels
        // (simulates a symbol-import: `import (narrator) from "characters.urd"`).
        let syms = vec![sym(
            "narrator",
            SymbolKind::Constant,
            Some(TypeAnnotation::Named(vec!["Character".to_string()])),
        )];

        // The TypeContext carries the struct definition from the workspace index.
        let mut type_ctx = TypeContext::default();
        type_ctx.structs.insert(
            "Character".to_string(),
            vec![
                urd::parser::ast::StructField {
                    name: "name".to_string(),
                    span: urd::parser::ast::TokSpan::default(),
                    type_annotation: TypeAnnotation::Str,
                },
                urd::parser::ast::StructField {
                    name: "name_color".to_string(),
                    span: urd::parser::ast::TokSpan::default(),
                    type_annotation: TypeAnnotation::Str,
                },
            ],
        );

        let items = items_for_context(
            &CompletionContext::StructFieldAccess {
                var_name: "narrator".to_string(),
                struct_name: "Character".to_string(),
            },
            &syms,
            &type_ctx,
        );

        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(
            names.contains(&"name"),
            "fallback must surface 'name' field from TypeContext"
        );
        assert!(
            names.contains(&"name_color"),
            "fallback must surface 'name_color' field from TypeContext"
        );
        assert_eq!(items.len(), 2, "should return exactly 2 fields");
    }

    #[test]
    fn struct_fields_sentinel_takes_priority_over_type_ctx() {
        // When sentinel symbols ARE present, the TypeContext fallback should
        // NOT be used (primary path wins, even if TypeContext has different data).
        let syms = vec![
            field("Character", "name", TypeAnnotation::Str),
            field("Character", "name_color", TypeAnnotation::Str),
        ];

        let mut type_ctx = TypeContext::default();
        // TypeContext has a conflicting (stale) definition with an extra field —
        // should be ignored because sentinels are present.
        type_ctx.structs.insert(
            "Character".to_string(),
            vec![urd::parser::ast::StructField {
                name: "stale_field".to_string(),
                span: urd::parser::ast::TokSpan::default(),
                type_annotation: TypeAnnotation::Int,
            }],
        );

        let items = items_for_context(
            &CompletionContext::StructFieldAccess {
                var_name: "hero".to_string(),
                struct_name: "Character".to_string(),
            },
            &syms,
            &type_ctx,
        );

        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"name"), "sentinel 'name' must appear");
        assert!(
            names.contains(&"name_color"),
            "sentinel 'name_color' must appear"
        );
        assert!(
            !names.contains(&"stale_field"),
            "TypeContext stale field must NOT appear when sentinels are present"
        );
    }

    #[test]
    fn enum_variants_via_type_ctx_fallback_plain_name() {
        // Symbol list has `Faction` enum symbol but NO variant sentinels
        // (simulates `import (Faction) from "characters.urd"` without whole-module).
        let syms = vec![sym("Faction", SymbolKind::Enum, None)];

        let mut type_ctx = TypeContext::default();
        type_ctx.enums.insert(
            "Faction".to_string(),
            vec![
                "Guild".to_string(),
                "Empire".to_string(),
                "Rebel".to_string(),
            ],
        );

        let items = items_for_context(
            &CompletionContext::EnumVariantAccess {
                enum_name: "Faction".to_string(),
            },
            &syms,
            &type_ctx,
        );

        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"Guild"), "fallback must surface 'Guild'");
        assert!(names.contains(&"Empire"), "fallback must surface 'Empire'");
        assert!(names.contains(&"Rebel"), "fallback must surface 'Rebel'");
        assert_eq!(items.len(), 3, "should return exactly 3 variants");
    }

    #[test]
    fn enum_variants_via_type_ctx_fallback_qualified_name_leaf_lookup() {
        // Qualified enum `chars.Faction` but no variant sentinels for the
        // `chars.` prefix (simulates a partial import).
        let syms = vec![sym("chars.Faction", SymbolKind::Enum, None)];

        let mut type_ctx = TypeContext::default();
        // TypeContext stores it under the unqualified leaf name "Faction".
        type_ctx.enums.insert(
            "Faction".to_string(),
            vec!["Guild".to_string(), "Empire".to_string()],
        );

        let items = items_for_context(
            &CompletionContext::EnumVariantAccess {
                enum_name: "chars.Faction".to_string(),
            },
            &syms,
            &type_ctx,
        );

        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"Guild"), "fallback must surface 'Guild'");
        assert!(names.contains(&"Empire"), "fallback must surface 'Empire'");
    }

    #[test]
    fn enum_variant_sentinel_takes_priority_over_type_ctx() {
        // When variant sentinels ARE present, TypeContext fallback must not fire.
        let syms = vec![variant("Faction", "Guild"), variant("Faction", "Empire")];

        let mut type_ctx = TypeContext::default();
        // TypeContext has a stale extra variant — must be ignored.
        type_ctx
            .enums
            .insert("Faction".to_string(), vec!["StaleVariant".to_string()]);

        let items = items_for_context(
            &CompletionContext::EnumVariantAccess {
                enum_name: "Faction".to_string(),
            },
            &syms,
            &type_ctx,
        );

        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"Guild"), "sentinel 'Guild' must appear");
        assert!(names.contains(&"Empire"), "sentinel 'Empire' must appear");
        assert!(
            !names.contains(&"StaleVariant"),
            "TypeContext stale variant must NOT appear when sentinels are present"
        );
    }

    // ── Real-world multiline integration ─────────────────────────────────
    //
    // These tests reproduce the exact failure shown in the screenshot:
    // a multiline Urd source where `narrator` is a *directly* imported
    // `const narrator: Character` from "characters.urd", the user types
    // `narrator.` on a line in the middle of the file, and the completion
    // engine must detect the dot context and return the struct fields.
    //
    // The critical subtlety: `before` passed to `detect_dot_context` is the
    // full source up to the cursor — a multi-line string — so the identifier
    // extraction must strip everything up to the last word boundary.

    /// Build the symbols that `main.rs` would assemble for a file that does:
    ///   import (narrator) from "characters.urd"
    /// (i.e. `narrator` is a Constant with Named(["Character"]) type, but
    /// the Character struct fields are NOT in the symbol list as sentinels.)
    fn narrator_symbols() -> Vec<Symbol> {
        vec![
            // The directly-imported constant.
            Symbol {
                name: "narrator".to_string(),
                kind: SymbolKind::Constant,
                span: chumsky::span::SimpleSpan::new((), 0..0),
                type_annotation: Some(TypeAnnotation::Named(vec!["Character".to_string()])),
                detail: Some("const narrator: Character".to_string()),
                doc_comment: None,
                is_extern: false,
            },
            // A few other symbols that should NOT appear in field completion.
            sym("prologue", SymbolKind::Label, None),
            sym("start", SymbolKind::Label, None),
            sym(
                "player_faction",
                SymbolKind::Constant,
                Some(TypeAnnotation::Named(vec!["Faction".to_string()])),
            ),
        ]
    }

    /// Build the TypeContext that `main.rs` would receive from
    /// `workspace.imported_type_context()` — Character struct and Faction enum
    /// loaded from the imported characters.urd file.
    fn narrator_type_ctx() -> TypeContext {
        let mut ctx = TypeContext::default();
        ctx.structs.insert(
            "Character".to_string(),
            vec![
                urd::parser::ast::StructField {
                    name: "name".to_string(),
                    span: urd::parser::ast::TokSpan::default(),
                    type_annotation: TypeAnnotation::Str,
                },
                urd::parser::ast::StructField {
                    name: "name_color".to_string(),
                    span: urd::parser::ast::TokSpan::default(),
                    type_annotation: TypeAnnotation::Str,
                },
            ],
        );
        ctx.enums.insert(
            "Faction".to_string(),
            vec![
                "Guild".to_string(),
                "Empire".to_string(),
                "Rebel".to_string(),
            ],
        );
        ctx
    }

    #[test]
    fn narrator_dot_detects_struct_field_access_context() {
        // Simulate the source text with `narrator.` at the end (cursor right
        // after the dot, as the editor sends completion immediately on `.`).
        let src = "import (narrator) from \"characters.urd\"\n\
                   \n\
                   label prologue {\n\
                   \tnewline: {\n\
                   \t\t\"some dialogue\"\n\
                   \t}\n\
                   \tnarrator.";
        let byte_offset = src.len(); // cursor at end
        let syms = narrator_symbols();

        let ctx = detect_completion_context(src, byte_offset, &syms);
        assert_eq!(
            ctx,
            CompletionContext::StructFieldAccess {
                var_name: "narrator".to_string(),
                struct_name: "Character".to_string(),
            },
            "typing `narrator.` must detect StructFieldAccess with struct_name=Character"
        );
    }

    #[test]
    fn narrator_dot_detects_struct_field_access_with_partial_field() {
        // Cursor is mid-word: `narrator.na` — partial field name already typed.
        let src = "import (narrator) from \"characters.urd\"\n\
                   \n\
                   label prologue {\n\
                   \tnarrator.na";
        let byte_offset = src.len();
        let syms = narrator_symbols();

        let ctx = detect_completion_context(src, byte_offset, &syms);
        assert_eq!(
            ctx,
            CompletionContext::StructFieldAccess {
                var_name: "narrator".to_string(),
                struct_name: "Character".to_string(),
            },
            "typing `narrator.na` must still detect StructFieldAccess"
        );
    }

    #[test]
    fn narrator_dot_returns_character_fields_via_type_ctx() {
        // Full end-to-end: `narrator.` in multiline source →
        // completion_items returns `name` and `name_color` via TypeContext fallback.
        let src = "import (narrator) from \"characters.urd\"\n\
                   \n\
                   label prologue {\n\
                   \tnarrator: { \"hello\" }\n\
                   \tnarrator.";
        let byte_offset = src.len();
        let syms = narrator_symbols();
        let type_ctx = narrator_type_ctx();

        // `_ast` is unused by completion_items; pass a dummy by parsing something valid.
        let (ast, _) = urd::compiler::loader::parse_source_spanned("label _dummy { end!() }\n");
        let ast = ast.expect("dummy parse must succeed");

        let items = completion_items(&ast, &syms, byte_offset, src, &type_ctx);
        let names: Vec<&str> = items.iter().map(|(n, _)| n.as_str()).collect();

        assert!(
            names.contains(&"name"),
            "must offer 'name' field; got {names:?}"
        );
        assert!(
            names.contains(&"name_color"),
            "must offer 'name_color' field; got {names:?}"
        );
        assert!(
            !names.contains(&"prologue"),
            "labels must not appear in struct field completion"
        );
        assert!(
            !names.contains(&"narrator"),
            "the variable itself must not appear as its own field"
        );
        // All returned items must be Variable kind (struct fields).
        for (name, kind) in &items {
            assert_eq!(
                *kind,
                SymbolKind::Variable,
                "expected Variable kind for field {name:?}, got {kind:?}"
            );
        }
    }

    #[test]
    fn narrator_dot_with_indent_and_tabs() {
        // Real Urd files use tab indentation; ensure whitespace before the
        // identifier doesn't confuse the backward scan.
        let src = "label prologue {\n\t\t\tnarrator.";
        let byte_offset = src.len();
        let syms = narrator_symbols();

        let ctx = detect_completion_context(src, byte_offset, &syms);
        assert_eq!(
            ctx,
            CompletionContext::StructFieldAccess {
                var_name: "narrator".to_string(),
                struct_name: "Character".to_string(),
            },
            "tab-indented `narrator.` must still resolve to StructFieldAccess"
        );
    }

    #[test]
    fn narrator_dot_on_line_after_closed_block() {
        // The exact shape from the screenshot: a closed `{ }` block on the
        // previous lines, then `narrator.` on the next line.
        let src = "label prologue {\n\
                   \tnarrator: {\n\
                   \t\t\"some text\"\n\
                   \t}\n\
                   \tnarrator.";
        let byte_offset = src.len();
        let syms = narrator_symbols();

        let ctx = detect_completion_context(src, byte_offset, &syms);
        assert_eq!(
            ctx,
            CompletionContext::StructFieldAccess {
                var_name: "narrator".to_string(),
                struct_name: "Character".to_string(),
            },
            "narrator. after a closed block must resolve to StructFieldAccess"
        );
    }
}
