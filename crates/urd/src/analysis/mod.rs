//! # Static Analysis Module
//!
//! This module provides best-effort static analysis passes over a parsed Urd AST.
//!
//! ## Passes
//!
//! ### Errors
//! - [`exhaustiveness`]: Non-exhaustive `match` over an enum.
//! - [`types`]: Type-annotation mismatches on declarations and assignments.
//! - [`dead_end`]: Execution paths with no recognised terminator.
//! - [`menu_structure`]: Empty menus (no options at all).
//! - [`const_reassign`]: Assignment to a `const`-declared binding.
//!
//! ### Warnings
//! - [`labels`]: `jump` / `let-call` to an undefined label.
//! - [`top_level`]: Flow-control or dialogue at the top level; duplicate `@entry`.
//! - [`unreachable_label`]: Labeled blocks that no jump or `@entry` can reach.
//! - [`menu_structure`]: Single-option menus (no real player choice).
//! - [`empty_dialogue`]: Dialogue lines with an empty or whitespace-only body.
//! - [`duplicate_menu_dest`]: Two menu options with structurally identical bodies.
//! - [`overwritten_assign`]: A variable is overwritten before its previous value is read.
//! - [`unused_var`]: A `let`/`const` variable is declared but never read.
//! - [`dead_branch`]: An `if` condition folds to a constant, making one branch dead.
//! - [`possible_typo`]: An identifier closely resembles a known name (edit distance ≤ 2).
//!
//! ### Opt-in
//! - [`loop_detection`]: Infinite dialogue loops (SCC with no escaping terminal path).
//!   Enable per-label with the `@lint(check_loops)` decorator.
//!
//! All passes always run to completion; errors from one pass do not suppress the others.

pub mod const_reassign;
pub mod context;
pub mod dead_branch;
pub mod dead_end;
pub mod duplicate_menu_dest;
pub mod empty_dialogue;
pub mod exhaustiveness;
pub mod labels;
pub mod loop_detection;
pub mod menu_structure;
pub mod overwritten_assign;
pub mod possible_typo;
pub mod top_level;
pub mod types;
pub mod unreachable_label;
pub mod unused_var;

#[cfg(test)]
mod tests;

use ariadne::{Label, Report, ReportKind, sources};
use chumsky::span::SimpleSpan;

use crate::parser::ast::{Ast, TypeAnnotation};

// ---------------------------------------------------------------------------
// StructFieldError
// ---------------------------------------------------------------------------

/// A single field-level problem in a struct type mismatch.
#[derive(Debug, Clone, PartialEq)]
pub enum StructFieldError {
    /// A required field is absent from the map literal.
    MissingField {
        /// The name of the field that is missing.
        field_name: String,
        /// The type that the missing field was expected to have.
        expected_type: TypeAnnotation,
    },
    /// A field is present but its literal value has the wrong type.
    WrongFieldType {
        /// The name of the field whose value has the wrong type.
        field_name: String,
        /// The type that was declared for this field.
        expected_type: TypeAnnotation,
        /// A human-readable description of the actual value's type.
        got: String,
    },
}

use context::AnalysisContext;

// ---------------------------------------------------------------------------
// NodeDescription — human-readable fallback label for ariadne output
// ---------------------------------------------------------------------------

/// Best-effort human-readable source location.
///
/// Used as a descriptive label in ariadne diagnostics and as a fallback
/// description for AST nodes built in tests (which carry zero spans).
#[derive(Debug, Clone, PartialEq)]
pub struct NodeDescription(pub String);

impl NodeDescription {
    /// A location that could not be determined.
    pub fn unknown() -> Self {
        NodeDescription("(unknown location)".into())
    }

    /// A top-level block in the script.
    pub fn top_level() -> Self {
        NodeDescription("top-level block".into())
    }

    /// A named label block (`label foo { ... }`).
    pub fn label(name: &str) -> Self {
        NodeDescription(format!("label '{name}'"))
    }

    /// Something inside a named label block.
    pub fn in_label(label: &str, what: &str) -> Self {
        NodeDescription(format!("{what} in label '{label}'"))
    }

    /// A menu option inside some enclosing context.
    pub fn menu_option(opt_label: &str, in_what: &NodeDescription) -> Self {
        NodeDescription(format!("menu option '{opt_label}' in {}", in_what.0))
    }

    /// A numbered match arm inside some enclosing context.
    pub fn match_arm(index: usize, in_what: &NodeDescription) -> Self {
        NodeDescription(format!("match arm #{index} in {}", in_what.0))
    }
}

impl std::fmt::Display for NodeDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

// ---------------------------------------------------------------------------
// TypoKind
// ---------------------------------------------------------------------------

/// Classifies what namespace a [`AnalysisError::PossibleTypo`] belongs to.
#[derive(Debug, Clone, PartialEq)]
pub enum TypoKind {
    /// A speaker identifier in a dialogue line (e.g. `zra:` instead of `zara:`).
    Speaker,
    /// A jump/let-call target label (e.g. `jump staart` instead of `jump start`).
    Label,
    /// A variable reference (e.g. `if visted_cave` instead of `if visited_cave`).
    Variable,
}

impl std::fmt::Display for TypoKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypoKind::Speaker => f.write_str("speaker"),
            TypoKind::Label => f.write_str("label"),
            TypoKind::Variable => f.write_str("variable"),
        }
    }
}

// ---------------------------------------------------------------------------
// AnalysisError
// ---------------------------------------------------------------------------

/// A single diagnostic produced by the static analyser.
#[derive(Debug, Clone, PartialEq)]
pub enum AnalysisError {
    // ── Errors ────────────────────────────────────────────────────────────
    /// A `match` over an enum did not cover all variants and had no wildcard arm.
    NonExhaustiveMatch {
        /// The name of the enum being matched over.
        enum_name: String,
        /// The variant names that are not covered by any arm.
        missing_variants: Vec<String>,
        /// The source span of the offending `match` node.
        /// A zero span (`0..0`) means the node was built outside the parser (e.g. in tests).
        span: SimpleSpan,
    },

    /// A value was assigned to a variable whose declared type is incompatible.
    TypeMismatch {
        /// The variable being assigned to.
        variable: String,
        /// The type that was declared for the variable.
        expected: TypeAnnotation,
        /// A human-readable description of the actual value's type.
        got: String,
        /// The source span of the offending assignment node.
        /// A zero span (`0..0`) means the node was built outside the parser (e.g. in tests).
        span: SimpleSpan,
    },

    /// A map literal assigned to a struct-typed variable has field errors.
    StructMismatch {
        /// The variable being assigned to.
        variable: String,
        /// The struct type name expected.
        struct_name: String,
        /// Each field-level problem found.
        field_errors: Vec<StructFieldError>,
        /// Source span.
        span: SimpleSpan,
    },

    /// A `jump` statement references a label that is not defined anywhere in
    /// the script.
    UndefinedLabel {
        /// The name used in the `jump` (or `let … = jump … and return`).
        label: String,
        /// Source span of the jump statement.
        span: SimpleSpan,
    },

    /// An execution path reaches its end without a recognised terminator.
    ///
    /// Valid terminators are `end!`, `todo!`, a bare `return`, or a `jump`.
    DeadEnd {
        /// The source span of the offending block/option node.
        /// A zero span (`0..0`) means the node was built outside the parser (e.g. in tests).
        span: SimpleSpan,
        /// A human-readable description of the dead-end location, used as a
        /// label in ariadne output and for zero-span fallback messages.
        description: NodeDescription,
    },

    /// A statement that is not a definition appeared at the top level.
    TopLevelFlow {
        /// Human-readable description of the offending node.
        description: String,
        /// Source span of the offending node.
        span: SimpleSpan,
    },

    /// More than one label is decorated with `@entry`.
    DuplicateEntry {
        /// Name of the second (or subsequent) `@entry` label.
        label: String,
        /// Source span of the duplicate `@entry` label.
        span: SimpleSpan,
    },

    /// A `const` variable was assigned a new value after its initial declaration.
    ConstReassignment {
        /// The name of the constant.
        name: String,
        /// Source span of the offending assignment node.
        span: SimpleSpan,
    },

    /// A `menu` block has no options — the player can never make a choice.
    EmptyMenu {
        /// Source span of the offending `menu` node.
        span: SimpleSpan,
    },

    // ── Warnings ──────────────────────────────────────────────────────────
    /// A labeled block can never be reached from any `jump`, `let-call`, or `@entry`.
    UnreachableLabel {
        /// The name of the unreachable label.
        label: String,
        /// Source span of the labeled block.
        span: SimpleSpan,
    },

    /// A `menu` block has only one option — the player has no real choice.
    SingleOptionMenu {
        /// Source span of the offending `menu` node.
        span: SimpleSpan,
    },

    /// A dialogue line has an empty string or an empty block body.
    EmptyDialogue {
        /// Speaker identifier (e.g. `"narrator"`).
        speaker: String,
        /// Source span of the offending dialogue node.
        span: SimpleSpan,
    },

    /// Two or more options in the same menu have structurally identical bodies,
    /// making the player's choice illusory.
    DuplicateMenuDestination {
        /// The label text of the first (earlier) duplicate option.
        first_option: String,
        /// The label text of the second (later) duplicate option.
        second_option: String,
        /// Source span of the second (duplicate) option.
        span: SimpleSpan,
    },

    /// A variable was assigned a value that was immediately overwritten without
    /// being read in between — the first write is effectless.
    OverwrittenAssignment {
        /// The variable name.
        name: String,
        /// Span of the overwriting (second) assignment.
        span: SimpleSpan,
    },

    /// A `let`/`const` variable was declared but never subsequently read anywhere
    /// in its enclosing label scope.
    UnusedVariable {
        /// The variable name.
        name: String,
        /// Source span of the declaration.
        span: SimpleSpan,
    },

    /// An `if` condition is composed entirely of `const` values and evaluates to a
    /// known constant at analysis time, making one branch permanently unreachable.
    AlwaysDeadBranch {
        /// `true` if the `then` branch is dead (condition is always `false`).
        /// `false` if the `else` branch is dead (condition is always `true`).
        dead_branch_is_then: bool,
        /// Source span of the `if` node.
        span: SimpleSpan,
    },

    /// An identifier closely resembles a known name in the same namespace,
    /// suggesting a typo (edit distance ≤ 2).
    PossibleTypo {
        /// The identifier as written in the source.
        written: String,
        /// The closest known name (the likely intended identifier).
        suggestion: String,
        /// What kind of identifier this is.
        kind: TypoKind,
        /// Source span of the suspicious identifier.
        span: SimpleSpan,
    },

    // ── Opt-in ────────────────────────────────────────────────────────────
    /// A set of labels forms a cycle with no escaping path to a terminator.
    ///
    /// This diagnostic is **opt-in**: it is only emitted for labels decorated
    /// with `@lint(check_loops)`.
    InfiniteDialogueLoop {
        /// The label that anchors the detected cycle.
        label: String,
        /// Source span of that label.
        span: SimpleSpan,
    },
}

impl AnalysisError {
    /// Returns the source span associated with this error.
    ///
    /// For AST nodes built in tests (without a real parser span) this will be
    /// a zero span `0..0`.
    pub fn span(&self) -> SimpleSpan {
        match self {
            AnalysisError::NonExhaustiveMatch { span, .. } => *span,
            AnalysisError::TypeMismatch { span, .. } => *span,
            AnalysisError::StructMismatch { span, .. } => *span,
            AnalysisError::UndefinedLabel { span, .. } => *span,
            AnalysisError::DeadEnd { span, .. } => *span,
            AnalysisError::TopLevelFlow { span, .. } => *span,
            AnalysisError::DuplicateEntry { span, .. } => *span,
            AnalysisError::ConstReassignment { span, .. } => *span,
            AnalysisError::EmptyMenu { span, .. } => *span,
            AnalysisError::UnreachableLabel { span, .. } => *span,
            AnalysisError::SingleOptionMenu { span, .. } => *span,
            AnalysisError::EmptyDialogue { span, .. } => *span,
            AnalysisError::DuplicateMenuDestination { span, .. } => *span,
            AnalysisError::OverwrittenAssignment { span, .. } => *span,
            AnalysisError::UnusedVariable { span, .. } => *span,
            AnalysisError::AlwaysDeadBranch { span, .. } => *span,
            AnalysisError::PossibleTypo { span, .. } => *span,
            AnalysisError::InfiniteDialogueLoop { span, .. } => *span,
        }
    }

    /// Returns `true` if this diagnostic is a warning rather than a hard error.
    pub fn is_warning(&self) -> bool {
        matches!(
            self,
            AnalysisError::UnreachableLabel { .. }
                | AnalysisError::SingleOptionMenu { .. }
                | AnalysisError::EmptyDialogue { .. }
                | AnalysisError::DuplicateMenuDestination { .. }
                | AnalysisError::OverwrittenAssignment { .. }
                | AnalysisError::UnusedVariable { .. }
                | AnalysisError::AlwaysDeadBranch { .. }
                | AnalysisError::PossibleTypo { .. }
                | AnalysisError::InfiniteDialogueLoop { .. }
        )
    }

    /// Returns `true` if the span is a zero span (i.e. no real source location).
    fn is_zero_span(span: &SimpleSpan) -> bool {
        span.start == 0 && span.end == 0
    }

    /// Format a span as a human-readable location string.
    ///
    /// Returns `"(unknown location)"` for zero spans and `"byte S..E"` otherwise.
    #[allow(dead_code)]
    fn format_span_loc(span: &SimpleSpan) -> String {
        if Self::is_zero_span(span) {
            "(unknown location)".to_owned()
        } else {
            format!("byte {}..{}", span.start, span.end)
        }
    }

    /// Returns a short human-readable message for this error (used in `Display`
    /// and as the ariadne report message).
    fn message(&self) -> String {
        match self {
            AnalysisError::NonExhaustiveMatch {
                enum_name,
                missing_variants,
                ..
            } => {
                format!(
                    "Non-exhaustive match on enum '{enum_name}': \
                     missing variants: {}",
                    missing_variants.join(", ")
                )
            }

            AnalysisError::TypeMismatch {
                variable,
                expected,
                got,
                ..
            } => {
                format!("Type mismatch for '{variable}': expected {expected:?}, got {got}")
            }

            AnalysisError::StructMismatch {
                variable,
                struct_name,
                field_errors,
                ..
            } => {
                let details: Vec<String> = field_errors
                    .iter()
                    .map(|fe| match fe {
                        StructFieldError::MissingField {
                            field_name,
                            expected_type,
                        } => format!("missing field '{field_name}': {expected_type:?}"),
                        StructFieldError::WrongFieldType {
                            field_name,
                            expected_type,
                            got,
                        } => format!("field '{field_name}': expected {expected_type:?}, got {got}"),
                    })
                    .collect();
                format!(
                    "Struct mismatch for '{variable}': expected struct '{struct_name}': {}",
                    details.join("; ")
                )
            }

            AnalysisError::UndefinedLabel { label, .. } => {
                format!("Undefined label '{label}'")
            }

            AnalysisError::DeadEnd { description, .. } => {
                format!(
                    "Dead end in {description}: execution path has no terminator \
                     (use `end!`, `todo!`, `return`, or `jump`)"
                )
            }

            AnalysisError::TopLevelFlow { description, .. } => {
                format!(
                    "Top-level {description} is not allowed; \
                     only definitions (let/const/global, enum, struct, decorator, import, label) \
                     may appear at the top level"
                )
            }

            AnalysisError::DuplicateEntry { label, .. } => {
                format!("Duplicate @entry decorator on label '{label}'")
            }

            AnalysisError::ConstReassignment { name, .. } => {
                format!(
                    "Constant reassignment: '{name}' is declared as `const` and cannot be reassigned"
                )
            }

            AnalysisError::EmptyMenu { .. } => {
                "Empty menu: the player can never make a choice (menu has no options)".to_owned()
            }

            AnalysisError::UnreachableLabel { label, .. } => {
                format!(
                    "Unreachable label '{label}': no `jump`, `let-call`, or `@entry` can reach this label"
                )
            }

            AnalysisError::SingleOptionMenu { .. } => {
                "Single-option menu: the player has no real choice (only one option)".to_owned()
            }

            AnalysisError::EmptyDialogue { speaker, .. } => {
                format!("Empty dialogue for speaker '{speaker}': the dialogue content is blank")
            }

            AnalysisError::DuplicateMenuDestination {
                first_option,
                second_option,
                ..
            } => {
                format!(
                    "Duplicate menu destination: options '{first_option}' and '{second_option}' \
                     have identical bodies"
                )
            }

            AnalysisError::OverwrittenAssignment { name, .. } => {
                format!(
                    "Overwritten assignment: '{name}' is assigned a value that is immediately \
                     overwritten without being read"
                )
            }

            AnalysisError::UnusedVariable { name, .. } => {
                format!("Unused variable: '{name}' is declared but never read")
            }

            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => {
                if *dead_branch_is_then {
                    "Always-dead branch: condition is always `false`, the `then` branch is never executed".to_owned()
                } else {
                    "Always-dead branch: condition is always `true`, the `else` branch is never executed".to_owned()
                }
            }

            AnalysisError::PossibleTypo {
                written,
                suggestion,
                kind,
                ..
            } => {
                format!(
                    "Possible typo: '{written}' looks like it could be the {kind} '{suggestion}'"
                )
            }

            AnalysisError::InfiniteDialogueLoop { label, .. } => {
                format!(
                    "Infinite dialogue loop: label '{label}' is part of a cycle with no \
                     escaping path to a terminator"
                )
            }
        }
    }

    /// Returns a short label string suitable for an ariadne `Label`.
    fn label_message(&self) -> String {
        match self {
            AnalysisError::NonExhaustiveMatch {
                enum_name,
                missing_variants,
                ..
            } => format!(
                "match on '{enum_name}' is missing: {}",
                missing_variants.join(", ")
            ),

            AnalysisError::TypeMismatch {
                variable,
                expected,
                got,
                ..
            } => format!("'{variable}' expects {expected:?} but got {got}"),

            AnalysisError::StructMismatch {
                variable,
                struct_name,
                field_errors,
                ..
            } => format!(
                "'{variable}' expects struct '{struct_name}' but has {} field error(s)",
                field_errors.len()
            ),

            AnalysisError::UndefinedLabel { label, .. } => {
                format!("jump to undefined label '{label}'")
            }

            AnalysisError::DeadEnd { description, .. } => {
                format!("{description}: no terminator on this path")
            }

            AnalysisError::TopLevelFlow { description, .. } => {
                format!("{description} not allowed at top level")
            }

            AnalysisError::DuplicateEntry { label, .. } => {
                format!("label '{label}' is a duplicate @entry")
            }

            AnalysisError::ConstReassignment { name, .. } => {
                format!("'{name}' is a constant and cannot be reassigned")
            }

            AnalysisError::EmptyMenu { .. } => "this menu has no options".to_owned(),

            AnalysisError::UnreachableLabel { label, .. } => {
                format!("label '{label}' is never jumped to")
            }

            AnalysisError::SingleOptionMenu { .. } => "this menu has only one option".to_owned(),

            AnalysisError::EmptyDialogue { speaker, .. } => {
                format!("'{speaker}' says nothing here")
            }

            AnalysisError::DuplicateMenuDestination {
                first_option,
                second_option,
                ..
            } => {
                format!("'{second_option}' has the same body as '{first_option}'")
            }

            AnalysisError::OverwrittenAssignment { name, .. } => {
                format!("'{name}' is overwritten here without being read first")
            }

            AnalysisError::UnusedVariable { name, .. } => {
                format!("'{name}' is declared here but never read")
            }

            AnalysisError::AlwaysDeadBranch {
                dead_branch_is_then,
                ..
            } => {
                if *dead_branch_is_then {
                    "the `then` branch is always dead (condition is always `false`)".to_owned()
                } else {
                    "the `else` branch is always dead (condition is always `true`)".to_owned()
                }
            }

            AnalysisError::PossibleTypo {
                written,
                suggestion,
                kind,
                ..
            } => {
                format!("'{written}' — did you mean the {kind} '{suggestion}'?")
            }

            AnalysisError::InfiniteDialogueLoop { label, .. } => {
                format!("label '{label}' anchors an infinite loop")
            }
        }
    }
}

impl std::fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message())
    }
}

// ---------------------------------------------------------------------------
// Ariadne rendering
// ---------------------------------------------------------------------------

/// Convert a byte offset into a Unicode codepoint (char) offset within `src`.
///
/// Ariadne's `sources()` helper indexes source text by codepoint, not by byte.
/// Logos, on the other hand, produces byte-based spans. This function bridges
/// the two by counting codepoints up to `byte_offset`.
///
/// If `byte_offset` falls in the middle of a multi-byte codepoint (which should
/// never happen for well-formed source), it is clamped to the nearest codepoint
/// boundary.
fn byte_to_char(src: &str, byte_offset: usize) -> usize {
    let clamped = byte_offset.min(src.len());
    src[..clamped].chars().count()
}

/// Render all diagnostics as rich ariadne reports, writing to `writer`.
///
/// For errors with a real source span the output is a colourful annotated
/// code snippet (ariadne style).  For **zero-span** errors (AST nodes built in
/// tests without parser spans) a plain `writeln!` fallback is used instead.
///
/// Byte offsets stored in spans are converted to Unicode codepoint offsets
/// before being passed to ariadne, which indexes source text by codepoint.
pub fn render_errors<W: std::io::Write>(
    errors: &[AnalysisError],
    src: &str,
    source_name: &str,
    writer: &mut W,
) -> std::io::Result<()> {
    for error in errors {
        let span = error.span();

        if AnalysisError::is_zero_span(&span) {
            // No real span — fall back to a plain text line.
            // Warnings use "warning:" prefix, errors use "error:".
            let prefix = if error.is_warning() {
                "warning"
            } else {
                "error"
            };
            writeln!(writer, "{prefix}: {}", error.message())?;
            continue;
        }

        // Logos produces byte offsets; ariadne's `sources()` indexes by
        // Unicode codepoint. Convert before building the report.
        let start = byte_to_char(src, span.start);
        let end = byte_to_char(src, span.end);
        let range = start..end;

        let name_owned = source_name.to_owned();

        let report_kind = if error.is_warning() {
            ReportKind::Warning
        } else {
            ReportKind::Error
        };

        let report = Report::<(String, std::ops::Range<usize>)>::build(
            report_kind,
            (name_owned.clone(), range.clone()),
        )
        .with_message(error.message())
        .with_label(Label::new((name_owned.clone(), range)).with_message(error.label_message()))
        .finish();

        report
            .write(sources([(name_owned, src.to_owned())]), &mut *writer)
            .map_err(|e| std::io::Error::other(e.to_string()))?;
    }

    Ok(())
}

/// Convenience wrapper that renders diagnostics to `stderr`.
pub fn render_errors_stderr(errors: &[AnalysisError], src: &str, source_name: &str) {
    let mut stderr = std::io::stderr();
    if let Err(e) = render_errors(errors, src, source_name, &mut stderr) {
        eprintln!("warning: failed to render diagnostics: {e}");
    }
}

// ---------------------------------------------------------------------------
// Public analysis entry point
// ---------------------------------------------------------------------------

/// Execute all analysis passes against `ast` using the pre-built `ctx`.
fn run_passes(ast: &Ast, ctx: &AnalysisContext) -> Vec<AnalysisError> {
    let mut errors: Vec<AnalysisError> = Vec::new();

    // ── Errors ────────────────────────────────────────────────────────────
    errors.extend(top_level::check(ast));
    errors.extend(exhaustiveness::check(ast, ctx));
    errors.extend(types::check(ast, ctx));
    errors.extend(dead_end::check(ast));
    errors.extend(menu_structure::check(ast));
    errors.extend(const_reassign::check(ast));

    // ── Warnings ──────────────────────────────────────────────────────────
    errors.extend(labels::check(ast, ctx));
    errors.extend(unreachable_label::check(ast));
    errors.extend(empty_dialogue::check(ast));
    errors.extend(duplicate_menu_dest::check(ast));
    errors.extend(overwritten_assign::check(ast));
    errors.extend(unused_var::check(ast));
    errors.extend(dead_branch::check(ast));
    errors.extend(possible_typo::check(ast, ctx));

    // ── Opt-in ────────────────────────────────────────────────────────────
    errors.extend(loop_detection::check(ast));

    errors
}

/// Run all analysis passes over `ast` and collect every diagnostic.
///
/// All passes always run to completion regardless of errors found by the
/// others; the returned `Vec` may contain diagnostics from multiple passes.
///
/// ## Pass order (errors first, then warnings)
///
/// **Errors:** `top_level` → `exhaustiveness` → `types` → `dead_end` →
/// `menu_structure` (empty menus) → `const_reassign`
///
/// **Warnings:** `labels` → `unreachable_label` → `menu_structure` (single-option) →
/// `empty_dialogue` → `duplicate_menu_dest` → `overwritten_assign` →
/// `unused_var` → `dead_branch` → `possible_typo`
///
/// **Opt-in:** `loop_detection` (only fires for labels decorated with
/// `@lint(check_loops)`)
pub fn analyze(ast: &Ast) -> Vec<AnalysisError> {
    let ctx = AnalysisContext::build(ast);
    run_passes(ast, &ctx)
}

/// Like [`analyze`] but also checks cross-module type references using
/// struct, enum, and label definitions from imported modules.
///
/// `imported_structs` maps `"alias.StructName"` → field list
/// (e.g. `"chars.Character"` → `[StructField { name: "hp", .. }, ...]`).
///
/// `imported_enums` maps `"alias.EnumName"` → variant list
/// (e.g. `"chars.Faction"` → `["hero", "villain"]`).
///
/// `imported_labels` is the set of label names that have been imported directly
/// into this file's scope without a qualifier (e.g. `"show_inventory"` from
/// `import (show_inventory) from "items.urd"`).  These are merged into
/// [`AnalysisContext::labels`] so that the label-resolution pass does not
/// raise [`AnalysisError::UndefinedLabel`] for them.
///
/// Both qualified (`"chars.Character"`) and unqualified (`"Character"`) keys
/// should be present in the maps so that the type checker can resolve either
/// spelling.  See [`context::AnalysisContext::build_with_imports`] for details.
pub fn analyze_with_imports(
    ast: &Ast,
    imported_structs: std::collections::HashMap<String, Vec<crate::parser::ast::StructField>>,
    imported_enums: std::collections::HashMap<String, Vec<String>>,
    imported_labels: std::collections::HashSet<String>,
) -> Vec<AnalysisError> {
    let ctx =
        AnalysisContext::build_with_imports(ast, imported_structs, imported_enums, imported_labels);
    run_passes(ast, &ctx)
}
