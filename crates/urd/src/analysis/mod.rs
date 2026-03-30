//! # Static Analysis Module
//!
//! This module provides best-effort static analysis passes over a parsed Urd AST.
//!
//! ## Passes
//!
//! - [`exhaustiveness`]: Checks that `match` statements cover all variants of the enum
//!   being scrutinised (or contain a wildcard arm).
//! - [`types`]: Checks that assigned values are compatible with declared type annotations.
//! - [`dead_end`]: Checks that every execution path ends with a recognised terminator
//!   (`end!`, `todo!`, `return`, or `jump`).
//!
//! All three passes always run to completion; errors from one pass do not suppress the
//! others.

pub mod context;
pub mod dead_end;
pub mod exhaustiveness;
pub mod labels;
pub mod types;

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
// AnalysisError
// ---------------------------------------------------------------------------

/// A single diagnostic produced by the static analyser.
#[derive(Debug, Clone, PartialEq)]
pub enum AnalysisError {
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
        }
    }

    /// Returns `true` if the span is a zero span (i.e. no real source location).
    fn is_zero_span(span: &SimpleSpan) -> bool {
        span.start == 0 && span.end == 0
    }

    /// Format a span as a human-readable location string.
    ///
    /// Returns `"(unknown location)"` for zero spans and `"byte S..E"` otherwise.
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
                span,
            } => {
                let loc = Self::format_span_loc(span);
                format!(
                    "Non-exhaustive match on enum '{enum_name}' at {loc}: \
                     missing variants: {}",
                    missing_variants.join(", ")
                )
            }

            AnalysisError::TypeMismatch {
                variable,
                expected,
                got,
                span,
            } => {
                let loc = Self::format_span_loc(span);
                format!(
                    "Type mismatch for '{variable}' at {loc}: \
                     expected {expected:?}, got {got}"
                )
            }

            AnalysisError::StructMismatch {
                variable,
                struct_name,
                field_errors,
                span,
            } => {
                let loc = Self::format_span_loc(span);
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
                    "Struct mismatch for '{variable}' at {loc}: \
                     expected struct '{struct_name}': {}",
                    details.join("; ")
                )
            }

            AnalysisError::UndefinedLabel { label, span } => {
                let loc = Self::format_span_loc(span);
                format!("Undefined label '{label}' at {loc}")
            }

            AnalysisError::DeadEnd { span, description } => {
                let loc = if Self::is_zero_span(span) {
                    description.to_string()
                } else {
                    format!("byte {}..{}", span.start, span.end)
                };
                format!(
                    "Dead end at {loc}: execution path has no terminator \
                     (use `end!`, `todo!`, `return`, or `jump`)"
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

/// Render all diagnostics as rich ariadne reports, writing to `writer`.
///
/// For errors with a real source span the output is a colourful annotated
/// code snippet (ariadne style).  For **zero-span** errors (AST nodes built in
/// tests without parser spans) a plain `writeln!` fallback is used instead.
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
            writeln!(writer, "error: {}", error.message())?;
            continue;
        }

        // Logos produces byte offsets; ariadne's `sources()` indexes by
        // Unicode codepoint. Convert before building the report.
        let start = byte_to_char(src, span.start);
        let end = byte_to_char(src, span.end);
        let range = start..end;

        let name_owned = source_name.to_owned();

        let report = Report::<(String, std::ops::Range<usize>)>::build(
            ReportKind::Error,
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

/// Run all three analysis passes over `ast` and collect every diagnostic.
///
/// All three passes always run to completion regardless of errors found by the
/// others; the returned `Vec` may therefore contain diagnostics from multiple
/// passes interleaved by insertion order (exhaustiveness, then types, then
/// dead-end).
pub fn analyze(ast: &Ast) -> Vec<AnalysisError> {
    let ctx = AnalysisContext::build(ast);
    let mut errors: Vec<AnalysisError> = Vec::new();
    errors.extend(exhaustiveness::check(ast, &ctx));
    errors.extend(types::check(ast, &ctx));
    errors.extend(labels::check(ast, &ctx));
    errors.extend(dead_end::check(ast));
    errors
}
