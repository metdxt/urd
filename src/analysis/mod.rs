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
pub mod types;

#[cfg(test)]
mod tests;

use chumsky::span::SimpleSpan;
use ariadne::{Label, Report, ReportKind, sources};

use crate::parser::ast::{Ast, TypeAnnotation};
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
            AnalysisError::DeadEnd { span, .. } => *span,
        }
    }

    /// Returns `true` if the span is a zero span (i.e. no real source location).
    fn is_zero_span(span: &SimpleSpan) -> bool {
        span.start == 0 && span.end == 0
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
                let loc = if Self::is_zero_span(span) {
                    "(unknown location)".to_owned()
                } else {
                    format!("byte {}..{}", span.start, span.end)
                };
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
                let loc = if Self::is_zero_span(span) {
                    "(unknown location)".to_owned()
                } else {
                    format!("byte {}..{}", span.start, span.end)
                };
                format!(
                    "Type mismatch for '{variable}' at {loc}: \
                     expected {expected:?}, got {got}"
                )
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
            } => format!(
                "'{variable}' expects {expected:?} but got {got}"
            ),

            AnalysisError::DeadEnd { description, .. } => format!(
                "{description}: no terminator on this path"
            ),
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

        let start = span.start;
        let end = span.end;
        let range = start..end;

        let name_owned = source_name.to_owned();

        let report = Report::<(String, std::ops::Range<usize>)>::build(
            ReportKind::Error,
            (name_owned.clone(), range.clone()),
        )
        .with_message(error.message())
        .with_label(
            Label::new((name_owned.clone(), range))
                .with_message(error.label_message()),
        )
        .finish();

        report
            .write(sources([(name_owned, src.to_owned())]), &mut *writer)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;
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
    errors.extend(dead_end::check(ast));
    errors
}
