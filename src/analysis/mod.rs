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

use crate::parser::ast::{Ast, TypeAnnotation};
use context::AnalysisContext;

/// Best-effort source location.
///
/// The AST does not embed source spans, so we use a human-readable description
/// that places a diagnostic in the reader's mental model of the script.
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

/// A single diagnostic produced by the static analyser.
#[derive(Debug, Clone, PartialEq)]
pub enum AnalysisError {
    /// A `match` over an enum did not cover all variants and had no wildcard arm.
    NonExhaustiveMatch {
        /// The name of the enum being matched over.
        enum_name: String,
        /// The variant names that are not covered by any arm.
        missing_variants: Vec<String>,
        /// Where in the script the match statement appears.
        location: NodeDescription,
    },

    /// A value was assigned to a variable whose declared type is incompatible.
    TypeMismatch {
        /// The variable being assigned to.
        variable: String,
        /// The type that was declared for the variable.
        expected: TypeAnnotation,
        /// A human-readable description of the actual value's type.
        got: String,
        /// Where in the script the assignment appears.
        location: NodeDescription,
    },

    /// An execution path reaches its end without a recognised terminator.
    ///
    /// Valid terminators are `end!`, `todo!`, a bare `return`, or a `jump`.
    DeadEnd {
        /// The path that is missing a terminator.
        location: NodeDescription,
    },
}

impl std::fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalysisError::NonExhaustiveMatch {
                enum_name,
                missing_variants,
                location,
            } => write!(
                f,
                "Non-exhaustive match on enum '{enum_name}' at {location}: \
                 missing variants: {}",
                missing_variants.join(", ")
            ),

            AnalysisError::TypeMismatch {
                variable,
                expected,
                got,
                location,
            } => write!(
                f,
                "Type mismatch for '{variable}' at {location}: \
                 expected {expected:?}, got {got}"
            ),

            AnalysisError::DeadEnd { location } => write!(
                f,
                "Dead end at {location}: execution path has no terminator \
                 (use `end!`, `todo!`, `return`, or `jump`)"
            ),
        }
    }
}

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
