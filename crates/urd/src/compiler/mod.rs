//! # Compiler Module
//!
//! This module transforms a parsed [`Ast`] into an [`IrGraph`] ready for VM
//! execution.  Compilation happens in two passes:
//!
//! 1. **Label scan** — walk the entire AST, collect every [`AstContent::LabeledBlock`]
//!    label name, and pre-allocate a [`IrNodeKind::Nop`] placeholder for each so that
//!    forward [`AstContent::Jump`] references can be resolved.
//!
//! 2. **Emit pass** — walk the AST again recursively, emitting [`IrNodeKind`]s into the
//!    graph.  The core function [`CompilerState::compile_node`] returns the *entry*
//!    [`NodeIndex`] of the sub-graph it just emitted; callers thread a `next` continuation
//!    through every call so that every chain is fully linked on the first pass.
//!    Control-flow edges are stored in the graph via [`IrEdge`] rather than inline fields.

pub mod loader;

mod helpers;
mod state;

#[cfg(test)]
mod tests;

use std::collections::HashSet;

use thiserror::Error;

use crate::{
    ir::IrGraph,
    parser::ast::Ast,
};

use state::CompilerState;

// ─── Public error type ────────────────────────────────────────────────────────

/// Errors that can occur during compilation of an [`Ast`] into an [`IrGraph`].
#[derive(Debug, Error)]
pub enum CompilerError {
    /// A [`AstContent::Jump`] targeted a label that was never defined.
    #[error("jump to unknown label `{0}`")]
    UnknownLabel(String),

    /// A cross-module jump targeted a label that exists but is not `@entry`.
    #[error("label `{0}` is not marked `@entry` and cannot be reached from another module")]
    PrivateLabel(String),

    /// Two labels with the same name were declared in the same compilation unit.
    #[error("duplicate label definition `{0}`")]
    DuplicateLabel(String),

    /// An AST node appeared at statement level where it is not permitted.
    #[error("invalid statement: {0}")]
    InvalidStatement(String),

    /// Failed to load a module during import resolution.
    #[error("module load error for '{path}': {message}")]
    ModuleLoadError {
        /// The import path that failed to load.
        path: String,
        /// Human-readable description of the failure.
        message: String,
    },

    /// Two whole-module imports used the same alias name.
    #[error("duplicate module alias `{0}`")]
    DuplicateAlias(String),

    /// A symbol import requested a label that does not exist in the target module.
    #[error("symbol `{symbol}` not found in module `{module}`")]
    MissingImportedSymbol {
        /// The symbol name that was requested.
        symbol: String,
        /// The module path where the symbol was expected.
        module: String,
    },

    /// A circular import was detected.
    #[error("circular import detected for '{0}'")]
    CircularImport(String),

    /// An internal compiler invariant was violated (indicates a compiler bug).
    #[error("internal compiler error: {0}")]
    Internal(String),
}

/// Maximum allowed compiler recursion depth.
const MAX_COMPILER_DEPTH: usize = 512;

// ─── Public API ───────────────────────────────────────────────────────────────

/// Stateless entry-point for Urd's AST-to-IR compiler.
///
/// Call [`Compiler::compile`] with a reference to the root [`Ast`] produced by
/// the parser to obtain an [`IrGraph`].
pub struct Compiler;

impl Compiler {
    /// Compile `ast` (the root of a parsed Urd script) into an [`IrGraph`].
    ///
    /// # Errors
    /// Returns [`CompilerError::UnknownLabel`] if a `jump` statement targets a
    /// label that does not exist anywhere in the script.
    ///
    /// Returns [`CompilerError::DuplicateLabel`] when the same label name is
    /// declared more than once in a single compilation unit.
    ///
    /// Returns [`CompilerError::InvalidStatement`] if an expression-only AST
    /// node appears at a position where a statement is expected.
    pub fn compile(ast: &Ast) -> Result<IrGraph, CompilerError> {
        let mut state = CompilerState::new();

        // Pass 1 — collect all label names and pre-allocate Nop placeholders.
        state.scan_labels(ast)?;

        // Pass 2 — emit IR nodes using top-level partitioning (@entry support).
        let entry = state.compile_top_level(ast)?;
        state.graph.entry = entry;

        Ok(state.graph)
    }

    /// Compile `ast` with access to a [`FileLoader`] for resolving `import` statements.
    ///
    /// This is the multi-file variant of [`Compiler::compile`]. When an
    /// `import "path" as alias` statement is encountered, the loader is called
    /// to fetch the source, which is then parsed and compiled recursively
    /// before being merged into the main graph.
    ///
    /// Circular imports are detected via a `visited` set of paths.
    pub fn compile_with_loader(
        ast: &Ast,
        loader: &dyn crate::vm::loader::FileLoader,
    ) -> Result<IrGraph, CompilerError> {
        // `in_progress` tracks modules currently on the recursion stack so we
        // can detect true import cycles (A→B→A).  `completed` tracks modules
        // that have already been fully compiled so we can skip them on
        // subsequent imports of the same path (diamond dependencies).
        let mut in_progress = HashSet::new();
        let mut completed = HashSet::new();
        loader::compile_recursive(ast, loader, &mut in_progress, &mut completed)
    }

    /// Compile `ast` into an [`IrGraph`] with localization ID generation enabled.
    ///
    /// `file_stem` should be the filename without extension (e.g. `"intro"` for `intro.urd`).
    /// All [`IrNodeKind::Dialogue`], [`IrNodeKind::Choice`], and option nodes will have
    /// their `loc_id` fields populated.
    ///
    /// The existing [`Compiler::compile`] remains unchanged — it produces `loc_id: None`
    /// everywhere, keeping all existing tests unaffected.
    pub fn compile_named(ast: &Ast, file_stem: &str) -> Result<IrGraph, CompilerError> {
        use crate::loc::IdContext;

        let mut state = CompilerState::new();
        state.id_ctx = Some(IdContext::new(file_stem));
        state.scan_labels(ast)?;
        let entry = state.compile_top_level(ast)?;
        state.graph.entry = entry;
        Ok(state.graph)
    }

    /// Compiles a multi-file script (resolving `import` statements via `loader`)
    /// while also assigning localisation keys based on the given `file_stem`.
    ///
    /// Combines the behaviour of [`Compiler::compile_named`] (loc_id generation)
    /// and [`Compiler::compile_with_loader`] (import resolution).
    pub fn compile_named_with_loader(
        ast: &Ast,
        file_stem: &str,
        loader: &dyn crate::vm::loader::FileLoader,
    ) -> Result<IrGraph, CompilerError> {
        crate::compiler::loader::compile_recursive_with_root_path(ast, file_stem, loader)
    }
}
