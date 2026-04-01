//! # Infinite Dialogue Loop Detection (Opt-in)
//!
//! Detects label cycles where every path stays within the cycle and no path
//! can escape to a terminal (`end!`, `todo!`, `return`).
//!
//! ## Opt-in mechanism
//!
//! This pass is **disabled by default**. It only emits diagnostics for labels
//! that carry the `@lint(check_loops)` decorator. Authors who intentionally
//! use infinite loops (hub worlds, pause menus) can leave the decorator off.
//!
//! ## Algorithm (stub)
//!
//! A full implementation would:
//! 1. Build the label jump-graph.
//! 2. Run Tarjan's SCC algorithm.
//! 3. For each SCC where no node has an escaping edge, check whether any
//!    label in the SCC carries `@lint(check_loops)`.
//! 4. If so, emit [`AnalysisError::InfiniteDialogueLoop`] for that label.
//!
//! The current implementation is a **placeholder stub** that always returns
//! an empty diagnostic list. The full SCC-based implementation will be added
//! in a future pass.

use crate::parser::ast::Ast;

use super::AnalysisError;

/// Run the opt-in infinite-loop detection pass over `ast`.
///
/// Currently a stub — always returns an empty `Vec`. The full SCC-based
/// implementation is tracked as a future enhancement.
pub fn check(_ast: &Ast) -> Vec<AnalysisError> {
    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;

    #[test]
    fn stub_returns_empty_for_any_input() {
        let src = "@entry\nlabel start {\n    jump start\n}\n";
        let ast = parse_source(src).expect("parse should succeed");
        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "stub should return no errors, got: {errors:?}"
        );
    }

    #[test]
    fn stub_returns_empty_for_empty_input() {
        let ast = crate::parser::ast::Ast::block(vec![]);
        let errors = check(&ast);
        assert!(errors.is_empty());
    }
}
