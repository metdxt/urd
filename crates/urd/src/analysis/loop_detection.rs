//! # Infinite Dialogue Loop Detection (Not Implemented)
//!
//! Detects label cycles where every path stays within the cycle and no path
//! can escape to a terminal (`end!`, `todo!`, `return`).
//!
//! ## Implementation status
//!
//! This module is a **placeholder**. The detector is not implemented yet and
//! returns no diagnostics.
//!
//! It is also intentionally **inactive in the analysis pipeline** (the pass is
//! not executed from `analysis::run_passes`) to keep behavior explicit and avoid
//! implying loop checks are currently enforced.
//!
//! ## Planned algorithm
//!
//! A future implementation is expected to:
//! 1. Build the label jump-graph.
//! 2. Run Tarjan's SCC algorithm.
//! 3. For each SCC where no node has an escaping edge, check whether any
//!    label in the SCC carries `@lint(check_loops)`.
//! 4. If so, emit [`AnalysisError::InfiniteDialogueLoop`] for that label.

use crate::parser::ast::Ast;

use super::AnalysisError;

/// Placeholder loop-detection pass.
///
/// This function is intentionally not implemented and always returns an empty
/// diagnostic list. The analysis pipeline currently keeps this pass inactive
/// until a real SCC-based implementation lands.
pub fn check(_ast: &Ast) -> Vec<AnalysisError> {
    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::loader::parse_source;

    #[test]
    fn not_implemented_detector_returns_empty_for_any_input() {
        let src = "@entry\nlabel start {\n    jump start\n}\n";
        let ast = parse_source(src).expect("parse should succeed");
        let errors = check(&ast);
        assert!(
            errors.is_empty(),
            "not-implemented detector must return no diagnostics, got: {errors:?}"
        );
    }

    #[test]
    fn not_implemented_detector_returns_empty_for_empty_input() {
        let ast = crate::parser::ast::Ast::block(vec![]);
        let errors = check(&ast);
        assert!(errors.is_empty());
    }
}
