//! # Semantic suggestion trait
//!
//! Defines the [`SemanticSuggest`] trait used as a fallback in the undefined-variable
//! pass when Levenshtein edit distance produces no close match.
//!
//! The concrete implementation lives in `urd-lsp` and uses the `potion-base-2M`
//! static embedding model via ONNX Runtime.  The `urd` crate only depends on
//! this trait (no ML crates pulled in here).

/// Pluggable backend for finding a semantically similar variable name.
///
/// Implementations compute dense embeddings for identifier names and return the
/// candidate whose embedding is most similar to the query's embedding, provided
/// the similarity exceeds an implementation-defined threshold.
///
/// # Identifier preprocessing
///
/// Implementors should split snake\_case names on `_` before embedding (e.g.
/// `player_name` → `"player name"`) so that the natural-language model has
/// a better chance of producing a meaningful vector.
pub trait SemanticSuggest: Send + Sync {
    /// Return the name from `candidates` that is most semantically similar to
    /// `query`, or `None` if no candidate clears the similarity threshold.
    ///
    /// `candidates` is a slice of **all in-scope variable names** at the point
    /// where `query` was undefined.  The implementation is free to embed
    /// all of them or only a subset.
    fn find_synonym(&self, query: &str, candidates: &[String]) -> Option<String>;
}
