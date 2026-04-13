//! # Possible Typo Analysis
//!
//! Detects likely typos in speaker names in dialogue lines
//! (e.g. `zra:` instead of `zara:`).
//!
//! Label-typo detection has been unified into the `labels` pass, which embeds
//! any suggestion directly in [`crate::analysis::AnalysisError::UndefinedLabel`].
//!
//! The primary detection strategy is Levenshtein edit distance (≤ 2) with
//! frequency / existence guards to keep false-positive rates low.  When
//! Levenshtein finds no close match, an optional [`SemanticSuggest`] backend
//! is consulted as a fallback.

use std::collections::{HashMap, HashSet};

use chumsky::span::{SimpleSpan, Span};

use crate::analysis::semantic_suggest::SemanticSuggest;
use crate::analysis::{AnalysisError, TypoKind};
use crate::parser::ast::{Ast, AstContent, Operator, walk_ast};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the possible-typo detection pass over `ast`.
///
/// Checks speaker names for likely typos (edit distance ≤ 2, with frequency
/// guard) and returns all discovered [`AnalysisError::PossibleTypo`] warnings.
///
/// Label-typo detection has been unified into the `labels` pass, which embeds
/// any suggestion directly in [`AnalysisError::UndefinedLabel`].
///
/// When `semantic` is `Some`, it is used as a fallback for speaker names
/// whenever Levenshtein edit distance produces no close match.
#[must_use]
pub fn check(ast: &Ast, semantic: Option<&dyn SemanticSuggest>) -> Vec<AnalysisError> {
    let mut errors = Vec::new();
    errors.extend(check_speaker_typos(ast, semantic));
    // check_label_typos removed — label suggestions now live in labels::check
    errors
}

// ---------------------------------------------------------------------------
// Sub-check 1: Speaker typos
// ---------------------------------------------------------------------------

/// Detect speaker identifiers that appear only once and are very close
/// (edit distance ≤ 2) to a name that appears ≥ 3× more often.
///
/// When `semantic` is `Some` and Levenshtein finds no match, the semantic
/// model is consulted.  The frequency guard (candidate must appear ≥ 3× as
/// often) still applies for semantic matches to suppress false positives.
fn check_speaker_typos(ast: &Ast, semantic: Option<&dyn SemanticSuggest>) -> Vec<AnalysisError> {
    // Pass 1 — collect frequency and the span of the first occurrence for
    // every speaker name encountered across all Dialogue nodes.
    let mut freq: HashMap<String, usize> = HashMap::new();
    let mut first_span: HashMap<String, SimpleSpan> = HashMap::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::Dialogue { speakers, .. } = node.content()
            && let AstContent::ExprList(exprs) = speakers.content()
        {
            for expr in exprs {
                if let AstContent::Value(RuntimeValue::IdentPath(path)) = expr.content()
                    && path.len() == 1
                {
                    let name = path[0].clone();
                    *freq.entry(name.clone()).or_insert(0) += 1;
                    first_span.entry(name).or_insert_with(|| expr.span());
                }
            }
        }
    });

    // Pass 2 — for every name appearing exactly once, find the closest
    // other name.  Emit a warning if that closest name appears ≥ 3× as often
    // AND the edit distance is ≤ 2.
    let names: Vec<String> = freq.keys().cloned().collect();
    let mut errors = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    for (written, &count) in &freq {
        if count != 1 || seen.contains(written.as_str()) {
            continue;
        }

        // Candidates: every other known speaker name.
        let other_names: Vec<String> = names
            .iter()
            .filter(|n| n.as_str() != written.as_str())
            .cloned()
            .collect();

        // Try Levenshtein first; fall back to semantic when it finds nothing.
        let suggestion = closest_match(written, other_names.iter())
            .and_then(|(dist, s)| if dist <= 2 { Some(s) } else { None })
            .or_else(|| semantic.and_then(|m| m.find_synonym(written, &other_names)));

        if let Some(suggestion) = suggestion {
            let candidate_freq = freq.get(&suggestion).copied().unwrap_or(0);
            // Only flag if the suggestion is substantially more common — this
            // guard applies to both Levenshtein and semantic matches to keep
            // false-positive rates low for speaker names.
            if candidate_freq >= 3 * count {
                seen.insert(written.clone());
                let span = first_span
                    .get(written)
                    .copied()
                    .unwrap_or(SimpleSpan::new((), 0..0));
                errors.push(AnalysisError::PossibleTypo {
                    written: written.clone(),
                    suggestion,
                    kind: TypoKind::Speaker,
                    span,
                });
            }
        }
    }

    errors
}

// ---------------------------------------------------------------------------
// Helper: walk AST collecting non-LHS IdentPath references
// ---------------------------------------------------------------------------

/// Recursively walk `node`, appending `(name, span)` to `refs` for every
/// single-segment `Value(IdentPath([name]))` node that is in a **non-LHS**,
/// **non-speaker** read position.
///
/// `in_lhs` signals that the current subtree is the write target of an
/// assignment or declaration and should therefore be skipped.
pub(crate) fn collect_var_refs(node: &Ast, in_lhs: bool, refs: &mut Vec<(String, SimpleSpan)>) {
    match node.content() {
        // ── Leaf: single-segment identifier in a value (read) position ─────────
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 && !in_lhs => {
            refs.push((path[0].clone(), node.span()));
        }
        // Other literal values: no children, nothing to do.
        AstContent::Value(_) => {}

        // ── Assignment: left is write, right is read ────────────────────────────
        AstContent::BinOp {
            op: Operator::Assign,
            left,
            right,
        } => {
            collect_var_refs(left, true, refs);
            collect_var_refs(right, false, refs);
        }

        // ── Variable / const / global declaration ───────────────────────────────
        AstContent::Declaration {
            decl_name,
            decl_defs,
            ..
        } => {
            collect_var_refs(decl_name, true, refs);
            collect_var_refs(decl_defs, false, refs);
        }

        // ── Subscript assignment: `obj[key] = value` ────────────────────────────
        // The object is being written to; key and value are reads.
        AstContent::SubscriptAssign { object, key, value } => {
            collect_var_refs(object, true, refs);
            collect_var_refs(key, false, refs);
            collect_var_refs(value, false, refs);
        }

        // ── Dialogue: speakers are handled by the speaker sub-check; skip them ──
        AstContent::Dialogue { content, .. } => {
            collect_var_refs(content, false, refs);
        }

        // ── Function call: func_path is a function reference, not a variable read
        AstContent::Call { params, .. } => {
            collect_var_refs(params, false, refs);
        }

        // ── Nodes whose only "identifiers" are plain Strings (no Ast children) ──
        AstContent::LetCall { .. }
        | AstContent::Jump { .. }
        | AstContent::EnumDecl { .. }
        | AstContent::StructDecl { .. }
        | AstContent::Import { .. } => {}

        // ── Match: check the scrutinee and each arm body; skip arm patterns ─────
        // Patterns are enum variant references and would produce false positives.
        AstContent::Match { scrutinee, arms } => {
            collect_var_refs(scrutinee, false, refs);
            for arm in arms {
                collect_var_refs(&arm.body, false, refs);
            }
        }

        // ── Decorator definition: only the body matters ─────────────────────────
        AstContent::DecoratorDef { body, .. } => {
            collect_var_refs(body, false, refs);
        }

        // ── Map literal: keys are static field names, not variable reads ─────────
        // `:{ name: "Narrator", name_color: "#aaa" }` — `name` and `name_color`
        // are struct-field-style keys, never variable references.
        AstContent::Map(pairs) => {
            for (_, value) in pairs {
                collect_var_refs(value, false, refs);
            }
        }

        // ── Default: recurse into every child with in_lhs = false ───────────────
        // Covers: BinOp (non-assign), UnaryOp, ExprList, Block, List, If,
        // LabeledBlock, Menu, MenuOption, Return, Subscript, etc.
        _ => {
            for child in node.children() {
                collect_var_refs(child, false, refs);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helper: find closest candidate by Levenshtein distance
// ---------------------------------------------------------------------------

/// Return `(distance, candidate.clone())` for the candidate in `candidates`
/// that has the smallest Levenshtein distance to `name`.
///
/// Returns `None` if the iterator yields no items.  Ties are broken in favour
/// of the first candidate encountered.
pub(crate) fn closest_match<'a>(
    name: &str,
    candidates: impl Iterator<Item = &'a String>,
) -> Option<(usize, String)> {
    let mut best: Option<(usize, String)> = None;
    for candidate in candidates {
        let dist = levenshtein(name, candidate);
        match &best {
            None => best = Some((dist, candidate.clone())),
            Some((best_dist, _)) if dist < *best_dist => {
                best = Some((dist, candidate.clone()));
            }
            _ => {}
        }
    }
    best
}

// ---------------------------------------------------------------------------
// Levenshtein distance
// ---------------------------------------------------------------------------

/// Classic iterative Levenshtein (edit) distance.
///
/// Returns `0` when `a == b`, and increases by one for each insertion,
/// deletion, or single-character substitution required to transform `a`
/// into `b`.
pub(crate) fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let m = a.len();
    let n = b.len();

    // `prev[j]` is the edit distance between a[0..i-1] and b[0..j].
    let mut prev: Vec<usize> = (0..=n).collect();
    let mut curr: Vec<usize> = vec![0usize; n + 1];

    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = usize::from(a[i - 1] != b[j - 1]);
            curr[j] = (curr[j - 1] + 1) // insertion
                .min(prev[j] + 1) // deletion
                .min(prev[j - 1] + cost); // substitution / match
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[n]
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::loader::parse_source;

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    // ── Levenshtein unit tests ────────────────────────────────────────────────

    #[test]
    fn levenshtein_same_string_is_zero() {
        assert_eq!(levenshtein("hello", "hello"), 0);
    }

    #[test]
    fn levenshtein_both_empty_is_zero() {
        assert_eq!(levenshtein("", ""), 0);
    }

    #[test]
    fn levenshtein_one_insertion() {
        // "cat" → "cats": insert 's' at the end
        assert_eq!(levenshtein("cat", "cats"), 1);
    }

    #[test]
    fn levenshtein_one_deletion() {
        // "cats" → "cat": delete 's'
        assert_eq!(levenshtein("cats", "cat"), 1);
    }

    #[test]
    fn levenshtein_one_substitution() {
        // "bat" → "cat": substitute 'b' → 'c'
        assert_eq!(levenshtein("bat", "cat"), 1);
    }

    #[test]
    fn levenshtein_two_edits_two_insertions() {
        // "ab" → "abcd": two insertions
        assert_eq!(levenshtein("ab", "abcd"), 2);
    }

    #[test]
    fn levenshtein_two_edits_two_substitutions() {
        // "abcd" → "axyd": substitute b→x, c→y
        assert_eq!(levenshtein("abcd", "axyd"), 2);
    }

    #[test]
    fn levenshtein_completely_different_strings() {
        // "abc" → "xyz": three substitutions
        assert_eq!(levenshtein("abc", "xyz"), 3);
    }

    #[test]
    fn levenshtein_empty_vs_nonempty() {
        assert_eq!(levenshtein("", "abc"), 3);
        assert_eq!(levenshtein("abc", ""), 3);
    }

    #[test]
    fn levenshtein_typical_typo_transposition() {
        // "visted_cave" (missing 'i') vs "visited_cave": one deletion
        assert_eq!(levenshtein("visted_cave", "visited_cave"), 1);
    }

    // ── Speaker typo tests ────────────────────────────────────────────────────

    #[test]
    fn speaker_typo_detected() {
        // "zara" appears 3 times (≥ 3× "zra"), "zra" appears once — distance is 1.
        let src = r#"
label scene {
    zara: "Line one."
    zara: "Line two."
    zara: "Line three."
    zra:  "Oops."
    end!()
}
"#;
        let ast = parse(src);
        let errors = check_speaker_typos(&ast, None);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::PossibleTypo {
                    written,
                    suggestion,
                    kind: TypoKind::Speaker,
                    ..
                } if written == "zra" && suggestion == "zara"
            )),
            "expected PossibleTypo(Speaker, \"zra\" → \"zara\"), got: {errors:?}"
        );
    }

    #[test]
    fn speaker_unique_name_no_close_match_not_flagged() {
        // "zaxxon" has no spelling-similar peer — should never be flagged.
        let src = r#"
label scene {
    narrator: "Line one."
    narrator: "Line two."
    zaxxon: "A totally unique line."
    end!()
}
"#;
        let ast = parse(src);
        let errors = check_speaker_typos(&ast, None);
        let flagged: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Speaker,
                        ..
                    }
                )
            })
            .collect();
        assert!(
            flagged.is_empty(),
            "expected no speaker typos, got: {flagged:?}"
        );
    }

    #[test]
    fn speaker_frequency_threshold_not_met_not_flagged() {
        // "zara" appears only 2 times; the 3× threshold requires ≥ 3, so "zra" is not flagged.
        let src = r#"
label scene {
    zara: "Line one."
    zara: "Line two."
    zra:  "Close, but threshold not met."
    end!()
}
"#;
        let ast = parse(src);
        let errors = check_speaker_typos(&ast, None);
        let flagged: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Speaker,
                        ..
                    }
                )
            })
            .collect();
        assert!(
            flagged.is_empty(),
            "expected no speaker typos (threshold not met), got: {flagged:?}"
        );
    }

    // ── Integration: check() aggregates all three sub-checks ─────────────────

    /// Helper that calls `check` with `semantic = None` for all existing tests.
    fn check_no_semantic(ast: &Ast) -> Vec<AnalysisError> {
        check(ast, None)
    }

    // ── Mock SemanticSuggest ──────────────────────────────────────────────────

    use crate::analysis::semantic_suggest::SemanticSuggest;

    /// A deterministic mock that returns a fixed suggestion for one specific
    /// query string, and `None` for everything else.
    struct MockSemantic {
        query: &'static str,
        answer: &'static str,
    }

    impl SemanticSuggest for MockSemantic {
        fn find_synonym(&self, query: &str, candidates: &[String]) -> Option<String> {
            if query == self.query && candidates.iter().any(|c| c == self.answer) {
                Some(self.answer.to_owned())
            } else {
                None
            }
        }
    }

    #[test]
    fn check_collects_all_sub_check_results() {
        // Speaker typo in the script; label-typo detection now lives in labels::check.
        let src = r#"
label start {
    end!()
}
label main_scene {
    zara: "Line one."
    zara: "Line two."
    zara: "Line three."
    zra: "Oops, typo speaker."
    jump staart
}
"#;
        let ast = parse(src);
        let errors = check_no_semantic(&ast);

        let speaker_count = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Speaker,
                        ..
                    }
                )
            })
            .count();
        let label_count = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Label,
                        ..
                    }
                )
            })
            .count();

        assert_eq!(
            speaker_count, 1,
            "expected 1 speaker typo, got {speaker_count}"
        );
        // Label suggestions are now embedded in UndefinedLabel, not emitted as
        // PossibleTypo — this check must produce zero label-kind warnings.
        assert_eq!(
            label_count, 0,
            "expected 0 label typos from possible_typo::check, got {label_count}"
        );
    }

    #[test]
    fn semantic_fallback_fires_for_speaker_beyond_levenshtein_threshold() {
        // "narrator" (appears once) vs "storyteller" (appears 3× more) —
        // edit distance >> 2, so Levenshtein alone produces nothing.
        // The mock returns "storyteller", proving the semantic path is reached.
        let src = r#"
label scene {
    storyteller: "Line one."
    storyteller: "Line two."
    storyteller: "Line three."
    narrator: "A different voice."
    end!()
}
"#;
        let ast = parse(src);
        let semantic = MockSemantic {
            query: "narrator",
            answer: "storyteller",
        };
        let errors = check(&ast, Some(&semantic));
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::PossibleTypo {
                    written,
                    suggestion,
                    kind: TypoKind::Speaker,
                    ..
                } if written == "narrator" && suggestion == "storyteller"
            )),
            "expected semantic PossibleTypo(Speaker, \"narrator\" → \"storyteller\"), got: {errors:?}"
        );
    }
}
