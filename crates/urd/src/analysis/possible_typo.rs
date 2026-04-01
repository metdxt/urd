//! # Possible Typo Analysis
//!
//! Detects likely typos in identifiers across three namespaces:
//!
//! - **Speaker names** in dialogue lines (e.g. `<zra>:` instead of `<zara>:`).
//! - **Jump target labels** (e.g. `jump staart` instead of `jump start`).
//! - **Variable references** (e.g. `if visted_cave` instead of `if visited_cave`).
//!
//! Uses Levenshtein edit distance (≤ 2) plus frequency / existence guards to
//! keep false-positive rates low.

use std::collections::{HashMap, HashSet};

use chumsky::span::{SimpleSpan, Span};

use crate::analysis::context::AnalysisContext;
use crate::analysis::{AnalysisError, TypoKind};
use crate::parser::ast::{Ast, AstContent, Operator, walk_ast};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the possible-typo detection pass over `ast`.
///
/// Combines three independent sub-checks — speakers, labels, variables — and
/// returns all discovered [`AnalysisError::PossibleTypo`] warnings.
pub fn check(ast: &Ast, ctx: &AnalysisContext) -> Vec<AnalysisError> {
    let mut errors = Vec::new();
    errors.extend(check_speaker_typos(ast));
    errors.extend(check_label_typos(ast, ctx));
    errors.extend(check_variable_typos(ast, ctx));
    errors
}

// ---------------------------------------------------------------------------
// Sub-check 1: Speaker typos
// ---------------------------------------------------------------------------

/// Detect speaker identifiers that appear only once and are very close
/// (edit distance ≤ 2) to a name that appears ≥ 3× more often.
fn check_speaker_typos(ast: &Ast) -> Vec<AnalysisError> {
    // Pass 1 — collect frequency and the span of the first occurrence for
    // every speaker name encountered across all Dialogue nodes.
    let mut freq: HashMap<String, usize> = HashMap::new();
    let mut first_span: HashMap<String, SimpleSpan> = HashMap::new();

    walk_ast(ast, &mut |node| {
        if let AstContent::Dialogue { speakers, .. } = node.content() {
            if let AstContent::ExprList(exprs) = speakers.content() {
                for expr in exprs {
                    if let AstContent::Value(RuntimeValue::IdentPath(path)) = expr.content() {
                        if path.len() == 1 {
                            let name = path[0].clone();
                            *freq.entry(name.clone()).or_insert(0) += 1;
                            first_span.entry(name).or_insert_with(|| node.span());
                        }
                    }
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
        let candidates = names.iter().filter(|n| n.as_str() != written.as_str());

        if let Some((dist, suggestion)) = closest_match(written, candidates) {
            if dist <= 2 {
                let candidate_freq = freq.get(&suggestion).copied().unwrap_or(0);
                // Only flag if the suggestion is substantially more common.
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
    }

    errors
}

// ---------------------------------------------------------------------------
// Sub-check 2: Label typos
// ---------------------------------------------------------------------------

/// Detect jump / let-call targets that are not defined but are very close
/// (edit distance ≤ 2) to a label that is defined.
///
/// Cross-module targets (containing `.`) are skipped — the single-file
/// analyser cannot validate those.  Targets with no close match are also
/// skipped: `UndefinedLabel` from `labels.rs` is already sufficient there.
fn check_label_typos(ast: &Ast, ctx: &AnalysisContext) -> Vec<AnalysisError> {
    let defined: Vec<String> = ctx.labels.iter().cloned().collect();
    let mut errors = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    walk_ast(ast, &mut |node| {
        // Extract the target string from Jump or LetCall; skip the node otherwise.
        let target: String = match node.content() {
            AstContent::Jump { label, .. } if !label.contains('.') => {
                if ctx.labels.contains(label) {
                    return; // defined — nothing to report
                }
                label.clone()
            }
            AstContent::LetCall { target, .. } if !target.contains('.') => {
                if ctx.labels.contains(target) {
                    return;
                }
                target.clone()
            }
            _ => return,
        };

        if seen.contains(&target) {
            return;
        }

        if let Some((dist, suggestion)) = closest_match(&target, defined.iter()) {
            if dist <= 2 {
                seen.insert(target.clone());
                errors.push(AnalysisError::PossibleTypo {
                    written: target,
                    suggestion,
                    kind: TypoKind::Label,
                    span: node.span(),
                });
            }
        }
    });

    errors
}

// ---------------------------------------------------------------------------
// Sub-check 3: Variable typos
// ---------------------------------------------------------------------------

/// Detect single-segment identifiers used in value positions that are not
/// declared anywhere and are very close (edit distance ≤ 2) to a declared
/// variable name.
fn check_variable_typos(ast: &Ast, ctx: &AnalysisContext) -> Vec<AnalysisError> {
    // Universe of "known" names that should NOT be flagged as typos.
    let known_vars = collect_all_declared_vars(ast);
    let enum_variants: HashSet<String> = ctx.enums.values().flatten().cloned().collect();
    let known_var_list: Vec<String> = known_vars.iter().cloned().collect();

    // Collect every single-segment IdentPath used in a non-LHS, non-speaker position.
    let mut refs: Vec<(String, SimpleSpan)> = Vec::new();
    collect_var_refs(ast, false, &mut refs);

    let mut errors = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    for (name, span) in refs {
        // ----- exclusion guards -------------------------------------------------
        // Too short: too many false positives for short names.
        if name.len() <= 2 {
            continue;
        }
        // Common wildcard / placeholder.
        if name == "_" {
            continue;
        }
        // Built-in terminators.
        if name == "end!" || name == "todo!" {
            continue;
        }
        // Already declared — not a typo.
        if known_vars.contains(&name) {
            continue;
        }
        // A label name used as a value — different kind of error.
        if ctx.labels.contains(&name) {
            continue;
        }
        // An enum variant — correct usage, not a typo.
        if enum_variants.contains(&name) {
            continue;
        }
        // Already emitted a warning for this identifier.
        if seen.contains(&name) {
            continue;
        }
        // -----------------------------------------------------------------------

        if let Some((dist, suggestion)) = closest_match(&name, known_var_list.iter()) {
            if dist <= 2 {
                seen.insert(name.clone());
                errors.push(AnalysisError::PossibleTypo {
                    written: name,
                    suggestion,
                    kind: TypoKind::Variable,
                    span,
                });
            }
        }
    }

    errors
}

// ---------------------------------------------------------------------------
// Helper: collect all declared variable / const / global names
// ---------------------------------------------------------------------------

/// Walk the entire AST and collect the name of every `Declaration` and
/// `LetCall` binding, regardless of nesting depth.
fn collect_all_declared_vars(ast: &Ast) -> HashSet<String> {
    let mut vars = HashSet::new();
    walk_ast(ast, &mut |node| match node.content() {
        AstContent::Declaration { decl_name, .. } => {
            if let AstContent::Value(RuntimeValue::IdentPath(path)) = decl_name.content() {
                if path.len() == 1 {
                    vars.insert(path[0].clone());
                }
            }
        }
        AstContent::LetCall { name, .. } => {
            vars.insert(name.clone());
        }
        _ => {}
    });
    vars
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
fn collect_var_refs(node: &Ast, in_lhs: bool, refs: &mut Vec<(String, SimpleSpan)>) {
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

        // ── Default: recurse into every child with in_lhs = false ───────────────
        // Covers: BinOp (non-assign), UnaryOp, ExprList, Block, List, Map, If,
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
fn closest_match<'a>(
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
fn levenshtein(a: &str, b: &str) -> usize {
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
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::analysis::context::AnalysisContext;
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
    <zara>: "Line one."
    <zara>: "Line two."
    <zara>: "Line three."
    <zra>:  "Oops."
    end!()
}
"#;
        let ast = parse(src);
        let errors = check_speaker_typos(&ast);
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
    <narrator>: "Line one."
    <narrator>: "Line two."
    <zaxxon>: "A totally unique line."
    end!()
}
"#;
        let ast = parse(src);
        let errors = check_speaker_typos(&ast);
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
    <zara>: "Line one."
    <zara>: "Line two."
    <zra>:  "Close, but threshold not met."
    end!()
}
"#;
        let ast = parse(src);
        let errors = check_speaker_typos(&ast);
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

    // ── Label typo tests ──────────────────────────────────────────────────────

    #[test]
    fn label_typo_detected() {
        // "start" is defined; "staart" is not and has edit distance 1.
        let src = r#"
label start {
    end!()
}
label other {
    jump staart
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_label_typos(&ast, &ctx);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::PossibleTypo {
                    written,
                    suggestion,
                    kind: TypoKind::Label,
                    ..
                } if written == "staart" && suggestion == "start"
            )),
            "expected PossibleTypo(Label, \"staart\" → \"start\"), got: {errors:?}"
        );
    }

    #[test]
    fn label_typo_not_flagged_when_no_close_match() {
        // "completelymissing" has no label within edit distance 2 — no PossibleTypo.
        // (UndefinedLabel from labels.rs handles the existence check.)
        let src = r#"
label start {
    end!()
}
label other {
    jump completelymissing
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_label_typos(&ast, &ctx);
        let flagged: Vec<_> = errors
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
            .collect();
        assert!(
            flagged.is_empty(),
            "expected no label typos (no close match), got: {flagged:?}"
        );
    }

    #[test]
    fn defined_label_jump_not_flagged() {
        // Jumping to an existing label must never produce a PossibleTypo.
        let src = r#"
label start {
    end!()
}
label entry {
    jump start
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_label_typos(&ast, &ctx);
        assert!(
            errors.is_empty(),
            "expected no errors for a valid jump target, got: {errors:?}"
        );
    }

    #[test]
    fn let_call_typo_detected() {
        // `let x = jump staart and return` — "staart" is a typo of "start".
        let src = r#"
label start {
    end!()
}
label caller {
    let result = jump staart and return
    end!()
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_label_typos(&ast, &ctx);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::PossibleTypo {
                    written,
                    suggestion,
                    kind: TypoKind::Label,
                    ..
                } if written == "staart" && suggestion == "start"
            )),
            "expected PossibleTypo(Label, \"staart\" → \"start\") for let-call, got: {errors:?}"
        );
    }

    // ── Variable typo tests ───────────────────────────────────────────────────

    #[test]
    fn variable_typo_detected() {
        // "visited_cave" is declared; "visted_cave" (one deletion) is used in a condition.
        let src = r#"
let visited_cave: bool = false
label check {
    if visted_cave {
        end!()
    } else {
        end!()
    }
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_variable_typos(&ast, &ctx);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                AnalysisError::PossibleTypo {
                    written,
                    suggestion,
                    kind: TypoKind::Variable,
                    ..
                } if written == "visted_cave" && suggestion == "visited_cave"
            )),
            "expected PossibleTypo(Variable, \"visted_cave\" → \"visited_cave\"), got: {errors:?}"
        );
    }

    #[test]
    fn short_variable_names_not_flagged() {
        // Names with ≤ 2 characters must never produce a PossibleTypo, regardless
        // of how close they are to a declared variable.
        let src = r#"
let visited_cave: bool = false
label check {
    if vc {
        end!()
    } else {
        end!()
    }
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_variable_typos(&ast, &ctx);
        let flagged_vc: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Variable,
                        written,
                        ..
                    } if written == "vc"
                )
            })
            .collect();
        assert!(
            flagged_vc.is_empty(),
            "expected short identifier 'vc' not to be flagged, got: {flagged_vc:?}"
        );
    }

    #[test]
    fn correctly_used_variable_not_flagged() {
        // Using a variable that is actually declared should produce no warning.
        let src = r#"
let health: int = 100
label check {
    if health {
        end!()
    } else {
        end!()
    }
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_variable_typos(&ast, &ctx);
        let flagged: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Variable,
                        ..
                    }
                )
            })
            .collect();
        assert!(
            flagged.is_empty(),
            "expected no variable typos for a correctly used variable, got: {flagged:?}"
        );
    }

    #[test]
    fn variable_typo_no_close_match_not_flagged() {
        // An undeclared name with no close match should not produce a PossibleTypo.
        let src = r#"
let health: int = 100
label check {
    if completelydifferent {
        end!()
    } else {
        end!()
    }
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_variable_typos(&ast, &ctx);
        let flagged: Vec<_> = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Variable,
                        written,
                        ..
                    } if written == "completelydifferent"
                )
            })
            .collect();
        assert!(
            flagged.is_empty(),
            "expected no variable typos for a name with no close match, got: {flagged:?}"
        );
    }

    // ── Deduplication test ────────────────────────────────────────────────────

    #[test]
    fn duplicate_variable_typo_emitted_only_once() {
        // The same typo appearing in two places should only produce one warning.
        let src = r#"
let visited_cave: bool = false
label check {
    if visted_cave {
        end!()
    } else {
        if visted_cave {
            end!()
        } else {
            end!()
        }
    }
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check_variable_typos(&ast, &ctx);
        let count = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Variable,
                        written,
                        ..
                    } if written == "visted_cave"
                )
            })
            .count();
        assert_eq!(
            count, 1,
            "expected exactly one warning for repeated typo, got {count}"
        );
    }

    // ── Integration: check() aggregates all three sub-checks ─────────────────

    #[test]
    fn check_collects_all_sub_check_results() {
        // One typo of each kind in a single script.
        let src = r#"
let visited_cave: bool = false
label start {
    end!()
}
label main_scene {
    <zara>: "Line one."
    <zara>: "Line two."
    <zara>: "Line three."
    <zra>: "Oops, typo speaker."
    if visted_cave { end!() } else { end!() }
    jump staart
}
"#;
        let ast = parse(src);
        let ctx = AnalysisContext::build(&ast);
        let errors = check(&ast, &ctx);

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
        let var_count = errors
            .iter()
            .filter(|e| {
                matches!(
                    e,
                    AnalysisError::PossibleTypo {
                        kind: TypoKind::Variable,
                        ..
                    }
                )
            })
            .count();

        assert_eq!(
            speaker_count, 1,
            "expected 1 speaker typo, got {speaker_count}"
        );
        assert_eq!(label_count, 1, "expected 1 label typo, got {label_count}");
        assert_eq!(var_count, 1, "expected 1 variable typo, got {var_count}");
    }
}
