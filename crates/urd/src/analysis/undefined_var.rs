//! # Undefined Variable Analysis
//!
//! Detects references to variables that have no corresponding declaration
//! visible in scope.
//!
//! ## Algorithm
//!
//! 1. **Global scope**: Collect all names declared at the top level of the
//!    script — `let`, `const`, `global`, `extern`, enum type names, struct type
//!    names, decorator names, and all import aliases.  These names are visible
//!    from every labeled block.
//!
//! 2. **Per-label scope**: For each `LabeledBlock`, collect every name declared
//!    anywhere within that block's subtree (let/const/global/let-call).  These
//!    names supplement the global scope for references inside that label only.
//!
//! 3. **Decorator body scope**: For `decorator` definitions, the declared
//!    parameter names are added to the scope before checking the body.
//!
//! 4. Every single-segment `IdentPath` in a read position (not on the LHS of
//!    an assignment, not a match-arm pattern) is checked against the combined
//!    scope, **including dialogue speaker identifiers**.  Unresolved names
//!    produce [`AnalysisError::UndefinedVariable`].
//!
//! 5. **Semantic fallback**: for undefined references longer than 2 characters
//!    where Levenshtein produces no close match, an optional [`SemanticSuggest`]
//!    backend is consulted.  It embeds the identifier name and all in-scope
//!    names and returns the most similar one above its internal threshold.
//!
//! ## Limitations (known false negatives)
//!
//! - **Use-before-declaration within a label**: a variable used before its
//!   `let` declaration in the same label body is NOT flagged because the pass
//!   collects all label-local names before checking any references.  The runtime
//!   will still fail with [`VmError::UndefinedVariable`].
//! - **Cross-branch scope**: a variable declared inside an `if` branch is
//!   treated as "in scope" for the entire label body, even for sibling branches.
//!
//! A future pass can add statement-order tracking to catch these cases.

use std::collections::HashSet;

use chumsky::span::SimpleSpan;

use crate::analysis::AnalysisError;
use crate::analysis::context::AnalysisContext;
use crate::analysis::possible_typo::{closest_match, collect_var_refs};
use crate::analysis::semantic_suggest::SemanticSuggest;
use crate::parser::ast::{Ast, AstContent, walk_ast};
use crate::runtime::value::RuntimeValue;

/// Built-in call targets that are always valid and require no declaration.
const BUILTINS: &[&str] = &["end!", "todo!"];

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Check for undefined variable references in `ast`.
///
/// Returns one [`AnalysisError::UndefinedVariable`] for every single-segment
/// identifier used in a read position that cannot be resolved to any declaration
/// visible in the current scope.
pub fn check(
    ast: &Ast,
    ctx: &AnalysisContext,
    semantic: Option<&dyn SemanticSuggest>,
) -> Vec<AnalysisError> {
    let global = collect_global_names(ast);

    // Enum variants and type names are valid identifiers in many positions
    // (match patterns, type annotations coerced to values, etc.).
    // Pre-exclude them to avoid false positives.
    let excluded: HashSet<String> = ctx
        .enums
        .keys()
        .chain(ctx.structs.keys())
        .chain(ctx.enums.values().flatten())
        .cloned()
        .collect();

    let mut errors = Vec::new();
    check_node(ast, &global, &excluded, &mut errors, semantic);
    errors
}

// ---------------------------------------------------------------------------
// Recursive node visitor
// ---------------------------------------------------------------------------

fn check_node(
    node: &Ast,
    global: &HashSet<String>,
    excluded: &HashSet<String>,
    errors: &mut Vec<AnalysisError>,
    semantic: Option<&dyn SemanticSuggest>,
) {
    match node.content() {
        // Top-level block: recurse into each statement looking for labeled
        // blocks and decorator definitions.
        AstContent::Block(stmts) => {
            for stmt in stmts {
                check_node(stmt, global, excluded, errors, semantic);
            }
        }

        // Labeled block: build the combined scope for this label and check
        // all variable references within the body.
        AstContent::LabeledBlock { block, .. } => {
            let local = collect_block_local_names(block);
            let scope = build_scope(global, &local, excluded);
            check_refs(block, &scope, errors, semantic);
        }

        // Decorator definition: params are in scope within the body.
        AstContent::DecoratorDef { params, body, .. } => {
            let param_names: HashSet<String> = params.iter().map(|p| p.name.clone()).collect();
            let local = collect_block_local_names(body);
            let mut scope = build_scope(global, &local, excluded);
            scope.extend(param_names);
            check_refs(body, &scope, errors, semantic);
        }

        // Function definition: body is in a completely isolated scope — only the
        // declared parameters are visible.  Global, const, and outer-scope names
        // are intentionally excluded (fn purity).
        AstContent::FnDef { params, body, .. } => {
            let param_names: HashSet<String> = params.iter().map(|p| p.name.clone()).collect();
            // Include local declarations within the fn body (e.g. `let x = …`).
            let fn_locals = collect_block_local_names(body);
            // Build an isolated scope: params + fn-body locals + builtins + type names.
            // Globals, consts, and externs are intentionally excluded.
            let mut fn_scope: HashSet<String> = excluded.clone();
            fn_scope.extend(BUILTINS.iter().map(|s| s.to_string()));
            fn_scope.extend(param_names);
            fn_scope.extend(fn_locals);
            check_refs(body, &fn_scope, errors, semantic);
        }

        // Everything else at the top level is either a definition node
        // (handled by top_level.rs as an error) or a no-op for this pass.
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Reference checker
// ---------------------------------------------------------------------------

/// Collect all single-segment read-position IdentPath references in `node`
/// (including dialogue speaker identifiers) and emit `UndefinedVariable` for
/// any not present in `scope`.
fn check_refs(
    node: &Ast,
    scope: &HashSet<String>,
    errors: &mut Vec<AnalysisError>,
    semantic: Option<&dyn SemanticSuggest>,
) {
    // Pre-build a vec of scope names for closest_match iteration.
    let scope_list: Vec<String> = scope.iter().cloned().collect();

    let mut refs: Vec<(String, SimpleSpan)> = Vec::new();
    // General variable reads (assignments RHS, conditions, expressions, …)
    collect_var_refs(node, false, &mut refs);
    // Dialogue speakers — `collect_var_refs` skips these intentionally for the
    // typo pass, but here they must be checked just like any other identifier.
    collect_speaker_refs(node, &mut refs);

    for (name, span) in refs {
        if BUILTINS.contains(&name.as_str()) {
            continue;
        }
        if !scope.contains(&name) {
            // Only compute a suggestion for names long enough to have a
            // meaningful edit distance (≤ 2 characters → too many false positives).
            let suggestion = if name.len() > 2 {
                // First try Levenshtein (fast, no ML inference).
                let lev = closest_match(&name, scope_list.iter())
                    .and_then(|(dist, s)| if dist <= 2 { Some(s) } else { None });
                // Fall back to semantic similarity when Levenshtein yields nothing.
                lev.or_else(|| semantic.and_then(|m| m.find_synonym(&name, &scope_list)))
            } else {
                None
            };
            errors.push(AnalysisError::UndefinedVariable {
                name,
                suggestion,
                span,
            });
        }
    }
}

/// Walk `node` and append `(name, span)` for every single-segment `IdentPath`
/// that appears in a dialogue `speakers` position.
fn collect_speaker_refs(node: &Ast, refs: &mut Vec<(String, SimpleSpan)>) {
    walk_ast(node, &mut |n| {
        if let AstContent::Dialogue { speakers, .. } = n.content()
            && let AstContent::ExprList(items) = speakers.content()
        {
            for item in items {
                if let AstContent::Value(RuntimeValue::IdentPath(path)) = item.content()
                    && path.len() == 1
                {
                    refs.push((path[0].clone(), item.span()));
                }
            }
        }
    });
}

// ---------------------------------------------------------------------------
// Scope builders
// ---------------------------------------------------------------------------

/// Build the effective scope set for a block: global names + local names,
/// with excluded type/variant names merged in (they're always valid).
fn build_scope(
    global: &HashSet<String>,
    local: &HashSet<String>,
    excluded: &HashSet<String>,
) -> HashSet<String> {
    global
        .iter()
        .chain(local.iter())
        .chain(excluded.iter())
        .cloned()
        .collect()
}

/// Collect all variable names declared anywhere within `block`'s subtree:
/// `let`, `const`, `global`, and `let-call` result names.
///
/// This is intentionally flat (not order-sensitive) — see the module-level
/// doc comment for the known false-negative trade-off.
fn collect_block_local_names(block: &Ast) -> HashSet<String> {
    let mut names = HashSet::new();
    walk_decls(block, &mut names);
    names
}

fn walk_decls(node: &Ast, names: &mut HashSet<String>) {
    match node.content() {
        AstContent::Declaration { decl_name, .. } => {
            if let AstContent::Value(RuntimeValue::IdentPath(path)) = decl_name.content()
                && path.len() == 1
            {
                names.insert(path[0].clone());
            }
        }
        AstContent::LetCall { name, .. } => {
            names.insert(name.clone());
        }
        _ => {
            for child in node.children() {
                walk_decls(child, names);
            }
        }
    }
}

/// Collect all globally-visible names from the top-level block of `ast`.
///
/// Includes: `let`/`const`/`global`/`extern` declarations, enum and struct
/// type names, decorator names, and all import aliases.
fn collect_global_names(ast: &Ast) -> HashSet<String> {
    let stmts: &[Ast] = match ast.content() {
        AstContent::Block(stmts) => stmts,
        AstContent::LabeledBlock { block, .. } => match block.content() {
            AstContent::Block(stmts) => stmts,
            _ => return HashSet::new(),
        },
        _ => return HashSet::new(),
    };

    let mut names = HashSet::new();

    for stmt in stmts {
        match stmt.content() {
            AstContent::Declaration { decl_name, .. } => {
                if let Some(name) = extract_ident(decl_name) {
                    names.insert(name);
                }
            }
            AstContent::ExternDeclaration { name, .. } => {
                if let Some(name) = extract_ident(name) {
                    names.insert(name);
                }
            }
            AstContent::Import { symbols, .. } => {
                for sym in symbols {
                    // Both module aliases (`import "m" as alias`) and
                    // symbol aliases (`import (sym as local) from "m"`)
                    // are bound under their `alias` name.
                    names.insert(sym.alias.clone());
                }
            }
            AstContent::EnumDecl { name, variants } => {
                names.insert(name.clone());
                names.extend(variants.iter().map(|(n, _)| n.clone()));
            }
            AstContent::StructDecl { name, .. } => {
                names.insert(name.clone());
            }
            AstContent::DecoratorDef { name, .. } => {
                names.insert(name.clone());
            }
            AstContent::FnDef {
                name: Some(fn_name),
                ..
            } => {
                names.insert(fn_name.clone());
            }
            _ => {}
        }
    }

    names
}

/// Extract a single-segment identifier name from an AST node.
fn extract_ident(node: &Ast) -> Option<String> {
    if let AstContent::Value(RuntimeValue::IdentPath(path)) = node.content()
        && path.len() == 1
    {
        Some(path[0].clone())
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::context::AnalysisContext;
    use crate::compiler::loader::parse_source;

    fn parse(src: &str) -> Ast {
        parse_source(src).expect("test source should parse")
    }

    fn ctx(ast: &Ast) -> AnalysisContext {
        AnalysisContext::build(ast)
    }

    fn errors(src: &str) -> Vec<AnalysisError> {
        let ast = parse(src);
        let ctx = ctx(&ast);
        check(&ast, &ctx, None)
    }

    fn assert_no_errors(errs: &[AnalysisError]) {
        assert!(errs.is_empty(), "expected no errors, got: {errs:#?}");
    }

    fn assert_undefined(errs: &[AnalysisError], expected_name: &str) {
        assert!(
            errs.iter().any(|e| matches!(
                e,
                AnalysisError::UndefinedVariable { name, .. } if name == expected_name
            )),
            "expected UndefinedVariable for '{expected_name}', got: {errs:#?}"
        );
    }

    // ── No-error cases ────────────────────────────────────────────────────

    #[test]
    fn all_vars_declared_no_errors() {
        assert_no_errors(&errors(
            r#"
let score = 0
label start {
    score = 1
    end!()
}
"#,
        ));
    }

    #[test]
    fn extern_in_scope() {
        assert_no_errors(&errors(
            r#"
extern narrator: str
label start {
    narrator: "Hello"
    end!()
}
"#,
        ));
    }

    #[test]
    fn global_var_in_scope_in_labels() {
        assert_no_errors(&errors(
            r#"
global visits: int = 0
label start {
    visits = visits + 1
    end!()
}
"#,
        ));
    }

    #[test]
    fn const_in_scope_in_labels() {
        assert_no_errors(&errors(
            r#"
const MAX: int = 10
label start {
    if MAX > 0 {
        end!()
    } else {
        end!()
    }
}
"#,
        ));
    }

    #[test]
    fn label_local_let_in_scope() {
        assert_no_errors(&errors(
            r#"
label start {
    let x = 5
    let y = x + 1
    end!()
}
"#,
        ));
    }

    #[test]
    fn let_call_result_in_scope() {
        assert_no_errors(&errors(
            r#"
label helper {
    return 42
}
label start {
    let result = jump helper and return
    let doubled = result + result
    end!()
}
"#,
        ));
    }

    #[test]
    fn enum_variants_not_flagged() {
        assert_no_errors(&errors(
            r#"
enum Direction { North, South, East, West }
label start {
    match North {
        North { end!() }
        _ { end!() }
    }
}
"#,
        ));
    }

    #[test]
    fn struct_type_name_not_flagged_in_annotations() {
        // Struct names appear in type annotations, which are not read-position
        // IdentPaths — they're parsed differently. Just confirm no error.
        assert_no_errors(&errors(
            r#"
struct Point { x: int, y: int }
extern origin: Point
label start {
    end!()
}
"#,
        ));
    }

    #[test]
    fn end_and_todo_builtins_not_flagged() {
        assert_no_errors(&errors(
            r#"
label start {
    end!()
}
label other {
    todo!()
}
"#,
        ));
    }

    #[test]
    fn decorator_params_in_scope_inside_body() {
        // Decorator params must be available inside the body.
        assert_no_errors(&errors(
            r#"
decorator greet(speaker) {
    speaker: "Hello"
    end!()
}
label start {
    end!()
}
"#,
        ));
    }

    // ── Error cases ───────────────────────────────────────────────────────

    #[test]
    fn completely_undeclared_var_is_flagged() {
        let errs = errors(
            r#"
label start {
    if ghost_var {
        end!()
    } else {
        end!()
    }
}
"#,
        );
        assert_undefined(&errs, "ghost_var");
    }

    #[test]
    fn label_local_var_not_visible_in_sibling_label() {
        // `local_x` is declared in `label start` but NOT in `label other`.
        // It should be flagged when referenced inside `label other`.
        let errs = errors(
            r#"
label start {
    let local_x = 5
    end!()
}
label other {
    if local_x {
        end!()
    } else {
        end!()
    }
}
"#,
        );
        assert_undefined(&errs, "local_x");
    }

    #[test]
    fn multiple_undeclared_vars_all_flagged() {
        let errs = errors(
            r#"
label start {
    let z = alpha + beta
    end!()
}
"#,
        );
        assert_undefined(&errs, "alpha");
        assert_undefined(&errs, "beta");
    }

    #[test]
    fn declared_var_in_rhs_of_own_decl_not_flagged_if_top_level() {
        // top-level `let x = x + 1` — `x` IS collected as a global name,
        // so it won't be flagged. (This is intentional: we don't track
        // top-level ordering — that's a runtime concern.)
        // The test just confirms no panic and reasonable behaviour.
        let _errs = errors(
            r#"
let x = 0
label start {
    end!()
}
"#,
        );
    }

    #[test]
    fn dialogue_speaker_undeclared_is_flagged() {
        // `player` is used as a speaker but never declared anywhere.
        let errs = errors(
            r#"
label start {
    player: "Hello"
    end!()
}
"#,
        );
        assert_undefined(&errs, "player");
    }

    #[test]
    fn dialogue_speaker_declared_as_extern_not_flagged() {
        assert_no_errors(&errors(
            r#"
extern player: str
label start {
    player: "Hello"
    end!()
}
"#,
        ));
    }

    #[test]
    fn dialogue_speaker_declared_as_const_not_flagged() {
        assert_no_errors(&errors(
            r#"
struct Character { name: str }
const narrator: Character = :{ name: "Narrator" }
label start {
    narrator: "Hello"
    end!()
}
"#,
        ));
    }

    #[test]
    fn dialogue_speaker_error_has_correct_span() {
        // Regression: speaker IdentPath nodes previously had zero spans (0..0),
        // causing the diagnostic to appear at the very start of the file.
        // The parser now applies with_span to each speaker node.
        let src = "label start {\n    player: \"Hello\"\n    end!()\n}\n";
        let ast = parse(src);
        let ctx = ctx(&ast);
        let errs = check(&ast, &ctx, None);
        let err = errs
            .iter()
            .find(
                |e| matches!(e, AnalysisError::UndefinedVariable { name, .. } if name == "player"),
            )
            .expect("expected UndefinedVariable for 'player'");
        if let AnalysisError::UndefinedVariable { span, .. } = err {
            assert!(
                span.start > 0,
                "span should point into the file, not the start (got 0..{})",
                span.end
            );
        }
    }

    #[test]
    fn undefined_var_with_close_match_suggests_correction() {
        // "visted_cave" is one edit away from "visited_cave" — should suggest it.
        let errs = errors(
            r#"
let visited_cave: bool = false
label check {
    if visted_cave {
        end!()
    } else {
        end!()
    }
}
"#,
        );
        let err = errs
            .iter()
            .find(|e| {
                matches!(e, AnalysisError::UndefinedVariable { name, .. } if name == "visted_cave")
            })
            .expect("expected UndefinedVariable for 'visted_cave'");
        if let AnalysisError::UndefinedVariable { suggestion, .. } = err {
            assert_eq!(
                suggestion.as_deref(),
                Some("visited_cave"),
                "expected suggestion 'visited_cave', got: {suggestion:?}"
            );
        }
    }

    #[test]
    fn undefined_var_no_close_match_has_no_suggestion() {
        let errs = errors(
            r#"
let health: int = 100
label check {
    if completelydifferent {
        end!()
    } else {
        end!()
    }
}
"#,
        );
        let err = errs
            .iter()
            .find(|e| {
                matches!(e, AnalysisError::UndefinedVariable { name, .. } if name == "completelydifferent")
            })
            .expect("expected UndefinedVariable for 'completelydifferent'");
        if let AnalysisError::UndefinedVariable { suggestion, .. } = err {
            assert!(
                suggestion.is_none(),
                "expected no suggestion for completely different name, got: {suggestion:?}"
            );
        }
    }

    #[test]
    fn undefined_short_name_has_no_suggestion() {
        // Short names (≤ 2 chars) never get suggestions regardless of scope contents.
        let errs = errors(
            r#"
let visited_cave: bool = false
label check {
    if vc {
        end!()
    } else {
        end!()
    }
}
"#,
        );
        // 'vc' IS undefined so an error is still emitted, but with no suggestion.
        let err = errs
            .iter()
            .find(|e| matches!(e, AnalysisError::UndefinedVariable { name, .. } if name == "vc"))
            .expect("expected UndefinedVariable for 'vc'");
        if let AnalysisError::UndefinedVariable { suggestion, .. } = err {
            assert!(
                suggestion.is_none(),
                "expected no suggestion for short name 'vc', got: {suggestion:?}"
            );
        }
    }

    #[test]
    fn undefined_speaker_with_close_match_suggests_correction() {
        // "narratr" as a speaker is one deletion away from "narrator".
        let errs = errors(
            r#"
const narrator: str = "Narrator"
label start {
    narratr: "Hello"
    end!()
}
"#,
        );
        let err = errs
            .iter()
            .find(
                |e| matches!(e, AnalysisError::UndefinedVariable { name, .. } if name == "narratr"),
            )
            .expect("expected UndefinedVariable for 'narratr'");
        if let AnalysisError::UndefinedVariable { suggestion, .. } = err {
            assert_eq!(
                suggestion.as_deref(),
                Some("narrator"),
                "expected suggestion 'narrator', got: {suggestion:?}"
            );
        }
    }

    #[test]
    fn map_literal_keys_not_flagged_as_undefined() {
        // `:{ name: "Narrator", name_color: "#aaa" }` — `name` and `name_color`
        // are struct-field-style keys, not variable reads. Must not produce errors.
        assert_no_errors(&errors(
            r#"
struct Character { name: str, name_color: str }
label start {
    let narrator = :{ name: "Narrator", name_color: "aaaaaa" }
    end!()
}
"#,
        ));
    }

    #[test]
    fn map_literal_values_that_are_vars_are_still_checked() {
        // The *values* in a map literal ARE variable reads and should still be
        // flagged if undefined.
        let errs = errors(
            r#"
label start {
    let x = :{ key: ghost_value }
    end!()
}
"#,
        );
        assert_undefined(&errs, "ghost_value");
    }

    #[test]
    fn no_false_positive_for_conditional_ref_to_top_level_var() {
        assert_no_errors(&errors(
            r#"
let visited = false
label start {
    if visited {
        end!()
    } else {
        visited = true
        end!()
    }
}
"#,
        ));
    }

    #[test]
    fn fn_body_cannot_see_global_variable() {
        // A global declared at top-level must NOT be visible inside a fn body.
        let errs = errors(
            r#"
global score: int = 100
fn get_score() -> int {
    return score
}
label start {
    end!()
}
"#,
        );
        assert_undefined(&errs, "score");
    }

    #[test]
    fn fn_params_are_in_scope_inside_body() {
        // Parameters must be visible inside the fn body.
        assert_no_errors(&errors(
            r#"
fn add(a: int, b: int) -> int {
    return a + b
}
label start {
    end!()
}
"#,
        ));
    }

    #[test]
    fn fn_body_let_inside_if_is_in_scope() {
        // `let` declarations inside `if` branches within a fn body must not
        // produce false-positive UndefinedVariable errors — walk_decls recurses
        // into all children including if/else blocks.
        assert_no_errors(&errors(
            r#"
fn example() -> int {
    if true {
        let x: int = 42
        return x
    }
    return 0
}
label start {
    end!()
}
"#,
        ));
    }

    #[test]
    fn fn_body_cannot_see_const() {
        // `const` declarations at the top level are also invisible inside fn bodies.
        let errs = errors(
            r#"
const MAX: int = 100
fn is_maxed(val: int) -> bool {
    return val == MAX
}
label start {
    end!()
}
"#,
        );
        assert_undefined(&errs, "MAX");
    }

    #[test]
    fn top_level_fn_name_visible_in_labels() {
        // Named functions defined at the top level are stored as
        // `RuntimeValue::Function` in the environment by the compiler
        // (`IrNodeKind::DefineFunction`).  They must be treated as
        // valid identifiers inside label scopes — e.g. when passed
        // by name to higher-order list methods like `.map(double)`.
        assert_no_errors(&errors(
            r#"
fn double(x) -> int {
    return x * 2
}
fn sum_two(a, b) -> int {
    return a + b
}
@entry
label start {
    let xs = [1, 2, 3]
    let doubled = xs.map(double)
    let total = xs.reduce(0, sum_two)
    end!()
}
"#,
        ));
    }

    #[test]
    fn top_level_fn_name_visible_across_multiple_labels() {
        // The function name should be in scope for every label, not just the first.
        assert_no_errors(&errors(
            r#"
fn greet(name) -> str {
    return "hello"
}
@entry
label a {
    let x = greet("world")
    jump b
}
label b {
    let y = greet("urd")
    end!()
}
"#,
        ));
    }

    #[test]
    fn anonymous_fn_not_in_global_scope() {
        // Only *named* FnDef nodes register a global name.
        // An anonymous function in statement position should not
        // magically create a binding visible elsewhere.
        let errs = errors(
            r#"
fn(x) { return x }
@entry
label start {
    let y = mystery
    end!()
}
"#,
        );
        assert_undefined(&errs, "mystery");
    }
}
