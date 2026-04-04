# Static Analysis Overview

Urd ships with **20+ static analysis passes** that catch bugs, style issues, and structural problems in your dialogue scripts before they ever reach a player.

## When Analysis Runs

Analysis runs automatically in two contexts:

- **Compilation** — every `quest run` or `quest export` invocation runs all enabled passes before execution begins. Errors halt compilation; warnings are printed but do not block execution.
- **Language Server (urd-lsp)** — the LSP runs analysis on every keystroke (debounced), surfacing diagnostics inline in your editor as you type.

You do not need to invoke analysis manually. It is always on.

## Severity Levels

Diagnostics are split into two severity levels:

| Level | Meaning | Effect |
|-------|---------|--------|
| **Error** | Semantically broken or guaranteed to fail at runtime | Compilation fails |
| **Warning** | Suspicious but not necessarily wrong | Compilation succeeds; diagnostic printed |

See [Error Lints](./error-lints.md) and [Warning Lints](./warning-lints.md) for the full catalogue.

## Opt-in Passes

Most passes run unconditionally. A few are opt-in because they are expensive or context-dependent:

### Loop Detection

Loop detection identifies infinite dialogue loops — label cycles with no escape path to a terminator. Because it requires compiling to IR and running SCC (strongly connected component) analysis on the label-jump graph, it is **opt-in per label**:

```urd
@lint(check_loops)
label danger_zone {
    narrator: "You are trapped."
    jump danger_zone
}
```

Only labels decorated with `@lint(check_loops)` are checked. See [Loop Detection](./loop-detection.md) for details.

### Spellcheck

The spellcheck pass scans dialogue strings for misspelled words and suggests corrections. It is **feature-gated** behind the `spellcheck` Cargo feature because it pulls in a dictionary dependency:

```bash
cargo build --features spellcheck
```

When enabled, misspelling diagnostics appear as warnings in both the CLI and the LSP.

## The AnalysisContext

Before any pass runs, Urd builds an `AnalysisContext` in a single walk over the AST. This context collects:

- **Labels** — every `label name { ... }` defined in the script
- **Enums** — every `enum Name { Variant, ... }` and its variants
- **Structs** — every `struct Name { field: type, ... }` and its fields
- **Top-level variables** — typed `const`, `global`, `let`, and `extern` declarations at the top level
- **Imported symbols** — labels, types, and variables brought in via `import`

All passes share this context by reference. No pass needs to re-walk the entire tree to discover definitions.

## Semantic Suggestions

When a pass detects a possible error involving an unknown name — an undefined label, an undefined variable, or a speaker that doesn't match any known constant — Urd attempts to suggest what you might have meant.

Suggestions are generated using two strategies:

1. **Levenshtein distance** — if a known name is within edit distance ≤ 2 of the unknown identifier, it is offered as a "did you mean?" suggestion.
2. **Synonym lookup** — a built-in synonym table maps common alternate spellings and abbreviations to canonical names (e.g. `colour` → `color`).

These suggestions appear in the diagnostic message:

```text
warning: Undefined label 'taven' — did you mean 'tavern'?
```

## Pass Independence

All passes run to completion independently. An error in one pass does **not** suppress diagnostics from other passes. This means you see the full picture on every compilation attempt, not a drip-feed of one error at a time.

## Pass List

### Errors

| Pass | Module | Description |
|------|--------|-------------|
| Type Mismatch | `types` | Wrong primitive type in a typed declaration |
| Struct Mismatch | `types` | Missing or wrongly-typed struct fields |
| Top-Level Flow | `top_level` | Flow statements outside any label |
| Duplicate Entry | `top_level` | Multiple labels with `@entry` |
| Const Reassignment | `const_reassign` | Assigning to a `const` binding |
| Empty Menu | `menu_structure` | Menu block with zero options |
| Non-Exhaustive Match | `exhaustiveness` | Match missing enum or dice variants |
| Dead End | `dead_end` | Label with no terminator |
| Fluent Decorator | `fluent_decorator` | Invalid `@fluent` usage |
| Id Decorator | `id_decorator` | Invalid `@id` usage |
| Undefined Variable | `undefined_var` | Variable used before declaration |

### Warnings

| Pass | Module | Description |
|------|--------|-------------|
| Unreachable Label | `unreachable_label` | Label never targeted by any jump or `@entry` |
| Empty Dialogue | `empty_dialogue` | Dialogue with empty content |
| Single-Option Menu | `menu_structure` | Menu with exactly one choice |
| Duplicate Menu Dest | `duplicate_menu_dest` | Two menu options with identical bodies |
| Overwritten Assignment | `overwritten_assign` | Value overwritten without being read |
| Unused Variable | `unused_var` | Declared but never used |
| Always-Dead Branch | `dead_branch` | Compile-time constant condition |
| Possible Typo | `possible_typo` | Identifier resembles a known name |
| Undefined Label | `labels` | Jump target doesn't exist (with suggestion) |
| Infinite Loop | `loop_detection` | Opt-in cycle detection |