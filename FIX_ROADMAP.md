## Urd Fix Roadmap

### Critical Path

The two longest chains are:

```/dev/null/critical-path.txt#L1-4
Phase 0 → Phase 1 (VM-4) → Phase 4/C (VM-5)
Phase 0 → Phase 3 (CMP-1) → Phase 3 (ANA-5)
                           → Phase 5/B (LSP-6, LSP-7)
```

Everything else is off the critical path and can be scheduled freely within its phase window.

---

### (Complete) Phase 0 — Hygiene
**No functional changes. Purpose: clear the lint noise and doc rot so every subsequent PR is on a clean baseline.**

All items are fully independent and can be parallelised.

| ID | File | Fix |
|----|------|-----|
| MIN-1 | `vm/eval.rs` L109 | Remove stale "Collection literals — not yet supported" comment above the fully-implemented `List` arm |
| MIN-2 | `lexer/strings.rs` L175 | Add a `// INVARIANT:` comment linking `unreachable!()` to the regex it depends on |
| MIN-3 | `parser/errors.rs` L157 | Replace bare `#[allow(clippy::expect_used)]` with an `// INVARIANT:` comment explaining the slice is non-empty |
| MIN-5 | `urd-quest/main.rs` L1 | Remove `#![allow(missing_docs)]`; add doc comments to all public items |
| PAR-3 | `parser/ast.rs` L285–289 | Delete the stale "extern const or extern global" sentence from `ExternDeclaration` doc |
| PAR-4 | `parser/mod.rs` L18 | Gate `pub mod test_util` and `parse_test!` behind `#[cfg(test)]` |
| ANA-2 | `analysis/mod.rs` L10 vs L893 | Reconcile: `top_level` is a warning; fix either the module doc or `analyze()`'s docstring to agree |
| ANA-3 | `analysis/possible_typo.rs` L40 | Remove the dead `_ctx: &AnalysisContext` parameter from `check()` and all call sites |
| ANA-4 | `analysis/loop_detection.rs` L528–530 | Remove `let _ = clusters` placeholder; either make it a named constant or delete it with a `// TODO` noting the planned use |
| LSP-9 | `semantic.rs` L248 | Delete `ident_path_from_ast` (never called) and its `#[allow(dead_code)]` |
| LSP-10 | `workspace.rs` L41, L279 | Remove `_path` field and `_modules_for` method, or rename and wire them in |
| LSP-11 | `user_dict.rs` L99, L153 | Remove `#[allow(dead_code)]` from `contains` and `path`; gate by `#[cfg(test)]` or remove if they are test-only |

**Exit criterion:** `cargo clippy -- -D warnings` green on all touched crates. `cargo test` passes. No `#[allow(dead_code)]` suppressor remaining on production code without an explanatory comment.

---

### (Completed) Phase 1 — Error Propagation & Diagnostic Correctness
**Depends on: Phase 0**

These two are independent of each other but must both land before Phase 4/Group C.

| ID | File | Fix |
|----|------|-----|
| ANA-1 | `analysis/mod.rs` L409–424 | Add `UndefinedLabel` to the `is_warning()` match; add a regression test asserting it returns `true` |
| VM-4 | `vm/mod.rs` L881–886 | Propagate `eval_expr` errors out of `exec_stmt_sync` instead of `log::warn` + discard; update all call sites to handle `Err` |

**Exit criterion:**
- `UndefinedLabel` diagnostics appear as warnings in both the CLI and the LSP — not as hard errors.
- A VM integration test verifies that a failing expression inside a decorator body propagates `Err(VmError::…)` to the caller rather than silently succeeding.

---

### (Completed) Phase 2 — Parser Foundations
**Depends on: Phase 0**

Both items are independent and can land in parallel. VM-8 (HOF list methods) is technically workable without anonymous functions, but every real-world HOF invocation requires them.

| ID | File | Fix |
|----|------|-----|
| PAR-1 | `parser/block.rs` L410–427, `parser/ast.rs` L420–430 | Add an anonymous-function expression parser path producing `FnDef { name: None, … }`; accept `fn(params) { body }` in expression position (e.g. as a function argument) |
| PAR-2 + MIN-4 | `parser/block.rs` L57–78 | Either implement 3+-segment label path parsing, or emit an explicit, actionable error for `a.b.c` naming the 2-segment limit; document the constraint in the public API |

**Exit criterion:**
- `fn(x: int) -> int { x + 1 }` in expression position parses and evaluates correctly.
- `list.map(fn(x) { x * 2 })` works end-to-end in a script.
- 3+-segment label paths either parse or produce a clear diagnostic — no silent failure.
- Parser unit tests cover anonymous function literals and the path-length boundary.

---

### (Completed) Phase 3 — Struct Runtime Representation & IR Rendering
**Depends on: Phase 0**

`CMP-1` and `IR-1` are independent and can be done in parallel. `ANA-5` gates on `CMP-1`.

| ID | Files | Fix | Order |
|----|-------|-----|-------|
| CMP-1 | `runtime/value.rs`, `compiler/mod.rs`, `vm/eval.rs`, `parser/ast.rs` | Add `RuntimeValue::Struct { name, fields: HashMap<String, RuntimeValue> }`; compile `StructDecl` to a `DefineStruct` IR node (not `Nop`); implement field read/write in eval; **add `span: SimpleSpan` to `StructField` and `(String, SimpleSpan)` pairs to `EnumDecl` variants** in the AST | First |
| IR-1 | `ir/sequence.rs` L446–456 | Fix `Switch` node rendering to include the actual scrutinee expression and per-arm labels instead of the static `"match ⟨expr⟩"` string | Parallel with CMP-1 |
| ANA-5 | `analysis/types.rs` L418–421 | Implement `unresolved_qualified_identpath_compatible`: accept a qualified identifier when it resolves to a declared struct/enum name in `AnalysisContext`; reject otherwise | After CMP-1 |

**Exit criterion:**
- A script declaring `struct Point { x: int, y: int }`, constructing a `Point`, and reading `point.x` evaluates correctly.
- `match` expressions render their scrutinee and arm labels in sequence diagrams.
- `unresolved_qualified_identpath_compatible` passes `chars.Character` when `Character` is declared in the `chars` module; fails for unknown names.
- `StructField` and enum variants carry `SimpleSpan`; all existing tests still pass.

---

### Phase 4 — VM Runtime Completeness
**Depends on: Phase 1 for VM-5; Phase 2 for VM-8. Groups A and B are independent of those and can start as soon as Phase 0 is done.**

#### Group A — Dispatch (one PR, same function in `eval.rs`)

| ID | File | Fix |
|----|------|-----|
| VM-1 | `vm/eval.rs` L78–82 | Implement method dispatch for `Map`, `Str`, `Int`, `Float`; return `VmError::UnknownMethod` for missing methods — never return silent `Null` |
| VM-2 | `vm/eval.rs` L98–104 | Return `VmError::UndefinedFunction` for unresolved free-function calls; check the built-in registry before falling through |

#### Group B — Independent VM fixes (parallel with Group A)

| ID | File | Fix |
|----|------|-----|
| VM-3 | `vm/eval.rs` L352–356 | Add a `DiceRoller` trait; store an instance in `Environment` or `DecoratorRegistry`; provide a default `rand`-based implementation; the error path remains only when no roller is registered |
| VM-6 | `vm/registry.rs` L68–93 | Store `(event_constraint, params, body)` in `define_script_decorator`; execute the body when the handler is invoked instead of returning an empty map |
| VM-7 | `runtime/value.rs` L97–101 | Add a `debug_assert!` (or `#[cfg(debug_assertions)]` guard) that `List` elements are never `ScriptDecorator` at construction time; document the invariant in the type's doc comment |

#### Group C — Gated on Phase 1 + Phase 2

| ID | File | Fix | Gate |
|----|------|-----|------|
| VM-5 | `vm/mod.rs` L904–908 | Replace the three empty constraint match arms with actual enforcement: return `VmError::ConstraintViolation` when a `Choice`-constrained decorator is applied to a `Dialogue` node | Phase 1 (VM-4 must propagate errors) |
| VM-8 | `vm/list_methods.rs` | Implement `filter`, `reduce`/`fold`, `find`, `any`, `all`, `sort_by`, `zip`; remove the `// ── Stubs` header; unit-test each | Phase 2 (PAR-1 anonymous functions) |

**Exit criterion:**
- Method calls on `Str`, `Int`, `Float`, `Map` return typed errors, never `Null`.
- Free-function calls to unknown names produce `VmError::UndefinedFunction`.
- Dice expressions evaluate using the pluggable roller; the VM default roller produces a valid integer.
- `@choice_only_decorator` on a `Dialogue` event returns `VmError::ConstraintViolation`.
- `define_script_decorator` with a body actually executes that body.
- All seven new HOFs work with both named and anonymous function arguments.
- `List` with a `ScriptDecorator` element panics (debug) or is caught at construction.
- Full unit test coverage for every new dispatch branch and HOF.

---

### Phase 5 — LSP Semantic Token Correctness
**Depends on: Phase 3 for LSP-6/7 (span-annotated fields/variants). Groups A and C are independent of Phase 3 and can start immediately after Phase 0.**

#### Group A — Sub-token identifier emission (one PR; all in `semantic.rs`)

| ID | Location | Fix |
|----|----------|-----|
| LSP-3 | L2008–2018 | Destructure `FnDef { name, … }`; emit `Function` token for the name span, `Keyword` only for `fn` |
| LSP-4 | L2043–2052 | Destructure `LetCall { name, target }`; emit `Variable` for name, `Label` for target |
| LSP-5 | L1882–1899 | Destructure `Jump { label }`; emit `Keyword` for the `jump` span only, `Label` for the label name span |
| LSP-8 | L1852–1878 | Replace `span.start + kw_len` arithmetic in `LabeledBlock` with the actual AST token span of the label name |

#### Group B — Declaration body tokens (parallel with Group A; LSP-6/7 gate on Phase 3)

| ID | Location | Fix | Gate |
|----|----------|-----|------|
| LSP-1 | L1777–1785 | Compute decorator byte span (offset from parent node by `@` + name length); emit `Decorator` tokens; remove `#[allow(dead_code)]` from `SemanticTokenType::Decorator` | Phase 0 only |
| LSP-6 | L1925–1945 | Use Phase 3 `(name, span)` variant pairs from `EnumDecl`; emit one `EnumMember` token per variant | Phase 3 (CMP-1) |
| LSP-7 | L1947–1963 | Use Phase 3 `StructField { name, span }` fields; emit one `Property` token per field name | Phase 3 (CMP-1) |

#### Group C — Go-to-definition correctness

| ID | Location | Fix |
|----|----------|-----|
| LSP-2 | L800–809 | Replace `Some(SimpleSpan::new((), 0..0))` in `find_definition_in_children` with `None`; callers return "not found" rather than navigating to byte 0 |

**Exit criterion:**
- Function names, variable names, label names, and jump targets are individually typed in the semantic token stream.
- `@decorator` annotations produce `Decorator`-typed tokens.
- Each enum variant and struct field name has its own token span.
- Go-to-definition either navigates to the correct definition or returns "not found" — it **never** navigates to the top of the file.
- `SemanticTokenType::Decorator` no longer needs `#[allow(dead_code)]`.
- LSP integration tests cover all newly emitted token types.

---

### Summary

| Phase | Issues | Parallelism | Hard dependency |
|-------|--------|-------------|-----------------|
| **0 — Hygiene** | MIN-1/2/3/5, PAR-3/4, ANA-2/3/4, LSP-9/10/11 | All parallel | None |
| **1 — Error propagation** | ANA-1, VM-4 | Both parallel | Phase 0 |
| **2 — Parser foundations** | PAR-1, PAR-2+MIN-4 | Both parallel | Phase 0 |
| **3 — Struct/IR** | CMP-1, IR-1, ANA-5 | IR-1 ‖ CMP-1; ANA-5 after CMP-1 | Phase 0 |
| **4 — VM completeness** | VM-1~2, VM-3, VM-5, VM-6, VM-7, VM-8 | Groups A ‖ B; Group C gated | VM-5 ← Phase 1; VM-8 ← Phase 2 |
| **5 — LSP token correctness** | LSP-1/2/3/4/5/6/7/8 | Groups A ‖ B ‖ C; LSP-6/7 gated | LSP-6/7 ← Phase 3 |

Phases 1, 2, and 3 can all be worked simultaneously once Phase 0 is done — they don't touch each other's write sets. Phase 4/Group C and Phase 5/Group B are the last pieces to close because they depend on the foundations built in phases 1–3.
