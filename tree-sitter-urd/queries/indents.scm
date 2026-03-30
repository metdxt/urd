; indents.scm — Indentation queries for Urd
; Targets tree-sitter's native indent query system (Helix, Zed, nvim-treesitter).
;
; Capture names:
;   @indent     — the contents of this node should be indented one level
;   @dedent     — this node / token reduces the indent level
;   @branch     — used for elif/else: dedent from previous block, then re-indent

; ── Generic block ─────────────────────────────────────────────────────────────

; Any curly-brace block indents its contents.
(block) @indent

; ── Label  `label name { ... }` ───────────────────────────────────────────────

(label_statement
  body: (block) @indent)

; ── If / elif / else ──────────────────────────────────────────────────────────

(if_statement
  consequence: (block) @indent)

; elif and else are branches — they dedent from the previous block and
; then start a new indented region.
(elif_clause) @branch
(elif_clause
  body: (block) @indent)

(else_clause) @branch
(else_clause
  body: (block) @indent)

; ── Match statement  `match expr { arm { } ... }` ────────────────────────────

(match_statement
  (match_arm
    body: (block) @indent))

; ── Menu  `menu { "opt" { } }` ────────────────────────────────────────────────

(menu_statement
  (menu_option
    body: (block) @indent))

; ── Dialogue block  `<speaker>: { "line" ... }` ──────────────────────────────

(dialogue_block) @indent

; ── Enum  `enum Foo { A, B }` ────────────────────────────────────────────────

(enum_declaration) @indent

; ── Struct  `struct Foo { field: type }` ──────────────────────────────────────

(struct_declaration) @indent

; ── Decorator definition  `decorator name(...) { }` ──────────────────────────

(decorator_definition
  body: (block) @indent)

; ── List literal  `[ a, b, c ]` ──────────────────────────────────────────────

(list_expr) @indent

; ── Map literal  `:{ key: val }` ─────────────────────────────────────────────

(map_expr) @indent

; ── Call argument list  `func(a, b)` ──────────────────────────────────────────

(call_expr) @indent

; ── Parenthesised expression  `(expr)` ────────────────────────────────────────

(paren_expr) @indent

; ── Decorator application argument list  `@name(args)` ───────────────────────

(decorator) @indent

; ── Closing delimiters always dedent ──────────────────────────────────────────

[
  "}"
  "]"
  ")"
] @dedent
