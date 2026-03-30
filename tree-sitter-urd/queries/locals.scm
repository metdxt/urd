; locals.scm — Scope, definition, and reference queries for Urd
; Used by editors (e.g. Helix, Neovim via nvim-treesitter) to track
; variable lifetimes, highlight definitions vs. uses, and navigate symbols.

; ── Scopes ────────────────────────────────────────────────────────────────────

; The entire source file is the outermost scope.
(source_file) @local.scope

; Every curly-brace block opens a new inner scope.
(block) @local.scope

; label bodies are top-level named scopes (like functions).
(label_statement
  body: (block) @local.scope)

; if/elif/else branches each have their own scope.
(if_statement
  consequence: (block) @local.scope)

(elif_clause
  body: (block) @local.scope)

(else_clause
  body: (block) @local.scope)

; match arms are individual scopes.
(match_arm
  body: (block) @local.scope)

; menu option bodies are individual scopes.
(menu_option
  body: (block) @local.scope)

; decorator definition body.
(decorator_definition
  body: (block) @local.scope)

; ── Definitions ───────────────────────────────────────────────────────────────

; `let name = ...`  — mutable variable, scoped to the enclosing block.
(declaration
  kind: "let"
  name: (identifier_path
    . (identifier) @local.definition))

; `const name = ...`  — immutable binding, scoped to the enclosing block.
(declaration
  kind: "const"
  name: (identifier_path
    . (identifier) @local.definition))

; `global name = ...`  — global variable, visible everywhere.
; We still record it as a definition so references can be resolved.
(declaration
  kind: "global"
  name: (identifier_path
    . (identifier) @local.definition))

; `let name = jump target and return`  — subroutine-call result binding.
(let_call_statement
  name: (identifier) @local.definition)

; label declarations — each label name is a definition in the file scope.
(label_statement
  name: (identifier) @local.definition)

; enum type name.
(enum_declaration
  name: (identifier) @local.definition)

; enum variants — defined inside the enum's scope.
(enum_declaration
  variant: (identifier) @local.definition)

; struct type name.
(struct_declaration
  name: (identifier) @local.definition)

; struct field names — defined inside the struct's scope.
(struct_field
  name: (identifier) @local.definition)

; decorator definition name.
(decorator_definition
  name: (identifier) @local.definition)

; decorator parameter names — scoped to the decorator body.
(decorator_param
  name: (identifier) @local.definition)

; import alias — `import "..." as alias` binds `alias` in the file scope.
(import_statement
  alias: (identifier) @local.definition)

; ── References ────────────────────────────────────────────────────────────────

; Any free-standing identifier path that is not in a definition position
; is a reference to a previously defined name.
(identifier_path
  (identifier) @local.reference)

; jump targets are references to label definitions.
(jump_statement
  label: (identifier_path
    (identifier) @local.reference))

(let_call_statement
  target: (identifier_path
    (identifier) @local.reference))

; Dialogue speaker expressions (typically variable paths).
(dialogue_statement
  speaker: (identifier_path
    (identifier) @local.reference))

; call_expr function path.
(call_expr
  function: (identifier_path
    (identifier) @local.reference))

; subscript object.
(subscript_expr
  object: (identifier_path
    (identifier) @local.reference))

; subscript assignment object.
(subscript_assignment
  object: (identifier_path
    (identifier) @local.reference))

; assignment target (mutating a previously declared variable).
(assignment
  target: (identifier_path
    (identifier) @local.reference))

; match scrutinee.
(match_statement
  scrutinee: (identifier_path
    (identifier) @local.reference))

; match arm patterns that are identifier paths (enum variants, labels, etc.).
(match_arm
  pattern: (identifier_path
    (identifier) @local.reference))

; String interpolation paths  {variable.path}  reference variables.
(interpolation
  (interpolation_path) @local.reference)
