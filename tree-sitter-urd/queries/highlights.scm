; highlights.scm — Syntax highlighting for Urd
; https://tree-sitter.github.io/tree-sitter/syntax-highlighting

; ── Comments ──────────────────────────────────────────────────────────────────

(line_comment) @comment.line

; ── Keywords ──────────────────────────────────────────────────────────────────

; Declaration kinds
"extern" @keyword.storage.modifier
"const"  @keyword.storage.modifier
"let"    @keyword.storage.modifier
"global" @keyword.storage.modifier

; Control flow
"if"     @keyword.control.conditional
"elif"   @keyword.control.conditional
"else"   @keyword.control.conditional
"match"  @keyword.control.conditional
"return" @keyword.control.return
"jump"   @keyword.control.jump

; Subroutine call modifier
"and"    @keyword.control

; Dialogue / narrative
"label"  @keyword.other
"menu"   @keyword.other

; Type definitions
"enum"      @keyword.storage.type
"struct"    @keyword.storage.type
"decorator" @keyword.storage.type

; Import
"import" @keyword.control.import
"as"     @keyword.control.import
"from"   @keyword.control.import

; ── Terminators ───────────────────────────────────────────────────────────────

(end_bang)  @keyword.control.return
(todo_bang) @keyword.control.return

; ── Operators ─────────────────────────────────────────────────────────────────

; Logical keyword operators — highlight as keyword.operator
(binary_expr
  operator: (binary_operator) @keyword.operator
  (#match? @keyword.operator "^(and|&&|or|\\|\\|)$"))

(unary_expr
  operator: (unary_operator) @keyword.operator
  (#match? @keyword.operator "^not$"))

; Arithmetic / bitwise / comparison — highlight as plain operator
(binary_expr
  operator: (binary_operator) @operator)

(unary_expr
  operator: (unary_operator) @operator)

; Assignment `=`
"=" @operator

; ── Literals ──────────────────────────────────────────────────────────────────

(integer) @constant.numeric.integer
(float)   @constant.numeric.float
(boolean) @constant.builtin.boolean
(null)    @constant.builtin
(dice)    @constant.numeric

; ── Strings ───────────────────────────────────────────────────────────────────

(string)                  @string
(string_literal_fragment) @string
(escape_sequence)         @string.escape

; String interpolation  {var}  or  {var:format}
(interpolation)           @string.special
(interpolation_path)      @variable.other.member
(format_spec)             @string.special.format

; ── Types ─────────────────────────────────────────────────────────────────────

; Built-in primitive type keywords
(type_name "int")   @type.builtin
(type_name "float") @type.builtin
(type_name "bool")  @type.builtin
(type_name "str")   @type.builtin
(type_name "null")  @type.builtin
(type_name "list")  @type.builtin
(type_name "map")   @type.builtin
(type_name "dice")  @type.builtin
(type_name "label") @type.builtin

; User-defined named types
(type_name (identifier_path)) @type

; Type annotation colon
(type_annotation ":") @punctuation.delimiter

; ── Declarations ──────────────────────────────────────────────────────────────

; `const x = ...` — constant binding
(declaration
  "const"
  name: (identifier_path) @constant)

; `let x = ...` / `global x = ...` — mutable binding
(declaration
  name: (identifier_path) @variable.declaration)

; `extern const x` — external constant binding (no initialiser)
(extern_declaration
  kind: "const"
  name: (identifier_path) @constant)

; `extern global x` — external mutable binding (no initialiser)
(extern_declaration
  kind: "global"
  name: (identifier_path) @variable.declaration)

; ── Labels ────────────────────────────────────────────────────────────────────

; label name { ... }  — the label name is like a function/section name
(label_statement
  name: (identifier) @label)

; jump target
(jump_statement
  label: (identifier_path) @label)

; let x = jump target and return
(let_call_statement
  target: (identifier_path) @label)
(let_call_statement
  name: (identifier) @variable.declaration)

; ── Dialogue ──────────────────────────────────────────────────────────────────

; <speaker>: ...  — the speaker expression
(dialogue_statement
  speaker: (_) @variable.other.member)

; menu option label strings
(menu_option
  label: (string) @string.special)

; ── Enum & Struct ─────────────────────────────────────────────────────────────

(enum_declaration
  name: (identifier) @type)

(enum_declaration
  variant: (identifier) @constant)

(struct_declaration
  name: (identifier) @type)

(struct_field
  name: (identifier) @variable.other.member)

; ── Decorators ────────────────────────────────────────────────────────────────

"@" @punctuation.special

(decorator
  name: (decorator_name) @attribute)

(decorator_definition
  name: (identifier) @attribute)

; event constraint  <event: dialogue>
(event_constraint "event"    @variable.builtin)
(event_constraint "dialogue" @constant.builtin)
(event_constraint "choice"   @constant.builtin)

(decorator_param
  name: (identifier) @variable.parameter)

; ── Imports ───────────────────────────────────────────────────────────────────

(import_statement
  path:  (string)     @string.special.path)
(import_statement
  alias: (identifier) @namespace)

(import_symbol
  name:  (identifier) @variable.other.member)
(import_symbol
  alias: (identifier) @namespace)

; ── Function / Call ───────────────────────────────────────────────────────────

(call_expr
  function: (identifier_path) @function)

; ── Variables & Paths ─────────────────────────────────────────────────────────

; Generic identifier paths not matched by a more specific rule above
(identifier_path) @variable

; Bare identifiers
(identifier) @variable

; Wildcard `_` in match patterns
(wildcard) @variable.builtin

; ── Punctuation ───────────────────────────────────────────────────────────────

["(" ")" "[" "]" "{" "}"] @punctuation.bracket

; dict start  :{
":{" @punctuation.bracket

["," ";" ":"] @punctuation.delimiter

; event constraint angle brackets
(event_constraint "<" @punctuation.bracket)
(event_constraint ">" @punctuation.bracket)
