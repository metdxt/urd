# tree-sitter-urd

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar for
**Urd** — an in-game dialogue scripting language designed for defining
dialogue trees, conditional logic, and interactive conversations in games.

## Language overview

Urd scripts are newline-significant files (`.urd`) that combine:

- Typed variable declarations (`const`, `let`, `global`)
- Arithmetic, bitwise, logical, and comparison expressions
- String interpolation (`"Hull at {hull_integrity}%"`)
- Dice-roll literals (`2d6`, `1D20`) for RPG mechanics
- `label` / `jump` for non-linear control flow
- `menu` blocks for player-choice branches
- `<speaker>: "..."` dialogue lines
- `if` / `elif` / `else` and `match` for branching logic
- `enum` and `struct` type definitions
- `decorator` definitions and `@decorator` application
- `import "path" as alias` for multi-file scripts

## Repository layout

```
tree-sitter-urd/
├── grammar.js            ← grammar definition (edit this)
├── tree-sitter.json      ← language metadata for the CLI
├── package.json          ← npm package manifest
├── queries/
│   ├── highlights.scm    ← syntax highlighting
│   ├── locals.scm        ← scope / definition / reference tracking
│   └── indents.scm       ← auto-indentation rules
└── test/
    └── corpus/
        ├── literals.txt  ← corpus tests for every literal form
        ├── expressions.txt
        └── statements.txt
```

## Building

You need **Node.js ≥ 18** and the `tree-sitter-cli`.

```sh
# Install dependencies
npm install

# Generate the parser (creates src/parser.c from grammar.js)
npm run generate

# Compile the native Node.js binding
npm run build
```

The generated `src/parser.c` is the portable artefact — commit it so
downstream consumers (editors, other languages) can build without Node.js.

## Testing

Corpus tests live in `test/corpus/*.txt`.  Each test is a pair of:

1. An Urd snippet.
2. The expected concrete syntax tree in s-expression form.

Run all tests:

```sh
npm test
# or
tree-sitter test
```

Parse a single file:

```sh
tree-sitter parse examples/quest/cave.urd
```

### Writing new corpus tests

Each test block follows this format (note the `===` / `---` delimiters):

```
================================================================================
My test name
================================================================================

let x: int = 42

--------------------------------------------------------------------------------

(source_file
  (declaration
    kind: "let"
    name: (identifier_path (identifier))
    type: (type_annotation (type_name))
    value: (integer)))
```

## Node types

### Statements

| Node | Description |
|------|-------------|
| `source_file` | Root node — sequence of top-level statements |
| `block` | `{ stmt* }` — a curly-brace block |
| `declaration` | `const`/`let`/`global` variable binding |
| `assignment` | `ident = expr` mutation |
| `subscript_assignment` | `ident[key] = expr` |
| `if_statement` | `if cond { } elif … else { }` |
| `elif_clause` | `elif cond { }` branch |
| `else_clause` | `else { }` branch |
| `match_statement` | `match expr { pattern { } … }` |
| `match_arm` | One arm of a `match` |
| `return_statement` | `return [expr]` |
| `jump_statement` | `jump label [and return]` |
| `let_call_statement` | `let x = jump label and return` |
| `label_statement` | `label name { }` — a named section |
| `dialogue_statement` | `<speaker>: "…"` or `<speaker>: { … }` |
| `dialogue_block` | Multi-line `{ "…" \n "…" }` dialogue content |
| `menu_statement` | `menu { "opt" { } … }` |
| `menu_option` | One option inside a `menu` |
| `enum_declaration` | `enum Name { A, B, C }` |
| `struct_declaration` | `struct Name { field: Type }` |
| `struct_field` | One field inside a `struct` |
| `decorator_definition` | `decorator name<event: kind>(params) { }` |
| `event_constraint` | `<event: dialogue>` or `<event: choice>` |
| `decorator_param` | One parameter in a decorator definition |
| `decorator` | `@name` or `@name(args…)` application |
| `decorated_statement` | Decorator(s) followed by a decoratable node |
| `import_statement` | `import "path" as alias` |

### Expressions

| Node | Description |
|------|-------------|
| `binary_expr` | `left op right` — all infix operators |
| `unary_expr` | `op operand` — `!`, `not`, `-` |
| `call_expr` | `func(args…)` |
| `subscript_expr` | `obj[key]` |
| `paren_expr` | `(expr)` |
| `list_expr` | `[elem, …]` |
| `map_expr` | `:{ key: val, … }` |
| `identifier_path` | Dot-separated path, e.g. `Direction.North` |
| `identifier` | Single name segment |
| `end_bang` | `end!` or `end!()` terminator |
| `todo_bang` | `todo!` or `todo!()` placeholder terminator |
| `wildcard` | `_` used in `match` patterns |

### Literals

| Node | Description |
|------|-------------|
| `integer` | Decimal / hex (`0x`) / octal (`0o`) / binary (`0b`); `_` separators allowed |
| `float` | Floating-point with `.` or `e` exponent |
| `boolean` | `true` or `false` |
| `null` | `null` |
| `dice` | `NdM` or `NDM` (e.g. `2d6`, `1D20`) |
| `string` | `"…"` — multiline, with escape sequences and interpolation |
| `string_literal_fragment` | Plain text segment inside a string |
| `escape_sequence` | `\n`, `\t`, `\r`, `\\`, `\"`, `\{`, `\}`, `\xHH`, `\uHHHH`, `\u{…}` |
| `interpolation` | `{variable.path}` or `{variable:format}` |
| `interpolation_path` | The variable path inside `{…}` |
| `format_spec` | The format string after `:` inside `{…}` |

### Types

| Node | Description |
|------|-------------|
| `type_annotation` | `: TypeName` following a name |
| `type_name` | Built-in (`int`, `float`, `bool`, `str`, `null`, `list`, `map`, `dice`, `label`) or a user-defined `identifier_path` |

### Named fields (selected)

```
declaration:          kind, name, type, value
assignment:           target, value
subscript_assignment: object, key, value
if_statement:         condition, consequence, elif_clause, else_clause
elif_clause:          condition, body
else_clause:          body
match_statement:      scrutinee, arm
match_arm:            pattern, body
return_statement:     value
jump_statement:       label
let_call_statement:   name, target
label_statement:      name, body
dialogue_statement:   speaker, content
dialogue_block:       line
menu_statement:       option
menu_option:          label, body
enum_declaration:     name, variant
struct_declaration:   name, field
struct_field:         name, type
decorator_definition: name, event_constraint, param, body
event_constraint:     kind
decorator_param:      name, type
decorator:            name, argument
import_statement:     path, alias
binary_expr:          left, operator, right
unary_expr:           operator, operand
call_expr:            function, argument
subscript_expr:       object, key
list_expr:            element
map_expr:             key, value
```

## Operator precedence

Higher number = tighter binding.

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 11 | `!` `not` `-` (unary) | prefix |
| 10 | `*` `/` `//` `%` | left |
| 9 | `+` `-` | left |
| 8 | `<<` `>>` | left |
| 7 | `>` `<` `>=` `<=` | left |
| 6 | `==` `!=` | left |
| 5 | `&` | left |
| 4 | `^` | left |
| 3 | `\|` | left |
| 2 | `and` `&&` | left |
| 1 | `or` `\|\|` | left |

## Queries

### `queries/highlights.scm`

Provides syntax highlighting captures compatible with the
[standard capture-name hierarchy](https://tree-sitter.github.io/tree-sitter/syntax-highlighting#highlight-names).

Key captures:

| Capture | Applied to |
|---------|-----------|
| `@keyword.storage.modifier` | `const`, `let`, `global` |
| `@keyword.control.conditional` | `if`, `elif`, `else`, `match` |
| `@keyword.control.return` | `return`, `end!`, `todo!` |
| `@keyword.control.jump` | `jump` |
| `@keyword.other` | `label`, `menu` |
| `@keyword.storage.type` | `enum`, `struct`, `decorator` |
| `@keyword.control.import` | `import`, `as` |
| `@keyword.operator` | `and`, `or`, `&&`, `\|\|`, `not` |
| `@operator` | arithmetic, bitwise, comparison operators |
| `@constant.numeric.integer` | integers |
| `@constant.numeric.float` | floats |
| `@constant.builtin.boolean` | `true`, `false` |
| `@constant.builtin` | `null` |
| `@constant.numeric` | dice literals |
| `@string` | string nodes |
| `@string.escape` | escape sequences |
| `@string.special` | interpolation nodes, menu option labels |
| `@variable.other.member` | interpolation paths, struct fields, speakers |
| `@type.builtin` | `int`, `float`, `bool`, `str`, `null`, `list`, `map`, `dice`, `label` |
| `@type` | user-defined type names, enum names, struct names |
| `@label` | label names, jump targets |
| `@attribute` | decorator names |
| `@function` | call expression function paths |
| `@variable` | generic identifier paths |
| `@comment.line` | `# …` comments |

### `queries/locals.scm`

Tracks lexical scopes, definitions, and references for rename, go-to-definition,
and symbol highlighting in editors that support `@local.*` captures.

- **Scopes**: `source_file`, `block`, label bodies, if/elif/else branches,
  match arms, menu option bodies, decorator bodies.
- **Definitions**: `let`/`const`/`global` bindings, `let_call_statement` names,
  `label` names, `enum` names and variants, `struct` names and fields,
  `decorator` names and parameters, `import` aliases.
- **References**: free `identifier_path` uses, jump targets, call function paths,
  assignment targets, match scrutinees and arm patterns, string interpolation paths.

### `queries/indents.scm`

Controls automatic indentation using `@indent`, `@dedent`, and `@branch`
captures.  Every `{` that opens a meaningful block (code block, label body,
if/elif/else, match, menu, dialogue block, enum, struct, decorator, list, map,
call argument list) increases indentation; the matching `}` decreases it.
`elif` and `else` are tagged `@branch` so they correctly step back from the
previous block before starting a new indented region.

## Editor integration

### Neovim (nvim-treesitter)

1. Place (or symlink) this directory somewhere on your Lua runtime path, e.g.
   `~/.local/share/nvim/lazy/tree-sitter-urd`.

2. Register the grammar in your Neovim config:

   ```lua
   local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
   parser_config.urd = {
     install_info = {
       url = "https://github.com/metdxt/urd",
       files = { "tree-sitter-urd/src/parser.c" },
       branch = "master",
       generate_requires_npm = false,
       requires_generate_from_grammar = false,
     },
     filetype = "urd",
   }
   ```

3. Associate the `.urd` file extension:

   ```lua
   vim.filetype.add({ extension = { urd = "urd" } })
   ```

4. Install: `:TSInstall urd`

### Helix

1. Add the grammar to `~/.config/helix/languages.toml`:

   ```toml
   [[language]]
   name = "urd"
   scope = "source.urd"
   file-types = ["urd"]
   comment-token = "#"
   indent = { tab-width = 4, unit = "    " }

   [[grammar]]
   name = "urd"
   source = { git = "https://github.com/metdxt/urd", subpath = "tree-sitter-urd" }
   ```

2. Copy query files into the Helix runtime directory:

   ```sh
   mkdir -p ~/.config/helix/runtime/queries/urd
   cp queries/*.scm ~/.config/helix/runtime/queries/urd/
   ```

3. Fetch and build: `hx --grammar fetch && hx --grammar build`

### Zed

The grammar is registered via `tree-sitter.json` and `package.json`.  Once the
parser is published to npm, Zed can pick it up automatically.  For local
development, point Zed's `languages` configuration at this directory.

## License

MIT