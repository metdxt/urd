# Grammar Reference

This chapter presents a simplified EBNF-style grammar for the Urd dialogue scripting language. It is intended as a readable reference, not an exhaustive formal specification — consult the parser source (`crates/urd/src/parser/`) for the definitive grammar.

## Notation

| Symbol | Meaning |
|--------|---------|
| `=` | Definition |
| `\|` | Alternation |
| `( )` | Grouping |
| `[ ]` | Optional (zero or one) |
| `{ }` | Repetition (zero or more) |
| `" "` | Terminal (keyword or punctuation) |
| `UPPER` | Token produced by the lexer |

---

## Program Structure

```ebnf
program         = { top_level_stmt } ;

top_level_stmt  = import_stmt
                | enum_decl
                | struct_decl
                | const_decl
                | global_decl
                | extern_decl
                | fn_def
                | decorator_def
                | labeled_block ;
```

## Imports

```ebnf
import_stmt     = "import" STRING "as" IDENT
                | "import" IDENT [ "as" IDENT ] "from" STRING
                | "import" "(" import_list ")" "from" STRING ;

import_list     = import_item { "," import_item } [ "," ] ;

import_item     = IDENT [ "as" IDENT ] ;
```

## Type Declarations

```ebnf
enum_decl       = "enum" IDENT "{" enum_body "}" ;

enum_body       = IDENT { IDENT } ;

struct_decl     = "struct" IDENT "{" struct_body "}" ;

struct_body     = struct_field { struct_field } ;

struct_field    = IDENT ":" type_annotation ;
```

## Variable Declarations

```ebnf
const_decl      = "const" IDENT [ ":" type_annotation ] "=" expr ;

global_decl     = "global" IDENT [ ":" type_annotation ] "=" expr ;

let_decl        = "let" IDENT [ ":" type_annotation ] "=" expr ;

extern_decl     = "extern" IDENT [ ":" type_annotation ] ;

type_annotation = "int" | "str" | "float" | "bool" | "null"
                | "list" | "map" | "dice" | "range" | IDENT ;
```

## Labels

```ebnf
labeled_block   = { decorator } "label" IDENT "{" block "}" ;

block           = { statement } ;
```

## Statements

```ebnf
statement       = dialogue
                | menu
                | if_stmt
                | match_stmt
                | jump_stmt
                | return_stmt
                | let_decl
                | let_call_stmt
                | assignment
                | end_stmt
                | todo_stmt ;

end_stmt        = "end!" "(" ")" ;

todo_stmt       = "todo!" "(" ")" ;
```

> **Note — no bare expression statements.** Unlike most imperative languages,
> Urd does **not** allow a standalone expression (such as a function call) as a
> statement. Writing `log("debug")` on its own is a parse error. To call a
> function for its side effects and discard the result, capture it in a named
> variable: `let _result = log("debug")`. (Bare `_` is a wildcard token, not
> an identifier, so `let _ = …` is also a parse error.) This keeps narrative
> scripts free of "do-nothing" lines that are almost always bugs in a dialogue
> context. See the
> [Functions](../language/functions.md#calling-without-capturing) chapter for
details.

## Dialogue

```ebnf
dialogue        = { decorator } ident_path { "," ident_path } ":" dialogue_body ;

dialogue_body   = STRING
                | "{" { STRING } "}" ;
```

A dialogue line consists of one or more speaker identifier paths (comma-separated) followed by `:` and either a single string or a multi-line block of strings.

## Menus

```ebnf
menu            = { decorator } "menu" "{" { menu_option } "}" ;

menu_option     = STRING "{" block "}"
                | "_" "{" block "}" ;
```

Each menu option is a string label followed by a block of statements that execute when the player selects that option.

## Control Flow

```ebnf
if_stmt         = "if" expr "{" block "}"
                  { "elif" expr "{" block "}" }
                  [ "else" "{" block "}" ] ;

match_stmt      = "match" expr "{" { match_arm } "}" ;

match_arm       = pattern "{" block "}" ;

pattern         = "_"
                | literal
                | IDENT "." IDENT
                | range_pattern
                | "[" pattern_list "]"
                | pattern "as" IDENT ;

range_pattern   = literal ".." literal
                | literal "..=" literal ;

pattern_list    = pattern { "," pattern } [ "," ] ;
```

## Jumps and Returns

```ebnf
jump_stmt       = "jump" ident_path [ "and" "return" ] ;

let_call_stmt   = "let" IDENT "=" "jump" ident_path "and" "return" ;

return_stmt     = "return" [ expr ] ;
```

The `jump ... and return` form is a subroutine call — control transfers to the target label and returns to the call site when the target executes `return`.

## Functions

```ebnf
fn_def          = "fn" IDENT "(" [ param_list ] ")" [ "->" type_annotation ] "{" block "}" ;

param_list      = param { "," param } [ "," ] ;

param           = IDENT [ ":" type_annotation ] ;
```

## Decorators

```ebnf
decorator_def   = "decorator" IDENT [ "<" event_constraint ">" ]
                  "(" [ param_list ] ")" "{" block "}" ;

event_constraint = "event" ":" IDENT ;

decorator       = "@" IDENT [ "(" arg_list ")" ] ;

arg_list        = expr { "," expr } [ "," ] ;
```

Script-defined decorators may declare an event constraint (e.g., `<event: dialogue>`) that restricts which event types the decorator can be applied to.

## Expressions

```ebnf
expr            = or_expr [ "=" expr ] ;

or_expr         = and_expr { ( "or" | "||" ) and_expr } ;

and_expr        = bitor_expr { ( "and" | "&&" ) bitor_expr } ;

bitor_expr      = bitxor_expr { "|" bitxor_expr } ;

bitxor_expr     = bitand_expr { "^" bitand_expr } ;

bitand_expr     = comparison { "&" comparison } ;

comparison      = relational { ( "==" | "!=" ) relational } ;

relational      = shift_expr { ( ">" | "<" | ">=" | "<=" | "in" ) shift_expr } ;

shift_expr      = additive { ( "<<" | ">>" | ".." | "..=" ) additive } ;

additive        = multiplicative { ( "+" | "-" ) multiplicative } ;

multiplicative  = unary { ( "*" | "/" | "//" | "%" ) unary } ;

unary           = ( "-" | "!" | "not" ) unary
                | postfix ;

postfix         = primary { call_or_index | method_call | field_access } ;

call_or_index   = "(" [ arg_list ] ")"
                | "[" expr "]" ;

method_call     = "." IDENT "(" [ arg_list ] ")" ;

field_access    = "." IDENT ;

primary         = literal
                | ident_path
                | "(" expr ")"
                | list_literal
                | map_literal
                | fn_literal ;
```

## Literals

```ebnf
literal         = INT_LIT
                | FLOAT_LIT
                | STRING
                | "true"
                | "false"
                | "null"
                | dice_lit ;

dice_lit        = INT_LIT "d" INT_LIT ;

list_literal    = "[" [ expr { "," expr } [ "," ] ] "]" ;

map_literal     = ":" "{" [ map_entry { "," map_entry } [ "," ] ] "}" ;

map_entry       = IDENT ":" expr ;

fn_literal      = "fn" "(" [ param_list ] ")" [ "->" type_annotation ] "{" block "}" ;
```

## Identifiers

```ebnf
ident_path      = IDENT { "." IDENT } ;
```

A dotted identifier path is used for qualified references (e.g., `module.label`, `Faction.Rebel`, `player.inventory.gold`).

## Assignments

```ebnf
assignment      = ident_path "=" expr
                | ident_path "[" expr "]" "=" expr ;
```

The second form is subscript assignment for lists and maps.

## Lexer Tokens

| Token | Description | Examples |
|-------|-------------|---------|
| `IDENT` | Identifier | `narrator`, `gold`, `Faction` |
| `INT_LIT` | Integer literal | `0`, `42`, `-7` |
| `FLOAT_LIT` | Float literal | `3.14`, `-0.5` |
| `STRING` | String literal with interpolation | `"hello"`, `"You have {gold} gold"` |

### String Interpolation

Strings support inline variable interpolation via `{variable}` or `{variable.field.path}` syntax. Only identifier paths are supported — arbitrary expressions are not allowed inside interpolation braces.

```urd
"You have {gold} gold pieces."
"Hello, {player.name}! Your health is {health}."
```

### Comments

```urd
# This is a line comment. Everything after # until end-of-line is ignored.

## This is a doc comment. It documents the next declaration.
```

---

## Operator Precedence (Highest to Lowest)

For quick reference, see also [Operator Reference](./operators.md).

| Precedence | Operators | Associativity |
|:----------:|-----------|:-------------:|
| 11 (highest) | `-` (unary), `!`, `not` | Right |
| 10 | `*`, `/`, `//`, `%` | Left |
| 9 | `+`, `-` | Left |
| 8 | `<<`, `>>`, `..`, `..=` | Left |
| 7 | `>`, `<`, `>=`, `<=`, `in` | Left |
| 6 | `==`, `!=` | Left |
| 5 | `&` | Left |
| 4 | `^` | Left |
| 3 | `\|` | Left |
| 2 | `and`, `&&` | Left |
| 1 | `or`, `\|\|` | Left |
| 0 (lowest) | `=` | Right |

---

> **Note:** This grammar is simplified for readability. The actual parser handles additional edge cases such as trailing commas, optional semicolons, newline sensitivity, and error recovery. When in doubt, the parser source in `crates/urd/src/parser/` is the authoritative reference.