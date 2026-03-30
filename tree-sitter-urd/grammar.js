/**
 * tree-sitter-urd
 *
 * Grammar for Urd — an in-game dialogue scripting language.
 * https://github.com/metdxt/urd
 */

// ---------------------------------------------------------------------------
// Helper combinators
// ---------------------------------------------------------------------------

/** Zero or more items separated by commas, no trailing comma. */
function commaSep(rule) {
    return optional(commaSep1(rule));
}

/** One or more items separated by commas, no trailing comma. */
function commaSep1(rule) {
    return seq(rule, repeat(seq(",", rule)));
}

/** Zero or more items separated by `sep`, with optional leading/trailing. */
function lineSep(sep, rule) {
    return seq(
        repeat(sep),
        optional(seq(rule, repeat(seq(repeat1(sep), rule)), repeat(sep))),
    );
}

// ---------------------------------------------------------------------------
// Grammar
// ---------------------------------------------------------------------------

module.exports = grammar({
    name: "urd",

    // Tokens that can appear anywhere between other tokens.
    extras: ($) => [
        /[ \t\r]+/, // horizontal whitespace
        $.line_comment,
    ],

    // The rule used for keyword extraction (conflicts with identifiers).
    word: ($) => $.identifier,

    // Explicit conflict resolutions needed by the GLR/Earley engine.
    conflicts: ($) => [
        // `identifier_path` vs `type_name` when a bare identifier follows `:`
        [$.type_name, $.identifier_path],
        // declaration vs assignment both start with an identifier
        [$.declaration, $.assignment],
        // let_call_statement vs declaration (both start with `let ident`)
        [$.let_call_statement, $.declaration],
        // let_call_statement vs identifier_path inside declaration value
        [$.identifier_path, $.let_call_statement],
        // call_expr vs identifier_path (identifier followed by `(`)
        [$.call_expr, $.identifier_path],
        // return_statement: `return` alone vs `return expr` — the optional
        // value is ambiguous when the next token could start an expression
        // (e.g. `return` followed by an integer on the same line).
        [$.return_statement],
        // subscript_assignment `ident[key] = val` vs a subscript_expr inside _expr
        [$._expr, $.subscript_assignment],
        // subscript_assignment vs plain assignment — both start with identifier_path
        [$.assignment, $.subscript_assignment],
    ],

    // Tokens that must be matched as complete atomic units.
    inline: ($) => [$._literal, $._statement, $._match_pattern],

    supertypes: ($) => [$._expr, $._statement, $._literal],

    rules: {
        // -----------------------------------------------------------------------
        // Top-level
        // -----------------------------------------------------------------------

        source_file: ($) => repeat($._statement_or_newline),

        _statement_or_newline: ($) => choice("\n", $._statement),

        _statement: ($) =>
            choice(
                $.declaration,
                $.let_call_statement,
                $.assignment,
                $.subscript_assignment,
                $.if_statement,
                $.match_statement,
                $.return_statement,
                $.jump_statement,
                $.label_statement,
                $.dialogue_statement,
                $.menu_statement,
                $.enum_declaration,
                $.struct_declaration,
                $.decorator_definition,
                $.decorated_statement,
                $.import_statement,
                $.block,
                // Bare expression statement (e.g. a call like `end!()`)
                $._expr,
            ),

        // -----------------------------------------------------------------------
        // Block
        // -----------------------------------------------------------------------

        block: ($) => seq("{", repeat(choice("\n", ";", $._statement)), "}"),

        // -----------------------------------------------------------------------
        // Comments
        // -----------------------------------------------------------------------

        line_comment: (_) => token(seq("#", /.*/)),

        // -----------------------------------------------------------------------
        // Literals
        // -----------------------------------------------------------------------

        _literal: ($) =>
            choice($.integer, $.float, $.boolean, $.null, $.dice, $.string),

        // Integer: decimal, hex (0x), octal (0o), binary (0b); underscores allowed.
        integer: (_) =>
            token(
                choice(
                    /0x[0-9a-fA-F][0-9a-fA-F_]*/,
                    /0b[01][01_]*/,
                    /0o[0-7][0-7_]*/,
                    /[0-9][0-9_]*/,
                ),
            ),

        // Float: requires `.` or exponent to distinguish from integer.
        float: (_) =>
            token(
                choice(
                    /(?:[0-9][0-9_]*)?\.([0-9][0-9_]*)(?:[eE][+-]?[0-9][0-9_]*)?/,
                    /[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*/,
                ),
            ),

        boolean: (_) => choice("true", "false"),

        null: (_) => "null",

        // Dice: NdM or NDM, 1-3 digits each side, e.g. 2d6, 1D20.
        dice: (_) => token(/\d{1,3}[dD]\d{1,3}/),

        // -----------------------------------------------------------------------
        // Strings (multiline, interpolated)
        // -----------------------------------------------------------------------

        string: ($) =>
            seq(
                '"',
                repeat(
                    choice(
                        $.string_literal_fragment,
                        $.escape_sequence,
                        $.interpolation,
                    ),
                ),
                '"',
            ),

        string_literal_fragment: (_) => token.immediate(/[^\\{"]+/),

        escape_sequence: (_) =>
            token.immediate(
                choice(
                    /\\[ntr\\"{}]/,
                    /\\x[0-9a-fA-F]{2}/,
                    /\\u[0-9a-fA-F]{4}/,
                    /\\u\{[0-9a-fA-F]{1,6}\}/,
                ),
            ),

        interpolation: ($) =>
            seq(
                token.immediate("{"),
                $.interpolation_path,
                optional(seq(":", $.format_spec)),
                token.immediate("}"),
            ),

        // Dot-separated identifier path inside `{...}`.
        interpolation_path: (_) =>
            token.immediate(
                /[ \t]*(_*[a-zA-Z][_a-zA-Z0-9]*)(\._*[a-zA-Z][_a-zA-Z0-9]*)*[ \t]*/,
            ),

        // Everything after `:` up to the closing `}`.
        format_spec: (_) => token.immediate(/[^}]+/),

        // -----------------------------------------------------------------------
        // Identifiers
        // -----------------------------------------------------------------------

        // Single-segment identifier (no dots).
        identifier: (_) => /(_*[a-zA-Z][_a-zA-Z0-9]*)/,

        // Dot-chained path, e.g. `module.label` or `Direction.North`.
        identifier_path: ($) =>
            seq($.identifier, repeat(seq(".", $.identifier))),

        // The `_` wildcard used in match patterns.
        wildcard: (_) => token("_"),

        // -----------------------------------------------------------------------
        // Type annotations  `:  TypeName`
        // -----------------------------------------------------------------------

        type_annotation: ($) => seq(":", $.type_name),

        type_name: ($) =>
            choice(
                "int",
                "float",
                "bool",
                "str",
                "list",
                "map",
                "dice",
                "label",
                // `null` is also a valid type name
                "null",
                // User-defined named type (e.g. `Direction`, `my_mod.Point`)
                $.identifier_path,
            ),

        // -----------------------------------------------------------------------
        // Expressions
        // -----------------------------------------------------------------------

        _expr: ($) =>
            choice(
                $._literal,
                $.identifier_path,
                $.call_expr,
                $.subscript_expr,
                $.list_expr,
                $.map_expr,
                $.paren_expr,
                $.unary_expr,
                $.binary_expr,
                $.end_bang,
                $.todo_bang,
            ),

        // [ e, e, ... ]
        list_expr: ($) =>
            seq("[", commaSep(field("element", $._expr)), optional(","), "]"),

        // :{ key: value, ... }
        map_expr: ($) =>
            seq(
                ":{",
                commaSep(
                    seq(field("key", $._expr), ":", field("value", $._expr)),
                ),
                optional(","),
                "}",
            ),

        // func(arg, ...)
        call_expr: ($) =>
            prec(
                13,
                seq(
                    field("function", $.identifier_path),
                    "(",
                    commaSep(field("argument", $._expr)),
                    optional(","),
                    ")",
                ),
            ),

        // obj[key]  — left-associative, highest postfix precedence
        subscript_expr: ($) =>
            prec.left(
                12,
                seq(field("object", $._expr), "[", field("key", $._expr), "]"),
            ),

        // (expr)
        paren_expr: ($) => seq("(", $._expr, ")"),

        // end!  or  end!()
        // prec(1) on the longer form so that when `(` follows `end!` the parser
        // eagerly consumes it as part of end_bang rather than leaving it dangling.
        end_bang: (_) =>
            choice(prec(1, seq(token("end!"), "(", ")")), token("end!")),

        // todo!  or  todo!()
        todo_bang: (_) =>
            choice(prec(1, seq(token("todo!"), "(", ")")), token("todo!")),

        // Named node for unary operator tokens — makes the operator visible
        // as (unary_operator) in the concrete syntax tree.
        unary_operator: (_) => choice("!", "not", "-"),

        // Unary operators: !, not, - (prefix)
        unary_expr: ($) =>
            prec(
                11,
                seq(
                    field(
                        "operator",
                        alias(choice("!", "not", "-"), $.unary_operator),
                    ),
                    field("operand", $._expr),
                ),
            ),

        // Named node for all binary operator tokens — makes the operator
        // visible as (binary_operator) in the concrete syntax tree.
        // Each prec.left(...) alternative below aliases its specific subset
        // of tokens to this type, preserving per-operator precedence.
        binary_operator: (_) =>
            choice(
                "*",
                "/",
                "//",
                "%",
                "+",
                "-",
                "<<",
                ">>",
                ">",
                "<",
                ">=",
                "<=",
                "==",
                "!=",
                "&",
                "^",
                "|",
                "and",
                "&&",
                "or",
                "||",
            ),

        // All binary operators — field names: left, operator, right.
        binary_expr: ($) =>
            choice(
                // precedence 10 — multiplicative
                prec.left(
                    10,
                    seq(
                        field("left", $._expr),
                        field(
                            "operator",
                            alias(
                                choice("*", "/", "//", "%"),
                                $.binary_operator,
                            ),
                        ),
                        field("right", $._expr),
                    ),
                ),
                // precedence 9 — additive
                prec.left(
                    9,
                    seq(
                        field("left", $._expr),
                        field(
                            "operator",
                            alias(choice("+", "-"), $.binary_operator),
                        ),
                        field("right", $._expr),
                    ),
                ),
                // precedence 8 — bitwise shifts
                prec.left(
                    8,
                    seq(
                        field("left", $._expr),
                        field(
                            "operator",
                            alias(choice("<<", ">>"), $.binary_operator),
                        ),
                        field("right", $._expr),
                    ),
                ),
                // precedence 7 — relational
                prec.left(
                    7,
                    seq(
                        field("left", $._expr),
                        field(
                            "operator",
                            alias(
                                choice(">", "<", ">=", "<="),
                                $.binary_operator,
                            ),
                        ),
                        field("right", $._expr),
                    ),
                ),
                // precedence 6 — equality
                prec.left(
                    6,
                    seq(
                        field("left", $._expr),
                        field(
                            "operator",
                            alias(choice("==", "!="), $.binary_operator),
                        ),
                        field("right", $._expr),
                    ),
                ),
                // precedence 5 — bitwise AND
                prec.left(
                    5,
                    seq(
                        field("left", $._expr),
                        field("operator", alias("&", $.binary_operator)),
                        field("right", $._expr),
                    ),
                ),
                // precedence 4 — bitwise XOR
                prec.left(
                    4,
                    seq(
                        field("left", $._expr),
                        field("operator", alias("^", $.binary_operator)),
                        field("right", $._expr),
                    ),
                ),
                // precedence 3 — bitwise OR
                prec.left(
                    3,
                    seq(
                        field("left", $._expr),
                        field("operator", alias("|", $.binary_operator)),
                        field("right", $._expr),
                    ),
                ),
                // precedence 2 — logical AND
                prec.left(
                    2,
                    seq(
                        field("left", $._expr),
                        field(
                            "operator",
                            alias(choice("and", "&&"), $.binary_operator),
                        ),
                        field("right", $._expr),
                    ),
                ),
                // precedence 1 — logical OR
                prec.left(
                    1,
                    seq(
                        field("left", $._expr),
                        field(
                            "operator",
                            alias(choice("or", "||"), $.binary_operator),
                        ),
                        field("right", $._expr),
                    ),
                ),
            ),

        // -----------------------------------------------------------------------
        // Declarations
        // -----------------------------------------------------------------------

        declaration: ($) =>
            seq(
                field("kind", choice("const", "let", "global")),
                field("name", $.identifier_path),
                optional(field("type", $.type_annotation)),
                "=",
                field("value", $._expr),
            ),

        // -----------------------------------------------------------------------
        // Assignment
        // -----------------------------------------------------------------------

        assignment: ($) =>
            seq(
                field("target", $.identifier_path),
                "=",
                field("value", $._expr),
            ),

        // obj[key] = value
        subscript_assignment: ($) =>
            seq(
                field("object", $.identifier_path),
                "[",
                field("key", $._expr),
                "]",
                "=",
                field("value", $._expr),
            ),

        // -----------------------------------------------------------------------
        // Control flow
        // -----------------------------------------------------------------------

        if_statement: ($) =>
            seq(
                "if",
                field("condition", $._expr),
                field("consequence", $.block),
                repeat(field("elif_clause", $.elif_clause)),
                optional(field("else_clause", $.else_clause)),
            ),

        elif_clause: ($) =>
            seq("elif", field("condition", $._expr), field("body", $.block)),

        else_clause: ($) => seq("else", field("body", $.block)),

        // match expr { pattern { ... } ... }
        match_statement: ($) =>
            seq(
                "match",
                field("scrutinee", $._expr),
                "{",
                repeat("\n"),
                repeat(seq(field("arm", $.match_arm), repeat("\n"))),
                "}",
            ),

        match_arm: ($) =>
            seq(field("pattern", $._match_pattern), field("body", $.block)),

        _match_pattern: ($) =>
            choice($.wildcard, $._literal, $.identifier_path),

        // return [expr]
        return_statement: ($) =>
            seq("return", optional(field("value", $._expr))),

        // jump label  or  jump label and return
        jump_statement: ($) =>
            seq(
                "jump",
                field("label", $.identifier_path),
                optional(seq("and", "return")),
            ),

        // let name = jump label and return
        let_call_statement: ($) =>
            seq(
                "let",
                field("name", $.identifier),
                "=",
                "jump",
                field("target", $.identifier_path),
                "and",
                "return",
            ),

        // -----------------------------------------------------------------------
        // Dialogue constructs
        // -----------------------------------------------------------------------

        // <speaker, ...>: "text"  or  <speaker>: { "line1" \n "line2" }
        // Speakers are restricted to identifier_path (not _expr) to avoid
        // ambiguity: if we used _expr the parser cannot tell whether `>`
        // closes the speaker list or is a comparison operator.
        dialogue_statement: ($) =>
            seq(
                token(prec(1, "<")),
                commaSep1(field("speaker", $.identifier_path)),
                token(prec(1, ">")),
                ":",
                field("content", choice($.string, $.dialogue_block)),
            ),

        // { "line1" \n "line2" \n ... }
        dialogue_block: ($) =>
            seq("{", repeat(choice("\n", ",", field("line", $.string))), "}"),

        // menu { "option" { ... } ... }
        menu_statement: ($) =>
            seq(
                "menu",
                "{",
                repeat("\n"),
                repeat(seq(field("option", $.menu_option), repeat("\n"))),
                "}",
            ),

        menu_option: ($) =>
            seq(field("label", $.string), field("body", $.block)),

        // label name { ... }
        label_statement: ($) =>
            seq("label", field("name", $.identifier), field("body", $.block)),

        // -----------------------------------------------------------------------
        // Type definitions
        // -----------------------------------------------------------------------

        // enum Direction { North, South, East, West }
        enum_declaration: ($) =>
            seq(
                "enum",
                field("name", $.identifier),
                "{",
                lineSep(choice(",", "\n"), field("variant", $.identifier)),
                "}",
            ),

        // struct Character { name: str \n name_color: str }
        struct_declaration: ($) =>
            seq(
                "struct",
                field("name", $.identifier),
                "{",
                lineSep(choice(",", "\n"), field("field", $.struct_field)),
                "}",
            ),

        struct_field: ($) =>
            seq(field("name", $.identifier), field("type", $.type_annotation)),

        // -----------------------------------------------------------------------
        // Decorator definitions  &  decorator application
        // -----------------------------------------------------------------------

        // decorator name<event: dialogue>(param: Type, ...) { ... }
        decorator_definition: ($) =>
            seq(
                "decorator",
                field("name", $.identifier),
                optional(field("event_constraint", $.event_constraint)),
                "(",
                commaSep(field("param", $.decorator_param)),
                optional(","),
                ")",
                field("body", $.block),
            ),

        // <event: dialogue>  or  <event: choice>
        event_constraint: ($) =>
            seq(
                "<",
                "event",
                ":",
                field("kind", choice("dialogue", "choice")),
                ">",
            ),

        decorator_param: ($) =>
            seq(
                field("name", $.identifier),
                optional(field("type", $.type_annotation)),
            ),

        // @name  or  @name(arg1, arg2, ...)
        decorator: ($) =>
            seq(
                "@",
                field("name", $.identifier),
                optional(
                    seq(
                        "(",
                        commaSep(field("argument", $._expr)),
                        optional(","),
                        ")",
                    ),
                ),
            ),

        // One or more @decorator lines followed by a decoratable statement.
        decorated_statement: ($) =>
            seq(repeat1(seq($.decorator, "\n")), $._decoratable_statement),

        _decoratable_statement: ($) =>
            choice(
                $.label_statement,
                $.dialogue_statement,
                $.menu_statement,
                $.block,
            ),

        // -----------------------------------------------------------------------
        // Imports
        // -----------------------------------------------------------------------

        // import "path/to/file" as alias
        import_statement: ($) =>
            seq(
                "import",
                field("path", $.string),
                "as",
                field("alias", $.identifier),
            ),
    },
});
