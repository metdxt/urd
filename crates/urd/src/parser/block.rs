//! # Block Parsing Module
//!
//! This module handles parsing of code blocks and statements.
//! Blocks are collections of statements enclosed in curly braces.

use chumsky::prelude::*;

use crate::{
    lexer::Token,
    parser::{
        ast::{
            Ast, DeclKind, Decorator, DecoratorParam, EventConstraint, FnParam, ImportSymbol,
            MatchArm, MatchPattern, StructField, TokSpan, TypeAnnotation,
        },
        expr::{
            comma_separated_exprs, declaration, expr, extern_declaration, type_annotation,
            type_name,
        },
    },
    runtime::value::RuntimeValue,
};

use super::aliases::{BoxedUrdParser, UrdInput, UrdParser};

/// Parses a 1-or-2-segment label path used by `jump` and `let … = jump … and return`.
///
/// - Single segment: `label_name`
/// - Two segments:   `module.label` (dot-joined for cross-module jumps)
/// - Three or more segments: emits a diagnostic and recovers by truncating to the
///   first two segments.
fn label_path_parser<'tok, I: UrdInput<'tok>>()
-> impl Parser<'tok, I, (String, SimpleSpan), extra::Err<Rich<'tok, Token, SimpleSpan>>> + Clone {
    select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
        Token::IdentPath(path) if path.len() == 2 => format!("{}.{}", path[0], path[1]),
    }
    .map_with(|lbl, extra| (lbl, extra.span()))
    .or(select! {
        Token::IdentPath(path) if path.len() >= 3 => path,
    }
    .validate(|path, extra, emitter| {
        emitter.emit(chumsky::error::Rich::custom(
            extra.span(),
            format!(
                "jump label path `{}` has {} segments; maximum is 2 \
                 (`module.label` for cross-module jumps)",
                path.join("."),
                path.len()
            ),
        ));
        path[..2].join(".") // best-effort recovery: truncate to first two segments
    })
    .map_with(|lbl, extra| (lbl, extra.span())))
}

/// Parser for a single statement, optionally preceded by `##` documentation comments.
///
/// One or more consecutive `## text` lines immediately before a statement are
/// collected, joined with `\n`, and attached to the resulting AST node via
/// [`Ast::with_doc_comment`].  Each doc-comment line must be followed by at
/// least one newline before the next doc-comment line or the statement itself.
pub fn statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    // Match a single `## text` line followed by at least one newline.
    let doc_line = select! { Token::DocComment(s) => s }
        .then_ignore(just(Token::Newline).repeated().at_least(1));

    // Collect zero or more consecutive doc-comment lines.
    let doc_comment = doc_line.repeated().collect::<Vec<String>>();

    doc_comment
        .then(statement_inner(code_block()))
        .map(|(docs, ast)| {
            if docs.is_empty() {
                ast
            } else {
                ast.with_doc_comment(docs.join("\n"))
            }
        })
        .boxed()
}

/// Parser for return statement (return expr)
pub fn return_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    just(Token::Return)
        .ignore_then(expr().or_not())
        .map_with(|val, extra| Ast::return_stmt(val).with_span(extra.span()))
        .boxed()
}

/// Parser for jump statement (`jump label` or `jump label and return`).
///
/// Labels may be single-segment (`jump my_scene`) or two-segment
/// (`jump module.my_scene`) for cross-module jumps.  Three or more segments
/// produce a diagnostic error with best-effort recovery (the path is
/// truncated to the first two segments).
pub fn jump_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    just(Token::Jump)
        .ignore_then(label_path_parser())
        .then(
            just(Token::And)
                .ignore_then(just(Token::Return))
                .or_not()
                .map(|r| r.is_some()),
        )
        .map_with(|((label, label_span), expects_return), extra| {
            Ast::jump_stmt(label, expects_return)
                .with_jump_label_span(label_span)
                .with_span(extra.span())
        })
        .boxed()
}

/// Parser for `let name = jump label and return` (subroutine call with result binding)
pub fn let_call_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    just(Token::Let)
        .ignore_then(
            select! {
                Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
            }
            .map_with(|name, extra| (name, extra.span())),
        )
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Jump))
        .then(label_path_parser())
        .then_ignore(just(Token::And))
        .then_ignore(just(Token::Return))
        .map_with(|((name, name_span), (target, target_span)), extra| {
            Ast::let_call(name, target)
                .with_let_call_spans(name_span, target_span)
                .with_span(extra.span())
        })
        .boxed()
}

/// Parser for import statements: `import "path" as alias`
pub fn import_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    import_statement_parser().boxed()
}

fn import_statement_parser<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    // Form 1: `import "path" as alias`  (whole-module import)
    let form1 = just(Token::Import)
        .ignore_then(select! {
            Token::StrLit(s) => s.to_string(),
        })
        .then_ignore(just(Token::As))
        .then(select! {
            Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
        })
        .map_with(|(path, alias), extra| Ast::import_module(path, alias).with_span(extra.span()));

    // Form 2: `import symbol as something from "path"`  (single-symbol import)
    let form2 = just(Token::Import)
        .ignore_then(select! {
            Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
        })
        .then(
            just(Token::As)
                .ignore_then(select! {
                    Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
                })
                .or_not(),
        )
        .then_ignore(just(Token::From))
        .then(select! {
            Token::StrLit(s) => s.to_string(),
        })
        .map_with(|((original, alias_opt), path), extra| {
            let alias = alias_opt.unwrap_or_else(|| original.clone());
            Ast::import(
                path,
                vec![ImportSymbol {
                    original: Some(original),
                    alias,
                }],
            )
            .with_span(extra.span())
        });

    // Form 3: `import (sym1 as a1, sym2) from "path"`  (multi-symbol import)
    let symbol_item = select! {
        Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
    }
    .then(
        just(Token::As)
            .ignore_then(select! {
                Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
            })
            .or_not(),
    )
    .map(|(original, alias_opt)| {
        let alias = alias_opt.unwrap_or_else(|| original.clone());
        ImportSymbol {
            original: Some(original),
            alias,
        }
    });

    let symbol_list = symbol_item
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen));

    let form3 = just(Token::Import)
        .ignore_then(symbol_list)
        .then_ignore(just(Token::From))
        .then(select! {
            Token::StrLit(s) => s.to_string(),
        })
        .map_with(|(symbols, path), extra| Ast::import(path, symbols).with_span(extra.span()));

    choice((form3, form2, form1)).boxed()
}

/// Parser for a single decorator: `@name` or `@name(arg1, arg2, ...)`
fn decorator_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    Decorator,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    let name = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
    }
    .labelled("decorator name");

    let args =
        comma_separated_exprs().delimited_by(just(Token::LeftParen), just(Token::RightParen));

    just(Token::At)
        .ignore_then(name)
        .then(args.or_not())
        .map_with(|(name, maybe_args), extra| match maybe_args {
            Some(args) => Decorator::new(name, args).with_span(extra.span()),
            None => Decorator::bare(name).with_span(extra.span()),
        })
        .boxed()
}

/// Parser for a sequence of decorators, each on its own line, ending with at least one newline
/// before the decorated statement.
fn decorators_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    Vec<Decorator>,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    decorator_parser()
        .separated_by(just(Token::Newline).repeated().at_least(1))
        .at_least(1)
        .allow_leading()
        .collect::<Vec<_>>()
        .then_ignore(just(Token::Newline).repeated().at_least(1))
}

/// Parser for built-in terminator calls: `end!`, `end!()`, `todo!`, `todo!()`.
///
/// Both the bare form (no parentheses) and the call form (with `()`) are
/// accepted and produce an identical `Call` AST node so that the dead-end
/// pass and compiler can treat them uniformly.
fn terminator_statement<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    select! {
        Token::EndBang  => "end!",
        Token::TodoBang => "todo!",
    }
    .then_ignore(
        just(Token::LeftParen)
            .then_ignore(just(Token::RightParen))
            .or_not(),
    )
    .map_with(|name, extra| {
        let span = extra.span();
        let func = Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]));
        Ast::call(func, Ast::expr_list(vec![])).with_span(span)
    })
}

/// Core statement parser — enumerates every valid statement form in Urd.
///
/// # Bare expression statements are **not** supported
///
/// Unlike most imperative languages, `statement_inner` has **no `expr()` fallback**.
/// A standalone expression such as a bare function call is *not* a valid statement:
///
/// ```text
/// # ✗ Parse error — bare call is not a statement
/// log("debug")
///
/// # ✓ Capture the return value (even if unused)
/// let _result = log("debug")
/// ```
///
/// This means every statement must match one of the explicit forms listed in the
/// combinator chain below (declaration, assignment, jump, return, dialogue,
/// menu, label, if/match, fn/decorator def, import, terminator, or a decorated
/// variant of the above).
///
/// # Why no `expr()` fallback?
///
/// Urd is a **dialogue scripting language**, not a general-purpose language.
/// The statement grammar is intentionally restrictive:
///
/// - **Readability over flexibility** — narrative scripts are read by writers and
///   designers, not just programmers.  Allowing arbitrary expressions as
///   statements (e.g. `a + b`, `true`, `items.len()`) would introduce
///   "do-nothing" lines that are almost always bugs in a dialogue context.
///
/// - **Side-effect–free expressions are meaningless** — Urd functions are pure
///   and cannot mutate ambient state.  A bare `add(1, 2)` has no observable
///   effect, so permitting it would only mask mistakes.
///
/// - **Clear intent** — requiring `let _result = …` for fire-and-forget calls
///   makes the "I don't care about the result" intent explicit and grep-able.
///   (Note: bare `_` is a wildcard token, not an identifier, so `let _ = …`
///   is a parse error — use a named variable like `_result` instead.)
///
/// // DESIGN NOTE: This is a deliberate omission.  The grammar reference
/// // (`docs/src/reference/grammar.md`) lists `expr_stmt` as a production but
/// // it was never implemented in the parser.  If bare expression statements are
/// // desired in the future (e.g. for side-effectful host callbacks), add
/// // `.or(expr())` as the **last** alternative in the chain below and update
/// // the grammar reference accordingly.  See the project roadmap for the
/// // tracking item "bare expression statements".
fn statement_inner<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    // Decoratable constructs: labeled_block, dialogue, menu, bare block
    let decoratable = labeled_block_parser(block.clone())
        .or(menu_parser(block.clone()))
        .or(dialogue())
        .or(block.clone());

    let decorated =
        decorators_parser()
            .then(decoratable.clone())
            .map_with(|(decorators, node), extra| {
                node.with_decorators(decorators).with_span(extra.span())
            });

    // decorated_declaration handles `@decorator\ndeclaration` where the
    // decorated thing is a `global`, `let`, or `const` declaration.
    // This is the mechanism that allows `@fluent global gold = 50` and
    // `@fluent("alias") let item_name = "value"` in urd source files.
    // It must be tried BEFORE `decorated` (which only covers labeled blocks,
    // menus, dialogues, and bare blocks) so that `@fluent\nlet x = …` is
    // recognised as a decorated declaration and not rejected because `let` is
    // not a `decoratable` construct.
    let decorated_declaration =
        decorators_parser()
            .then(declaration())
            .map_with(|(decorators, node), extra| {
                node.with_decorators(decorators).with_span(extra.span())
            });

    // let_call_statement must come FIRST (before declaration) because it also
    // starts with `let` but is a statement form that must not yield mid-expression.
    // terminator_statement comes early to claim EndBang/TodoBang before the
    // generic expression fallback can consume them.
    // anon_fn_assignment must precede declaration() because both start with
    // `let`/`const`/`global`; the `fn` token after `=` distinguishes them.
    //
    // NOTE: There is intentionally no `.or(expr())` fallback here.
    // See the doc comment on this function for the rationale.
    terminator_statement()
        .or(let_call_statement())
        .or(assignment())
        .or(extern_declaration())
        .or(anon_fn_assignment(block.clone()))
        .or(declaration())
        .or(if_parser(block.clone()))
        .or(return_statement())
        .or(jump_statement())
        .or(import_statement())
        .or(enum_decl_parser())
        .or(struct_decl_parser())
        .or(match_parser(block.clone()))
        .or(fn_def_parser(block.clone()))
        .or(decorator_def_parser(block))
        .or(decorated_declaration)
        .or(decorated)
        .or(decoratable)
}

/// Parser for assignment statements (ident = expr) and subscript assignment (ident[key] = expr).
/// This allows mutating previously declared variables and map/list entries.
pub fn assignment<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    // Subscript assignment: ident[key] = value  (must come FIRST — more specific)
    let subscript_assign = select! {
        Token::IdentPath(path) if path.len() == 1 => path
    }
    .map_with(|path, extra| Ast::value(RuntimeValue::IdentPath(path)).with_span(extra.span()))
    .then(expr().delimited_by(just(Token::LeftBracket), just(Token::RightBracket)))
    .then_ignore(just(Token::Assign))
    .then(expr())
    .map_with(|((object, key), value), extra| {
        Ast::subscript_assign(object, key, value).with_span(extra.span())
    });

    // Plain assignment: ident = expr  (existing logic)
    // Accepts both single-segment locals (`score = 1`) and multi-segment
    // cross-module globals (`lib.score = 1`).  The compiler distinguishes them
    // via the BinOp(Assign) path — no length guard needed here.
    let plain_assign = select! {
        Token::IdentPath(path) => path
    }
    .map_with(|path, extra| Ast::value(RuntimeValue::IdentPath(path)).with_span(extra.span()))
    .then_ignore(just(Token::Assign))
    .then(expr())
    .map_with(|(ident, value), extra| Ast::assign_op(ident, value).with_span(extra.span()));

    subscript_assign.or(plain_assign).boxed()
}

/// Parses the optional `<event: dialogue|choice>` constraint clause.
/// Returns `EventConstraint::Any` when the clause is absent.
fn event_constraint_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    EventConstraint,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    let kind = select! {
        Token::IdentPath(p) if p == ["dialogue"] => EventConstraint::Dialogue,
        Token::IdentPath(p) if p == ["choice"] => EventConstraint::Choice,
    }
    .labelled("event kind (dialogue or choice)");

    just(Token::LessThan)
        .ignore_then(select! { Token::IdentPath(p) if p == ["event"] => () })
        .ignore_then(just(Token::Colon))
        .ignore_then(kind)
        .then_ignore(just(Token::GreaterThan))
        .or_not()
        .map(|opt| opt.unwrap_or(EventConstraint::Any))
}

/// Parses `(name: Type, name2, ...)` — the parameter list of a decorator definition.
fn decorator_params_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    Vec<DecoratorParam>,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    let param = select! {
        Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
    }
    .labelled("parameter name")
    .then(type_annotation().or_not())
    .map(|(name, type_annotation)| DecoratorParam {
        name,
        type_annotation,
    });

    param
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
}

/// Parses `(name: Type, name2, ...)` — the parameter list of a function definition.
pub fn fn_params_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    Vec<FnParam>,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    let param = select! {
        Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
    }
    .labelled("parameter name")
    .then(type_annotation().or_not())
    .map(|(name, type_annotation)| FnParam {
        name,
        type_annotation,
    });

    param
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
}

/// Parses `-> TypeName` — the return-type annotation on a function definition.
pub fn return_type_parser<'tok, I: UrdInput<'tok>>() -> impl Parser<
    'tok,
    I,
    TypeAnnotation,
    chumsky::extra::Err<chumsky::error::Rich<'tok, Token, chumsky::span::SimpleSpan>>,
> + Clone {
    just(Token::Arrow).ignore_then(type_name().labelled("return type"))
}

/// Parses a full function definition:
/// `fn [name](param: Type, ...) -> RetType { body }`
///
/// The name is optional; when absent the result is an anonymous function
/// expression (`FnDef { name: None, ... }`).  The return type and parameters
/// are also both optional.
fn fn_def_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let name = select! {
        Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
    }
    .map_with(|n, extra| (n, extra.span()))
    .labelled("function name")
    .or_not();

    just(Token::Fn)
        .ignore_then(name)
        .then(fn_params_parser())
        .then(return_type_parser().or_not())
        .then(block)
        .map_with(|(((name_opt, params), ret_type), body), extra| {
            let (name_str, name_span) = match name_opt {
                Some((n, s)) => (Some(n), Some(s)),
                None => (None, None),
            };
            Ast::fn_def(name_str, params, ret_type, body)
                .with_fn_name_span(name_span)
                .with_span(extra.span())
        })
        .boxed()
}

/// Parses `let/const/global name [: Type] = fn(params) [-> RetType] { body }` —
/// an anonymous-function value bound to a variable.
///
/// This must be tried *before* the regular [`declaration`] parser because both
/// start with a `let`/`const`/`global` keyword.  The `fn` token immediately
/// after `=` uniquely identifies this form.
///
/// The `block` parameter is the enclosing [`code_block`] context, threaded
/// through so that anonymous function bodies can contain nested block constructs
/// (including further anonymous functions) without triggering construction-time
/// infinite recursion.
fn anon_fn_assignment<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let decl_word = select! {
        Token::Const  => DeclKind::Constant,
        Token::Let    => DeclKind::Variable,
        Token::Global => DeclKind::Global,
    };

    let ident = select! {
        Token::IdentPath(path) if path.len() == 1 => Ast::value(RuntimeValue::IdentPath(path)),
    }
    .labelled("variable name");

    let anon_fn = just(Token::Fn)
        .ignore_then(fn_params_parser())
        .then(return_type_parser().or_not())
        .then(block)
        .map_with(|((params, ret_type), body), extra| {
            Ast::fn_def(None, params, ret_type, body).with_span(extra.span())
        });

    decl_word
        .then(ident)
        .then(type_annotation().or_not())
        .then_ignore(just(Token::Assign))
        .then(anon_fn)
        .map_with(|(((decl, name), annotation), def), extra| {
            let span = extra.span();
            match annotation {
                Some(ann) => Ast::typed_decl(decl, name, ann, def).with_span(span),
                None => Ast::decl(decl, name, def).with_span(span),
            }
        })
        .labelled("anonymous function binding")
}

/// Parses a full decorator definition:
/// `decorator name<event: dialogue|choice>(param: Type, ...) { body }`
fn decorator_def_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let name = select! {
        Token::IdentPath(p) if p.len() == 1 => p[0].clone(),
    }
    .labelled("decorator name");

    just(Token::DecoratorKw)
        .ignore_then(name)
        .then(event_constraint_parser())
        .then(decorator_params_parser())
        .then(block)
        .map_with(|(((name, event_constraint), params), body), extra| {
            Ast::decorator_def(name, event_constraint, params, body).with_span(extra.span())
        })
        .boxed()
}

/// Parser for dialogue lines
pub fn dialogue<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    let content = select! {
        Token::StrLit(s) => Ast::value(RuntimeValue::Str(s)),
    }
    .map_with(|ast, extra| ast.with_span(extra.span()))
    .or(expr()
        .separated_by(
            just(Token::Comma)
                .ignored()
                .or(just(Token::Newline).repeated().at_least(1).ignored()),
        )
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
        .map(Ast::expr_list));

    // Speakers are restricted to `identifier_path` tokens (not arbitrary
    // expressions).  This matches the tree-sitter grammar and avoids ambiguity
    // with other identifier-starting constructs (assignments use `=`, declarations
    // use a keyword prefix, so a bare `ident, ...: content` pattern is unambiguous).
    let speaker_path = select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    }
    .map_with(|ast, extra| ast.with_span(extra.span()));

    let speakers = speaker_path
        .separated_by(just(Token::Comma))
        .at_least(1)
        .collect::<Vec<_>>()
        .map_with(|paths, extra| Ast::expr_list(paths).with_span(extra.span()));

    speakers
        .then_ignore(just(Token::Colon))
        .then(content)
        .map_with(|(speakers, content), extra| {
            Ast::dialogue(speakers, content).with_span(extra.span())
        })
        .boxed()
}

/// Parser for if/elif/else statement
pub fn if_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    if_parser(code_block()).boxed()
}

/// Parser for labeled block
pub fn labeled_block<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    labeled_block_parser(code_block()).boxed()
}

fn labeled_block_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let ident = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
    }
    .map_with(|name, extra| (name, extra.span()))
    .labelled("identifier");

    just(Token::Label)
        .ignore_then(ident)
        .then(block)
        .map_with(|((label, label_span), body), extra| {
            Ast::labeled_block(label, body)
                .with_label_span(label_span)
                .with_span(extra.span())
        })
        .boxed()
}

fn if_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let condition = expr();

    let else_block = just(Token::Else).ignore_then(block.clone());

    let elif = just(Token::Elif)
        .ignore_then(condition.clone())
        .then(block.clone());

    just(Token::If)
        .ignore_then(condition)
        .then(block)
        .then(elif.repeated().collect::<Vec<_>>())
        .then(else_block.or_not())
        .map_with(|(((cond, body), elifs), else_b), extra| {
            let span = extra.span();
            let else_part = elifs
                .into_iter()
                .rfold(else_b, |acc, (c, b)| Some(Ast::if_stmt(c, b, acc)));
            Ast::if_stmt(cond, body, else_part).with_span(span)
        })
        .boxed()
}

/// Parser for a code block delimited by curly braces.
/// Contains a list of statements.
pub fn code_block<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    recursive(|block| {
        let separator = just(Token::Newline).or(just(Token::Semicolon));

        statement_inner(block)
            .separated_by(separator.repeated().at_least(1))
            .allow_leading()
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
            .map_with(|stmts, extra| Ast::block(stmts).with_span(extra.span()))
    })
    .boxed()
}

/// Parser for menu statement
pub fn menu<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    menu_parser(code_block()).boxed()
}

/// Parser for enum declarations: `enum Foo { A, B, C }`
pub fn enum_decl<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    enum_decl_parser().boxed()
}

/// Parses a struct declaration: `struct Name { field: Type ... }`
pub fn struct_decl<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    struct_decl_parser().boxed()
}

/// Parser for match statements: `match expr { pattern { ... } ... }`
pub fn match_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    match_parser(code_block()).boxed()
}

/// Parser for decorator definitions: `decorator name<event: kind>(params...) { body }`
pub fn decorator_def<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    decorator_def_parser(code_block()).boxed()
}

/// Parser for extern declarations (`extern const/global name: Type`)
pub fn extern_decl<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    extern_declaration().boxed()
}

fn menu_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let string_option = decorators_parser()
        .or_not()
        .then(select! { Token::StrLit(s) => s })
        .then(block.clone())
        .map_with(|((maybe_decorators, label), code_block), extra| {
            // Use Display trait to convert ParsedString to String
            let node = Ast::menu_option(format!("{}", label), code_block, false);
            match maybe_decorators {
                Some(decs) => node.with_decorators(decs).with_span(extra.span()),
                None => node.with_span(extra.span()),
            }
        });

    let default_option = just(Token::Wildcard)
        .ignore_then(block)
        .map_with(|code_block, extra| Ast::menu_default_option(code_block).with_span(extra.span()));

    let option = string_option.or(default_option);

    let options = option
        .separated_by(just(Token::Newline).repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Menu)
        .ignore_then(options.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
        .map_with(|options, extra| Ast::menu(options).with_span(extra.span()))
        .boxed()
}

/// Parses an enum declaration: `enum Name { Variant1, Variant2, ... }`
///
/// Variants can be separated by commas, newlines, or a mix of both.
/// Trailing separators are allowed.
fn enum_decl_parser<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    // Separator: comma or newline, at least one, trailing allowed
    let sep = just(Token::Comma)
        .or(just(Token::Newline))
        .repeated()
        .at_least(1);

    let variant = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone()
    }
    .labelled("enum variant")
    .map_with(|name, extra| (name, TokSpan(extra.span())));

    let variants = variant
        .separated_by(sep)
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>();

    let name = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone()
    }
    .labelled("enum name");

    just(Token::Enum)
        .ignore_then(name)
        .then(variants.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
        .map_with(|(name, variants), extra| Ast::enum_decl(name, variants).with_span(extra.span()))
        .boxed()
}

/// Parses a struct declaration: `struct Name { field: Type, ... }`
///
/// Fields can be separated by commas, newlines, or a mix of both.
/// Trailing separators are allowed.
fn struct_decl_parser<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    // Separator: comma or newline, at least one, trailing allowed
    let sep = just(Token::Comma)
        .or(just(Token::Newline))
        .repeated()
        .at_least(1);

    let field_name = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone()
    }
    .labelled("field name");

    let field = field_name
        .map_with(|name, extra| (name, extra.span()))
        .then(type_annotation())
        .map(|((name, span), type_annotation)| StructField {
            name,
            span: TokSpan(span),
            type_annotation,
        })
        .labelled("struct field");

    let fields = field
        .separated_by(sep)
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>();

    let struct_name = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone()
    }
    .labelled("struct name");

    just(Token::Struct)
        .ignore_then(struct_name)
        .then(fields.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
        .map_with(|(name, fields), extra| Ast::struct_decl(name, fields).with_span(extra.span()))
        .boxed()
}

/// Parses a match statement: `match expr { pattern { ... } ... }`
///
/// Each arm is `pattern block`. Patterns are separated by newlines.
/// Supported patterns: `_` wildcard, literals (bool/int/float/str/null), and identifier paths.
fn match_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let range_pattern = select! { Token::IntLit(i) => i }
        .then(
            just(Token::DotDotEq)
                .to(true)
                .or(just(Token::DotDot).to(false)),
        )
        .then(select! { Token::IntLit(i) => i })
        .then(
            just(Token::As)
                .ignore_then(select! {
                    Token::IdentPath(p) if p.len() == 1 => p[0].clone()
                })
                .or_not(),
        )
        .map(|(((start, inclusive), end), binding)| MatchPattern::Range {
            start: Ast::value(RuntimeValue::Int(start)),
            end: Ast::value(RuntimeValue::Int(end)),
            inclusive,
            binding,
        });

    let array_element = select! { Token::IntLit(i) => Some(Ast::value(RuntimeValue::Int(i))) }
        .or(just(Token::Wildcard).to(None));

    let array_pattern = array_element
        .separated_by(just(Token::Comma))
        .at_least(1)
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
        .map(MatchPattern::Array);

    let pattern = select! { Token::Wildcard => MatchPattern::Wildcard }
        .or(range_pattern)
        .or(array_pattern)
        .or(select! {
            Token::Null => MatchPattern::Value(Ast::value(RuntimeValue::Null)),
            Token::BoolLit(b) => MatchPattern::Value(Ast::value(RuntimeValue::Bool(b))),
            Token::IntLit(i) => MatchPattern::Value(Ast::value(RuntimeValue::Int(i))),
            Token::FloatLit(f) => MatchPattern::Value(Ast::value(RuntimeValue::Float(f))),
            Token::StrLit(s) => MatchPattern::Value(Ast::value(RuntimeValue::Str(s))),
            Token::IdentPath(p) => MatchPattern::Value(Ast::value(RuntimeValue::IdentPath(p))),
        })
        .labelled("match pattern");

    let arm = pattern
        .then(block)
        .map(|(pattern, body)| MatchArm::new(pattern, body));

    let arms = arm
        .separated_by(just(Token::Newline).repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Match)
        .ignore_then(expr())
        .then(arms.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
        .map_with(|(scrutinee, arms), extra| {
            Ast::match_stmt(scrutinee, arms).with_span(extra.span())
        })
        .boxed()
}

/// Parser for a bare script body (list of statements without braces).
pub fn script<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    // Separators between statements: newlines and semicolons.
    // Doc comments (## ...) are NOT separators — they are consumed by
    // `statement()` itself and attached to the following AST node as
    // `doc_comment`.  Including DocComment here would cause `allow_leading()`
    // to swallow them before `statement()` ever sees them.
    let separator = just(Token::Newline).or(just(Token::Semicolon));

    statement()
        .separated_by(separator.repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .map_with(|stmts, extra| Ast::block(stmts).with_span(extra.span()))
        .boxed()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_test, parser::ast::AstContent};

    #[test]
    fn test_fn_def_parses() {
        use crate::parser::ast::{FnParam, TypeAnnotation};
        let result = parse_test!(
            code_block(),
            "{\n  fn add(x: int, y: int) -> int { return x }\n}"
        );
        assert!(result.is_ok(), "fn def failed to parse: {:?}", result);
        if let Ok(ast) = result {
            if let AstContent::Block(stmts) = ast.content() {
                assert_eq!(stmts.len(), 1);
                let stmt = &stmts[0];
                assert!(
                    matches!(stmt.content(), AstContent::FnDef { name: Some(n), .. } if n == "add"),
                    "expected FnDef named 'add', got {:?}",
                    stmt.content()
                );
                if let AstContent::FnDef {
                    name,
                    params,
                    ret_type,
                    ..
                } = stmt.content()
                {
                    assert_eq!(name.as_deref(), Some("add"));
                    assert_eq!(
                        params,
                        &vec![
                            FnParam {
                                name: "x".into(),
                                type_annotation: Some(TypeAnnotation::Int)
                            },
                            FnParam {
                                name: "y".into(),
                                type_annotation: Some(TypeAnnotation::Int)
                            },
                        ]
                    );
                    assert_eq!(*ret_type, Some(TypeAnnotation::Int));
                }
            } else {
                panic!("expected Block, got {:?}", ast.content());
            }
        }
    }

    #[test]
    fn test_fn_def_no_return_type() {
        let result = parse_test!(code_block(), "{\n  fn noop() { end! }\n}");
        assert!(
            result.is_ok(),
            "fn def without return type failed: {:?}",
            result
        );
        if let Ok(ast) = result
            && let AstContent::Block(stmts) = ast.content()
        {
            assert_eq!(stmts.len(), 1);
            if let AstContent::FnDef { ret_type, .. } = stmts[0].content() {
                assert_eq!(*ret_type, None, "expected no return type");
            } else {
                panic!("expected FnDef, got {:?}", stmts[0].content());
            }
        }
    }

    #[test]
    fn test_fn_def_no_params() {
        let result = parse_test!(code_block(), "{\n  fn zero() -> int { return 0 }\n}");
        assert!(result.is_ok(), "fn def with no params failed: {:?}", result);
        if let Ok(ast) = result
            && let AstContent::Block(stmts) = ast.content()
        {
            assert_eq!(stmts.len(), 1);
            if let AstContent::FnDef {
                params, ret_type, ..
            } = stmts[0].content()
            {
                assert!(params.is_empty(), "expected empty params");
                assert_eq!(
                    *ret_type,
                    Some(crate::parser::ast::TypeAnnotation::Int),
                    "expected int return type"
                );
            } else {
                panic!("expected FnDef, got {:?}", stmts[0].content());
            }
        }
    }

    #[test]
    fn import_statement_parses() {
        let result = parse_test!(import_statement(), r#"import "foo.urd" as foo"#);
        let ast = result.expect("import statement should parse");
        if let AstContent::Import { path, symbols } = ast.content() {
            assert_eq!(path, "foo.urd");
            assert_eq!(symbols.len(), 1);
            assert_eq!(symbols[0].original, None);
            assert_eq!(symbols[0].alias, "foo");
        } else {
            panic!("Expected AstContent::Import, got {:?}", ast.content());
        }
    }

    #[test]
    fn import_statement_nested_path() {
        let result = parse_test!(
            import_statement(),
            r#"import "path/to/module.urd" as mymod"#
        );
        let ast = result.expect("import with path separators should parse");
        if let AstContent::Import { path, symbols } = ast.content() {
            assert_eq!(path, "path/to/module.urd");
            assert_eq!(symbols.len(), 1);
            assert_eq!(symbols[0].original, None);
            assert_eq!(symbols[0].alias, "mymod");
        } else {
            panic!("Expected AstContent::Import, got {:?}", ast.content());
        }
    }

    #[test]
    fn import_single_symbol_parses() {
        let result = parse_test!(import_statement(), r#"import symbol as sym from "foo.urd""#);
        let ast = result.expect("single-symbol import with alias should parse");
        if let AstContent::Import { path, symbols } = ast.content() {
            assert_eq!(path, "foo.urd");
            assert_eq!(symbols.len(), 1);
            assert_eq!(symbols[0].original, Some("symbol".to_string()));
            assert_eq!(symbols[0].alias, "sym");
        } else {
            panic!("Expected AstContent::Import, got {:?}", ast.content());
        }
    }

    #[test]
    fn import_single_symbol_no_alias_parses() {
        let result = parse_test!(import_statement(), r#"import symbol from "foo.urd""#);
        let ast = result.expect("single-symbol import without alias should parse");
        if let AstContent::Import { path, symbols } = ast.content() {
            assert_eq!(path, "foo.urd");
            assert_eq!(symbols.len(), 1);
            assert_eq!(symbols[0].original, Some("symbol".to_string()));
            assert_eq!(symbols[0].alias, "symbol");
        } else {
            panic!("Expected AstContent::Import, got {:?}", ast.content());
        }
    }

    #[test]
    fn import_multi_symbol_parses() {
        let result = parse_test!(
            import_statement(),
            r#"import (sym1 as a1, sym2) from "foo.urd""#
        );
        let ast = result.expect("multi-symbol import should parse");
        if let AstContent::Import { path, symbols } = ast.content() {
            assert_eq!(path, "foo.urd");
            assert_eq!(symbols.len(), 2);
            assert_eq!(symbols[0].original, Some("sym1".to_string()));
            assert_eq!(symbols[0].alias, "a1");
            assert_eq!(symbols[1].original, Some("sym2".to_string()));
            assert_eq!(symbols[1].alias, "sym2");
        } else {
            panic!("Expected AstContent::Import, got {:?}", ast.content());
        }
    }

    #[test]
    fn jump_local_label_parses() {
        let result = parse_test!(jump_statement(), "jump my_label");
        let ast = result.expect("local jump should parse");
        if let AstContent::Jump {
            label,
            expects_return,
            ..
        } = ast.content()
        {
            assert_eq!(label, "my_label");
            assert!(
                !expects_return,
                "plain jump should have expects_return=false"
            );
        } else {
            panic!("Expected AstContent::Jump, got {:?}", ast.content());
        }
    }

    #[test]
    fn jump_cross_module_encodes_dot_notation() {
        let result = parse_test!(jump_statement(), "jump mymod.scene_start");
        let ast = result.expect("cross-module jump should parse");
        if let AstContent::Jump {
            label,
            expects_return,
            ..
        } = ast.content()
        {
            // Dot-notation is the convention for cross-module jumps; the compiler splits on '.'
            assert_eq!(label, "mymod.scene_start");
            assert!(!expects_return);
        } else {
            panic!("Expected AstContent::Jump, got {:?}", ast.content());
        }
    }

    /// PAR-2: a 3-segment label must produce an error whose message mentions
    /// the segment limit, while still emitting a best-effort recovery value.
    #[test]
    fn jump_three_segment_label_emits_max_2_error() {
        let result = parse_test!(jump_statement(), "jump a.b.c");
        let Err(errors) = result else {
            panic!("expected an error for a 3-segment jump label, but parsing succeeded");
        };
        assert!(
            errors
                .iter()
                .any(|e| e.contains("maximum is 2") || e.contains("2 segments")),
            "expected error mentioning 'maximum is 2' or '2 segments', got: {errors:?}"
        );
    }

    /// PAR-1: `let f = fn(params) -> ret { body }` should parse as a
    /// `Declaration` whose RHS is a `FnDef` with `name: None`.
    #[test]
    fn anon_fn_declaration_name_is_none() {
        let result = parse_test!(statement(), "let f = fn(x: int) -> int { return x }");
        assert!(
            result.is_ok(),
            "anon fn declaration should parse: {result:?}"
        );
        let ast = result.unwrap();
        let AstContent::Declaration { decl_defs, .. } = ast.content() else {
            panic!("expected Declaration, got {:?}", ast.content());
        };
        let AstContent::FnDef {
            name,
            params,
            ret_type,
            ..
        } = decl_defs.content()
        else {
            panic!("expected FnDef in decl_defs, got {:?}", decl_defs.content());
        };
        assert_eq!(*name, None, "anonymous fn should have name=None");
        assert_eq!(params.len(), 1, "expected 1 param");
        assert_eq!(params[0].name, "x");
        assert_eq!(
            params[0].type_annotation,
            Some(crate::parser::ast::TypeAnnotation::Int)
        );
        assert_eq!(*ret_type, Some(crate::parser::ast::TypeAnnotation::Int));
    }

    #[test]
    fn jump_and_return_parses() {
        let src = "jump my_label and return";
        let result = parse_test!(statement(), src);
        assert!(result.is_ok(), "should parse: {result:?}");
        let ast = result.unwrap();
        match ast.content() {
            AstContent::Jump {
                label,
                expects_return,
                ..
            } => {
                assert_eq!(label, "my_label");
                assert!(*expects_return, "expects_return should be true");
            }
            other => panic!("expected Jump, got {other:?}"),
        }
    }

    #[test]
    fn let_call_parses() {
        let src = "let result = jump my_label and return";
        let result = parse_test!(statement(), src);
        assert!(result.is_ok(), "should parse: {result:?}");
        let ast = result.unwrap();
        match ast.content() {
            AstContent::LetCall { name, target, .. } => {
                assert_eq!(name, "result");
                assert_eq!(target, "my_label");
            }
            other => panic!("expected LetCall, got {other:?}"),
        }
    }

    #[test]
    fn jump_without_and_return_has_expects_return_false() {
        let src = "jump plain_label";
        let result = parse_test!(statement(), src);
        assert!(result.is_ok(), "should parse: {result:?}");
        let ast = result.unwrap();
        match ast.content() {
            AstContent::Jump {
                label,
                expects_return,
                ..
            } => {
                assert_eq!(label, "plain_label");
                assert!(!expects_return, "expects_return should be false");
            }
            other => panic!("expected Jump, got {other:?}"),
        }
    }

    #[test]
    fn end_bang_with_parens_parses_as_call() {
        let result = parse_test!(statement(), "end!()");
        let ast = result.expect("end!() should parse as a statement");
        assert!(
            matches!(ast.content(), AstContent::Call { .. }),
            "expected AstContent::Call, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn end_bang_bare_parses_as_call() {
        let result = parse_test!(statement(), "end!");
        let ast = result.expect("bare end! should parse as a statement");
        assert!(
            matches!(ast.content(), AstContent::Call { .. }),
            "expected AstContent::Call, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn todo_bang_with_parens_parses_as_call() {
        let result = parse_test!(statement(), "todo!()");
        let ast = result.expect("todo!() should parse as a statement");
        assert!(
            matches!(ast.content(), AstContent::Call { .. }),
            "expected AstContent::Call, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn todo_bang_bare_parses_as_call() {
        let result = parse_test!(statement(), "todo!");
        let ast = result.expect("bare todo! should parse as a statement");
        assert!(
            matches!(ast.content(), AstContent::Call { .. }),
            "expected AstContent::Call, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn end_bang_in_script_produces_call_node() {
        use crate::runtime::value::RuntimeValue;
        let result = parse_test!(super::script(), "end!()\n");
        let ast = result.expect("end!() in script should parse");
        // The root is a Block; first statement should be a Call with func "end!"
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1, "expected 1 statement");
            assert!(
                matches!(stmts[0].content(), AstContent::Call { .. }),
                "expected Call, got {:?}",
                stmts[0].content()
            );
            if let AstContent::Call { func_path, .. } = stmts[0].content() {
                assert!(
                    matches!(
                        func_path.content(),
                        AstContent::Value(RuntimeValue::IdentPath(p)) if p == &["end!"]
                    ),
                    "expected IdentPath([\"end!\"]), got {:?}",
                    func_path.content()
                );
            }
        } else {
            panic!("expected Block root, got {:?}", ast.content());
        }
    }

    #[test]
    fn todo_bang_in_labeled_block_parses() {
        let result = parse_test!(super::script(), "label stub {\n    todo!\n}\n");
        assert!(
            result.is_ok(),
            "todo! inside a labeled block should parse: {:?}",
            result
        );
    }

    #[test]
    fn end_bang_and_todo_bang_produce_same_call_shape() {
        use crate::runtime::value::RuntimeValue;
        let end_result = parse_test!(statement(), "end!()").expect("end!() should parse");
        let todo_result = parse_test!(statement(), "todo!()").expect("todo!() should parse");

        // Both should be Call nodes.
        assert!(matches!(end_result.content(), AstContent::Call { .. }));
        assert!(matches!(todo_result.content(), AstContent::Call { .. }));

        // The func_path of each should be a single-segment IdentPath.
        let end_name = if let AstContent::Call { func_path, .. } = end_result.content() {
            match func_path.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) => p[0].clone(),
                other => panic!("unexpected func_path: {other:?}"),
            }
        } else {
            unreachable!()
        };
        let todo_name = if let AstContent::Call { func_path, .. } = todo_result.content() {
            match func_path.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) => p[0].clone(),
                other => panic!("unexpected func_path: {other:?}"),
            }
        } else {
            unreachable!()
        };

        assert_eq!(end_name, "end!");
        assert_eq!(todo_name, "todo!");
    }

    #[test]
    fn struct_decl_parses_basic() {
        use crate::parser::ast::TypeAnnotation;
        let result = parse_test!(
            struct_decl_parser(),
            "struct Player {\n    name: str\n    health: int\n}"
        );
        let ast = result.expect("basic struct decl should parse");
        if let AstContent::StructDecl { name, fields } = ast.content() {
            assert_eq!(name, "Player");
            assert_eq!(fields.len(), 2);
            // Compare name and type_annotation only; span is assigned by the
            // parser and varies with source position, so we don't assert it.
            assert_eq!(fields[0].name, "name");
            assert_eq!(fields[0].type_annotation, TypeAnnotation::Str);
            assert_eq!(fields[1].name, "health");
            assert_eq!(fields[1].type_annotation, TypeAnnotation::Int);
        } else {
            panic!("Expected AstContent::StructDecl, got {:?}", ast.content());
        }
    }

    #[test]
    fn struct_decl_parses_comma_separated() {
        use crate::parser::ast::TypeAnnotation;
        let result = parse_test!(struct_decl_parser(), "struct Vec2 { x: float, y: float }");
        let ast = result.expect("comma-separated struct decl should parse");
        if let AstContent::StructDecl { name, fields } = ast.content() {
            assert_eq!(name, "Vec2");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name, "x");
            assert_eq!(fields[0].type_annotation, TypeAnnotation::Float);
            assert_eq!(fields[1].name, "y");
            assert_eq!(fields[1].type_annotation, TypeAnnotation::Float);
        } else {
            panic!("Expected AstContent::StructDecl, got {:?}", ast.content());
        }
    }

    #[test]
    fn struct_decl_accessible_via_statement() {
        let result = parse_test!(statement(), "struct Empty {}");
        let ast = result.expect("struct decl via statement() should parse");
        assert!(
            matches!(ast.content(), AstContent::StructDecl { .. }),
            "expected AstContent::StructDecl, got {:?}",
            ast.content()
        );
    }

    #[test]
    fn doc_comment_attached_to_statement() {
        // A single ## line immediately before a statement must be attached as
        // doc_comment on the resulting AST node.
        let result = parse_test!(statement(), "## hello world\nlet x = 1");
        let ast = result.expect("doc comment + statement should parse");
        assert_eq!(
            ast.doc_comment.as_deref(),
            Some("hello world"),
            "doc comment should be attached; content was {:?}",
            ast.content()
        );
    }

    #[test]
    fn doc_comment_multiline_attached_to_statement() {
        // Multiple ## lines are joined with '\n'.
        let result = parse_test!(statement(), "## first\n## second\nconst X = 1");
        let ast = result.expect("multiline doc comment + statement should parse");
        assert_eq!(
            ast.doc_comment.as_deref(),
            Some("first\nsecond"),
            "multiple doc comment lines should be joined; content was {:?}",
            ast.content()
        );
    }

    #[test]
    fn statement_without_doc_comment_has_none() {
        let result = parse_test!(statement(), "let y = 2");
        let ast = result.expect("bare statement should parse");
        assert!(
            ast.doc_comment.is_none(),
            "doc_comment should be None when no ## precedes the statement"
        );
    }

    #[test]
    fn doc_comment_attached_in_script() {
        use crate::compiler::loader::parse_source;
        // parse_source goes through script() → statement(); the doc comment
        // must survive all the way to the child AST node inside the Block.
        let src = "## The player\nlet player = 1\n";
        let block = parse_source(src).expect("script with doc comment should parse");
        // The block contains one child: the declaration.
        let children = block.children();
        assert_eq!(children.len(), 1, "expected exactly one statement in block");
        assert_eq!(
            children[0].doc_comment.as_deref(),
            Some("The player"),
            "doc comment must be attached to the child statement inside the block"
        );
    }

    /// Regression test: the single-string content node of a dialogue statement
    /// must carry a non-zero span so that LSP diagnostics (e.g. spellcheck
    /// warnings) point at the actual string location, not at byte 0 of the file.
    ///
    /// Before the fix, `select! { Token::StrLit(s) => Ast::value(...) }` in
    /// `dialogue()` lacked a `.map_with(|ast, extra| ast.with_span(extra.span()))`
    /// call, causing every single-string dialogue content node to retain the
    /// default `SimpleSpan::new(0, 0)`.
    #[test]
    fn dialogue_single_string_content_span_is_nonzero() {
        use crate::compiler::loader::parse_source;

        // The string literal starts at some non-zero offset inside the label
        // block. After the fix, content.span() must reflect that.
        let src = r#"
label start {
    narrator: "Hello, world!"
    end!()
}
"#;
        let ast = parse_source(src).expect("source should parse");

        // Walk the AST to find the Dialogue node.
        let mut found = false;
        crate::parser::ast::walk_ast(&ast, &mut |node| {
            if let AstContent::Dialogue { content, .. } = node.content() {
                let span = content.span();
                assert!(
                    span.start > 0 || span.end > 0,
                    "dialogue content span must not be the default zero span (0..0); \
                     got {}..{}",
                    span.start,
                    span.end,
                );
                // The span must enclose at least the string literal characters.
                assert!(
                    span.end > span.start,
                    "dialogue content span end ({}) must be greater than start ({})",
                    span.end,
                    span.start,
                );
                found = true;
            }
        });

        assert!(
            found,
            "expected to find at least one Dialogue node in the AST"
        );
    }

    /// Companion test: the block-form dialogue (`speaker: { "line1" "line2" }`)
    /// already had correct spans on individual items via `expr()` → `atom_internal`.
    /// Verify both items carry non-zero, distinct spans.
    #[test]
    fn dialogue_block_content_item_spans_are_nonzero() {
        use crate::compiler::loader::parse_source;
        use crate::runtime::value::RuntimeValue;

        let src = r#"
label start {
    narrator: {
        "First line."
        "Second line."
    }
    end!()
}
"#;
        let ast = parse_source(src).expect("source should parse");

        let mut item_spans: Vec<(usize, usize)> = Vec::new();
        crate::parser::ast::walk_ast(&ast, &mut |node| {
            if let AstContent::Dialogue { content, .. } = node.content() {
                match content.content() {
                    AstContent::ExprList(items) | AstContent::Block(items) => {
                        for item in items {
                            if let AstContent::Value(RuntimeValue::Str(_)) = item.content() {
                                let span = item.span();
                                item_spans.push((span.start, span.end));
                            }
                        }
                    }
                    _ => {}
                }
            }
        });

        assert_eq!(
            item_spans.len(),
            2,
            "expected two string items in the block dialogue"
        );
        for (start, end) in &item_spans {
            assert!(
                end > start,
                "each item span must be non-empty; got {}..{}",
                start,
                end,
            );
        }
        // The two strings are on different lines, so their spans must not overlap.
        let (_, e0) = item_spans[0];
        let (s1, _) = item_spans[1];
        assert!(
            s1 >= e0,
            "second item span start ({s1}) must be >= first item span end ({e0})"
        );
    }

    #[test]
    fn fluent_decorator_on_global_declaration_parses() {
        let result = parse_test!(statement(), "@fluent\nglobal gold = 50");
        assert!(
            result.is_ok(),
            "@fluent on global declaration should parse: {result:?}"
        );
        let ast = result.unwrap();
        assert!(
            ast.decorators().iter().any(|d| d.name() == "fluent"),
            "parsed node must carry the @fluent decorator"
        );
        match ast.content() {
            AstContent::Declaration { kind, .. } => {
                assert_eq!(*kind, DeclKind::Global, "must be a Global declaration");
            }
            other => panic!("expected Declaration, got {other:?}"),
        }
    }

    #[test]
    fn fluent_alias_decorator_on_let_declaration_parses() {
        let result = parse_test!(
            statement(),
            "@fluent(\"item\")\nlet item_name = \"Health Potion\""
        );
        assert!(
            result.is_ok(),
            "@fluent(\"alias\") on let declaration should parse: {result:?}"
        );
        let ast = result.unwrap();
        let fluent_dec = ast
            .decorators()
            .iter()
            .find(|d| d.name() == "fluent")
            .expect("must have @fluent decorator");
        match fluent_dec.args().content() {
            AstContent::ExprList(items) => {
                assert_eq!(items.len(), 1, "must have one argument");
                match items[0].content() {
                    AstContent::Value(RuntimeValue::Str(ps)) => {
                        assert_eq!(ps.to_string(), "item", "alias must be \"item\"");
                    }
                    other => panic!("argument must be a string literal, got {other:?}"),
                }
            }
            other => panic!("decorator args must be ExprList, got {other:?}"),
        }
        match ast.content() {
            AstContent::Declaration { kind, .. } => {
                assert_eq!(*kind, DeclKind::Variable, "must be a Variable declaration");
            }
            other => panic!("expected Declaration, got {other:?}"),
        }
    }

    #[test]
    fn fluent_decorator_on_const_declaration_parses() {
        let result = parse_test!(statement(), "@fluent\nconst max_gold = 999");
        assert!(
            result.is_ok(),
            "@fluent on const declaration should parse: {result:?}"
        );
        let ast = result.unwrap();
        assert!(
            ast.decorators().iter().any(|d| d.name() == "fluent"),
            "parsed node must carry the @fluent decorator"
        );
        match ast.content() {
            AstContent::Declaration { kind, .. } => {
                assert_eq!(*kind, DeclKind::Constant, "must be a Constant declaration");
            }
            other => panic!("expected Declaration, got {other:?}"),
        }
    }

    // ── Match Range / Array pattern parser tests ──────────────────────────────

    /// `2..18` parses to `MatchPattern::Range { inclusive: false, binding: None, .. }`.
    #[test]
    fn test_match_range_exclusive_parses() {
        use crate::parser::ast::MatchPattern;
        let result = parse_test!(code_block(), "{\n  match x {\n    2..18 { end! }\n  }\n}");
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 1);
                assert!(
                    matches!(
                        &arms[0].pattern,
                        MatchPattern::Range {
                            inclusive: false,
                            binding: None,
                            ..
                        }
                    ),
                    "expected Range {{ inclusive: false, binding: None }}, got {:?}",
                    arms[0].pattern
                );
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// `1..=20` parses to `MatchPattern::Range { inclusive: true, .. }`.
    #[test]
    fn test_match_range_inclusive_parses() {
        use crate::parser::ast::MatchPattern;
        let result = parse_test!(code_block(), "{\n  match x {\n    1..=20 { end! }\n  }\n}");
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 1);
                assert!(
                    matches!(
                        &arms[0].pattern,
                        MatchPattern::Range {
                            inclusive: true,
                            ..
                        }
                    ),
                    "expected inclusive Range, got {:?}",
                    arms[0].pattern
                );
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// `2..18 as n` captures binding `Some("n")`.
    #[test]
    fn test_match_range_with_binding_parses() {
        use crate::parser::ast::MatchPattern;
        let result = parse_test!(
            code_block(),
            "{\n  match x {\n    2..18 as n { end! }\n  }\n}"
        );
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 1);
                assert!(
                    matches!(
                        &arms[0].pattern,
                        MatchPattern::Range { binding: Some(b), .. } if b == "n"
                    ),
                    "expected Range with binding Some(\"n\"), got {:?}",
                    arms[0].pattern
                );
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// `[ 6 ]` parses to `MatchPattern::Array` with one `Int(6)` element.
    #[test]
    fn test_match_array_single_parses() {
        use crate::{parser::ast::MatchPattern, runtime::value::RuntimeValue};
        let result = parse_test!(
            code_block(),
            "{\n  match dice {\n    [ 6 ] { end! }\n  }\n}"
        );
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 1);
                if let MatchPattern::Array(elems) = &arms[0].pattern {
                    assert_eq!(elems.len(), 1, "expected 1 array element");
                    let elem = elems[0].as_ref().expect("expected Some element");
                    assert!(
                        matches!(elem.content(), AstContent::Value(RuntimeValue::Int(6))),
                        "expected Int(6), got {:?}",
                        elem.content()
                    );
                } else {
                    panic!("expected Array pattern, got {:?}", arms[0].pattern);
                }
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// `[ 1, 20 ]` parses to `MatchPattern::Array` with elements `Int(1)` and `Int(20)`.
    #[test]
    fn test_match_array_multi_parses() {
        use crate::{parser::ast::MatchPattern, runtime::value::RuntimeValue};
        let result = parse_test!(
            code_block(),
            "{\n  match dice {\n    [ 1, 20 ] { end! }\n  }\n}"
        );
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 1);
                if let MatchPattern::Array(elems) = &arms[0].pattern {
                    assert_eq!(elems.len(), 2, "expected 2 array elements");
                    let e0 = elems[0].as_ref().expect("expected Some element");
                    assert!(
                        matches!(e0.content(), AstContent::Value(RuntimeValue::Int(1))),
                        "expected Int(1), got {:?}",
                        e0.content()
                    );
                    let e1 = elems[1].as_ref().expect("expected Some element");
                    assert!(
                        matches!(e1.content(), AstContent::Value(RuntimeValue::Int(20))),
                        "expected Int(20), got {:?}",
                        e1.content()
                    );
                } else {
                    panic!("expected Array pattern, got {:?}", arms[0].pattern);
                }
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// A match block containing `Value`, `Range`, and `Value` arms all parse correctly.
    #[test]
    fn test_match_value_still_works_alongside_range() {
        use crate::parser::ast::MatchPattern;
        let src = "{\n  match x {\n    1 { end! }\n    2..19 { end! }\n    20 { end! }\n  }\n}";
        let result = parse_test!(code_block(), src);
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 3, "expected 3 arms, got {}", arms.len());
                assert!(
                    matches!(&arms[0].pattern, MatchPattern::Value(_)),
                    "arm[0] should be Value, got {:?}",
                    arms[0].pattern
                );
                assert!(
                    matches!(&arms[1].pattern, MatchPattern::Range { .. }),
                    "arm[1] should be Range, got {:?}",
                    arms[1].pattern
                );
                assert!(
                    matches!(&arms[2].pattern, MatchPattern::Value(_)),
                    "arm[2] should be Value, got {:?}",
                    arms[2].pattern
                );
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// An exclusive `Range` without a binding displays as `"3..7"`.
    #[test]
    fn test_match_range_display() {
        use crate::{
            parser::ast::{Ast, MatchPattern},
            runtime::value::RuntimeValue,
        };
        let pattern = MatchPattern::Range {
            start: Ast::value(RuntimeValue::Int(3)),
            end: Ast::value(RuntimeValue::Int(7)),
            inclusive: false,
            binding: None,
        };
        assert_eq!(pattern.to_string(), "3..7");
    }

    /// An exclusive `Range` with binding `"x"` displays as `"3..7 as x"`.
    #[test]
    fn test_match_range_display_with_binding() {
        use crate::{
            parser::ast::{Ast, MatchPattern},
            runtime::value::RuntimeValue,
        };
        let pattern = MatchPattern::Range {
            start: Ast::value(RuntimeValue::Int(3)),
            end: Ast::value(RuntimeValue::Int(7)),
            inclusive: false,
            binding: Some("x".to_string()),
        };
        assert_eq!(pattern.to_string(), "3..7 as x");
    }

    /// `MatchPattern::Array([Int(5)])` displays as `"[5]"`.
    #[test]
    fn test_match_array_display() {
        use crate::{
            parser::ast::{Ast, MatchPattern},
            runtime::value::RuntimeValue,
        };
        let pattern = MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(5)))]);
        assert_eq!(pattern.to_string(), "[5]");
    }

    /// `[ 1, _ ]` parses to `MatchPattern::Array` with `Some(Int(1))` and `None`.
    #[test]
    fn test_match_array_wildcard_parses() {
        use crate::{parser::ast::MatchPattern, runtime::value::RuntimeValue};
        let result = parse_test!(
            code_block(),
            "{\n  match dice {\n    [ 1, _ ] { end! }\n  }\n}"
        );
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 1);
                if let MatchPattern::Array(elems) = &arms[0].pattern {
                    assert_eq!(elems.len(), 2, "expected 2 array elements");
                    let e0 = elems[0].as_ref().expect("expected Some for first element");
                    assert!(
                        matches!(e0.content(), AstContent::Value(RuntimeValue::Int(1))),
                        "expected Int(1), got {:?}",
                        e0.content()
                    );
                    assert!(
                        elems[1].is_none(),
                        "expected None (wildcard) for second element, got {:?}",
                        elems[1]
                    );
                } else {
                    panic!("expected Array pattern, got {:?}", arms[0].pattern);
                }
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// `[ _, _ ]` parses to `MatchPattern::Array` with two `None` elements.
    #[test]
    fn test_match_array_all_wildcards_parses() {
        use crate::parser::ast::MatchPattern;
        let result = parse_test!(
            code_block(),
            "{\n  match dice {\n    [ _, _ ] { end! }\n  }\n}"
        );
        assert!(result.is_ok(), "should parse: {:?}", result);
        let ast = result.unwrap();
        if let AstContent::Block(stmts) = ast.content() {
            assert_eq!(stmts.len(), 1);
            if let AstContent::Match { arms, .. } = stmts[0].content() {
                assert_eq!(arms.len(), 1);
                if let MatchPattern::Array(elems) = &arms[0].pattern {
                    assert_eq!(elems.len(), 2, "expected 2 array elements");
                    assert!(
                        elems[0].is_none(),
                        "expected None (wildcard) for first element"
                    );
                    assert!(
                        elems[1].is_none(),
                        "expected None (wildcard) for second element"
                    );
                } else {
                    panic!("expected Array pattern, got {:?}", arms[0].pattern);
                }
            } else {
                panic!("expected Match, got {:?}", stmts[0].content());
            }
        } else {
            panic!("expected Block, got {:?}", ast.content());
        }
    }

    /// `MatchPattern::Array` with wildcards displays correctly.
    #[test]
    fn test_match_array_wildcard_display() {
        use crate::{
            parser::ast::{Ast, MatchPattern},
            runtime::value::RuntimeValue,
        };
        let pattern = MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(1))), None]);
        assert_eq!(pattern.to_string(), "[1, _]");
    }

    /// Regression test for slow-unit found by `fuzz_compiler`.
    ///
    /// The artifact `slow-unit-65926283d25e66b44c568348615b4e1e35c497c6`
    /// is 176 bytes of deeply-nested, unmatched `[` brackets.  This input
    /// triggers combinatorial explosion in the parser because every `[`
    /// is ambiguous: it could start a list literal (`atom_internal`'s
    /// `list` alternative) **or** a postfix subscript suffix.  When the
    /// matching `]` never arrives the parser backtracks and retries the
    /// other interpretation, leading to exponential work in the number of
    /// open brackets.
    ///
    /// The bottleneck is **parsing**, not lexing or compiling.
    #[test]
    fn test_slow_unit_fuzz_compiler_nested_brackets() {
        use crate::compiler::Compiler;
        use crate::parser::test_util::{lex_to_vec, make_input};
        use chumsky::Parser as _;
        use std::time::Instant;

        // Exact content of the slow-unit artifact (176 bytes).
        let src = "b[a[ho[W=7=h/[W=7=h/[a[ho[W=7=h/[a[o8o[W=7=h/[W=7=h/\
                    [a[o8o[W=7=h/[W=7=h/[a[ho[W=7=h/[a[[W=7=h/[a[o8o[W=7=h/\
                    [W=7=h/[a[ho[W=7=h/[a[[a[ho[W=[a[ho[W=7=h/[a[o8V.o3==878,\
                    ..o3==878,";

        // --- Phase 1: Lex ------------------------------------------------
        let t0 = Instant::now();
        let tokens = lex_to_vec(src);
        let lex_dur = t0.elapsed();

        // --- Phase 2: Parse ----------------------------------------------
        let input = make_input(&tokens, src.len());
        let t1 = Instant::now();
        let parse_result = script().parse(input);
        let parse_dur = t1.elapsed();

        // --- Phase 3: Compile (only if parse produced an AST) ------------
        let compile_dur;
        if let Some(ast) = parse_result.into_output() {
            let t2 = Instant::now();
            let _ = Compiler::compile(&ast);
            compile_dur = t2.elapsed();
        } else {
            compile_dur = std::time::Duration::ZERO;
        }

        let total = lex_dur + parse_dur + compile_dur;

        eprintln!("--- slow-unit timing breakdown ---");
        eprintln!("  lex:     {lex_dur:>10.3?}");
        eprintln!("  parse:   {parse_dur:>10.3?}");
        eprintln!("  compile: {compile_dur:>10.3?}");
        eprintln!("  total:   {total:>10.3?}");
        eprintln!("---------------------------------");

        // The original slow-unit took >60 s.  After a fix, parsing this
        // 176-byte input should complete in well under 5 s even on slow CI
        // machines.  If this assert fires, the exponential backtracking
        // on nested brackets has regressed.
        assert!(
            total < std::time::Duration::from_secs(5),
            "slow-unit regression: total time {total:.3?} exceeds 5 s budget \
             (lex={lex_dur:.3?}, parse={parse_dur:.3?}, compile={compile_dur:.3?})"
        );
    }
}
