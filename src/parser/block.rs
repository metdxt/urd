//! # Block Parsing Module
//!
//! This module handles parsing of code blocks and statements.
//! Blocks are collections of statements enclosed in curly braces.

use chumsky::prelude::*;

use crate::{
    lexer::Token,
    parser::{
        ast::{Ast, Decorator, DecoratorParam, EventConstraint, MatchArm, MatchPattern},
        expr::{comma_separated_exprs, declaration, expr, type_annotation},
    },
    runtime::value::RuntimeValue,
};

use super::aliases::{BoxedUrdParser, UrdInput, UrdParser};

/// Parser for a single statement.
pub fn statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    statement_inner(code_block()).boxed()
}

/// Parser for return statement (return expr)
pub fn return_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    just(Token::Return)
        .ignore_then(expr().or_not())
        .map_with(|val, extra| Ast::return_stmt(val).with_span(extra.span()))
        .boxed()
}

/// Parser for jump statement (`jump label` or `jump label and return`)
pub fn jump_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    let label = select! {
        // Local jump: `jump label_name`
        Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
        // Cross-module jump: `jump module_name.label_name`
        // Encoded as "module_name.label_name" — the compiler detects the dot.
        Token::IdentPath(path) if path.len() == 2 => format!("{}.{}", path[0], path[1]),
    };

    just(Token::Jump)
        .ignore_then(label)
        .then(
            just(Token::And)
                .ignore_then(just(Token::Return))
                .or_not()
                .map(|r| r.is_some()),
        )
        .map_with(|(label, expects_return), extra| {
            Ast::jump_stmt(label, expects_return).with_span(extra.span())
        })
        .boxed()
}

/// Parser for `let name = jump label and return` (subroutine call with result binding)
pub fn let_call_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    let label = select! {
        Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
        Token::IdentPath(path) if path.len() == 2 => format!("{}.{}", path[0], path[1]),
    };

    just(Token::Let)
        .ignore_then(select! {
            Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
        })
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Jump))
        .then(label)
        .then_ignore(just(Token::And))
        .then_ignore(just(Token::Return))
        .map_with(|(name, target), extra| Ast::let_call(name, target).with_span(extra.span()))
        .boxed()
}

/// Parser for import statements: `import "path" as alias`
pub fn import_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    import_statement_parser().boxed()
}

fn import_statement_parser<'tok, I: UrdInput<'tok>>() -> impl UrdParser<'tok, I> {
    just(Token::Import)
        .ignore_then(select! {
            Token::StrLit(s) => s.to_string(),
        })
        .then_ignore(just(Token::As))
        .then(select! {
            Token::IdentPath(path) if path.len() == 1 => path[0].clone(),
        })
        .map_with(|(path, alias), extra| Ast::import(path, alias).with_span(extra.span()))
        .boxed()
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
        .map(|(name, maybe_args)| match maybe_args {
            Some(args) => Decorator::new(name, args),
            None => Decorator::bare(name),
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

/// Helper for statement parsing, allowing recursion injection.
fn statement_inner<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    // Decoratable constructs: labeled_block, dialogue, menu, bare block
    let decoratable = labeled_block_parser(block.clone())
        .or(menu_parser(block.clone()))
        .or(dialogue())
        .or(block.clone());

    let decorated = decorators_parser()
        .then(decoratable.clone())
        .map_with(|(decorators, node), extra| {
            node.with_decorators(decorators).with_span(extra.span())
        });

    // let_call_statement must come FIRST (before declaration) because it also
    // starts with `let` but is a statement form that must not yield mid-expression.
    let_call_statement()
        .or(assignment())
        .or(declaration())
        .or(if_parser(block.clone()))
        .or(return_statement())
        .or(jump_statement())
        .or(import_statement())
        .or(enum_decl_parser())
        .or(match_parser(block.clone()))
        .or(decorator_def_parser(block))
        .or(decorated)
        .or(decoratable)
}

/// Parser for assignment statements (ident = expr) and subscript assignment (ident[key] = expr).
/// This allows mutating previously declared variables and map/list entries.
pub fn assignment<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    // Subscript assignment: ident[key] = value  (must come FIRST — more specific)
    let subscript_assign = select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    }
    .then(expr().delimited_by(just(Token::LeftBracket), just(Token::RightBracket)))
    .then_ignore(just(Token::Assign))
    .then(expr())
    .map_with(|((object, key), value), extra| {
        Ast::subscript_assign(object, key, value).with_span(extra.span())
    });

    // Plain assignment: ident = expr  (existing logic)
    let plain_assign = select! {
        Token::IdentPath(path) => Ast::value(RuntimeValue::IdentPath(path))
    }
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

/// Parses a full decorator definition:
/// `decorator name<event: dialogue>(param: Type, ...) { body }`
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

    comma_separated_exprs()
        .delimited_by(just(Token::LessThan), just(Token::GreaterThan))
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
    .labelled("identifier");

    just(Token::Label)
        .ignore_then(ident)
        .then(block)
        .map_with(|(label, body), extra| {
            Ast::labeled_block(label, body).with_span(extra.span())
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

/// Parser for match statements: `match expr { pattern { ... } ... }`
pub fn match_statement<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    match_parser(code_block()).boxed()
}

/// Parser for decorator definitions: `decorator name<event: kind>(params...) { body }`
pub fn decorator_def<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    decorator_def_parser(code_block()).boxed()
}

fn menu_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let option_label = select! {
        Token::StrLit(s) => s,
    };

    let option = option_label.then(block).map_with(|(label, code_block), extra| {
        // Use Display trait to convert ParsedString to String
        Ast::menu_option(format!("{}", label), code_block).with_span(extra.span())
    });

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
    .labelled("enum variant");

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
        .map_with(|(name, variants), extra| {
            Ast::enum_decl(name, variants).with_span(extra.span())
        })
        .boxed()
}

/// Parses a match statement: `match expr { pattern { ... } ... }`
///
/// Each arm is `pattern block`. Patterns are separated by newlines.
/// Supported patterns: `_` wildcard, literals (bool/int/float/str/null), and identifier paths.
fn match_parser<'tok, I: UrdInput<'tok>>(
    block: impl UrdParser<'tok, I> + 'tok,
) -> impl UrdParser<'tok, I> {
    let pattern = select! { Token::Wildcard => MatchPattern::Wildcard }
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

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)]

    use super::*;
    use crate::{parse_test, parser::ast::AstContent};

    #[test]
    fn import_statement_parses() {
        let result = parse_test!(import_statement(), r#"import "foo.urd" as foo"#);
        let ast = result.expect("import statement should parse");
        if let AstContent::Import { path, alias } = ast.content() {
            assert_eq!(path, "foo.urd");
            assert_eq!(alias, "foo");
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
        if let AstContent::Import { path, alias } = ast.content() {
            assert_eq!(path, "path/to/module.urd");
            assert_eq!(alias, "mymod");
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
        } = ast.content()
        {
            assert_eq!(label, "my_label");
            assert!(!expects_return, "plain jump should have expects_return=false");
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
        } = ast.content()
        {
            // Dot-notation is the convention for cross-module jumps; the compiler splits on '.'
            assert_eq!(label, "mymod.scene_start");
            assert!(!expects_return);
        } else {
            panic!("Expected AstContent::Jump, got {:?}", ast.content());
        }
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
            AstContent::LetCall { name, target } => {
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
            } => {
                assert_eq!(label, "plain_label");
                assert!(!expects_return, "expects_return should be false");
            }
            other => panic!("expected Jump, got {other:?}"),
        }
    }
}

/// Parser for a bare script body (list of statements without braces).
pub fn script<'tok, I: UrdInput<'tok>>() -> BoxedUrdParser<'tok, I> {
    let separator = just(Token::Newline).or(just(Token::Semicolon));

    statement()
        .separated_by(separator.repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .map_with(|stmts, extra| Ast::block(stmts).with_span(extra.span()))
        .boxed()
}
