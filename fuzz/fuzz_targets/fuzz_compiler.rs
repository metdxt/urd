#![no_main]

use libfuzzer_sys::fuzz_target;

use chumsky::Parser as _;

use urd::compiler::Compiler;
use urd::parser::block::script;
use urd::parser::test_util::{lex_to_vec, make_input};

fuzz_target!(|data: &[u8]| {
    // Only fuzz valid UTF-8 — the lexer operates on `&str`.
    let src = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Phase 1: Lex — collect tokens, converting errors to Token::Error.
    let tokens = lex_to_vec(src);

    // Phase 2: Parse — feed tokens into the script parser.
    let input = make_input(&tokens, src.len());
    let parse_result = script().parse(input);

    let ast = match parse_result.into_output() {
        Some(ast) => ast,
        None => return,
    };

    // Phase 3: Compile — transform AST into IR graph.
    // We don't care about compiler errors, only panics.
    let _ = Compiler::compile(&ast);
});
