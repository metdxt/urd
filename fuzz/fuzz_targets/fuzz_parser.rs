#![no_main]

use libfuzzer_sys::fuzz_target;

use chumsky::Parser as _;

use urd::parser::block::script;
use urd::parser::expr::reset_expr_fuel;
use urd::parser::test_util::{lex_to_vec, make_input};

fuzz_target!(|data: &[u8]| {
    // Only valid UTF-8 can be fed to the lexer.
    let src = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Lex: collect tokens (errors become Token::Error) via the test_util helper.
    let tokens = lex_to_vec(src);

    // Build chumsky input from the token slice.
    let input = make_input(&tokens, src.len());

    // Reset the per-parse fuel counter so deeply nested / ambiguous bracket
    // sequences are bounded rather than exponential.
    reset_expr_fuel();

    // Parse: we don't care about the result, only that it doesn't panic.
    let _ = script().parse(input);
});
