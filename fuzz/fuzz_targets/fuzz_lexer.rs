#![no_main]

use libfuzzer_sys::fuzz_target;
use urd::lexer::lex_src;

fuzz_target!(|data: &[u8]| {
    // Only fuzz valid UTF-8 — the lexer operates on `&str`.
    if let Ok(src) = std::str::from_utf8(data) {
        // Exhaust the entire token stream; no token should cause a panic.
        let lexer = lex_src(src);
        for _token in lexer {}
    }
});
