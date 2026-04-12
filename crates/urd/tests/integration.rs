#![allow(missing_docs)]

#[path = "integration/fixtures.rs"]
mod fixtures;

#[path = "integration/dot_render.rs"]
mod dot_render;

#[path = "integration/parser_block_basic.rs"]
mod parser_block_basic;

#[path = "integration/parser_block_enum_match.rs"]
mod parser_block_enum_match;

#[path = "integration/parser_block_menu.rs"]
mod parser_block_menu;

#[path = "integration/parser_expr.rs"]
mod parser_expr;

#[path = "integration/parser_decorator_def.rs"]
mod parser_decorator_def;

#[path = "integration/import.rs"]
mod import;

#[path = "integration/mermaid_render.rs"]
mod mermaid_render;

#[path = "integration/vm_decorator.rs"]
mod vm_decorator;

#[path = "integration/adversarial.rs"]
mod adversarial;

#[path = "integration/adversarial_new.rs"]
mod adversarial_new;

#[path = "integration/adversarial_round2.rs"]
mod adversarial_round2;

#[path = "integration/parser_fn_return_type.rs"]
mod parser_fn_return_type;

#[path = "integration/string_format_and_extern.rs"]
mod string_format_and_extern;

#[path = "integration/builtin_methods.rs"]
mod builtin_methods;

#[path = "integration/in_operator_and_functions.rs"]
mod in_operator_and_functions;

#[path = "integration/example_smoke.rs"]
mod example_smoke;

#[path = "integration/extern_object.rs"]
mod extern_object;
