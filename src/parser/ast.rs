use crate::runtime::value::RuntimeValue;

#[derive(Debug, Clone)]
pub struct Ast {
    content: AstContent,
}

#[derive(Debug, Clone)]
pub enum AstContent {
    Value(RuntimeValue),
}

impl Ast {
    pub fn new(content: AstContent) -> Self {
        Ast { content }
    }

    /// Construct new value AST node
    pub fn value(val: RuntimeValue) -> Self {
        Ast::new(AstContent::Value(val))
    }
}
