//! # Abstract Syntax Tree (AST) Module
//!
//! This module defines the Abstract Syntax Tree for the Urd language. The AST represents
//! the hierarchical structure of parsed code and is used by the runtime for evaluation.
//!
//! ## Components
//!
//! - [`Ast`]: The main AST node that contains AST content
//! - [`AstContent`]: Enum representing different types of AST nodes
//! - [`Operator`]: Enum for binary operations
//! - [`UnaryOperator`]: Enum for unary operations

use crate::runtime::value::RuntimeValue;

/// Represents a node in the Abstract Syntax Tree.
#[derive(Debug, Clone)]
pub struct Ast {
    /// The content of this AST node
    content: AstContent,
}

/// Represents binary operators in the Urd language.
#[derive(Debug, Clone)]
pub enum Operator {
    /// Addition operator (+)
    Plus,
    /// Subtraction operator (-)
    Minus,
    /// Multiplication operator (*)
    Multiply,
    /// Division operator (/)
    Divide,
    /// Integer division operator (//)
    DoubleSlash,
    /// Modulo operator (%)
    Percent,
    /// Equality operator (==)
    Equals,
    /// Inequality operator (!=)
    NotEquals,
    /// Greater than operator (>)
    GreaterThan,
    /// Less than operator (<)
    LessThan,
    /// Greater than or equals operator (>=)
    GreaterThanOrEquals,
    /// Less than or equals operator (<=)
    LessThanOrEquals,
    /// Bitwise AND operator (&)
    BitwiseAnd,
    /// Bitwise OR operator (|)
    BitwiseOr,
    /// Bitwise XOR operator (^)
    BitwiseXor,
    /// Left shift operator (<<)
    LeftShift,
    /// Right shift operator (>>)
    RightShift,
    /// Logical AND operator (and/&&)
    And,
    /// Logical OR operator (or/||)
    Or,
    /// Assignment operator (=)
    Assign,
}

/// Represents unary operators in the Urd language.
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    /// Bitwise NOT operator (!)
    BitwiseNot,
    /// Negation operator (-)
    Negate,
}

/// Represents the different types of content an AST node can contain.
#[derive(Debug, Clone)]
pub enum AstContent {
    /// A literal value
    Value(RuntimeValue),
    /// A binary operation with an operator and two operands
    BinOp {
        /// The binary operator
        op: Operator,
        /// The left operand
        left: Box<Ast>,
        /// The right operand
        right: Box<Ast>,
    },
    /// A unary operation with an operator and a single operand
    UnaryOp {
        /// The unary operator
        op: UnaryOperator,
        /// The operand
        expr: Box<Ast>,
    },
}

impl Ast {
    /// Creates a new AST node with the given content.
    pub fn new(content: AstContent) -> Self {
        Ast { content }
    }

    /// Creates a new value AST node.
    pub fn value(val: RuntimeValue) -> Self {
        Ast::new(AstContent::Value(val))
    }

    /// Creates a new binary operation AST node.
    pub fn binop(op: Operator, l: Ast, r: Ast) -> Self {
        Ast::new(AstContent::BinOp {
            op,
            left: Box::new(l),
            right: Box::new(r),
        })
    }

    /// Creates a new unary operation AST node.
    pub fn unary(op: UnaryOperator, expr: Ast) -> Self {
        Ast::new(AstContent::UnaryOp {
            op,
            expr: Box::new(expr),
        })
    }

    /// Gets the content of this AST node.
    pub fn content(&self) -> &AstContent {
        &self.content
    }

    #[allow(missing_docs)]
    pub fn add_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Plus, l, r)
    }
    #[allow(missing_docs)]
    pub fn substract_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Minus, l, r)
    }
    #[allow(missing_docs)]
    pub fn multiply_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Multiply, l, r)
    }
    #[allow(missing_docs)]
    pub fn divide_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Divide, l, r)
    }
    #[allow(missing_docs)]
    pub fn floordiv_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::DoubleSlash, l, r)
    }
    #[allow(missing_docs)]
    pub fn modulo_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Percent, l, r)
    }
    #[allow(missing_docs)]
    pub fn equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Equals, l, r)
    }
    #[allow(missing_docs)]
    pub fn not_equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::NotEquals, l, r)
    }
    #[allow(missing_docs)]
    pub fn greater_than_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::GreaterThan, l, r)
    }
    #[allow(missing_docs)]
    pub fn less_than_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::LessThan, l, r)
    }
    #[allow(missing_docs)]
    pub fn greater_than_or_equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::GreaterThanOrEquals, l, r)
    }
    #[allow(missing_docs)]
    pub fn less_than_or_equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::LessThanOrEquals, l, r)
    }
    #[allow(missing_docs)]
    pub fn bitwise_and_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::BitwiseAnd, l, r)
    }
    #[allow(missing_docs)]
    pub fn bitwise_or_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::BitwiseOr, l, r)
    }
    #[allow(missing_docs)]
    pub fn bitwise_xor_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::BitwiseXor, l, r)
    }
    #[allow(missing_docs)]
    pub fn left_shift_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::LeftShift, l, r)
    }
    #[allow(missing_docs)]
    pub fn right_shift_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::RightShift, l, r)
    }
    #[allow(missing_docs)]
    pub fn and_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::And, l, r)
    }
    #[allow(missing_docs)]
    pub fn or_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Or, l, r)
    }
    #[allow(missing_docs)]
    pub fn assign_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Assign, l, r)
    }

    #[allow(missing_docs)]
    pub fn bitwise_not_op(expr: Ast) -> Self {
        Ast::unary(UnaryOperator::BitwiseNot, expr)
    }

    #[allow(missing_docs)]
    pub fn negate_op(expr: Ast) -> Self {
        Ast::unary(UnaryOperator::Negate, expr)
    }
}
