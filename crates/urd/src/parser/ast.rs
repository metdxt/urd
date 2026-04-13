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
//! - [`TypeAnnotation`]: Enum for optional static type annotations

use chumsky::span::{SimpleSpan, Span};

use crate::runtime::value::RuntimeValue;

/// Represents a node in the Abstract Syntax Tree.
#[derive(Debug, Clone)]
pub struct Ast {
    /// A list of decorators for event. Only makes sense for Block, LabeledBlock, Dialogue, Menu, MenuOption. Ignored otherwise.
    decorators: Vec<Decorator>,
    /// The content of this AST node
    content: AstContent,
    /// The source span this node was parsed from. Zero span when constructed outside the parser.
    span: SimpleSpan,
    /// Optional documentation comment (`## ...`) attached to this node.
    pub doc_comment: Option<String>,
}

impl PartialEq for Ast {
    /// Compares two AST nodes for structural equality, **ignoring source spans and doc comments**.
    ///
    /// Spans are source-location metadata injected by the parser; manually-constructed
    /// nodes (e.g. in tests) carry a zero span `0..0`, while parsed nodes carry real
    /// byte-offset spans. Excluding spans from equality makes test assertions robust
    /// to this difference. Doc comments are also excluded so that adding or removing
    /// documentation does not break structural equality checks.
    fn eq(&self, other: &Self) -> bool {
        self.decorators == other.decorators && self.content == other.content
    }
}

/// A decorator applied to an AST node, using Python-like `@name` or `@name(args)` syntax.
#[derive(Debug, Clone)]
pub struct Decorator {
    /// The decorator's name (a single-segment identifier).
    name: String,
    /// The argument list passed to the decorator. Always an [`AstContent::ExprList`] node,
    /// which is empty when the decorator is written without parentheses.
    args: Ast,
    /// Source span covering the entire decorator (from `@` through closing `)` if present).
    /// Zero span when constructed outside the parser.
    span: SimpleSpan,
}

impl PartialEq for Decorator {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
}

impl Decorator {
    /// Creates a new decorator with a name and an argument list (ExprList node).
    ///
    /// The `span` field is `0..0` when constructed outside the parser; the parser sets it via [`Decorator::with_span`].
    pub fn new(name: String, args: Ast) -> Self {
        Decorator {
            name,
            args,
            span: SimpleSpan::new((), 0..0),
        }
    }

    /// Creates a new decorator with no arguments.
    ///
    /// The `span` field is `0..0` when constructed outside the parser; the parser sets it via [`Decorator::with_span`].
    pub fn bare(name: String) -> Self {
        Decorator {
            name,
            args: Ast::expr_list(vec![]),
            span: SimpleSpan::new((), 0..0),
        }
    }

    /// Returns the decorator's name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the decorator's argument list (an ExprList node).
    pub fn args(&self) -> &Ast {
        &self.args
    }

    /// Returns the source span covering this decorator.
    pub fn span(&self) -> SimpleSpan {
        self.span
    }

    /// Set the source span on this decorator, returning `self` (builder).
    #[must_use]
    pub fn with_span(mut self, span: SimpleSpan) -> Self {
        self.span = span;
        self
    }
}

/// Represents binary operators in the Urd language.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// Exclusive range operator (..)
    RangeExclusive,
    /// Inclusive range operator (..=)
    RangeInclusive,
    /// Membership test operator (in)
    In,
}

/// Represents unary operators in the Urd language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    /// Bitwise NOT operator (!)
    BitwiseNot,
    /// Negation operator (-)
    Negate,
    /// Logical NOT operator (not)
    Not,
}

/// Data declaration modes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclKind {
    /// Globally available variable, typically represents the data game wants to preserve, global state.
    Global,
    /// Immutable data
    Constant,
    /// Scoped variable declared with `let`. Always creates a new binding in the
    /// innermost scope, shadowing any outer variable with the same name.
    Variable,
    /// Bare assignment (`x = value`). Searches outward through the scope chain
    /// for an existing binding and mutates it in-place. If no binding is found,
    /// creates a new one in the innermost scope (implicit declaration).
    ///
    /// This variant is **never** produced by the parser — it is synthesised by
    /// the compiler when lowering `BinOp { op: Assign }` nodes to IR.
    Assignment,
}

/// Represents an optional static type annotation on a variable declaration or parameter.
///
/// Type annotations use `: TypeName` syntax, e.g. `let x: int = 5`.
/// The annotation is purely informational at parse time; the runtime may use it for
/// type-checking or documentation purposes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnnotation {
    /// The `int` primitive type
    Int,
    /// The `float` primitive type
    Float,
    /// The `bool` primitive type
    Bool,
    /// The `str` primitive type
    Str,
    /// The `null` type
    Null,
    /// The `list` collection type
    List,
    /// The `map` collection type
    Map,
    /// The `dice` type (e.g. `2d6`)
    Dice,
    /// A named user-defined type, e.g. an enum variant path like `Direction`
    Named(Vec<String>),
    /// The `range` type (e.g. `0..5` or `0..=5`)
    Range,
}

/// The optional event-kind constraint on a decorator definition.
/// `decorator foo<event: dialogue>(...)` → `EventConstraint::Dialogue`
/// If the `<event: ...>` clause is absent, the decorator applies to any event.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EventConstraint {
    /// Constrains the decorator to dialogue events only
    Dialogue,
    /// Constrains the decorator to choice/menu events only
    Choice,
    /// No constraint — the decorator applies to any event kind
    Any,
}

/// A pattern in a match arm.
#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern {
    /// The `_` wildcard — matches anything.
    Wildcard,
    /// A literal or identifier path — matches by value equality.
    ///
    /// Covers: `IntLit`, `FloatLit`, `BoolLit`, `StrLit`, `Null`,
    /// `IdentPath` (enum variant or variable).
    ///
    /// When the scrutinee evaluates to a [`RuntimeValue::Roll`] (i.e. the
    /// result of a dice expression), the comparison is made against the
    /// **sum** of all individual die values.
    Value(Ast),
    /// A numeric range pattern — matches when the scrutinee falls within the
    /// given bounds.
    ///
    /// Syntax: `start..end` (exclusive upper bound) or `start..=end`
    /// (inclusive upper bound), with an optional `as name` binding that
    /// captures the matched scalar value inside the arm body.
    ///
    /// When the scrutinee evaluates to a [`RuntimeValue::Roll`] the **sum**
    /// of all dice is compared against the range.  For `Int` or `Float`
    /// scrutinees the value is compared directly.
    Range {
        /// Lower bound of the range (inclusive).
        start: Ast,
        /// Upper bound of the range.
        end: Ast,
        /// `true` when the upper bound is inclusive (`..=`), `false` for
        /// exclusive (`..`).
        inclusive: bool,
        /// Optional variable name to bind the matched scalar value to inside
        /// the arm body.
        binding: Option<String>,
    },
    /// An array pattern — matches individual die values element-by-element.
    ///
    /// Syntax: `[ val1, val2, ... ]`
    ///
    /// Requires the scrutinee to evaluate to a [`RuntimeValue::Roll`] with
    /// the **same number of elements** as the pattern array.  Each element
    /// is compared in order against the corresponding die result.
    Array(Vec<Option<Ast>>),
}

impl std::fmt::Display for MatchPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MatchPattern::Wildcard => write!(f, "_"),
            MatchPattern::Value(ast) => match ast.content() {
                AstContent::Value(RuntimeValue::Str(s)) => write!(f, "{s}"),
                AstContent::Value(RuntimeValue::IdentPath(p)) => write!(f, "{}", p.join(".")),
                AstContent::Value(RuntimeValue::Int(i)) => write!(f, "{i}"),
                AstContent::Value(RuntimeValue::Float(fv)) => write!(f, "{fv}"),
                AstContent::Value(RuntimeValue::Bool(b)) => write!(f, "{b}"),
                AstContent::Value(RuntimeValue::Null) => write!(f, "null"),
                _ => write!(f, "_"),
            },
            MatchPattern::Range {
                start,
                end,
                inclusive,
                binding,
            } => {
                let sep = if *inclusive { "..=" } else { ".." };
                match start.content() {
                    AstContent::Value(RuntimeValue::Int(i)) => write!(f, "{i}")?,
                    _ => write!(f, "_")?,
                }
                write!(f, "{sep}")?;
                match end.content() {
                    AstContent::Value(RuntimeValue::Int(i)) => write!(f, "{i}")?,
                    _ => write!(f, "_")?,
                }
                if let Some(name) = binding {
                    write!(f, " as {name}")?;
                }
                Ok(())
            }
            MatchPattern::Array(elems) => {
                write!(f, "[")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    match elem {
                        None => write!(f, "_")?,
                        Some(ast) => match ast.content() {
                            AstContent::Value(RuntimeValue::Int(v)) => write!(f, "{v}")?,
                            _ => write!(f, "_")?,
                        },
                    }
                }
                write!(f, "]")
            }
        }
    }
}

/// A single named parameter in a `decorator` definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecoratorParam {
    /// The parameter name (plain identifier)
    pub name: String,
    /// Optional static type annotation (informational only)
    pub type_annotation: Option<TypeAnnotation>,
    /// The byte span of the parameter in source code.
    pub span: chumsky::span::SimpleSpan,
}

/// A single parameter in a [`AstContent::FnDef`] parameter list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnParam {
    /// The parameter name.
    pub name: String,
    /// Optional type annotation (stripped at compile time; documentation only).
    pub type_annotation: Option<TypeAnnotation>,
    /// The byte span of the parameter in source code.
    pub span: chumsky::span::SimpleSpan,
}

/// A sub-token source span used solely for IDE syntax highlighting.
///
/// Always compares equal under [`PartialEq`] so that structural AST equality
/// remains span-insensitive: two nodes with identical content but different
/// source positions are considered equal, which is the correct semantics for
/// structural analysis passes (e.g. duplicate-menu-destination detection).
///
/// Access the inner [`SimpleSpan`] via the public `.0` field.
#[derive(Debug, Clone, Copy)]
pub struct TokSpan(pub SimpleSpan);

impl Default for TokSpan {
    fn default() -> Self {
        Self(SimpleSpan::new((), 0..0))
    }
}

impl PartialEq for TokSpan {
    /// Always returns `true` — sub-token spans are excluded from structural equality.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl From<SimpleSpan> for TokSpan {
    fn from(s: SimpleSpan) -> Self {
        Self(s)
    }
}

/// A single field in a `struct` definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    /// The field name (plain identifier)
    pub name: String,
    /// The source span of the field name token.
    pub span: TokSpan,
    /// The field's required type annotation
    pub type_annotation: TypeAnnotation,
}

/// A single arm in a match statement.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    /// The pattern to match against
    pub pattern: MatchPattern,
    /// The block to execute when matched (always AstContent::Block)
    pub body: Ast,
}

impl MatchArm {
    /// Creates a new match arm.
    pub fn new(pattern: MatchPattern, body: Ast) -> Self {
        MatchArm { pattern, body }
    }
}

/// A single symbol import specifier: `symbol_name as local_alias` or just `symbol_name`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportSymbol {
    /// The original name in the source module. `None` means the whole module is imported.
    pub original: Option<String>,
    /// The local alias to use in the importing file.
    pub alias: String,
}

/// Represents the different types of content an AST node can contain.
#[derive(Debug, Clone, PartialEq)]
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

    /// Comma separated list of expressions
    ExprList(Vec<Ast>),

    /// Variable/const declaration (global/let/const ... = ...)
    Declaration {
        /// Kind of declaration (const, global, let keywords)
        kind: DeclKind,
        /// Name(s) of variables/consts being declared, typically identifier(s)
        decl_name: Box<Ast>,
        /// Optional static type annotation (`: TypeName`) placed between the name and `=`
        type_annotation: Option<TypeAnnotation>,
        /// Definitions. Typically expressions to be computed
        decl_defs: Box<Ast>,
    },

    /// An `extern const` declaration: value provided by the host runtime.
    ///
    /// An `extern` declaration: a value provided by the host runtime.
    ///
    /// Syntax: `extern name` or `extern name: Type`
    ///
    /// The type annotation is optional but recommended for linting and autocompletion.
    /// There is no initialiser — the runtime injects the value before execution via
    /// [`crate::vm::env::Environment::provide_extern`].
    ///
    /// Extern values are controlled entirely by the runtime: they are neither
    /// save/loadable game state nor guaranteed to be constant. Scripts may not
    /// reassign them.
    ExternDeclaration {
        /// The variable name (a single-segment [`AstContent::Value(RuntimeValue::IdentPath)`]).
        name: Box<Ast>,
        /// Optional type hint used by static analysis and autocompletion.
        type_annotation: Option<TypeAnnotation>,
    },

    /// A call to function
    Call {
        /// Function to call, typically Value with identifier node
        func_path: Box<Ast>,
        /// Parameters for the function, ExprList
        params: Box<Ast>,
    },

    /// Block of code. Contains statements following one each other in chronological order
    Block(Vec<Ast>),

    /// List construction [a, b, c]
    List(Vec<Ast>),

    /// Map construction :{ key: value, key2: value2 }
    Map(Vec<(Ast, Ast)>),

    /// If statement
    If {
        /// Condition expression
        condition: Box<Ast>,
        /// Block to execute if condition is true
        then_block: Box<Ast>,
        /// Optional else block (or elif chain)
        else_block: Option<Box<Ast>>,
    },

    /// Labeled block: label ident { ... }
    LabeledBlock {
        /// Label identifier
        label: String,
        /// Source span of the label name token.
        label_span: TokSpan,
        /// The block of code
        block: Box<Ast>,
    },

    /// Dialogue line(s)
    Dialogue {
        /// Character(s) speaking, usually an ExprList of identifiers or expressions
        speakers: Box<Ast>,
        /// The content being said, usually an ExprList of strings or a single string
        content: Box<Ast>,
    },

    /// Menu with options for user selection
    Menu {
        /// List of menu options, each with a label and an associated code block
        options: Vec<Ast>,
    },

    /// A single selectable option within a [`AstContent::Menu`].
    MenuOption {
        /// The display label shown to the user for this option.
        label: String,
        /// The block of code executed when this option is chosen.
        content: Box<Ast>,
        /// Whether this is a wildcard/default option (`_`).
        is_default: bool,
    },

    /// Return statement with optional value to return
    Return {
        /// Optional expression to return
        value: Option<Box<Ast>>,
    },

    /// Jump statement to transfer control to a labeled block
    Jump {
        /// Label identifier to jump to
        label: String,
        /// Source span of the label identifier token.
        label_span: TokSpan,
        /// If true, push a call frame so we can `return` back here.
        expects_return: bool,
    },

    /// Subroutine call with result binding: `let name = jump label and return`
    LetCall {
        /// Variable to store the return value in.
        name: String,
        /// Source span of the bound variable name token.
        name_span: TokSpan,
        /// Target label to jump to.
        target: String,
        /// Source span of the target label token.
        target_span: TokSpan,
    },

    /// Enum declaration: enum Foo { A, B, C }
    EnumDecl {
        /// The enum's name (e.g. "Direction")
        name: String,
        /// Ordered list of variant names with their source spans.
        variants: Vec<(String, TokSpan)>,
    },

    /// Struct declaration: `struct Name { field: Type ... }`
    StructDecl {
        /// The struct's name (e.g. "Player")
        name: String,
        /// Ordered list of field definitions
        fields: Vec<StructField>,
    },

    /// Match statement: match expr { pattern { ... } ... }
    Match {
        /// The expression being matched
        scrutinee: Box<Ast>,
        /// Ordered list of match arms
        arms: Vec<MatchArm>,
    },

    /// A function definition (named or anonymous).
    ///
    /// - Named: `fn foo(x: int, y: str) -> int { x + 1 }` — introduces `foo` into scope as a
    ///   `RuntimeValue::Function`. Compiled to `IrNodeKind::DefineFunction`.
    /// - Anonymous: `fn(x: int) -> int { x + 1 }` — an expression that evaluates to
    ///   `RuntimeValue::Function`. Can be passed as a value (e.g. to `list.map`).
    ///
    /// # Purity
    /// Function bodies run in an isolated environment containing only the bound
    /// parameters — no access to outer scope variables.
    FnDef {
        /// `Some(name)` for a named function declaration; `None` for an anonymous
        /// function expression.
        name: Option<String>,
        /// Source span of the function name token, or `None` for anonymous functions.
        name_span: Option<TokSpan>,
        /// Ordered parameter list.
        params: Vec<FnParam>,
        /// Optional return-type annotation (documentation only; not enforced at runtime).
        ret_type: Option<TypeAnnotation>,
        /// The function body block.
        body: Box<Ast>,
    },

    /// Definition of a script-level decorator: `decorator name<event: kind>(params) { body }`
    DecoratorDef {
        /// The decorator's name
        name: String,
        /// Optional event-kind constraint
        event_constraint: EventConstraint,
        /// Ordered list of named parameters
        params: Vec<DecoratorParam>,
        /// The body block (always AstContent::Block)
        body: Box<Ast>,
    },

    /// Subscript read: `expr[key]`
    Subscript {
        /// The object being indexed
        object: Box<Ast>,
        /// The index/key expression
        key: Box<Ast>,
    },

    /// Subscript assignment: `ident[key] = value`
    ///
    /// The parser restricts the left-hand side to a single-segment identifier
    /// (e.g. `map["key"] = value`). Multi-segment paths and arbitrary expressions
    /// are not supported as the subscript base.
    SubscriptAssign {
        /// The object being indexed
        object: Box<Ast>,
        /// The index/key expression
        key: Box<Ast>,
        /// The value to assign
        value: Box<Ast>,
    },

    /// Import another script file or specific symbols from it.
    ///
    /// - Whole-module form: `import "path/to/file" as module_name`
    /// - Single-symbol form: `import symbol as alias from "path"`
    /// - Multi-symbol form:  `import (sym1 as a1, sym2) from "path"`
    Import {
        /// The raw file path string as written in source.
        path: String,
        /// The list of symbols to import. Each entry is `(original_name, local_alias)`.
        /// For the whole-module form `import "path" as alias`, this will be a single entry
        /// `(None, alias)` where `None` means "the whole module".
        /// For symbol imports each entry carries the original name and its local alias.
        symbols: Vec<ImportSymbol>,
    },
}

#[allow(missing_docs)]
impl Ast {
    /// Creates a new AST node with the given content and a zero span.
    pub fn new(content: AstContent) -> Self {
        Ast {
            content,
            decorators: vec![],
            span: SimpleSpan::new((), 0..0),
            doc_comment: None,
        }
    }

    /// Creates a new AST node with the given content and decorators, and a zero span.
    pub fn new_decorated(content: AstContent, decorators: Vec<Decorator>) -> Self {
        Ast {
            content,
            decorators,
            span: SimpleSpan::new((), 0..0),
            doc_comment: None,
        }
    }

    /// Attaches decorators to this AST node, replacing any existing ones.
    #[must_use]
    pub fn with_decorators(mut self, decorators: Vec<Decorator>) -> Self {
        self.decorators = decorators;
        self
    }

    /// Attaches a documentation comment to this AST node, returning `self`.
    #[must_use]
    pub fn with_doc_comment(mut self, doc: String) -> Self {
        self.doc_comment = Some(doc);
        self
    }

    /// Replaces the span on this node, returning `self`. Used by parsers after construction.
    #[must_use]
    pub fn with_span(mut self, span: SimpleSpan) -> Self {
        self.span = span;
        self
    }

    /// Set the label-name span on a [`AstContent::LabeledBlock`] node (builder).
    ///
    /// Has no effect if called on a non-`LabeledBlock` node.
    #[must_use]
    pub fn with_label_span(mut self, label_span: SimpleSpan) -> Self {
        debug_assert!(
            matches!(self.content, AstContent::LabeledBlock { .. }),
            "with_label_span called on non-LabeledBlock node"
        );
        if let AstContent::LabeledBlock {
            label_span: ref mut ls,
            ..
        } = self.content
        {
            *ls = TokSpan(label_span);
        }
        self
    }

    /// Set the label span on a [`AstContent::Jump`] node (builder).
    ///
    /// Has no effect on non-`Jump` nodes.
    #[must_use]
    pub fn with_jump_label_span(mut self, label_span: SimpleSpan) -> Self {
        debug_assert!(
            matches!(self.content, AstContent::Jump { .. }),
            "with_jump_label_span called on non-Jump node"
        );
        if let AstContent::Jump {
            label_span: ref mut ls,
            ..
        } = self.content
        {
            *ls = TokSpan(label_span);
        }
        self
    }

    /// Set the name and target spans on a [`AstContent::LetCall`] node (builder).
    ///
    /// Has no effect on non-`LetCall` nodes.
    #[must_use]
    pub fn with_let_call_spans(mut self, name_span: SimpleSpan, target_span: SimpleSpan) -> Self {
        debug_assert!(
            matches!(self.content, AstContent::LetCall { .. }),
            "with_let_call_spans called on non-LetCall node"
        );
        if let AstContent::LetCall {
            name_span: ref mut ns,
            target_span: ref mut ts,
            ..
        } = self.content
        {
            *ns = TokSpan(name_span);
            *ts = TokSpan(target_span);
        }
        self
    }

    /// Set the function-name span on a [`AstContent::FnDef`] node (builder).
    ///
    /// Has no effect on non-`FnDef` nodes.
    #[must_use]
    pub fn with_fn_name_span(mut self, name_span: Option<SimpleSpan>) -> Self {
        debug_assert!(
            matches!(self.content, AstContent::FnDef { .. }),
            "with_fn_name_span called on non-FnDef node"
        );
        if let AstContent::FnDef {
            name_span: ref mut ns,
            ..
        } = self.content
        {
            *ns = name_span.map(TokSpan);
        }
        self
    }

    /// Returns the source span this node was parsed from.
    pub fn span(&self) -> SimpleSpan {
        self.span
    }

    /// Returns the decorators attached to this AST node.
    pub fn decorators(&self) -> &[Decorator] {
        &self.decorators
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

    pub fn add_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Plus, l, r)
    }
    pub fn subtract_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Minus, l, r)
    }
    pub fn multiply_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Multiply, l, r)
    }
    pub fn divide_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Divide, l, r)
    }
    pub fn floordiv_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::DoubleSlash, l, r)
    }
    pub fn modulo_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Percent, l, r)
    }
    pub fn equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Equals, l, r)
    }
    pub fn not_equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::NotEquals, l, r)
    }
    pub fn greater_than_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::GreaterThan, l, r)
    }
    pub fn less_than_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::LessThan, l, r)
    }
    pub fn greater_than_or_equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::GreaterThanOrEquals, l, r)
    }
    pub fn less_than_or_equals_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::LessThanOrEquals, l, r)
    }
    pub fn bitwise_and_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::BitwiseAnd, l, r)
    }
    pub fn bitwise_or_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::BitwiseOr, l, r)
    }
    pub fn bitwise_xor_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::BitwiseXor, l, r)
    }
    pub fn left_shift_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::LeftShift, l, r)
    }
    pub fn right_shift_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::RightShift, l, r)
    }
    pub fn and_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::And, l, r)
    }
    pub fn or_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Or, l, r)
    }
    pub fn not_op(expr: Ast) -> Self {
        Ast::unary(UnaryOperator::Not, expr)
    }
    pub fn assign_op(l: Ast, r: Ast) -> Self {
        Ast::binop(Operator::Assign, l, r)
    }
    /// Creates a `BinOp` node with `Operator::RangeExclusive`.
    pub fn range_exclusive_op(left: Ast, right: Ast) -> Ast {
        Ast::binop(Operator::RangeExclusive, left, right)
    }
    /// Creates a `BinOp` node with `Operator::RangeInclusive`.
    pub fn range_inclusive_op(left: Ast, right: Ast) -> Ast {
        Ast::binop(Operator::RangeInclusive, left, right)
    }
    /// Creates a `BinOp` node with `Operator::In`.
    pub fn in_op(left: Ast, right: Ast) -> Ast {
        Ast::binop(Operator::In, left, right)
    }

    pub fn bitwise_not_op(expr: Ast) -> Self {
        Ast::unary(UnaryOperator::BitwiseNot, expr)
    }

    pub fn negate_op(expr: Ast) -> Self {
        Ast::unary(UnaryOperator::Negate, expr)
    }

    /// Create declaration type node without a type annotation.
    pub fn decl(kind: DeclKind, name: Ast, def: Ast) -> Self {
        Ast::new(AstContent::Declaration {
            kind,
            decl_name: Box::new(name),
            type_annotation: None,
            decl_defs: Box::new(def),
        })
    }

    /// Create an extern declaration node.
    pub fn extern_decl(name: Ast, type_annotation: Option<TypeAnnotation>) -> Self {
        Ast::new(AstContent::ExternDeclaration {
            name: Box::new(name),
            type_annotation,
        })
    }

    /// Create declaration type node with an explicit type annotation.
    pub fn typed_decl(kind: DeclKind, name: Ast, annotation: TypeAnnotation, def: Ast) -> Self {
        Ast::new(AstContent::Declaration {
            kind,
            decl_name: Box::new(name),
            type_annotation: Some(annotation),
            decl_defs: Box::new(def),
        })
    }

    /// Create comma-separated list of expressions
    pub fn expr_list(exprs: Vec<Ast>) -> Self {
        Self::new(AstContent::ExprList(exprs))
    }

    /// Create function call node, accept two AST nodes,
    /// one for function path (typically Value(RuntimeValue::IdentPath) node),
    /// other for params (usually, comma-separated list of expressions, ExprList)
    pub fn call(func_path: Ast, params: Ast) -> Self {
        Self::new(AstContent::Call {
            func_path: Box::new(func_path),
            params: Box::new(params),
        })
    }

    /// Create block of code from statements
    pub fn block(statements: Vec<Ast>) -> Self {
        Self::new(AstContent::Block(statements))
    }

    /// Create list
    pub fn list(items: Vec<Ast>) -> Self {
        Self::new(AstContent::List(items))
    }

    /// Create map
    pub fn map(items: Vec<(Ast, Ast)>) -> Self {
        Self::new(AstContent::Map(items))
    }

    /// Create if statement
    pub fn if_stmt(condition: Ast, then_block: Ast, else_block: Option<Ast>) -> Self {
        Self::new(AstContent::If {
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block: else_block.map(Box::new),
        })
    }

    /// Create labeled block
    pub fn labeled_block(label: String, block: Ast) -> Self {
        Self::new(AstContent::LabeledBlock {
            label,
            label_span: TokSpan::default(),
            block: Box::new(block),
        })
    }

    /// Create dialogue node
    pub fn dialogue(speakers: Ast, content: Ast) -> Self {
        Self::new(AstContent::Dialogue {
            speakers: Box::new(speakers),
            content: Box::new(content),
        })
    }

    /// Create menu node
    pub fn menu(options: Vec<Ast>) -> Self {
        Self::new(AstContent::Menu { options })
    }

    /// Create a single menu option node
    pub fn menu_option(label: String, content: Ast, is_default: bool) -> Self {
        Self::new(AstContent::MenuOption {
            label,
            content: Box::new(content),
            is_default,
        })
    }

    /// Create a default/wildcard menu option node (`_ { ... }`)
    pub fn menu_default_option(content: Ast) -> Self {
        Self::new(AstContent::MenuOption {
            label: "_".to_string(),
            content: Box::new(content),
            is_default: true,
        })
    }

    /// Create return statement node
    pub fn return_stmt(value: Option<Ast>) -> Self {
        Self::new(AstContent::Return {
            value: value.map(Box::new),
        })
    }

    /// Create jump statement node
    pub fn jump_stmt(label: String, expects_return: bool) -> Self {
        Self::new(AstContent::Jump {
            label,
            label_span: TokSpan::default(),
            expects_return,
        })
    }

    /// Create a subroutine call with result binding node (`let name = jump target and return`)
    pub fn let_call(name: String, target: String) -> Self {
        Self::new(AstContent::LetCall {
            name,
            name_span: TokSpan::default(),
            target,
            target_span: TokSpan::default(),
        })
    }

    /// Create an enum declaration node
    pub fn enum_decl(name: String, variants: Vec<(String, TokSpan)>) -> Self {
        Self::new(AstContent::EnumDecl { name, variants })
    }

    /// Create a struct declaration node
    pub fn struct_decl(name: String, fields: Vec<StructField>) -> Self {
        Self::new(AstContent::StructDecl { name, fields })
    }

    /// Create a match statement node
    pub fn match_stmt(scrutinee: Ast, arms: Vec<MatchArm>) -> Self {
        Self::new(AstContent::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    /// Create a decorator definition node
    pub fn decorator_def(
        name: String,
        event_constraint: EventConstraint,
        params: Vec<DecoratorParam>,
        body: Ast,
    ) -> Self {
        Self::new(AstContent::DecoratorDef {
            name,
            event_constraint,
            params,
            body: Box::new(body),
        })
    }

    /// Construct a [`AstContent::FnDef`] node.
    pub fn fn_def(
        name: Option<String>,
        params: Vec<FnParam>,
        ret_type: Option<TypeAnnotation>,
        body: Ast,
    ) -> Self {
        Self::new(AstContent::FnDef {
            name,
            name_span: None,
            params,
            ret_type,
            body: Box::new(body),
        })
    }

    /// Create a subscript-read node (`obj[key]`)
    pub fn subscript(object: Ast, key: Ast) -> Self {
        Self::new(AstContent::Subscript {
            object: Box::new(object),
            key: Box::new(key),
        })
    }

    /// Create a subscript-assign node (`obj[key] = value`)
    pub fn subscript_assign(object: Ast, key: Ast, value: Ast) -> Self {
        Self::new(AstContent::SubscriptAssign {
            object: Box::new(object),
            key: Box::new(key),
            value: Box::new(value),
        })
    }

    /// Creates an `Import` node with an explicit symbol list.
    pub fn import(path: String, symbols: Vec<ImportSymbol>) -> Self {
        Self::new(AstContent::Import { path, symbols })
    }

    /// Convenience constructor for the whole-module form: `import "path" as alias`.
    ///
    /// Produces a single `ImportSymbol { original: None, alias }` entry.
    pub fn import_module(path: String, alias: String) -> Self {
        Self::new(AstContent::Import {
            path,
            symbols: vec![ImportSymbol {
                original: None,
                alias,
            }],
        })
    }

    /// Returns references to all direct child [`Ast`] nodes contained in this node.
    ///
    /// This provides a single canonical enumeration of children, eliminating the
    /// need for each analysis pass to hand-roll the same `match` over [`AstContent`]
    /// variants.
    pub fn children(&self) -> Vec<&Ast> {
        match &self.content {
            AstContent::Value(_)
            | AstContent::Jump { .. }
            | AstContent::LetCall { .. }
            | AstContent::Import { .. }
            | AstContent::EnumDecl { .. }
            | AstContent::StructDecl { .. } => vec![],

            AstContent::BinOp { left, right, .. } => vec![left, right],
            AstContent::UnaryOp { expr, .. } => vec![expr],

            AstContent::ExprList(items) | AstContent::Block(items) | AstContent::List(items) => {
                items.iter().collect()
            }

            AstContent::Declaration {
                decl_name,
                decl_defs,
                ..
            } => vec![decl_name, decl_defs],

            AstContent::ExternDeclaration { name, .. } => vec![name],

            AstContent::Call { func_path, params } => vec![func_path, params],

            AstContent::Map(pairs) => pairs.iter().flat_map(|(k, v)| [k, v]).collect(),

            AstContent::If {
                condition,
                then_block,
                else_block,
            } => {
                let mut c = vec![condition.as_ref(), then_block.as_ref()];
                if let Some(eb) = else_block {
                    c.push(eb);
                }
                c
            }

            AstContent::LabeledBlock { block, .. } => vec![block],

            AstContent::Dialogue { speakers, content } => vec![speakers, content],

            AstContent::Menu { options } => options.iter().collect(),

            AstContent::MenuOption { content, .. } => vec![content],

            AstContent::Return { value } => value.iter().map(|v| v.as_ref()).collect(),

            AstContent::Match { scrutinee, arms } => {
                let mut c = vec![scrutinee.as_ref()];
                for arm in arms {
                    match &arm.pattern {
                        MatchPattern::Value(v) => c.push(v),
                        MatchPattern::Array(elems) => {
                            for ast in elems.iter().flatten() {
                                c.push(ast);
                            }
                        }
                        MatchPattern::Range { start, end, .. } => {
                            c.push(start);
                            c.push(end);
                        }
                        MatchPattern::Wildcard => {}
                    }
                    c.push(&arm.body);
                }
                c
            }

            AstContent::FnDef { body, .. } => vec![body.as_ref()],

            AstContent::DecoratorDef { body, .. } => vec![body.as_ref()],

            AstContent::Subscript { object, key } => vec![object, key],

            AstContent::SubscriptAssign { object, key, value } => {
                vec![object, key, value]
            }
        }
    }
}

/// Walks the AST tree depth-first (pre-order), calling `visitor` on each node.
///
/// Uses an explicit stack instead of recursion to avoid stack overflow on
/// deeply nested AST trees.
pub fn walk_ast<F: FnMut(&Ast)>(node: &Ast, visitor: &mut F) {
    let mut stack: Vec<&Ast> = vec![node];
    while let Some(current) = stack.pop() {
        visitor(current);
        // Push children in reverse order so that the first child is processed first
        // (since we pop from the end of the stack).
        let children = current.children();
        for child in children.iter().rev() {
            stack.push(child);
        }
    }
}

/// Returns the maximum nesting depth of the AST tree.
///
/// Used for post-parse validation to reject inputs that would
/// cause stack overflows in the compiler.
pub fn ast_depth(node: &Ast) -> usize {
    let mut max_depth: usize = 0;
    let mut stack: Vec<(&Ast, usize)> = vec![(node, 1)];
    while let Some((current, depth)) = stack.pop() {
        if depth > max_depth {
            max_depth = depth;
        }
        for child in current.children().iter().rev() {
            stack.push((child, depth + 1));
        }
    }
    max_depth
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::value::RuntimeValue;

    /// Helper: the simplest possible leaf node (no children).
    fn leaf() -> Ast {
        Ast::value(RuntimeValue::Null)
    }

    // ── walk_ast ─────────────────────────────────────────────────────

    #[test]
    fn walk_ast_single_leaf() {
        let node = leaf();
        let mut count = 0;
        walk_ast(&node, &mut |_| count += 1);
        assert_eq!(count, 1);
    }

    #[test]
    fn walk_ast_block_visits_all_children() {
        let block = Ast::block(vec![leaf(), leaf(), leaf()]);
        let mut count = 0;
        walk_ast(&block, &mut |_| count += 1);
        // block itself + 3 children = 4
        assert_eq!(count, 4);
    }

    #[test]
    fn walk_ast_nested_blocks() {
        // outer -> [inner -> [leaf]]
        let inner = Ast::block(vec![leaf()]);
        let outer = Ast::block(vec![inner]);
        let mut count = 0;
        walk_ast(&outer, &mut |_| count += 1);
        // outer + inner + leaf = 3
        assert_eq!(count, 3);
    }

    #[test]
    fn walk_ast_pre_order_visits_parent_first() {
        let child = leaf();
        let block = Ast::block(vec![child]);
        let mut order = vec![];
        walk_ast(&block, &mut |node| {
            order.push(matches!(node.content(), AstContent::Block(_)));
        });
        // Parent (Block=true) should come before child (Block=false)
        assert_eq!(order, vec![true, false]);
    }

    #[test]
    fn walk_ast_if_stmt_visits_all_branches() {
        let cond = leaf();
        let then_block = Ast::block(vec![leaf()]);
        let else_block = Ast::block(vec![leaf()]);
        let if_node = Ast::if_stmt(cond, then_block, Some(else_block));

        let mut count = 0;
        walk_ast(&if_node, &mut |_| count += 1);
        // if_node + cond + then_block + then_child + else_block + else_child = 6
        assert_eq!(count, 6);
    }

    #[test]
    fn walk_ast_if_stmt_without_else() {
        let cond = leaf();
        let then_block = Ast::block(vec![leaf()]);
        let if_node = Ast::if_stmt(cond, then_block, None);

        let mut count = 0;
        walk_ast(&if_node, &mut |_| count += 1);
        // if_node + cond + then_block + then_child = 4
        assert_eq!(count, 4);
    }

    #[test]
    fn walk_ast_binop() {
        let binop = Ast::add_op(leaf(), leaf());
        let mut count = 0;
        walk_ast(&binop, &mut |_| count += 1);
        // binop + left + right = 3
        assert_eq!(count, 3);
    }

    #[test]
    fn walk_ast_unary_op() {
        let unary = Ast::negate_op(leaf());
        let mut count = 0;
        walk_ast(&unary, &mut |_| count += 1);
        // unary + operand = 2
        assert_eq!(count, 2);
    }

    #[test]
    fn walk_ast_empty_block() {
        let block = Ast::block(vec![]);
        let mut count = 0;
        walk_ast(&block, &mut |_| count += 1);
        // Only the block itself
        assert_eq!(count, 1);
    }

    #[test]
    fn walk_ast_children_visited_left_to_right() {
        let a = Ast::value(RuntimeValue::Int(1));
        let b = Ast::value(RuntimeValue::Int(2));
        let c = Ast::value(RuntimeValue::Int(3));
        let block = Ast::block(vec![a, b, c]);

        let mut values = vec![];
        walk_ast(&block, &mut |node| {
            if let AstContent::Value(RuntimeValue::Int(n)) = node.content() {
                values.push(*n);
            }
        });
        assert_eq!(values, vec![1, 2, 3]);
    }

    // ── ast_depth ────────────────────────────────────────────────────

    #[test]
    fn ast_depth_single_leaf() {
        assert_eq!(ast_depth(&leaf()), 1);
    }

    #[test]
    fn ast_depth_flat_block() {
        let block = Ast::block(vec![leaf(), leaf(), leaf()]);
        // block (depth 1) -> each child (depth 2)
        assert_eq!(ast_depth(&block), 2);
    }

    #[test]
    fn ast_depth_nested_blocks() {
        // inner block at depth 3
        let inner = Ast::block(vec![leaf()]);
        let middle = Ast::block(vec![inner]);
        let outer = Ast::block(vec![middle]);
        // outer(1) -> middle(2) -> inner(3) -> leaf(4)
        assert_eq!(ast_depth(&outer), 4);
    }

    #[test]
    fn ast_depth_asymmetric_tree() {
        // Left branch is deeper than right
        let deep = Ast::block(vec![Ast::block(vec![leaf()])]);
        let shallow = leaf();
        let root = Ast::block(vec![deep, shallow]);
        // root(1) -> deep(2) -> inner(3) -> leaf(4) is the deepest path
        assert_eq!(ast_depth(&root), 4);
    }

    #[test]
    fn ast_depth_empty_block() {
        let block = Ast::block(vec![]);
        // Just the block itself
        assert_eq!(ast_depth(&block), 1);
    }

    #[test]
    fn ast_depth_binop() {
        let binop = Ast::add_op(leaf(), leaf());
        // binop(1) -> operand(2)
        assert_eq!(ast_depth(&binop), 2);
    }

    #[test]
    fn ast_depth_if_stmt_with_else() {
        // condition is a leaf, then/else each wrap a leaf in a block
        let cond = leaf();
        let then_block = Ast::block(vec![leaf()]);
        let else_block = Ast::block(vec![Ast::block(vec![leaf()])]);
        let if_node = Ast::if_stmt(cond, then_block, Some(else_block));
        // if(1) -> else_block(2) -> inner_block(3) -> leaf(4)
        assert_eq!(ast_depth(&if_node), 4);
    }

    #[test]
    fn ast_depth_deeply_nested() {
        // Build a chain: block -> block -> ... -> leaf  (10 levels of blocks + 1 leaf)
        let mut node = leaf();
        for _ in 0..10 {
            node = Ast::block(vec![node]);
        }
        assert_eq!(ast_depth(&node), 11);
    }
}
