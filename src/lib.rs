//! # Urd - In-game Dialogue Scripting Language
//!
//! Urd is a scripting language designed specifically for in-game dialogues. It provides a simple yet powerful
//! syntax for defining dialogue trees, conditional logic, and interactive conversations in games.
//!
//! ## Features
//!
//! - Expression evaluation with support for arithmetic, logical, and comparison operators
//! - String interpolation and formatting
//! - Dice rolling mechanics for RPG-style dialogues
//! - Variable management and assignment
//! - Conditional branching and control flow
//!
//! ## Modules
//!
//! - [`erro`]: Error handling and reporting
//! - [`lexer`]: Tokenization of source code
//! - [`parser`]: Parsing tokens into an Abstract Syntax Tree (AST)
//! - [`runtime`]: Runtime evaluation and value management

#![feature(type_alias_impl_trait)]

pub mod erro;
pub mod lexer;
pub mod parser;
pub mod runtime;
