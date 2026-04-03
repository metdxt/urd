//! # Urd - In-game Dialogue Scripting Language
//!
//! Urd is a scripting language designed specifically for in-game dialogues. It provides a simple yet powerful
//! syntax for defining dialogue trees, conditional logic, and interactive conversations in games.
//!
//! ## Quick Start
//!
//! ```rust,ignore
//! use urd::prelude::*;
//!
//! // 1. Parse + compile
//! let src = std::fs::read_to_string("dialogue.urd").unwrap();
//! let ast = urd::compiler::loader::parse_source(&src).unwrap();
//! let graph = urd::compiler::Compiler::compile(&ast).unwrap();
//!
//! // 2. Register decorators (optional)
//! let mut registry = DecoratorRegistry::new();
//! registry.register("color", |args| {
//!     let mut m = std::collections::HashMap::new();
//!     if let Some(urd::RuntimeValue::Str(s)) = args.first() {
//!         m.insert("color".into(), urd::RuntimeValue::Str(s.clone()));
//!     }
//!     m
//! });
//!
//! // 3. Run
//! let mut vm = Vm::new(graph, registry).unwrap();
//! loop {
//!     match vm.next(None) {
//!         urd::VmStep::Event(urd::Event::Dialogue { speakers, lines, .. }) => { /* render */ }
//!         urd::VmStep::Event(urd::Event::Choice { options, .. }) => { /* show menu, call vm.next(Some(idx)) */ }
//!         urd::VmStep::Ended => break,
//!         urd::VmStep::Error(e) => eprintln!("runtime error: {e}"),
//!     }
//! }
//! ```
//!
//! ## Modules
//!
//! - [`analysis`]: Static analysis passes (exhaustiveness, types, dead-end)
//! - [`erro`]: Error handling and reporting
//! - [`lexer`]: Tokenization of source code
//! - [`parser`]: Parsing tokens into an Abstract Syntax Tree (AST)
//! - [`runtime`]: Runtime evaluation and value management
//! - [`vm`]: Virtual machine and execution

#![feature(trait_alias)]

pub mod analysis;
pub mod compiler;
pub mod erro;
pub mod ir;
pub mod lexer;
pub mod loc;
pub mod parser;
pub mod runtime;
pub mod vm;

// ── Top-level re-exports for game engine integrators ─────────────────────────

/// The compiled IR event types that the VM emits.
pub use ir::{ChoiceEvent, Event};

/// The return type of [`Vm::next`] — replaces `Option<Result<Event, VmError>>`.
pub use ir::VmStep;

/// Runtime value type (what decorators receive and events contain).
pub use runtime::value::RuntimeValue;

/// The virtual machine that drives script execution.
pub use vm::Vm;

/// Error type returned by [`Vm::new`]; also carried inside [`VmStep::Error`].
pub use vm::VmError;

/// The decorator handler registry.
pub use vm::registry::DecoratorRegistry;

/// File loader trait and built-in implementations.
pub use vm::loader::{FileLoader, FsLoader, MemLoader};

/// Frontend-agnostic localisation trait — implement to plug translated text into the VM.
pub use loc::Localizer;

/// Compiler error type.
pub use compiler::CompilerError;

/// Convenience prelude — `use urd::prelude::*` brings in all the types a game
/// engine integration typically needs.
pub mod prelude {
    pub use super::{
        ChoiceEvent, CompilerError, DecoratorRegistry, Event, FileLoader, FsLoader, Localizer,
        MemLoader, RuntimeValue, Vm, VmError, VmStep,
    };
}
