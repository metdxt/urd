//! # Runtime Module
//!
//! This module contains the runtime components of the Urd language interpreter.
//! It provides the value system and evaluation mechanisms used during the
//! execution of parsed Urd scripts.
//!
//! ## Submodules
//!
//! - [`value`]: Value types and operations used at runtime
//! - [`extern_object`]: External object bridge for host game engine integration

pub mod extern_object;
pub mod value;
