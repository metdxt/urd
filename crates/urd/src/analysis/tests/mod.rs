//! Integration-style unit tests for the static analysis passes.
//!
//! Each sub-module covers one pass:
//! - [`dead_end_tests`]    — dead-end detection
//! - [`exhaustiveness_tests`] — match exhaustiveness
//! - [`types_tests`]       — type-annotation compatibility
//! - [`render_tests`]      — ariadne rendering with real parser spans

mod dead_end_tests;
mod exhaustiveness_tests;
mod render_tests;
mod types_tests;
