//! # VM Module
//!
//! This module implements the Urd virtual machine — the runtime that walks a
//! compiled [`IrGraph`] and emits [`Event`]s for the consumer (game engine,
//! test harness, etc.) to handle.
//!
//! ## Entry points
//!
//! - [`Vm::new`] — create a VM from a compiled [`IrGraph`] and a
//!   [`DecoratorRegistry`]; performs a validation pass over all decorators.
//! - [`Vm::next`] — advance execution by one *observable* step, returning
//!   the next [`Event`] (or `None` when the script ends).
//!
//! ## Architecture
//!
//! The VM is a simple interpreter loop over a petgraph [`StableGraph`].
//! Internal nodes (branches, assignments, jumps …) are consumed silently;
//! only [`IrNodeKind::Dialogue`] and [`IrNodeKind::Choice`] surface as
//! [`Event`]s.  The caller drives the loop by repeatedly calling
//! [`Vm::next`], supplying `choice: Some(idx)` only when responding to a
//! [`Event::Choice`].

pub mod env;
pub mod eval;
mod float_methods;
mod int_methods;
mod list_methods;
pub mod loader;
mod map_methods;
pub mod registry;
mod str_methods;

pub use env::{CallFrame, Environment};
pub use eval::{eval_expr, eval_expr_list};
pub use registry::DecoratorRegistry;

use std::collections::{HashMap, HashSet};

use petgraph::Direction;
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;
use thiserror::Error;

use crate::{
    compiler::CompilerError,
    ir::{ChoiceEvent, Event, IrChoiceOption, IrEdge, IrGraph, IrNodeKind, VmStep},
    parser::ast::{AstContent, DeclKind, Decorator, EventConstraint, MatchPattern},
    runtime::value::RuntimeValue,
};

use self::eval::{is_truthy, values_equal};

// ─── Graph helpers ────────────────────────────────────────────────────────────

/// Returns the single `Next`-edge successor of `idx`, or `None` if there is
/// no such edge (i.e. the node is a terminal or its continuation is absent).
fn next_of(graph: &IrGraph, idx: NodeIndex) -> Option<NodeIndex> {
    graph
        .graph
        .edges_directed(idx, Direction::Outgoing)
        .find(|e| matches!(e.weight(), IrEdge::Next))
        .map(|e| e.target())
}

// ─── Error type ───────────────────────────────────────────────────────────────

/// Errors that can occur during VM construction or execution.
#[derive(Debug, Error)]
pub enum VmError {
    /// A decorator name used in the script was not registered in the
    /// [`DecoratorRegistry`].
    #[error("unknown decorator '@{name}' used at node {node_id:?}")]
    UnknownDecorator {
        /// The unregistered decorator name.
        name: String,
        /// The IR node at which the unknown decorator appeared.
        node_id: NodeIndex,
    },

    /// A `jump` targeted a label not present in the compiled graph.
    ///
    /// Should never occur after a successful compilation pass, but is kept as
    /// a defensive runtime error.
    #[error("unknown label '{0}'")]
    UnknownLabel(String),

    /// A variable was referenced before it was declared.
    #[error("undefined variable '{0}'")]
    UndefinedVariable(String),

    /// A runtime type mismatch (e.g. adding a string to an integer).
    #[error("type error: {0}")]
    TypeError(String),

    /// An AST node appeared in an expression context where it is not valid.
    #[error("invalid expression context: {0}")]
    InvalidExpression(String),

    /// The player supplied a choice index that is out of bounds.
    #[error("choice index {index} out of bounds (len={len})")]
    ChoiceOutOfBounds {
        /// The supplied index.
        index: usize,
        /// The number of available options.
        len: usize,
    },

    /// A list index was out of bounds.
    #[error("index {index} out of bounds for list of length {len}")]
    IndexOutOfBounds {
        /// The supplied index (may be negative for reverse-indexing attempts).
        index: i64,
        /// The length of the list.
        len: usize,
    },

    /// An error propagated from the compiler (e.g. when constructing test graphs).
    #[error("compiler error: {0}")]
    CompilerError(#[from] CompilerError),

    /// A language feature that is not yet implemented was encountered.
    #[error("not yet implemented: {0}")]
    NotImplemented(String),

    /// A value declared with `extern` was not injected by the host runtime
    /// before script execution started.
    #[error("extern value '{0}' was not provided by the host runtime before execution")]
    ExternNotProvided(String),

    /// A method was called on a value type that does not support it.
    ///
    /// For example, calling `.push()` on an integer would produce this error.
    #[error("unknown method '{0}' for the given type")]
    UnknownMethod(String),

    /// A free-function call referenced a name that is not defined in any
    /// reachable scope.
    #[error("undefined function '{0}'")]
    UndefinedFunction(String),

    /// A script-defined decorator was applied to an event kind it does not
    /// accept, as declared by its `<event: …>` constraint clause.
    #[error(
        "constraint violation: decorator '@{decorator}' requires a '{constraint}' event, \
         but was applied to a '{actual_event}' event"
    )]
    ConstraintViolation {
        /// The name of the decorator whose constraint was violated.
        decorator: String,
        /// The constraint declared on the decorator (e.g. `"choice"`).
        constraint: String,
        /// The actual event kind it was applied to (e.g. `"dialogue"`).
        actual_event: String,
    },
}

// ─── Decorator validation helper ─────────────────────────────────────────────

/// Check that a single decorator name is registered (either in the Rust
/// registry or as a script-defined decorator). Returns `Err` on the first
/// unregistered name.
fn check_decorator_known(
    dec: &Decorator,
    registry: &DecoratorRegistry,
    script_defined: &HashSet<&str>,
    node_id: NodeIndex,
) -> Result<(), VmError> {
    if !registry.handlers.contains_key(dec.name()) && !script_defined.contains(dec.name()) {
        return Err(VmError::UnknownDecorator {
            name: dec.name().to_string(),
            node_id,
        });
    }
    Ok(())
}

// ─── Dice rolling ─────────────────────────────────────────────────────────────

/// A pluggable dice-rolling backend.
///
/// Implement this trait to substitute the default `rand`-based roller with a
/// deterministic stub in tests, or a custom RNG in production.
pub trait DiceRoller: Send + Sync {
    /// Roll `count` dice, each with `sides` faces, and return the total.
    ///
    /// Each die produces a value in `1..=sides`.  A `count` of `0` returns
    /// `0`.  Behaviour for `sides == 0` is implementation-defined.
    fn roll(&self, count: u32, sides: u32) -> i64;
}

/// Default [`DiceRoller`] implementation backed by [`rand::thread_rng`].
///
/// Used by the VM unless the host replaces it via dependency injection.
pub struct DefaultDiceRoller;

impl DiceRoller for DefaultDiceRoller {
    fn roll(&self, count: u32, sides: u32) -> i64 {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        (0..count).map(|_| rng.gen_range(1..=sides) as i64).sum()
    }
}

// ─── VmState ─────────────────────────────────────────────────────────────────

/// Mutable execution state of the VM.
///
/// Separated from the immutable [`IrGraph`] so the borrow checker can verify
/// that reading graph nodes never aliases the mutable state (cursor, env,
/// call stack, etc.).
#[derive(Debug)]
struct VmState {
    /// The node currently being processed, or `None` when execution has ended.
    cursor: Option<NodeIndex>,
    /// The runtime variable environment.
    env: Environment,
    /// The call stack for labeled-block "function" calls.
    call_stack: Vec<CallFrame>,
    /// Set when the last emitted event was a [`Event::Choice`]; `None`
    /// otherwise.  Cleared when the player provides a valid choice index.
    pending_choice: Option<NodeIndex>,
    /// Registry of named decorator handlers.
    registry: DecoratorRegistry,
}

// ─── Vm ──────────────────────────────────────────────────────────────────────

/// The Urd virtual machine.
///
/// The VM owns a compiled [`IrGraph`] (immutable during execution) and a
/// [`VmState`] (mutable), and steps through the graph one observable event
/// at a time when [`Vm::next`] is called.  Internal nodes are processed
/// silently without returning to the caller.
///
/// Splitting the struct this way lets the borrow checker prove that reading
/// nodes from the graph never aliases the mutable cursor / environment /
/// call-stack state — no `unsafe` required.
#[derive(Debug)]
pub struct Vm {
    /// The compiled IR graph (immutable during execution).
    graph: IrGraph,
    /// Pre-built map from label name → the `Next`-successor of the matching
    /// [`IrNodeKind::ExitScope`] node. Replaces the former O(n) scan in
    /// `find_exit_scope_next`.
    exit_scope_map: HashMap<String, Option<NodeIndex>>,
    /// All mutable execution state.
    state: VmState,
}

impl Vm {
    /// Creates a new VM from a compiled [`IrGraph`] and a [`DecoratorRegistry`].
    ///
    /// Performs a **validation pass** over every [`IrNodeKind::Dialogue`] and
    /// [`IrNodeKind::Choice`] node, checking that all decorator names are
    /// registered in `registry`.
    ///
    /// # Errors
    /// Returns [`VmError::UnknownDecorator`] if any decorator used in the
    /// script is not registered.
    pub fn new(graph: IrGraph, registry: DecoratorRegistry) -> Result<Self, VmError> {
        // Pass 1: collect all names defined by DefineScriptDecorator nodes so
        // that the validation pass below can accept them without a Rust handler.
        let script_defined: HashSet<&str> = graph
            .graph
            .node_weights()
            .filter_map(|k| match k {
                IrNodeKind::DefineScriptDecorator { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect();

        // Pass 2: validate every decorator used in Dialogue / Choice nodes.
        for node_idx in graph.graph.node_indices() {
            let kind = match graph.graph.node_weight(node_idx) {
                Some(k) => k,
                None => continue,
            };
            match kind {
                IrNodeKind::Dialogue { decorators, .. } => {
                    for dec in decorators {
                        check_decorator_known(dec, &registry, &script_defined, node_idx)?;
                    }
                }
                IrNodeKind::Choice {
                    decorators,
                    options,
                    loc_id: _,
                } => {
                    for dec in decorators {
                        check_decorator_known(dec, &registry, &script_defined, node_idx)?;
                    }
                    for opt in options {
                        for dec in &opt.decorators {
                            check_decorator_known(dec, &registry, &script_defined, node_idx)?;
                        }
                    }
                }
                _ => {}
            }
        }

        // Pre-seed every compiled label name as a RuntimeValue::Label builtin
        // so scripts can reference labels as values (e.g. pass them to decorator
        // arguments) without polluting the globals map that a save-file system
        // would serialise.
        let mut env = Environment::new();
        for (name, &node_idx) in &graph.labels {
            env.define_builtin(
                name.clone(),
                RuntimeValue::Label {
                    name: name.clone(),
                    node_id: node_idx,
                },
            );
        }

        // Pre-build the exit-scope lookup map (label → continuation NodeIndex).
        // This replaces the former O(n) linear scan done on every EnterScope.
        let exit_scope_map: HashMap<String, Option<NodeIndex>> = graph
            .graph
            .node_indices()
            .filter_map(|idx| match graph.graph.node_weight(idx) {
                Some(IrNodeKind::ExitScope { label }) => {
                    let next = next_of(&graph, idx);
                    Some((label.clone(), next))
                }
                _ => None,
            })
            .collect();

        let cursor = graph.entry;
        Ok(Vm {
            graph,
            exit_scope_map,
            state: VmState {
                cursor,
                env,
                call_stack: Vec::new(),
                pending_choice: None,
                registry,
            },
        })
    }

    /// Advance the VM by one observable step and return the next [`Event`].
    ///
    /// Pass `choice: Some(idx)` when responding to a [`Event::Choice`] event;
    /// pass `None` for all other steps.
    ///
    /// Returns [`VmStep::Ended`] when the script has ended.
    #[allow(clippy::collapsible_if)]
    pub fn next(&mut self, choice: Option<usize>) -> VmStep {
        // Split borrows: the graph and exit-scope map are immutable for the
        // entire loop, while the state (cursor, env, call-stack, …) is mutated
        // freely.  This lets the borrow checker verify the invariant without
        // any `unsafe` code.
        let graph = &self.graph;
        let exit_scope_map = &self.exit_scope_map;
        let state = &mut self.state;

        loop {
            let node_idx = match state.cursor {
                Some(idx) => idx,
                None => return VmStep::Ended,
            };

            let node_kind = match graph.graph.node_weight(node_idx) {
                Some(k) => k,
                None => return VmStep::Ended,
            };

            match node_kind {
                // ── Terminal ─────────────────────────────────────────────────
                IrNodeKind::End => return VmStep::Ended,

                IrNodeKind::Todo => {
                    log::warn!("todo!() reached — this execution path is not yet implemented");
                    return VmStep::Ended;
                }

                // ── Nop / merge point ────────────────────────────────────────
                IrNodeKind::Nop => {
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Variable assignment ──────────────────────────────────────
                IrNodeKind::Assign { var, scope, expr } => {
                    let value = match eval_expr(expr, &state.env) {
                        Ok(v) => v,
                        Err(e) => return VmStep::Error(e),
                    };
                    if let Err(e) = state.env.set(var, value, scope) {
                        return VmStep::Error(e);
                    }
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Side-effecting expression ────────────────────────────────
                IrNodeKind::Eval { expr } => {
                    // With the graph/state split, `expr` borrows from `graph`
                    // and `state.env` is a separate struct — no clone needed.
                    match expr.content() {
                        AstContent::SubscriptAssign { object, key, value } => {
                            if let Err(e) =
                                eval_subscript_assign(object, key, value, &mut state.env)
                            {
                                return VmStep::Error(e);
                            }
                        }
                        _ => {
                            if let Err(e) = eval_expr(expr, &state.env) {
                                return VmStep::Error(e);
                            }
                        }
                    }
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Conditional branch ───────────────────────────────────────
                IrNodeKind::Branch { condition } => {
                    let cond_val = match eval_expr(condition, &state.env) {
                        Ok(v) => v,
                        Err(e) => return VmStep::Error(e),
                    };
                    let mut then_node: Option<NodeIndex> = None;
                    let mut else_node: Option<NodeIndex> = None;
                    for e in graph.graph.edges_directed(node_idx, Direction::Outgoing) {
                        match e.weight() {
                            IrEdge::Then => then_node = Some(e.target()),
                            IrEdge::Else => else_node = Some(e.target()),
                            _ => {}
                        }
                    }
                    state.cursor = if is_truthy(&cond_val) {
                        then_node
                    } else {
                        else_node
                    };
                }

                // ── Multi-way pattern match ──────────────────────────────────
                IrNodeKind::Switch { scrutinee, arms } => {
                    let scrutinee_val = match eval_expr(scrutinee, &state.env) {
                        Ok(v) => v,
                        Err(e) => return VmStep::Error(e),
                    };

                    let mut matched = false;
                    for (i, arm) in arms.iter().enumerate() {
                        let is_match = match &arm.pattern {
                            MatchPattern::Wildcard => true,
                            MatchPattern::Value(pat_ast) => match eval_expr(pat_ast, &state.env) {
                                Ok(pat_val) => values_equal(&scrutinee_val, &pat_val),
                                Err(_) => false,
                            },
                        };
                        if is_match {
                            let target = graph
                                .graph
                                .edges_directed(node_idx, Direction::Outgoing)
                                .find(|e| {
                                    if let IrEdge::Arm(j) = e.weight() {
                                        *j == i
                                    } else {
                                        false
                                    }
                                })
                                .map(|e| e.target());
                            state.cursor = target;
                            matched = true;
                            break;
                        }
                    }

                    if !matched {
                        let default_target = graph
                            .graph
                            .edges_directed(node_idx, Direction::Outgoing)
                            .find(|e| matches!(e.weight(), IrEdge::Default))
                            .map(|e| e.target());
                        state.cursor = default_target;
                    }
                }

                // ── Unconditional jump ───────────────────────────────────────
                IrNodeKind::Jump => {
                    // Jump does NOT push a call frame — it is a tail transfer.
                    let target = graph
                        .graph
                        .edges_directed(node_idx, Direction::Outgoing)
                        .find(|e| matches!(e.weight(), IrEdge::Jump))
                        .map(|e| e.target());
                    state.cursor = target;
                }

                // ── Return ───────────────────────────────────────────────────
                IrNodeKind::Return { value } => {
                    // Evaluate return value first (before popping the frame).
                    let return_val = if let Some(val_ast) = value {
                        match eval_expr(val_ast, &state.env) {
                            Ok(v) => Some(v),
                            Err(e) => return VmStep::Error(e),
                        }
                    } else {
                        None
                    };

                    match state.call_stack.pop() {
                        Some(frame) => {
                            // Unwind any extra scopes pushed since the call.
                            while state.env.depth() > frame.scope_depth {
                                state.env.pop_scope();
                            }
                            // Store return value if the call frame has a binding.
                            if let (Some(var_name), Some(val)) = (&frame.assign_to_var, return_val)
                            {
                                if let Err(e) = state.env.set(var_name, val, &DeclKind::Variable) {
                                    return VmStep::Error(e);
                                }
                            }
                            state.cursor = frame.return_cursor;
                        }
                        None => return VmStep::Ended, // No frame → script ends.
                    }
                }

                // ── Subroutine call (with optional result binding) ────────────
                //
                // We bypass the target's EnterScope node to avoid a double frame
                // on the call stack (EnterScope always pushes its own frame).
                // Instead, LetCall:
                //   1. Reads body_entry directly from the EnterScope node's Next edge.
                //   2. Pushes its own scope + frame (with return_cursor = Ret edge target).
                //   3. Jumps to body_entry.
                //
                // ExitScope (on fall-through) and Return (on explicit return) both
                // pop the LetCall frame and land at `return_cursor`.
                IrNodeKind::LetCall { var } => {
                    let callee_idx = graph
                        .graph
                        .edges_directed(node_idx, Direction::Outgoing)
                        .find(|e| matches!(e.weight(), IrEdge::Call))
                        .map(|e| e.target());

                    let ret_idx = graph
                        .graph
                        .edges_directed(node_idx, Direction::Outgoing)
                        .find(|e| matches!(e.weight(), IrEdge::Ret))
                        .map(|e| e.target());

                    // Resolve body_entry: if the callee is an EnterScope node,
                    // jump past it into the body directly.
                    let body_entry = match callee_idx {
                        Some(ci) => match graph.graph.node_weight(ci) {
                            Some(IrNodeKind::EnterScope { .. }) => next_of(graph, ci),
                            _ => Some(ci),
                        },
                        None => None,
                    };

                    let assign = if var.is_empty() {
                        None
                    } else {
                        Some(var.clone())
                    };

                    // Push a scope to match the eventual ExitScope's pop.
                    state.env.push_scope();
                    state.call_stack.push(CallFrame {
                        return_cursor: ret_idx,
                        scope_depth: state.env.depth() - 1,
                        assign_to_var: assign,
                    });
                    state.cursor = body_entry;
                }

                // ── Enter labeled-block scope ────────────────────────────────
                IrNodeKind::EnterScope { label } => {
                    // O(1) lookup via pre-built map instead of O(n) scan.
                    let return_cursor = exit_scope_map.get(label.as_str()).and_then(|v| *v);

                    state.env.push_scope();
                    state.call_stack.push(CallFrame {
                        return_cursor,
                        scope_depth: state.env.depth() - 1,
                        assign_to_var: None,
                    });
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Exit labeled-block scope ─────────────────────────────────
                IrNodeKind::ExitScope { .. } => {
                    state.env.pop_scope();
                    // Pop the matching call frame (pushed by either EnterScope
                    // on normal fall-through, or LetCall on a subroutine call).
                    // Use the frame's `return_cursor` rather than the ExitScope's
                    // Next edge so that LetCall fall-throughs land at the
                    // call-site continuation (LetCall's Ret edge), not back at
                    // the LetCall node itself.
                    let next_id = match state.call_stack.pop() {
                        Some(frame) => frame.return_cursor,
                        None => None,
                    };
                    state.cursor = next_id;
                }

                // ── Enum declaration ─────────────────────────────────────────
                IrNodeKind::DefineEnum { name, variants } => {
                    state.env.define_enum(name.clone(), variants.clone());
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Struct declaration ───────────────────────────────────────
                IrNodeKind::DefineStruct { name, fields } => {
                    state.env.define_struct(name.clone(), fields.clone());
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Script-defined decorator registration ────────────────────
                //
                // At execution time we store a `RuntimeValue::ScriptDecorator`
                // in the environment under `name`.  When a `@name(args)` usage
                // is encountered during Dialogue/Choice evaluation, `apply_decorator`
                // finds it there and calls `apply_script_decorator` to run the body.
                IrNodeKind::DefineScriptDecorator {
                    name,
                    event_constraint,
                    params,
                    body,
                } => {
                    let decorator_val = RuntimeValue::ScriptDecorator {
                        event_constraint: event_constraint.clone(),
                        params: params.clone(),
                        body: Box::new(body.clone()),
                    };
                    if let Err(e) = state.env.set(name, decorator_val, &DeclKind::Variable) {
                        return VmStep::Error(e);
                    }
                    state.cursor = next_of(graph, node_idx);
                }

                // ── User-defined function registration ───────────────────────
                //
                // Stores a `RuntimeValue::Function` in the environment under
                // `name`, making it callable as `name(args)` from any
                // expression that follows in the same scope.
                IrNodeKind::DefineFunction { name, params, body } => {
                    let func_val = RuntimeValue::Function {
                        params: params.clone(),
                        body: Box::new(body.clone()),
                    };
                    if let Err(e) = state.env.set(name, func_val, &DeclKind::Variable) {
                        return VmStep::Error(e);
                    }
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Extern declaration validation ────────────────────────────
                IrNodeKind::ExternDecl { name, .. } => {
                    // Verify the host runtime injected a value for this extern.
                    if state.env.get(name).is_err() {
                        return VmStep::Error(VmError::ExternNotProvided(name.clone()));
                    }
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Dialogue event ───────────────────────────────────────────
                IrNodeKind::Dialogue {
                    speakers,
                    lines,
                    decorators,
                    loc_id,
                } => {
                    let speakers_vec = match eval_expr_list(speakers, &state.env) {
                        Ok(v) => v,
                        Err(e) => return VmStep::Error(e),
                    };
                    let lines_vec = match eval_expr_list(lines, &state.env) {
                        Ok(v) => v,
                        Err(e) => return VmStep::Error(e),
                    };

                    // Evaluate decorators and merge their fields.
                    // `apply_decorator` checks the env first (for script-defined
                    // decorators), then falls back to the Rust registry.
                    let mut fields: HashMap<String, RuntimeValue> = HashMap::new();
                    for dec in decorators {
                        match apply_decorator(
                            dec,
                            &state.env,
                            &state.registry,
                            fields,
                            &EventConstraint::Dialogue,
                        ) {
                            Ok(new_fields) => fields = new_fields,
                            Err(e) => return VmStep::Error(e),
                        }
                    }

                    state.cursor = next_of(graph, node_idx);
                    return VmStep::Event(Event::Dialogue {
                        speakers: speakers_vec,
                        lines: lines_vec,
                        fields,
                        loc_id: loc_id.clone(),
                    });
                }

                // ── Choice event ─────────────────────────────────────────────
                IrNodeKind::Choice {
                    options,
                    decorators,
                    loc_id,
                } => {
                    match choice {
                        // ── Player provided a choice index ───────────────────
                        Some(idx) => {
                            if idx >= options.len() {
                                log::warn!(
                                    "Choice index {} out of bounds (len={}); \
                                         re-emitting Choice event",
                                    idx,
                                    options.len()
                                );
                                // Re-emit without advancing.
                                return build_choice_event(
                                    options,
                                    decorators,
                                    loc_id,
                                    &state.env,
                                    &state.registry,
                                );
                            }
                            // Follow the Option(idx) edge to the chosen option's entry.
                            let entry = graph
                                .graph
                                .edges_directed(node_idx, Direction::Outgoing)
                                .find(|e| {
                                    if let IrEdge::Option(i) = e.weight() {
                                        *i == idx
                                    } else {
                                        false
                                    }
                                })
                                .map(|e| e.target());
                            state.pending_choice = None;
                            state.cursor = entry;
                            // Continue loop — the choice selection itself is
                            // not an observable event.
                        }

                        // ── No choice provided ───────────────────────────────
                        None => {
                            if state.pending_choice == Some(node_idx) {
                                // Already waiting for a choice at this node — re-emit.
                                log::warn!(
                                    "Choice at node {:?} is already pending; \
                                         re-emitting Event::Choice without advancing",
                                    node_idx
                                );
                            } else {
                                state.pending_choice = Some(node_idx);
                            }
                            return build_choice_event(
                                options,
                                decorators,
                                loc_id,
                                &state.env,
                                &state.registry,
                            );
                        }
                    }
                }
            }
        }
    }

    /// Returns a reference to the VM's current [`Environment`].
    pub fn env(&self) -> &Environment {
        &self.state.env
    }

    /// Returns a reference to the compiled [`IrGraph`].
    pub fn graph(&self) -> &IrGraph {
        &self.graph
    }

    /// Inject a runtime-provided extern value into the VM's environment.
    ///
    /// This must be called for every name declared `extern` in the script
    /// **before** the first [`Vm::next`] call. May also be called between steps
    /// to update a live extern value.
    ///
    /// See [`Environment::provide_extern`] for storage semantics.
    pub fn provide_extern(&mut self, name: &str, value: RuntimeValue) {
        self.state.env.provide_extern(name, value);
    }
}

// ─── Subscript-assign helper ──────────────────────────────────────────────────

/// Handles `object[key] = value` mutations by modifying the map stored under
/// `object`'s identifier path in `env`.
fn eval_subscript_assign(
    object: &crate::parser::ast::Ast,
    key: &crate::parser::ast::Ast,
    value: &crate::parser::ast::Ast,
    env: &mut Environment,
) -> Result<(), VmError> {
    use crate::parser::ast::AstContent;

    // Resolve the variable name that holds the map or list.
    let var_name = match object.content() {
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => path[0].clone(),
        _ => {
            return Err(VmError::InvalidExpression(
                "subscript assign: object must be a simple identifier".into(),
            ));
        }
    };

    // Evaluate the key and new value.
    let key_val = eval_expr(key, env)?;
    let new_val = eval_expr(value, env)?;

    // Fetch the current value of the variable.
    let current = env
        .get(&var_name)
        .map_err(|_| VmError::UndefinedVariable(var_name.clone()))?;

    match current {
        RuntimeValue::Map(mut map) => {
            let key_str = match key_val {
                RuntimeValue::Str(ref ps) => ps.to_string(),
                RuntimeValue::Int(i) => i.to_string(),
                other => {
                    return Err(VmError::TypeError(format!(
                        "map key must be Str or Int, got {other:?}"
                    )));
                }
            };
            map.insert(key_str, Box::new(new_val));
            env.set(&var_name, RuntimeValue::Map(map), &DeclKind::Variable)
        }
        RuntimeValue::List(mut list) => {
            let idx = match key_val {
                RuntimeValue::Int(i) => i,
                other => {
                    return Err(VmError::TypeError(format!(
                        "list index must be Int, got {other:?}"
                    )));
                }
            };
            let len = list.len();
            // Support Python-style negative indexing.
            let actual = if idx < 0 {
                let pos = len as i64 + idx;
                if pos < 0 {
                    return Err(VmError::IndexOutOfBounds { index: idx, len });
                }
                pos as usize
            } else {
                let pos = idx as usize;
                if pos >= len {
                    return Err(VmError::IndexOutOfBounds { index: idx, len });
                }
                pos
            };
            list[actual] = new_val;
            env.set(&var_name, RuntimeValue::List(list), &DeclKind::Variable)
        }
        other => Err(VmError::TypeError(format!(
            "subscript assign: {var_name} is not a map or list, got {other:?}"
        ))),
    }
}

// ─── Sync block executor ─────────────────────────────────────────────────────

/// Executes a sequence of AST statements synchronously, collecting any
/// side-effects into `env`.
///
/// Used by script-defined decorator bodies and other inline-evaluated blocks
/// that must run to completion before the VM can continue.
fn exec_block_sync(
    block: &crate::parser::ast::Ast,
    env: &mut Environment,
    event: &mut HashMap<String, RuntimeValue>,
) -> Result<(), VmError> {
    use crate::parser::ast::AstContent;

    let stmts = match block.content() {
        AstContent::Block(stmts) => stmts,
        _ => {
            // Single statement.
            return exec_stmt_sync(block, env, event);
        }
    };

    for stmt in stmts {
        exec_stmt_sync(stmt, env, event)?;
    }
    Ok(())
}

/// Executes a single AST statement inside a decorator body.
fn exec_stmt_sync(
    stmt: &crate::parser::ast::Ast,
    env: &mut Environment,
    event: &mut HashMap<String, RuntimeValue>,
) -> Result<(), VmError> {
    use crate::parser::ast::AstContent;

    match stmt.content() {
        AstContent::Declaration {
            kind,
            decl_name,
            decl_defs,
            ..
        } => {
            let name_str = match decl_name.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
                _ => {
                    return Err(VmError::InvalidExpression(
                        "declaration name must be an identifier".into(),
                    ));
                }
            };
            let v = eval_expr(decl_defs, env)?;
            env.set(&name_str, v, kind)?;
        }
        AstContent::SubscriptAssign { object, key, value } => {
            // Check if the object is `event` — if so, write into the event map
            // instead of the variable environment.
            let is_event = matches!(
                object.content(),
                AstContent::Value(RuntimeValue::IdentPath(p)) if p.len() == 1 && p[0] == "event"
            );
            if is_event {
                let key_val = eval_expr(key, env)?;
                let new_val = eval_expr(value, env)?;
                let key_str = match key_val {
                    RuntimeValue::Str(ref ps) => ps.to_string(),
                    RuntimeValue::Int(i) => i.to_string(),
                    other => {
                        return Err(VmError::TypeError(format!(
                            "event key must be Str or Int, got {other:?}"
                        )));
                    }
                };
                event.insert(key_str, new_val);
            } else {
                eval_subscript_assign(object, key, value, env)?;
            }
        }
        _ => {
            eval_expr(stmt, env)?;
        }
    }
    Ok(())
}

// ─── Script-decorator execution ──────────────────────────────────────────────

/// Bundles the compile-time definition data for a script-defined decorator.
///
/// Passed by reference to [`apply_script_decorator`] so that function stays
/// within Clippy's `too_many_arguments` limit.
struct ScriptDecoratorDef<'a> {
    /// The decorator's registered name (used in error messages).
    name: &'a str,
    /// The event-kind constraint declared on the decorator.
    event_constraint: &'a EventConstraint,
    /// Ordered parameter names.
    params: &'a [String],
    /// The decorator body block.
    body: &'a crate::parser::ast::Ast,
}

/// Runs a script-defined decorator body against `event`, mutating its fields.
///
/// # Errors
///
/// Returns [`VmError::ConstraintViolation`] when `def.event_constraint` is
/// `Dialogue` but `actual_kind` is `Choice`, or vice-versa.
fn apply_script_decorator(
    def: &ScriptDecoratorDef<'_>,
    args: &[crate::parser::ast::Ast],
    env: &Environment,
    mut event: HashMap<String, RuntimeValue>,
    actual_kind: &EventConstraint,
) -> Result<HashMap<String, RuntimeValue>, VmError> {
    // Enforce the event-kind constraint declared on the decorator.
    match def.event_constraint {
        EventConstraint::Any => {}
        EventConstraint::Dialogue => {
            if matches!(actual_kind, EventConstraint::Choice) {
                return Err(VmError::ConstraintViolation {
                    decorator: def.name.to_string(),
                    constraint: "dialogue".to_string(),
                    actual_event: "choice".to_string(),
                });
            }
        }
        EventConstraint::Choice => {
            if matches!(actual_kind, EventConstraint::Dialogue) {
                return Err(VmError::ConstraintViolation {
                    decorator: def.name.to_string(),
                    constraint: "choice".to_string(),
                    actual_event: "dialogue".to_string(),
                });
            }
        }
    }

    // Bind arguments to parameter names in a fresh inner scope.
    let mut inner_env = env.clone();
    inner_env.push_scope();
    for (param, arg_ast) in def.params.iter().zip(args.iter()) {
        let val = eval_expr(arg_ast, env)?;
        inner_env.set(param.as_str(), val, &DeclKind::Variable)?;
    }
    // Expose the mutable `event` map as an IdentPath value so the body can
    // read existing fields.  Mutations are applied via `exec_block_sync`.
    let event_snapshot: HashMap<String, Box<RuntimeValue>> = event
        .iter()
        .map(|(k, v)| (k.clone(), Box::new(v.clone())))
        .collect();
    inner_env.set(
        "event",
        RuntimeValue::Map(event_snapshot),
        &DeclKind::Variable,
    )?;

    exec_block_sync(def.body, &mut inner_env, &mut event)?;
    Ok(event)
}

// ─── Decorator dispatch ───────────────────────────────────────────────────────

/// Applies one decorator to the running `event` field map.
///
/// Checks the environment for a script-defined decorator first; falls back to
/// the Rust [`DecoratorRegistry`].
///
/// `actual_kind` indicates the kind of event being decorated (`Dialogue` or
/// `Choice`); it is forwarded to [`apply_script_decorator`] for constraint
/// checking.  Pass [`EventConstraint::Any`] when the kind is irrelevant.
fn apply_decorator(
    dec: &Decorator,
    env: &Environment,
    registry: &DecoratorRegistry,
    event: HashMap<String, RuntimeValue>,
    actual_kind: &EventConstraint,
) -> Result<HashMap<String, RuntimeValue>, VmError> {
    // Check whether this decorator is script-defined (stored in env).
    if let Ok(RuntimeValue::ScriptDecorator {
        event_constraint,
        params,
        body,
    }) = env.get(dec.name())
    {
        // dec.args() always returns &Ast; unpack ExprList items as argument ASTs.
        let args_ast = dec.args();
        let args: Vec<crate::parser::ast::Ast> = match args_ast.content() {
            AstContent::ExprList(items) => items.clone(),
            _ => vec![args_ast.clone()],
        };
        return apply_script_decorator(
            &ScriptDecoratorDef {
                name: dec.name(),
                event_constraint: &event_constraint,
                params: &params,
                body: &body,
            },
            &args,
            env,
            event,
            actual_kind,
        );
    }

    // Fall back to Rust-registered handler.
    let extra_fields = registry.apply(dec, env)?;
    Ok(event.into_iter().chain(extra_fields).collect())
}

// ─── Choice event builder ─────────────────────────────────────────────────────

/// Builds an [`Event::Choice`] from a set of choice options and decorators.
fn build_choice_event(
    options: &[IrChoiceOption],
    decorators: &[Decorator],
    loc_id: &Option<String>,
    env: &Environment,
    registry: &DecoratorRegistry,
) -> VmStep {
    // Evaluate top-level choice decorators.
    let mut fields: HashMap<String, RuntimeValue> = HashMap::new();
    for dec in decorators {
        match apply_decorator(dec, env, registry, fields, &EventConstraint::Choice) {
            Ok(new_fields) => fields = new_fields,
            Err(e) => return VmStep::Error(e),
        }
    }

    // Build per-option ChoiceEvent entries.
    let mut choice_options: Vec<ChoiceEvent> = Vec::new();
    for opt in options {
        let mut opt_fields: HashMap<String, RuntimeValue> = HashMap::new();
        for dec in &opt.decorators {
            match apply_decorator(dec, env, registry, opt_fields, &EventConstraint::Choice) {
                Ok(new_fields) => opt_fields = new_fields,
                Err(e) => return VmStep::Error(e),
            }
        }
        choice_options.push(ChoiceEvent {
            label: opt.label.clone(),
            fields: opt_fields,
            loc_id: opt.loc_id.clone(),
        });
    }

    VmStep::Event(Event::Choice {
        options: choice_options,
        fields,
        loc_id: loc_id.clone(),
    })
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::Compiler,
        lexer::strings::ParsedString,
        parser::ast::{Ast, AstContent, DeclKind, Decorator, MatchArm, MatchPattern, TokSpan},
        runtime::value::RuntimeValue,
    };
    use chumsky::span::Span as _;

    // ── Shared helpers ────────────────────────────────────────────────────────

    fn ident(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_string()]))
    }

    fn int(n: i64) -> Ast {
        Ast::value(RuntimeValue::Int(n))
    }

    fn str_lit(s: &str) -> Ast {
        Ast::value(RuntimeValue::Str(ParsedString::new_plain(s)))
    }

    fn decl(name: &str, val: Ast) -> Ast {
        Ast::decl(DeclKind::Variable, ident(name), val)
    }

    fn empty_registry() -> DecoratorRegistry {
        DecoratorRegistry::new()
    }

    fn build_vm(ast: Ast) -> Vm {
        let graph = Compiler::compile(&ast).expect("compile failed");
        Vm::new(graph, empty_registry()).expect("vm construction failed")
    }

    fn build_vm_named(ast: Ast, file_stem: &str) -> Vm {
        let graph = crate::compiler::Compiler::compile_named(&ast, file_stem)
            .expect("compile_named failed");
        Vm::new(graph, empty_registry()).expect("vm construction failed")
    }

    // ── Tests ─────────────────────────────────────────────────────────────────

    /// Dialogue compiled with `compile_named` carries the generated `loc_id`.
    #[test]
    fn test_dialogue_event_carries_loc_id() {
        let speakers = Ast::expr_list(vec![str_lit("narrator")]);
        let lines = Ast::expr_list(vec![str_lit("Hello!")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let labeled = Ast::labeled_block("start".to_string(), dialogue);
        let ast = Ast::block(vec![labeled]);

        let mut vm = build_vm_named(ast, "intro");

        match vm.next(None) {
            VmStep::Event(Event::Dialogue { loc_id, .. }) => {
                assert_eq!(
                    loc_id.as_deref(),
                    Some("intro-start-line_1"),
                    "expected intro-start-line_1, got {:?}",
                    loc_id
                );
            }
            other => panic!("expected Dialogue event, got {:?}", other),
        }
    }

    /// `compile` (no stem) produces `loc_id: None` on emitted Dialogue events.
    #[test]
    fn test_dialogue_event_no_loc_id_without_file_stem() {
        let speakers = Ast::expr_list(vec![str_lit("narrator")]);
        let lines = Ast::expr_list(vec![str_lit("Hello!")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![dialogue]);

        let mut vm = build_vm(ast);

        match vm.next(None) {
            VmStep::Event(Event::Dialogue { loc_id, .. }) => {
                assert!(
                    loc_id.is_none(),
                    "compile() should yield loc_id=None, got {:?}",
                    loc_id
                );
            }
            other => panic!("expected Dialogue event, got {:?}", other),
        }
    }

    /// Choice event and each option carry their generated `loc_id` values.
    #[test]
    fn test_choice_event_carries_loc_ids() {
        let opt1 = Ast::menu_option("yes".to_string(), Ast::block(vec![]));
        let opt2 = Ast::menu_option("no".to_string(), Ast::block(vec![]));
        let menu = Ast::menu(vec![opt1, opt2]);
        let labeled = Ast::labeled_block("start".to_string(), menu);
        let ast = Ast::block(vec![labeled]);

        let mut vm = build_vm_named(ast, "test");

        match vm.next(None) {
            VmStep::Event(Event::Choice {
                loc_id, options, ..
            }) => {
                assert_eq!(
                    loc_id.as_deref(),
                    Some("test-start-menu_1"),
                    "Choice event loc_id mismatch: got {:?}",
                    loc_id
                );
                assert_eq!(options.len(), 2);
                assert_eq!(
                    options[0].loc_id.as_deref(),
                    Some("test-start-menu_1-yes"),
                    "option[0] loc_id mismatch: got {:?}",
                    options[0].loc_id
                );
                assert_eq!(
                    options[1].loc_id.as_deref(),
                    Some("test-start-menu_1-no"),
                    "option[1] loc_id mismatch: got {:?}",
                    options[1].loc_id
                );
            }
            other => panic!("expected Choice event, got {:?}", other),
        }
    }

    /// A script `let x = 1` followed by a Dialogue emits exactly one
    /// `Event::Dialogue` and then ends.
    #[test]
    fn test_let_then_dialogue_emits_event() {
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hello!")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![decl("x", int(1)), dialogue]);

        let mut vm = build_vm(ast);

        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue {
                speakers, lines, ..
            } => {
                assert_eq!(speakers.len(), 1);
                assert_eq!(lines.len(), 1);
                match &lines[0] {
                    RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "Hello!"),
                    other => panic!("expected Str, got {:?}", other),
                }
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }

        assert!(
            matches!(vm.next(None), VmStep::Ended),
            "script should end after dialogue"
        );
    }

    /// A `Branch` on `true` follows `then_node`; the dialogue reads the
    /// variable set in the then-block.
    #[test]
    fn test_branch_true_follows_then() {
        let condition = Ast::value(RuntimeValue::Bool(true));
        let then_block = Ast::block(vec![decl("x", int(1))]);
        let else_block = Ast::block(vec![decl("x", int(2))]);
        let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

        let speakers = Ast::expr_list(vec![str_lit("Bob")]);
        let lines = Ast::expr_list(vec![ident("x")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![if_ast, dialogue]);

        let mut vm = build_vm(ast);
        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };

        match ev {
            Event::Dialogue { lines, .. } => {
                assert_eq!(
                    lines,
                    vec![RuntimeValue::Int(1)],
                    "then-branch should set x=1"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// A `Branch` on `false` follows `else_node`.
    #[test]
    fn test_branch_false_follows_else() {
        let condition = Ast::value(RuntimeValue::Bool(false));
        let then_block = Ast::block(vec![decl("result", int(1))]);
        let else_block = Ast::block(vec![decl("result", int(2))]);
        let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

        let speakers = Ast::expr_list(vec![str_lit("Narrator")]);
        let lines = Ast::expr_list(vec![ident("result")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![if_ast, dialogue]);

        let mut vm = build_vm(ast);
        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { lines, .. } => {
                assert_eq!(
                    lines,
                    vec![RuntimeValue::Int(2)],
                    "else-branch should set result=2"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `next(None)` on a Menu emits `Event::Choice`; calling `next(None)` again
    /// re-emits it (with a log warning); `next(Some(0))` clears the pending
    /// choice and advances into the option body.
    #[test]
    fn test_choice_flow() {
        let opt_a = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![decl("picked", int(1))]),
        );
        let opt_b = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![decl("picked", int(2))]),
        );
        let ast = Ast::menu(vec![opt_a, opt_b]);

        let mut vm = build_vm(ast);

        // First call — emits Choice and sets pending_choice.
        let ev1 = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("first next(None) should emit Event, got {:?}", other),
        };
        assert!(
            matches!(ev1, Event::Choice { .. }),
            "first next(None) should emit Choice, got {:?}",
            ev1
        );
        // cursor and pending_choice are both Option<NodeIndex>.
        let choice_cursor = vm.state.cursor;
        assert_eq!(
            vm.state.pending_choice, choice_cursor,
            "pending_choice must equal cursor after first next(None)"
        );

        // Second call with None — re-emits Choice.
        let ev2 = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("second next(None) should emit Event, got {:?}", other),
        };
        assert!(
            matches!(ev2, Event::Choice { ref options, .. } if options.len() == 2),
            "second next(None) should re-emit same Choice"
        );

        // Provide a valid choice (option 0).
        // Option 0's body has `let picked = 1` — internal, so the loop ends.
        let ev3 = vm.next(Some(0));
        assert!(
            vm.state.pending_choice.is_none(),
            "pending_choice must be cleared after valid choice"
        );
        // Option body has no dialogue → script ends → VmStep::Ended.
        assert!(
            matches!(ev3, VmStep::Ended),
            "expected script end after choosing option 0, got {:?}",
            ev3
        );
    }

    /// An out-of-bounds choice index re-emits the Choice event with a warning
    /// and does NOT advance the cursor.
    #[test]
    fn test_choice_out_of_bounds_reemits() {
        let opt = Ast::menu_option("Only".to_string(), Ast::block(vec![]));
        let ast = Ast::menu(vec![opt]);
        let mut vm = build_vm(ast);

        // Consume the initial None-choice emission.
        match vm.next(None) {
            VmStep::Event(_) => {}
            other => panic!("expected Event, got {:?}", other),
        };

        // Provide an out-of-bounds index.
        let ev = match vm.next(Some(99)) {
            VmStep::Event(e) => e,
            other => panic!("out-of-bounds should re-emit Event, got {:?}", other),
        };
        assert!(
            matches!(ev, Event::Choice { .. }),
            "out-of-bounds choice should re-emit Choice, got {:?}",
            ev
        );
    }

    /// A `Jump` to a known label advances the cursor and the dialogue inside
    /// the label is reached.
    #[test]
    fn test_jump_advances_to_label() {
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Jumped here!")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let labeled = Ast::labeled_block("scene1".to_string(), Ast::block(vec![dialogue]));
        let jump = Ast::jump_stmt("scene1".to_string(), false);
        let ast = Ast::block(vec![jump, labeled]);

        let mut vm = build_vm(ast);

        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(ps.to_string(), "Jumped here!");
                }
                other => panic!("expected Str line, got {:?}", other),
            },
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `Vm::new` returns `VmError::UnknownDecorator` when the compiled script
    /// uses a decorator name that is not registered.
    #[test]
    fn test_unknown_decorator_fails_validation() {
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hi")]);
        let deco = Decorator::bare("mystery_deco".to_string());
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let graph = Compiler::compile(&dialogue).expect("compile failed");
        let result = Vm::new(graph, empty_registry());

        assert!(
            matches!(
                result,
                Err(VmError::UnknownDecorator { ref name, .. }) if name == "mystery_deco"
            ),
            "expected UnknownDecorator, got {:?}",
            result
        );
    }

    /// A `Vm::new` call succeeds when all decorators used in the script are
    /// registered.
    #[test]
    fn test_known_decorator_passes_validation() {
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hi")]);
        let deco = Decorator::bare("mood".to_string());
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let graph = Compiler::compile(&dialogue).expect("compile failed");
        let mut registry = DecoratorRegistry::new();
        registry.register("mood", |_args| HashMap::new());

        assert!(
            Vm::new(graph, registry).is_ok(),
            "registered decorator should pass validation"
        );
    }

    /// `Return` inside a labeled block unwinds back to the continuation after
    /// the block (the dialogue that follows it in the script).
    #[test]
    fn test_return_exits_to_continuation() {
        let labeled = Ast::labeled_block(
            "myblock".to_string(),
            Ast::block(vec![Ast::return_stmt(None)]),
        );
        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("After block")]);
        let after_dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![labeled, after_dialogue]);

        let mut vm = build_vm(ast);

        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => assert_eq!(ps.to_string(), "After block"),
                other => panic!("expected Str, got {:?}", other),
            },
            other => panic!("expected Dialogue after return, got {:?}", other),
        }
        assert!(
            matches!(vm.next(None), VmStep::Ended),
            "script should end after dialogue"
        );
    }

    /// A `Return` with no call frame on the stack ends the script.
    #[test]
    fn test_return_with_empty_call_stack_ends_script() {
        let ast = Ast::return_stmt(None);
        let mut vm = build_vm(ast);
        assert!(
            matches!(vm.next(None), VmStep::Ended),
            "Return with empty call stack should end script"
        );
    }

    // ── Expression evaluator unit tests ───────────────────────────────────────

    /// Basic arithmetic: Int+Int, Float+Int coercion, negation.
    #[test]
    fn test_eval_arithmetic() {
        let env = Environment::new();

        let add = Ast::add_op(int(3), int(4));
        assert_eq!(eval_expr(&add, &env).expect("3+4"), RuntimeValue::Int(7));

        let mixed = Ast::add_op(Ast::value(RuntimeValue::Float(2.5)), int(1));
        assert_eq!(
            eval_expr(&mixed, &env).expect("2.5+1"),
            RuntimeValue::Float(3.5)
        );

        let neg = Ast::negate_op(int(5));
        assert_eq!(eval_expr(&neg, &env).expect("-5"), RuntimeValue::Int(-5));

        let not_true = Ast::not_op(Ast::value(RuntimeValue::Bool(true)));
        assert_eq!(
            eval_expr(&not_true, &env).expect("not true"),
            RuntimeValue::Bool(false)
        );
    }

    /// Comparison operators produce Bool results.
    #[test]
    fn test_eval_comparison() {
        let env = Environment::new();

        let gt = Ast::greater_than_op(int(5), int(3));
        assert_eq!(eval_expr(&gt, &env).expect("5>3"), RuntimeValue::Bool(true));

        let eq = Ast::equals_op(int(4), int(4));
        assert_eq!(
            eval_expr(&eq, &env).expect("4==4"),
            RuntimeValue::Bool(true)
        );

        let neq = Ast::not_equals_op(int(4), int(5));
        assert_eq!(
            eval_expr(&neq, &env).expect("4!=5"),
            RuntimeValue::Bool(true)
        );
    }

    /// Shift with a negative count must return a VM error (never panic).
    #[test]
    fn test_eval_left_shift_negative_count_errors() {
        let env = Environment::new();
        let expr = Ast::left_shift_op(int(8), int(-1));

        match eval_expr(&expr, &env) {
            Err(VmError::TypeError(msg)) => {
                assert!(
                    msg.contains("invalid shift count -1"),
                    "unexpected error message: {msg}"
                );
            }
            other => panic!(
                "expected TypeError for negative shift count, got {:?}",
                other
            ),
        }
    }

    /// Shift with a count larger than 63 must return a VM error (never panic).
    #[test]
    fn test_eval_right_shift_too_large_count_errors() {
        let env = Environment::new();
        let expr = Ast::right_shift_op(int(8), int(64));

        match eval_expr(&expr, &env) {
            Err(VmError::TypeError(msg)) => {
                assert!(
                    msg.contains("invalid shift count 64"),
                    "unexpected error message: {msg}"
                );
            }
            other => panic!(
                "expected TypeError for too-large shift count, got {:?}",
                other
            ),
        }
    }

    /// Boundary shift counts (0 and 63) remain valid.
    #[test]
    fn test_eval_shift_boundary_counts_are_valid() {
        let env = Environment::new();

        let left_zero = Ast::left_shift_op(int(1), int(0));
        match eval_expr(&left_zero, &env) {
            Ok(RuntimeValue::Int(1)) => {}
            Ok(other) => panic!("1 << 0 should be Int(1), got {:?}", other),
            Err(err) => panic!("1 << 0 should be valid, got error: {err}"),
        }

        let right_max = Ast::right_shift_op(int(i64::MAX), int(63));
        match eval_expr(&right_max, &env) {
            Ok(RuntimeValue::Int(0)) => {}
            Ok(other) => panic!("i64::MAX >> 63 should be Int(0), got {:?}", other),
            Err(err) => panic!("i64::MAX >> 63 should be valid, got error: {err}"),
        }
    }

    /// Short-circuit `And` and `Or`.
    #[test]
    fn test_eval_logical_short_circuit() {
        let env = Environment::new();

        let and_short = Ast::and_op(
            Ast::value(RuntimeValue::Bool(false)),
            ident("undefined_var"),
        );
        assert_eq!(
            eval_expr(&and_short, &env).expect("false and x"),
            RuntimeValue::Bool(false)
        );

        let or_short = Ast::or_op(
            Ast::value(RuntimeValue::Bool(true)),
            ident("also_undefined"),
        );
        assert_eq!(
            eval_expr(&or_short, &env).expect("true or x"),
            RuntimeValue::Bool(true)
        );
    }

    /// Variable lookup succeeds when defined; returns `UndefinedVariable`
    /// otherwise.
    #[test]
    fn test_variable_lookup() {
        let mut env = Environment::new();
        env.set("hp", RuntimeValue::Int(100), &DeclKind::Variable)
            .expect("set failed");

        assert_eq!(
            eval_expr(&ident("hp"), &env).expect("hp"),
            RuntimeValue::Int(100)
        );
        assert!(
            matches!(
                eval_expr(&ident("missing"), &env),
                Err(VmError::UndefinedVariable(_))
            ),
            "undefined variable should error"
        );
    }

    /// Decorator registry: registered decorator fields are applied; unknown
    /// decorator returns an error.
    #[test]
    fn test_decorator_registry_apply() {
        let mut registry = DecoratorRegistry::new();
        registry.register("mood", |args| {
            let mut m = HashMap::new();
            if let Some(RuntimeValue::Str(s)) = args.first() {
                m.insert("mood".to_string(), RuntimeValue::Str(s.clone()));
            }
            m
        });

        let env = Environment::new();
        let deco = Decorator::new("mood".to_string(), Ast::expr_list(vec![str_lit("happy")]));

        let fields = registry.apply(&deco, &env).expect("apply should succeed");
        assert!(fields.contains_key("mood"), "fields should contain 'mood'");

        let unknown_deco = Decorator::bare("ghost".to_string());
        assert!(
            registry.apply(&unknown_deco, &env).is_err(),
            "unknown decorator should error"
        );
    }

    /// Environment: `const` cannot be reassigned.
    #[test]
    fn test_const_immutability() {
        let mut env = Environment::new();
        env.set("MAX", RuntimeValue::Int(100), &DeclKind::Constant)
            .expect("first const set");
        let result = env.set("MAX", RuntimeValue::Int(200), &DeclKind::Constant);
        assert!(
            matches!(result, Err(VmError::TypeError(_))),
            "reassigning a const should be a TypeError"
        );
    }

    /// VM execution: plain assignment (`x = 2`) to a constant must error.
    #[test]
    fn test_plain_assignment_to_const_errors_at_runtime() {
        let const_decl = Ast::decl(DeclKind::Constant, ident("x"), int(1));
        let plain_assign = Ast::assign_op(ident("x"), int(2));
        let ast = Ast::block(vec![const_decl, plain_assign]);

        let mut vm = build_vm(ast);

        match vm.next(None) {
            VmStep::Error(VmError::TypeError(msg)) => {
                assert!(
                    msg.contains("cannot assign to constant 'x'"),
                    "expected const-assignment runtime error, got: {msg}"
                );
            }
            other => panic!(
                "expected VmStep::Error(TypeError) for const reassignment, got {:?}",
                other
            ),
        }
    }

    /// Environment: globals are accessible from nested scopes.
    #[test]
    fn test_globals_visible_in_nested_scope() {
        let mut env = Environment::new();
        env.set("score", RuntimeValue::Int(0), &DeclKind::Global)
            .expect("set global");
        env.push_scope();
        assert_eq!(
            env.get("score").expect("global visible in inner scope"),
            RuntimeValue::Int(0)
        );
        env.pop_scope();
    }

    /// A complete script with `DefineEnum` + `Switch` reaches the right arm.
    #[test]
    fn test_switch_on_enum_variant() {
        let enum_decl = Ast::enum_decl(
            "Direction".to_string(),
            vec![
                ("North".to_string(), TokSpan::default()),
                ("South".to_string(), TokSpan::default()),
            ],
        );

        let north_path = Ast::value(RuntimeValue::IdentPath(vec![
            "Direction".to_string(),
            "North".to_string(),
        ]));
        let dir_decl = Ast::decl(DeclKind::Variable, ident("dir"), north_path);

        let north_arm = MatchArm::new(
            MatchPattern::Value(Ast::value(RuntimeValue::Str(ParsedString::new_plain(
                "North",
            )))),
            Ast::block(vec![Ast::dialogue(
                Ast::expr_list(vec![str_lit("Alice")]),
                Ast::expr_list(vec![str_lit("going north")]),
            )]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![Ast::dialogue(
                Ast::expr_list(vec![str_lit("Alice")]),
                Ast::expr_list(vec![str_lit("other")]),
            )]),
        );
        let match_stmt = Ast::match_stmt(ident("dir"), vec![north_arm, wild_arm]);

        let ast = Ast::block(vec![enum_decl, dir_decl, match_stmt]);
        let mut vm = build_vm(ast);

        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(ps.to_string(), "going north");
                }
                other => panic!("expected 'going north', got {:?}", other),
            },
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `eval_expr_list` returns all elements of an ExprList.
    #[test]
    fn test_eval_expr_list_all_elements() {
        let env = Environment::new();
        let list = Ast::expr_list(vec![int(1), int(2), int(3)]);
        let result = eval_expr_list(&list, &env).expect("eval");
        assert_eq!(
            result,
            vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3)
            ]
        );
    }

    /// A Dialogue with multiple speakers correctly emits all of them.
    #[test]
    fn test_dialogue_multiple_speakers() {
        let speakers = Ast::expr_list(vec![str_lit("Alice"), str_lit("Bob")]);
        let lines = Ast::expr_list(vec![str_lit("Together!")]);
        let ast = Ast::dialogue(speakers, lines);

        let mut vm = build_vm(ast);
        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { speakers, .. } => {
                assert_eq!(speakers.len(), 2, "expected 2 speakers");
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    // ── Script-decorator integration tests ────────────────────────────────────

    /// A script-defined decorator that writes `event["camera_shake"] = amount`
    /// should produce a Dialogue event with that field set.
    #[test]
    fn test_script_decorator_mutates_event_fields() {
        use crate::parser::ast::{DecoratorParam, EventConstraint};

        let event_ident = Ast::value(RuntimeValue::IdentPath(vec!["event".to_string()]));
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("camera_shake"),
        ));
        let amount_ident = Ast::value(RuntimeValue::IdentPath(vec!["amount".to_string()]));
        let subscript_assign = Ast::subscript_assign(event_ident, key_ast, amount_ident);
        let body = Ast::block(vec![subscript_assign]);

        let decorator_def = Ast::decorator_def(
            "shake".to_string(),
            EventConstraint::Any,
            vec![DecoratorParam {
                name: "amount".to_string(),
                type_annotation: None,
            }],
            body,
        );

        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Watch out!")]);
        let deco = Decorator::new(
            "shake".to_string(),
            Ast::expr_list(vec![Ast::value(RuntimeValue::Float(0.5))]),
        );
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let ast = Ast::block(vec![decorator_def, dialogue]);
        let mut vm = build_vm(ast);

        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { fields, .. } => {
                assert!(
                    fields.contains_key("camera_shake"),
                    "expected 'camera_shake' in fields, got {:?}",
                    fields
                );
                assert_eq!(
                    fields.get("camera_shake"),
                    Some(&RuntimeValue::Float(0.5)),
                    "camera_shake should be Float(0.5)"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// A VM built with an empty Rust registry but a script-defined decorator
    /// must NOT return `VmError::UnknownDecorator`.
    #[test]
    fn test_script_decorator_does_not_require_rust_registration() {
        use crate::parser::ast::EventConstraint;

        let decorator_def = Ast::decorator_def(
            "noop".to_string(),
            EventConstraint::Any,
            vec![],
            Ast::block(vec![]),
        );

        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hi")]);
        let deco = Decorator::new("noop".to_string(), Ast::expr_list(vec![]));
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let ast = Ast::block(vec![decorator_def, dialogue]);
        let graph = Compiler::compile(&ast).expect("compile ok");

        let result = Vm::new(graph, empty_registry());
        assert!(
            result.is_ok(),
            "script-defined decorator should not require Rust registration, got {:?}",
            result
        );
    }

    /// Evaluating a Map literal `:{\"key\": 42}` returns a `RuntimeValue::Map`.
    #[test]
    fn test_map_literal_eval() {
        let env = Environment::new();
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("key"),
        ));
        let val_ast = int(42);
        let map_ast = Ast::map(vec![(key_ast, val_ast)]);

        let result = eval_expr(&map_ast, &env).expect("map eval");
        match result {
            RuntimeValue::Map(m) => {
                assert_eq!(m.len(), 1);
                assert_eq!(*m["key"], RuntimeValue::Int(42));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    /// `Map[key]` subscript read returns the correct value.
    #[test]
    fn test_subscript_read() {
        let mut env = Environment::new();

        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        ));
        let val_ast = int(99);
        let map_ast = Ast::map(vec![(key_ast, val_ast)]);
        env.set("m", eval_expr(&map_ast, &env).unwrap(), &DeclKind::Variable)
            .unwrap();

        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()])),
            Ast::value(RuntimeValue::Str(
                crate::lexer::strings::ParsedString::new_plain("a"),
            )),
        );

        let result = eval_expr(&subscript, &env).expect("subscript read");
        assert_eq!(result, RuntimeValue::Int(99));
    }

    /// Subscript assignment `m["a"] = 99` mutates the map stored in the env.
    #[test]
    fn test_subscript_assign_mutates_map() {
        let mut env = Environment::new();

        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        ));
        let val_ast = int(1);
        let map_ast = Ast::map(vec![(key_ast, val_ast)]);
        env.set("m", eval_expr(&map_ast, &env).unwrap(), &DeclKind::Variable)
            .unwrap();

        let obj = Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()]));
        let key = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("a"),
        ));
        let val = int(99);
        eval_subscript_assign(&obj, &key, &val, &mut env).expect("subscript assign");

        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["m".to_string()])),
            Ast::value(RuntimeValue::Str(
                crate::lexer::strings::ParsedString::new_plain("a"),
            )),
        );
        let result = eval_expr(&subscript, &env).expect("read back");
        assert_eq!(
            result,
            RuntimeValue::Int(99),
            "m[\"a\"] should be 99 after assign"
        );
    }

    #[test]
    fn test_list_literal_eval_via_vm() {
        // Verify that list literals evaluate to RuntimeValue::List.
        let env = Environment::default();
        let ast = Ast::list(vec![
            Ast::value(RuntimeValue::Int(10)),
            Ast::value(RuntimeValue::Int(20)),
            Ast::value(RuntimeValue::Int(30)),
        ]);
        let result = eval_expr(&ast, &env).expect("list eval");
        assert_eq!(
            result,
            RuntimeValue::List(vec![
                RuntimeValue::Int(10),
                RuntimeValue::Int(20),
                RuntimeValue::Int(30),
            ])
        );
    }

    #[test]
    fn test_subscript_assign_mutates_list() {
        let mut env = Environment::new();
        // Declare: let xs: list = [1, 2, 3]
        env.set(
            "xs",
            RuntimeValue::List(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ]),
            &DeclKind::Variable,
        )
        .unwrap();

        // xs[1] = 99
        let obj = Ast::value(RuntimeValue::IdentPath(vec!["xs".into()]));
        let key = Ast::value(RuntimeValue::Int(1));
        let val = Ast::value(RuntimeValue::Int(99));
        eval_subscript_assign(&obj, &key, &val, &mut env).expect("subscript assign");

        // Read back xs[1]
        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["xs".into()])),
            Ast::value(RuntimeValue::Int(1)),
        );
        let result = eval_expr(&subscript, &env).expect("subscript read");
        assert_eq!(result, RuntimeValue::Int(99));
    }

    #[test]
    fn test_subscript_assign_list_negative_index() {
        let mut env = Environment::new();
        env.set(
            "xs",
            RuntimeValue::List(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ]),
            &DeclKind::Variable,
        )
        .unwrap();

        // xs[-1] = 42  — should update the last element
        let obj = Ast::value(RuntimeValue::IdentPath(vec!["xs".into()]));
        let key = Ast::value(RuntimeValue::Int(-1));
        let val = Ast::value(RuntimeValue::Int(42));
        eval_subscript_assign(&obj, &key, &val, &mut env).expect("negative index assign");

        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["xs".into()])),
            Ast::value(RuntimeValue::Int(-1)),
        );
        let result = eval_expr(&subscript, &env).expect("negative subscript read");
        assert_eq!(result, RuntimeValue::Int(42));
    }

    #[test]
    fn test_subscript_assign_list_out_of_bounds() {
        let mut env = Environment::new();
        env.set(
            "xs",
            RuntimeValue::List(vec![RuntimeValue::Int(1)]),
            &DeclKind::Variable,
        )
        .unwrap();

        let obj = Ast::value(RuntimeValue::IdentPath(vec!["xs".into()]));
        let key = Ast::value(RuntimeValue::Int(5));
        let val = Ast::value(RuntimeValue::Int(0));
        let err =
            eval_subscript_assign(&obj, &key, &val, &mut env).expect_err("should be out of bounds");
        assert!(matches!(
            err,
            VmError::IndexOutOfBounds { index: 5, len: 1 }
        ));
    }

    #[test]
    fn test_label_values_preseeded_in_env() {
        let body = Ast::block(vec![]);
        let labeled = Ast::labeled_block("scene_one".to_string(), body);
        let script = Ast::block(vec![labeled]);

        let graph = Compiler::compile(&script).expect("compile");
        let vm = Vm::new(graph, empty_registry()).expect("vm construction");

        let expected_node_id = *vm.graph().labels.get("scene_one").expect("label in graph");
        let val = vm.env().get("scene_one").expect("label should be in env");
        assert_eq!(
            val,
            RuntimeValue::Label {
                name: "scene_one".to_string(),
                node_id: expected_node_id,
            }
        );
    }

    #[test]
    fn test_label_value_equality() {
        // Equality is by node_id — the canonical execution reference.
        assert!(values_equal(
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeIndex::new(0),
            },
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeIndex::new(0),
            },
        ));
        // Different node indices → not equal, even if names match.
        assert!(!values_equal(
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeIndex::new(0),
            },
            &RuntimeValue::Label {
                name: "a".to_string(),
                node_id: NodeIndex::new(1),
            },
        ));
    }

    #[test]
    fn test_label_value_in_decorator_fields() {
        use crate::parser::ast::{DecoratorParam, EventConstraint};

        let fallback_label = Ast::labeled_block("fallback".to_string(), Ast::block(vec![]));

        let event_ident = Ast::value(RuntimeValue::IdentPath(vec!["event".to_string()]));
        let key_ast = Ast::value(RuntimeValue::Str(
            crate::lexer::strings::ParsedString::new_plain("next"),
        ));
        let param_ident = Ast::value(RuntimeValue::IdentPath(vec!["fallback_label".to_string()]));
        let subscript_assign = Ast::subscript_assign(event_ident, key_ast, param_ident);
        let dec_body = Ast::block(vec![subscript_assign]);

        let decorator_def = Ast::decorator_def(
            "timed".to_string(),
            EventConstraint::Any,
            vec![DecoratorParam {
                name: "fallback_label".to_string(),
                type_annotation: None,
            }],
            dec_body,
        );

        let speakers = Ast::expr_list(vec![str_lit("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hurry!")]);
        let deco = Decorator::new(
            "timed".to_string(),
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "fallback".to_string(),
            ]))]),
        );
        let dialogue = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco],
        );

        let ast = Ast::block(vec![fallback_label, decorator_def, dialogue]);
        let mut vm = build_vm(ast);

        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { fields, .. } => {
                assert!(
                    fields.contains_key("next"),
                    "expected 'next' in fields, got {:?}",
                    fields
                );
                match fields.get("next") {
                    Some(RuntimeValue::Label { name, .. }) => assert_eq!(
                        name, "fallback",
                        "expected Label pointing at 'fallback', got name '{name}'"
                    ),
                    other => panic!("expected Label for 'next' field, got {:?}", other),
                }
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// `jump label and return` (compiled to LetCall) pushes a call frame,
    /// executes the label body, and after `return` resumes at the continuation.
    #[test]
    fn test_subroutine_call_and_return() {
        let speakers1 = Ast::expr_list(vec![str_lit("Alice")]);
        let lines1 = Ast::expr_list(vec![str_lit("Hello from greet")]);
        let greet_dialogue = Ast::dialogue(speakers1, lines1);
        let greet_body = Ast::block(vec![greet_dialogue, Ast::return_stmt(None)]);
        let greet_label = Ast::labeled_block("greet".to_string(), greet_body);

        let call_jump = Ast::jump_stmt("greet".to_string(), true);

        let speakers2 = Ast::expr_list(vec![str_lit("Alice")]);
        let lines2 = Ast::expr_list(vec![str_lit("After call")]);
        let after_dialogue = Ast::dialogue(speakers2, lines2);

        let main_return = Ast::return_stmt(None);

        let ast = Ast::block(vec![call_jump, after_dialogue, main_return, greet_label]);
        let mut vm = build_vm(ast);

        let ev1 = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected first Event, got {:?}", other),
        };
        match ev1 {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => assert_eq!(
                    ps.to_string(),
                    "Hello from greet",
                    "first event should be the greet dialogue"
                ),
                other => panic!("expected Str, got {:?}", other),
            },
            other => panic!("expected Dialogue for greet, got {:?}", other),
        }

        let ev2 = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected second Event, got {:?}", other),
        };
        match ev2 {
            Event::Dialogue { lines, .. } => match &lines[0] {
                RuntimeValue::Str(ps) => assert_eq!(
                    ps.to_string(),
                    "After call",
                    "second event should be the post-call dialogue"
                ),
                other => panic!("expected Str, got {:?}", other),
            },
            other => panic!("expected Dialogue after return, got {:?}", other),
        }

        assert!(
            matches!(vm.next(None), VmStep::Ended),
            "script should end after second dialogue"
        );
    }

    /// `let result = jump double and return` binds the subroutine's return
    /// value to `result` and execution continues after the call site.
    #[test]
    fn test_let_call_captures_return_value() {
        let double_body = Ast::block(vec![Ast::return_stmt(Some(int(42)))]);
        let double_label = Ast::labeled_block("double".to_string(), double_body);

        let let_call = Ast::let_call("result".to_string(), "double".to_string());

        let speakers = Ast::expr_list(vec![str_lit("Bot")]);
        let lines = Ast::expr_list(vec![ident("result")]);
        let dialogue = Ast::dialogue(speakers, lines);

        let ast = Ast::block(vec![let_call, dialogue, double_label]);
        let mut vm = build_vm(ast);

        let ev = match vm.next(None) {
            VmStep::Event(e) => e,
            other => panic!("expected Event, got {:?}", other),
        };
        match ev {
            Event::Dialogue { lines, .. } => {
                assert_eq!(
                    lines,
                    vec![RuntimeValue::Int(42)],
                    "result should be the return value 42"
                );
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }

        assert!(
            matches!(vm.next(None), VmStep::Ended),
            "script should end after dialogue"
        );
    }

    #[test]
    fn test_todo_bang_ends_script() {
        use crate::ir::{IrEdge, IrGraph, IrNodeKind};
        use petgraph::stable_graph::StableGraph;

        let mut g: StableGraph<IrNodeKind, IrEdge> = StableGraph::new();
        let todo_id = g.add_node(IrNodeKind::Todo);
        let graph = IrGraph {
            graph: g,
            entry: Some(todo_id),
            labels: HashMap::new(),
            cluster_names: HashMap::new(),
            label_sources: HashMap::new(),
        };
        let registry = DecoratorRegistry::new();
        let mut vm = Vm::new(graph, registry).unwrap();
        let result = vm.next(None);
        assert!(
            matches!(result, VmStep::Ended),
            "todo!() should end the script, got: {result:?}"
        );
    }

    #[test]
    fn test_end_bang_ends_script() {
        use crate::ir::{IrEdge, IrGraph, IrNodeKind};
        use petgraph::stable_graph::StableGraph;

        let mut g: StableGraph<IrNodeKind, IrEdge> = StableGraph::new();
        let end_id = g.add_node(IrNodeKind::End);
        let graph = IrGraph {
            graph: g,
            entry: Some(end_id),
            labels: HashMap::new(),
            cluster_names: HashMap::new(),
            label_sources: HashMap::new(),
        };
        let registry = DecoratorRegistry::new();
        let mut vm = Vm::new(graph, registry).unwrap();
        let result = vm.next(None);
        assert!(
            matches!(result, VmStep::Ended),
            "end!() should end the script, got: {result:?}"
        );
    }

    #[test]
    fn test_jump_without_return_does_not_push_frame() {
        let dest_label = Ast::labeled_block("dest".to_string(), Ast::block(vec![]));
        let jump = Ast::jump_stmt("dest".to_string(), false);
        let ast = Ast::block(vec![dest_label, jump]);

        let graph = Compiler::compile(&ast).expect("compile failed");

        // Plain jump must emit IrNodeKind::Jump, never LetCall.
        let has_let_call = graph
            .graph
            .node_weights()
            .any(|k| matches!(k, IrNodeKind::LetCall { .. }));
        assert!(
            !has_let_call,
            "plain `jump label` must not emit a LetCall node; graph = {graph:?}"
        );

        let has_jump = graph
            .graph
            .node_weights()
            .any(|k| matches!(k, IrNodeKind::Jump));
        assert!(
            has_jump,
            "plain `jump label` must emit a Jump node; graph = {graph:?}"
        );
    }

    #[test]
    fn test_extern_provided_runs_ok() {
        use crate::compiler::Compiler;
        use crate::compiler::loader::parse_source;
        use crate::runtime::value::RuntimeValue;
        use crate::vm::Vm;
        use crate::vm::registry::DecoratorRegistry;

        let src = r#"
extern narrator: str
label start {
    narrator: "Hello"
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");
        vm.provide_extern(
            "narrator",
            RuntimeValue::Str(ParsedString::new_plain("Narrator")),
        );
        // Should run without error
        let step = vm.next(None);
        assert!(
            !matches!(step, VmStep::Error(_)),
            "unexpected error: {:?}",
            step
        );
    }

    #[test]
    fn test_extern_not_provided_returns_error() {
        use crate::compiler::Compiler;
        use crate::compiler::loader::parse_source;
        use crate::vm::Vm;
        use crate::vm::VmError;
        use crate::vm::registry::DecoratorRegistry;

        let src = r#"
extern narrator: str
label start {
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");
        // Do NOT call provide_extern — should error

        // Step until we hit an error or end
        let mut found_error = false;
        for _ in 0..10 {
            match vm.next(None) {
                VmStep::Error(VmError::ExternNotProvided(name)) => {
                    assert_eq!(name, "narrator");
                    found_error = true;
                    break;
                }
                VmStep::Error(e) => panic!("unexpected error: {:?}", e),
                VmStep::Ended => break,
                VmStep::Event(_) => {}
            }
        }
        assert!(found_error, "expected ExternNotProvided error");
    }
}
