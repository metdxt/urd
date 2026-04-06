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
mod range_methods;
pub mod registry;
mod str_methods;

pub use env::{CallFrame, Environment};
pub use eval::{eval_expr, eval_expr_list};
pub use registry::DecoratorRegistry;

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use petgraph::Direction;
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;
use thiserror::Error;

use crate::loc::Localizer;
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

    /// A `match` expression had no arm that matched the scrutinee and no
    /// wildcard / default arm was present.
    #[error("non-exhaustive match: {0}")]
    NonExhaustiveMatch(String),

    /// The script exceeded its step budget (infinite loop guard).
    ///
    /// The default budget is 1 000 000 steps. Call [`Vm::set_step_budget`] to
    /// raise, lower, or disable the limit.
    #[error(
        "execution budget exceeded: script executed too many steps without ending (possible infinite loop)"
    )]
    BudgetExceeded,

    /// The call stack depth limit was reached.
    ///
    /// The default limit is 256 frames. Indicates unbounded mutual recursion or
    /// a pathological `LetCall` cycle in the script.
    #[error("call stack overflow: maximum call depth ({0}) exceeded")]
    StackOverflow(usize),
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
    /// Roll `count` dice each with `sides` faces and return each individual result.
    ///
    /// Each element is in the range `1..=sides`. An empty `Vec` is returned
    /// when `count == 0`. Behaviour for `sides == 0` is implementation-defined.
    fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64>;

    /// Roll and return the total (sum of all individual results).
    ///
    /// # Overflow behaviour
    ///
    /// The default implementation uses checked addition and saturates to
    /// `i64::MAX` on overflow.  This is a limitation of the `i64` return
    /// type — callers that need proper error propagation should use
    /// [`Self::roll_individual`] and sum the results with
    /// [`crate::vm::eval::checked_sum_rolls`] instead.
    fn roll(&self, count: u32, sides: u32) -> i64 {
        self.roll_individual(count, sides)
            .iter()
            .try_fold(0i64, |acc, &x| acc.checked_add(x))
            .unwrap_or(i64::MAX)
    }
}

/// Default [`DiceRoller`] implementation backed by [`rand::rng`].
///
/// Used by the VM unless the host replaces it via dependency injection.
pub struct DefaultDiceRoller;

impl DiceRoller for DefaultDiceRoller {
    fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64> {
        /// Maximum number of dice the default roller will allocate.
        const MAX_DICE: u32 = 255;

        if count == 0 {
            return vec![];
        }
        if sides == 0 {
            // Should be unreachable: eval_runtime_value guards `sides >= 1`.
            // Return zeros defensively rather than panicking in random_range.
            return vec![0; count.min(MAX_DICE) as usize];
        }
        let capped = count.min(MAX_DICE);
        use rand::RngExt;
        let mut rng = rand::rng();
        (0..capped)
            .map(|_| rng.random_range(1..=sides) as i64)
            .collect()
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
    /// Steps remaining before [`VmError::BudgetExceeded`] fires.
    /// `None` means unlimited (use only in tests or trusted contexts).
    steps_remaining: Option<u64>,
    /// Maximum allowed call stack depth before [`VmError::StackOverflow`] fires.
    max_call_depth: usize,
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
pub struct Vm {
    /// The compiled IR graph (immutable during execution).
    graph: IrGraph,
    /// Pre-built map from label name → the `Next`-successor of the matching
    /// [`IrNodeKind::ExitScope`] node. Replaces the former O(n) scan in
    /// `find_exit_scope_next`.
    exit_scope_map: HashMap<String, Option<NodeIndex>>,
    /// All mutable execution state.
    state: VmState,
    /// Optional localizer for translating dialogue and choice events.
    localizer: Option<Arc<dyn Localizer>>,
}

impl std::fmt::Debug for Vm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vm")
            .field("graph", &self.graph)
            .field("exit_scope_map", &self.exit_scope_map)
            .field("state", &self.state)
            .field("localizer", &self.localizer.as_ref().map(|_| "<Localizer>"))
            .finish()
    }
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

        let env = Environment::new();

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
                steps_remaining: Some(1_000_000),
                max_call_depth: 256,
            },
            localizer: None,
        })
    }

    /// Attach a [`Localizer`] to the VM.
    ///
    /// When a localizer is present, the VM calls
    /// [`Localizer::localize`] for every [`Event::Dialogue`] and every
    /// [`ChoiceEvent`] that has a `loc_id`. If the localizer returns
    /// `Some(text)`, it is stored in `localized_text` / `localized_label`
    /// on the emitted event.
    ///
    /// Call this before the first [`Vm::next`] invocation.
    pub fn with_localizer(mut self, localizer: Arc<dyn Localizer>) -> Self {
        self.localizer = Some(localizer);
        self
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
        let localizer = &self.localizer;

        loop {
            // ── Execution budget guard ────────────────────────────────────────
            if let Some(ref mut remaining) = state.steps_remaining {
                if *remaining == 0 {
                    return VmStep::Error(VmError::BudgetExceeded);
                }
                *remaining -= 1;
            }

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
                IrNodeKind::Assign {
                    var,
                    scope,
                    expr,
                    fluent_alias,
                } => {
                    let value = match eval_expr(expr, &state.env) {
                        Ok(v) => v,
                        Err(e) => return VmStep::Error(e),
                    };
                    if let Err(e) = state.env.set(var, value.clone(), scope) {
                        return VmStep::Error(e);
                    }
                    if let Some(alias) = fluent_alias {
                        state.env.set_fluent_binding(alias, value);
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

                    // For Roll scrutinees precompute: scalar sum (used by Value
                    // and Range arms) and the individual die vector (used by Array arms).
                    let (scalar_val, roll_individuals): (RuntimeValue, Option<Vec<i64>>) =
                        match &scrutinee_val {
                            RuntimeValue::Roll(dice) => {
                                let sum = match eval::checked_sum_rolls(dice) {
                                    Ok(s) => s,
                                    Err(e) => return VmStep::Error(e),
                                };
                                (RuntimeValue::Int(sum), Some(dice.clone()))
                            }
                            other => (other.clone(), None),
                        };

                    let mut matched = false;
                    for (i, arm) in arms.iter().enumerate() {
                        // Returns (matched, optional_binding_value).
                        let (is_match, binding_val) = match &arm.pattern {
                            MatchPattern::Wildcard => (true, None),
                            MatchPattern::Value(pat_ast) => {
                                let ok = match eval_expr(pat_ast, &state.env) {
                                    // Compare against the scalar (sum for Roll, value otherwise).
                                    Ok(pat_val) => values_equal(&scalar_val, &pat_val),
                                    Err(_) => false,
                                };
                                (ok, None)
                            }
                            MatchPattern::Range {
                                start,
                                end,
                                inclusive,
                                binding,
                            } => {
                                let start_i =
                                    eval_expr(start, &state.env).ok().and_then(|v| match v {
                                        RuntimeValue::Int(i) => Some(i),
                                        _ => None,
                                    });
                                let end_i = eval_expr(end, &state.env).ok().and_then(|v| match v {
                                    RuntimeValue::Int(i) => Some(i),
                                    _ => None,
                                });
                                let scalar = match &scalar_val {
                                    RuntimeValue::Int(n) => Some(*n),
                                    RuntimeValue::Float(f) => {
                                        if f.is_nan() {
                                            None
                                        } else {
                                            Some(*f as i64)
                                        }
                                    }
                                    _ => None,
                                };
                                match (start_i, end_i, scalar) {
                                    (Some(s), Some(e), Some(n)) => {
                                        let in_range = if *inclusive {
                                            s <= n && n <= e
                                        } else {
                                            s <= n && n < e
                                        };
                                        // Capture the matched scalar for `as name` bindings.
                                        let bnd = if in_range {
                                            binding
                                                .as_ref()
                                                .map(|name| (name.clone(), RuntimeValue::Int(n)))
                                        } else {
                                            None
                                        };
                                        (in_range, bnd)
                                    }
                                    _ => (false, None),
                                }
                            }
                            MatchPattern::Array(elems) => {
                                let ok = match &roll_individuals {
                                    Some(dice) if dice.len() == elems.len() => {
                                        elems.iter().zip(dice.iter()).all(
                                            |(pat_ast_opt, &die_val)| match pat_ast_opt {
                                                None => true, // wildcard — always matches
                                                Some(pat_ast) => matches!(
                                                    eval_expr(pat_ast, &state.env),
                                                    Ok(RuntimeValue::Int(v)) if v == die_val
                                                ),
                                            },
                                        )
                                    }
                                    _ => false,
                                };
                                (ok, None)
                            }
                        };
                        if is_match {
                            // Inject any `as name` binding directly into the current scope so it
                            // is immediately visible inside the arm body.  Match arm bodies are
                            // plain blocks (no EnterScope wrapper), so deferring to `pending_binding`
                            // would leave the variable inaccessible.  The variable lives in the
                            // enclosing scope, which is consistent with how all variables in plain
                            // blocks behave in Urd (they are not automatically dropped on arm exit).
                            if let Some((name, val)) = binding_val {
                                if let Err(e) = state.env.set(&name, val, &DeclKind::Variable) {
                                    return VmStep::Error(e);
                                }
                            }
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
                        match default_target {
                            Some(target) => state.cursor = Some(target),
                            None => {
                                return VmStep::Error(VmError::NonExhaustiveMatch(
                                    "no arm matched and no wildcard/default present".into(),
                                ));
                            }
                        }
                    }
                }

                // ── Unconditional jump ───────────────────────────────────────
                IrNodeKind::Jump => {
                    // Tail-transfer semantics: discard any label scopes accumulated since
                    // the last LetCall boundary (or the script root if there is none).
                    // Each LetCall frame records `scope_depth` = the depth _before_ its
                    // own push_scope, so the "live base" for this context is
                    // `scope_depth + 1`.  Without any subroutine on the stack, the base is
                    // the root scope at depth 1.
                    //
                    // NOTE: a LetCall frame that is orphaned by a jump-without-return inside
                    // the subroutine body is a known limitation: the frame remains on the
                    // call_stack until the next ExitScope or Return that depth-matches it.
                    let target_depth = state
                        .call_stack
                        .last()
                        .map(|f| f.scope_depth + 1)
                        .unwrap_or(1);
                    while state.env.depth() > target_depth {
                        if let Err(e) = state.env.pop_scope() {
                            return VmStep::Error(e);
                        }
                    }
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
                                if let Err(e) = state.env.pop_scope() {
                                    return VmStep::Error(e);
                                }
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

                    // Guard call stack depth before pushing a new frame.
                    if state.call_stack.len() >= state.max_call_depth {
                        return VmStep::Error(VmError::StackOverflow(state.max_call_depth));
                    }

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
                IrNodeKind::EnterScope { .. } => {
                    // Only manage scope; CallFrame is pushed exclusively by LetCall for
                    // subroutine calls.  Plain Jump transfers never push a frame — they are
                    // tail-call semantics.
                    state.env.push_scope();
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Exit labeled-block scope ─────────────────────────────────
                IrNodeKind::ExitScope { label } => {
                    if let Err(e) = state.env.pop_scope() {
                        return VmStep::Error(e);
                    }
                    // After pop_scope(), current depth equals the depth at which LetCall
                    // pushed its scope.  If the top frame's scope_depth matches, this
                    // ExitScope is the fall-through exit of a subroutine; pop the frame
                    // and return to the call-site continuation.
                    // If there is no matching frame, the label was entered via a plain
                    // Jump (tail-transfer) — continue at the graph's natural successor.
                    let current_depth = state.env.depth();
                    let frame_matches = state
                        .call_stack
                        .last()
                        .is_some_and(|f| f.scope_depth == current_depth);
                    let next_id = if frame_matches {
                        state.call_stack.pop().and_then(|f| f.return_cursor)
                    } else {
                        exit_scope_map.get(label.as_str()).and_then(|v| *v)
                    };
                    state.cursor = next_id;
                }

                // ── Block-scope push (if/match branches) ─────────────────
                IrNodeKind::PushScope => {
                    state.env.push_scope();
                    state.cursor = next_of(graph, node_idx);
                }

                // ── Block-scope pop (if/match branches) ──────────────────
                IrNodeKind::PopScope => {
                    if let Err(e) = state.env.pop_scope() {
                        return VmStep::Error(e);
                    }
                    state.cursor = next_of(graph, node_idx);
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

                    let fluent_vars = collect_fluent_vars(&state.env, lines);
                    let localized_text = localizer.as_ref().and_then(|loc| {
                        loc_id
                            .as_deref()
                            .and_then(|id| loc.localize(id, &fluent_vars))
                    });
                    state.cursor = next_of(graph, node_idx);
                    return VmStep::Event(Event::Dialogue {
                        speakers: speakers_vec,
                        lines: lines_vec,
                        fields,
                        loc_id: loc_id.clone(),
                        fluent_vars,
                        localized_text,
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
                            // Count only non-default options (visible to the host).
                            let real_count = options.iter().filter(|o| !o.is_default).count();
                            if idx >= real_count {
                                log::warn!(
                                    "Choice index {} out of bounds (len={}); \
                                         re-emitting Choice event",
                                    idx,
                                    real_count
                                );
                                // Re-emit without advancing.
                                return build_choice_event(
                                    options,
                                    decorators,
                                    loc_id,
                                    &state.env,
                                    &state.registry,
                                    localizer.as_ref(),
                                );
                            }
                            // Map host-visible index (among non-default options)
                            // to the IR graph index (which includes defaults).
                            let graph_idx = {
                                let mut real_idx = 0;
                                let mut found = None;
                                for (i, opt) in options.iter().enumerate() {
                                    if opt.is_default {
                                        continue;
                                    }
                                    if real_idx == idx {
                                        found = Some(i);
                                        break;
                                    }
                                    real_idx += 1;
                                }
                                match found {
                                    Some(gi) => gi,
                                    None => {
                                        // Should be unreachable after the bounds
                                        // check above, but handle gracefully.
                                        log::error!(
                                            "Choice index {idx} passed bounds check \
                                             but no matching non-default option found"
                                        );
                                        return build_choice_event(
                                            options,
                                            decorators,
                                            loc_id,
                                            &state.env,
                                            &state.registry,
                                            localizer.as_ref(),
                                        );
                                    }
                                }
                            };
                            // Follow the Option(graph_idx) edge to the chosen
                            // option's entry.
                            let entry = graph
                                .graph
                                .edges_directed(node_idx, Direction::Outgoing)
                                .find(|e| {
                                    if let IrEdge::Option(i) = e.weight() {
                                        *i == graph_idx
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
                                // Already pending — check for a default option.
                                let default_entry = options
                                    .iter()
                                    .enumerate()
                                    .find(|(_, opt)| opt.is_default)
                                    .and_then(|(i, _)| {
                                        graph
                                            .graph
                                            .edges_directed(
                                                node_idx,
                                                Direction::Outgoing,
                                            )
                                            .find(|e| {
                                                matches!(e.weight(), IrEdge::Option(opt_idx) if *opt_idx == i)
                                            })
                                            .map(|e| e.target())
                                    });

                                if let Some(entry) = default_entry {
                                    // Follow the default option.
                                    state.pending_choice = None;
                                    state.cursor = Some(entry);
                                    // Continue loop — not an observable event.
                                } else {
                                    // No default — re-emit as before.
                                    log::warn!(
                                        "Choice at node {:?} is already pending; \
                                             re-emitting Event::Choice without advancing",
                                        node_idx
                                    );
                                    return build_choice_event(
                                        options,
                                        decorators,
                                        loc_id,
                                        &state.env,
                                        &state.registry,
                                        localizer.as_ref(),
                                    );
                                }
                            } else {
                                state.pending_choice = Some(node_idx);
                                return build_choice_event(
                                    options,
                                    decorators,
                                    loc_id,
                                    &state.env,
                                    &state.registry,
                                    localizer.as_ref(),
                                );
                            }
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

    /// Sets the maximum number of VM steps before
    /// [`VmStep::Error(VmError::BudgetExceeded)`] is returned.
    ///
    /// Pass `None` for unlimited execution. Only use `None` in tests or
    /// other trusted contexts where an infinite loop cannot occur.
    ///
    /// The default budget is `Some(1_000_000)`.
    pub fn set_step_budget(&mut self, budget: Option<u64>) {
        self.state.steps_remaining = budget;
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
            exec_stmt_sync(block, env, event)?;
            refresh_event_snapshot(env, event)?;
            return Ok(());
        }
    };

    for stmt in stmts {
        exec_stmt_sync(stmt, env, event)?;
        // After each statement, refresh the `event` binding in `env` so that
        // subsequent reads (e.g. `event["x"]`) see the latest mutations
        // applied by `exec_stmt_sync` to the canonical `event` map.
        refresh_event_snapshot(env, event)?;
    }
    Ok(())
}

/// Re-synchronises the `"event"` variable in `env` with the canonical `event`
/// map so that expression evaluation sees the most recent mutations.
fn refresh_event_snapshot(
    env: &mut Environment,
    event: &HashMap<String, RuntimeValue>,
) -> Result<(), VmError> {
    let snapshot: HashMap<String, Box<RuntimeValue>> = event
        .iter()
        .map(|(k, v)| (k.clone(), Box::new(v.clone())))
        .collect();
    env.set("event", RuntimeValue::Map(snapshot), &DeclKind::Variable)
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

    // Arity check: parameter count must match argument count exactly.
    if def.params.len() != args.len() {
        return Err(VmError::TypeError(format!(
            "decorator '{}' expects {} argument(s), got {}",
            def.name,
            def.params.len(),
            args.len()
        )));
    }

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

// ─── Fluent variable helpers ──────────────────────────────────────────────────

/// Collect Fluent variable bindings from the current environment.
///
/// Merges two sources:
/// 1. `@fluent`-tagged bindings accumulated in the environment's fluent scope.
/// 2. Variable names referenced via string interpolation in `lines_ast` —
///    resolved against the environment and added if not already present.
///
/// The result is ready to pass to a [`Localizer`].
fn collect_fluent_vars(
    env: &Environment,
    lines_ast: &crate::parser::ast::Ast,
) -> HashMap<String, RuntimeValue> {
    let mut vars = env.collect_fluent_bindings();

    for (path, format) in collect_interpolations(lines_ast) {
        // Full dotted path → Fluent key: dots become hyphens (e.g. "inv.gold" → "inv-gold").
        // Single-segment paths are unchanged.
        let key = path.replace('.', "-");

        // Use the shared resolution helper so that namespace paths
        // ("inv.gold" → env key "inv::gold") are handled identically to
        // the inline string evaluator, not just via flat env.get().
        if let Ok(raw_val) = eval::resolve_interp_path(&path, env) {
            let value = if let Some(ref fmt) = format {
                // Pre-format using the same logic as the inline string evaluator
                // so Fluent receives the already-formatted string (e.g. "30.00")
                // rather than a raw number it would format independently.
                let formatted = eval::format_runtime_value(&raw_val, Some(fmt));
                RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain(&formatted))
            } else {
                raw_val
            };
            vars.insert(key, value);
        }
    }

    vars
}

/// Recursively collect all `(path, format)` interpolation pairs from an AST.
/// Deduplicates by path; for collisions the first format specifier seen wins.
fn collect_interpolations(ast: &crate::parser::ast::Ast) -> Vec<(String, Option<String>)> {
    use crate::lexer::strings::StringPart;

    fn inner(
        ast: &crate::parser::ast::Ast,
        out: &mut std::collections::HashMap<String, Option<String>>,
    ) {
        if let AstContent::Value(RuntimeValue::Str(ps)) = ast.content() {
            for part in ps.parts() {
                if let StringPart::Interpolation(interp) = part {
                    out.entry(interp.path.clone())
                        .or_insert_with(|| interp.format.clone());
                }
            }
        }
        for child in ast.children() {
            inner(child, out);
        }
    }

    let mut map = std::collections::HashMap::new();
    inner(ast, &mut map);
    let mut result: Vec<(String, Option<String>)> = map.into_iter().collect();
    result.sort_by(|a, b| a.0.cmp(&b.0));
    result
}

/// Extract `{varname}` and `{varname:fmt}` interpolation placeholders from a
/// plain option-label string (e.g. `"Buy for {price:.2} gold"` →
/// `[("price", Some(".2"))]`).
///
/// Splits each brace interior on the first `:` to separate the variable path
/// from an optional format specifier. The path is validated (alphanumeric,
/// `_`, `.` only); the format specifier is kept verbatim.
fn extract_label_interp_vars(label: &str) -> Vec<(String, Option<String>)> {
    let mut vars = Vec::new();
    let bytes = label.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'{' {
            let start = i + 1;
            if let Some(rel_end) = label[start..].find('}') {
                let inner = label[start..start + rel_end].trim();
                if !inner.is_empty() {
                    let (path_str, fmt) = if let Some(colon) = inner.find(':') {
                        let p = inner[..colon].trim();
                        let f = inner[colon + 1..].trim();
                        (
                            p,
                            if f.is_empty() {
                                None
                            } else {
                                Some(f.to_string())
                            },
                        )
                    } else {
                        (inner, None)
                    };
                    if !path_str.is_empty()
                        && path_str
                            .chars()
                            .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
                    {
                        vars.push((path_str.to_string(), fmt));
                    }
                }
                i = start + rel_end + 1;
                continue;
            }
        }
        i += 1;
    }
    vars
}

// ─── Choice event builder ─────────────────────────────────────────────────────

/// Builds an [`Event::Choice`] from a set of choice options and decorators.
fn build_choice_event(
    options: &[IrChoiceOption],
    decorators: &[Decorator],
    loc_id: &Option<String>,
    env: &Environment,
    registry: &DecoratorRegistry,
    localizer: Option<&Arc<dyn Localizer>>,
) -> VmStep {
    // Evaluate top-level choice decorators.
    let mut fields: HashMap<String, RuntimeValue> = HashMap::new();
    for dec in decorators {
        match apply_decorator(dec, env, registry, fields, &EventConstraint::Choice) {
            Ok(new_fields) => fields = new_fields,
            Err(e) => return VmStep::Error(e),
        }
    }

    let has_default = options.iter().any(|o| o.is_default);

    // Build per-option ChoiceEvent entries (excluding default/wildcard options).
    let mut choice_options: Vec<ChoiceEvent> = Vec::new();
    for opt in options {
        if opt.is_default {
            continue; // Default options are not visible to the player.
        }
        let mut opt_fields: HashMap<String, RuntimeValue> = HashMap::new();
        for dec in &opt.decorators {
            match apply_decorator(dec, env, registry, opt_fields, &EventConstraint::Choice) {
                Ok(new_fields) => opt_fields = new_fields,
                Err(e) => return VmStep::Error(e),
            }
        }

        // Start with @fluent-tagged bindings, then overwrite with current env
        // values for any {varname} placeholders found in the raw option label.
        // This ensures mutable variables (e.g. price after haggling) are
        // reflected correctly in the Fluent context, matching the same
        // semantics applied to Dialogue events in `collect_fluent_vars`.
        let mut option_fluent_vars = env.collect_fluent_bindings();
        for (path, format) in extract_label_interp_vars(&opt.label) {
            // Full dotted path → Fluent key: dots become hyphens (e.g. "inv.gold" → "inv-gold").
            let key = path.replace('.', "-");
            if let Ok(raw_val) = eval::resolve_interp_path(&path, env) {
                let value = if let Some(ref fmt) = format {
                    let formatted = eval::format_runtime_value(&raw_val, Some(fmt));
                    RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain(&formatted))
                } else {
                    raw_val
                };
                option_fluent_vars.insert(key, value);
            }
        }

        let localized_label = localizer.and_then(|loc| {
            opt.loc_id
                .as_deref()
                .and_then(|id| loc.localize(id, &option_fluent_vars))
        });
        choice_options.push(ChoiceEvent {
            label: opt.label.clone(),
            fields: opt_fields,
            loc_id: opt.loc_id.clone(),
            fluent_vars: option_fluent_vars,
            localized_label,
        });
    }

    VmStep::Event(Event::Choice {
        options: choice_options,
        fields,
        loc_id: loc_id.clone(),
        fluent_vars: env.collect_fluent_bindings(),
        has_default,
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
        let opt1 = Ast::menu_option("yes".to_string(), Ast::block(vec![]), false);
        let opt2 = Ast::menu_option("no".to_string(), Ast::block(vec![]), false);
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
        // Declare x before the if so it survives block-scope pop.
        let pre_decl = decl("x", int(0));
        let then_block = Ast::block(vec![Ast::assign_op(ident("x"), int(1))]);
        let else_block = Ast::block(vec![Ast::assign_op(ident("x"), int(2))]);
        let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

        let speakers = Ast::expr_list(vec![str_lit("Bob")]);
        let lines = Ast::expr_list(vec![ident("x")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![pre_decl, if_ast, dialogue]);

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
        // Declare result before the if so it survives block-scope pop.
        let pre_decl = decl("result", int(0));
        let then_block = Ast::block(vec![Ast::assign_op(ident("result"), int(1))]);
        let else_block = Ast::block(vec![Ast::assign_op(ident("result"), int(2))]);
        let if_ast = Ast::if_stmt(condition, then_block, Some(else_block));

        let speakers = Ast::expr_list(vec![str_lit("Narrator")]);
        let lines = Ast::expr_list(vec![ident("result")]);
        let dialogue = Ast::dialogue(speakers, lines);
        let ast = Ast::block(vec![pre_decl, if_ast, dialogue]);

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
            false,
        );
        let opt_b = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![decl("picked", int(2))]),
            false,
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
        let opt = Ast::menu_option("Only".to_string(), Ast::block(vec![]), false);
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
        let _ = env.pop_scope();
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

    // ── extract_label_interp_vars ────────────────────────────────────────────

    #[test]
    fn extract_label_interp_vars_empty_string() {
        let vars = extract_label_interp_vars("");
        assert!(vars.is_empty());
    }

    #[test]
    fn extract_label_interp_vars_no_placeholders() {
        let vars = extract_label_interp_vars("Buy the potion");
        assert!(vars.is_empty());
    }

    #[test]
    fn extract_label_interp_vars_single_placeholder() {
        let vars = extract_label_interp_vars("Buy it for {price} gold");
        assert_eq!(vars, vec![("price".to_string(), None)]);
    }

    #[test]
    fn extract_label_interp_vars_multiple_placeholders() {
        let vars = extract_label_interp_vars("Pay {price} from your {gold} gold");
        assert!(vars.contains(&("price".to_string(), None)));
        assert!(vars.contains(&("gold".to_string(), None)));
    }

    #[test]
    fn extract_label_interp_vars_dotted_path() {
        let vars = extract_label_interp_vars("Hello {player.name}!");
        assert_eq!(vars, vec![("player.name".to_string(), None)]);
    }

    #[test]
    fn extract_label_interp_vars_ignores_empty_braces() {
        let vars = extract_label_interp_vars("broken {} placeholder");
        assert!(vars.is_empty(), "empty braces must not produce a var");
    }

    #[test]
    fn extract_label_interp_vars_with_float_format() {
        let vars = extract_label_interp_vars("Cost: {price:.2} gold");
        assert_eq!(vars, vec![("price".to_string(), Some(".2".to_string()))]);
    }

    #[test]
    fn extract_label_interp_vars_with_zero_pad_format() {
        let vars = extract_label_interp_vars("Turn {turns:03} of 100");
        assert_eq!(vars, vec![("turns".to_string(), Some("03".to_string()))]);
    }

    #[test]
    fn extract_label_interp_vars_mixed_format_and_plain() {
        let vars = extract_label_interp_vars("{count:02} items for {price} gold");
        assert!(vars.contains(&("count".to_string(), Some("02".to_string()))));
        assert!(vars.contains(&("price".to_string(), None)));
    }

    // ── stale @fluent binding fixes ─────────────────────────────────────────

    /// Verify that after `@fluent global gold = 50` followed by `gold = 20`,
    /// the Fluent context passed to the localizer contains the CURRENT value
    /// (20), not the stale initial value (50) cached from the declaration.
    #[test]
    fn fluent_var_uses_current_value_after_global_mutation() {
        use crate::compiler::Compiler;
        use crate::compiler::loader::parse_source;
        use crate::vm::Vm;
        use crate::vm::registry::DecoratorRegistry;
        use std::sync::Arc;

        let src = r#"
const n = :{ name: "N", name_color: "white" }
@fluent
global gold = 50
@entry
label start {
    gold = 20
    n: "You have {gold} gold."
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile_named(&ast, "test").expect("compile");
        let vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");

        // Mock localizer: captures the fluent vars map passed for the known
        // message ID and returns them serialised as "key=value" pairs so the
        // test can assert on the exact value without needing fluent_bundle.
        struct CapturingLocalizer;
        impl crate::Localizer for CapturingLocalizer {
            fn localize(
                &self,
                id: &str,
                vars: &std::collections::HashMap<String, RuntimeValue>,
            ) -> Option<String> {
                if id == "test-start-line_1" {
                    let gold = vars.get("gold")?;
                    Some(format!("gold={gold:?}"))
                } else {
                    None
                }
            }
        }

        let mut vm = vm.with_localizer(Arc::new(CapturingLocalizer));

        // Advance until the Dialogue event.
        let mut localized: Option<String> = None;
        for _ in 0..20 {
            match vm.next(None) {
                VmStep::Event(crate::ir::Event::Dialogue { localized_text, .. }) => {
                    localized = localized_text;
                    break;
                }
                VmStep::Ended => break,
                VmStep::Error(e) => panic!("VM error: {:?}", e),
                _ => {}
            }
        }

        // The localizer must receive gold=20 (current), not gold=50 (stale
        // @fluent initial binding).
        assert_eq!(
            localized.as_deref(),
            Some("gold=Int(20)"),
            "FTL context must carry current gold=20, not stale gold=50"
        );
    }

    /// Verify that when a string interpolation carries a format specifier
    /// (e.g. `{price:.2}`), `collect_fluent_vars` passes the **pre-formatted**
    /// string (`"30.00"`) to the Fluent localizer rather than the raw
    /// `Float(30.0)`.
    #[test]
    fn fluent_var_pre_formatted_with_specifier() {
        use crate::compiler::Compiler;
        use crate::compiler::loader::parse_source;
        use crate::vm::Vm;
        use crate::vm::registry::DecoratorRegistry;
        use std::sync::Arc;

        let src = r#"
const n = :{ name: "N", name_color: "white" }
@fluent
global price = 30.0
@entry
label start {
    n: "Total: {price:.2} gold."
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile_named(&ast, "test").expect("compile");
        let vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");

        struct CapturingLocalizer;
        impl crate::Localizer for CapturingLocalizer {
            fn localize(
                &self,
                _id: &str,
                vars: &std::collections::HashMap<String, RuntimeValue>,
            ) -> Option<String> {
                // Return a tagged string so the test can distinguish a
                // pre-formatted Str from a raw Float.
                match vars.get("price")? {
                    RuntimeValue::Str(ps) => Some(format!("str:{}", ps)),
                    other => Some(format!("other:{other:?}")),
                }
            }
        }

        let mut vm = vm.with_localizer(Arc::new(CapturingLocalizer));

        let mut localized: Option<String> = None;
        for _ in 0..20 {
            match vm.next(None) {
                VmStep::Event(crate::ir::Event::Dialogue { localized_text, .. }) => {
                    localized = localized_text;
                    break;
                }
                VmStep::Ended => break,
                VmStep::Error(e) => panic!("VM error: {:?}", e),
                _ => {}
            }
        }

        // The Fluent context must contain the pre-formatted "30.00" string, not
        // the raw Float(30.0) that would be formatted independently by Fluent.
        assert_eq!(
            localized.as_deref(),
            Some("str:30.00"),
            "FTL context must carry pre-formatted price=\"30.00\", not raw Float(30.0)"
        );
    }

    /// Verify that a dotted-path interpolation (`{inv.gold}`) produces a Fluent
    /// key using `-` as the separator (`inv-gold`) and resolves the value via
    /// the module-namespace lookup (`inv::gold` in the environment).
    #[test]
    fn fluent_vars_dotted_path_uses_hyphen_key() {
        use crate::compiler::Compiler;
        use crate::compiler::loader::parse_source;
        use crate::vm::Vm;
        use crate::vm::registry::DecoratorRegistry;
        use std::sync::Arc;

        let src = r#"
const n = :{ name: "N", name_color: "white" }
@entry
label start {
    n: "You have {inv.gold} coins."
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile_named(&ast, "test").expect("compile");
        let mut vm = Vm::new(graph, DecoratorRegistry::default()).expect("vm");
        // Inject the namespaced variable so that resolve_interp_path("inv.gold")
        // finds "inv::gold" in the externs slot.
        vm.provide_extern("inv::gold", RuntimeValue::Int(77));

        struct CapturingLocalizer;
        impl crate::Localizer for CapturingLocalizer {
            fn localize(
                &self,
                id: &str,
                vars: &std::collections::HashMap<String, RuntimeValue>,
            ) -> Option<String> {
                if id == "test-start-line_1" {
                    // Key must be "inv-gold" (hyphen), not "inv_gold" (underscore)
                    // and not "gold" (last segment only).
                    let val = vars.get("inv-gold")?;
                    Some(format!("inv-gold={val:?}"))
                } else {
                    None
                }
            }
        }

        let mut vm = vm.with_localizer(Arc::new(CapturingLocalizer));

        let mut localized: Option<String> = None;
        for _ in 0..20 {
            match vm.next(None) {
                VmStep::Event(crate::ir::Event::Dialogue { localized_text, .. }) => {
                    localized = localized_text;
                    break;
                }
                VmStep::Ended => break,
                VmStep::Error(e) => panic!("VM error: {:?}", e),
                _ => {}
            }
        }

        assert_eq!(
            localized.as_deref(),
            Some("inv-gold=Int(77)"),
            "Fluent vars must use hyphen separator and resolve namespaced env key"
        );
    }

    /// Unconditional jump loops between two labeled blocks must NOT accumulate
    /// [`CallFrame`]s on the call stack.
    ///
    /// **Known bug**: [`IrNodeKind::EnterScope`] always pushes a [`CallFrame`]
    /// regardless of whether the label was entered via [`IrNodeKind::LetCall`]
    /// (subroutine) or a plain [`IrNodeKind::Jump`] (tail transfer).  Because
    /// an unconditional `jump` never reaches [`IrNodeKind::ExitScope`] of the
    /// *previous* label, the pushed frame is never popped.
    ///
    /// After 20 dialogue events from the A → B → A loop the call stack has ≈ 20
    /// leaked frames; this test asserts 0 and therefore **FAILS** against the
    /// current implementation.
    #[test]
    fn test_unconditional_jump_loop_leaks_call_frames() {
        use crate::compiler::loader::parse_source;

        // A simple two-label ping-pong loop.  Each label emits one Dialogue event
        // before jumping to the other label so that vm.next() returns control to
        // the test after each observable step.
        let src = r#"
@entry
label alpha {
    Narrator: "in alpha"
    jump beta
}

label beta {
    Narrator: "in beta"
    jump alpha
}
"#;

        let ast = parse_source(src).expect("parse must succeed");
        let graph = Compiler::compile(&ast).expect("compile must succeed");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm construction must succeed");

        // Drive 20 observable events (10 full alpha→beta cycles).
        for i in 0..20 {
            match vm.next(None) {
                VmStep::Error(e) => panic!("unexpected VM error at step {i}: {e}"),
                VmStep::Ended => break,
                VmStep::Event(_) => {}
            }
        }

        // A correct implementation uses tail-call semantics for unconditional
        // jumps: no CallFrame should be pushed when entering a label via `jump`.
        // The current implementation leaks one frame per EnterScope hit.
        assert_eq!(
            vm.state.call_stack.len(),
            0,
            "BUG: jump loop leaked {} CallFrame(s) after 20 steps. \
             Plain `jump` must not accumulate frames on the call stack. \
             Fix: only push a CallFrame for LetCall (subroutine), not for Jump.",
            vm.state.call_stack.len()
        );
        assert!(
            vm.state.env.depth() <= 2,
            "BUG: jump loop grew scope stack to depth {} after 20 steps. \
             Plain `jump` must not accumulate scopes (base 1 + current label = 2 max).",
            vm.state.env.depth()
        );
    }

    // ── Dice-match helpers ────────────────────────────────────────────────────

    /// A deterministic dice roller that returns a fixed sequence of values,
    /// truncated to the requested `count`.  Used to make dice-evaluation
    /// tests reproducible.
    struct StubRoller(Vec<i64>);

    impl DiceRoller for StubRoller {
        fn roll_individual(&self, count: u32, _sides: u32) -> Vec<i64> {
            self.0[..count as usize].to_vec()
        }
    }

    /// Build a VM with a deterministic [`StubRoller`] injected into the
    /// environment before the first step.
    fn build_vm_with_roller(ast: Ast, roller: impl DiceRoller + 'static) -> Vm {
        let graph = Compiler::compile(&ast).expect("compile failed");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm init failed");
        vm.state.env.set_dice_roller(Box::new(roller));
        vm
    }

    /// Construct a minimal two-part `Dialogue` AST node for use in match arm bodies.
    fn dialogue_node(speaker: &str, line: &str) -> Ast {
        Ast::dialogue(
            Ast::expr_list(vec![str_lit(speaker)]),
            Ast::expr_list(vec![str_lit(line)]),
        )
    }

    /// Advance the VM one step and assert it emits a `Dialogue` event whose
    /// first line matches `expected`.
    fn expect_dialogue_line(vm: &mut Vm, expected: &str) {
        match vm.next(None) {
            VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(ps.to_string(), expected, "dialogue line text mismatch")
                }
                other => panic!("expected Str line, got {:?}", other),
            },
            other => panic!("expected Dialogue event, got {:?}", other),
        }
    }

    // ── VM dice-match integration tests ──────────────────────────────────────

    /// A `Value` arm is compared against the **sum** of the roll.
    ///
    /// `1d6` rolls `[5]` → sum=5 → arm `5` matches → `"hit 5"` dialogue fires.
    #[test]
    fn test_match_value_coerces_roll_to_sum() {
        let scrutinee = Ast::value(RuntimeValue::Dice(1, 6));
        let hit_arm = MatchArm::new(
            MatchPattern::Value(Ast::value(RuntimeValue::Int(5))),
            Ast::block(vec![dialogue_node("narrator", "hit 5")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "no match")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![hit_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![5]));

        expect_dialogue_line(&mut vm, "hit 5");
    }

    /// An exclusive `Range` arm matches when the sum falls strictly inside the bounds.
    ///
    /// `1d20` rolls `[15]`, arm `2..18` → 2 ≤ 15 < 18 → matches → `"mid"` fires.
    #[test]
    fn test_match_range_exclusive_matches() {
        let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
        let range_arm = MatchArm::new(
            MatchPattern::Range {
                start: Ast::value(RuntimeValue::Int(2)),
                end: Ast::value(RuntimeValue::Int(18)),
                inclusive: false,
                binding: None,
            },
            Ast::block(vec![dialogue_node("narrator", "mid")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "out")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![range_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![15]));

        expect_dialogue_line(&mut vm, "mid");
    }

    /// The exclusive upper bound is itself excluded from the range.
    ///
    /// `1d20` rolls `[18]`, arm `2..18` → 18 is NOT < 18 → no match →
    /// wildcard fires → `"out of range"`.
    #[test]
    fn test_match_range_exclusive_boundary_excluded() {
        let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
        let range_arm = MatchArm::new(
            MatchPattern::Range {
                start: Ast::value(RuntimeValue::Int(2)),
                end: Ast::value(RuntimeValue::Int(18)),
                inclusive: false,
                binding: None,
            },
            Ast::block(vec![dialogue_node("narrator", "in range")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "out of range")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![range_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![18]));

        expect_dialogue_line(&mut vm, "out of range");
    }

    /// The inclusive upper bound is itself included in the range.
    ///
    /// `1d20` rolls `[18]`, arm `2..=18` → 18 ≤ 18 → matches → `"in range"` fires.
    #[test]
    fn test_match_range_inclusive_boundary_included() {
        let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
        let range_arm = MatchArm::new(
            MatchPattern::Range {
                start: Ast::value(RuntimeValue::Int(2)),
                end: Ast::value(RuntimeValue::Int(18)),
                inclusive: true,
                binding: None,
            },
            Ast::block(vec![dialogue_node("narrator", "in range")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "out of range")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![range_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![18]));

        expect_dialogue_line(&mut vm, "in range");
    }

    /// An `as name` binding injects the matched scalar into the arm body scope.
    ///
    /// `1d20` rolls `[7]`, arm `1..=20 as roll_val` → `roll_val` is bound to
    /// `Int(7)` inside the arm body.  The dialogue emits `roll_val` as a line,
    /// which must evaluate to `RuntimeValue::Int(7)`.
    #[test]
    fn test_match_range_binding_injects_variable() {
        let scrutinee = Ast::value(RuntimeValue::Dice(1, 20));
        let arm = MatchArm::new(
            MatchPattern::Range {
                start: Ast::value(RuntimeValue::Int(1)),
                end: Ast::value(RuntimeValue::Int(20)),
                inclusive: true,
                binding: Some("roll_val".to_string()),
            },
            // Emit the bound variable as the dialogue line to verify its value.
            Ast::block(vec![Ast::dialogue(
                Ast::expr_list(vec![str_lit("narrator")]),
                Ast::expr_list(vec![ident("roll_val")]),
            )]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![7]));

        match vm.next(None) {
            VmStep::Event(Event::Dialogue { lines, .. }) => {
                assert_eq!(
                    lines[0],
                    RuntimeValue::Int(7),
                    "binding `roll_val` must resolve to Int(7) inside the arm body"
                );
            }
            other => panic!("expected Dialogue event, got {:?}", other),
        }
    }

    /// An `Array` pattern matches element-by-element against individual die results.
    ///
    /// `1d6` rolls `[6]`, arm `[6]` → exact element match → `"max"` fires.
    #[test]
    fn test_match_array_single_die_matches() {
        let scrutinee = Ast::value(RuntimeValue::Dice(1, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(6)))]),
            Ast::block(vec![dialogue_node("narrator", "max")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "other")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![6]));

        expect_dialogue_line(&mut vm, "max");
    }

    /// An `Array` pattern fails when the die value differs from the pattern element.
    ///
    /// `1d6` rolls `[5]`, arm `[6]` → 5 ≠ 6 → no match → wildcard → `"not max"`.
    #[test]
    fn test_match_array_single_die_no_match() {
        let scrutinee = Ast::value(RuntimeValue::Dice(1, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(6)))]),
            Ast::block(vec![dialogue_node("narrator", "max")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "not max")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![5]));

        expect_dialogue_line(&mut vm, "not max");
    }

    /// A multi-element `Array` pattern matches each die result in order.
    ///
    /// `2d6` rolls `[1, 6]`, arm `[1, 6]` → both elements match → `"snake+six"` fires.
    #[test]
    fn test_match_array_multi_die_matches() {
        let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![
                Some(Ast::value(RuntimeValue::Int(1))),
                Some(Ast::value(RuntimeValue::Int(6))),
            ]),
            Ast::block(vec![dialogue_node("narrator", "snake+six")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "other")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![1, 6]));

        expect_dialogue_line(&mut vm, "snake+six");
    }

    /// `Array` pattern comparison is strictly order-sensitive.
    ///
    /// `2d6` rolls `[6, 1]`, arm `[1, 6]` → die[0]=6 ≠ pat[0]=1 → no match →
    /// wildcard fires → `"wrong order"`.
    #[test]
    fn test_match_array_multi_die_wrong_order() {
        let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![
                Some(Ast::value(RuntimeValue::Int(1))),
                Some(Ast::value(RuntimeValue::Int(6))),
            ]),
            Ast::block(vec![dialogue_node("narrator", "one then six")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "wrong order")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![6, 1]));

        expect_dialogue_line(&mut vm, "wrong order");
    }

    /// A wildcard element in an `Array` pattern matches any die value at that position.
    ///
    /// `2d6` rolls `[1, 6]`, arm `[1, _]` → first matches, second is wildcard → fires.
    #[test]
    fn test_match_array_wildcard_element_matches() {
        let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(1))), None]),
            Ast::block(vec![dialogue_node("narrator", "one then any")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "other")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![1, 6]));

        expect_dialogue_line(&mut vm, "one then any");
    }

    /// Wildcard element still matches when the concrete die value differs.
    ///
    /// `2d6` rolls `[1, 3]`, arm `[1, _]` → first matches, second is wildcard → fires.
    #[test]
    fn test_match_array_wildcard_element_matches_any_value() {
        let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(1))), None]),
            Ast::block(vec![dialogue_node("narrator", "one then any")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "other")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![1, 3]));

        expect_dialogue_line(&mut vm, "one then any");
    }

    /// A concrete element still rejects a mismatch even when wildcards are present.
    ///
    /// `2d6` rolls `[2, 6]`, arm `[1, _]` → die[0]=2 ≠ pat[0]=1 → no match → wildcard.
    #[test]
    fn test_match_array_wildcard_element_rejects_mismatch() {
        let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![Some(Ast::value(RuntimeValue::Int(1))), None]),
            Ast::block(vec![dialogue_node("narrator", "one then any")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "not one")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![2, 6]));

        expect_dialogue_line(&mut vm, "not one");
    }

    /// An all-wildcard `Array` pattern `[_, _]` matches any 2-die roll.
    ///
    /// `2d6` rolls `[4, 2]`, arm `[_, _]` → both wildcards → fires.
    #[test]
    fn test_match_array_all_wildcards_matches() {
        let scrutinee = Ast::value(RuntimeValue::Dice(2, 6));
        let array_arm = MatchArm::new(
            MatchPattern::Array(vec![None, None]),
            Ast::block(vec![dialogue_node("narrator", "any pair")]),
        );
        let wild_arm = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![dialogue_node("narrator", "fallback")]),
        );
        let match_stmt = Ast::match_stmt(scrutinee, vec![array_arm, wild_arm]);
        let ast = Ast::block(vec![match_stmt]);
        let mut vm = build_vm_with_roller(ast, StubRoller(vec![4, 2]));

        expect_dialogue_line(&mut vm, "any pair");
    }

    // ── Wildcard / default menu option tests ─────────────────────────────────

    /// A menu with 2 real options and 1 default emits an `Event::Choice` with
    /// only 2 visible options, and `has_default == true`.
    #[test]
    fn test_menu_default_option_not_in_event() {
        let opt_a = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![dialogue_node("narrator", "picked A")]),
            false,
        );
        let opt_b = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![dialogue_node("narrator", "picked B")]),
            false,
        );
        let opt_default =
            Ast::menu_default_option(Ast::block(vec![dialogue_node("narrator", "default fired")]));
        let ast = Ast::menu(vec![opt_a, opt_b, opt_default]);
        let mut vm = build_vm(ast);

        match vm.next(None) {
            VmStep::Event(Event::Choice {
                options,
                has_default,
                ..
            }) => {
                assert_eq!(options.len(), 2, "default option must be excluded");
                assert!(has_default, "has_default must be true");
                assert_eq!(options[0].label, "Option A");
                assert_eq!(options[1].label, "Option B");
            }
            other => panic!("expected Choice event, got {:?}", other),
        }
    }

    /// When a menu has a default option and is already pending, calling
    /// `vm.next(None)` follows the default branch instead of re-emitting.
    #[test]
    fn test_menu_default_triggered_by_none() {
        let opt_a = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![dialogue_node("narrator", "picked A")]),
            false,
        );
        let opt_default =
            Ast::menu_default_option(Ast::block(vec![dialogue_node("narrator", "default fired")]));
        let ast = Ast::menu(vec![opt_a, opt_default]);
        let mut vm = build_vm(ast);

        // First next(None) → emits Choice and sets pending.
        match vm.next(None) {
            VmStep::Event(Event::Choice { has_default, .. }) => {
                assert!(has_default);
            }
            other => panic!("expected Choice event, got {:?}", other),
        }

        // Second next(None) → follows default option, emits its dialogue.
        match vm.next(None) {
            VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
                RuntimeValue::Str(ps) => {
                    assert_eq!(
                        ps.to_string(),
                        "default fired",
                        "expected default branch dialogue"
                    );
                }
                other => panic!("expected Str line, got {:?}", other),
            },
            other => panic!("expected Dialogue from default branch, got {:?}", other),
        }
    }

    /// A menu WITHOUT a default option re-emits the Choice event when
    /// `vm.next(None)` is called while already pending (existing behavior).
    #[test]
    fn test_menu_without_default_reemits_on_none() {
        let opt_a = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![dialogue_node("narrator", "picked A")]),
            false,
        );
        let opt_b = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![dialogue_node("narrator", "picked B")]),
            false,
        );
        let ast = Ast::menu(vec![opt_a, opt_b]);
        let mut vm = build_vm(ast);

        // First next(None) → emits Choice.
        match vm.next(None) {
            VmStep::Event(Event::Choice { has_default, .. }) => {
                assert!(!has_default, "no default option present");
            }
            other => panic!("expected Choice event, got {:?}", other),
        }

        // Second next(None) → re-emits Choice (no default to follow).
        match vm.next(None) {
            VmStep::Event(Event::Choice { has_default, .. }) => {
                assert!(!has_default, "still no default");
            }
            other => panic!("expected re-emitted Choice event, got {:?}", other),
        }
    }

    /// Host-provided choice indices map correctly when a default option sits
    /// between real options: menu ["A", _ , "B"] → host sees indices 0 ("A")
    /// and 1 ("B"); the default is skipped in the index mapping.
    #[test]
    fn test_menu_default_choice_index_mapping() {
        let opt_a = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![dialogue_node("narrator", "picked A")]),
            false,
        );
        let opt_default =
            Ast::menu_default_option(Ast::block(vec![dialogue_node("narrator", "default fired")]));
        let opt_b = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![dialogue_node("narrator", "picked B")]),
            false,
        );
        // Order: A (idx 0), default (idx 1), B (idx 2)
        let ast = Ast::menu(vec![opt_a, opt_default, opt_b]);

        // ── Test host index 0 → "A" ─────────────────────────────────────
        {
            let mut vm = build_vm(ast.clone());
            match vm.next(None) {
                VmStep::Event(Event::Choice { options, .. }) => {
                    assert_eq!(options.len(), 2);
                    assert_eq!(options[0].label, "Option A");
                    assert_eq!(options[1].label, "Option B");
                }
                other => panic!("expected Choice, got {:?}", other),
            }
            // Choose index 0 → should follow "Option A"
            match vm.next(Some(0)) {
                VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
                    RuntimeValue::Str(ps) => {
                        assert_eq!(ps.to_string(), "picked A", "index 0 should map to Option A");
                    }
                    other => panic!("expected Str line, got {:?}", other),
                },
                other => panic!("expected Dialogue from Option A, got {:?}", other),
            }
        }

        // ── Test host index 1 → "B" (not default!) ──────────────────────
        {
            let mut vm = build_vm(ast.clone());
            match vm.next(None) {
                VmStep::Event(Event::Choice { .. }) => {}
                other => panic!("expected Choice, got {:?}", other),
            }
            // Choose index 1 → should follow "Option B", NOT default
            match vm.next(Some(1)) {
                VmStep::Event(Event::Dialogue { lines, .. }) => match &lines[0] {
                    RuntimeValue::Str(ps) => {
                        assert_eq!(ps.to_string(), "picked B", "index 1 should map to Option B");
                    }
                    other => panic!("expected Str line, got {:?}", other),
                },
                other => panic!("expected Dialogue from Option B, got {:?}", other),
            }
        }
    }

    // ── Block-scope / shadowing tests ─────────────────────────────────────────

    /// `let` inside an `if` block creates a new binding that shadows the outer
    /// one without mutating it. After the block ends the outer value is restored.
    #[test]
    fn test_let_shadows_outer_variable() {
        use crate::compiler::loader::parse_source;

        let src = r#"
@entry
label start {
    let x = 10
    if true {
        let x = 42
        narrator: "inner x is {x}"
    }
    narrator: "outer x is {x}"
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm");

        expect_dialogue_line(&mut vm, "inner x is 42");
        expect_dialogue_line(&mut vm, "outer x is 10");
        assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
    }

    /// Bare assignment (`x = 42`) without `let` searches outward and mutates
    /// the binding found in the enclosing scope.
    #[test]
    fn test_bare_assignment_mutates_outer_variable() {
        use crate::compiler::loader::parse_source;

        let src = r#"
@entry
label start {
    let x = 10
    if true {
        x = 42
    }
    narrator: "x is {x}"
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm");

        expect_dialogue_line(&mut vm, "x is 42");
        assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
    }

    /// Variables declared inside an `if` block don't leak into the outer scope.
    /// The script continues normally after the block ends.
    #[test]
    fn test_block_scope_variables_dont_leak() {
        use crate::compiler::loader::parse_source;

        let src = r#"
@entry
label start {
    if true {
        let temp = 99
    }
    narrator: "done"
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm");

        expect_dialogue_line(&mut vm, "done");
        assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
    }

    /// `let` can shadow a `const` — the constant check is bypassed for
    /// `DeclKind::Variable`, so the local binding wins.
    #[test]
    fn test_let_can_shadow_constant() {
        use crate::compiler::loader::parse_source;

        let src = r#"
const MAX = 100
@entry
label start {
    let MAX = 999
    narrator: "MAX is {MAX}"
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm");

        expect_dialogue_line(&mut vm, "MAX is 999");
        assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
    }

    /// Variables declared inside a `match` arm body don't leak to the
    /// continuation after the match statement.
    #[test]
    fn test_match_arm_block_scoping() {
        use crate::compiler::loader::parse_source;

        let src = r#"
@entry
label start {
    let val = 1
    match val {
        1 {
            let result = "one"
            narrator: "matched {result}"
        }
        _ {
            let result = "other"
            narrator: "matched {result}"
        }
    }
    narrator: "done"
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm");

        expect_dialogue_line(&mut vm, "matched one");
        expect_dialogue_line(&mut vm, "done");
        assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
    }

    /// `jump` from inside nested `if` blocks correctly unwinds all block
    /// scopes (PushScope levels) in addition to the label scope (EnterScope).
    #[test]
    fn test_jump_cleans_up_block_scopes() {
        use crate::compiler::loader::parse_source;

        let src = r#"
@entry
label start {
    if true {
        let temp = 1
        if true {
            let temp2 = 2
            jump finish
        }
    }
    end!()
}

label finish {
    narrator: "arrived"
    end!()
}
"#;
        let ast = parse_source(src).expect("parse");
        let graph = Compiler::compile(&ast).expect("compile");
        let mut vm = Vm::new(graph, empty_registry()).expect("vm");

        expect_dialogue_line(&mut vm, "arrived");
        assert!(matches!(vm.next(None), VmStep::Ended), "script should end");
    }
}
