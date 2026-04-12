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

mod decorators;
mod exec;
mod fluent;

#[cfg(test)]
mod tests;

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
    ir::{Event, IrEdge, IrGraph, IrNodeKind, VmStep},
    parser::ast::{AstContent, DeclKind, EventConstraint, MatchPattern},
    runtime::value::RuntimeValue,
};

use self::eval::{eval_speakers_list, is_truthy, values_equal};

use decorators::{apply_decorator, check_decorator_known};
use exec::eval_subscript_assign;
use fluent::{build_choice_event, collect_fluent_vars};

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

/// Default [`DiceRoller`] implementation backed by [`fastrand`].
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
        (0..capped)
            .map(|_| fastrand::u32(1..=sides) as i64)
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
        let cursor = graph.entry;
        Self::build(graph, registry, cursor)
    }

    /// Creates a new VM starting at the specified `@entry` label.
    ///
    /// The `label` must be the name of an `@entry`-decorated label in the
    /// compiled graph. Use [`IrGraph::entry_labels`] to discover available
    /// entry points.
    ///
    /// # Errors
    ///
    /// - [`VmError::UnknownLabel`] if `label` is not a known `@entry` label.
    /// - [`VmError::UnknownDecorator`] if any script decorator is not registered.
    pub fn new_at(
        graph: IrGraph,
        registry: DecoratorRegistry,
        label: &str,
    ) -> Result<Self, VmError> {
        // Verify the label exists and is @entry-decorated.
        if !graph.entry_labels.contains(label) {
            return Err(VmError::UnknownLabel(format!(
                "label '{}' is not an @entry label (available: {:?})",
                label, graph.entry_labels
            )));
        }
        let cursor = graph.labels.get(label).copied();
        Self::build(graph, registry, cursor)
    }

    /// Shared constructor: validates decorators, builds the exit-scope map,
    /// and wires up the initial VM state starting at `cursor`.
    fn build(
        graph: IrGraph,
        registry: DecoratorRegistry,
        cursor: Option<NodeIndex>,
    ) -> Result<Self, VmError> {
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

    /// Attach a custom [`DiceRoller`] to the VM.
    ///
    /// Replaces the default [`DefaultDiceRoller`] used for `d6`, `2d8` etc.
    /// expressions. Call this before the first [`Vm::next`] invocation.
    pub fn with_dice_roller(mut self, roller: impl DiceRoller + 'static) -> Self {
        self.state.env.set_dice_roller(Box::new(roller));
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
                    let speakers_vec = match eval_speakers_list(speakers, &state.env) {
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
