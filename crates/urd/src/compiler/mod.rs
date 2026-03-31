//! # Compiler Module
//!
//! This module transforms a parsed [`Ast`] into an [`IrGraph`] ready for VM
//! execution.  Compilation happens in two passes:
//!
//! 1. **Label scan** — walk the entire AST, collect every [`AstContent::LabeledBlock`]
//!    label name, and pre-allocate a [`IrNodeKind::Nop`] placeholder for each so that
//!    forward [`AstContent::Jump`] references can be resolved.
//!
//! 2. **Emit pass** — walk the AST again recursively, emitting [`IrNode`]s into the
//!    arena.  The core function [`CompilerState::compile_node`] returns the *entry*
//!    [`NodeId`] of the sub-graph it just emitted; callers thread a `next` continuation
//!    through every call so that every chain is fully linked on the first pass.

pub mod loader;

use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    ir::{IrChoiceOption, IrGraph, IrNodeKind, NODE_END, NodeId, SwitchArm},
    parser::ast::{Ast, AstContent, DeclKind, MatchPattern, Operator},
};

// ─── Public error type ────────────────────────────────────────────────────────

/// Errors that can occur during compilation of an [`Ast`] into an [`IrGraph`].
#[derive(Debug, Error)]
pub enum CompilerError {
    /// A [`AstContent::Jump`] targeted a label that was never defined.
    #[error("jump to unknown label `{0}`")]
    UnknownLabel(String),

    /// An AST node appeared at statement level where it is not permitted.
    #[error("invalid statement: {0}")]
    InvalidStatement(String),

    /// Failed to load a module during import resolution.
    #[error("module load error for '{path}': {message}")]
    ModuleLoadError {
        /// The import path that failed to load.
        path: String,
        /// Human-readable description of the failure.
        message: String,
    },

    /// A circular import was detected.
    #[error("circular import detected for '{0}'")]
    CircularImport(String),
}

// ─── Public API ───────────────────────────────────────────────────────────────

/// Stateless entry-point for Urd's AST-to-IR compiler.
///
/// Call [`Compiler::compile`] with a reference to the root [`Ast`] produced by
/// the parser to obtain an [`IrGraph`].
pub struct Compiler;

impl Compiler {
    /// Compile `ast` (the root of a parsed Urd script) into an [`IrGraph`].
    ///
    /// # Errors
    /// Returns [`CompilerError::UnknownLabel`] if a `jump` statement targets a
    /// label that does not exist anywhere in the script.
    ///
    /// Returns [`CompilerError::InvalidStatement`] if an expression-only AST
    /// node appears at a position where a statement is expected.
    pub fn compile(ast: &Ast) -> Result<IrGraph, CompilerError> {
        let mut state = CompilerState::new();

        // Pass 1 — collect all label names and pre-allocate Nop placeholders.
        state.scan_labels(ast);

        // Pass 2 — emit IR nodes.
        let entry = state.compile_node(ast, NODE_END)?;
        state.graph.entry = entry;

        Ok(state.graph)
    }

    /// Compile `ast` with access to a [`FileLoader`] for resolving `import` statements.
    ///
    /// This is the multi-file variant of [`Compiler::compile`]. When an
    /// `import "path" as alias` statement is encountered, the loader is called
    /// to fetch the source, which is then parsed and compiled recursively
    /// before being merged into the main graph.
    ///
    /// Circular imports are detected via a `visited` set of paths.
    pub fn compile_with_loader(
        ast: &Ast,
        loader: &dyn crate::vm::loader::FileLoader,
    ) -> Result<IrGraph, CompilerError> {
        // `in_progress` tracks modules currently on the recursion stack so we
        // can detect true import cycles (A→B→A).  `completed` tracks modules
        // that have already been fully compiled so we can skip them on
        // subsequent imports of the same path (diamond dependencies).
        let mut in_progress = HashSet::new();
        let mut completed = HashSet::new();
        loader::compile_recursive(ast, loader, &mut in_progress, &mut completed)
    }
}

// ─── Internal compiler state ──────────────────────────────────────────────────

/// Internal mutable state threaded through both compilation passes.
struct CompilerState {
    graph: IrGraph,
    /// Maps label names → the NodeId of their pre-allocated Nop placeholder.
    label_placeholders: HashMap<String, NodeId>,
    /// Entry NodeIds of imported modules (offset-adjusted after merge), in
    /// import order. Used by `compile_recursive` to chain module prologues
    /// (DefineEnum, global assignments, etc.) before the main entry so that
    /// cross-module enum variants and globals are available at runtime before
    /// the importing module's own code begins executing.
    import_prologues: Vec<crate::ir::NodeId>,
}

impl CompilerState {
    fn new() -> Self {
        CompilerState {
            graph: IrGraph::new(),
            label_placeholders: HashMap::new(),
            import_prologues: Vec::new(),
        }
    }

    // ── Pass 1: label scan ────────────────────────────────────────────────────

    /// Recursively walk `ast` and pre-allocate a [`IrNodeKind::Nop`] for every
    /// [`AstContent::LabeledBlock`] encountered.
    fn scan_labels(&mut self, ast: &Ast) {
        match ast.content() {
            AstContent::LabeledBlock { label, block } => {
                // Pre-allocate the placeholder (we will patch it in pass 2).
                let placeholder = self.graph.push(IrNodeKind::Nop { next: NODE_END });
                self.label_placeholders.insert(label.clone(), placeholder);
                // Recurse into the block body.
                self.scan_labels(block);
            }
            AstContent::Block(stmts) => {
                for stmt in stmts {
                    self.scan_labels(stmt);
                }
            }
            AstContent::If {
                then_block,
                else_block,
                ..
            } => {
                self.scan_labels(then_block);
                if let Some(eb) = else_block {
                    self.scan_labels(eb);
                }
            }
            AstContent::Menu { options } => {
                for opt in options {
                    if let AstContent::MenuOption { content, .. } = opt.content() {
                        self.scan_labels(content);
                    }
                }
            }
            AstContent::Match { arms, .. } => {
                for arm in arms {
                    self.scan_labels(&arm.body);
                }
            }
            // DecoratorDef bodies are stored as raw Ast for lazy apply-time
            // evaluation — they are never compiled into IR nodes, so any labels
            // inside them are private to the body and must not be pre-allocated
            // in the outer label_placeholders map.
            // All other nodes either have no sub-statements or only expressions.
            _ => {}
        }
    }

    // ── Pass 2: emit IR nodes ─────────────────────────────────────────────────

    /// Compile `ast` into one or more arena nodes.
    ///
    /// Returns the **entry** [`NodeId`] of the sub-graph produced. The VM will
    /// continue to `next` after this sub-graph finishes (unless the sub-graph
    /// contains a [`IrNodeKind::Return`] or [`IrNodeKind::Jump`], which ignore
    /// `next`).
    fn compile_node(&mut self, ast: &Ast, next: NodeId) -> Result<NodeId, CompilerError> {
        match ast.content() {
            // ── Block ────────────────────────────────────────────────────────
            AstContent::Block(stmts) => {
                if stmts.is_empty() {
                    // Empty block — emit a Nop so there is always a valid NodeId.
                    let id = self.graph.push(IrNodeKind::Nop { next });
                    return Ok(id);
                }
                // Compile right-to-left, threading `next` through each statement.
                let mut continuation = next;
                for stmt in stmts.iter().rev() {
                    continuation = self.compile_node(stmt, continuation)?;
                }
                Ok(continuation)
            }

            // ── Declaration ──────────────────────────────────────────────────
            AstContent::Declaration {
                kind,
                decl_name,
                decl_defs,
                ..
            } => {
                // Extract the variable name from the decl_name node.
                let var = extract_name(decl_name)?;
                let id = self.graph.push(IrNodeKind::Assign {
                    var,
                    scope: kind.clone(),
                    expr: *decl_defs.clone(),
                    next,
                });
                Ok(id)
            }

            // ── Assignment BinOp (x = expr) ──────────────────────────────────
            AstContent::BinOp { op, left, right } if *op == Operator::Assign => {
                // Detect cross-module assignment: `alias.var = value` → store as "alias::var"
                // with Global scope so it persists in the module's namespace.
                let (var, scope) = match left.content() {
                    AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(path))
                        if path.len() == 2 =>
                    {
                        (crate::ir::namespace(&path[0], &path[1]), DeclKind::Global)
                    }
                    _ => (extract_name(left)?, DeclKind::Variable),
                };
                let id = self.graph.push(IrNodeKind::Assign {
                    var,
                    scope,
                    expr: *right.clone(),
                    next,
                });
                Ok(id)
            }

            // ── Call (side-effecting) ────────────────────────────────────────
            AstContent::Call { func_path, .. } => {
                // Detect built-in terminators by their canonical name.
                let call_name = match func_path.content() {
                    AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(p))
                        if p.len() == 1 =>
                    {
                        Some(p[0].as_str())
                    }
                    _ => None,
                };

                match call_name {
                    Some("end!") => {
                        let id = self.graph.push(IrNodeKind::End);
                        Ok(id)
                    }
                    Some("todo!") => {
                        let id = self.graph.push(IrNodeKind::Todo);
                        Ok(id)
                    }
                    _ => {
                        let id = self.graph.push(IrNodeKind::Eval {
                            expr: ast.clone(),
                            next,
                        });
                        Ok(id)
                    }
                }
            }

            // ── If / else ────────────────────────────────────────────────────
            AstContent::If {
                condition,
                then_block,
                else_block,
            } => {
                // Allocate a merge-point Nop that both branches converge to.
                let merge = self.graph.push(IrNodeKind::Nop { next });

                let then_node = self.compile_node(then_block, merge)?;
                let else_node = match else_block {
                    Some(eb) => self.compile_node(eb, merge)?,
                    None => merge,
                };

                let id = self.graph.push(IrNodeKind::Branch {
                    condition: *condition.clone(),
                    then_node,
                    else_node,
                });
                Ok(id)
            }

            // ── LabeledBlock ─────────────────────────────────────────────────
            AstContent::LabeledBlock { label, block } => {
                // Retrieve the pre-allocated placeholder NodeId from pass 1.
                let placeholder_id = self
                    .label_placeholders
                    .get(label)
                    .copied()
                    .ok_or_else(|| CompilerError::UnknownLabel(label.clone()))?;

                // Emit the ExitScope node that runs *after* the block body.
                let exit_id = self.graph.push(IrNodeKind::ExitScope {
                    label: label.clone(),
                    next,
                });

                // Compile the block body, continuing to ExitScope.
                let body_entry = self.compile_node(block, exit_id)?;

                // Patch the placeholder Nop → EnterScope pointing at body_entry.
                self.graph.node_mut(placeholder_id).kind = IrNodeKind::EnterScope {
                    label: label.clone(),
                    next: body_entry,
                };

                // Register in the graph's public label map.
                self.graph.labels.insert(label.clone(), placeholder_id);

                Ok(placeholder_id)
            }

            // ── Jump ─────────────────────────────────────────────────────────
            AstContent::Jump {
                label,
                expects_return,
            } => {
                // Check for cross-module dot-notation: "alias.label_name"
                let target = resolve_label(label, &self.label_placeholders, &self.graph.labels)?;

                if *expects_return {
                    // `jump label and return` — subroutine call without a binding.
                    // Use an empty var name as the "discard return value" sentinel.
                    let id = self.graph.push(IrNodeKind::LetCall {
                        var: String::new(),
                        target,
                        next,
                    });
                    Ok(id)
                } else {
                    let id = self.graph.push(IrNodeKind::Jump { target });
                    Ok(id)
                }
            }

            // ── LetCall ───────────────────────────────────────────────────────
            AstContent::LetCall { name, target } => {
                // Check for cross-module dot-notation: "alias.label_name"
                let target_id =
                    resolve_label(target, &self.label_placeholders, &self.graph.labels)?;

                let id = self.graph.push(IrNodeKind::LetCall {
                    var: name.clone(),
                    target: target_id,
                    next,
                });
                Ok(id)
            }

            // ── Return ───────────────────────────────────────────────────────
            AstContent::Return { value } => {
                let id = self.graph.push(IrNodeKind::Return {
                    value: value.as_deref().cloned(),
                });
                Ok(id)
            }

            // ── Dialogue ─────────────────────────────────────────────────────
            AstContent::Dialogue { speakers, content } => {
                let id = self.graph.push(IrNodeKind::Dialogue {
                    speakers: *speakers.clone(),
                    lines: *content.clone(),
                    decorators: ast.decorators().to_vec(),
                    next,
                });
                Ok(id)
            }

            // ── Menu ─────────────────────────────────────────────────────────
            AstContent::Menu { options } => {
                let mut ir_options = Vec::with_capacity(options.len());

                for opt_ast in options {
                    match opt_ast.content() {
                        AstContent::MenuOption { label, content } => {
                            // Each option body continues to NODE_END (or `next`
                            // if the designer wants the menu to fall through —
                            // we follow the spec: use `next` so options can
                            // rejoin after the menu).
                            let entry = self.compile_node(content, next)?;
                            ir_options.push(IrChoiceOption {
                                label: label.clone(),
                                entry,
                                decorators: opt_ast.decorators().to_vec(),
                            });
                        }
                        _ => {
                            return Err(CompilerError::InvalidStatement(
                                "expected MenuOption inside Menu".to_string(),
                            ));
                        }
                    }
                }

                let id = self.graph.push(IrNodeKind::Choice {
                    options: ir_options,
                    decorators: ast.decorators().to_vec(),
                });
                Ok(id)
            }

            // ── Match ────────────────────────────────────────────────────────
            AstContent::Match { scrutinee, arms } => {
                // Allocate a merge Nop that all non-wildcard arms converge to.
                let merge = self.graph.push(IrNodeKind::Nop { next });

                let mut switch_arms: Vec<SwitchArm> = Vec::with_capacity(arms.len());
                let mut default: Option<NodeId> = None;

                for arm in arms {
                    let target = self.compile_node(&arm.body, merge)?;
                    match &arm.pattern {
                        MatchPattern::Wildcard => {
                            default = Some(target);
                        }
                        MatchPattern::Value(_) => {
                            switch_arms.push(SwitchArm {
                                pattern: arm.pattern.clone(),
                                target,
                            });
                        }
                    }
                }

                let id = self.graph.push(IrNodeKind::Switch {
                    scrutinee: *scrutinee.clone(),
                    arms: switch_arms,
                    default,
                });
                Ok(id)
            }

            // ── EnumDecl ─────────────────────────────────────────────────────
            AstContent::EnumDecl { name, variants } => {
                let id = self.graph.push(IrNodeKind::DefineEnum {
                    name: name.clone(),
                    variants: variants.clone(),
                    next,
                });
                Ok(id)
            }

            // ── Pure expressions at statement level ──────────────────────────
            // These are wrapped in an Eval node so that any side effects
            // (e.g. a non-Call BinOp that triggers a runtime hook) are preserved.
            AstContent::BinOp { .. }
            | AstContent::UnaryOp { .. }
            | AstContent::ExprList(_)
            | AstContent::Value(_)
            | AstContent::List(_)
            | AstContent::Map(_) => {
                let id = self.graph.push(IrNodeKind::Eval {
                    expr: ast.clone(),
                    next,
                });
                Ok(id)
            }

            // ── MenuOption outside a Menu — shouldn't happen ─────────────────
            AstContent::MenuOption { label, .. } => Err(CompilerError::InvalidStatement(format!(
                "MenuOption `{}` appeared outside a Menu",
                label
            ))),

            // ── DecoratorDef ─────────────────────────────────────────────────
            AstContent::DecoratorDef {
                name,
                event_constraint,
                params,
                body,
            } => {
                // Strip type annotations — runtime only needs parameter names.
                let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
                let id = self.graph.push(IrNodeKind::DefineScriptDecorator {
                    name: name.clone(),
                    event_constraint: event_constraint.clone(),
                    params: param_names,
                    body: *body.clone(),
                    next,
                });
                Ok(id)
            }

            // ── Subscript (index read at statement level) ────────────────────
            AstContent::Subscript { .. } => {
                let id = self.graph.push(IrNodeKind::Eval {
                    expr: ast.clone(),
                    next,
                });
                Ok(id)
            }

            // ── SubscriptAssign (index write) ────────────────────────────────
            AstContent::SubscriptAssign { .. } => {
                let id = self.graph.push(IrNodeKind::Eval {
                    expr: ast.clone(),
                    next,
                });
                Ok(id)
            }

            // ── Import ───────────────────────────────────────────────────────
            // Import nodes are fully resolved during pass 0 (collect_imports).
            // At compile-node time they are no-ops that simply fall through.
            AstContent::Import { .. } => Ok(self.graph.push(IrNodeKind::Nop { next })),

            // ── StructDecl ───────────────────────────────────────────────────
            // Struct declarations are analysis-only — no runtime representation
            // is needed. They compile to a Nop so execution flows through them.
            AstContent::StructDecl { .. } => Ok(self.graph.push(IrNodeKind::Nop { next })),
        }
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

/// Extract a plain variable name string from a name-bearing AST node.
///
/// Accepts:
/// - `Value(IdentPath([name]))` — a simple single-segment identifier
/// - `Value(IdentPath([alias, var]))` — a 2-segment cross-module path; encoded
///   as `"alias::var"` for storage in the globals map.
///
/// # Errors
/// Returns [`CompilerError::InvalidStatement`] for any other node shape.
fn extract_name(ast: &Ast) -> Result<String, CompilerError> {
    match ast.content() {
        AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(path))
            if path.len() == 1 =>
        {
            Ok(path[0].clone())
        }
        // Cross-module assignment: `alias.var = value` → store as "alias::var"
        AstContent::Value(crate::runtime::value::RuntimeValue::IdentPath(path))
            if path.len() == 2 =>
        {
            Ok(crate::ir::namespace(&path[0], &path[1]))
        }
        other => Err(CompilerError::InvalidStatement(format!(
            "expected an identifier for variable name, got {:?}",
            other
        ))),
    }
}

/// Resolve a label string to a [`NodeId`], handling both local and cross-module
/// (dot-notation `alias.label_name`) references.
///
/// Local labels are looked up in `label_placeholders` (populated by the label scan pass).
/// Cross-module labels (`alias.name`) are looked up in the graph's `labels` map, which
/// is pre-populated by the import pass.
fn resolve_label(
    label: &str,
    label_placeholders: &HashMap<String, NodeId>,
    graph_labels: &HashMap<String, NodeId>,
) -> Result<NodeId, CompilerError> {
    if let Some(dot_pos) = label.find('.') {
        let alias = &label[..dot_pos];
        let label_name = &label[dot_pos + 1..];
        let namespaced = crate::ir::namespace(alias, label_name);
        graph_labels
            .get(&namespaced)
            .copied()
            .ok_or_else(|| CompilerError::UnknownLabel(label.to_owned()))
    } else {
        label_placeholders
            .get(label)
            .copied()
            .ok_or_else(|| CompilerError::UnknownLabel(label.to_owned()))
    }
}

// ─── Unit tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::{
        ir::{IrNodeKind, NODE_END},
        parser::ast::{Ast, AstContent, DeclKind, MatchArm, MatchPattern},
        runtime::value::RuntimeValue,
        vm::loader::MemLoader,
    };

    // ── decorator tests ───────────────────────────────────────────────────────

    #[test]
    fn test_decorator_def_compiles_to_define_script_decorator() {
        use crate::parser::ast::{DecoratorParam, EventConstraint};

        let body = Ast::block(vec![]);
        let decorator_ast = Ast::decorator_def(
            "shake".to_string(),
            EventConstraint::Any,
            vec![DecoratorParam {
                name: "amount".to_string(),
                type_annotation: None,
            }],
            body,
        );
        let script_ast = Ast::block(vec![decorator_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        let entry = &graph.nodes[graph.entry.0 as usize];
        assert!(
            matches!(
                &entry.kind,
                IrNodeKind::DefineScriptDecorator { name, params, .. }
                if name == "shake" && params == &["amount"]
            ),
            "expected DefineScriptDecorator at entry, got {:?}",
            entry.kind
        );
    }

    #[test]
    fn test_decorator_def_then_dialogue_chains() {
        use crate::parser::ast::{AstContent, EventConstraint};

        let body = Ast::block(vec![]);
        let dec_ast = Ast::decorator_def("voiced".to_string(), EventConstraint::Any, vec![], body);
        let dialogue_ast = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(Ast::value(RuntimeValue::IdentPath(vec![
                    "Alice".to_string(),
                ]))),
                content: Box::new(Ast::value(RuntimeValue::IdentPath(vec!["x".to_string()]))),
            },
            vec![],
        );
        let script_ast = Ast::block(vec![dec_ast, dialogue_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        let entry_node = &graph.nodes[graph.entry.0 as usize];
        let IrNodeKind::DefineScriptDecorator { next, .. } = &entry_node.kind else {
            panic!("expected DefineScriptDecorator, got {:?}", entry_node.kind)
        };
        assert!(
            matches!(
                &graph.nodes[next.0 as usize].kind,
                IrNodeKind::Dialogue { .. }
            ),
            "expected Dialogue after DefineScriptDecorator"
        );
    }

    #[test]
    fn test_decorator_def_no_params() {
        use crate::parser::ast::EventConstraint;

        let body = Ast::block(vec![]);
        let decorator_ast = Ast::decorator_def(
            "highlight".to_string(),
            EventConstraint::Dialogue,
            vec![],
            body,
        );
        let script_ast = Ast::block(vec![decorator_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        let entry = &graph.nodes[graph.entry.0 as usize];
        assert!(
            matches!(
                &entry.kind,
                IrNodeKind::DefineScriptDecorator {
                    name,
                    params,
                    event_constraint,
                    ..
                }
                if name == "highlight"
                    && params.is_empty()
                    && matches!(event_constraint, crate::parser::ast::EventConstraint::Dialogue)
            ),
            "expected DefineScriptDecorator(highlight) at entry, got {:?}",
            entry.kind
        );
    }

    #[test]
    fn test_decorator_def_body_labels_are_private() {
        use crate::parser::ast::EventConstraint;

        // Labels declared inside a decorator body are private — the body is
        // stored as raw Ast for lazy apply-time evaluation and is never compiled
        // into IR nodes, so its labels must NOT appear in graph.labels and a
        // jump targeting one from outside must fail with UnknownLabel.
        let inner_labeled = Ast::labeled_block("inner_scene".to_string(), Ast::block(vec![]));
        let body = Ast::block(vec![inner_labeled]);
        let decorator_ast =
            Ast::decorator_def("wrapper".to_string(), EventConstraint::Any, vec![], body);
        // Compiling the decorator alone must succeed and must NOT register the
        // inner label in the public graph.labels map.
        let script_ast = Ast::block(vec![decorator_ast]);
        let graph = Compiler::compile(&script_ast).expect("compile failed");
        assert!(
            !graph.labels.contains_key("inner_scene"),
            "inner_scene is private to the decorator body — must not appear in graph.labels"
        );

        // A jump to a label that only exists inside a decorator body must
        // produce a CompilerError::UnknownLabel.
        let inner_labeled2 = Ast::labeled_block("inner_scene".to_string(), Ast::block(vec![]));
        let body2 = Ast::block(vec![inner_labeled2]);
        let decorator_ast2 =
            Ast::decorator_def("wrapper2".to_string(), EventConstraint::Any, vec![], body2);
        let jump_ast = Ast::jump_stmt("inner_scene".to_string(), false);
        let script_with_jump = Ast::block(vec![decorator_ast2, jump_ast]);
        let result = Compiler::compile(&script_with_jump);
        assert!(
            matches!(result, Err(CompilerError::UnknownLabel(ref l)) if l == "inner_scene"),
            "expected UnknownLabel for jump into decorator body, got {:?}",
            result
        );
    }

    // ── helpers ──────────────────────────────────────────────────────────────

    fn ident(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_string()]))
    }

    fn int(n: i64) -> Ast {
        Ast::value(RuntimeValue::Int(n))
    }

    fn str_lit(s: &str) -> Ast {
        use crate::lexer::strings::ParsedString;
        Ast::value(RuntimeValue::Str(ParsedString::new_plain(s)))
    }

    fn decl(name: &str, val: Ast) -> Ast {
        Ast::decl(DeclKind::Variable, ident(name), val)
    }

    fn node_kind(graph: &IrGraph, id: NodeId) -> &IrNodeKind {
        &graph.nodes[id.0 as usize].kind
    }

    // ── tests ─────────────────────────────────────────────────────────────────

    /// A Block with two declarations compiles to a right-linked Assign chain.
    #[test]
    fn test_block_declarations_chain() {
        let ast = Ast::block(vec![decl("x", int(1)), decl("y", int(2))]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        // entry should be the first Assign (x = 1)
        let entry = graph.entry;
        match node_kind(&graph, entry) {
            IrNodeKind::Assign {
                var, scope, next, ..
            } => {
                assert_eq!(var, "x");
                assert_eq!(*scope, DeclKind::Variable);
                // next should point at the second Assign
                let next2 = *next;
                match node_kind(&graph, next2) {
                    IrNodeKind::Assign {
                        var: var2,
                        next: next3,
                        ..
                    } => {
                        assert_eq!(var2, "y");
                        // After the last statement the chain should end
                        assert_eq!(*next3, NODE_END);
                    }
                    other => panic!("expected second Assign, got {:?}", other),
                }
            }
            other => panic!("expected Assign, got {:?}", other),
        }
    }

    /// An If without else: `else_node` of the Branch should point directly at
    /// the merge `Nop`, while `then_node` points at the compiled then-body.
    #[test]
    fn test_if_without_else_branch_else_is_merge() {
        let condition = Ast::equals_op(ident("a"), int(0));
        let then_block = Ast::block(vec![decl("x", int(1))]);
        let ast = Ast::if_stmt(condition.clone(), then_block, None);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        let entry = graph.entry;
        match node_kind(&graph, entry) {
            IrNodeKind::Branch {
                then_node,
                else_node,
                ..
            } => {
                // else_node must be the merge Nop itself (no else body was compiled).
                match node_kind(&graph, *else_node) {
                    IrNodeKind::Nop { .. } => {}
                    other => panic!(
                        "expected else_node to be a Nop merge point, got {:?}",
                        other
                    ),
                }

                // then_node must be a different node (the compiled then-body).
                assert_ne!(
                    then_node, else_node,
                    "then_node must differ from the merge Nop else_node"
                );

                // The then-body's last node must link back to the same merge Nop.
                match node_kind(&graph, *then_node) {
                    IrNodeKind::Assign { next, .. } => {
                        assert_eq!(
                            *next, *else_node,
                            "then-body's next must point at the merge Nop"
                        );
                    }
                    other => panic!("expected Assign as then-body entry, got {:?}", other),
                }
            }
            other => panic!("expected Branch, got {:?}", other),
        }
    }

    /// LabeledBlock + Jump: Jump.target should equal the EnterScope NodeId.
    #[test]
    fn test_labeled_block_and_jump_resolve() {
        // label scene1 { let x = 42 }
        // jump scene1
        let labeled =
            Ast::labeled_block("scene1".to_string(), Ast::block(vec![decl("x", int(42))]));
        let jump = Ast::jump_stmt("scene1".to_string(), false);
        let ast = Ast::block(vec![labeled, jump]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        // The labels map must contain "scene1".
        let enter_id = match graph.labels.get("scene1") {
            Some(id) => *id,
            None => panic!("label 'scene1' not registered in graph.labels"),
        };

        // Find the Jump node by walking the graph nodes.
        let jump_node = match graph
            .nodes
            .iter()
            .find(|n| matches!(n.kind, IrNodeKind::Jump { .. }))
        {
            Some(n) => n,
            None => panic!("no Jump node found in graph"),
        };

        match &jump_node.kind {
            IrNodeKind::Jump { target } => {
                assert_eq!(
                    *target, enter_id,
                    "Jump.target must point at EnterScope NodeId"
                );
            }
            _ => unreachable!(),
        }

        // The entry for "scene1" must be an EnterScope node.
        match node_kind(&graph, enter_id) {
            IrNodeKind::EnterScope { label, .. } => {
                assert_eq!(label, "scene1");
            }
            other => panic!("expected EnterScope, got {:?}", other),
        }
    }

    /// Dialogue node preserves decorators.
    #[test]
    fn test_dialogue_preserves_decorators() {
        use crate::parser::ast::Decorator;

        let speakers = Ast::expr_list(vec![ident("Alice")]);
        let lines = Ast::expr_list(vec![str_lit("Hello!")]);
        let deco = Decorator::bare("mood".to_string());

        let ast = Ast::new_decorated(
            AstContent::Dialogue {
                speakers: Box::new(speakers),
                content: Box::new(lines),
            },
            vec![deco.clone()],
        );

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        match node_kind(&graph, graph.entry) {
            IrNodeKind::Dialogue { decorators, .. } => {
                assert_eq!(decorators.len(), 1);
                assert_eq!(decorators[0].name(), "mood");
            }
            other => panic!("expected Dialogue, got {:?}", other),
        }
    }

    /// Menu with two options compiles to a Choice with two IrChoiceOptions.
    #[test]
    fn test_menu_two_options() {
        let opt1 = Ast::menu_option(
            "Option A".to_string(),
            Ast::block(vec![decl("picked", int(1))]),
        );
        let opt2 = Ast::menu_option(
            "Option B".to_string(),
            Ast::block(vec![decl("picked", int(2))]),
        );
        let ast = Ast::menu(vec![opt1, opt2]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        match node_kind(&graph, graph.entry) {
            IrNodeKind::Choice { options, .. } => {
                assert_eq!(options.len(), 2, "expected 2 options");
                assert_eq!(options[0].label, "Option A");
                assert_eq!(options[1].label, "Option B");
                // Each entry must point at a real node (not NODE_END itself at top).
                assert_ne!(options[0].entry, NODE_END);
                assert_ne!(options[1].entry, NODE_END);
            }
            other => panic!("expected Choice, got {:?}", other),
        }
    }

    /// Match statement compiles to Switch with correct arm count and a Nop merge.
    #[test]
    fn test_match_compiles_to_switch() {
        let scrutinee = ident("direction");
        let arm_north = MatchArm::new(
            MatchPattern::Value(Ast::value(RuntimeValue::IdentPath(vec![
                "North".to_string(),
            ]))),
            Ast::block(vec![decl("heading", int(0))]),
        );
        let arm_wild = MatchArm::new(
            MatchPattern::Wildcard,
            Ast::block(vec![decl("heading", int(-1))]),
        );
        let ast = Ast::match_stmt(scrutinee, vec![arm_north, arm_wild]);

        let graph = match Compiler::compile(&ast) {
            Ok(g) => g,
            Err(e) => panic!("compile failed: {}", e),
        };

        match node_kind(&graph, graph.entry) {
            IrNodeKind::Switch { arms, default, .. } => {
                assert_eq!(arms.len(), 1, "expected one non-wildcard arm");
                assert!(default.is_some(), "expected a default from wildcard arm");
            }
            other => panic!("expected Switch, got {:?}", other),
        }
    }

    /// Jump to an undefined label returns a CompilerError::UnknownLabel.
    #[test]
    fn test_jump_unknown_label_error() {
        let ast = Ast::jump_stmt("nonexistent".to_string(), false);
        let result = Compiler::compile(&ast);
        assert!(
            matches!(result, Err(CompilerError::UnknownLabel(ref l)) if l == "nonexistent"),
            "expected UnknownLabel error, got {:?}",
            result,
        );
    }

    // ── import / compile_with_loader tests ───────────────────────────────────

    /// compile_with_loader resolves a simple import: the merged graph contains
    /// the module's label under "alias::label_name".
    #[test]
    fn test_compile_with_loader_resolves_import() {
        let mut loader = MemLoader::new();
        loader.add("lib.urd", "label greet { let msg = \"hello\" }");

        let main_ast = Ast::block(vec![Ast::import("lib.urd".to_string(), "lib".to_string())]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        assert!(
            graph.labels.contains_key("lib::greet"),
            "expected 'lib::greet' in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// The same module imported from two different places (diamond dependency)
    /// does not trigger a CircularImport error.
    #[test]
    fn test_diamond_import_does_not_error() {
        let mut loader = MemLoader::new();
        // base.urd is imported by both mid.urd and directly by main
        loader.add("base.urd", "label entry { let x = 1 }");
        loader.add("mid.urd", "import \"base.urd\" as base");

        let main_ast = Ast::block(vec![
            Ast::import("mid.urd".to_string(), "mid".to_string()),
            Ast::import("base.urd".to_string(), "base".to_string()),
        ]);

        let result = Compiler::compile_with_loader(&main_ast, &loader);
        assert!(
            result.is_ok(),
            "diamond import should not produce a circular error, got: {:?}",
            result
        );
        let graph = result.unwrap();
        assert!(
            graph.labels.contains_key("base::entry"),
            "'base::entry' must be in graph.labels, got: {:?}",
            graph.labels.keys().collect::<Vec<_>>()
        );
    }

    /// A `jump alias.label` resolves to the correct (merged) NodeId.
    #[test]
    fn test_cross_module_jump_resolves_to_merged_node_id() {
        let mut loader = MemLoader::new();
        loader.add("scenes.urd", "label intro { let x = 1 }");

        let import_node = Ast::import("scenes.urd".to_string(), "scenes".to_string());
        // jump scenes.intro  → stored as Jump { label: "scenes.intro" }
        let jump_node = Ast::jump_stmt("scenes.intro".to_string(), false);
        let main_ast = Ast::block(vec![import_node, jump_node]);

        let graph =
            Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

        // The merged label must exist.
        let enter_id = *graph
            .labels
            .get("scenes::intro")
            .expect("'scenes::intro' not in graph.labels");

        // Find the Jump node and verify its target.
        let jump_node = graph
            .nodes
            .iter()
            .find(|n| matches!(n.kind, IrNodeKind::Jump { .. }))
            .expect("no Jump node in graph");

        match &jump_node.kind {
            IrNodeKind::Jump { target } => {
                assert_eq!(
                    *target, enter_id,
                    "Jump.target must equal the merged EnterScope NodeId"
                );
            }
            _ => unreachable!(),
        }
    }

    /// Circular imports produce CompilerError::CircularImport.
    #[test]
    fn test_circular_import_returns_error() {
        // a.urd imports b.urd, b.urd imports a.urd  → circular
        let mut loader = MemLoader::new();
        loader.add("a.urd", "import \"b.urd\" as b");
        loader.add("b.urd", "import \"a.urd\" as a");

        let main_ast = Ast::block(vec![Ast::import("a.urd".to_string(), "a".to_string())]);

        let result = Compiler::compile_with_loader(&main_ast, &loader);
        assert!(
            matches!(result, Err(CompilerError::CircularImport(ref p)) if p == "a.urd"),
            "expected CircularImport(\"a.urd\"), got {:?}",
            result
        );
    }

    /// A missing module produces CompilerError::ModuleLoadError.
    #[test]
    fn test_missing_module_returns_load_error() {
        let loader = MemLoader::new(); // empty — no files registered

        let main_ast = Ast::block(vec![Ast::import(
            "missing.urd".to_string(),
            "missing".to_string(),
        )]);

        let result = Compiler::compile_with_loader(&main_ast, &loader);
        assert!(
            matches!(
                result,
                Err(CompilerError::ModuleLoadError { ref path, .. }) if path == "missing.urd"
            ),
            "expected ModuleLoadError for 'missing.urd', got {:?}",
            result
        );
    }

    /// `end!()` call compiles to an `IrNodeKind::End` terminal node.
    #[test]
    fn test_end_bang_compiles_to_end_node() {
        // Build a Call AST node with func_path = IdentPath(["end!"]) manually,
        // without relying on the lexer/parser.
        let func_path = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_string()]));
        let params = Ast::block(vec![]);
        let call_ast = Ast::call(func_path, params);
        let script_ast = Ast::block(vec![call_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        assert!(
            graph
                .nodes
                .iter()
                .any(|n| matches!(n.kind, IrNodeKind::End)),
            "expected an End node in the graph, got: {:?}",
            graph.nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_todo_bang_compiles_to_todo_node() {
        // Build a Call AST node with func_path = IdentPath(["todo!"]) manually.
        let func_path = Ast::value(RuntimeValue::IdentPath(vec!["todo!".to_string()]));
        let params = Ast::block(vec![]);
        let call_ast = Ast::call(func_path, params);
        let script_ast = Ast::block(vec![call_ast]);

        let graph = Compiler::compile(&script_ast).expect("compile failed");
        assert!(
            graph
                .nodes
                .iter()
                .any(|n| matches!(n.kind, IrNodeKind::Todo)),
            "expected a Todo node in the graph, got: {:?}",
            graph.nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
        );
    }

    /// Cross-module assignment `alias.var = value` compiles to an Assign node
    /// with Global scope and a namespaced key "alias::var".
    #[test]
    fn test_cross_module_assignment_compiles_to_global_scope() {
        // Build AST for: `mod.counter = 42`
        let lhs = Ast::value(RuntimeValue::IdentPath(vec![
            "mod".to_string(),
            "counter".to_string(),
        ]));
        let rhs = int(42);
        let assign = Ast::assign_op(lhs, rhs);
        let ast = Ast::block(vec![assign]);

        let graph = Compiler::compile(&ast).expect("compile failed");

        match node_kind(&graph, graph.entry) {
            IrNodeKind::Assign { var, scope, .. } => {
                assert_eq!(var, "mod::counter", "variable name must be namespaced");
                assert_eq!(
                    *scope,
                    DeclKind::Global,
                    "cross-module assignment must use Global scope"
                );
            }
            other => panic!("expected Assign, got {:?}", other),
        }
    }
}
