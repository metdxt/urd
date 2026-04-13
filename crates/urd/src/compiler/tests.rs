use super::*;
use crate::{
    ir::{IrEdge, IrGraph, IrNodeKind},
    parser::ast::{Ast, AstContent, DeclKind, MatchArm, MatchPattern},
    runtime::value::RuntimeValue,
    vm::loader::MemLoader,
};
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;

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
    let entry_idx = graph.entry.expect("graph must have an entry node");
    assert!(
        matches!(
            node_kind(&graph, entry_idx),
            IrNodeKind::DefineScriptDecorator { name, params, .. }
            if name == "shake" && params == &["amount"]
        ),
        "expected DefineScriptDecorator at entry, got {:?}",
        node_kind(&graph, entry_idx)
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
    let entry_idx = graph.entry.expect("graph must have an entry node");
    let entry_kind = node_kind(&graph, entry_idx);
    let IrNodeKind::DefineScriptDecorator { .. } = entry_kind else {
        panic!("expected DefineScriptDecorator, got {:?}", entry_kind)
    };

    // Follow the Next edge from DefineScriptDecorator to find Dialogue.
    let next_idx = next_of(&graph, entry_idx).expect("DefineScriptDecorator must have a Next edge");
    assert!(
        matches!(node_kind(&graph, next_idx), IrNodeKind::Dialogue { .. }),
        "expected Dialogue after DefineScriptDecorator, got {:?}",
        node_kind(&graph, next_idx)
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
    let entry_idx = graph.entry.expect("entry");
    assert!(
        matches!(
            node_kind(&graph, entry_idx),
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
        node_kind(&graph, entry_idx)
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

/// Borrow the [`IrNodeKind`] for a given [`NodeIndex`] from the graph.
///
/// # Panics
/// Panics if `id` is not a valid node in the graph.
fn node_kind(graph: &IrGraph, id: NodeIndex) -> &IrNodeKind {
    &graph.graph[id]
}

/// Follow the single outgoing [`IrEdge::Next`] edge from `id`, returning
/// the target [`NodeIndex`] if one exists.
fn next_of(graph: &IrGraph, id: NodeIndex) -> Option<NodeIndex> {
    graph
        .graph
        .edges(id)
        .find(|e| matches!(e.weight(), IrEdge::Next))
        .map(|e| e.target())
}

/// Follow an outgoing edge of a specific kind from `id`.
fn edge_target(graph: &IrGraph, id: NodeIndex, kind: &IrEdge) -> Option<NodeIndex> {
    graph
        .graph
        .edges(id)
        .find(|e| e.weight() == kind)
        .map(|e| e.target())
}

// ── tests ─────────────────────────────────────────────────────────────────

/// A Block with two declarations compiles to a right-linked Assign chain
/// (connected by Next edges).
#[test]
fn test_block_declarations_chain() {
    let ast = Ast::block(vec![decl("x", int(1)), decl("y", int(2))]);

    let graph = match Compiler::compile(&ast) {
        Ok(g) => g,
        Err(e) => panic!("compile failed: {}", e),
    };

    // entry should be the first Assign (x = 1)
    let entry = graph.entry.expect("graph must have entry");
    match node_kind(&graph, entry) {
        IrNodeKind::Assign { var, scope, .. } => {
            assert_eq!(var, "x");
            assert_eq!(*scope, DeclKind::Variable);
            // Follow the Next edge to the second Assign.
            let next2 = next_of(&graph, entry).expect("first Assign must have a Next edge");
            match node_kind(&graph, next2) {
                IrNodeKind::Assign { var: var2, .. } => {
                    assert_eq!(var2, "y");
                    // After the last statement the chain must end (no Next edge).
                    assert!(
                        next_of(&graph, next2).is_none(),
                        "last Assign must have no outgoing Next edge"
                    );
                }
                other => panic!("expected second Assign, got {:?}", other),
            }
        }
        other => panic!("expected Assign, got {:?}", other),
    }
}

/// An If without else: the `Else` edge of the Branch should point directly
/// at the merge `Nop`, while the `Then` edge points at the compiled then-body.
#[test]
fn test_if_without_else_branch_else_is_merge() {
    let condition = Ast::equals_op(ident("a"), int(0));
    let then_block = Ast::block(vec![decl("x", int(1))]);
    let ast = Ast::if_stmt(condition.clone(), then_block, None);

    let graph = match Compiler::compile(&ast) {
        Ok(g) => g,
        Err(e) => panic!("compile failed: {}", e),
    };

    let entry = graph.entry.expect("entry");
    match node_kind(&graph, entry) {
        IrNodeKind::Branch { .. } => {
            let then_idx =
                edge_target(&graph, entry, &IrEdge::Then).expect("Branch must have a Then edge");
            let else_idx =
                edge_target(&graph, entry, &IrEdge::Else).expect("Branch must have an Else edge");

            // else_idx must be the merge Nop itself (no else body was compiled).
            match node_kind(&graph, else_idx) {
                IrNodeKind::Nop => {}
                other => panic!(
                    "expected else target to be a Nop merge point, got {:?}",
                    other
                ),
            }

            // then_idx must be a different node (the compiled then-body).
            assert_ne!(
                then_idx, else_idx,
                "then target must differ from the merge Nop else target"
            );

            // The then branch is now wrapped: PushScope → body → PopScope → merge.
            match node_kind(&graph, then_idx) {
                IrNodeKind::PushScope => {
                    let body_idx = next_of(&graph, then_idx)
                        .expect("PushScope must have a Next edge to the body");
                    match node_kind(&graph, body_idx) {
                        IrNodeKind::Assign { .. } => {}
                        other => panic!("expected Assign inside PushScope, got {:?}", other),
                    }
                    let pop_idx = next_of(&graph, body_idx)
                        .expect("then-body Assign must have a Next edge to PopScope");
                    match node_kind(&graph, pop_idx) {
                        IrNodeKind::PopScope => {}
                        other => panic!("expected PopScope after then-body, got {:?}", other),
                    }
                    let pop_next =
                        next_of(&graph, pop_idx).expect("PopScope must have a Next edge to merge");
                    assert_eq!(
                        pop_next, else_idx,
                        "PopScope's Next must point at the merge Nop"
                    );
                }
                other => panic!("expected PushScope as then-body entry, got {:?}", other),
            }
        }
        other => panic!("expected Branch, got {:?}", other),
    }
}

/// LabeledBlock + Jump: the Jump edge target should equal the EnterScope NodeIndex.
#[test]
fn test_labeled_block_and_jump_resolve() {
    // label scene1 { let x = 42 }
    // jump scene1
    let labeled = Ast::labeled_block("scene1".to_string(), Ast::block(vec![decl("x", int(42))]));
    let jump = Ast::jump_stmt("scene1".to_string(), false);
    let ast = Ast::block(vec![labeled, jump]);

    let graph = match Compiler::compile(&ast) {
        Ok(g) => g,
        Err(e) => panic!("compile failed: {}", e),
    };

    // The labels map must contain "scene1".
    let enter_id = match graph.labels.get("scene1") {
        Some(&id) => id,
        None => panic!("label 'scene1' not registered in graph.labels"),
    };

    // Find the Jump node by iterating over all nodes.
    let jump_idx = graph
        .graph
        .node_indices()
        .find(|&idx| matches!(graph.graph[idx], IrNodeKind::Jump))
        .expect("no Jump node found in graph");

    // The Jump edge must target the EnterScope node.
    let jump_target =
        edge_target(&graph, jump_idx, &IrEdge::Jump).expect("Jump node must have a Jump edge");
    assert_eq!(
        jump_target, enter_id,
        "Jump edge target must point at EnterScope NodeIndex"
    );

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

    let entry = graph.entry.expect("entry");
    match node_kind(&graph, entry) {
        IrNodeKind::Dialogue { decorators, .. } => {
            assert_eq!(decorators.len(), 1);
            assert_eq!(decorators[0].name(), "mood");
        }
        other => panic!("expected Dialogue, got {:?}", other),
    }
}

/// Menu with two options compiles to a Choice with two IrChoiceOptions and
/// outgoing Option(0)/Option(1) edges.
#[test]
fn test_menu_two_options() {
    let opt1 = Ast::menu_option(
        "Option A".to_string(),
        Ast::block(vec![decl("picked", int(1))]),
        false,
    );
    let opt2 = Ast::menu_option(
        "Option B".to_string(),
        Ast::block(vec![decl("picked", int(2))]),
        false,
    );
    let ast = Ast::menu(vec![opt1, opt2]);

    let graph = match Compiler::compile(&ast) {
        Ok(g) => g,
        Err(e) => panic!("compile failed: {}", e),
    };

    let entry = graph.entry.expect("entry");
    match node_kind(&graph, entry) {
        IrNodeKind::Choice { options, .. } => {
            assert_eq!(options.len(), 2, "expected 2 options");
            assert_eq!(options[0].label, "Option A");
            assert_eq!(options[1].label, "Option B");
            // Each option must have a corresponding outgoing Option(i) edge.
            let opt0_entry = graph
                .graph
                .edges(entry)
                .find(|e| matches!(e.weight(), IrEdge::Option(0)))
                .map(|e| e.target());
            let opt1_entry = graph
                .graph
                .edges(entry)
                .find(|e| matches!(e.weight(), IrEdge::Option(1)))
                .map(|e| e.target());
            assert!(opt0_entry.is_some(), "Choice must have an Option(0) edge");
            assert!(opt1_entry.is_some(), "Choice must have an Option(1) edge");
            assert_ne!(
                opt0_entry, opt1_entry,
                "options must target different entry nodes"
            );
        }
        other => panic!("expected Choice, got {:?}", other),
    }
}

/// Match statement compiles to Switch with correct arm count and a Default edge.
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

    let entry = graph.entry.expect("entry");
    match node_kind(&graph, entry) {
        IrNodeKind::Switch { arms, .. } => {
            assert_eq!(arms.len(), 1, "expected one non-wildcard arm");
            // The wildcard arm must produce a Default edge.
            let has_default = graph
                .graph
                .edges(entry)
                .any(|e| matches!(e.weight(), IrEdge::Default));
            assert!(
                has_default,
                "expected a Default edge from Switch for wildcard arm"
            );
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

/// Duplicate labels in a single module are rejected at compile time.
#[test]
fn test_duplicate_label_in_single_module_errors() {
    let ast = Ast::block(vec![
        Ast::labeled_block("scene".to_string(), Ast::block(vec![])),
        Ast::labeled_block("scene".to_string(), Ast::block(vec![])),
    ]);

    let result = Compiler::compile(&ast);

    assert!(
        matches!(result, Err(CompilerError::DuplicateLabel(ref l)) if l == "scene"),
        "expected DuplicateLabel('scene'), got {:?}",
        result
    );
}

/// Duplicate labels inside an imported module are rejected in loader path.
#[test]
fn test_duplicate_label_in_imported_module_errors() {
    let mut loader = MemLoader::new();
    loader.add(
        "dup.urd",
        "label same { let x = 1 }\nlabel same { let y = 2 }\n",
    );

    let main_ast = Ast::block(vec![Ast::import_module(
        "dup.urd".to_string(),
        "dup".to_string(),
    )]);

    let result = Compiler::compile_with_loader(&main_ast, &loader);

    assert!(
        matches!(result, Err(CompilerError::DuplicateLabel(ref l)) if l == "same"),
        "expected DuplicateLabel('same') from imported module, got {:?}",
        result
    );
}

// ── import / compile_with_loader tests ───────────────────────────────────

/// compile_with_loader resolves a simple import: the merged graph contains
/// the module's label under "alias::label_name".
#[test]
fn test_compile_with_loader_resolves_import() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", "label greet { let msg = \"hello\" }");

    let main_ast = Ast::block(vec![Ast::import_module(
        "lib.urd".to_string(),
        "lib".to_string(),
    )]);

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
        Ast::import_module("mid.urd".to_string(), "mid".to_string()),
        Ast::import_module("base.urd".to_string(), "base".to_string()),
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

/// A `jump alias.label` resolves to the correct (merged) NodeIndex.
#[test]
fn test_cross_module_jump_resolves_to_merged_node_id() {
    let mut loader = MemLoader::new();
    loader.add("scenes.urd", "@entry\nlabel intro { let x = 1 }");

    let import_node = Ast::import_module("scenes.urd".to_string(), "scenes".to_string());
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

    // Find the Jump node and verify its Jump edge target.
    let jump_idx = graph
        .graph
        .node_indices()
        .find(|&idx| matches!(graph.graph[idx], IrNodeKind::Jump))
        .expect("no Jump node in graph");

    let jump_target =
        edge_target(&graph, jump_idx, &IrEdge::Jump).expect("Jump node must have a Jump edge");
    assert_eq!(
        jump_target, enter_id,
        "Jump edge target must equal the merged EnterScope NodeIndex"
    );
}

/// Circular imports (a.urd ↔ b.urd) now compile successfully.
///
/// Because top-level is definitions-only, neither module needs the other
/// to be fully compiled before its own labels can be pre-allocated.
/// The 4-phase flat pipeline handles this by pre-allocating all label Nop
/// stubs before any IR is emitted.
#[test]
fn test_circular_import_compiles_successfully() {
    let mut loader = MemLoader::new();
    loader.add(
        "a.urd",
        "import \"b.urd\" as b\n@entry\nlabel a_label {\n  jump b.b_label\n}\n",
    );
    loader.add(
        "b.urd",
        "import \"a.urd\" as a\n@entry\nlabel b_label {\n  jump a.a_label\n}\n",
    );

    let main_ast = Ast::block(vec![Ast::import_module(
        "a.urd".to_string(),
        "a".to_string(),
    )]);

    let result = Compiler::compile_with_loader(&main_ast, &loader);
    assert!(
        result.is_ok(),
        "circular import should compile successfully, got: {:?}",
        result
    );

    let graph = result.unwrap();
    assert!(
        graph.labels.contains_key("a::a_label"),
        "a::a_label must be in the label map; got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    assert!(
        graph.labels.contains_key("b::b_label"),
        "b::b_label (from a's whole-module import of b as 'b') must be in the label map; got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
}

/// A mutual symbol import also compiles cleanly.
#[test]
fn test_circular_symbol_import_compiles_successfully() {
    let mut loader = MemLoader::new();
    loader.add(
        "items.urd",
        "import (greet) from \"greetings.urd\"\nlabel show_items {\n  jump greet\n}\n",
    );
    loader.add(
        "greetings.urd",
        "import (show_items) from \"items.urd\"\nlabel greet {\n  jump show_items\n}\n",
    );

    let main_src = "import (show_items) from \"items.urd\"\nlabel start {\n  jump show_items\n}\n";
    let main_ast = crate::compiler::loader::parse_source(main_src).expect("parse");
    let result = Compiler::compile_with_loader(&main_ast, &loader);
    assert!(
        result.is_ok(),
        "circular symbol import should compile successfully, got: {:?}",
        result
    );
    let graph = result.unwrap();
    assert!(
        graph.labels.contains_key("show_items"),
        "show_items must be directly accessible; labels: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
}

/// A missing module produces CompilerError::ModuleLoadError.
#[test]
fn test_missing_module_returns_load_error() {
    let loader = MemLoader::new(); // empty — no files registered

    let main_ast = Ast::block(vec![Ast::import_module(
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
    let func_path = Ast::value(RuntimeValue::IdentPath(vec!["end!".to_string()]));
    let params = Ast::block(vec![]);
    let call_ast = Ast::call(func_path, params);
    let script_ast = Ast::block(vec![call_ast]);

    let graph = Compiler::compile(&script_ast).expect("compile failed");
    assert!(
        graph
            .graph
            .node_weights()
            .any(|k| matches!(k, IrNodeKind::End)),
        "expected an End node in the graph, got: {:?}",
        graph.graph.node_weights().collect::<Vec<_>>()
    );
}

#[test]
fn test_todo_bang_compiles_to_todo_node() {
    let func_path = Ast::value(RuntimeValue::IdentPath(vec!["todo!".to_string()]));
    let params = Ast::block(vec![]);
    let call_ast = Ast::call(func_path, params);
    let script_ast = Ast::block(vec![call_ast]);

    let graph = Compiler::compile(&script_ast).expect("compile failed");
    assert!(
        graph
            .graph
            .node_weights()
            .any(|k| matches!(k, IrNodeKind::Todo)),
        "expected a Todo node in the graph, got: {:?}",
        graph.graph.node_weights().collect::<Vec<_>>()
    );
}

/// Single-symbol import: `import greet as hello from "lib.urd"` should
/// expose "hello" directly in graph.labels (no "lib::" prefix).
#[test]
fn test_single_symbol_import_exposes_alias_directly() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", "label greet { let msg = \"hello\" }");

    let import_node = Ast::import(
        "lib.urd".to_string(),
        vec![crate::parser::ast::ImportSymbol {
            original: Some("greet".to_string()),
            alias: "hello".to_string(),
        }],
    );
    let main_ast = Ast::block(vec![import_node]);

    let graph =
        Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

    assert!(
        graph.labels.contains_key("hello"),
        "expected 'hello' in graph.labels, got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    // The whole-module namespace prefix must NOT be present.
    assert!(
        !graph.labels.contains_key("lib::greet"),
        "whole-module namespace 'lib::greet' must not appear for a symbol import"
    );
}

/// Single-symbol import with no alias: `import greet from "lib.urd"` should
/// expose "greet" directly (alias == original).
#[test]
fn test_single_symbol_import_no_alias_uses_original_name() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", "label greet { let msg = \"hello\" }");

    let import_node = Ast::import(
        "lib.urd".to_string(),
        vec![crate::parser::ast::ImportSymbol {
            original: Some("greet".to_string()),
            alias: "greet".to_string(),
        }],
    );
    let main_ast = Ast::block(vec![import_node]);

    let graph =
        Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

    assert!(
        graph.labels.contains_key("greet"),
        "expected 'greet' in graph.labels, got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
}

/// Multi-symbol import: `import (intro as start, outro) from "scenes.urd"`
/// should expose both "start" and "outro" directly in graph.labels.
#[test]
fn test_multi_symbol_import_exposes_all_aliases() {
    let mut loader = MemLoader::new();
    loader.add(
        "scenes.urd",
        "label intro { let x = 1 }\nlabel outro { let y = 2 }",
    );

    let import_node = Ast::import(
        "scenes.urd".to_string(),
        vec![
            crate::parser::ast::ImportSymbol {
                original: Some("intro".to_string()),
                alias: "start".to_string(),
            },
            crate::parser::ast::ImportSymbol {
                original: Some("outro".to_string()),
                alias: "outro".to_string(),
            },
        ],
    );
    let main_ast = Ast::block(vec![import_node]);

    let graph =
        Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

    assert!(
        graph.labels.contains_key("start"),
        "expected 'start' in graph.labels, got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    assert!(
        graph.labels.contains_key("outro"),
        "expected 'outro' in graph.labels, got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
    // The symbol labels should point to valid (different) nodes.
    assert_ne!(
        graph.labels["start"], graph.labels["outro"],
        "intro and outro must map to different NodeIndexes"
    );
}

/// Symbol import aliases point to the same node as the original label would
/// after a whole-module import.  Verify that the NodeIndex for "hello" (aliasing
/// "greet") matches the EnterScope node emitted for that label.
#[test]
fn test_symbol_import_alias_points_to_correct_enter_scope_node() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", "label greet { let msg = \"hello\" }");

    let import_node = Ast::import(
        "lib.urd".to_string(),
        vec![crate::parser::ast::ImportSymbol {
            original: Some("greet".to_string()),
            alias: "hello".to_string(),
        }],
    );
    let main_ast = Ast::block(vec![import_node]);

    let graph =
        Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

    let hello_id = *graph.labels.get("hello").expect("'hello' not in labels");
    assert!(
        matches!(node_kind(&graph, hello_id), IrNodeKind::EnterScope { .. }),
        "alias 'hello' must point to an EnterScope node, got: {:?}",
        node_kind(&graph, hello_id)
    );
}

/// Symbol import from an already-completed module (diamond-like) still
/// registers the alias without re-running the prologue.
#[test]
fn test_symbol_import_from_already_completed_module_still_aliases() {
    let mut loader = MemLoader::new();
    // lib.urd is first imported as a whole module, then its symbol is
    // also imported directly — the second import hits the `completed` path.
    loader.add("lib.urd", "label greet { let msg = \"hello\" }");

    let whole_import = Ast::import_module("lib.urd".to_string(), "lib".to_string());
    let sym_import = Ast::import(
        "lib.urd".to_string(),
        vec![crate::parser::ast::ImportSymbol {
            original: Some("greet".to_string()),
            alias: "greet_direct".to_string(),
        }],
    );
    let main_ast = Ast::block(vec![whole_import, sym_import]);

    let graph =
        Compiler::compile_with_loader(&main_ast, &loader).expect("compile_with_loader failed");

    // Whole-module label must still be there.
    assert!(
        graph.labels.contains_key("lib::greet"),
        "whole-module label 'lib::greet' must be present"
    );
    // Symbol alias must also be registered even though the path was already compiled.
    assert!(
        graph.labels.contains_key("greet_direct"),
        "symbol alias 'greet_direct' must be present after completed-path import, got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
}

/// Cross-module assignment `alias.var = value` compiles to an Assign node
/// with Global scope and a namespaced key "alias::var".
#[test]
fn test_single_symbol_import_label_jumpable() {
    // `jump show_inventory` should compile when `show_inventory` is a
    // directly-imported symbol (registered in graph.labels via apply_aliases,
    // NOT in label_placeholders).
    let items_src = "label show_inventory {\n  return\n}\n";
    let main_src =
        "import (show_inventory) from \"items.urd\"\nlabel start {\n  jump show_inventory\n}\n";
    let mut loader = MemLoader::new();
    loader.add("items.urd", items_src);
    let ast = crate::compiler::loader::parse_source(main_src).expect("parse");
    let graph = Compiler::compile_with_loader(&ast, &loader).expect("compile");
    // The graph should have a Jump edge leading to the show_inventory EnterScope node.
    assert!(
        graph.labels.contains_key("show_inventory"),
        "show_inventory must be in labels; got: {:?}",
        graph.labels.keys().collect::<Vec<_>>()
    );
}

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

    let entry = graph.entry.expect("entry");
    match node_kind(&graph, entry) {
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

// ── Localization ID tests ────────────────────────────────────────────────

#[test]
fn test_compile_named_dialogue_gets_loc_id() {
    let speakers = Ast::expr_list(vec![str_lit("narrator")]);
    let lines = Ast::expr_list(vec![str_lit("Hello")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![dialogue]));
    let ast = Ast::block(vec![labeled]);

    let graph = Compiler::compile_named(&ast, "intro").expect("compile_named failed");

    let mut found = false;
    for node_idx in graph.graph.node_indices() {
        if let IrNodeKind::Dialogue { loc_id, .. } = &graph.graph[node_idx] {
            assert_eq!(loc_id.as_deref(), Some("intro-start-line_1"));
            found = true;
        }
    }
    assert!(found, "no Dialogue node found in graph");
}

#[test]
fn test_compile_no_file_stem_loc_id_is_none() {
    let speakers = Ast::expr_list(vec![str_lit("narrator")]);
    let lines = Ast::expr_list(vec![str_lit("Hello")]);
    let dialogue = Ast::dialogue(speakers, lines);
    let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![dialogue]));
    let ast = Ast::block(vec![labeled]);

    let graph = Compiler::compile(&ast).expect("compile failed");

    for node_idx in graph.graph.node_indices() {
        if let IrNodeKind::Dialogue { loc_id, .. } = &graph.graph[node_idx] {
            assert!(
                loc_id.is_none(),
                "loc_id should be None when no file stem given"
            );
        }
    }
}

#[test]
fn test_compile_named_menu_gets_loc_id() {
    let opt1 = Ast::menu_option("alcohol".to_string(), Ast::block(vec![]), false);
    let opt2 = Ast::menu_option("nicotine".to_string(), Ast::block(vec![]), false);
    let menu = Ast::menu(vec![opt1, opt2]);
    let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![menu]));
    let ast = Ast::block(vec![labeled]);

    let graph = Compiler::compile_named(&ast, "intro").expect("compile_named failed");

    let mut found_choice = false;
    for node_idx in graph.graph.node_indices() {
        if let IrNodeKind::Choice {
            loc_id, options, ..
        } = &graph.graph[node_idx]
        {
            assert_eq!(loc_id.as_deref(), Some("intro-start-menu_1"));
            assert_eq!(
                options[0].loc_id.as_deref(),
                Some("intro-start-menu_1-alcohol")
            );
            assert_eq!(
                options[1].loc_id.as_deref(),
                Some("intro-start-menu_1-nicotine")
            );
            found_choice = true;
        }
    }
    assert!(found_choice, "no Choice node found in graph");
}

#[test]
fn test_compile_named_two_menus_independent_counters() {
    let menu1 = Ast::menu(vec![Ast::menu_option(
        "a".to_string(),
        Ast::block(vec![]),
        false,
    )]);
    let menu2 = Ast::menu(vec![Ast::menu_option(
        "b".to_string(),
        Ast::block(vec![]),
        false,
    )]);
    let labeled = Ast::labeled_block("start".to_string(), Ast::block(vec![menu1, menu2]));
    let ast = Ast::block(vec![labeled]);

    let graph = Compiler::compile_named(&ast, "file").expect("compile_named failed");

    let mut choice_ids: Vec<String> = graph
        .graph
        .node_indices()
        .filter_map(|idx| {
            if let IrNodeKind::Choice { loc_id, .. } = &graph.graph[idx] {
                loc_id.clone()
            } else {
                None
            }
        })
        .collect();
    choice_ids.sort();
    assert_eq!(choice_ids, vec!["file-start-menu_1", "file-start-menu_2"]);
}

// ── @entry restriction tests ─────────────────────────────────────────

/// A whole-module import of a label without `@entry` must fail with
/// `PrivateLabel` when a cross-module jump targets it.
#[test]
fn test_cross_module_jump_to_non_entry_label_fails() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", "label secret { let x = 1 }");

    let main_src = "import \"lib.urd\" as lib\nlabel start {\n  jump lib.secret\n}\n";
    let ast = crate::compiler::loader::parse_source(main_src).expect("parse");
    let result = Compiler::compile_with_loader(&ast, &loader);

    assert!(
        matches!(result, Err(CompilerError::PrivateLabel(ref l)) if l == "lib.secret"),
        "expected PrivateLabel('lib.secret'), got: {:?}",
        result,
    );
}

/// A whole-module import of a label decorated with `@entry` must succeed.
#[test]
fn test_cross_module_jump_to_entry_label_succeeds() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", "@entry\nlabel public_api { let x = 1 }");

    let main_src = "import \"lib.urd\" as lib\nlabel start {\n  jump lib.public_api\n}\n";
    let ast = crate::compiler::loader::parse_source(main_src).expect("parse");
    let result = Compiler::compile_with_loader(&ast, &loader);

    assert!(
        result.is_ok(),
        "jump to @entry label should succeed, got: {:?}",
        result,
    );
}

/// Symbol imports (`import (label) from "..."`) bypass the `@entry`
/// restriction — the explicit opt-in is sufficient.
#[test]
fn test_symbol_import_bypasses_entry_restriction() {
    let mut loader = MemLoader::new();
    loader.add("lib.urd", "label secret { let x = 1 }");

    let main_src = "import (secret) from \"lib.urd\"\nlabel start {\n  jump secret\n}\n";
    let ast = crate::compiler::loader::parse_source(main_src).expect("parse");
    let result = Compiler::compile_with_loader(&ast, &loader);

    assert!(
        result.is_ok(),
        "symbol import should bypass @entry restriction, got: {:?}",
        result,
    );
}

/// Two dialogues in the same labeled block must receive loc IDs in **source
/// order**: the first speaker in the source file gets `line_1`, the second
/// gets `line_2`.  This is a regression test for the bug where the
/// right-to-left compilation pass caused IDs to be assigned in reverse.
#[test]
fn test_two_dialogues_in_same_block_get_source_order_ids() {
    // narrator appears first in source → must get line_1
    // elara appears second in source → must get line_2
    let narrator_lines = Ast::expr_list(vec![str_lit("You step into the Wandering Bazaar.")]);
    let elara_lines = Ast::expr_list(vec![str_lit("Welcome! I am Elara.")]);
    let narrator_dlg = Ast::dialogue(Ast::expr_list(vec![str_lit("narrator")]), narrator_lines);
    let elara_dlg = Ast::dialogue(Ast::expr_list(vec![str_lit("elara")]), elara_lines);
    let label_body = Ast::block(vec![narrator_dlg, elara_dlg]);
    let ast = Ast::block(vec![Ast::labeled_block("start".to_string(), label_body)]);

    let graph = Compiler::compile_named(&ast, "merchant").expect("compile failed");

    // Collect (first-string-content, loc_id) for every Dialogue node.
    let mut id_map: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for idx in graph.graph.node_indices() {
        if let IrNodeKind::Dialogue {
            loc_id: Some(id),
            lines,
            ..
        } = &graph.graph[idx]
        {
            // The lines node is an ExprList whose first element is a Str.
            // We identify which dialogue this is by inspecting the text.
            let hint = format!("{:?}", lines);
            id_map.insert(hint, id.clone());
        }
    }

    // Find the entry matching each expected loc_id.
    let narrator_id = id_map
        .iter()
        .find(|(hint, _)| hint.contains("Wandering Bazaar"))
        .map(|(_, id)| id.as_str());
    let elara_id = id_map
        .iter()
        .find(|(hint, _)| hint.contains("Elara"))
        .map(|(_, id)| id.as_str());

    assert_eq!(
        narrator_id,
        Some("merchant-start-line_1"),
        "narrator (first in source) must be merchant-start-line_1; got: {:?}",
        id_map,
    );
    assert_eq!(
        elara_id,
        Some("merchant-start-line_2"),
        "elara (second in source) must be merchant-start-line_2; got: {:?}",
        id_map,
    );
}

/// `struct Foo { a: int, b: str }` compiles to a single `IrNodeKind::DefineStruct`
/// node carrying the struct name and ordered field names.
#[test]
fn test_struct_decl_compiles_to_define_struct() {
    use crate::parser::ast::{StructField, TokSpan, TypeAnnotation};

    let fields = vec![
        StructField {
            name: "a".into(),
            span: TokSpan::default(),
            type_annotation: TypeAnnotation::Int,
        },
        StructField {
            name: "b".into(),
            span: TokSpan::default(),
            type_annotation: TypeAnnotation::Str,
        },
    ];
    let struct_ast = Ast::struct_decl("Foo".to_string(), fields);
    let script_ast = Ast::block(vec![struct_ast]);

    let graph = Compiler::compile(&script_ast).expect("compile failed");
    let entry_idx = graph.entry.expect("graph must have an entry node");

    match node_kind(&graph, entry_idx) {
        IrNodeKind::DefineStruct { name, fields } => {
            assert_eq!(name, "Foo");
            assert_eq!(fields, &["a", "b"]);
            assert!(
                next_of(&graph, entry_idx).is_none(),
                "single DefineStruct must have no outgoing Next edge"
            );
        }
        other => panic!("expected DefineStruct at entry, got {:?}", other),
    }
}

/// Building an AST deeper than `MAX_COMPILER_DEPTH` (512) must cause the
/// compiler to bail out with `CompilerError::Internal` rather than blowing
/// the call stack.
#[test]
fn test_max_compiler_depth_exceeded_returns_error() {
    // Build 513 nested if-blocks: if true { if true { if true { … } } }
    // The innermost body is an empty block.
    const DEPTH: usize = 513;

    let mut ast = Ast::block(vec![]);
    for _ in 0..DEPTH {
        let condition = Ast::equals_op(int(1), int(1));
        ast = Ast::if_stmt(condition, ast, None);
    }
    // Wrap in a top-level block so it looks like a normal script.
    let script = Ast::block(vec![ast]);

    let err = Compiler::compile(&script)
        .expect_err("compiling a script nested beyond MAX_COMPILER_DEPTH must fail");

    match &err {
        CompilerError::Internal(msg) => {
            assert!(
                msg.contains("maximum nesting depth exceeded"),
                "unexpected Internal message: {msg}",
            );
        }
        other => panic!("expected CompilerError::Internal for depth overflow, got: {other}",),
    }
}
