//! # Analysis Context
//!
//! Provides [`AnalysisContext`], built once per analysis run from a single pass
//! over the AST, and [`ScopeStack`], used per-pass for tracking variable type
//! annotations as blocks are entered and exited.

use std::collections::{HashMap, HashSet};

use crate::parser::ast::{Ast, AstContent, DeclKind, StructField, TypeAnnotation};
use crate::runtime::value::RuntimeValue;

// ---------------------------------------------------------------------------
// extract_decl_name
// ---------------------------------------------------------------------------

/// Extracts a plain (single-segment) variable name from a declaration-name AST node.
///
/// Returns `Some(name)` when `node` contains
/// `AstContent::Value(RuntimeValue::IdentPath(path))` with exactly one segment.
/// Returns `None` for multi-segment paths, non-value nodes, or anything else.
pub fn extract_decl_name(node: &Ast) -> Option<String> {
    match node.content() {
        AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() == 1 => {
            Some(path[0].clone())
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// AnalysisContext
// ---------------------------------------------------------------------------

/// Pre-computed, read-only information derived from a single pass over the AST.
///
/// Built once by [`AnalysisContext::build`] and then shared (by reference)
/// across all three analysis passes so that each pass does not need to re-walk
/// the tree to discover enum definitions or top-level variable annotations.
#[derive(Debug, Default)]
pub struct AnalysisContext {
    /// All enum declarations found anywhere in the script: `enum_name -> [variant, ...]`.
    pub enums: HashMap<String, Vec<String>>,

    /// All struct declarations found anywhere in the script: `struct_name -> [field, ...]`.
    pub structs: HashMap<String, Vec<StructField>>,

    /// Top-level typed variable declarations: `variable_name -> TypeAnnotation`.
    ///
    /// Only variables declared as direct children of the outermost `Block` that
    /// carry an explicit `: TypeAnnotation` are recorded here.
    pub top_level_vars: HashMap<String, TypeAnnotation>,

    /// All label names defined anywhere in the script.
    pub labels: HashSet<String>,
}

impl AnalysisContext {
    /// Build the context by walking `root` once.
    ///
    /// - **Enums** are collected recursively from the whole tree.
    /// - **Top-level typed variables** are collected only from the direct
    ///   statements of the outermost [`AstContent::Block`] (or the block inside
    ///   a top-level [`AstContent::LabeledBlock`]).
    pub fn build(root: &Ast) -> Self {
        let mut ctx = AnalysisContext::default();
        collect_enums(root, &mut ctx.enums);
        collect_structs(root, &mut ctx.structs);
        collect_top_level_vars(root, &mut ctx.top_level_vars);
        collect_labels(root, &mut ctx.labels);
        ctx
    }

    /// Build an analysis context from `root`, augmented with struct and enum
    /// definitions from imported modules.
    ///
    /// `imported_structs`: maps `"alias.StructName"` → field list (e.g. `"chars.Character"`)
    /// `imported_enums`:   maps `"alias.EnumName"` → variant list (e.g. `"chars.Faction"`)
    ///
    /// Both qualified (`"chars.Character"`) and unqualified (`"Character"`) keys
    /// are inserted so the type checker can resolve either form.
    pub fn build_with_imports(
        root: &Ast,
        imported_structs: HashMap<String, Vec<StructField>>,
        imported_enums: HashMap<String, Vec<String>>,
    ) -> Self {
        let mut ctx = Self::build(root);
        for (name, fields) in imported_structs {
            ctx.structs.insert(name, fields);
        }
        for (name, variants) in imported_enums {
            ctx.enums.insert(name, variants);
        }
        ctx
    }
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/// Recursively walks the entire subtree rooted at `node` and collects every
/// label name into `labels`.
fn collect_labels(node: &Ast, labels: &mut HashSet<String>) {
    match node.content() {
        AstContent::LabeledBlock { label, block } => {
            labels.insert(label.clone());
            collect_labels(block, labels);
        }
        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_labels(stmt, labels);
            }
        }
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_labels(condition, labels);
            collect_labels(then_block, labels);
            if let Some(eb) = else_block {
                collect_labels(eb, labels);
            }
        }
        AstContent::Match { arms, .. } => {
            for arm in arms {
                collect_labels(&arm.body, labels);
            }
        }
        AstContent::Menu { options } => {
            for opt in options {
                collect_labels(opt, labels);
            }
        }
        AstContent::MenuOption { content, .. } => {
            collect_labels(content, labels);
        }
        AstContent::DecoratorDef { body, .. } => {
            collect_labels(body, labels);
        }
        _ => {}
    }
}

/// Recursively walks the entire subtree rooted at `node` and inserts every
/// [`AstContent::EnumDecl`] it finds into `enums`.
fn collect_enums(node: &Ast, enums: &mut HashMap<String, Vec<String>>) {
    match node.content() {
        AstContent::EnumDecl { name, variants } => {
            enums.insert(name.clone(), variants.clone());
        }

        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_enums(stmt, enums);
            }
        }

        AstContent::LabeledBlock { block, .. } => {
            collect_enums(block, enums);
        }

        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_enums(condition, enums);
            collect_enums(then_block, enums);
            if let Some(eb) = else_block {
                collect_enums(eb, enums);
            }
        }

        AstContent::Declaration { decl_defs, .. } => {
            collect_enums(decl_defs, enums);
        }

        AstContent::Menu { options } => {
            for opt in options {
                collect_enums(opt, enums);
            }
        }

        AstContent::MenuOption { content, .. } => {
            collect_enums(content, enums);
        }

        AstContent::Match { scrutinee, arms } => {
            collect_enums(scrutinee, enums);
            for arm in arms {
                collect_enums(&arm.body, enums);
            }
        }

        AstContent::DecoratorDef { body, .. } => {
            collect_enums(body, enums);
        }

        AstContent::Return { value } => {
            if let Some(v) = value {
                collect_enums(v, enums);
            }
        }

        AstContent::BinOp { left, right, .. } => {
            collect_enums(left, enums);
            collect_enums(right, enums);
        }

        AstContent::UnaryOp { expr, .. } => {
            collect_enums(expr, enums);
        }

        AstContent::ExprList(exprs) => {
            for e in exprs {
                collect_enums(e, enums);
            }
        }

        AstContent::List(items) => {
            for item in items {
                collect_enums(item, enums);
            }
        }

        AstContent::Call { func_path, params } => {
            collect_enums(func_path, enums);
            collect_enums(params, enums);
        }

        AstContent::Subscript { object, key } => {
            collect_enums(object, enums);
            collect_enums(key, enums);
        }

        AstContent::SubscriptAssign { object, key, value } => {
            collect_enums(object, enums);
            collect_enums(key, enums);
            collect_enums(value, enums);
        }

        AstContent::Dialogue { speakers, content } => {
            collect_enums(speakers, enums);
            collect_enums(content, enums);
        }

        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                collect_enums(k, enums);
                collect_enums(v, enums);
            }
        }

        // Leaf nodes that cannot contain an EnumDecl.
        AstContent::Value(_)
        | AstContent::Jump { .. }
        | AstContent::LetCall { .. }
        | AstContent::Import { .. }
        | AstContent::StructDecl { .. } => {}
    }
}

/// Recursively walks the entire subtree rooted at `node` and inserts every
/// [`AstContent::StructDecl`] it finds into `structs`.
fn collect_structs(node: &Ast, structs: &mut HashMap<String, Vec<StructField>>) {
    match node.content() {
        AstContent::StructDecl { name, fields } => {
            structs.insert(name.clone(), fields.clone());
        }

        AstContent::Block(stmts) => {
            for stmt in stmts {
                collect_structs(stmt, structs);
            }
        }

        AstContent::LabeledBlock { block, .. } => {
            collect_structs(block, structs);
        }

        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_structs(condition, structs);
            collect_structs(then_block, structs);
            if let Some(eb) = else_block {
                collect_structs(eb, structs);
            }
        }

        AstContent::Declaration { decl_defs, .. } => {
            collect_structs(decl_defs, structs);
        }

        AstContent::Menu { options } => {
            for opt in options {
                collect_structs(opt, structs);
            }
        }

        AstContent::MenuOption { content, .. } => {
            collect_structs(content, structs);
        }

        AstContent::Match { scrutinee, arms } => {
            collect_structs(scrutinee, structs);
            for arm in arms {
                collect_structs(&arm.body, structs);
            }
        }

        AstContent::DecoratorDef { body, .. } => {
            collect_structs(body, structs);
        }

        AstContent::Return { value } => {
            if let Some(v) = value {
                collect_structs(v, structs);
            }
        }

        AstContent::BinOp { left, right, .. } => {
            collect_structs(left, structs);
            collect_structs(right, structs);
        }

        AstContent::UnaryOp { expr, .. } => {
            collect_structs(expr, structs);
        }

        AstContent::ExprList(exprs) => {
            for e in exprs {
                collect_structs(e, structs);
            }
        }

        AstContent::List(items) => {
            for item in items {
                collect_structs(item, structs);
            }
        }

        AstContent::Call { func_path, params } => {
            collect_structs(func_path, structs);
            collect_structs(params, structs);
        }

        AstContent::Subscript { object, key } => {
            collect_structs(object, structs);
            collect_structs(key, structs);
        }

        AstContent::SubscriptAssign { object, key, value } => {
            collect_structs(object, structs);
            collect_structs(key, structs);
            collect_structs(value, structs);
        }

        AstContent::Dialogue { speakers, content } => {
            collect_structs(speakers, structs);
            collect_structs(content, structs);
        }

        AstContent::Map(pairs) => {
            for (k, v) in pairs {
                collect_structs(k, structs);
                collect_structs(v, structs);
            }
        }

        // Leaf nodes that cannot contain a StructDecl.
        AstContent::Value(_)
        | AstContent::Jump { .. }
        | AstContent::LetCall { .. }
        | AstContent::Import { .. }
        | AstContent::EnumDecl { .. } => {}
    }
}

/// Examines the *direct* children of the outermost block in `root` and records
/// every [`AstContent::Declaration`] that carries a type annotation into `vars`.
///
/// Two root shapes are accepted:
/// - `root` is itself a `Block` -- its direct children are scanned.
/// - `root` is a `LabeledBlock` wrapping a `Block` -- the inner block's direct
///   children are scanned.
///
/// Any other shape is silently skipped.
fn collect_top_level_vars(root: &Ast, vars: &mut HashMap<String, TypeAnnotation>) {
    let stmts: &[Ast] = match root.content() {
        AstContent::Block(stmts) => stmts,
        AstContent::LabeledBlock { block, .. } => match block.content() {
            AstContent::Block(stmts) => stmts,
            _ => return,
        },
        _ => return,
    };

    for stmt in stmts {
        if let AstContent::Declaration {
            decl_name,
            type_annotation: Some(ann),
            kind,
            ..
        } = stmt.content()
        {
            // Accept global, const, and let declarations equally.
            // The match is here only to satisfy the exhaustiveness checker.
            match kind {
                DeclKind::Global | DeclKind::Constant | DeclKind::Variable => {}
            }
            if let Some(name) = extract_decl_name(decl_name) {
                vars.insert(name, ann.clone());
            }
        }
    }
}

// ---------------------------------------------------------------------------
// ScopeStack
// ---------------------------------------------------------------------------

/// A stack of variable-type scopes used by individual analysis passes.
///
/// The stack is seeded from [`AnalysisContext::top_level_vars`] and is pushed /
/// popped as the pass enters and exits nested blocks. Lookups search from the
/// innermost scope outward so that inner declarations shadow outer ones.
#[derive(Debug)]
pub struct ScopeStack {
    /// Frames ordered from outermost (index `0`) to innermost (last).
    /// `frames[0]` is seeded from top-level variable annotations.
    frames: Vec<HashMap<String, TypeAnnotation>>,
}

impl ScopeStack {
    /// Create a new stack pre-seeded with `top_level` variable annotations at
    /// the outermost frame.
    pub fn new(top_level: &HashMap<String, TypeAnnotation>) -> Self {
        ScopeStack {
            frames: vec![top_level.clone()],
        }
    }

    /// Push a fresh, empty scope frame (call when entering a nested block).
    pub fn push(&mut self) {
        self.frames.push(HashMap::new());
    }

    /// Pop the innermost scope frame (call when leaving a nested block).
    ///
    /// Does nothing if the stack only has the outermost frame left, to avoid
    /// panicking on malformed ASTs.
    pub fn pop(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }

    /// Declare `name` with type `ann` in the innermost frame.
    ///
    /// Re-declaring the same name in the same frame overwrites the previous
    /// annotation.
    pub fn declare(&mut self, name: String, ann: TypeAnnotation) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name, ann);
        }
    }

    /// Look up `name` from the innermost frame outward.
    ///
    /// Returns `None` if the name is not declared in any visible scope.
    pub fn lookup(&self, name: &str) -> Option<&TypeAnnotation> {
        for frame in self.frames.iter().rev() {
            if let Some(ann) = frame.get(name) {
                return Some(ann);
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{AstContent, DeclKind, StructField, TypeAnnotation};
    use crate::runtime::value::RuntimeValue;

    // -----------------------------------------------------------------------
    // Test helpers
    // -----------------------------------------------------------------------

    fn ident(name: &str) -> Ast {
        Ast::value(RuntimeValue::IdentPath(vec![name.to_owned()]))
    }

    fn int_val() -> Ast {
        Ast::value(RuntimeValue::Int(0))
    }

    fn typed_decl_node(name: &str, ann: TypeAnnotation, def: Ast) -> Ast {
        Ast::typed_decl(DeclKind::Variable, ident(name), ann, def)
    }

    // -----------------------------------------------------------------------
    // extract_decl_name
    // -----------------------------------------------------------------------

    #[test]
    fn extract_single_segment_succeeds() {
        let node = ident("foo");
        assert_eq!(extract_decl_name(&node), Some("foo".to_owned()));
    }

    #[test]
    fn extract_multi_segment_returns_none() {
        let node = Ast::value(RuntimeValue::IdentPath(vec![
            "foo".to_owned(),
            "bar".to_owned(),
        ]));
        assert_eq!(extract_decl_name(&node), None);
    }

    #[test]
    fn extract_empty_path_returns_none() {
        let node = Ast::value(RuntimeValue::IdentPath(vec![]));
        assert_eq!(extract_decl_name(&node), None);
    }

    #[test]
    fn extract_non_ident_returns_none() {
        let node = Ast::value(RuntimeValue::Int(42));
        assert_eq!(extract_decl_name(&node), None);
    }

    // -----------------------------------------------------------------------
    // AnalysisContext::build -- enum collection
    // -----------------------------------------------------------------------

    #[test]
    fn build_collects_top_level_enum() {
        let enum_node = Ast::enum_decl(
            "Dir".to_owned(),
            vec!["North".to_owned(), "South".to_owned()],
        );
        let root = Ast::block(vec![enum_node]);
        let ctx = AnalysisContext::build(&root);
        assert_eq!(
            ctx.enums.get("Dir"),
            Some(&vec!["North".to_owned(), "South".to_owned()])
        );
    }

    #[test]
    fn build_collects_enum_nested_in_label() {
        let enum_node = Ast::enum_decl(
            "Color".to_owned(),
            vec!["Red".to_owned(), "Green".to_owned(), "Blue".to_owned()],
        );
        let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![enum_node]));
        let root = Ast::block(vec![label]);
        let ctx = AnalysisContext::build(&root);
        assert!(ctx.enums.contains_key("Color"));
        assert_eq!(ctx.enums["Color"].len(), 3);
    }

    #[test]
    fn build_collects_multiple_enums() {
        let e1 = Ast::enum_decl("A".to_owned(), vec!["X".to_owned()]);
        let e2 = Ast::enum_decl("B".to_owned(), vec!["Y".to_owned(), "Z".to_owned()]);
        let root = Ast::block(vec![e1, e2]);
        let ctx = AnalysisContext::build(&root);
        assert_eq!(ctx.enums.len(), 2);
        assert!(ctx.enums.contains_key("A"));
        assert!(ctx.enums.contains_key("B"));
    }

    // -----------------------------------------------------------------------
    // AnalysisContext::build -- struct collection
    // -----------------------------------------------------------------------

    #[test]
    fn build_collects_top_level_struct() {
        let fields = vec![
            StructField {
                name: "name".into(),
                type_annotation: TypeAnnotation::Str,
            },
            StructField {
                name: "health".into(),
                type_annotation: TypeAnnotation::Int,
            },
        ];
        let root = Ast::block(vec![Ast::new(AstContent::StructDecl {
            name: "Player".into(),
            fields: fields.clone(),
        })]);
        let ctx = AnalysisContext::build(&root);
        assert_eq!(ctx.structs.get("Player"), Some(&fields));
    }

    #[test]
    fn build_collects_struct_nested_in_label() {
        let fields = vec![StructField {
            name: "x".into(),
            type_annotation: TypeAnnotation::Float,
        }];
        let inner = Ast::new(AstContent::StructDecl {
            name: "Vec2".into(),
            fields: fields.clone(),
        });
        let root = Ast::labeled_block("scene".into(), Ast::block(vec![inner]));
        let ctx = AnalysisContext::build(&root);
        assert_eq!(ctx.structs.get("Vec2"), Some(&fields));
    }

    // -----------------------------------------------------------------------
    // AnalysisContext::build -- top-level variable collection
    // -----------------------------------------------------------------------

    #[test]
    fn build_collects_typed_top_level_var() {
        let decl = typed_decl_node("score", TypeAnnotation::Int, int_val());
        let root = Ast::block(vec![decl]);
        let ctx = AnalysisContext::build(&root);
        assert_eq!(ctx.top_level_vars.get("score"), Some(&TypeAnnotation::Int));
    }

    #[test]
    fn build_ignores_untyped_top_level_var() {
        let untyped = Ast::decl(DeclKind::Variable, ident("tmp"), int_val());
        let root = Ast::block(vec![untyped]);
        let ctx = AnalysisContext::build(&root);
        assert!(!ctx.top_level_vars.contains_key("tmp"));
    }

    #[test]
    fn build_collects_multiple_typed_vars() {
        let d1 = typed_decl_node("a", TypeAnnotation::Int, int_val());
        let d2 = typed_decl_node("b", TypeAnnotation::Bool, int_val());
        let root = Ast::block(vec![d1, d2]);
        let ctx = AnalysisContext::build(&root);
        assert_eq!(ctx.top_level_vars.get("a"), Some(&TypeAnnotation::Int));
        assert_eq!(ctx.top_level_vars.get("b"), Some(&TypeAnnotation::Bool));
    }

    #[test]
    fn build_does_not_collect_vars_inside_nested_label() {
        let inner_decl = typed_decl_node("hidden", TypeAnnotation::Str, int_val());
        let label = Ast::labeled_block("scene".to_owned(), Ast::block(vec![inner_decl]));
        let root = Ast::block(vec![label]);
        let ctx = AnalysisContext::build(&root);
        // Variables declared inside a label block are NOT top-level.
        assert!(!ctx.top_level_vars.contains_key("hidden"));
    }

    #[test]
    fn build_accepts_labeled_block_as_root() {
        // A root that is itself a LabeledBlock wrapping a Block.
        let decl = typed_decl_node("x", TypeAnnotation::Float, int_val());
        let root = Ast::labeled_block("main".to_owned(), Ast::block(vec![decl]));
        let ctx = AnalysisContext::build(&root);
        assert_eq!(ctx.top_level_vars.get("x"), Some(&TypeAnnotation::Float));
    }

    // -----------------------------------------------------------------------
    // ScopeStack
    // -----------------------------------------------------------------------

    #[test]
    fn scope_stack_lookup_top_level_var() {
        let mut top = HashMap::new();
        top.insert("x".to_owned(), TypeAnnotation::Int);
        let stack = ScopeStack::new(&top);
        assert_eq!(stack.lookup("x"), Some(&TypeAnnotation::Int));
    }

    #[test]
    fn scope_stack_unknown_var_returns_none() {
        let stack = ScopeStack::new(&HashMap::new());
        assert_eq!(stack.lookup("nope"), None);
    }

    #[test]
    fn scope_stack_inner_shadows_outer() {
        let mut top = HashMap::new();
        top.insert("x".to_owned(), TypeAnnotation::Int);
        let mut stack = ScopeStack::new(&top);

        stack.push();
        stack.declare("x".to_owned(), TypeAnnotation::Bool);
        assert_eq!(stack.lookup("x"), Some(&TypeAnnotation::Bool));

        stack.pop();
        assert_eq!(stack.lookup("x"), Some(&TypeAnnotation::Int));
    }

    #[test]
    fn scope_stack_declare_visible_after_push() {
        let mut stack = ScopeStack::new(&HashMap::new());
        stack.push();
        stack.declare("y".to_owned(), TypeAnnotation::Float);
        assert_eq!(stack.lookup("y"), Some(&TypeAnnotation::Float));
    }

    #[test]
    fn scope_stack_declare_gone_after_pop() {
        let mut stack = ScopeStack::new(&HashMap::new());
        stack.push();
        stack.declare("y".to_owned(), TypeAnnotation::Float);
        stack.pop();
        assert_eq!(stack.lookup("y"), None);
    }

    #[test]
    fn scope_stack_pop_below_bottom_is_safe() {
        let mut top = HashMap::new();
        top.insert("k".to_owned(), TypeAnnotation::Dice);
        let mut stack = ScopeStack::new(&top);
        // Multiple extra pops must not panic; the bottom frame must survive.
        stack.pop();
        stack.pop();
        stack.pop();
        assert_eq!(stack.lookup("k"), Some(&TypeAnnotation::Dice));
    }

    #[test]
    fn scope_stack_multiple_nested_frames() {
        let mut stack = ScopeStack::new(&HashMap::new());

        stack.push();
        stack.declare("a".to_owned(), TypeAnnotation::Int);

        stack.push();
        stack.declare("b".to_owned(), TypeAnnotation::Str);

        // Both should be visible from the innermost frame.
        assert_eq!(stack.lookup("a"), Some(&TypeAnnotation::Int));
        assert_eq!(stack.lookup("b"), Some(&TypeAnnotation::Str));

        stack.pop();
        // "b" is gone; "a" is still visible.
        assert_eq!(stack.lookup("a"), Some(&TypeAnnotation::Int));
        assert_eq!(stack.lookup("b"), None);

        stack.pop();
        assert_eq!(stack.lookup("a"), None);
    }
}
