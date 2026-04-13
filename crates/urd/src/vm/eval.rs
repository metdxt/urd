//! Pure expression evaluator: converts AST expressions to [`RuntimeValue`]s.

use std::cell::Cell;
use std::collections::HashMap;

use crate::lexer::strings::{Interpolation, ParsedString, StringPart};
use crate::parser::ast::{AstContent, DeclKind, MatchPattern, Operator, UnaryOperator};
use crate::runtime::value::RuntimeValue;

use super::VmError;
use super::env::Environment;
use super::float_methods;
use super::int_methods;
use super::list_methods;
use super::map_methods;
use super::range_methods;
use super::str_methods;

// ─── Expression evaluator ─────────────────────────────────────────────────────

/// Evaluate an [`crate::parser::ast::Ast`] expression to a [`RuntimeValue`].
///
/// This function is pure with respect to the *environment* — it does not
/// mutate it.  Statement-level assignments are handled by
/// [`IrNodeKind::Assign`] nodes and never reach this function in normal
/// compiled code; the `Operator::Assign` arm in expression context simply
/// evaluates the rhs and returns it without storing.
///
/// # Errors
/// Returns [`VmError`] on type errors, undefined variables, or AST nodes that
/// are invalid in an expression context.
pub fn eval_expr(
    ast: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<RuntimeValue, VmError> {
    match ast.content() {
        // ── Literal / identifier ─────────────────────────────────────────────
        AstContent::Value(rv) => eval_runtime_value(rv, env),

        // ── Binary operation ─────────────────────────────────────────────────
        AstContent::BinOp { op, left, right } => eval_binop(op, left, right, env),

        // ── Unary operation ──────────────────────────────────────────────────
        AstContent::UnaryOp { op, expr } => {
            let val = eval_expr(expr, env)?;
            eval_unary(op, val)
        }

        // ── Expression list — evaluate each, return the last ─────────────────
        AstContent::ExprList(items) => {
            let mut last = RuntimeValue::Null;
            for item in items {
                last = eval_expr(item, env)?;
            }
            Ok(last)
        }

        // ── Function / method call ────────────────────────────────────────────
        AstContent::Call { func_path, params } => {
            // Evaluate all arguments eagerly (preserves side-effects, and we
            // need the values regardless of dispatch outcome).
            let args: Vec<RuntimeValue> = match params.content() {
                AstContent::ExprList(items) => items
                    .iter()
                    .map(|a| eval_expr(a, env))
                    .collect::<Result<_, _>>()?,
                _ => vec![eval_expr(params, env)?],
            };

            match func_path.content() {
                AstContent::Value(RuntimeValue::IdentPath(path)) if path.len() >= 2 => {
                    // Potentially a method call: receiver = path[0..n-1], method = path[n-1].
                    let method = path[path.len() - 1].as_str();
                    let receiver_path = path[..path.len() - 1].to_vec();

                    // Resolve the receiver value through the environment.
                    let receiver =
                        eval_runtime_value(&RuntimeValue::IdentPath(receiver_path), env)?;

                    match receiver {
                        RuntimeValue::List(list) => list_methods::dispatch(list.clone(), method, &args, env),
                        RuntimeValue::Roll(rolls) => {
                            let as_list: Vec<RuntimeValue> =
                                rolls.iter().map(|&n| RuntimeValue::Int(n)).collect();
                            list_methods::dispatch(crate::runtime::value::shared(as_list), method, &args, env)
                        }
                        RuntimeValue::Str(s) => str_methods::dispatch(s, method, &args),
                        RuntimeValue::Int(n) => int_methods::dispatch(n, method, &args),
                        RuntimeValue::Float(f) => float_methods::dispatch(f, method, &args),
                        RuntimeValue::Map(m) => map_methods::dispatch(m.clone(), method, &args),
                        RuntimeValue::Range {
                            start,
                            end,
                            inclusive,
                        } => range_methods::dispatch(start, end, inclusive, method, &args),
                        RuntimeValue::Extern(handle) => match method {
                            "to_string" => handle
                                .display()
                                .map(|s| RuntimeValue::Str(ParsedString::new_plain(&s)))
                                .map_err(VmError::TypeError),
                            "fields" => handle
                                .fields()
                                .map(|names| {
                                    RuntimeValue::List(crate::runtime::value::shared(
                                        names
                                            .into_iter()
                                            .map(|n| RuntimeValue::Str(ParsedString::new_plain(&n)))
                                            .collect::<Vec<_>>(),
                                    ))
                                })
                                .map_err(VmError::TypeError),
                            "cast" => {
                                if args.len() != 1 {
                                    return Err(VmError::TypeError(
                                        "cast() expects exactly 1 argument (target type name)"
                                            .into(),
                                    ));
                                }
                                let target = match &args[0] {
                                    RuntimeValue::Str(ps) => ps.to_string(),
                                    other => {
                                        return Err(VmError::TypeError(format!(
                                            "cast() argument must be a Str, got {other:?}"
                                        )));
                                    }
                                };
                                handle.cast(&target).map_err(VmError::TypeError)
                            }
                            "type_name" => handle
                                .type_name()
                                .map(|s| RuntimeValue::Str(ParsedString::new_plain(&s)))
                                .map_err(VmError::TypeError),
                            _ => Err(VmError::UnknownMethod(format!(
                                "method '{method}' is not defined on extern object"
                            ))),
                        },
                        _ => Err(VmError::UnknownMethod(format!(
                            "method '{}' is not defined on this value type",
                            method
                        ))),
                    }
                }
                _ => {
                    // Check if this is a call to a named function value in scope.
                    if let AstContent::Value(RuntimeValue::IdentPath(path)) = func_path.content()
                        && let Ok(RuntimeValue::Function { params, body }) =
                            eval_runtime_value(&RuntimeValue::IdentPath(path.clone()), env)
                    {
                        return exec_fn_body(&body, &params, &args, env);
                    }
                    let path_str = match func_path.content() {
                        AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
                        _ => "<unknown>".to_string(),
                    };
                    // Check if this is a struct constructor call.
                    if let AstContent::Value(RuntimeValue::IdentPath(path)) = func_path.content()
                        && path.len() == 1
                        && let Some(field_names) = env.get_struct_schema(&path[0])
                    {
                        if args.len() != field_names.len() {
                            return Err(VmError::TypeError(format!(
                                "struct '{}' expects {} field(s), got {}",
                                path[0],
                                field_names.len(),
                                args.len()
                            )));
                        }
                        let fields: std::collections::HashMap<String, RuntimeValue> =
                            field_names.iter().cloned().zip(args).collect();
                        return Ok(RuntimeValue::Struct {
                            name: path[0].clone(),
                            fields: crate::runtime::value::shared(fields),
                        });
                    }
                    // Global aggregate builtins: min(x), max(x), sum(x) where x is Roll or List
                    if let AstContent::Value(RuntimeValue::IdentPath(path)) = func_path.content()
                        && path.len() == 1
                        && args.len() == 1
                    {
                        let fn_name = path[0].as_str();
                        let arg_as_list: Option<Vec<RuntimeValue>> = match &args[0] {
                            RuntimeValue::Roll(rolls) => {
                                Some(rolls.iter().map(|&n| RuntimeValue::Int(n)).collect())
                            }
                            RuntimeValue::List(items) => Some(items.borrow().clone()),
                            _ => None,
                        };
                        if let Some(list) = arg_as_list {
                            match fn_name {
                                "min" | "max" | "sum" => {
                                    return list_methods::dispatch(crate::runtime::value::shared(list.clone()), fn_name, &[], env);
                                }
                                _ => {}
                            }
                        }
                    }
                    Err(VmError::UndefinedFunction(format!(
                        "function '{}' is not defined",
                        path_str
                    )))
                }
            }
        }

        AstContent::List(items) => {
            let mut elements = Vec::with_capacity(items.len());
            for item in items {
                elements.push(eval_expr(item, env)?);
            }
            Ok(RuntimeValue::List(crate::runtime::value::shared(elements)))
        }
        AstContent::Map(pairs) => {
            let mut map = HashMap::new();
            for (key_ast, val_ast) in pairs {
                // Map keys written as bare identifiers (e.g. `name:` in
                // `:{name: "x"}`) must be treated as literal string keys, NOT
                // as variable lookups.  Only fall through to full expression
                // evaluation when the key is something else (e.g. a computed
                // string expression).
                let key_str = match key_ast.content() {
                    AstContent::Value(RuntimeValue::IdentPath(p)) => {
                        p.last().cloned().unwrap_or_default()
                    }
                    _ => {
                        let key_val = eval_expr(key_ast, env)?;
                        match &key_val {
                            RuntimeValue::Str(ps) => ps.to_string(),
                            RuntimeValue::IdentPath(p) => p.last().cloned().unwrap_or_default(),
                            other => {
                                return Err(VmError::TypeError(format!(
                                    "map key must be Str or identifier, got {:?}",
                                    other
                                )));
                            }
                        }
                    }
                };
                let val = eval_expr(val_ast, env)?;
                map.insert(key_str, Box::new(val));
            }
            Ok(RuntimeValue::Map(crate::runtime::value::shared(map)))
        }

        // ── Invalid expression contexts ──────────────────────────────────────
        AstContent::Block(_) => Err(VmError::InvalidExpression(
            "Block cannot appear in expression context".to_string(),
        )),
        AstContent::If { .. } => Err(VmError::InvalidExpression(
            "If cannot appear in expression context".to_string(),
        )),
        AstContent::Declaration { .. } => Err(VmError::InvalidExpression(
            "Declaration cannot appear in expression context".to_string(),
        )),
        AstContent::LabeledBlock { .. } => Err(VmError::InvalidExpression(
            "LabeledBlock cannot appear in expression context".to_string(),
        )),
        AstContent::Dialogue { .. } => Err(VmError::InvalidExpression(
            "Dialogue cannot appear in expression context".to_string(),
        )),
        AstContent::Menu { .. } => Err(VmError::InvalidExpression(
            "Menu cannot appear in expression context".to_string(),
        )),
        AstContent::MenuOption { .. } => Err(VmError::InvalidExpression(
            "MenuOption cannot appear in expression context".to_string(),
        )),
        AstContent::Return { .. } => Err(VmError::InvalidExpression(
            "Return cannot appear in expression context".to_string(),
        )),
        AstContent::Jump { .. } => Err(VmError::InvalidExpression(
            "Jump cannot appear in expression context".to_string(),
        )),
        AstContent::LetCall { .. } => Err(VmError::InvalidExpression(
            "LetCall cannot appear in expression context".to_string(),
        )),
        AstContent::EnumDecl { .. } => Err(VmError::InvalidExpression(
            "EnumDecl cannot appear in expression context".to_string(),
        )),
        AstContent::Match { .. } => Err(VmError::InvalidExpression(
            "Match cannot appear in expression context".to_string(),
        )),
        AstContent::DecoratorDef { .. } => Err(VmError::InvalidExpression(
            "DecoratorDef cannot appear in expression context".to_string(),
        )),
        AstContent::FnDef { params, body, .. } => {
            // Both named and anonymous fn expressions produce a Function value.
            // Named fn *declarations* (statement position) are lowered to
            // IrNodeKind::DefineFunction by the compiler and never reach
            // eval_expr.  Anonymous fn expressions reach this arm directly.
            let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
            Ok(RuntimeValue::Function {
                params: param_names,
                body: body.clone(),
            })
        }
        AstContent::Subscript { object, key } => {
            let obj_val = eval_expr(object, env)?;
            let key_val = eval_expr(key, env)?;

            match obj_val {
                RuntimeValue::Map(map) => {
                    let key_str = match &key_val {
                        RuntimeValue::Str(ps) => ps.to_string(),
                        RuntimeValue::Int(i) => i.to_string(),
                        other => {
                            return Err(VmError::TypeError(format!(
                                "map key must be Str or Int, got {:?}",
                                other
                            )));
                        }
                    };
                    map.borrow().get(&key_str).map(|v| *v.clone()).ok_or_else(|| {
                        VmError::UndefinedVariable(format!("key '{}' not found in map", key_str))
                    })
                }
                RuntimeValue::List(list) => {
                    let idx = match key_val {
                        RuntimeValue::Int(i) => i,
                        other => {
                            return Err(VmError::TypeError(format!(
                                "list index must be Int, got {:?}",
                                other
                            )));
                        }
                    };
                    let len = list.borrow().len();
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
                    Ok(list.borrow()[actual].clone())
                }
                RuntimeValue::Roll(rolls) => {
                    let as_list: Vec<RuntimeValue> =
                        rolls.iter().map(|&n| RuntimeValue::Int(n)).collect();
                    let idx = match key_val {
                        RuntimeValue::Int(i) => i,
                        other => {
                            return Err(VmError::TypeError(format!(
                                "list index must be Int, got {:?}",
                                other
                            )));
                        }
                    };
                    let len = as_list.len();
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
                    Ok(as_list[actual].clone())
                }
                RuntimeValue::Struct { ref fields, .. } => {
                    let key_str = match &key_val {
                        RuntimeValue::Str(ps) => ps.to_string(),
                        other => {
                            return Err(VmError::TypeError(format!(
                                "struct field key must be a Str, got {:?}",
                                other
                            )));
                        }
                    };
                    fields.borrow().get(&key_str).cloned().ok_or_else(|| {
                        VmError::UndefinedVariable(format!(
                            "field '{}' not found on struct",
                            key_str
                        ))
                    })
                }
                RuntimeValue::Extern(handle) => {
                    let key_str = match &key_val {
                        RuntimeValue::Str(ps) => ps.to_string(),
                        other => {
                            return Err(VmError::TypeError(format!(
                                "extern field key must be a Str, got {other:?}"
                            )));
                        }
                    };
                    handle.get(&key_str).map_err(VmError::TypeError)
                }
                other => Err(VmError::TypeError(format!(
                    "subscript requires Map, List, Struct, or Extern, got {:?}",
                    other
                ))),
            }
        }
        AstContent::SubscriptAssign { .. } => Err(VmError::InvalidExpression(
            "SubscriptAssign must be used as a statement, not in expression context".to_string(),
        )),

        // Import is a top-level directive, not an evaluable expression.
        AstContent::Import { .. } => Err(VmError::InvalidExpression(
            "Import is not allowed in expression context".to_string(),
        )),
        AstContent::StructDecl { .. } => Err(VmError::InvalidExpression(
            "StructDecl cannot appear in expression context".to_string(),
        )),

        AstContent::ExternDeclaration { .. } => Err(VmError::InvalidExpression(
            "ExternDeclaration cannot appear in expression context".to_string(),
        )),
    }
}

/// Fully evaluate an [`AstContent::ExprList`] (or any other node) as a
/// `Vec<RuntimeValue>`, returning *all* elements rather than only the last.
///
/// Used when the IR stores an ExprList of speakers or dialogue lines and the
/// consumer needs every individual value.
pub fn eval_expr_list(
    ast: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<Vec<RuntimeValue>, VmError> {
    match ast.content() {
        AstContent::ExprList(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(eval_expr(item, env)?);
            }
            Ok(out)
        }
        _ => Ok(vec![eval_expr(ast, env)?]),
    }
}

/// Evaluates one speaker AST node.
///
/// Identical to [`eval_expr`] but with one extra rule: if evaluation fails
/// with [`VmError::UndefinedVariable`] *and* the node is a bare
/// `Value(IdentPath([name]))`, the identifier name is returned as a plain
/// `RuntimeValue::Str` instead of propagating the error.
///
/// This lets `alice: "Hello"` work without a prior `const alice = ...`
/// declaration — the speaker becomes the string `"alice"`.  When a variable
/// *is* defined (e.g. `const alice = :{ name: "Alice", portrait: "…" }`),
/// that richer value is returned instead.
pub(super) fn eval_speaker(
    ast: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<RuntimeValue, VmError> {
    match eval_expr(ast, env) {
        Ok(v) => Ok(v),
        Err(VmError::UndefinedVariable(_)) => {
            // Only fall back for a bare single-segment identifier.
            if let AstContent::Value(RuntimeValue::IdentPath(path)) = ast.content() {
                Ok(RuntimeValue::Str(ParsedString::new_plain(&path.join("."))))
            } else {
                // Non-ident expressions (calls, bin-ops …) should still error.
                eval_expr(ast, env)
            }
        }
        Err(e) => Err(e),
    }
}

/// Like [`eval_expr_list`] but evaluates every element through [`eval_speaker`].
pub(super) fn eval_speakers_list(
    ast: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<Vec<RuntimeValue>, VmError> {
    match ast.content() {
        AstContent::ExprList(items) => items.iter().map(|item| eval_speaker(item, env)).collect(),
        _ => Ok(vec![eval_speaker(ast, env)?]),
    }
}

// ── Internal expression helpers ───────────────────────────────────────────────

/// Evaluate a [`RuntimeValue`] that may be an [`RuntimeValue::IdentPath`]
/// (variable or enum-variant lookup) or a [`RuntimeValue::Str`] (which may
/// contain interpolation segments).
pub(super) fn eval_runtime_value(
    rv: &RuntimeValue,
    env: &Environment,
) -> Result<RuntimeValue, VmError> {
    match rv {
        RuntimeValue::IdentPath(path) => match path.len() {
            0 => Err(VmError::UndefinedVariable("<empty path>".to_string())),
            1 => {
                // Single-segment: try variable lookup first, then search all
                // registered enums for a matching variant name.
                match env.get(&path[0]) {
                    Ok(v) => Ok(v),
                    Err(VmError::UndefinedVariable(_)) => {
                        // Try treating as a bare enum variant (e.g. `North`
                        // when there is only one enum with that variant).
                        for variants in env.enums().values() {
                            if variants.iter().any(|v| v == &path[0]) {
                                return Ok(RuntimeValue::Str(ParsedString::new_plain(&path[0])));
                            }
                        }
                        Err(VmError::UndefinedVariable(path[0].clone()))
                    }
                    Err(e) => Err(e),
                }
            }
            2 => {
                // Two-segment: try `EnumName.Variant` first, then try
                // module-namespaced variable "alias::var_name", then fall back
                // to plain variable lookup of the first segment.
                if let Ok(variant) = env.get_enum_variant(&path[0], &path[1]) {
                    return Ok(variant);
                }
                // Try module-namespaced variable: `alias.var` → `alias::var`
                let namespaced = crate::ir::namespace(&path[0], &path[1]);
                if let Ok(v) = env.get(&namespaced) {
                    return Ok(v);
                }
                // Try struct field access: path = ["struct_var", "field"].
                if let Ok(RuntimeValue::Struct { ref fields, .. }) = env.get(&path[0])
                    && let Some(val) = fields.borrow().get(&path[1])
                {
                    return Ok(val.clone());
                }
                // Try extern object field access: path = ["extern_var", "field"].
                if let Ok(RuntimeValue::Extern(ref handle)) = env.get(&path[0]) {
                    return handle.get(&path[1]).map_err(VmError::TypeError);
                }
                Err(VmError::UndefinedVariable(format!(
                    "{}.{}",
                    path[0], path[1]
                )))
            }
            _ => {
                // Three-segment path: `alias.EnumName.Variant`
                // During IrGraph::merge, enum names are namespaced as
                // "alias::EnumName", so we must build that key to resolve the
                // variant (e.g. `chars.Faction.Rebel` → enum "chars::Faction",
                // variant "Rebel").
                let namespaced_enum = crate::ir::namespace(&path[0], &path[1]);
                env.get_enum_variant(&namespaced_enum, &path[2])
            }
        },
        RuntimeValue::Str(ps) => {
            // Resolve any interpolation placeholders.
            Ok(RuntimeValue::Str(interpolate_string(ps, env)))
        }
        RuntimeValue::Dice(count, sides) => {
            if *sides == 0 {
                return Err(VmError::TypeError("dice: sides must be >= 1".into()));
            }
            env.roll_dice_individual(*count as u32, *sides as u32)
                .map(RuntimeValue::Roll)
        }
        other => Ok(other.clone()),
    }
}

/// Resolve a dotted interpolation path (e.g. `"inv.gold"`) against the
/// environment, using the same rules as inline string interpolation:
///
/// - 1 segment  → direct env lookup
/// - 2 segments → enum-variant lookup, then module-namespaced key (`a::b`)
/// - 3+ segments → module-namespaced key for first two segments (`a::b`)
///
/// This is the single authoritative resolution routine shared by
/// [`interpolate_string`] and `vm::collect_fluent_vars` / `vm::build_choice_event`.
pub(super) fn resolve_interp_path(path: &str, env: &Environment) -> Result<RuntimeValue, VmError> {
    let segments: Vec<&str> = path.split('.').collect();
    match segments.len() {
        0 => Err(VmError::UndefinedVariable("<empty>".to_string())),
        1 => env.get(segments[0]),
        2 => env
            .get_enum_variant(segments[0], segments[1])
            .or_else(|_| env.get(&crate::ir::namespace(segments[0], segments[1])))
            .or_else(|_| {
                // Struct field access: `struct_var.field`
                if let Ok(RuntimeValue::Struct { ref fields, .. }) = env.get(segments[0])
                    && let Some(val) = fields.borrow().get(segments[1])
                {
                    return Ok(val.clone());
                }
                // Extern object field access: `extern_var.field`
                if let Ok(RuntimeValue::Extern(ref handle)) = env.get(segments[0]) {
                    return handle.get(segments[1]).map_err(VmError::TypeError);
                }
                Err(VmError::UndefinedVariable(format!(
                    "{}.{}",
                    segments[0], segments[1]
                )))
            }),
        _ => env.get(&crate::ir::namespace(segments[0], segments[1])),
    }
}

/// Resolve all [`StringPart::Interpolation`] segments in `ps` by looking up
/// their variable paths in `env`.
///
/// Unresolvable paths emit a [`log::warn!`] and are left as their original
/// `{path}` placeholder text.
pub(super) fn interpolate_string(ps: &ParsedString, env: &Environment) -> ParsedString {
    let mut new_parts: Vec<StringPart> = Vec::new();

    for part in ps.parts() {
        match part {
            StringPart::Interpolation(interp) => {
                match resolve_interp_path(&interp.path, env) {
                    Ok(val) => {
                        let s = format_runtime_value(&val, interp.format.as_deref());
                        new_parts.push(StringPart::Literal(s));
                    }
                    Err(_) => {
                        log::warn!(
                            "string interpolation: undefined variable path '{}'",
                            interp.path
                        );
                        // Preserve the placeholder as-is.
                        new_parts.push(StringPart::Interpolation(Interpolation {
                            path: interp.path.clone(),
                            format: interp.format.clone(),
                        }));
                    }
                }
            }
            other => new_parts.push(other.clone()),
        }
    }

    ParsedString::new_from_parts(new_parts)
}

/// Format a [`RuntimeValue`] as a string, applying a simple format specifier
/// when provided.
///
/// Supported specifiers:
/// - `"02"` → zero-pad integer to at-least-2 digits
/// - `".2"` → float to 2 decimal places
#[allow(clippy::collapsible_if)]
pub(super) fn format_runtime_value(val: &RuntimeValue, format: Option<&str>) -> String {
    let mut visited = std::collections::HashSet::new();
    format_runtime_value_inner(val, format, &mut visited)
}

fn format_runtime_value_inner(
    val: &RuntimeValue,
    format: Option<&str>,
    visited: &mut std::collections::HashSet<usize>,
) -> String {
    match (val, format) {
        (RuntimeValue::Int(i), Some(fmt)) => {
            if let Some(width_str) = fmt.strip_prefix('0')
                && let Ok(width) = width_str.parse::<usize>()
            {
                let width = width.min(64);
                return format!("{:0>width$}", i, width = width);
            }
            i.to_string()
        }
        (RuntimeValue::Float(f), Some(fmt)) => {
            if let Some(prec_str) = fmt.strip_prefix('.')
                && let Ok(prec) = prec_str.parse::<usize>()
            {
                let prec = prec.min(64);
                return format!("{:.prec$}", f, prec = prec);
            }
            f.to_string()
        }
        (rv, _) => match rv {
            RuntimeValue::Null => "null".to_string(),
            RuntimeValue::Bool(b) => b.to_string(),
            RuntimeValue::Int(i) => i.to_string(),
            RuntimeValue::Float(f) => f.to_string(),
            RuntimeValue::Str(ps) => ps.to_string(),
            RuntimeValue::Dice(count, sides) => format!("{}d{}", count, sides),
            RuntimeValue::Roll(rolls) => {
                let parts = rolls
                    .iter()
                    .map(|r| r.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{parts}]")
            }
            RuntimeValue::IdentPath(path) => path.join("."),
            RuntimeValue::Map(m) => format!("map({})", m.borrow().len()),
            RuntimeValue::List(items) => {
                let ptr = items.as_ptr();
                if visited.contains(&ptr) {
                    return "[...]".to_string();
                }
                visited.insert(ptr);
                let parts: Vec<String> = items
                    .borrow()
                    .iter()
                    .map(|v| format_runtime_value_inner(v, None, visited))
                    .collect();
                visited.remove(&ptr);
                format!("[{}]", parts.join(", "))
            }
            RuntimeValue::Range {
                start,
                end,
                inclusive,
            } => {
                if *inclusive {
                    format!("{start}..={end}")
                } else {
                    format!("{start}..{end}")
                }
            }
            RuntimeValue::ScriptDecorator { .. } => "<decorator>".to_string(),
            RuntimeValue::Function { params, .. } => {
                format!("fn({})", params.join(", "))
            }
            RuntimeValue::Struct { name, fields } => {
                format!("{}({})", name, fields.borrow().len())
            }
            RuntimeValue::Extern(handle) => handle
                .display()
                .unwrap_or_else(|e| format!("<extern: {e}>")),
        },
    }
}

/// Evaluate a binary operation node.
pub(super) fn eval_binop(
    op: &Operator,
    left: &crate::parser::ast::Ast,
    right: &crate::parser::ast::Ast,
    env: &Environment,
) -> Result<RuntimeValue, VmError> {
    // Short-circuit logical operators before evaluating both sides.
    match op {
        Operator::And => {
            let lv = eval_expr(left, env)?;
            if !is_truthy(&lv) {
                return Ok(RuntimeValue::Bool(false));
            }
            let rv = eval_expr(right, env)?;
            return Ok(RuntimeValue::Bool(is_truthy(&rv)));
        }
        Operator::Or => {
            let lv = eval_expr(left, env)?;
            if is_truthy(&lv) {
                return Ok(RuntimeValue::Bool(true));
            }
            let rv = eval_expr(right, env)?;
            return Ok(RuntimeValue::Bool(is_truthy(&rv)));
        }
        // Assignment in expression context: evaluate rhs, return it
        // (mutation is handled by IrNodeKind::Assign; this path is a no-op store).
        Operator::Assign => {
            return eval_expr(right, env);
        }
        _ => {}
    }

    let lv = eval_expr(left, env)?;
    let rv_val = eval_expr(right, env)?;

    match op {
        Operator::Plus => numeric_binop(
            lv,
            rv_val,
            |a, b| {
                a.checked_add(b)
                    .ok_or_else(|| VmError::TypeError("integer overflow in addition".into()))
            },
            |a, b| a + b,
        ),
        Operator::Minus => numeric_binop(
            lv,
            rv_val,
            |a, b| {
                a.checked_sub(b)
                    .ok_or_else(|| VmError::TypeError("integer overflow in subtraction".into()))
            },
            |a, b| a - b,
        ),
        Operator::Multiply => numeric_binop(
            lv,
            rv_val,
            |a, b| {
                a.checked_mul(b)
                    .ok_or_else(|| VmError::TypeError("integer overflow in multiplication".into()))
            },
            |a, b| a * b,
        ),
        Operator::Divide => numeric_div(lv, rv_val),
        Operator::DoubleSlash => numeric_floordiv(lv, rv_val),
        Operator::Percent => numeric_mod(lv, rv_val),
        Operator::Equals => Ok(RuntimeValue::Bool(values_equal(&lv, &rv_val))),
        Operator::NotEquals => Ok(RuntimeValue::Bool(!values_equal(&lv, &rv_val))),
        Operator::GreaterThan => numeric_cmp(lv, rv_val, |a, b| a > b, |a, b| a > b),
        Operator::LessThan => numeric_cmp(lv, rv_val, |a, b| a < b, |a, b| a < b),
        Operator::GreaterThanOrEquals => numeric_cmp(lv, rv_val, |a, b| a >= b, |a, b| a >= b),
        Operator::LessThanOrEquals => numeric_cmp(lv, rv_val, |a, b| a <= b, |a, b| a <= b),
        Operator::BitwiseAnd => int_bitop(lv, rv_val, |a, b| a & b),
        Operator::BitwiseOr => int_bitop(lv, rv_val, |a, b| a | b),
        Operator::BitwiseXor => int_bitop(lv, rv_val, |a, b| a ^ b),
        Operator::LeftShift => safe_int_shiftop(lv, rv_val, ShiftDirection::Left),
        Operator::RightShift => safe_int_shiftop(lv, rv_val, ShiftDirection::Right),
        Operator::RangeExclusive => match (lv, rv_val) {
            (RuntimeValue::Int(start), RuntimeValue::Int(end)) => Ok(RuntimeValue::Range {
                start,
                end,
                inclusive: false,
            }),
            (l, r) => Err(VmError::TypeError(format!(
                "range operator `..` requires two Int values, got {:?} and {:?}",
                l, r
            ))),
        },
        Operator::RangeInclusive => match (lv, rv_val) {
            (RuntimeValue::Int(start), RuntimeValue::Int(end)) => Ok(RuntimeValue::Range {
                start,
                end,
                inclusive: true,
            }),
            (l, r) => Err(VmError::TypeError(format!(
                "range operator `..=` requires two Int values, got {:?} and {:?}",
                l, r
            ))),
        },
        Operator::In => match rv_val {
            RuntimeValue::Range {
                start,
                end,
                inclusive,
            } => match lv {
                RuntimeValue::Int(n) => {
                    let contains = if inclusive {
                        n >= start && n <= end
                    } else {
                        n >= start && n < end
                    };
                    Ok(RuntimeValue::Bool(contains))
                }
                other => Err(VmError::TypeError(format!(
                    "`in` requires an Int on the left-hand side for Range, got {:?}",
                    other
                ))),
            },
            RuntimeValue::List(items) => {
                let found = items.borrow().iter().any(|el| values_equal(&lv, el));
                Ok(RuntimeValue::Bool(found))
            }
            RuntimeValue::Map(map) => match lv {
                RuntimeValue::Str(ref s) => {
                    let key = s.to_string();
                    Ok(RuntimeValue::Bool(map.borrow().contains_key(&key)))
                }
                other => Err(VmError::TypeError(format!(
                    "`in` requires a Str on the left-hand side for Map, got {:?}",
                    other
                ))),
            },
            RuntimeValue::Str(ref rhs_s) => match lv {
                RuntimeValue::Str(ref lhs_s) => {
                    let found = rhs_s.to_string().contains(&lhs_s.to_string());
                    Ok(RuntimeValue::Bool(found))
                }
                other => Err(VmError::TypeError(format!(
                    "`in` requires a Str on the left-hand side for Str, got {:?}",
                    other
                ))),
            },
            other => Err(VmError::TypeError(format!(
                "`in` requires a Range, List, Map, or Str on the right-hand side, got {:?}",
                other
            ))),
        },
        // Handled above via early return.
        Operator::And | Operator::Or | Operator::Assign => unreachable!(),
    }
}

/// Evaluate a unary operation.
pub(super) fn eval_unary(op: &UnaryOperator, val: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match op {
        UnaryOperator::Not => Ok(RuntimeValue::Bool(!is_truthy(&val))),
        UnaryOperator::Negate => match val {
            RuntimeValue::Int(i) => i.checked_neg().map(RuntimeValue::Int).ok_or_else(|| {
                VmError::TypeError(
                    "integer overflow: cannot negate the minimum integer value".into(),
                )
            }),
            RuntimeValue::Float(f) => Ok(RuntimeValue::Float(-f)),
            other => Err(VmError::TypeError(format!(
                "cannot negate non-numeric value {:?}",
                other
            ))),
        },
        UnaryOperator::BitwiseNot => match val {
            RuntimeValue::Int(i) => Ok(RuntimeValue::Int(!i)),
            other => Err(VmError::TypeError(format!(
                "bitwise NOT requires Int, got {:?}",
                other
            ))),
        },
    }
}

// ── Arithmetic helpers ────────────────────────────────────────────────────────

/// Compute the checked sum of a roll's individual dice results.
///
/// Returns `Err(VmError::TypeError)` on integer overflow instead of panicking
/// or silently wrapping/saturating.
pub(super) fn checked_sum_rolls(rolls: &[i64]) -> Result<i64, VmError> {
    rolls.iter().try_fold(0i64, |acc, &x| {
        acc.checked_add(x)
            .ok_or_else(|| VmError::TypeError("dice roll sum overflows i64".into()))
    })
}

pub(super) fn to_float(v: &RuntimeValue) -> Option<f64> {
    match v {
        RuntimeValue::Float(f) => Some(*f),
        RuntimeValue::Int(i) => Some(*i as f64),
        RuntimeValue::Roll(rolls) => checked_sum_rolls(rolls).ok().map(|s| s as f64),
        _ => None,
    }
}

pub(super) fn numeric_binop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    int_op: impl Fn(i64, i64) -> Result<i64, VmError>,
    float_op: impl Fn(f64, f64) -> f64,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Int(int_op(*a, *b)?)),
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
            Ok(RuntimeValue::Float(float_op(*a, *b)))
        }
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            Ok(RuntimeValue::Float(float_op(a, b)))
        }
    }
}

#[allow(dead_code)]
pub(super) fn numeric_int_binop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    op: impl Fn(i64, i64) -> i64,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Int(op(*a, *b))),
        _ => Err(VmError::TypeError(format!(
            "integer-only operation requires two Int values, got {:?} and {:?}",
            lv, rv
        ))),
    }
}

pub(super) fn numeric_mod(lv: RuntimeValue, rv: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
            if *b == 0 {
                return Err(VmError::TypeError("integer modulo by zero".into()));
            }
            if *a == i64::MIN && *b == -1 {
                return Err(VmError::TypeError(
                    "integer modulo overflow (i64::MIN % -1)".into(),
                ));
            }
            Ok(RuntimeValue::Int(a % b))
        }
        _ => Err(VmError::TypeError(format!(
            "integer-only operation requires two Int values, got {:?} and {:?}",
            lv, rv
        ))),
    }
}

pub(super) fn numeric_div(lv: RuntimeValue, rv: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
            if *b == 0 {
                return Err(VmError::TypeError("integer division by zero".to_string()));
            }
            if *a == i64::MIN && *b == -1 {
                return Err(VmError::TypeError(
                    "integer overflow: i64::MIN / -1 is not representable".into(),
                ));
            }
            Ok(RuntimeValue::Int(a / b))
        }
        (RuntimeValue::Float(_), RuntimeValue::Float(b)) if *b == 0.0 => {
            Err(VmError::TypeError("float division by zero".into()))
        }
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Ok(RuntimeValue::Float(a / b)),
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            if b == 0.0 {
                return Err(VmError::TypeError("float division by zero".into()));
            }
            Ok(RuntimeValue::Float(a / b))
        }
    }
}

fn int_floor_div(a: i64, b: i64) -> i64 {
    let d = a / b;
    let r = a % b;
    if (r != 0) && ((r < 0) != (b < 0)) {
        d - 1
    } else {
        d
    }
}

pub(super) fn numeric_floordiv(
    lv: RuntimeValue,
    rv: RuntimeValue,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
            if *b == 0 {
                return Err(VmError::TypeError("floor division by zero".to_string()));
            }
            if *a == i64::MIN && *b == -1 {
                return Err(VmError::TypeError(
                    "integer overflow: i64::MIN // -1 is not representable".into(),
                ));
            }
            Ok(RuntimeValue::Int(int_floor_div(*a, *b)))
        }
        (RuntimeValue::Float(_), RuntimeValue::Float(b)) if *b == 0.0 => {
            Err(VmError::TypeError("float floor division by zero".into()))
        }
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
            Ok(RuntimeValue::Float((a / b).floor()))
        }
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            if b == 0.0 {
                return Err(VmError::TypeError("float floor division by zero".into()));
            }
            Ok(RuntimeValue::Float((a / b).floor()))
        }
    }
}

pub(super) fn numeric_cmp(
    lv: RuntimeValue,
    rv: RuntimeValue,
    int_cmp: impl Fn(i64, i64) -> bool,
    float_cmp: impl Fn(f64, f64) -> bool,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Bool(int_cmp(*a, *b))),
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
            Ok(RuntimeValue::Bool(float_cmp(*a, *b)))
        }
        (RuntimeValue::Int(a), RuntimeValue::Float(b)) => {
            if b.fract() == 0.0 && *b >= i64::MIN as f64 && *b < (i64::MAX as f64) {
                Ok(RuntimeValue::Bool(int_cmp(*a, *b as i64)))
            } else {
                Ok(RuntimeValue::Bool(float_cmp(*a as f64, *b)))
            }
        }
        (RuntimeValue::Float(a), RuntimeValue::Int(b)) => {
            if a.fract() == 0.0 && *a >= i64::MIN as f64 && *a < (i64::MAX as f64) {
                Ok(RuntimeValue::Bool(int_cmp(*a as i64, *b)))
            } else {
                Ok(RuntimeValue::Bool(float_cmp(*a, *b as f64)))
            }
        }
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            Ok(RuntimeValue::Bool(float_cmp(a, b)))
        }
    }
}

pub(super) fn int_bitop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    op: impl Fn(i64, i64) -> i64,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Int(op(*a, *b))),
        _ => Err(VmError::TypeError(format!(
            "bitwise operation requires two Int values, got {:?} and {:?}",
            lv, rv
        ))),
    }
}

#[derive(Copy, Clone, Debug)]
pub(super) enum ShiftDirection {
    Left,
    Right,
}

pub(super) fn safe_int_shiftop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    direction: ShiftDirection,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(shift)) => {
            if *shift < 0 || *shift > 63 {
                return Err(VmError::TypeError(format!(
                    "invalid shift count {shift}; expected 0..=63"
                )));
            }

            let shift_u32 = *shift as u32;
            let out = match direction {
                ShiftDirection::Left => {
                    let result = *a << shift_u32;
                    // Verify no bits were lost: shifting back must recover the original value
                    if (result >> shift_u32) != *a {
                        return Err(VmError::TypeError(format!(
                            "left shift overflow: {a} << {shift}"
                        )));
                    }
                    result
                }
                ShiftDirection::Right => *a >> shift_u32,
            };
            Ok(RuntimeValue::Int(out))
        }
        _ => Err(VmError::TypeError(format!(
            "bitwise shift requires two Int values, got {:?} and {:?}",
            lv, rv
        ))),
    }
}

/// Returns `true` if `v` is truthy.
///
/// - `Null` → `false`
/// - `Bool(b)` → `b`
/// - `Int(0)` → `false`; any other `Int` → `true`
/// - `Float(0.0)` → `false`; any other `Float` → `true`
/// - Everything else → `true`
pub(super) fn is_truthy(v: &RuntimeValue) -> bool {
    match v {
        RuntimeValue::Null => false,
        RuntimeValue::Bool(b) => *b,
        RuntimeValue::Int(i) => *i != 0,
        // NaN is explicitly falsy — a NaN comparison should never pass a guard.
        RuntimeValue::Float(f) => *f != 0.0 && !f.is_nan(),
        // An empty list is falsy; a non-empty list is truthy.
        RuntimeValue::List(items) => !items.borrow().is_empty(),
        RuntimeValue::Roll(rolls) => !rolls.is_empty(),
        // A non-empty range is truthy.
        RuntimeValue::Range {
            start,
            end,
            inclusive,
        } => {
            if *inclusive {
                end >= start
            } else {
                end > start
            }
        }
        // An empty string is falsy; a non-empty string is truthy.
        RuntimeValue::Str(s) => !s.to_string().is_empty(),
        // An empty map is falsy; a non-empty map is truthy.
        RuntimeValue::Map(m) => !m.borrow().is_empty(),
        // A struct with no fields is falsy; one with fields is truthy.
        RuntimeValue::Struct { fields, .. } => !fields.borrow().is_empty(),
        RuntimeValue::Extern(_) => true,
        _ => true,
    }
}

/// Returns `true` if two [`RuntimeValue`]s are structurally equal.
pub(super) fn values_equal(a: &RuntimeValue, b: &RuntimeValue) -> bool {
    let mut visited = std::collections::HashSet::new();
    values_equal_inner(a, b, &mut visited)
}

fn values_equal_inner(
    a: &RuntimeValue,
    b: &RuntimeValue,
    visited: &mut std::collections::HashSet<(usize, usize)>,
) -> bool {
    match (a, b) {
        (RuntimeValue::Null, RuntimeValue::Null) => true,
        (RuntimeValue::Bool(x), RuntimeValue::Bool(y)) => x == y,
        (RuntimeValue::Int(x), RuntimeValue::Int(y)) => x == y,
        (RuntimeValue::Float(x), RuntimeValue::Float(y)) => x == y,
        // Cross-type numeric equality — avoid i64→f64 bit loss above 2^53.
        // Use `< (i64::MAX as f64)` rather than `<=` because i64::MAX = 2^63-1 rounds
        // UP to 2^63 when cast to f64; accepting the rounded value would produce a
        // false equality when casting it back to i64 (saturating cast → i64::MAX).
        (RuntimeValue::Int(x), RuntimeValue::Float(y)) => {
            if y.fract() != 0.0 {
                false
            } else if *y >= i64::MIN as f64 && *y < (i64::MAX as f64) {
                (*y as i64) == *x
            } else {
                false
            }
        }
        (RuntimeValue::Float(x), RuntimeValue::Int(y)) => {
            if x.fract() != 0.0 {
                false
            } else if *x >= i64::MIN as f64 && *x < (i64::MAX as f64) {
                (*x as i64) == *y
            } else {
                false
            }
        }
        (RuntimeValue::Str(x), RuntimeValue::Str(y)) => x.to_string() == y.to_string(),
        (RuntimeValue::List(xs), RuntimeValue::List(ys)) => {
            let ptr_pair = (xs.as_ptr(), ys.as_ptr());
            if visited.contains(&ptr_pair) {
                return true; // Assume equal to break cycle
            }
            visited.insert(ptr_pair);
            let eq = xs.borrow().len() == ys.borrow().len() && xs.borrow().iter().zip(ys.borrow().iter()).all(|(a, b)| values_equal_inner(a, b, visited));
            visited.remove(&ptr_pair);
            eq
        }
        (RuntimeValue::Map(xs), RuntimeValue::Map(ys)) => {
            let ptr_pair = (xs.as_ptr(), ys.as_ptr());
            if visited.contains(&ptr_pair) {
                return true;
            }
            visited.insert(ptr_pair);
            let eq = xs.borrow().len() == ys.borrow().len()
                && xs
                    .borrow()
                    .iter()
                    .all(|(k, v)| ys.borrow().get(k).is_some_and(|yv| values_equal_inner(v, yv, visited)));
            visited.remove(&ptr_pair);
            eq
        }
        (
            RuntimeValue::Struct {
                name: n1,
                fields: f1,
            },
            RuntimeValue::Struct {
                name: n2,
                fields: f2,
            },
        ) => {
            let ptr_pair = (f1.as_ptr(), f2.as_ptr());
            if visited.contains(&ptr_pair) {
                return true;
            }
            visited.insert(ptr_pair);
            let eq = n1 == n2
                && f1.borrow().len() == f2.borrow().len()
                && f1
                    .borrow()
                    .iter()
                    .all(|(k, v)| f2.borrow().get(k).is_some_and(|fv| values_equal_inner(v, fv, visited)));
            visited.remove(&ptr_pair);
            eq
        }
        (
            RuntimeValue::Range {
                start: s1,
                end: e1,
                inclusive: i1,
            },
            RuntimeValue::Range {
                start: s2,
                end: e2,
                inclusive: i2,
            },
        ) => s1 == s2 && e1 == e2 && i1 == i2,
        (RuntimeValue::Roll(a), RuntimeValue::Roll(b)) => a == b,
        (RuntimeValue::Extern(a), RuntimeValue::Extern(b)) => a == b,
        // Function and ScriptDecorator have no meaningful value equality — two distinct
        // function objects are never considered the same even if their bodies are
        // identical.  Any type not listed above (including future variants) defaults
        // to inequality.
        _ => false,
    }
}

// ── Pure function execution ───────────────────────────────────────────────────

/// Internal result type for function body execution.
///
/// Separates normal last-expression completion from an explicit `return`
/// statement so that early-exit propagation works correctly across nested
/// `if`/`block` calls without relying on panics or side-channels.
enum FnExecResult {
    /// Normal fall-through — value is the last evaluated expression.
    Normal(RuntimeValue),
    /// Explicit `return expr;` was encountered; propagate upward immediately.
    Return(RuntimeValue),
}

// ─── Recursion depth guard ────────────────────────────────────────────────────

thread_local! {
    /// Tracks the current recursion depth for pure function bodies
    /// ([`exec_fn_body`]).  Each entry increments the counter; the RAII
    /// [`FnDepthGuard`] decrements it on drop so that early `?`-returns and
    /// panics cannot leak a stale count.
    static FN_CALL_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// Maximum recursive call depth for pure function bodies.
///
/// Matches the VM's default `max_call_depth` (256).
const MAX_FN_CALL_DEPTH: usize = 256;

/// RAII guard that increments [`FN_CALL_DEPTH`] on creation and decrements it
/// on drop, ensuring correct depth tracking even when errors short-circuit
/// execution via `?`.
struct FnDepthGuard;

impl FnDepthGuard {
    /// Increment depth and check the recursion limit.
    ///
    /// # Errors
    ///
    /// Returns [`VmError::StackOverflow`] when the limit is reached.
    fn enter() -> Result<Self, VmError> {
        FN_CALL_DEPTH.with(|d| {
            let current = d.get();
            if current >= MAX_FN_CALL_DEPTH {
                return Err(VmError::StackOverflow(MAX_FN_CALL_DEPTH));
            }
            d.set(current + 1);
            Ok(FnDepthGuard)
        })
    }
}

impl Drop for FnDepthGuard {
    fn drop(&mut self) {
        FN_CALL_DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}

// ─── Function body execution ──────────────────────────────────────────────────
/// Execute a pure function body with the given arguments.
///
/// Creates a **fully isolated** [`Environment`] containing only the bound
/// parameters — no access to outer-scope variables.  This is the purity
/// guarantee: function bodies cannot read or write ambient state.
///
/// Both explicit `return expr` and the implicit last-expression return are
/// supported.
///
/// # Recursion limit
///
/// Each call increments a thread-local depth counter (via [`FnDepthGuard`]).
/// When the counter reaches [`MAX_FN_CALL_DEPTH`] (256), the call fails with
/// [`VmError::StackOverflow`].  The counter is decremented automatically when
/// the guard drops, so early returns and errors are handled correctly.
///
/// # Errors
///
/// - [`VmError::StackOverflow`] when the recursion depth limit is exceeded.
/// - [`VmError::TypeError`] on arity mismatch.
/// - Any [`VmError`] produced by evaluating the body.
pub(super) fn exec_fn_body(
    body: &crate::parser::ast::Ast,
    params: &[String],
    args: &[RuntimeValue],
    caller_env: &Environment,
) -> Result<RuntimeValue, VmError> {
    let _guard = FnDepthGuard::enter()?;

    if params.len() != args.len() {
        return Err(VmError::TypeError(format!(
            "function expects {} argument(s), got {}",
            params.len(),
            args.len()
        )));
    }

    // Isolated environment — pure: no globals, no externs, no outer scope.
    // Propagate the caller's dice roller so that dice expressions inside
    // function bodies use the VM's configured roller instead of the default.
    let mut fn_env = Environment::new();
    if let Some(roller) = caller_env.dice_roller() {
        fn_env.set_dice_roller_arc(roller);
    }
    for (name, val) in params.iter().zip(args.iter()) {
        fn_env.set(name, val.clone(), &DeclKind::Variable)?;
    }

    match exec_fn_block(body, &mut fn_env)? {
        FnExecResult::Normal(v) | FnExecResult::Return(v) => Ok(v),
    }
}

/// Execute a block AST node in a function context, propagating `return`.
fn exec_fn_block(
    block: &crate::parser::ast::Ast,
    env: &mut Environment,
) -> Result<FnExecResult, VmError> {
    match block.content() {
        AstContent::Block(stmts) => {
            let mut last = RuntimeValue::Null;
            for stmt in stmts {
                match exec_fn_stmt(stmt, env)? {
                    FnExecResult::Return(v) => return Ok(FnExecResult::Return(v)),
                    FnExecResult::Normal(v) => last = v,
                }
            }
            Ok(FnExecResult::Normal(last))
        }
        // Bare expression body — treat as a single implicit-return expression.
        _ => eval_expr(block, env).map(FnExecResult::Normal),
    }
}

/// Execute a single statement within a function body.
///
/// Handles:
/// - `return` (explicit early exit)
/// - `let` / `const` declarations
/// - `if` / `else` branches
/// - `x = expr` assignments
/// - nested blocks
/// - nested `fn` definitions (inner closures stored in scope)
/// - bare expressions (value becomes the block's last value)
///
/// Deliberate **omissions** (invalid in pure function bodies):
/// `dialogue`, `menu`, `jump`, `import` — side-effectful or
/// control-flow-across-labels operations that violate purity.  These
/// fall through to `eval_expr` which returns an appropriate error.
fn exec_fn_stmt(
    stmt: &crate::parser::ast::Ast,
    env: &mut Environment,
) -> Result<FnExecResult, VmError> {
    match stmt.content() {
        // ── Explicit return ───────────────────────────────────────────────
        AstContent::Return { value } => {
            let val = match value {
                Some(expr) => eval_expr(expr, env)?,
                None => RuntimeValue::Null,
            };
            Ok(FnExecResult::Return(val))
        }

        // ── Nested block ──────────────────────────────────────────────────
        AstContent::Block(_) => exec_fn_block(stmt, env),

        // ── If / else ────────────────────────────────────────────────────
        AstContent::If {
            condition,
            then_block,
            else_block,
        } => {
            let cond = eval_expr(condition, env)?;
            if is_truthy(&cond) {
                exec_fn_block(then_block, env)
            } else if let Some(else_branch) = else_block {
                exec_fn_block(else_branch, env)
            } else {
                Ok(FnExecResult::Normal(RuntimeValue::Null))
            }
        }

        // ── Variable declaration: let / const ─────────────────────────────
        AstContent::Declaration {
            kind,
            decl_name,
            decl_defs,
            ..
        } => {
            let name = match decl_name.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) if p.len() == 1 => p[0].clone(),
                _ => {
                    return Err(VmError::InvalidExpression(
                        "declaration name must be a simple identifier".into(),
                    ));
                }
            };
            let val = eval_expr(decl_defs, env)?;
            env.set(&name, val, kind)?;
            Ok(FnExecResult::Normal(RuntimeValue::Null))
        }

        // ── Assignment: x = expr ──────────────────────────────────────────
        AstContent::BinOp {
            op: Operator::Assign,
            left,
            right,
        } => {
            let val = eval_expr(right, env)?;
            match left.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) if p.len() == 1 => {
                    env.set(&p[0], val, &DeclKind::Variable)?;
                }
                _ => {
                    return Err(VmError::InvalidExpression(
                        "assignment target in function body must be a simple identifier".into(),
                    ));
                }
            }
            Ok(FnExecResult::Normal(RuntimeValue::Null))
        }

        // ── Nested fn definition ──────────────────────────────────────────
        AstContent::FnDef {
            name: Some(n),
            params,
            body,
            ..
        } => {
            let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
            let func = RuntimeValue::Function {
                params: param_names,
                body: body.clone(),
            };
            env.set(n, func, &DeclKind::Variable)?;
            Ok(FnExecResult::Normal(RuntimeValue::Null))
        }

        // ── Multi-way pattern match ───────────────────────────────────────
        AstContent::Match { scrutinee, arms } => {
            let scrutinee_val = eval_expr(scrutinee, env)?;

            // For Roll scrutinees precompute: scalar sum (used by Value and
            // Range arms) and the individual die vector (used by Array arms).
            let (scalar_val, roll_individuals): (RuntimeValue, Option<Vec<i64>>) =
                match &scrutinee_val {
                    RuntimeValue::Roll(dice) => {
                        let sum = checked_sum_rolls(dice)?;
                        (RuntimeValue::Int(sum), Some(dice.clone()))
                    }
                    other => (other.clone(), None),
                };

            for arm in arms {
                let is_match = match &arm.pattern {
                    MatchPattern::Wildcard => true,

                    MatchPattern::Value(pat_ast) => match eval_expr(pat_ast, env) {
                        Ok(pat_val) => values_equal(&scalar_val, &pat_val),
                        Err(_) => false,
                    },

                    MatchPattern::Range {
                        start,
                        end,
                        inclusive,
                        binding,
                    } => {
                        let start_i = eval_expr(start, env).ok().and_then(|v| match v {
                            RuntimeValue::Int(i) => Some(i),
                            _ => None,
                        });
                        let end_i = eval_expr(end, env).ok().and_then(|v| match v {
                            RuntimeValue::Int(i) => Some(i),
                            _ => None,
                        });
                        let scalar = match &scalar_val {
                            RuntimeValue::Int(n) => Some(*n),
                            RuntimeValue::Float(f) => {
                                if f.is_nan()
                                    || *f < (i64::MIN as f64)
                                    || *f > (i64::MAX as f64)
                                {
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
                                if in_range && let Some(name) = binding {
                                    env.set(name, RuntimeValue::Int(n), &DeclKind::Variable)?;
                                }
                                in_range
                            }
                            _ => false,
                        }
                    }

                    MatchPattern::Array(elems) => match &roll_individuals {
                        Some(dice) if dice.len() == elems.len() => {
                            elems
                                .iter()
                                .zip(dice.iter())
                                .all(|(pat_ast_opt, &die_val)| {
                                    match pat_ast_opt {
                                        None => true, // wildcard — always matches
                                        Some(pat_ast) => matches!(
                                            eval_expr(pat_ast, env),
                                            Ok(RuntimeValue::Int(v)) if v == die_val
                                        ),
                                    }
                                })
                        }
                        _ => false,
                    },
                };

                if is_match {
                    return exec_fn_block(&arm.body, env);
                }
            }

            // No arm matched — return Null (non-exhaustive match in fn body
            // is a silent no-op rather than an error, matching the fn-body
            // semantics for `if` without `else`).
            Ok(FnExecResult::Normal(RuntimeValue::Null))
        }

        // ── Bare expression (implicit last-value return) ───────────────────
        _ => eval_expr(stmt, env).map(FnExecResult::Normal),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{Ast, DeclKind, Operator, UnaryOperator};
    use crate::runtime::value::RuntimeValue;
    use crate::vm::env::Environment;

    #[test]
    fn test_list_literal_evaluates() {
        let env = Environment::default();
        let ast = Ast::list(vec![
            Ast::value(RuntimeValue::Int(1)),
            Ast::value(RuntimeValue::Int(2)),
            Ast::value(RuntimeValue::Int(3)),
        ]);
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(
            result,
            RuntimeValue::List(crate::runtime::value::shared(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ]))
        );
    }

    #[test]
    fn test_list_subscript_read() {
        let env = Environment::default();
        let list_ast = Ast::list(vec![
            Ast::value(RuntimeValue::Int(10)),
            Ast::value(RuntimeValue::Int(20)),
            Ast::value(RuntimeValue::Int(30)),
        ]);
        let subscript = Ast::subscript(list_ast, Ast::value(RuntimeValue::Int(1)));
        let result = eval_expr(&subscript, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(20));
    }

    #[test]
    fn test_list_subscript_negative_index() {
        let env = Environment::default();
        let list_ast = Ast::list(vec![
            Ast::value(RuntimeValue::Int(10)),
            Ast::value(RuntimeValue::Int(20)),
            Ast::value(RuntimeValue::Int(30)),
        ]);
        let subscript = Ast::subscript(list_ast, Ast::value(RuntimeValue::Int(-1)));
        let result = eval_expr(&subscript, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(30));
    }

    #[test]
    fn test_list_subscript_out_of_bounds() {
        let env = Environment::default();
        let list_ast = Ast::list(vec![Ast::value(RuntimeValue::Int(1))]);
        let subscript = Ast::subscript(list_ast, Ast::value(RuntimeValue::Int(5)));
        let err = eval_expr(&subscript, &env).unwrap_err();
        assert!(matches!(
            err,
            VmError::IndexOutOfBounds { index: 5, len: 1 }
        ));
    }

    #[test]
    fn test_list_is_truthy() {
        assert!(!is_truthy(&RuntimeValue::List(crate::runtime::value::shared(vec![]))));
        assert!(is_truthy(&RuntimeValue::List(crate::runtime::value::shared(vec![RuntimeValue::Int(1)]))));
    }

    #[test]
    fn test_list_values_equal() {
        let a = RuntimeValue::List(crate::runtime::value::shared(vec![RuntimeValue::Int(1), RuntimeValue::Int(2)]));
        let b = RuntimeValue::List(crate::runtime::value::shared(vec![RuntimeValue::Int(1), RuntimeValue::Int(2)]));
        let c = RuntimeValue::List(crate::runtime::value::shared(vec![RuntimeValue::Int(1), RuntimeValue::Int(9)]));
        assert!(values_equal(&a, &b));
        assert!(!values_equal(&a, &c));
        assert!(!values_equal(&a, &RuntimeValue::List(crate::runtime::value::shared(vec![]))));
    }

    #[test]
    fn test_list_format() {
        let list = RuntimeValue::List(crate::runtime::value::shared(vec![
            RuntimeValue::Int(1),
            RuntimeValue::Bool(true),
            RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain("hello")),
        ]));
        let formatted = format_runtime_value(&list, None);
        assert_eq!(formatted, "[1, true, hello]");
    }

    #[test]
    fn test_call_list_len_method() {
        let mut env = Environment::new();
        env.set(
            "xs",
            RuntimeValue::List(crate::runtime::value::shared(vec![
                RuntimeValue::Int(10),
                RuntimeValue::Int(20),
                RuntimeValue::Int(30),
            ])),
            &DeclKind::Variable,
        )
        .unwrap();
        // xs.len() parses as Call { func_path: IdentPath(["xs","len"]), params: ExprList([]) }
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["xs".into(), "len".into()])),
            Ast::expr_list(vec![]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(3));
    }

    #[test]
    fn test_call_list_append_method() {
        let mut env = Environment::new();
        env.set(
            "xs",
            RuntimeValue::List(crate::runtime::value::shared(vec![RuntimeValue::Int(1), RuntimeValue::Int(2)])),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["xs".into(), "append".into()])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::Int(3))]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(
            result,
            RuntimeValue::List(crate::runtime::value::shared(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ]))
        );
    }

    #[test]
    fn test_call_list_contains_method() {
        let mut env = Environment::new();
        env.set(
            "xs",
            RuntimeValue::List(crate::runtime::value::shared(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ])),
            &DeclKind::Variable,
        )
        .unwrap();

        let call_hit = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec![
                "xs".into(),
                "contains".into(),
            ])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::Int(2))]),
        );
        assert_eq!(
            eval_expr(&call_hit, &env).unwrap(),
            RuntimeValue::Bool(true)
        );

        let call_miss = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec![
                "xs".into(),
                "contains".into(),
            ])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::Int(99))]),
        );
        assert_eq!(
            eval_expr(&call_miss, &env).unwrap(),
            RuntimeValue::Bool(false)
        );
    }

    #[test]
    fn test_exec_fn_body_basic() {
        // fn add(x, y): x + y — implicit last-expression return.
        let body = Ast::block(vec![Ast::binop(
            Operator::Plus,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::IdentPath(vec!["y".into()])),
        )]);
        let env = Environment::new();
        let result = exec_fn_body(
            &body,
            &["x".to_string(), "y".to_string()],
            &[RuntimeValue::Int(3), RuntimeValue::Int(4)],
            &env,
        )
        .unwrap();
        assert_eq!(result, RuntimeValue::Int(7));
    }

    #[test]
    fn test_exec_fn_body_explicit_return() {
        // fn double(x): return x * 2 — explicit early return.
        let body = Ast::block(vec![Ast::return_stmt(Some(Ast::binop(
            Operator::Multiply,
            Ast::value(RuntimeValue::IdentPath(vec!["x".into()])),
            Ast::value(RuntimeValue::Int(2)),
        )))]);
        let env = Environment::new();
        let result = exec_fn_body(&body, &["x".to_string()], &[RuntimeValue::Int(5)], &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(10));
    }

    #[test]
    fn test_exec_fn_body_wrong_arg_count() {
        // Arity mismatch must produce a TypeError.
        let body = Ast::block(vec![]);
        let env = Environment::new();
        let err = exec_fn_body(&body, &["x".to_string()], &[], &env).unwrap_err();
        assert!(
            matches!(err, VmError::TypeError(_)),
            "expected TypeError, got {err:?}"
        );
    }

    #[test]
    fn test_exec_fn_body_no_outer_scope() {
        // Pure functions must not see variables from the call site.
        // `outer` only exists in the host env; exec_fn_body must NOT receive it.
        let body = Ast::block(vec![Ast::value(RuntimeValue::IdentPath(vec![
            "outer".into(),
        ]))]);
        // exec_fn_body creates a fresh isolated environment — outer is absent.
        let env = Environment::new();
        let err = exec_fn_body(&body, &[], &[], &env).unwrap_err();
        assert!(
            matches!(err, VmError::UndefinedVariable(_)),
            "expected UndefinedVariable, got {err:?}"
        );
    }

    #[test]
    fn test_exec_fn_body_recursion_depth_limit() {
        use super::{FN_CALL_DEPTH, MAX_FN_CALL_DEPTH};

        // Artificially set the depth counter to the limit so the next
        // exec_fn_body call triggers StackOverflow.
        FN_CALL_DEPTH.with(|d| d.set(MAX_FN_CALL_DEPTH));

        let body = Ast::block(vec![Ast::value(RuntimeValue::Int(42))]);
        let env = Environment::new();
        let err = exec_fn_body(&body, &[], &[], &env).unwrap_err();
        assert!(
            matches!(err, VmError::StackOverflow(n) if n == MAX_FN_CALL_DEPTH),
            "expected StackOverflow({MAX_FN_CALL_DEPTH}), got {err:?}"
        );

        // Depth unchanged — enter() bails before incrementing.
        FN_CALL_DEPTH.with(|d| {
            assert_eq!(d.get(), MAX_FN_CALL_DEPTH);
            d.set(0); // reset for other tests on this thread
        });
    }

    #[test]
    fn test_exec_fn_body_depth_resets_after_normal_call() {
        use super::FN_CALL_DEPTH;

        FN_CALL_DEPTH.with(|d| d.set(0));

        let body = Ast::block(vec![Ast::value(RuntimeValue::Int(7))]);
        let env = Environment::new();
        let result = exec_fn_body(&body, &[], &[], &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(7));

        // Guard dropped → depth back to 0.
        FN_CALL_DEPTH.with(|d| assert_eq!(d.get(), 0));
    }

    #[test]
    fn test_exec_fn_body_depth_resets_after_error() {
        use super::FN_CALL_DEPTH;

        FN_CALL_DEPTH.with(|d| d.set(0));

        // Trigger an arity error inside exec_fn_body — guard must still drop.
        let body = Ast::block(vec![]);
        let env = Environment::new();
        let _ = exec_fn_body(&body, &["x".to_string()], &[], &env);

        FN_CALL_DEPTH.with(|d| {
            assert_eq!(d.get(), 0, "depth leaked after error");
        });
    }

    #[test]
    fn test_format_function_shows_params() {
        let val = RuntimeValue::Function {
            params: vec!["x".to_string(), "y".to_string()],
            body: Box::new(Ast::block(vec![])),
        };
        assert_eq!(format_runtime_value(&val, None), "fn(x, y)");
    }

    #[test]
    fn test_format_function_no_params() {
        let val = RuntimeValue::Function {
            params: vec![],
            body: Box::new(Ast::value(RuntimeValue::Null)),
        };
        assert_eq!(format_runtime_value(&val, None), "fn()");
    }

    // ── Struct construction ───────────────────────────────────────────────────

    #[test]
    fn test_struct_construction_positional() {
        // struct Point { x: int, y: int }
        // let p = Point(3, 7)  →  RuntimeValue::Struct { name: "Point", fields: {x:3, y:7} }
        let mut env = Environment::new();
        env.define_struct("Point".into(), vec!["x".into(), "y".into()]);

        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["Point".into()])),
            Ast::expr_list(vec![
                Ast::value(RuntimeValue::Int(3)),
                Ast::value(RuntimeValue::Int(7)),
            ]),
        );

        let result = eval_expr(&call, &env).expect("struct construction should succeed");
        match result {
            RuntimeValue::Struct { name, fields } => {
                assert_eq!(name, "Point");
                assert_eq!(fields.borrow().get("x"), Some(&RuntimeValue::Int(3)));
                assert_eq!(fields.borrow().get("y"), Some(&RuntimeValue::Int(7)));
            }
            other => panic!("expected Struct, got {other:?}"),
        }
    }

    #[test]
    fn test_struct_construction_wrong_arity_errors() {
        let mut env = Environment::new();
        env.define_struct("Point".into(), vec!["x".into(), "y".into()]);

        // Pass only one argument to a 2-field struct — should error.
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["Point".into()])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::Int(1))]),
        );

        let result = eval_expr(&call, &env);
        assert!(
            matches!(result, Err(VmError::TypeError(_))),
            "expected TypeError for wrong arity, got {result:?}"
        );
    }

    // ── Struct field access via Subscript ─────────────────────────────────────

    #[test]
    fn test_struct_subscript_read() {
        let mut env = Environment::new();
        let mut fields = std::collections::HashMap::new();
        fields.insert("x".into(), RuntimeValue::Int(10));
        fields.insert("y".into(), RuntimeValue::Int(20));
        env.set(
            "pt",
            RuntimeValue::Struct {
                name: "Point".into(),
                fields: crate::runtime::value::shared(fields),
            },
            &crate::parser::ast::DeclKind::Variable,
        )
        .unwrap();

        // pt["x"]  →  Int(10)
        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["pt".into()])),
            Ast::value(RuntimeValue::Str(
                crate::lexer::strings::ParsedString::new_plain("x"),
            )),
        );
        let result = eval_expr(&subscript, &env).expect("subscript read should succeed");
        assert_eq!(result, RuntimeValue::Int(10));
    }

    #[test]
    fn test_struct_subscript_missing_field_errors() {
        let mut env = Environment::new();
        let mut fields = std::collections::HashMap::new();
        fields.insert("x".into(), RuntimeValue::Int(5));
        env.set(
            "pt",
            RuntimeValue::Struct {
                name: "Point".into(),
                fields: crate::runtime::value::shared(fields),
            },
            &crate::parser::ast::DeclKind::Variable,
        )
        .unwrap();

        let subscript = Ast::subscript(
            Ast::value(RuntimeValue::IdentPath(vec!["pt".into()])),
            Ast::value(RuntimeValue::Str(
                crate::lexer::strings::ParsedString::new_plain("z"),
            )),
        );
        let result = eval_expr(&subscript, &env);
        assert!(
            matches!(result, Err(VmError::UndefinedVariable(_))),
            "expected UndefinedVariable for missing field, got {result:?}"
        );
    }

    // ── Struct field access via IdentPath (point.x) ───────────────────────────

    #[test]
    fn test_struct_field_access_via_ident_path() {
        let mut env = Environment::new();
        let mut fields = std::collections::HashMap::new();
        fields.insert("hp".into(), RuntimeValue::Int(100));
        env.set(
            "player",
            RuntimeValue::Struct {
                name: "Player".into(),
                fields: crate::runtime::value::shared(fields),
            },
            &crate::parser::ast::DeclKind::Variable,
        )
        .unwrap();

        // Evaluating IdentPath ["player", "hp"] should resolve via struct field access.
        let result = eval_runtime_value(
            &RuntimeValue::IdentPath(vec!["player".into(), "hp".into()]),
            &env,
        )
        .expect("struct field via IdentPath should succeed");
        assert_eq!(result, RuntimeValue::Int(100));
    }

    #[test]
    fn test_struct_format() {
        let mut fields = std::collections::HashMap::new();
        fields.insert("x".into(), RuntimeValue::Int(1));
        fields.insert("y".into(), RuntimeValue::Int(2));
        let val = RuntimeValue::Struct {
            name: "Point".into(),
            fields: crate::runtime::value::shared(fields),
        };
        // format_runtime_value should show "Point(2)" — name + field count.
        let formatted = format_runtime_value(&val, None);
        assert!(
            formatted.starts_with("Point("),
            "expected format to start with 'Point(', got '{formatted}'"
        );
    }

    #[test]
    fn test_negate_int_min_overflows_in_release_panics_in_debug() {
        // -i64::MIN is mathematically i64::MAX + 1, which cannot be represented
        // as i64.  eval_unary uses plain `-i` with no overflow check.
        //
        // Correct behaviour: return Err(VmError::TypeError) — unrepresentable.
        // Debug build:   panics  ("attempt to negate with overflow").
        // Release build: silently wraps to i64::MIN (wrong value).
        //
        // This test asserts the *correct* behaviour and therefore fails against
        // the current implementation in BOTH build profiles.
        let result = std::panic::catch_unwind(|| {
            eval_unary(&UnaryOperator::Negate, RuntimeValue::Int(i64::MIN))
        });

        match result {
            Err(_panic) => {
                // Debug build: the panic was caught.  The VM should have returned
                // VmError::TypeError instead of panicking.
                panic!(
                    "BUG (debug): eval_unary(-i64::MIN) panicked with arithmetic overflow. \
                     It should return Err(VmError::TypeError) instead."
                );
            }
            Ok(Ok(RuntimeValue::Int(got))) => {
                // Release build: returned a value — it must NOT wrap back to i64::MIN.
                assert_ne!(
                    got,
                    i64::MIN,
                    "BUG (release): eval_unary(-i64::MIN) wrapped to i64::MIN (silent overflow). \
                     It should return Err(VmError::TypeError) instead."
                );
                panic!(
                    "BUG (release): eval_unary(-i64::MIN) returned Int({got}) — unexpected wrapped \
                     value; should return Err(VmError::TypeError)."
                );
            }
            Ok(Err(_vm_err)) => {
                // This is the correct path — a VmError was returned, not a panic.
                // The test passes only if we reach here.
            }
            Ok(Ok(other)) => panic!("unexpected non-Int result: {other:?}"),
        }
    }

    #[test]
    fn test_values_equal_int_float_precision_loss_above_2_pow_53() {
        // 2^53 = 9_007_199_254_740_992 is the largest integer exactly representable
        // as f64.  2^53 + 1 = 9_007_199_254_740_993 is NOT exactly representable:
        // casting it to f64 rounds down to 2^53.
        //
        // values_equal(Int(2^53+1), Float(2^53)) should return FALSE — they are
        // arithmetically different — but the i64→f64 cast loses the +1, making
        // the bitwise comparison true.
        //
        // This test asserts the correct (false) result and therefore FAILS against
        // the current implementation.
        const TWO_POW_53: i64 = 9_007_199_254_740_992;
        const TWO_POW_53_PLUS_1: i64 = 9_007_199_254_740_993;
        let int_val = RuntimeValue::Int(TWO_POW_53_PLUS_1);
        let float_val = RuntimeValue::Float(TWO_POW_53 as f64);

        assert!(
            !values_equal(&int_val, &float_val),
            "BUG: Int(2^53+1) == Float(2^53) incorrectly returned true. \
             The i64→f64 cast loses the low bit, making distinct values appear equal. \
             Fix: use a widening comparison or reject cross-type numeric equality."
        );
    }

    #[test]
    fn test_floordiv_int_int_and_float_float_must_agree_for_same_values() {
        // floor(-7 / -2) = floor(3.5) = 3.
        //
        // Int//Int path uses div_euclid(-7, -2) = 4   ← WRONG (Euclidean, not floor)
        // Float//Float path uses (-7.0/-2.0).floor() = 3.0  ← correct
        //
        // Both paths implement the `//` operator; they MUST produce the same result
        // for the same mathematical input.  This test asserts equality and therefore
        // FAILS against the current implementation.
        let int_result = numeric_floordiv(RuntimeValue::Int(-7), RuntimeValue::Int(-2))
            .expect("Int(-7) // Int(-2) should not error");
        let float_result = numeric_floordiv(RuntimeValue::Float(-7.0), RuntimeValue::Float(-2.0))
            .expect("Float(-7.0) // Float(-2.0) should not error");

        // Normalise both to f64 for comparison (they should both be 3.0).
        let int_as_f64 = match int_result {
            RuntimeValue::Int(n) => n as f64,
            RuntimeValue::Float(f) => f,
            other => panic!("unexpected Int//Int result type: {other:?}"),
        };
        let float_val = match float_result {
            RuntimeValue::Int(n) => n as f64,
            RuntimeValue::Float(f) => f,
            other => panic!("unexpected Float//Float result type: {other:?}"),
        };

        assert_eq!(
            int_as_f64, float_val,
            "BUG: Int(-7)//Int(-2) = {int_as_f64} but Float(-7.0)//Float(-2.0) = {float_val}. \
             The `//` operator must have consistent semantics regardless of operand type. \
             Fix: use integer floor division (not div_euclid) for the Int//Int path."
        );
    }

    #[test]
    fn test_values_equal_int_max_float_boundary() {
        // i64::MAX = 2^63-1 cannot be exactly represented as f64; it rounds UP to 2^63.
        // So Int(i64::MAX) must NOT equal Float(i64::MAX as f64) = 2^63.
        let int_max = RuntimeValue::Int(i64::MAX);
        let float_2pow63 = RuntimeValue::Float(i64::MAX as f64); // = 9223372036854775808.0
        assert!(
            !values_equal(&int_max, &float_2pow63),
            "Int(i64::MAX) must not equal Float(i64::MAX as f64) — the cast rounds up to 2^63"
        );
        // Symmetric
        assert!(
            !values_equal(&float_2pow63, &int_max),
            "Float(i64::MAX as f64) must not equal Int(i64::MAX)"
        );
    }

    #[test]
    fn test_floordiv_int_min_by_neg_one_errors() {
        // i64::MIN // -1 = -(i64::MIN) = i64::MAX + 1, which overflows.
        // Must return Err, not panic.
        let result = numeric_floordiv(RuntimeValue::Int(i64::MIN), RuntimeValue::Int(-1));
        assert!(
            matches!(result, Err(VmError::TypeError(_))),
            "i64::MIN // -1 should return VmError::TypeError, got {result:?}"
        );
    }

    #[test]
    fn test_div_int_min_by_neg_one_errors() {
        let result = numeric_div(RuntimeValue::Int(i64::MIN), RuntimeValue::Int(-1));
        assert!(
            matches!(result, Err(VmError::TypeError(_))),
            "i64::MIN / -1 should return VmError::TypeError, got {result:?}"
        );
    }

    #[test]
    fn test_values_equal_struct_value_semantics() {
        use std::collections::HashMap;
        let make = |name: &str, x: i64| RuntimeValue::Struct {
            name: name.to_string(),
            fields: {
                let mut m = HashMap::new();
                m.insert("x".to_string(), RuntimeValue::Int(x));
                crate::runtime::value::shared(m)
            },
        };
        assert!(
            values_equal(&make("Point", 1), &make("Point", 1)),
            "identical structs must be equal"
        );
        assert!(
            !values_equal(&make("Point", 1), &make("Point", 2)),
            "different field values must not be equal"
        );
        assert!(
            !values_equal(&make("Point", 1), &make("Vec", 1)),
            "different type names must not be equal"
        );
    }

    // ── resolve_interp_path ───────────────────────────────────────────────────

    #[test]
    fn resolve_interp_path_single_segment_found() {
        let mut env = Environment::new();
        env.set("gold", RuntimeValue::Int(42), &DeclKind::Variable)
            .unwrap();
        let result = resolve_interp_path("gold", &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(42));
    }

    #[test]
    fn resolve_interp_path_single_segment_missing_returns_err() {
        let env = Environment::new();
        let result = resolve_interp_path("missing", &env);
        assert!(
            result.is_err(),
            "undefined single-segment path must return Err"
        );
    }

    #[test]
    fn resolve_interp_path_two_segment_module_namespaced_key() {
        // Store "inv::gold" (the compiled namespace form) and resolve via the
        // dotted path "inv.gold" — the helper must expand the dot to "::".
        let mut env = Environment::new();
        let ns_key = crate::ir::namespace("inv", "gold");
        env.set(&ns_key, RuntimeValue::Int(99), &DeclKind::Variable)
            .unwrap();
        let result = resolve_interp_path("inv.gold", &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(99));
    }

    // ── Range ─────────────────────────────────────────────────────────────────

    #[test]
    fn test_range_exclusive_constructs() {
        let env = Environment::default();
        let ast = Ast::range_exclusive_op(
            Ast::value(RuntimeValue::Int(0)),
            Ast::value(RuntimeValue::Int(5)),
        );
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Range {
                start: 0,
                end: 5,
                inclusive: false
            }
        );
    }

    #[test]
    fn test_range_inclusive_constructs() {
        let env = Environment::default();
        let ast = Ast::range_inclusive_op(
            Ast::value(RuntimeValue::Int(0)),
            Ast::value(RuntimeValue::Int(5)),
        );
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Range {
                start: 0,
                end: 5,
                inclusive: true
            }
        );
    }

    #[test]
    fn test_in_operator_true() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(3)),
            Ast::range_exclusive_op(
                Ast::value(RuntimeValue::Int(0)),
                Ast::value(RuntimeValue::Int(5)),
            ),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn test_in_operator_false_at_exclusive_bound() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(5)),
            Ast::range_exclusive_op(
                Ast::value(RuntimeValue::Int(0)),
                Ast::value(RuntimeValue::Int(5)),
            ),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn test_in_operator_true_at_inclusive_bound() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(5)),
            Ast::range_inclusive_op(
                Ast::value(RuntimeValue::Int(0)),
                Ast::value(RuntimeValue::Int(5)),
            ),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn test_range_len_method() {
        let mut env = Environment::new();
        env.set(
            "r",
            RuntimeValue::Range {
                start: 0,
                end: 5,
                inclusive: false,
            },
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["r".into(), "len".into()])),
            Ast::expr_list(vec![]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(5));
    }

    #[test]
    fn test_range_is_truthy_nonempty() {
        assert!(is_truthy(&RuntimeValue::Range {
            start: 0,
            end: 5,
            inclusive: false
        }));
    }

    #[test]
    fn test_range_is_truthy_empty() {
        assert!(!is_truthy(&RuntimeValue::Range {
            start: 5,
            end: 5,
            inclusive: false
        }));
    }

    #[test]
    fn test_range_is_truthy_inclusive_single_element() {
        assert!(is_truthy(&RuntimeValue::Range {
            start: 5,
            end: 5,
            inclusive: true
        }));
    }

    #[test]
    fn test_range_values_equal() {
        let r1 = RuntimeValue::Range {
            start: 0,
            end: 5,
            inclusive: false,
        };
        let r2 = RuntimeValue::Range {
            start: 0,
            end: 5,
            inclusive: false,
        };
        assert!(values_equal(&r1, &r2));
    }

    #[test]
    fn test_range_values_not_equal_different_bound() {
        let r1 = RuntimeValue::Range {
            start: 0,
            end: 5,
            inclusive: false,
        };
        let r2 = RuntimeValue::Range {
            start: 0,
            end: 6,
            inclusive: false,
        };
        assert!(!values_equal(&r1, &r2));
    }

    #[test]
    fn test_range_values_not_equal_inclusive_vs_exclusive() {
        let r1 = RuntimeValue::Range {
            start: 0,
            end: 5,
            inclusive: false,
        };
        let r2 = RuntimeValue::Range {
            start: 0,
            end: 5,
            inclusive: true,
        };
        assert!(!values_equal(&r1, &r2));
    }

    #[test]
    fn test_in_operator_wrong_rhs_type_errors() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(3)),
            Ast::value(RuntimeValue::Int(5)),
        );
        assert!(eval_expr(&ast, &env).is_err());
    }

    // ── `in` operator: List, Map, Str tests ──────────────────────────────────

    #[test]
    fn test_in_list_found() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(2)),
            Ast::list(vec![
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2)),
                Ast::value(RuntimeValue::Int(3)),
            ]),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn test_in_list_not_found() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(99)),
            Ast::list(vec![
                Ast::value(RuntimeValue::Int(1)),
                Ast::value(RuntimeValue::Int(2)),
                Ast::value(RuntimeValue::Int(3)),
            ]),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn test_in_list_string() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("b"))),
            Ast::list(vec![
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("a"))),
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("b"))),
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("c"))),
            ]),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn test_in_list_mixed_types() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(1)),
            Ast::list(vec![
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("a"))),
                Ast::value(RuntimeValue::Str(ParsedString::new_plain("b"))),
            ]),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn test_in_map_key_found() {
        let env = Environment::default();
        let mut map = HashMap::new();
        map.insert("key".to_string(), Box::new(RuntimeValue::Int(1)));
        map.insert("other".to_string(), Box::new(RuntimeValue::Int(2)));
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("key"))),
            Ast::value(RuntimeValue::Map(crate::runtime::value::shared(map))),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn test_in_map_key_not_found() {
        let env = Environment::default();
        let mut map = HashMap::new();
        map.insert("key".to_string(), Box::new(RuntimeValue::Int(1)));
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("missing"))),
            Ast::value(RuntimeValue::Map(crate::runtime::value::shared(map))),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn test_in_map_non_string_key_errors() {
        let env = Environment::default();
        let mut map = HashMap::new();
        map.insert("key".to_string(), Box::new(RuntimeValue::Int(1)));
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(42)),
            Ast::value(RuntimeValue::Map(crate::runtime::value::shared(map))),
        );
        assert!(eval_expr(&ast, &env).is_err());
    }

    #[test]
    fn test_in_string_found() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("sub"))),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("substring"))),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn test_in_string_not_found() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("xyz"))),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("hello world"))),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn test_in_string_non_string_lhs_errors() {
        let env = Environment::default();
        let ast = Ast::in_op(
            Ast::value(RuntimeValue::Int(42)),
            Ast::value(RuntimeValue::Str(ParsedString::new_plain("hello"))),
        );
        assert!(eval_expr(&ast, &env).is_err());
    }

    // ── RuntimeValue::Roll tests ──────────────────────────────────────────────

    /// A deterministic roller that returns a fixed sequence of values,
    /// truncated to the requested `count`.  Used to make dice-evaluation
    /// tests reproducible.
    struct FixedRoller(Vec<i64>);

    impl crate::vm::DiceRoller for FixedRoller {
        fn roll_individual(&self, count: u32, _sides: u32) -> Vec<i64> {
            self.0[..count as usize].to_vec()
        }
    }

    // 1. Dice evaluates to Roll ────────────────────────────────────────────────

    #[test]
    fn test_dice_evaluates_to_roll_with_fixed_roller() {
        let mut env = Environment::new();
        env.set_dice_roller(Box::new(FixedRoller(vec![3, 5])));
        // Evaluating Dice(2, 6) with the fixed roller must produce Roll([3, 5]).
        let ast = Ast::value(RuntimeValue::Dice(2, 6));
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Roll(vec![3, 5]));
    }

    #[test]
    fn test_dice_evaluates_to_roll_element_count_matches_n() {
        let mut env = Environment::new();
        // Roller supplies 4 fixed values; we only ask for 3.
        env.set_dice_roller(Box::new(FixedRoller(vec![2, 4, 6, 1])));
        let ast = Ast::value(RuntimeValue::Dice(3, 6));
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Roll(vec![2, 4, 6]));
        if let RuntimeValue::Roll(dice) = result {
            assert_eq!(dice.len(), 3);
        }
    }

    #[test]
    fn test_dice_evaluates_to_roll_zero_count_returns_empty() {
        let mut env = Environment::new();
        env.set_dice_roller(Box::new(FixedRoller(vec![])));
        let ast = Ast::value(RuntimeValue::Dice(0, 6));
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Roll(vec![]));
    }

    // 2. Arithmetic coercion: Roll is treated as its sum ──────────────────────

    #[test]
    fn test_roll_plus_int_coerces_to_sum() {
        // Roll([3, 5]) has sum 8; 8 + 2 = 10.0 (float because Roll is not Int)
        let env = Environment::default();
        let ast = Ast::binop(
            Operator::Plus,
            Ast::value(RuntimeValue::Roll(vec![3, 5])),
            Ast::value(RuntimeValue::Int(2)),
        );
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Float(10.0));
    }

    #[test]
    fn test_roll_minus_int_coerces_to_sum() {
        // Roll([3, 5]) sum = 8; 8 - 1 = 7.0
        let env = Environment::default();
        let ast = Ast::binop(
            Operator::Minus,
            Ast::value(RuntimeValue::Roll(vec![3, 5])),
            Ast::value(RuntimeValue::Int(1)),
        );
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Float(7.0));
    }

    #[test]
    fn test_roll_multiply_int_coerces_to_sum() {
        // Roll([2, 4]) sum = 6; 6 * 3 = 18.0
        let env = Environment::default();
        let ast = Ast::binop(
            Operator::Multiply,
            Ast::value(RuntimeValue::Roll(vec![2, 4])),
            Ast::value(RuntimeValue::Int(3)),
        );
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Float(18.0));
    }

    #[test]
    fn test_roll_divide_int_coerces_to_sum() {
        // Roll([4, 2]) sum = 6; 6 / 2 = 3.0
        let env = Environment::default();
        let ast = Ast::binop(
            Operator::Divide,
            Ast::value(RuntimeValue::Roll(vec![4, 2])),
            Ast::value(RuntimeValue::Int(2)),
        );
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Float(3.0));
    }

    #[test]
    fn test_int_plus_roll_coerces_roll_to_sum() {
        // Roll is on the rhs; same coercion applies: 2 + 8 = 10.0
        let env = Environment::default();
        let ast = Ast::binop(
            Operator::Plus,
            Ast::value(RuntimeValue::Int(2)),
            Ast::value(RuntimeValue::Roll(vec![3, 5])),
        );
        let result = eval_expr(&ast, &env).unwrap();
        assert_eq!(result, RuntimeValue::Float(10.0));
    }

    // 3. Method dispatch on Roll ───────────────────────────────────────────────

    #[test]
    fn test_roll_method_min() {
        let mut env = Environment::new();
        env.set(
            "roll",
            RuntimeValue::Roll(vec![3, 1, 5, 2]),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["roll".into(), "min".into()])),
            Ast::expr_list(vec![]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(1));
    }

    #[test]
    fn test_roll_method_max() {
        let mut env = Environment::new();
        env.set(
            "roll",
            RuntimeValue::Roll(vec![3, 1, 5, 2]),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["roll".into(), "max".into()])),
            Ast::expr_list(vec![]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(5));
    }

    #[test]
    fn test_roll_method_sum() {
        let mut env = Environment::new();
        env.set(
            "roll",
            RuntimeValue::Roll(vec![3, 1, 5, 2]),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["roll".into(), "sum".into()])),
            Ast::expr_list(vec![]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(11));
    }

    #[test]
    fn test_roll_method_len() {
        let mut env = Environment::new();
        env.set(
            "roll",
            RuntimeValue::Roll(vec![3, 1, 5, 2]),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["roll".into(), "len".into()])),
            Ast::expr_list(vec![]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(4));
    }

    #[test]
    fn test_roll_method_min_on_single_element() {
        let mut env = Environment::new();
        env.set("roll", RuntimeValue::Roll(vec![6]), &DeclKind::Variable)
            .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["roll".into(), "min".into()])),
            Ast::expr_list(vec![]),
        );
        assert_eq!(eval_expr(&call, &env).unwrap(), RuntimeValue::Int(6));
    }

    // 4. Free-function builtins: min(roll), max(roll), sum(roll) ──────────────

    #[test]
    fn test_roll_free_function_min() {
        let mut env = Environment::new();
        env.set(
            "roll",
            RuntimeValue::Roll(vec![2, 6, 4]),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["min".into()])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "roll".into(),
            ]))]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(2));
    }

    #[test]
    fn test_roll_free_function_max() {
        let mut env = Environment::new();
        env.set(
            "roll",
            RuntimeValue::Roll(vec![2, 6, 4]),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["max".into()])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "roll".into(),
            ]))]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(6));
    }

    #[test]
    fn test_roll_free_function_sum() {
        let mut env = Environment::new();
        env.set(
            "roll",
            RuntimeValue::Roll(vec![2, 6, 4]),
            &DeclKind::Variable,
        )
        .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["sum".into()])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "roll".into(),
            ]))]),
        );
        let result = eval_expr(&call, &env).unwrap();
        assert_eq!(result, RuntimeValue::Int(12));
    }

    #[test]
    fn test_roll_free_function_min_on_empty_roll_returns_null() {
        let mut env = Environment::new();
        env.set("roll", RuntimeValue::Roll(vec![]), &DeclKind::Variable)
            .unwrap();
        let call = Ast::call(
            Ast::value(RuntimeValue::IdentPath(vec!["min".into()])),
            Ast::expr_list(vec![Ast::value(RuntimeValue::IdentPath(vec![
                "roll".into(),
            ]))]),
        );
        assert_eq!(eval_expr(&call, &env).unwrap(), RuntimeValue::Null);
    }

    // 5. Subscript indexing into Roll ─────────────────────────────────────────

    #[test]
    fn test_roll_subscript_first_element() {
        let env = Environment::default();
        let ast = Ast::subscript(
            Ast::value(RuntimeValue::Roll(vec![3, 5])),
            Ast::value(RuntimeValue::Int(0)),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Int(3));
    }

    #[test]
    fn test_roll_subscript_second_element() {
        let env = Environment::default();
        let ast = Ast::subscript(
            Ast::value(RuntimeValue::Roll(vec![3, 5])),
            Ast::value(RuntimeValue::Int(1)),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Int(5));
    }

    #[test]
    fn test_roll_subscript_negative_index() {
        // -1 refers to the last element.
        let env = Environment::default();
        let ast = Ast::subscript(
            Ast::value(RuntimeValue::Roll(vec![3, 5])),
            Ast::value(RuntimeValue::Int(-1)),
        );
        assert_eq!(eval_expr(&ast, &env).unwrap(), RuntimeValue::Int(5));
    }

    #[test]
    fn test_roll_subscript_out_of_bounds_errors() {
        let env = Environment::default();
        let ast = Ast::subscript(
            Ast::value(RuntimeValue::Roll(vec![3, 5])),
            Ast::value(RuntimeValue::Int(5)),
        );
        let err = eval_expr(&ast, &env).unwrap_err();
        assert!(
            matches!(err, VmError::IndexOutOfBounds { index: 5, len: 2 }),
            "expected IndexOutOfBounds, got {err:?}"
        );
    }

    // 6. format_runtime_value for Roll ────────────────────────────────────────

    #[test]
    fn test_roll_format_produces_bracketed_list() {
        let roll = RuntimeValue::Roll(vec![3, 5]);
        assert_eq!(format_runtime_value(&roll, None), "[3, 5]");
    }

    #[test]
    fn test_roll_format_single_element() {
        let roll = RuntimeValue::Roll(vec![6]);
        assert_eq!(format_runtime_value(&roll, None), "[6]");
    }

    #[test]
    fn test_roll_format_empty_roll_is_empty_brackets() {
        let roll = RuntimeValue::Roll(vec![]);
        assert_eq!(format_runtime_value(&roll, None), "[]");
    }

    #[test]
    fn test_roll_format_multiple_elements() {
        let roll = RuntimeValue::Roll(vec![1, 2, 3, 4]);
        assert_eq!(format_runtime_value(&roll, None), "[1, 2, 3, 4]");
    }

    // 7. Equality semantics for Roll ──────────────────────────────────────────

    #[test]
    fn test_roll_values_equal_same_elements_same_order() {
        let a = RuntimeValue::Roll(vec![3, 5]);
        let b = RuntimeValue::Roll(vec![3, 5]);
        assert!(values_equal(&a, &b));
    }

    #[test]
    fn test_roll_values_not_equal_different_order() {
        // Order matters: [3, 5] != [5, 3]
        let a = RuntimeValue::Roll(vec![3, 5]);
        let b = RuntimeValue::Roll(vec![5, 3]);
        assert!(!values_equal(&a, &b));
    }

    #[test]
    fn test_roll_values_not_equal_different_length() {
        let a = RuntimeValue::Roll(vec![3, 5]);
        let b = RuntimeValue::Roll(vec![3]);
        assert!(!values_equal(&a, &b));
    }

    #[test]
    fn test_roll_values_equal_empty_rolls() {
        let a = RuntimeValue::Roll(vec![]);
        let b = RuntimeValue::Roll(vec![]);
        assert!(values_equal(&a, &b));
    }

    #[test]
    fn test_roll_not_equal_to_int_with_same_sum() {
        // Roll([3, 5]) must not compare equal to Int(8) — different types.
        let roll = RuntimeValue::Roll(vec![3, 5]);
        let int = RuntimeValue::Int(8);
        assert!(!values_equal(&roll, &int));
    }

    #[test]
    fn test_roll_not_equal_to_list_with_same_elements() {
        // Roll([3, 5]) must not compare equal to List([Int(3), Int(5)]).
        let roll = RuntimeValue::Roll(vec![3, 5]);
        let list = RuntimeValue::List(crate::runtime::value::shared(vec![RuntimeValue::Int(3), RuntimeValue::Int(5)]));
        assert!(!values_equal(&roll, &list));
    }

    // 8. Truthiness for Roll ──────────────────────────────────────────────────

    #[test]
    fn test_roll_non_empty_is_truthy() {
        assert!(is_truthy(&RuntimeValue::Roll(vec![3, 5])));
    }

    #[test]
    fn test_roll_single_element_is_truthy() {
        assert!(is_truthy(&RuntimeValue::Roll(vec![1])));
    }

    #[test]
    fn test_roll_empty_is_falsy() {
        assert!(!is_truthy(&RuntimeValue::Roll(vec![])));
    }
}
