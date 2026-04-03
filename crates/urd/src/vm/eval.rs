//! Pure expression evaluator: converts AST expressions to [`RuntimeValue`]s.

use std::collections::HashMap;

use crate::lexer::strings::{Interpolation, ParsedString, StringPart};
use crate::parser::ast::{AstContent, DeclKind, Operator, UnaryOperator};
use crate::runtime::value::RuntimeValue;

use super::VmError;
use super::env::Environment;
use super::float_methods;
use super::int_methods;
use super::list_methods;
use super::map_methods;
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
                        RuntimeValue::List(list) => list_methods::dispatch(list, method, &args),
                        RuntimeValue::Str(s) => str_methods::dispatch(s, method, &args),
                        RuntimeValue::Int(n) => int_methods::dispatch(n, method, &args),
                        RuntimeValue::Float(f) => float_methods::dispatch(f, method, &args),
                        RuntimeValue::Map(m) => map_methods::dispatch(m, method, &args),
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
                        return exec_fn_body(&body, &params, &args);
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
                            fields,
                        });
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
            Ok(RuntimeValue::List(elements))
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
            Ok(RuntimeValue::Map(map))
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
                    map.get(&key_str).map(|v| *v.clone()).ok_or_else(|| {
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
                    Ok(list[actual].clone())
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
                    fields.get(&key_str).cloned().ok_or_else(|| {
                        VmError::UndefinedVariable(format!(
                            "field '{}' not found on struct",
                            key_str
                        ))
                    })
                }
                other => Err(VmError::TypeError(format!(
                    "subscript requires Map, List, or Struct, got {:?}",
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
                    && let Some(val) = fields.get(&path[1])
                {
                    return Ok(val.clone());
                }
                // Fallback: plain first-segment lookup (legacy behaviour).
                env.get(&path[0])
                    .map_err(|_| VmError::UndefinedVariable(format!("{}.{}", path[0], path[1])))
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
        RuntimeValue::Dice(count, sides) => env
            .roll_dice(*count as u32, *sides as u32)
            .map(RuntimeValue::Int),
        other => Ok(other.clone()),
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
                let segments: Vec<&str> = interp.path.split('.').collect();
                let resolved = match segments.len() {
                    0 => Err(VmError::UndefinedVariable("<empty>".to_string())),
                    1 => env.get(segments[0]),
                    2 => env
                        .get_enum_variant(segments[0], segments[1])
                        // Try module-namespaced key: `inv.gold` → `inv::gold`
                        .or_else(|_| {
                            let ns = crate::ir::namespace(segments[0], segments[1]);
                            env.get(&ns)
                        })
                        // Fall back to the bare second segment: merged globals
                        // from imported modules live in the flat env under their
                        // original name (e.g. `gold`, not `inv::gold`).
                        .or_else(|_| env.get(segments[1])),
                    _ => {
                        // 3+ segments: try `alias::name` for the first two, then
                        // bare last segment, then first segment.
                        let ns = crate::ir::namespace(segments[0], segments[1]);
                        env.get(&ns)
                            .or_else(|_| env.get(segments[segments.len() - 1]))
                            .or_else(|_| env.get(segments[0]))
                    }
                };

                match resolved {
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
    match (val, format) {
        (RuntimeValue::Int(i), Some(fmt)) => {
            if let Some(width_str) = fmt.strip_prefix('0') {
                if let Ok(width) = width_str.parse::<usize>() {
                    return format!("{:0>width$}", i, width = width);
                }
            }
            i.to_string()
        }
        (RuntimeValue::Float(f), Some(fmt)) => {
            if let Some(prec_str) = fmt.strip_prefix('.') {
                if let Ok(prec) = prec_str.parse::<usize>() {
                    return format!("{:.prec$}", f, prec = prec);
                }
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
            RuntimeValue::IdentPath(path) => path.join("."),
            RuntimeValue::Label { name, .. } => name.clone(),
            RuntimeValue::Map(m) => format!("map({})", m.len()),
            RuntimeValue::List(items) => {
                let parts: Vec<String> = items
                    .iter()
                    .map(|v| format_runtime_value(v, None))
                    .collect();
                format!("[{}]", parts.join(", "))
            }
            RuntimeValue::ScriptDecorator { .. } => "<decorator>".to_string(),
            RuntimeValue::Function { params, .. } => {
                format!("fn({})", params.join(", "))
            }
            RuntimeValue::Struct { name, fields } => {
                format!("{}({})", name, fields.len())
            }
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
        Operator::Plus => numeric_binop(lv, rv_val, |a, b| a + b, |a, b| a + b),
        Operator::Minus => numeric_binop(lv, rv_val, |a, b| a - b, |a, b| a - b),
        Operator::Multiply => numeric_binop(lv, rv_val, |a, b| a * b, |a, b| a * b),
        Operator::Divide => numeric_div(lv, rv_val),
        Operator::DoubleSlash => numeric_floordiv(lv, rv_val),
        Operator::Percent => numeric_int_binop(lv, rv_val, |a, b| a % b),
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
        // Handled above via early return.
        Operator::And | Operator::Or | Operator::Assign => unreachable!(),
    }
}

/// Evaluate a unary operation.
pub(super) fn eval_unary(op: &UnaryOperator, val: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match op {
        UnaryOperator::Not => Ok(RuntimeValue::Bool(!is_truthy(&val))),
        UnaryOperator::Negate => match val {
            RuntimeValue::Int(i) => Ok(RuntimeValue::Int(-i)),
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

pub(super) fn to_float(v: &RuntimeValue) -> Option<f64> {
    match v {
        RuntimeValue::Float(f) => Some(*f),
        RuntimeValue::Int(i) => Some(*i as f64),
        _ => None,
    }
}

pub(super) fn numeric_binop(
    lv: RuntimeValue,
    rv: RuntimeValue,
    int_op: impl Fn(i64, i64) -> i64,
    float_op: impl Fn(f64, f64) -> f64,
) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Int(int_op(*a, *b))),
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

pub(super) fn numeric_div(lv: RuntimeValue, rv: RuntimeValue) -> Result<RuntimeValue, VmError> {
    match (&lv, &rv) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
            if *b == 0 {
                return Err(VmError::TypeError("integer division by zero".to_string()));
            }
            Ok(RuntimeValue::Int(a / b))
        }
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Ok(RuntimeValue::Float(a / b)),
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
            Ok(RuntimeValue::Float(a / b))
        }
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
            Ok(RuntimeValue::Int(a.div_euclid(*b)))
        }
        _ => {
            let a = to_float(&lv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", lv)))?;
            let b = to_float(&rv)
                .ok_or_else(|| VmError::TypeError(format!("expected number, got {:?}", rv)))?;
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
                ShiftDirection::Left => *a << shift_u32,
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
        RuntimeValue::List(items) => !items.is_empty(),
        _ => true,
    }
}

/// Returns `true` if two [`RuntimeValue`]s are structurally equal.
pub(super) fn values_equal(a: &RuntimeValue, b: &RuntimeValue) -> bool {
    match (a, b) {
        (RuntimeValue::Null, RuntimeValue::Null) => true,
        (RuntimeValue::Bool(x), RuntimeValue::Bool(y)) => x == y,
        (RuntimeValue::Int(x), RuntimeValue::Int(y)) => x == y,
        (RuntimeValue::Float(x), RuntimeValue::Float(y)) => x == y,
        // Cross-type numeric equality.
        (RuntimeValue::Int(x), RuntimeValue::Float(y)) => (*x as f64) == *y,
        (RuntimeValue::Float(x), RuntimeValue::Int(y)) => *x == (*y as f64),
        (RuntimeValue::Str(x), RuntimeValue::Str(y)) => x.to_string() == y.to_string(),
        (RuntimeValue::Label { node_id: x, .. }, RuntimeValue::Label { node_id: y, .. }) => x == y,
        (RuntimeValue::List(xs), RuntimeValue::List(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| values_equal(a, b))
        }
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

/// Execute a pure function body with the given arguments.
///
/// Creates a **fully isolated** [`Environment`] containing only the bound
/// parameters — no access to outer-scope variables.  This is the purity
/// guarantee: function bodies cannot read or write ambient state.
///
/// Both explicit `return expr` and the implicit last-expression return are
/// supported.
///
/// # Errors
///
/// - [`VmError::TypeError`] on arity mismatch.
/// - Any [`VmError`] produced by evaluating the body.
pub(super) fn exec_fn_body(
    body: &crate::parser::ast::Ast,
    params: &[String],
    args: &[RuntimeValue],
) -> Result<RuntimeValue, VmError> {
    if params.len() != args.len() {
        return Err(VmError::TypeError(format!(
            "function expects {} argument(s), got {}",
            params.len(),
            args.len()
        )));
    }

    // Isolated environment — pure: no globals, no externs, no outer scope.
    let mut fn_env = Environment::new();
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
            RuntimeValue::List(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ])
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
        assert!(!is_truthy(&RuntimeValue::List(vec![])));
        assert!(is_truthy(&RuntimeValue::List(vec![RuntimeValue::Int(1)])));
    }

    #[test]
    fn test_list_values_equal() {
        let a = RuntimeValue::List(vec![RuntimeValue::Int(1), RuntimeValue::Int(2)]);
        let b = RuntimeValue::List(vec![RuntimeValue::Int(1), RuntimeValue::Int(2)]);
        let c = RuntimeValue::List(vec![RuntimeValue::Int(1), RuntimeValue::Int(9)]);
        assert!(values_equal(&a, &b));
        assert!(!values_equal(&a, &c));
        assert!(!values_equal(&a, &RuntimeValue::List(vec![])));
    }

    #[test]
    fn test_list_format() {
        let list = RuntimeValue::List(vec![
            RuntimeValue::Int(1),
            RuntimeValue::Bool(true),
            RuntimeValue::Str(crate::lexer::strings::ParsedString::new_plain("hello")),
        ]);
        let formatted = format_runtime_value(&list, None);
        assert_eq!(formatted, "[1, true, hello]");
    }

    #[test]
    fn test_call_list_len_method() {
        let mut env = Environment::new();
        env.set(
            "xs",
            RuntimeValue::List(vec![
                RuntimeValue::Int(10),
                RuntimeValue::Int(20),
                RuntimeValue::Int(30),
            ]),
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
            RuntimeValue::List(vec![RuntimeValue::Int(1), RuntimeValue::Int(2)]),
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
            RuntimeValue::List(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ])
        );
    }

    #[test]
    fn test_call_list_contains_method() {
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
        let result = exec_fn_body(
            &body,
            &["x".to_string(), "y".to_string()],
            &[RuntimeValue::Int(3), RuntimeValue::Int(4)],
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
        let result = exec_fn_body(&body, &["x".to_string()], &[RuntimeValue::Int(5)]).unwrap();
        assert_eq!(result, RuntimeValue::Int(10));
    }

    #[test]
    fn test_exec_fn_body_wrong_arg_count() {
        // Arity mismatch must produce a TypeError.
        let body = Ast::block(vec![]);
        let err = exec_fn_body(&body, &["x".to_string()], &[]).unwrap_err();
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
        let err = exec_fn_body(&body, &[], &[]).unwrap_err();
        assert!(
            matches!(err, VmError::UndefinedVariable(_)),
            "expected UndefinedVariable, got {err:?}"
        );
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
                assert_eq!(fields.get("x"), Some(&RuntimeValue::Int(3)));
                assert_eq!(fields.get("y"), Some(&RuntimeValue::Int(7)));
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
                fields,
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
                fields,
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
                fields,
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
            fields,
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
}
