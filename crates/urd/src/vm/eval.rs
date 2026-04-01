//! Pure expression evaluator: converts AST expressions to [`RuntimeValue`]s.

use std::collections::HashMap;

use crate::lexer::strings::{Interpolation, ParsedString, StringPart};
use crate::parser::ast::{AstContent, Operator, UnaryOperator};
use crate::runtime::value::RuntimeValue;

use super::VmError;
use super::env::Environment;

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

        // ── Function call — not yet implemented ──────────────────────────────
        AstContent::Call { func_path, params } => {
            // Evaluate arguments so their side-effects (if any) still run.
            if let AstContent::ExprList(args) = params.content() {
                for arg in args {
                    let _ = eval_expr(arg, env)?;
                }
            }
            let path_str = match func_path.content() {
                AstContent::Value(RuntimeValue::IdentPath(p)) => p.join("."),
                _ => "<unknown>".to_string(),
            };
            log::warn!(
                "function call to '{}' is not yet implemented; returning Null",
                path_str
            );
            Ok(RuntimeValue::Null)
        }

        // ── Collection literals — not yet supported ──────────────────────────
        AstContent::List(_) => {
            log::warn!("List literals are not yet supported; returning Null");
            Ok(RuntimeValue::Null)
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
        AstContent::Subscript { object, key } => {
            let obj_val = eval_expr(object, env)?;
            let key_val = eval_expr(key, env)?;
            let key_str = match &key_val {
                RuntimeValue::Str(ps) => ps.to_string(),
                RuntimeValue::Int(i) => i.to_string(),
                other => {
                    return Err(VmError::TypeError(format!(
                        "subscript key must be Str or Int, got {:?}",
                        other
                    )));
                }
            };
            match obj_val {
                RuntimeValue::Map(map) => map.get(&key_str).map(|v| *v.clone()).ok_or_else(|| {
                    VmError::UndefinedVariable(format!("key '{}' not found in map", key_str))
                }),
                other => Err(VmError::TypeError(format!(
                    "subscript requires Map, got {:?}",
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
        RuntimeValue::Dice(count, sides) => Err(VmError::NotImplemented(format!(
            "dice evaluation ({}d{}) — wire up a dice roller via DecoratorRegistry or a custom evaluator",
            count, sides
        ))),
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
            RuntimeValue::ScriptDecorator { .. } => "<decorator>".to_string(),
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
        _ => false,
    }
}
