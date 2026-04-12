//! Fluent variable collection, string interpolation helpers, and choice event
//! builder.
//!
//! These free functions are used by the main VM loop (`mod.rs`) to prepare
//! localisation data and construct [`Event::Choice`] values.

use std::collections::HashMap;
use std::sync::Arc;

use crate::ir::{ChoiceEvent, Event, IrChoiceOption, VmStep};
use crate::loc::Localizer;
use crate::parser::ast::{AstContent, Decorator, EventConstraint};
use crate::runtime::value::RuntimeValue;

use super::decorators::apply_decorator;
use super::env::Environment;
use super::registry::DecoratorRegistry;
use super::eval;

// ─── Fluent variable helpers ──────────────────────────────────────────────────

/// Collect Fluent variable bindings from the current environment.
///
/// Merges two sources:
/// 1. `@fluent`-tagged bindings accumulated in the environment's fluent scope.
/// 2. Variable names referenced via string interpolation in `lines_ast` —
///    resolved against the environment and added if not already present.
///
/// The result is ready to pass to a [`Localizer`].
pub(super) fn collect_fluent_vars(
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
pub(super) fn collect_interpolations(
    ast: &crate::parser::ast::Ast,
) -> Vec<(String, Option<String>)> {
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
pub(super) fn extract_label_interp_vars(label: &str) -> Vec<(String, Option<String>)> {
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
pub(super) fn build_choice_event(
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
