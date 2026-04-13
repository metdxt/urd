//! Documentation for Urd's built-in methods, keywords, and terminators.
//!
//! Used by the hover system to display rich documentation for language
//! primitives that don't have user-defined symbols.
//!
//! This module is purely a static documentation database — it has no
//! dependency on the VM or runtime.

// ── Method documentation ─────────────────────────────────────────────────────

/// A single method documentation entry.
struct MethodDoc {
    /// The type this method belongs to (e.g. "str", "list").
    owner: &'static str,
    /// The method name (e.g. "len", "contains").
    name: &'static str,
    /// The full signature line shown in the code block (e.g. "str.len() -> int").
    signature: &'static str,
    /// A one-line description of what the method does.
    description: &'static str,
}

/// Master table of all built-in method documentation.
///
/// Entries are grouped by owner type for readability, but the table is flat
/// for simple linear scanning (the total count is small enough that hashing
/// would be over-engineering).
static METHODS: &[MethodDoc] = &[
    // ── str ──────────────────────────────────────────────────────────────
    MethodDoc {
        owner: "str",
        name: "len",
        signature: "str.len() -> int",
        description: "Returns the number of characters in the string.",
    },
    MethodDoc {
        owner: "str",
        name: "is_empty",
        signature: "str.is_empty() -> bool",
        description: "Returns true if the string has no characters.",
    },
    MethodDoc {
        owner: "str",
        name: "to_upper",
        signature: "str.to_upper() -> str",
        description: "Returns the string in uppercase.",
    },
    MethodDoc {
        owner: "str",
        name: "to_lower",
        signature: "str.to_lower() -> str",
        description: "Returns the string in lowercase.",
    },
    MethodDoc {
        owner: "str",
        name: "trim",
        signature: "str.trim() -> str",
        description: "Removes leading and trailing whitespace.",
    },
    MethodDoc {
        owner: "str",
        name: "trim_start",
        signature: "str.trim_start() -> str",
        description: "Removes leading whitespace.",
    },
    MethodDoc {
        owner: "str",
        name: "trim_end",
        signature: "str.trim_end() -> str",
        description: "Removes trailing whitespace.",
    },
    MethodDoc {
        owner: "str",
        name: "contains",
        signature: "str.contains(substring: str) -> bool",
        description: "Returns true if the string contains the substring.",
    },
    MethodDoc {
        owner: "str",
        name: "starts_with",
        signature: "str.starts_with(prefix: str) -> bool",
        description: "Returns true if the string starts with the prefix.",
    },
    MethodDoc {
        owner: "str",
        name: "ends_with",
        signature: "str.ends_with(suffix: str) -> bool",
        description: "Returns true if the string ends with the suffix.",
    },
    MethodDoc {
        owner: "str",
        name: "split",
        signature: "str.split(separator: str) -> list",
        description: "Splits the string by separator, returns a list of strings.",
    },
    MethodDoc {
        owner: "str",
        name: "replace",
        signature: "str.replace(from: str, to: str) -> str",
        description: "Replaces all occurrences of `from` with `to`.",
    },
    MethodDoc {
        owner: "str",
        name: "slice",
        signature: "str.slice(start: int, end?: int) -> str",
        description: "Returns a substring. Negative indices count from the end.",
    },
    MethodDoc {
        owner: "str",
        name: "to_int",
        signature: "str.to_int() -> int",
        description: "Parses the string as an integer. Errors on invalid input.",
    },
    MethodDoc {
        owner: "str",
        name: "to_float",
        signature: "str.to_float() -> float",
        description: "Parses the string as a float. Errors on invalid input.",
    },
    MethodDoc {
        owner: "str",
        name: "chars",
        signature: "str.chars() -> list",
        description: "Returns a list of single-character strings.",
    },
    MethodDoc {
        owner: "str",
        name: "repeat",
        signature: "str.repeat(n: int) -> str",
        description: "Repeats the string `n` times.",
    },
    MethodDoc {
        owner: "str",
        name: "lines",
        signature: "str.lines() -> list",
        description: "Splits the string on newline characters.",
    },
    // ── int ──────────────────────────────────────────────────────────────
    MethodDoc {
        owner: "int",
        name: "to_string",
        signature: "int.to_string() -> str",
        description: "Converts the integer to its string representation.",
    },
    MethodDoc {
        owner: "int",
        name: "to_float",
        signature: "int.to_float() -> float",
        description: "Converts the integer to a float.",
    },
    MethodDoc {
        owner: "int",
        name: "abs",
        signature: "int.abs() -> int",
        description: "Returns the absolute value.",
    },
    MethodDoc {
        owner: "int",
        name: "min",
        signature: "int.min(other: int) -> int",
        description: "Returns the minimum of self and other.",
    },
    MethodDoc {
        owner: "int",
        name: "max",
        signature: "int.max(other: int) -> int",
        description: "Returns the maximum of self and other.",
    },
    MethodDoc {
        owner: "int",
        name: "clamp",
        signature: "int.clamp(min: int, max: int) -> int",
        description: "Clamps the value to the range [min, max].",
    },
    MethodDoc {
        owner: "int",
        name: "pow",
        signature: "int.pow(exponent: int) -> int",
        description: "Raises to the power. Exponent must be non-negative.",
    },
    MethodDoc {
        owner: "int",
        name: "signum",
        signature: "int.signum() -> int",
        description: "Returns -1, 0, or 1 indicating the sign.",
    },
    // ── float ────────────────────────────────────────────────────────────
    MethodDoc {
        owner: "float",
        name: "to_string",
        signature: "float.to_string() -> str",
        description: "Converts the float to its string representation.",
    },
    MethodDoc {
        owner: "float",
        name: "to_int",
        signature: "float.to_int() -> int",
        description: "Truncates toward zero.",
    },
    MethodDoc {
        owner: "float",
        name: "abs",
        signature: "float.abs() -> float",
        description: "Returns the absolute value.",
    },
    MethodDoc {
        owner: "float",
        name: "floor",
        signature: "float.floor() -> float",
        description: "Rounds down to nearest integer.",
    },
    MethodDoc {
        owner: "float",
        name: "ceil",
        signature: "float.ceil() -> float",
        description: "Rounds up to nearest integer.",
    },
    MethodDoc {
        owner: "float",
        name: "round",
        signature: "float.round() -> float",
        description: "Rounds to nearest integer (half away from zero).",
    },
    MethodDoc {
        owner: "float",
        name: "sqrt",
        signature: "float.sqrt() -> float",
        description: "Square root. Errors on negative values.",
    },
    MethodDoc {
        owner: "float",
        name: "min",
        signature: "float.min(other: float) -> float",
        description: "Returns the minimum of self and other.",
    },
    MethodDoc {
        owner: "float",
        name: "max",
        signature: "float.max(other: float) -> float",
        description: "Returns the maximum of self and other.",
    },
    MethodDoc {
        owner: "float",
        name: "clamp",
        signature: "float.clamp(min: float, max: float) -> float",
        description: "Clamps the value to the range [min, max].",
    },
    MethodDoc {
        owner: "float",
        name: "pow",
        signature: "float.pow(exponent: float) -> float",
        description: "Raises to the power.",
    },
    MethodDoc {
        owner: "float",
        name: "is_nan",
        signature: "float.is_nan() -> bool",
        description: "Returns true if the value is NaN.",
    },
    MethodDoc {
        owner: "float",
        name: "is_finite",
        signature: "float.is_finite() -> bool",
        description: "Returns true if the value is neither NaN nor infinity.",
    },
    MethodDoc {
        owner: "float",
        name: "signum",
        signature: "float.signum() -> float",
        description: "Returns -1.0, 0.0, or 1.0 indicating the sign.",
    },
    // ── list ─────────────────────────────────────────────────────────────
    MethodDoc {
        owner: "list",
        name: "len",
        signature: "list.len() -> int",
        description: "Returns the number of elements.",
    },
    MethodDoc {
        owner: "list",
        name: "get",
        signature: "list.get(index: int) -> any",
        description: "Returns element at index. Negative indices count from end.",
    },
    MethodDoc {
        owner: "list",
        name: "first",
        signature: "list.first() -> any",
        description: "Returns the first element. Errors on empty list.",
    },
    MethodDoc {
        owner: "list",
        name: "last",
        signature: "list.last() -> any",
        description: "Returns the last element. Errors on empty list.",
    },
    MethodDoc {
        owner: "list",
        name: "contains",
        signature: "list.contains(value) -> bool",
        description: "Returns true if the list contains the value.",
    },
    MethodDoc {
        owner: "list",
        name: "append",
        signature: "list.append(value) -> list",
        description: "Returns a new list with value added at the end.",
    },
    MethodDoc {
        owner: "list",
        name: "prepend",
        signature: "list.prepend(value) -> list",
        description: "Returns a new list with value added at the start.",
    },
    MethodDoc {
        owner: "list",
        name: "pop",
        signature: "list.pop() -> list",
        description: "Returns a new list without the last element.",
    },
    MethodDoc {
        owner: "list",
        name: "concat",
        signature: "list.concat(other: list) -> list",
        description: "Returns a new list concatenating both lists.",
    },
    MethodDoc {
        owner: "list",
        name: "reversed",
        signature: "list.reversed() -> list",
        description: "Returns a new list in reverse order.",
    },
    MethodDoc {
        owner: "list",
        name: "with",
        signature: "list.with(index: int, value) -> list",
        description: "Returns a new list with element at index replaced.",
    },
    MethodDoc {
        owner: "list",
        name: "slice",
        signature: "list.slice(start: int, end?: int) -> list",
        description: "Returns a sublist. Negative indices count from the end.",
    },
    MethodDoc {
        owner: "list",
        name: "join",
        signature: "list.join(separator?: str) -> str",
        description: "Joins elements into a string, optionally separated by the given separator.",
    },
    MethodDoc {
        owner: "list",
        name: "map",
        signature: "list.map(fn) -> list",
        description: "Applies function to each element, returns new list.",
    },
    MethodDoc {
        owner: "list",
        name: "filter",
        signature: "list.filter(fn) -> list",
        description: "Keeps elements where predicate returns true.",
    },
    MethodDoc {
        owner: "list",
        name: "reduce",
        signature: "list.reduce(initial, fn) -> any",
        description: "Reduces the list with an accumulator function.",
    },
    MethodDoc {
        owner: "list",
        name: "fold",
        signature: "list.fold(initial, fn) -> any",
        description: "Alias for reduce. Reduces the list with an accumulator function.",
    },
    MethodDoc {
        owner: "list",
        name: "find",
        signature: "list.find(fn) -> any",
        description: "Returns the first element matching the predicate, or null.",
    },
    MethodDoc {
        owner: "list",
        name: "any",
        signature: "list.any(fn) -> bool",
        description: "Returns true if any element matches the predicate.",
    },
    MethodDoc {
        owner: "list",
        name: "all",
        signature: "list.all(fn) -> bool",
        description: "Returns true if all elements match the predicate.",
    },
    MethodDoc {
        owner: "list",
        name: "sort_by",
        signature: "list.sort_by(fn) -> list",
        description: "Returns a new sorted list using the comparator function.",
    },
    MethodDoc {
        owner: "list",
        name: "zip",
        signature: "list.zip(other: list) -> list",
        description: "Zips two lists into a list of pairs.",
    },
    MethodDoc {
        owner: "list",
        name: "min",
        signature: "list.min() -> any",
        description: "Returns the minimum element.",
    },
    MethodDoc {
        owner: "list",
        name: "max",
        signature: "list.max() -> any",
        description: "Returns the maximum element.",
    },
    MethodDoc {
        owner: "list",
        name: "sum",
        signature: "list.sum() -> int | float",
        description: "Returns the sum of all elements.",
    },
    // ── map ──────────────────────────────────────────────────────────────
    MethodDoc {
        owner: "map",
        name: "get",
        signature: "map.get(key: str) -> any",
        description: "Returns the value for key, or null if missing.",
    },
    MethodDoc {
        owner: "map",
        name: "has",
        signature: "map.has(key: str) -> bool",
        description: "Returns true if the key exists.",
    },
    MethodDoc {
        owner: "map",
        name: "keys",
        signature: "map.keys() -> list",
        description: "Returns a sorted list of keys.",
    },
    MethodDoc {
        owner: "map",
        name: "values",
        signature: "map.values() -> list",
        description: "Returns a list of values (sorted by key).",
    },
    MethodDoc {
        owner: "map",
        name: "len",
        signature: "map.len() -> int",
        description: "Returns the number of entries.",
    },
    MethodDoc {
        owner: "map",
        name: "is_empty",
        signature: "map.is_empty() -> bool",
        description: "Returns true if the map has no entries.",
    },
    MethodDoc {
        owner: "map",
        name: "set",
        signature: "map.set(key: str, value) -> map",
        description: "Returns a new map with the key set.",
    },
    MethodDoc {
        owner: "map",
        name: "remove",
        signature: "map.remove(key: str) -> map",
        description: "Returns a new map with the key removed.",
    },
    MethodDoc {
        owner: "map",
        name: "merge",
        signature: "map.merge(other: map) -> map",
        description: "Returns a new map merging both. `other` wins on conflicts.",
    },
    // ── range ────────────────────────────────────────────────────────────
    MethodDoc {
        owner: "range",
        name: "len",
        signature: "range.len() -> int",
        description: "Returns the number of integers in the range.",
    },
    MethodDoc {
        owner: "range",
        name: "contains",
        signature: "range.contains(n: int) -> bool",
        description: "Returns true if n is within the range.",
    },
];

/// Returns hover documentation for a built-in method, or `None` if the
/// method name is not a known built-in for any type.
///
/// `receiver_type` is an optional hint (e.g. `"list"`, `"str"`) to narrow
/// the result.  When `None`, the method name is matched across all types
/// and all matching overloads are shown, separated by `---`.
pub fn method_doc(method: &str, receiver_type: Option<&str>) -> Option<String> {
    let matches: Vec<&MethodDoc> = METHODS
        .iter()
        .filter(|m| m.name == method && receiver_type.is_none_or(|rt| m.owner == rt))
        .collect();

    if matches.is_empty() {
        return None;
    }

    let mut parts: Vec<String> = Vec::with_capacity(matches.len());
    for m in &matches {
        parts.push(format!("```urd\n{}\n```\n\n{}", m.signature, m.description,));
    }

    Some(parts.join("\n\n---\n\n"))
}

// ── Keyword documentation ────────────────────────────────────────────────────

/// A single keyword documentation entry.
struct KeywordDoc {
    /// The keyword as it appears in source (e.g. "label", "end!").
    keyword: &'static str,
    /// Short heading shown in bold.
    heading: &'static str,
    /// Longer description (may contain markdown).
    description: &'static str,
}

static KEYWORD_DOCS: &[KeywordDoc] = &[
    KeywordDoc {
        keyword: "label",
        heading: "label",
        description: "Defines a named block that can be jumped to with `jump`.\n\n\
                      Labels are the primary unit of narrative flow in Urd.",
    },
    KeywordDoc {
        keyword: "jump",
        heading: "jump",
        description: "Transfers control to a label.\n\n\
                      `jump label_name` moves execution to the target label.\n\
                      `jump label_name and return` calls it as a subroutine, \
                      resuming after the jump when the target returns.",
    },
    KeywordDoc {
        keyword: "return",
        heading: "return",
        description: "Returns a value from a label (subroutine) or function.\n\n\
                      Inside a label reached via `jump … and return`, \
                      `return` resumes execution at the call site.\n\
                      Inside a `fn`, returns the function's result.",
    },
    KeywordDoc {
        keyword: "menu",
        heading: "menu",
        description: "Presents a choice to the player.\n\n\
                      Each option is prefixed with `>` and may have an optional \
                      `if` guard to control visibility.",
    },
    KeywordDoc {
        keyword: "match",
        heading: "match",
        description: "Pattern-matches a value against one or more arms.\n\n\
                      Each arm is `| pattern => body`. Use `_` as a wildcard.",
    },
    KeywordDoc {
        keyword: "if",
        heading: "if",
        description: "Conditional branching.\n\n\
                      `if condition { … }` executes the block when the condition is truthy. \
                      Can be followed by `elif` and `else` branches.",
    },
    KeywordDoc {
        keyword: "elif",
        heading: "elif",
        description: "Additional conditional branch after `if`.\n\n\
                      `elif condition { … }` is checked when all preceding \
                      `if`/`elif` conditions were falsy.",
    },
    KeywordDoc {
        keyword: "else",
        heading: "else",
        description: "Fallback branch of an `if`/`elif` chain.\n\n\
                      `else { … }` executes when all preceding conditions were falsy.",
    },
    KeywordDoc {
        keyword: "let",
        heading: "let",
        description: "Declares a mutable local variable.\n\n\
                      `let name = value` — the variable is scoped to the enclosing block.",
    },
    KeywordDoc {
        keyword: "const",
        heading: "const",
        description: "Declares an immutable constant.\n\n\
                      `const name = value` — the value cannot be reassigned.",
    },
    KeywordDoc {
        keyword: "global",
        heading: "global",
        description: "Declares a global variable that persists across jumps.\n\n\
                      `global name = value` — the variable is accessible from any label \
                      and its value is preserved in save data.",
    },
    KeywordDoc {
        keyword: "extern",
        heading: "extern",
        description: "Declares a variable provided by the host application.\n\n\
                      `extern const name: type` — the value is injected at runtime \
                      and cannot be modified by the script.",
    },
    KeywordDoc {
        keyword: "fn",
        heading: "fn",
        description: "Defines a pure function.\n\n\
                      `fn name(params) = expression` — functions are expressions, \
                      have no side effects, and can be passed as values.",
    },
    KeywordDoc {
        keyword: "enum",
        heading: "enum",
        description: "Defines an enumeration type.\n\n\
                      `enum Name { Variant1, Variant2 }` — enum variants are accessed \
                      as `Name.Variant`.",
    },
    KeywordDoc {
        keyword: "struct",
        heading: "struct",
        description: "Defines a struct type.\n\n\
                      `struct Name { field1: type, field2: type }` — instances are \
                      created with `Name { field1: value, … }`.",
    },
    KeywordDoc {
        keyword: "decorator",
        heading: "decorator",
        description: "Defines a script decorator.\n\n\
                      Decorators annotate labels with metadata and can wrap \
                      their execution behaviour.",
    },
    KeywordDoc {
        keyword: "import",
        heading: "import",
        description: "Imports symbols from another file.\n\n\
                      `import (name) from \"file.urd\"` — imported symbols become \
                      available in the current scope.",
    },
    KeywordDoc {
        keyword: "in",
        heading: "in",
        description: "Membership test operator.\n\n\
                      `value in collection` — works with ranges, lists, maps, and strings.",
    },
    KeywordDoc {
        keyword: "and",
        heading: "and",
        description: "Logical AND operator.\n\n\
                      `a and b` — returns `b` if `a` is truthy, otherwise `a`. Short-circuits.",
    },
    KeywordDoc {
        keyword: "or",
        heading: "or",
        description: "Logical OR operator.\n\n\
                      `a or b` — returns `a` if `a` is truthy, otherwise `b`. Short-circuits.",
    },
    KeywordDoc {
        keyword: "not",
        heading: "not",
        description: "Logical NOT operator.\n\n\
                      `not a` — returns `true` if `a` is falsy, `false` otherwise.",
    },
    // Terminators — matched by the bare word without `()`.
    KeywordDoc {
        keyword: "end",
        heading: "end!()",
        description: "Terminates the VM immediately.\n\n\
                      Signals that the script has reached a terminal point. \
                      The host application receives an end-of-script event.",
    },
    KeywordDoc {
        keyword: "todo",
        heading: "todo!()",
        description: "Marks unfinished code.\n\n\
                      Terminates with an error at runtime, indicating that this \
                      code path has not been implemented yet.",
    },
];

/// Returns hover documentation for a language keyword or terminator, or
/// `None` if the word is not a known keyword.
pub fn keyword_doc(keyword: &str) -> Option<String> {
    // Normalize: `end!` and `end` both match the `end` entry, etc.
    let normalized = keyword.strip_suffix('!').unwrap_or(keyword);

    KEYWORD_DOCS
        .iter()
        .find(|k| k.keyword == normalized)
        .map(|k| format!("**{}**\n\n{}", k.heading, k.description))
}

// ── Built-in decorator documentation ─────────────────────────────────────────

/// A single built-in decorator documentation entry.
struct DecoratorDoc {
    /// The decorator name without `@` (e.g. "entry", "fluent").
    name: &'static str,
    /// The signature shown in the code block.
    signature: &'static str,
    /// A description of what the decorator does.
    description: &'static str,
}

static DECORATOR_DOCS: &[DecoratorDoc] = &[
    DecoratorDoc {
        name: "entry",
        signature: "@entry",
        description: "Marks a label as a VM entry point.\n\n\
                      The VM begins execution at the first `@entry` label by default. \
                      Use `Vm::new_at(graph, registry, \"label_name\")` to start at a \
                      specific entry point.\n\n\
                      If no label has `@entry`, the first label in source order is used.\n\n\
                      In multi-file projects, only `@entry` labels are valid cross-module \
                      jump targets.\n\n\
                      Only valid on `label` statements.",
    },
    DecoratorDoc {
        name: "fluent",
        signature: "@fluent / @fluent(\"alias\")",
        description: "Marks a variable for localisation via Project Fluent.\n\n\
                      `@fluent` — uses the variable name as the Fluent message key.\n\
                      `@fluent(\"alias\")` — uses the given string as the key instead.\n\n\
                      The Urd compiler generates `.ftl` files from `@fluent`-annotated \
                      variables, and the `Localizer` trait resolves them at runtime.\n\n\
                      Only valid on `let`, `const`, `global`, and `extern` declarations.",
    },
    DecoratorDoc {
        name: "id",
        signature: "@id(\"custom-segment\")",
        description: "Overrides the auto-generated localisation ID segment.\n\n\
                      By default, each dialogue line and menu option receives a \
                      sequential ID based on its position within the label. \
                      `@id(\"custom\")` replaces the auto-generated segment with \
                      the given string, making IDs stable across reordering.\n\n\
                      Valid on `label`, `menu`, `match`, `if`, and dialogue statements.",
    },
    DecoratorDoc {
        name: "lint",
        signature: "@lint(check_loops)",
        description: "Opts a label into additional static analysis checks.\n\n\
                      `@lint(check_loops)` — enables infinite-loop detection for \
                      this label. The analysis pass traces all jump paths from \
                      the label and reports an error if every path eventually \
                      loops back without an escape (return, end, or menu).\n\n\
                      Only valid on `label` statements.",
    },
];

/// Returns hover documentation for a built-in decorator, or `None` if the
/// name is not a known built-in decorator.
///
/// The `name` should be the decorator name without the `@` prefix.
pub fn decorator_doc(name: &str) -> Option<String> {
    DECORATOR_DOCS
        .iter()
        .find(|d| d.name == name)
        .map(|d| format!("```urd\n{}\n```\n\n{}", d.signature, d.description))
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    // ── decorator_doc ────────────────────────────────────────────────────

    #[test]
    fn decorator_doc_entry_returns_some() {
        let doc = decorator_doc("entry");
        assert!(doc.is_some(), "expected Some for @entry");
        let doc = doc.unwrap();
        assert!(doc.contains("@entry"), "must contain @entry: {doc}");
        assert!(doc.contains("entry point"), "must describe purpose: {doc}");
    }

    #[test]
    fn decorator_doc_fluent_returns_some() {
        let doc = decorator_doc("fluent");
        assert!(doc.is_some(), "expected Some for @fluent");
        let doc = doc.unwrap();
        assert!(doc.contains("@fluent"), "must contain @fluent: {doc}");
        assert!(
            doc.contains("localisation") || doc.contains("localization"),
            "must mention localisation: {doc}"
        );
    }

    #[test]
    fn decorator_doc_id_returns_some() {
        let doc = decorator_doc("id");
        assert!(doc.is_some(), "expected Some for @id");
        let doc = doc.unwrap();
        assert!(doc.contains("@id"), "must contain @id: {doc}");
    }

    #[test]
    fn decorator_doc_lint_returns_some() {
        let doc = decorator_doc("lint");
        assert!(doc.is_some(), "expected Some for @lint");
        let doc = doc.unwrap();
        assert!(
            doc.contains("check_loops"),
            "must mention check_loops: {doc}"
        );
    }

    #[test]
    fn decorator_doc_unknown_returns_none() {
        assert!(decorator_doc("nonexistent").is_none());
        assert!(decorator_doc("scene").is_none());
    }

    #[test]
    fn decorator_doc_all_entries_have_content() {
        for d in DECORATOR_DOCS {
            assert!(!d.name.is_empty(), "decorator name must not be empty");
            assert!(
                !d.signature.is_empty(),
                "signature must not be empty for {}",
                d.name
            );
            assert!(
                !d.description.is_empty(),
                "description must not be empty for {}",
                d.name
            );
            let doc = decorator_doc(d.name);
            assert!(
                doc.is_some(),
                "decorator_doc must return Some for {}",
                d.name
            );
        }
    }

    // ── method_doc ───────────────────────────────────────────────────────

    #[test]
    fn method_doc_known_method_returns_some() {
        let doc = method_doc("len", None);
        assert!(doc.is_some(), "expected Some for 'len'");
        let text = doc.unwrap();
        // Should contain at least one code block with a signature.
        assert!(
            text.contains("```urd"),
            "expected urd code block, got: {text}"
        );
        assert!(text.contains("len()"), "expected signature, got: {text}");
    }

    #[test]
    fn method_doc_unknown_method_returns_none() {
        assert!(method_doc("nonexistent", None).is_none());
        assert!(method_doc("", None).is_none());
        assert!(method_doc("foobar", Some("str")).is_none());
    }

    #[test]
    fn method_doc_ambiguous_name_shows_all_overloads() {
        // `len` exists on str, list, map, and range.
        let doc = method_doc("len", None).unwrap();
        assert!(doc.contains("str.len"), "should contain str.len: {doc}");
        assert!(doc.contains("list.len"), "should contain list.len: {doc}");
        assert!(doc.contains("map.len"), "should contain map.len: {doc}");
        assert!(doc.contains("range.len"), "should contain range.len: {doc}");
        // Overloads are separated by horizontal rules.
        assert!(
            doc.contains("---"),
            "overloads should be separated by ---: {doc}"
        );
    }

    #[test]
    fn method_doc_filtered_by_receiver_type() {
        let doc = method_doc("len", Some("str")).unwrap();
        assert!(doc.contains("str.len"), "should contain str.len: {doc}");
        assert!(
            !doc.contains("list.len"),
            "should NOT contain list.len: {doc}"
        );
    }

    #[test]
    fn method_doc_contains_shows_multiple_types() {
        // `contains` exists on str, list, and range.
        let doc = method_doc("contains", None).unwrap();
        assert!(doc.contains("str.contains"), "missing str.contains: {doc}");
        assert!(
            doc.contains("list.contains"),
            "missing list.contains: {doc}"
        );
        assert!(
            doc.contains("range.contains"),
            "missing range.contains: {doc}"
        );
    }

    #[test]
    fn method_doc_unique_method_single_entry() {
        let doc = method_doc("trim", None).unwrap();
        // Only str has `trim`, so no separator.
        assert!(!doc.contains("---"), "should be a single entry: {doc}");
        assert!(doc.contains("str.trim"), "should show str.trim: {doc}");
    }

    #[test]
    fn method_doc_includes_description() {
        let doc = method_doc("append", None).unwrap();
        assert!(
            doc.contains("Returns a new list with value added at the end"),
            "should include description: {doc}"
        );
    }

    // ── keyword_doc ─────────────────────────────────────────────────────

    #[test]
    fn keyword_doc_known_keyword_returns_some() {
        let doc = keyword_doc("label");
        assert!(doc.is_some(), "expected Some for 'label'");
        let text = doc.unwrap();
        assert!(
            text.contains("**label**"),
            "should contain bold heading: {text}"
        );
        assert!(
            text.contains("Defines a named block"),
            "should contain description: {text}"
        );
    }

    #[test]
    fn keyword_doc_unknown_keyword_returns_none() {
        assert!(keyword_doc("notakeyword").is_none());
        assert!(keyword_doc("").is_none());
        assert!(keyword_doc("foobar").is_none());
    }

    #[test]
    fn keyword_doc_all_keywords_covered() {
        let keywords = [
            "label",
            "jump",
            "return",
            "menu",
            "match",
            "if",
            "elif",
            "else",
            "let",
            "const",
            "global",
            "extern",
            "fn",
            "enum",
            "struct",
            "decorator",
            "import",
            "in",
            "and",
            "or",
            "not",
            "end",
            "todo",
        ];
        for kw in keywords {
            assert!(
                keyword_doc(kw).is_some(),
                "keyword_doc({kw:?}) should return Some"
            );
        }
    }

    #[test]
    fn keyword_doc_end_bang_normalizes() {
        // `end!` should match the `end` entry.
        let doc = keyword_doc("end!").unwrap();
        assert!(doc.contains("end!()"), "should show end!(): {doc}");
    }

    #[test]
    fn keyword_doc_todo_bang_normalizes() {
        let doc = keyword_doc("todo!").unwrap();
        assert!(doc.contains("todo!()"), "should show todo!(): {doc}");
    }

    #[test]
    fn keyword_doc_if_mentions_elif_else() {
        let doc = keyword_doc("if").unwrap();
        assert!(doc.contains("elif"), "if doc should mention elif: {doc}");
        assert!(doc.contains("else"), "if doc should mention else: {doc}");
    }

    #[test]
    fn keyword_doc_jump_mentions_return() {
        let doc = keyword_doc("jump").unwrap();
        assert!(
            doc.contains("and return"),
            "jump doc should mention 'and return': {doc}"
        );
    }

    // ── Method table integrity ──────────────────────────────────────────

    #[test]
    fn all_method_signatures_start_with_owner() {
        for m in METHODS {
            assert!(
                m.signature.starts_with(m.owner),
                "signature {:?} should start with owner {:?}",
                m.signature,
                m.owner,
            );
        }
    }

    #[test]
    fn all_method_signatures_contain_method_name() {
        for m in METHODS {
            let expected = format!(".{}(", m.name);
            assert!(
                m.signature.contains(&expected),
                "signature {:?} should contain {:?}",
                m.signature,
                expected,
            );
        }
    }

    #[test]
    fn no_empty_descriptions() {
        for m in METHODS {
            assert!(
                !m.description.is_empty(),
                "method {}.{} has empty description",
                m.owner,
                m.name,
            );
        }
        for k in KEYWORD_DOCS {
            assert!(
                !k.description.is_empty(),
                "keyword {:?} has empty description",
                k.keyword,
            );
        }
    }
}
