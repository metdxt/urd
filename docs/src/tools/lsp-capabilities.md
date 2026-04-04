# LSP Capabilities

`urd-lsp` implements a broad set of Language Server Protocol features, giving you a rich editing experience in any LSP-compatible editor.

---

## Hover

Hovering over a symbol displays contextual information:

- **Variables**: type annotation (if present), declaration kind (`let`, `const`, `global`, `extern`)
- **Labels**: the label name and any decorators applied to it
- **Struct/Enum references**: type definition summary
- **Localization key badge**: when the cursor is on a dialogue line or menu option, a 🔑 badge shows the generated `loc_id`

The localization key badge is especially useful during translation work — hover over any line to see exactly which Fluent message ID it maps to.

---

## Completion

Context-aware completions are triggered automatically as you type. The trigger characters are:

| Character | Context |
|-----------|---------|
| `.`       | Struct field access, enum variant access |
| `:`       | Type annotations, speaker syntax |
| ` ` (space) | After keywords like `jump`, `let`, `global` |
| `@`       | Decorator names |

Completions include:

- **Labels** — all labels in the current file (for `jump` targets)
- **Decorators** — built-in decorators like `@entry`, `@fluent`, `@id`, `@lint`
- **Variables** — all variables visible in the current scope
- **Struct fields** — when typing after a `.` on a struct-typed variable
- **Enum variants** — when typing after a `.` on an enum type
- **Imported symbols** — symbols brought in via `import` statements, including cross-file struct and enum definitions
- **Keywords** — language keywords appropriate to the current context

Each completion item includes a detail string describing the symbol kind (e.g., "label", "global variable", "enum variant").

---

## Go to Definition

**Shortcut**: Usually `gd` or `F12` depending on your editor.

Jump to the definition of:

- Labels (from `jump` statements)
- Variables (from usage sites back to `let`, `const`, `global`, or `extern` declarations)
- Cross-file symbols (via the `WorkspaceIndex` — follows `import` statements to the defining file)

For qualified references like `alias.name`, the LSP resolves through the workspace index to find the original definition in the imported module.

---

## Find References

**Shortcut**: Usually `gr` or `Shift+F12` depending on your editor.

Find all usages of a symbol across:

- The current file (local references)
- All files in the workspace (cross-file references via the `WorkspaceIndex`)

This works for labels, variables, constants, globals, and imported symbols.

---

## Rename

**Shortcut**: Usually `F2` depending on your editor.

Rename a symbol across all its usages in the current file. The LSP implements both `prepareRename` (to validate the rename location and show the current name) and `rename` (to apply the change).

Rename works for all known symbol types: variables, constants, globals, and labels.

---

## Document Symbols

The outline/symbol view in your editor is populated with:

| Symbol Kind | LSP Kind |
|-------------|----------|
| Labels | Namespace |
| Enums | Enum |
| Structs | Struct |
| Decorators | Function |
| Constants | Constant |
| Globals | Variable |
| Extern declarations | Variable |

This powers features like the breadcrumb bar, outline panel, and `Go to Symbol in File` commands.

---

## Semantic Tokens

`urd-lsp` provides full semantic token highlighting with 11 token types:

| Index | Token Type | Used For |
|-------|-----------|----------|
| 0 | `keyword` | `label`, `jump`, `menu`, `if`, `match`, `let`, `const`, `global`, `extern`, `import`, `enum`, `struct`, `end!`, `todo!`, `true`, `false`, `null` |
| 1 | `namespace` | Label names |
| 2 | `variable` | Variable and identifier references |
| 3 | `string` | String literals |
| 4 | `number` | Integer and float literals, dice expressions |
| 5 | `operator` | `+`, `-`, `==`, `!=`, `>=`, `and`, `or`, `not`, etc. |
| 6 | `enumMember` | Enum variant references |
| 7 | `struct` | Struct type references |
| 8 | `decorator` | `@entry`, `@fluent`, `@id`, `@lint`, and custom decorators |
| 9 | `function` | Function calls like `end!()`, `todo!()` |
| 10 | `property` | Struct field names in access expressions |

Semantic tokens provide more accurate highlighting than tree-sitter alone, because they are computed from the fully parsed and analyzed AST.

---

## Code Actions

Code actions appear as quick fixes (usually via `Ctrl+.` or the lightbulb icon):

### Create Label

When you reference a label that doesn't exist (e.g., `jump new_scene`), the LSP offers:

> **Create label 'new_scene'**

Accepting this action appends a new label block at the end of the file:

```urd
label new_scene {
    todo!()
}
```

### Spellcheck Replacements

When the `spellcheck` feature is enabled and a misspelled word is detected:

> **Replace with 'correct_word'**

The replacement preserves the casing of the original word (lowercase, Title Case, or ALL CAPS).

### Add to Dictionary

For words that are intentionally non-standard (character names, invented terms):

> **Add 'word' to dictionary**

This appends the word to the `.urd-dict` file in the workspace root and immediately re-runs spellcheck on all open documents.

---

## Diagnostics

All analysis passes run on every edit, providing real-time feedback. See [Diagnostics & Lints](./lsp-diagnostics.md) for the full list of diagnostics.

Diagnostics are published after every document change (full text sync) and cover parse errors, static analysis warnings, and optionally spelling errors.

---

## Execute Command

The LSP registers one server-side command (when `spellcheck` is enabled):

- **`urd.addToDictionary`** — adds a word to the `.urd-dict` user dictionary and re-runs spellcheck on all open documents

This command is invoked automatically when you accept the "Add to dictionary" code action.