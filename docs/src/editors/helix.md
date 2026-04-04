# Helix

[Helix](https://helix-editor.com/) has native tree-sitter support and can be configured to use the Urd grammar and language server with a few additions to your `languages.toml`.

---

## Prerequisites

Before configuring Helix, make sure you have:

1. **`urd-lsp`** installed and available on your `$PATH`:

   ```bash
   cargo install --path crates/urd-lsp
   ```

2. **`tree-sitter-urd`** grammar source available (cloned as part of the Urd repository).

---

## Grammar Setup

Add the Urd grammar to your Helix `languages.toml`. This file is typically located at `~/.config/helix/languages.toml`.

```toml
[[language]]
name = "urd"
scope = "source.urd"
injection-regex = "urd"
file-types = ["urd"]
comment-token = "#"
indent = { tab-width = 4, unit = "    " }
language-servers = ["urd-lsp"]
roots = []

[[grammar]]
name = "urd"
source = { git = "https://github.com/metdxt/urd", subpath = "tree-sitter-urd", rev = "main" }
```

After editing `languages.toml`, fetch and build the grammar:

```bash
helix --grammar fetch
helix --grammar build
```

---

## Query Files

Helix needs tree-sitter query files to provide syntax highlighting, indentation, and local variable tracking. Copy the query files from the `tree-sitter-urd` repository into the Helix runtime directory.

The Helix runtime queries directory is typically at:

- `~/.config/helix/runtime/queries/urd/`

Copy the queries:

```bash
mkdir -p ~/.config/helix/runtime/queries/urd

cp tree-sitter-urd/queries/highlights.scm ~/.config/helix/runtime/queries/urd/
cp tree-sitter-urd/queries/indents.scm    ~/.config/helix/runtime/queries/urd/
cp tree-sitter-urd/queries/locals.scm     ~/.config/helix/runtime/queries/urd/
```

The available query files are:

| File | Purpose |
|---|---|
| `highlights.scm` | Syntax highlighting — maps tree-sitter nodes to highlight scopes |
| `indents.scm` | Auto-indentation rules based on block structure |
| `locals.scm` | Local variable scoping for improved highlighting accuracy |

---

## Language Server Configuration

Add the `urd-lsp` language server definition to your `languages.toml`:

```toml
[language-server.urd-lsp]
command = "urd-lsp"
```

If you installed `urd-lsp` with the `spellcheck` feature (the default), you can pass initialization options to configure spellcheck behavior:

```toml
[language-server.urd-lsp]
command = "urd-lsp"

[language-server.urd-lsp.config]
spellcheckLanguage = "en"
```

---

## Full Example Configuration

Here is a complete `~/.config/helix/languages.toml` for Urd support:

```toml
# ── Urd language server ──────────────────────────────────────────────

[language-server.urd-lsp]
command = "urd-lsp"

# ── Urd language definition ──────────────────────────────────────────

[[language]]
name = "urd"
scope = "source.urd"
injection-regex = "urd"
file-types = ["urd"]
comment-token = "#"
indent = { tab-width = 4, unit = "    " }
language-servers = ["urd-lsp"]
roots = []

# ── Urd grammar source ──────────────────────────────────────────────

[[grammar]]
name = "urd"
source = { git = "https://github.com/metdxt/urd", subpath = "tree-sitter-urd", rev = "main" }
```

---

## Verifying the Setup

1. Open a `.urd` file in Helix
2. Check that syntax highlighting is working (keywords like `label`, `menu`, `jump` should be colored)
3. Verify the LSP is connected by running `:lsp-workspace-command` from command mode
4. Hover over a variable or label name — you should see type information and localization key badges

---

## Troubleshooting

**No syntax highlighting:**
- Make sure you ran `helix --grammar fetch && helix --grammar build` after adding the grammar
- Verify that the query files exist in `~/.config/helix/runtime/queries/urd/`
- Check `:log-open` for grammar loading errors

**LSP not connecting:**
- Confirm `urd-lsp` is on your `$PATH`: run `which urd-lsp` in your terminal
- Check the Helix log (`:log-open`) for language server errors
- Ensure the `language-servers = ["urd-lsp"]` line is present in the `[[language]]` block

**Spellcheck diagnostics not appearing:**
- Spellcheck runs on save, not on every keystroke — save the file first
- Verify `urd-lsp` was compiled with the `spellcheck` feature (it is enabled by default)