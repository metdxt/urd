# Zed

[Zed](https://zed.dev) has first-class support for Urd via the **urd** extension, which provides syntax highlighting, code intelligence, and full LSP integration.

## Installation

### Via the Extension Marketplace

1. Open the command palette (`Cmd+Shift+P` on macOS / `Ctrl+Shift+P` on Linux)
2. Run **zed: extensions**
3. Search for **Urd** and click **Install**

### Local / Development Install

If you want to hack on the extension itself or test unreleased changes:

1. Clone the extension repository:

   ```bash
   git clone https://github.com/metdxt/urd-zed
   ```

2. Open Zed and run the command **zed: install dev extension** from the command palette
3. Point the file picker at the `urd-zed` directory
4. Zed will load the extension from disk and reload it automatically when files change

## Features

The extension delivers the full editing experience for `.urd` files:

- **Syntax highlighting** — keywords, labels, strings, numbers, enums, decorators, comments, and more via a tree-sitter grammar
- **Auto-indentation** — smart indentation rules based on block structure (`label {}`, `menu {}`, `if {}`, etc.)
- **Bracket matching & auto-close** — automatic closing and matching of `()`, `[]`, `{}`, and string delimiters
- **Code outline** — navigate labels, enums, and decorators from Zed's outline panel
- **Full LSP integration** — hover, completion, go-to-definition, find references, rename, diagnostics, semantic tokens, code actions, and spellcheck — all powered by `urd-lsp`

## Language Server

The extension automatically downloads pre-built `urd-lsp` binaries from [metdxt/urd releases](https://github.com/metdxt/urd/releases) on GitHub.

| Platform | Architecture | Status |
|----------|-------------|--------|
| Linux    | x86_64      | ✅ Pre-built binary |
| Linux    | aarch64     | ✅ Pre-built binary |
| Windows  | x86_64      | ✅ Pre-built binary |
| Windows  | aarch64     | ✅ Pre-built binary |
| macOS    | any         | ❌ No pre-built binary |

### Fallback: manual install

If `urd-lsp` is already on your `$PATH` (e.g. installed via `cargo install`), the extension will use that binary instead of downloading anything. This is the recommended approach on **macOS**, where no pre-built binaries are available:

```bash
cargo install --git https://github.com/metdxt/urd urd-lsp
```

Make sure `~/.cargo/bin` is in your `$PATH`, then restart Zed.

## Configuration

No special configuration is required. The extension activates automatically for any file with the `.urd` extension.

### Spellcheck language

The language server supports an optional `spellcheckLanguage` initialization option. To set it in Zed, add the following to your `settings.json`:

```json
{
  "lsp": {
    "urd-lsp": {
      "initialization_options": {
        "spellcheckLanguage": "english"
      }
    }
  }
}
```

When omitted, the language server automatically detects the language of dialogue text using [whatlang](https://crates.io/crates/whatlang).

## Repository

The extension source lives at [github.com/metdxt/urd-zed](https://github.com/metdxt/urd-zed). For issues related to the grammar or the language server itself, file them on the main repository: [github.com/metdxt/urd](https://github.com/metdxt/urd/issues).