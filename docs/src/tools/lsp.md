# Language Server (urd-lsp)

`urd-lsp` is a full-featured Language Server Protocol implementation for the Urd dialogue scripting language. It provides real-time feedback, navigation, and refactoring support in any editor that speaks LSP.

## Architecture

The server is built on [tower-lsp](https://github.com/ebkalderon/tower-lsp) and communicates over **stdin/stdout** using the standard JSON-RPC protocol. It runs as a single-threaded async Tokio process — one instance per editor workspace.

```
Editor  ←──JSON-RPC over stdin/stdout──→  urd-lsp
                                            ├── Parser (chumsky)
                                            ├── IR Compiler
                                            ├── 20+ Analysis Passes
                                            ├── Workspace Index (cross-file)
                                            └── Spellcheck (optional)
```

Every keystroke triggers a full reparse and analysis cycle. The server maintains a `WorkspaceIndex` that tracks all `.urd` files in the project, enabling cross-file go-to-definition, find-references, and import-aware completions.

## Installation

### From source (recommended)

```bash
cargo install --path crates/urd-lsp
```

To install **without** the spellcheck feature (smaller binary, no embedded dictionaries):

```bash
cargo install --no-default-features --path crates/urd-lsp
```

### Pre-built binaries

Pre-built binaries are available on the [GitHub Releases](https://github.com/metdxt/urd/releases) page for the following platforms:

| Platform | Architecture | Status |
|----------|-------------|--------|
| Linux    | x86_64      | ✅ Pre-built binary |
| Linux    | aarch64     | ✅ Pre-built binary |
| Windows  | x86_64      | ✅ Pre-built binary |
| Windows  | aarch64     | ✅ Pre-built binary |
| macOS    | any         | ❌ Build from source |

## Features at a Glance

| Feature | Description |
|---------|-------------|
| **Diagnostics** | All 20+ analysis passes run on every edit — errors and warnings appear instantly |
| **Hover** | Variable types, label info, decorator fields, and localization key badges |
| **Completion** | Context-aware suggestions for labels, decorators, struct fields, enum variants, and imported symbols |
| **Go to Definition** | Jump to any symbol — local or cross-file via the workspace index |
| **Find References** | Find all usages of a symbol across the entire workspace |
| **Rename** | Safe rename with `prepareRename` validation |
| **Document Symbols** | Labels, enums, structs, decorators, constants, and globals in the outline panel |
| **Semantic Tokens** | 11 token types for rich syntax highlighting beyond what tree-sitter provides |
| **Code Actions** | "Create label" quick fix, spellcheck replacements, "Add to dictionary" |
| **Spellcheck** | Optional SymSpell-based spell checking with automatic language detection |

For detailed information on each feature, see:

- [Capabilities](./lsp-capabilities.md) — full list of LSP features and how they work
- [Diagnostics & Lints](./lsp-diagnostics.md) — every diagnostic the server can emit
- [Spellcheck](./lsp-spellcheck.md) — spell checking configuration and usage

## Optional Features

The `spellcheck` Cargo feature is **enabled by default**. It adds:

- Automatic language detection via [whatlang](https://github.com/grstrz/whatlang-rs)
- Per-language [SymSpell](https://github.com/wolfgarbe/SymSpell) dictionaries (embedded in the binary)
- A persistent user dictionary (`.urd-dict` file in the workspace root)
- "Replace with '…'" and "Add to dictionary" code actions

Disable it with `--no-default-features` if binary size or compile time is a concern.

## Configuration

The server accepts initialization options from the client:

| Option | Type | Description |
|--------|------|-------------|
| `spellcheckLanguage` | `string` | Force a specific language for spell checking (e.g. `"en"`, `"pl"`). When omitted, language is auto-detected per document via whatlang. |

How you pass these options depends on your editor — see the [Zed](../editors/zed.md), [Neovim](../editors/neovim.md), and [Helix](../editors/helix.md) setup guides for examples.

## Verifying the Installation

After installing, verify the binary is on your PATH:

```bash
urd-lsp --version
```

The server is designed to be launched by your editor automatically. You should not need to run it manually — just configure your editor to use `urd-lsp` as the language server for `.urd` files.