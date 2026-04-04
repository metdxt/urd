# Installation

This page covers how to install the Urd toolchain — the core library, the Quest CLI runner, and the language server.

## Prerequisites

Urd requires the **Rust nightly toolchain**. The workspace uses edition 2024, which is only available on nightly.

If you don't have Rust installed, grab it from [rustup.rs](https://rustup.rs/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Then install the nightly toolchain:

```bash
rustup toolchain install nightly
```

> **Note:** The Urd repository includes a `rust-toolchain.toml` that pins the channel to `nightly`, so Cargo will automatically use the correct toolchain when you build from within the repository. You don't need to set nightly as your global default.

## Installing from Source

Clone the repository:

```bash
git clone https://github.com/metdxt/urd.git
cd urd
```

### Quest CLI

Quest is the interactive terminal runner for Urd scripts. It lets you play through dialogue scripts, export IR graphs, and generate localization stubs.

```bash
cargo install --path crates/urd-quest
```

This installs the `quest` binary into your Cargo bin directory (typically `~/.cargo/bin/`).

### Language Server

The Urd language server (`urd-lsp`) provides diagnostics, hover, go-to-definition, rename, completions, and optional spellcheck for any editor that speaks LSP.

Install with spellcheck support (the default):

```bash
cargo install --path crates/urd-lsp
```

Install without spellcheck for a leaner binary:

```bash
cargo install --no-default-features --path crates/urd-lsp
```

This installs the `urd-lsp` binary. See the [Editor Setup](../editors/zed.md) section for how to configure your editor to use it.

## Adding Urd as a Library Dependency

To embed Urd in your own game or application, add it as a Cargo dependency.

From crates.io (when published):

```bash
cargo add urd
```

From the Git repository directly:

```toml
[dependencies]
urd = { git = "https://github.com/metdxt/urd" }
```

Or if you've cloned the repo locally and want a path dependency:

```toml
[dependencies]
urd = { path = "../urd/crates/urd" }
```

### The `spellcheck` Feature

The core `urd` crate ships with an optional `spellcheck` feature that enables:

- **SymSpell** — fast approximate string matching for spell-checking dialogue text
- **whatlang** — automatic language detection
- **ureq** — lazy dictionary download into the user's cache directory

This feature is **disabled by default** to keep the core library lean — no embedded dictionaries, no network code unless you opt in.

Enable it explicitly:

```bash
cargo build -p urd --features spellcheck
```

Or in your `Cargo.toml`:

```toml
[dependencies]
urd = { git = "https://github.com/metdxt/urd", features = ["spellcheck"] }
```

> **Note:** The `urd-lsp` crate enables `spellcheck` by default. If you're only using the core library for embedding, you probably don't need it.

## Verifying Your Installation

After installing Quest, verify everything is working:

```bash
quest --help
```

You should see output describing the available subcommands (`run`, `export`, `gen-l10n`). If the command is not found, make sure `~/.cargo/bin` is in your `PATH`.

To do a quick smoke test, run the bundled example from inside the repository:

```bash
quest run examples/quest/cave.urd
```

This launches an interactive terminal session with a short fantasy adventure. If you see dialogue text and menu choices, everything is installed correctly.

## What's Next?

- **[Your First Script](./first-script.md)** — write a `.urd` file from scratch and learn the basics.
- **[Running with Quest](./running-with-quest.md)** — explore everything the CLI runner can do.