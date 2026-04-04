# Quest CLI

`quest` is the command-line runner and utility tool for Urd dialogue scripts. It compiles, runs, exports, and generates localization files — everything you need to develop and test Urd scripts outside of a game engine.

## Installation

Install from the Urd monorepo:

```bash
cargo install --path crates/urd-quest
```

Verify the installation:

```bash
quest --help
```

You should see output describing the available subcommands.

## Subcommands

Quest provides three subcommands:

| Subcommand | Description |
|---|---|
| [`run`](./quest-run.md) | Run a script interactively in the terminal |
| [`export`](./quest-export.md) | Export the compiled IR graph as DOT or Mermaid |
| [`gen-l10n`](./quest-gen-l10n.md) | Generate Fluent `.ftl` localization stubs |

### Default Behavior

When invoked without a subcommand, `quest` runs the built-in example script:

```bash
# These are equivalent:
quest
quest run examples/quest/cave.urd
```

## Quick Examples

Run a script interactively:

```bash
quest run my_dialogue.urd
```

Run with a specific locale:

```bash
quest run my_dialogue.urd --locale pl-PL
```

Export a Mermaid flowchart:

```bash
quest export my_dialogue.urd --format mermaid
```

Generate localization stubs:

```bash
quest gen-l10n my_dialogue.urd
```

## How It Works

Under the hood, `quest` uses the same `urd` library that game engines use. When you run a script, it:

1. **Parses** the `.urd` file (and any imports) into an AST
2. **Compiles** the AST into an IR graph
3. **Runs analysis** passes to catch errors and warnings
4. **Creates a VM** and drives it in a pull-based loop
5. **Renders** dialogue events and choice menus in the terminal

This means that if a script works in `quest`, it will work identically in your game engine — the execution model is the same.