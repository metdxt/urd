# Running with Quest

Quest is Urd's interactive CLI runner. It compiles and executes `.urd` scripts directly in your terminal, letting you play through dialogue, make choices, and see your story in action without wiring up a game engine. It also serves as a Swiss-army knife for Urd workflows — exporting IR graphs, generating localization stubs, and more.

## Basic Usage

### Running a Script

The most common command is `quest run`:

```bash
quest run hello.urd
```

This compiles `hello.urd`, runs all static analysis passes, and launches an interactive terminal session powered by [crossterm](https://docs.rs/crossterm). Dialogue lines are printed sequentially — press **Enter** to advance. When the VM yields a `Choice` event, Quest renders a numbered menu and waits for your selection.

### The Default Script

If you run `quest` with no arguments from within the Urd repository:

```bash
quest
```

it defaults to `quest run examples/quest/cave.urd` — a short fantasy adventure that demonstrates globals, menus, conditionals, string interpolation, and multi-label story structure. It's a good way to verify your installation and get a feel for how Urd scripts play out.

## The Interactive Terminal UI

Quest's terminal UI is deliberately minimal. Here's what to expect:

- **Dialogue** — speaker names are rendered with ANSI colors (matching the `name_color` field from the speaker struct). Each dialogue line is printed on its own line. Press **Enter** to advance to the next event.
- **Menus** — choices are displayed as a numbered list. Type the number of your choice and press **Enter**. Invalid input is re-prompted.
- **End of script** — when the VM reaches `end!()`, Quest prints a separator and exits cleanly.

The UI is intentionally plain. Quest is a development and testing tool, not a final presentation layer — that's your engine's job.

## Static Analysis

Before running a script, Quest automatically executes all of Urd's static analysis passes. If any errors are found, they are reported with source spans and colored diagnostics (via [ariadne](https://docs.rs/ariadne)) and execution is aborted. Warnings are printed but do not prevent execution.

This means `quest run` doubles as a quick lint check during development. If you just want to validate a script without playing through it, the analysis output alone is often enough.

## Localization with `--locale`

Quest has built-in support for [Project Fluent](https://projectfluent.org/) localization. If your script uses the `@fluent` decorator and you've generated `.ftl` translation files, you can run the script in a specific locale:

```bash
quest run merchant.urd --locale pl-PL
```

### Automatic Locale Discovery

When you omit the `--locale` flag, Quest looks for an `i18n/` directory next to the script file. For example, given:

```text
merchant.urd
i18n/
  en-US/
    merchant.ftl
  pl-PL/
    merchant.ftl
```

Quest will:

1. **Single locale found** — load it silently, no prompt.
2. **Multiple locales found** — present an interactive picker menu in the terminal so you can choose which locale to use.
3. **No `i18n/` directory** — run without localization (dialogue text is used as-is from the script).

The `-l` short flag also works:

```bash
quest run merchant.urd -l en-US
```

## Exporting Graphs

The `quest export` command compiles a script and renders the IR graph in a visual format, without executing it:

```bash
quest export script.urd --format mermaid
```

### Supported Formats

| Format | Flag | Description |
|--------|------|-------------|
| **Mermaid** | `--format mermaid` | A [Mermaid](https://mermaid.js.org/) flowchart. Paste the output into any Mermaid-compatible renderer (GitHub Markdown, the Mermaid Live Editor, etc.) to see a visual diagram of your dialogue graph. This is the default format. |
| **Graphviz DOT** | `--format dot` | A [Graphviz](https://graphviz.org/) DOT file. Pipe it to `dot -Tpng` or `dot -Tsvg` for a rendered image. |

### Writing to a File

By default, `quest export` writes to stdout. Use `--output` (or `-o`) to write to a file instead:

```bash
quest export script.urd --format dot -o graph.dot
dot -Tpng graph.dot -o graph.png
```

### Example Workflow

A typical graph-export workflow might look like this:

```bash
# Export to Mermaid and preview in the browser
quest export cave.urd --format mermaid -o cave.mmd

# Or export to DOT and render with Graphviz
quest export cave.urd --format dot -o cave.dot
dot -Tsvg cave.dot -o cave.svg
```

This is invaluable for debugging complex branching stories — you can see at a glance which labels connect to which, spot orphaned nodes, and verify that every path reaches an `end!()`.

## Generating Localization Stubs

The `quest gen-l10n` command scans a compiled script for all localizable strings and generates a Fluent `.ftl` stub file:

```bash
quest gen-l10n merchant.urd
```

By default, the generated file is written to `i18n/en-US/` relative to the script's parent directory. Use `--output` (or `-o`) to specify a different directory:

```bash
quest gen-l10n merchant.urd -o i18n/pl-PL/
```

The generated `.ftl` file contains message IDs for every localizable dialogue string in the script, with the original English text as placeholders. Hand this file to your translators, and they can fill in the translations without ever touching your `.urd` source.

### Typical Localization Workflow

```bash
# 1. Write your script with @fluent-tagged globals
#    (see the Localization chapter for details)

# 2. Generate the English stub
quest gen-l10n merchant.urd

# 3. Copy it for a new locale
cp -r i18n/en-US i18n/pl-PL

# 4. Translate the .ftl file in i18n/pl-PL/

# 5. Run in the new locale
quest run merchant.urd --locale pl-PL
```

## Pipe Mode (Non-TTY)

When Quest detects that stdin is not a terminal (e.g., when piped from another process or running in CI), it switches to **pipe mode**:

- Dialogue lines are printed to stdout as plain text (no ANSI color codes, no interactive prompts).
- Menus are printed with numeric indices, and Quest reads the choice index from stdin as a plain line of text.

This makes it possible to script Quest non-interactively:

```bash
echo -e "0\n1\n0" | quest run hello.urd
```

Each line of input corresponds to a choice index (0-based) for the next menu encountered. This is useful for automated testing, CI pipelines, or feeding a script through a bot.

## Command Reference

| Command | Description |
|---------|-------------|
| `quest run <script>` | Run a script interactively (default subcommand) |
| `quest run <script> --locale <TAG>` | Run with a specific Fluent locale |
| `quest export <script> --format <FMT>` | Export the IR graph (`mermaid` or `dot`) |
| `quest export <script> -o <FILE>` | Write export output to a file |
| `quest gen-l10n <script>` | Generate a Fluent `.ftl` stub |
| `quest gen-l10n <script> -o <DIR>` | Write the stub to a specific directory |
| `quest --help` | Show help and all available options |

## What's Next?

- **[Language Overview](../language/overview.md)** — learn the full Urd language in depth.
- **[Integration Guide](../integration/overview.md)** — embed Urd in your own game engine.
- **[Localization](../localization/overview.md)** — set up full Fluent localization for your scripts.