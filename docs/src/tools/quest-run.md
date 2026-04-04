# Running Scripts

The `run` subcommand launches an interactive dialogue session in your terminal.

```bash
quest run <script>
```

If no subcommand is given, `quest` defaults to `run` with the built-in example script (`examples/quest/cave.urd`).

## Basic Usage

```bash
# Run a specific script
quest run my_story.urd

# Run the default example
quest
```

## Localization

Pass `--locale` (or `-l`) to load translations for a specific locale:

```bash
quest run script.urd --locale pl-PL
```

### Automatic Locale Discovery

When `--locale` is not specified, `quest` looks for an `i18n/` directory next to the script:

```text
my_project/
├── merchant.urd
└── i18n/
    ├── en-US/
    │   └── merchant.ftl
    └── pl-PL/
        └── merchant.ftl
```

- **One locale found** — loaded silently.
- **Multiple locales found** — an interactive picker is shown so you can choose.
- **No `i18n/` directory** — the script runs without localization (original text is displayed as-is).

When a locale is loaded, `quest` prints a banner:

```text
  🌐 pl-PL  Polish (Poland)
```

## Interactive Mode (TTY)

When standard input is a terminal, `quest` uses a full Crossterm-based TUI:

- Dialogue lines are printed sequentially. Press **Enter** to advance.
- Menus are displayed as selectable lists. Use **↑**/**↓** arrow keys to navigate and **Enter** to confirm.
- Press **Ctrl+C** or **q** at any prompt to quit.

Speaker names are rendered in color when the script provides a `name_color` field on the speaker struct.

## Pipe Mode (Non-TTY)

When stdin is not a terminal (e.g. piped input), `quest` switches to a non-interactive mode:

- Dialogue lines are printed directly to stdout.
- Menus print each option with a numeric index, then read a line from stdin to determine the selection.

This is useful for automated testing or scripting:

```bash
echo "0" | quest run merchant.urd
```

The index is **zero-based** — `0` selects the first option, `1` the second, and so on.

## Examples

```bash
# Run with Polish localization
quest run examples/localization/merchant.urd --locale pl-PL

# Run and pipe choices for automated testing
printf "0\n1\n0\n" | quest run examples/quest/cave.urd

# Just run the built-in cave example interactively
quest
```

## Options Reference

| Flag | Short | Description |
|------|-------|-------------|
| `<script>` | | Path to the `.urd` script file (default: `examples/quest/cave.urd`) |
| `--locale <TAG>` | `-l` | Force a specific BCP 47 locale tag (e.g. `en-US`, `pl-PL`) |