# Spellcheck

The Urd language server includes an integrated spellcheck system for dialogue text. It is **feature-gated** — the `spellcheck` Cargo feature must be enabled at compile time (it is on by default).

## Building With Spellcheck

Spellcheck is included by default:

```bash
cargo install --path crates/urd-lsp
```

To build **without** spellcheck (smaller binary, no dictionary support):

```bash
cargo install --no-default-features --path crates/urd-lsp
```

## How It Works

The spellcheck system uses [SymSpell](https://github.com/wolfgarbe/SymSpell) for fast approximate string matching. Language detection is performed automatically via [whatlang](https://github.com/grstrz/whatlang-rs), so the server can handle scripts written in different natural languages.

Dictionaries are **not** embedded in the binary. Instead, they are downloaded lazily from the [hermitdave/FrequencyWords](https://github.com/hermitdave/FrequencyWords) corpus the first time a language is used, and cached locally under `{cache_dir}/urd-spellcheck/` (e.g. `~/.cache/urd-spellcheck/` on Linux). This means **network access is required on first use** of each language. Subsequent runs use the cached dictionaries.

Spellcheck runs **on save** (not on every keystroke) to avoid the latency of dictionary lookups during interactive editing. Diagnostics from the spellcheck pass appear alongside the regular analysis diagnostics with the source tag `urd-spell`.

## Configuration

### Language Override

By default, the server auto-detects the language of dialogue text using whatlang. You can force a specific language by passing `spellcheckLanguage` in your editor's LSP initialization options:

```json
{
  "initializationOptions": {
    "spellcheckLanguage": "english"
  }
}
```

When set, all files in the workspace use the specified language for spellchecking, bypassing automatic detection.

Supported languages: `"english"`, `"german"`, `"spanish"`, `"french"`, `"hebrew"`, `"italian"`, `"russian"`, `"chinese"`.

## User Dictionary

False positives are inevitable — character names, invented words, and game-specific terminology will be flagged. The user dictionary lets you suppress these.

### File Location

The dictionary file is `.urd-dict` in the workspace root directory. The server loads it automatically on startup.

### File Format

The file is plain text, one word per line. Lines starting with `#` are comments. All words are stored in lowercase.

```text
# Urd user dictionary – words in this file are not spell-checked.
elara
bazaar
myword
```

The file is created automatically the first time you add a word via the code action (see below), complete with a comment header.

## Code Actions

When the cursor is on a misspelled word, the language server offers two quick-fix code actions:

### Replace with '...'

If SymSpell finds a close match, the server offers a replacement action. This is the **preferred** action — it appears first in the quick-fix menu and is triggered by the default quick-fix keybinding.

The replacement respects the casing of the original word:

- **Lowercase** original → suggestion returned as-is
- **Title Case** original → suggestion is capitalised
- **ALL CAPS** original → suggestion is uppercased

### Add to Dictionary

Always available regardless of whether a spelling suggestion exists. This action:

1. Executes the `urd.addToDictionary` command on the server
2. Appends the word (lowercased) to the `.urd-dict` file
3. Re-runs spellcheck on all open documents so the word is immediately unflagged

If the `.urd-dict` file does not exist yet, it is created with a comment header.

## Diagnostic Format

Spellcheck diagnostics appear as **warnings** with the source `urd-spell`. Each diagnostic carries structured data including the misspelled word and the suggested replacement (if any), which editors use to power the code actions described above.

## Example Workflow

1. Write some dialogue in your `.urd` script:

```urd
narrator: "You enter the mystirious cave."
```

2. Save the file. The LSP flags `mystirious` with an `urd-spell` warning.

3. Place your cursor on the word and open quick-fixes:
   - **Replace with 'mysterious'** — fixes the typo in place
   - **Add 'mystirious' to dictionary** — if it's intentional

4. If you choose "Add to dictionary", the word is written to `.urd-dict` and the warning disappears from all open files.

## Limitations

- Spellcheck only examines **dialogue text** (string content in dialogue lines and menu option labels). Variable names, label names, and comments are not checked.
- Dictionaries are downloaded on first use from the [FrequencyWords](https://github.com/hermitdave/FrequencyWords) corpus and cached under `{cache_dir}/urd-spellcheck/`. Network access is required the first time each language is used. Supported languages: English, German, Spanish, French, Hebrew, Italian, Russian, and Chinese.
- The user dictionary is workspace-scoped. There is no global dictionary across projects — each workspace has its own `.urd-dict`.