# Neovim

Neovim gets Urd support through two components: **tree-sitter-urd** for syntax highlighting and **urd-lsp** for full language server features.

---

## Tree-sitter Grammar

The Urd tree-sitter grammar provides syntax highlighting, indentation, and local scope tracking.

### Installing the Grammar

Add the Urd grammar to your tree-sitter configuration. If you use **nvim-treesitter**, add a custom parser entry in your Neovim config:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

parser_config.urd = {
  install_info = {
    url = "https://github.com/metdxt/urd",
    files = { "tree-sitter-urd/src/parser.c" },
    location = "tree-sitter-urd",
    branch = "main",
    generate_requires_npm = false,
    requires_generate_from_grammar = false,
  },
  filetype = "urd",
}
```

Then install the parser:

```vim
:TSInstall urd
```

### Query Files

The grammar ships with three query files in `tree-sitter-urd/queries/`:

| File | Purpose |
|---|---|
| `highlights.scm` | Syntax highlighting — keywords, labels, strings, numbers, operators, decorators, etc. |
| `indents.scm` | Smart indentation rules based on block structure |
| `locals.scm` | Local scope and definition tracking for variable references |

Copy these into your Neovim runtime queries directory so tree-sitter can find them:

```bash
mkdir -p ~/.config/nvim/queries/urd
cp tree-sitter-urd/queries/highlights.scm ~/.config/nvim/queries/urd/
cp tree-sitter-urd/queries/indents.scm    ~/.config/nvim/queries/urd/
cp tree-sitter-urd/queries/locals.scm     ~/.config/nvim/queries/urd/
```

### File Type Detection

Register `.urd` files so Neovim recognises them automatically:

```lua
vim.filetype.add({
  extension = {
    urd = "urd",
  },
})
```

---

## Language Server

The `urd-lsp` language server provides diagnostics, completions, hover info, go-to-definition, find references, rename, semantic tokens, code actions, and optional spellchecking.

### Installing urd-lsp

```bash
# With spellcheck (default)
cargo install --path crates/urd-lsp

# Without spellcheck (smaller binary)
cargo install --no-default-features --path crates/urd-lsp
```

Make sure the `urd-lsp` binary is on your `$PATH`.

### Configuration with nvim-lspconfig

If you use [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig), you can register `urd-lsp` as a custom server:

```lua
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")

if not configs.urd_lsp then
  configs.urd_lsp = {
    default_config = {
      cmd = { "urd-lsp" },
      filetypes = { "urd" },
      root_dir = lspconfig.util.root_pattern(".git", ".urd-dict"),
      single_file_support = true,
      settings = {},
      init_options = {
        -- Force a specific spellcheck language (optional).
        -- Omit to let urd-lsp auto-detect via whatlang.
        -- spellcheckLanguage = "English",
      },
    },
  }
end

lspconfig.urd_lsp.setup({
  on_attach = function(client, bufnr)
    -- Your standard on_attach keybindings here
    local opts = { buffer = bufnr, noremap = true, silent = true }
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
    vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  end,
})
```

### Manual Configuration (without nvim-lspconfig)

You can also start `urd-lsp` manually using the built-in LSP client:

```lua
vim.api.nvim_create_autocmd("FileType", {
  pattern = "urd",
  callback = function()
    vim.lsp.start({
      name = "urd-lsp",
      cmd = { "urd-lsp" },
      root_dir = vim.fs.dirname(vim.fs.find({ ".git", ".urd-dict" }, { upward = true })[1]),
      init_options = {},
    })
  end,
})
```

---

## Complete Example Configuration

Here is a self-contained Neovim configuration snippet that sets up both tree-sitter and LSP for Urd:

```lua
-- 1. File type detection
vim.filetype.add({ extension = { urd = "urd" } })

-- 2. Tree-sitter parser
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.urd = {
  install_info = {
    url = "https://github.com/metdxt/urd",
    files = { "tree-sitter-urd/src/parser.c" },
    location = "tree-sitter-urd",
    branch = "main",
  },
  filetype = "urd",
}

-- 3. Language server
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")

if not configs.urd_lsp then
  configs.urd_lsp = {
    default_config = {
      cmd = { "urd-lsp" },
      filetypes = { "urd" },
      root_dir = lspconfig.util.root_pattern(".git", ".urd-dict"),
      single_file_support = true,
    },
  }
end

lspconfig.urd_lsp.setup({})
```

---

## Troubleshooting

- **No highlighting** — Make sure the query files are in `~/.config/nvim/queries/urd/` and that the parser is installed (`:TSInstallInfo` should show `urd` as installed).
- **LSP not starting** — Run `urd-lsp` directly in a terminal to confirm it is installed and on your `PATH`. Check `:LspLog` for error output.
- **Spellcheck not working** — The LSP runs spellcheck on save, not on every keystroke. Save the file (`:w`) to trigger it. Ensure `urd-lsp` was built with the `spellcheck` feature (the default).