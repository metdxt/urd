# Introduction

**Urd** is a dialogue scripting language designed to be embedded in any game engine.

You write `.urd` scripts — plain-text files that read like screenplays — and the Urd compiler transforms them into a directed graph (the IR). A lightweight, pull-based virtual machine then walks that graph one step at a time, yielding structured events (`Dialogue`, `Choice`) that your engine renders however it pleases. Urd doesn't know what a screen is. It doesn't draw anything. It emits events and gets out of the way.

## The Name

Urd is named after **Urðr**, one of the three Norns in Norse mythology. The Norns tend the roots of Yggdrasil and shape the fate of gods and mortals alike. Urðr governs what *has become* — destiny already written. A fitting patron for a language whose job is to hold the script of every conversation, every branching choice, every twist of fate in your game's story.

## Why Urd?

Most dialogue systems are tightly coupled to a specific engine, require async runtimes, or rely on callback spaghetti. Urd takes a different approach:

- **Engine-agnostic** — Urd is a Rust library. It compiles to any target Rust compiles to. No engine plugins, no runtime dependencies on Unity, Godot, Unreal, or anything else.
- **Pull-based** — Your game loop drives execution. Call `vm.next()` when you're ready for the next event. No threads, no callbacks, no async, no hidden control flow.
- **Zero overhead when idle** — The VM does nothing until you ask it to step. It holds state, not execution context.
- **Comprehensive static analysis** — Over a dozen lint passes catch bugs at compile time: type errors, unreachable labels, dead branches, exhaustiveness gaps, undefined variables, possible typos, infinite loops, and more.
- **Localization-ready** — First-class [Project Fluent](https://projectfluent.org/) integration. Tag your globals, generate `.ftl` stubs, and let translators handle pluralization and grammatical case without touching your scripts.

## The Compilation Pipeline

Every Urd script flows through a well-defined pipeline:

```text
Source (.urd)
    │
    ▼
  Lexer        ── tokenizes raw text into a token stream
    │
    ▼
  Parser       ── builds an Abstract Syntax Tree (AST)
    │
    ▼
  Analysis     ── comprehensive static analysis over the AST
    │
    ▼
  Compiler     ── lowers the AST into an IrGraph (directed graph)
    │
    ▼
  VM           ── walks the graph, yielding Events
    │
    ▼
  Your Engine  ── renders dialogue, shows menus, plays audio, etc.
```

The key insight: **your engine only interacts with the VM**. Everything upstream — lexing, parsing, analysis, compilation — happens once. At runtime you just step through the graph.

## Key Features

- **Dialogue as a first-class construct** — speakers are values, not magic strings. Multi-line dialogue blocks keep scripts readable.
- **Menus and branching** — player choices are expressed declaratively with `menu` blocks.
- **Labels and jumps** — named sections with `goto`-style control flow, plus `jump ... and return` for subroutine-style calls.
- **Static analysis** — over a dozen lint passes including type checking, exhaustiveness analysis, dead-end detection, unreachable label warnings, loop detection, possible typo suggestions, and optional spellcheck.
- **Localization** — first-class Fluent integration with the `@fluent` decorator and automatic `.ftl` scaffold generation.
- **Multi-file imports** — split your story across files with `import`. Circular imports are handled gracefully.
- **Decorators** — attach custom metadata to dialogue events. Define them in-script or register handlers from your engine at runtime.
- **Dice notation** — `2d6`, `1d20+5`, and pattern matching on roll results are built into the language.
- **Enums and structs** — define your own types with full exhaustiveness checking in `match` expressions.
- **Functions** — pure functions for logic that doesn't produce dialogue.
- **String interpolation** — embed variables and field access directly in dialogue text.
- **Full LSP** — the `urd-lsp` language server provides diagnostics, hover, go-to-definition, rename, completion, and spellcheck.
- **Tree-sitter grammar** — syntax highlighting and structural editing for Neovim, Helix, and Zed.

## Hello, World

Here's the smallest possible Urd script:

```urd
const narrator = :{ name: "Narrator", name_color: "white" }

@entry
label start {
    narrator: "Hello, world!"
    end!()
}
```

This defines a speaker called `narrator`, marks `start` as the entry point with the `@entry` decorator, emits a single line of dialogue, and ends execution. When your engine steps the VM, it will receive one `Dialogue` event containing the speaker metadata and the text `"Hello, world!"`, followed by an `Ended` signal.

## The Urd Ecosystem

Urd is a workspace of several crates and tools that work together:

| Component | Description |
|-----------|-------------|
| **`urd`** | The core library — lexer, parser, compiler, analysis, IR, and VM. Add it as a dependency to embed Urd in your game. |
| **`urd-lsp`** | A Language Server Protocol implementation providing real-time diagnostics, hover info, go-to-definition, rename, completions, and optional spellcheck. |
| **`quest`** | An interactive CLI runner for playing Urd scripts in the terminal, exporting IR graphs, and generating localization stubs. |
| **`tree-sitter-urd`** | A tree-sitter grammar for Urd, enabling syntax highlighting and structural queries in any editor that supports tree-sitter. |
| **`urd-zed`** | A Zed editor extension bundling the tree-sitter grammar and LSP integration for a first-class editing experience. |

## What's Next?

- **[Installation](./getting-started/installation.md)** — get Urd, Quest, and the LSP set up on your machine.
- **[Your First Script](./getting-started/first-script.md)** — write a short interactive dialogue from scratch.
- **[Running with Quest](./getting-started/running-with-quest.md)** — play your script in the terminal.