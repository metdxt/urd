# Summary

[Introduction](./introduction.md)

# Getting Started

- [Installation](./getting-started/installation.md)
- [Your First Script](./getting-started/first-script.md)
- [Running with Quest](./getting-started/running-with-quest.md)

# Language Guide

- [Overview](./language/overview.md)
- [Variables & Types](./language/variables-and-types.md)
    - [Constants](./language/constants.md)
    - [Globals](./language/globals.md)
    - [Type Annotations](./language/type-annotations.md)
- [Operators](./language/operators.md)
- [Strings & Interpolation](./language/strings.md)
- [Dialogue](./language/dialogue.md)
    - [Speakers](./language/speakers.md)
    - [Multi-line Dialogue](./language/multiline-dialogue.md)
- [Labels & Jump](./language/labels-and-jump.md)
    - [Entry Points](./language/entry-points.md)
    - [Jump and Return](./language/jump-and-return.md)
- [Menus & Choices](./language/menus.md)
- [Control Flow](./language/control-flow.md)
    - [If / Elif / Else](./language/if-elif-else.md)
    - [Match](./language/match.md)
- [Functions](./language/functions.md)
- [Enums](./language/enums.md)
- [Structs](./language/structs.md)
- [Dice](./language/dice.md)
- [Lists & Maps](./language/lists-and-maps.md)
- [Decorators](./language/decorators.md)
    - [Built-in Decorators](./language/builtin-decorators.md)
    - [Script-Defined Decorators](./language/script-decorators.md)
- [Imports & Multi-file Projects](./language/imports.md)
- [Extern Values](./language/extern-values.md)
- [Comments & Documentation](./language/comments.md)

# Integration Guide

- [Overview](./integration/overview.md)
- [Compiling Scripts](./integration/compiling.md)
- [The Virtual Machine](./integration/vm.md)
    - [Pull-Based Execution](./integration/pull-based-execution.md)
    - [Events & Choices](./integration/events-and-choices.md)
    - [Step Budget & Limits](./integration/budget-and-limits.md)
- [Decorator Registry](./integration/decorator-registry.md)
- [File Loaders](./integration/file-loaders.md)
- [Extern Values](./integration/extern-values.md)
- [Dice Roller](./integration/dice-roller.md)
- [Error Handling](./integration/error-handling.md)

# Localization

- [Overview](./localization/overview.md)
- [The @fluent Decorator](./localization/fluent-decorator.md)
- [Generating .ftl Files](./localization/generating-ftl.md)
- [Implementing a Localizer](./localization/implementing-localizer.md)
- [Plural Rules & Grammar](./localization/plural-rules.md)

# Tools

- [Quest CLI](./tools/quest.md)
    - [Running Scripts](./tools/quest-run.md)
    - [Exporting Graphs](./tools/quest-export.md)
    - [Generating Localization](./tools/quest-gen-l10n.md)
- [Language Server (urd-lsp)](./tools/lsp.md)
    - [Capabilities](./tools/lsp-capabilities.md)
    - [Diagnostics & Lints](./tools/lsp-diagnostics.md)
    - [Spellcheck](./tools/lsp-spellcheck.md)

# Editor Setup

- [Zed](./editors/zed.md)
- [Neovim](./editors/neovim.md)
- [Helix](./editors/helix.md)

# Static Analysis

- [Overview](./analysis/overview.md)
- [Error Lints](./analysis/error-lints.md)
- [Warning Lints](./analysis/warning-lints.md)
- [Loop Detection](./analysis/loop-detection.md)

# API Reference

- [Prelude](./api/prelude.md)
- [RuntimeValue](./api/runtime-value.md)
- [Event & ChoiceEvent](./api/event.md)
- [VmStep & VmError](./api/vm-step.md)
- [CompilerError](./api/compiler-error.md)

# Examples

- [Cave Adventure](./examples/cave-adventure.md)
- [Multi-file Project](./examples/multifile.md)
- [Localized Merchant](./examples/localized-merchant.md)
- [Circular Imports](./examples/circular-imports.md)

---

[Keyword Reference](./reference/keywords.md)
[Operator Reference](./reference/operators.md)
[Built-in Methods](./reference/builtin-methods.md)
[Grammar (EBNF)](./reference/grammar.md)