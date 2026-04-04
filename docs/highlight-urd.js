hljs.registerLanguage("urd", function (hljs) {
  var STRING = {
    className: "string",
    begin: /"/,
    end: /"/,
    contains: [
      hljs.BACKSLASH_ESCAPE,
      {
        className: "subst",
        begin: /\{/,
        end: /\}/,
        keywords: {
          keyword:
            "label jump menu return if elif else match fn decorator import " +
            "from as in const let global extern struct enum event and or not",
          literal: "true false null",
        },
        contains: [hljs.C_NUMBER_MODE],
      },
    ],
  };

  var DECORATOR = {
    className: "meta",
    begin: /@[a-zA-Z_]\w*/,
    starts: {
      end: /[)\s]/,
      returnEnd: true,
      contains: [
        STRING,
        hljs.C_NUMBER_MODE,
        {
          className: "meta-string",
          begin: /\(/,
          end: /\)/,
          contains: [STRING, hljs.C_NUMBER_MODE],
        },
      ],
    },
    relevance: 5,
  };

  // `label name`, `fn name`, `decorator name`
  var DEFINITION = {
    beginKeywords: "label fn decorator",
    end: /[{(<]/,
    excludeEnd: true,
    contains: [
      {
        className: "title",
        begin: /[a-zA-Z_]\w*/,
        relevance: 0,
      },
    ],
  };

  // `enum Name`, `struct Name`
  var TYPE_DEFINITION = {
    beginKeywords: "enum struct",
    end: /\{/,
    excludeEnd: true,
    contains: [
      {
        className: "type",
        begin: /[A-Z]\w*/,
        relevance: 0,
      },
    ],
  };

  // `jump target` and `jump module.target`
  var JUMP = {
    beginKeywords: "jump",
    end: /$/,
    excludeEnd: true,
    contains: [
      {
        className: "title",
        begin: /[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*/,
        relevance: 0,
      },
    ],
  };

  // `import "file.urd" as alias`  /  `import (X, Y) from "file.urd"`
  var IMPORT = {
    beginKeywords: "import",
    end: /$/,
    excludeEnd: true,
    keywords: {
      keyword: "import from as",
    },
    contains: [
      STRING,
      {
        className: "type",
        begin: /\b[A-Z]\w*\b/,
        relevance: 0,
      },
    ],
  };

  // Dialogue speaker:  `narrator:`  (identifier at line start followed by colon)
  // Excludes `:{` which is dict/map syntax
  var SPEAKER = {
    className: "symbol",
    begin: /^[ \t]*[a-zA-Z_]\w*[ \t]*:(?!\{)/m,
    relevance: 2,
  };

  // Dice notation: 2d6, 3D20, 1d20
  var DICE = {
    className: "number",
    begin: /\b[0-9]{1,3}[dD][0-9]{1,3}\b/,
    relevance: 5,
  };

  // Capitalized type names: Faction, Character, Item, Faction.Guild
  var TYPE_NAME = {
    className: "type",
    begin: /\b[A-Z]\w*\b/,
    relevance: 0,
  };

  // Type annotations:  `: int`, `: str`, `-> int`
  var TYPE_ANNOTATION = {
    className: "type",
    begin: /(?::|\->)\s*(?:int|float|bool|str|null|list|map|dice|range)\b/,
    relevance: 0,
  };

  // Return type arrow with named type:  `-> Faction`
  var RETURN_TYPE = {
    className: "type",
    begin: /->\s*[A-Z]\w*/,
    relevance: 0,
  };

  // Doc comments:  ## ...
  var DOC_COMMENT = {
    className: "comment",
    begin: /##/,
    end: /$/,
    relevance: 2,
  };

  // Line comments:  # ...
  var LINE_COMMENT = {
    className: "comment",
    begin: /#/,
    end: /$/,
    relevance: 0,
  };

  // end!() and todo!()
  var BANG_MACROS = {
    className: "built_in",
    begin: /\b(?:end|todo)!\s*\(\s*\)/,
    relevance: 5,
  };

  return {
    name: "Urd",
    aliases: ["urd"],
    case_insensitive: false,
    keywords: {
      keyword:
        "label jump menu return if elif else match fn decorator import " +
        "from as in const let global extern struct enum event and or not",
      literal: "true false null",
    },
    contains: [
      DOC_COMMENT,
      LINE_COMMENT,
      BANG_MACROS,
      DECORATOR,
      DEFINITION,
      TYPE_DEFINITION,
      JUMP,
      IMPORT,
      TYPE_ANNOTATION,
      RETURN_TYPE,
      SPEAKER,
      STRING,
      DICE,
      {
        className: "number",
        begin: /\b0[xX][0-9a-fA-F][0-9a-fA-F_]*\b/,
        relevance: 0,
      },
      {
        className: "number",
        begin: /\b0[bB][01][01_]*\b/,
        relevance: 0,
      },
      {
        className: "number",
        begin: /\b0[oO][0-7][0-7_]*\b/,
        relevance: 0,
      },
      {
        className: "number",
        begin: /\b[0-9][0-9_]*(?:\.[0-9][0-9_]*)?(?:[eE][+-]?[0-9]+)?\b/,
        relevance: 0,
      },
      TYPE_NAME,
    ],
  };
});

// Re-highlight code blocks that book.js already processed (with hljs 10.x)
// before the urd language was registered. We must strip the `hljs` class and
// reset content to plain text so highlightBlock will re-parse from scratch.
document.querySelectorAll("code.language-urd").forEach(function (block) {
  var raw = block.textContent;
  block.className = "language-urd";
  block.removeAttribute("data-highlighted");
  block.innerHTML = "";
  block.textContent = raw;
  hljs.highlightBlock(block);
});
