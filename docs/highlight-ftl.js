hljs.registerLanguage("ftl", function (hljs) {
  // --- Placeables (inside { }) ---

  var VARIABLE = {
    className: "variable",
    begin: /\$[a-zA-Z][a-zA-Z0-9_-]*/,
    relevance: 5,
  };

  var TERM_REF = {
    className: "keyword",
    begin: /-[a-zA-Z][a-zA-Z0-9_-]*/,
    relevance: 3,
  };

  var FUNCTION_CALL = {
    className: "built_in",
    begin: /[A-Z][A-Z0-9_-]*(?=\s*\()/,
    relevance: 5,
  };

  var NUMBER = {
    className: "number",
    begin: /-?\b\d+(?:\.\d+)?\b/,
    relevance: 0,
  };

  var STRING_LITERAL = {
    className: "string",
    begin: /"/,
    end: /"/,
    contains: [hljs.BACKSLASH_ESCAPE],
    relevance: 0,
  };

  var SELECTOR_ARROW = {
    className: "punctuation",
    begin: /->/,
    relevance: 3,
  };

  var DEFAULT_MARKER = {
    className: "selector-tag",
    begin: /\*(?=\[)/,
    relevance: 3,
  };

  var VARIANT_KEY = {
    className: "symbol",
    begin: /\[/,
    end: /\]/,
    contains: [NUMBER],
    relevance: 0,
  };

  // The placeable expression context — everything inside { }
  var PLACEABLE = {
    className: "template-tag",
    begin: /\{/,
    end: /\}/,
    contains: [
      VARIABLE,
      FUNCTION_CALL,
      TERM_REF,
      STRING_LITERAL,
      NUMBER,
      SELECTOR_ARROW,
      DEFAULT_MARKER,
      VARIANT_KEY,
    ],
    relevance: 5,
  };

  // Allow placeables to nest (selectors contain placeables in their values)
  VARIANT_KEY.contains.push(VARIABLE);
  PLACEABLE.contains = [PLACEABLE].concat(PLACEABLE.contains);

  // --- Top-level constructs ---

  // ### Resource comments (section headings)
  var RESOURCE_COMMENT = {
    className: "section",
    begin: /^###/m,
    end: /$/m,
    relevance: 5,
  };

  // ## Group comments
  var GROUP_COMMENT = {
    className: "meta",
    begin: /^##/m,
    end: /$/m,
    relevance: 3,
  };

  // # Single-line comments
  var LINE_COMMENT = {
    className: "comment",
    begin: /^#/m,
    end: /$/m,
    relevance: 0,
  };

  // Term definitions: -brand-name = ...
  var TERM_DEFINITION = {
    className: "keyword",
    begin: /^-[a-zA-Z][a-zA-Z0-9_-]*/m,
    relevance: 5,
  };

  // Message identifiers: message-id = ...  (start of line, before =)
  var MESSAGE_ID = {
    className: "attr",
    begin: /^[a-zA-Z][a-zA-Z0-9_-]*(?=\s*=)/m,
    relevance: 3,
  };

  // Attribute accessors: .attribute = ...  (indented, starts with dot)
  var ATTRIBUTE = {
    className: "attr",
    begin: /^\s+\.[a-zA-Z][a-zA-Z0-9_-]*/m,
    relevance: 3,
  };

  return {
    name: "Fluent",
    aliases: ["ftl", "fluent"],
    case_insensitive: false,
    contains: [
      RESOURCE_COMMENT,
      GROUP_COMMENT,
      LINE_COMMENT,
      TERM_DEFINITION,
      MESSAGE_ID,
      ATTRIBUTE,
      PLACEABLE,
      STRING_LITERAL,
      NUMBER,
    ],
  };
});

// Re-highlight code blocks that book.js already processed before the ftl
// language was registered.  Strip the `hljs` class and reset content to plain
// text so highlightBlock will re-parse from scratch.
document.querySelectorAll("code.language-ftl").forEach(function (block) {
  var raw = block.textContent;
  block.className = "language-ftl";
  block.removeAttribute("data-highlighted");
  block.innerHTML = "";
  block.textContent = raw;
  hljs.highlightBlock(block);
});
