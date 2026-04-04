hljs.registerLanguage('urd', function(hljs) {
  var KEYWORDS = {
    $pattern: /[a-z_!][a-z_!]*/,
    keyword:
      'label jump menu return if elif else match fn decorator import from as in ' +
      'const let global extern struct enum event and or not',
    literal: 'true false null',
    built_in: 'end! todo!',
  };

  // # line comments and ## doc comments
  var LINE_COMMENT = hljs.COMMENT('#', '$', {
    relevance: 0,
    variants: [
      { begin: '##', scope: 'doctag' },
      { begin: '#' },
    ],
  });

  var DOC_COMMENT = {
    scope: 'comment doctag',
    begin: '##',
    end: '$',
    relevance: 2,
  };

  var PLAIN_COMMENT = {
    scope: 'comment',
    begin: /#(?!#)/,
    end: '$',
    relevance: 0,
  };

  // String interpolation: {expr} inside strings
  var INTERPOLATION = {
    scope: 'subst',
    begin: /\{/,
    end: /\}/,
    keywords: KEYWORDS,
    contains: [], // filled later to allow nesting
  };

  // Double-quoted strings with interpolation and escape sequences
  var STRING = {
    scope: 'string',
    begin: '"',
    end: '"',
    contains: [
      hljs.BACKSLASH_ESCAPE,
      INTERPOLATION,
    ],
  };

  // Dice notation: 2d6, 3D20, 1d20
  var DICE = {
    scope: 'number',
    match: /\b[0-9]{1,3}[dD][0-9]{1,3}\b/,
    relevance: 5,
  };

  // Numbers: integers (decimal, hex, binary, octal) and floats
  var NUMBER = {
    scope: 'number',
    variants: [
      { match: /\b0[xX][0-9a-fA-F][0-9a-fA-F_]*\b/ },
      { match: /\b0[bB][01][01_]*\b/ },
      { match: /\b0[oO][0-7][0-7_]*\b/ },
      { match: /\b[0-9][0-9_]*\.[0-9][0-9_]*(?:[eE][+-]?[0-9]+)?\b/ },
      { match: /\b\.[0-9][0-9_]*(?:[eE][+-]?[0-9]+)?\b/ },
      { match: /\b[0-9][0-9_]*[eE][+-]?[0-9]+\b/ },
      { match: /\b[0-9][0-9_]*\b/ },
    ],
    relevance: 0,
  };

  // Decorator annotations: @entry, @fluent("alias"), @timed(10.0)
  var DECORATOR = {
    scope: 'meta',
    begin: /@[a-zA-Z_]\w*/,
    relevance: 5,
  };

  // Label definition: label name {
  var LABEL_DEF = {
    match: [/\blabel\b/, /\s+/, /[a-zA-Z_]\w*/],
    scope: {
      1: 'keyword',
      3: 'title.function',
    },
  };

  // Jump target: jump label_name, jump module.label
  var JUMP_TARGET = {
    match: [/\bjump\b/, /\s+/, /[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*/],
    scope: {
      1: 'keyword',
      3: 'title.function',
    },
  };

  // Function definition: fn name(
  var FN_DEF = {
    match: [/\bfn\b/, /\s+/, /[a-zA-Z_]\w*/],
    scope: {
      1: 'keyword',
      3: 'title.function',
    },
  };

  // Decorator definition: decorator name<
  var DECORATOR_DEF = {
    match: [/\bdecorator\b/, /\s+/, /[a-zA-Z_]\w*/],
    scope: {
      1: 'keyword',
      3: 'title.function',
    },
  };

  // Enum definition: enum Name {
  var ENUM_DEF = {
    match: [/\benum\b/, /\s+/, /[A-Z]\w*/],
    scope: {
      1: 'keyword',
      3: 'title.class',
    },
  };

  // Struct definition: struct Name {
  var STRUCT_DEF = {
    match: [/\bstruct\b/, /\s+/, /[A-Z]\w*/],
    scope: {
      1: 'keyword',
      3: 'title.class',
    },
  };

  // Import path strings: import "file.urd" as alias
  var IMPORT_PATH = {
    match: [/\bimport\b/, /\s+/, /"[^"]*"/],
    scope: {
      1: 'keyword',
      3: 'string',
    },
  };

  // Type annotations after colon: : int, : str, : Faction
  var TYPE_ANNOTATION = {
    match: [/:/, /\s*/, /\b(?:int|float|bool|str|null|list|map|dice|range)\b/],
    scope: {
      1: 'punctuation',
      3: 'type',
    },
  };

  // Named types (capitalized): : Faction, : Character, : Item
  var NAMED_TYPE = {
    match: [/:/, /\s*/, /\b[A-Z]\w*\b/],
    scope: {
      1: 'punctuation',
      3: 'type',
    },
  };

  // Return type arrow: -> int, -> str
  var RETURN_TYPE = {
    match: [/->/, /\s*/, /\b(?:int|float|bool|str|null|list|map|dice|range)\b/],
    scope: {
      1: 'punctuation',
      3: 'type',
    },
  };

  // Return type arrow with named type: -> Faction
  var RETURN_NAMED_TYPE = {
    match: [/->/, /\s*/, /\b[A-Z]\w*\b/],
    scope: {
      1: 'punctuation',
      3: 'type',
    },
  };

  // Enum variant access: Faction.Guild, Item.Rare
  var ENUM_ACCESS = {
    match: /\b[A-Z]\w*\.[A-Z]\w*\b/,
    scope: 'variable.constant',
    relevance: 3,
  };

  // Dialogue speaker: identifier followed by colon at start-ish of a line
  var SPEAKER = {
    match: /^\s*[a-zA-Z_]\w*\s*:/,
    scope: 'tag',
    relevance: 2,
  };

  // Map literal start: :{
  var DICT_START = {
    match: /:\{/,
    scope: 'punctuation',
    relevance: 0,
  };

  // Range operators
  var RANGE_OP = {
    match: /\.\.=?/,
    scope: 'operator',
    relevance: 0,
  };

  // Operators
  var OPERATORS = {
    match: /[+\-*/%]=?|[!=<>]=|<<|>>|&&|\|\||[&|^~]/,
    scope: 'operator',
    relevance: 0,
  };

  // Wildcard in match/menu
  var WILDCARD = {
    match: /\b_\b/,
    scope: 'keyword',
    relevance: 0,
  };

  // Fill in interpolation contents
  INTERPOLATION.contains = [
    STRING,
    NUMBER,
    DICE,
    ENUM_ACCESS,
  ];

  return {
    name: 'Urd',
    aliases: ['urd'],
    case_insensitive: false,
    keywords: KEYWORDS,
    contains: [
      DOC_COMMENT,
      PLAIN_COMMENT,
      LABEL_DEF,
      JUMP_TARGET,
      FN_DEF,
      DECORATOR_DEF,
      ENUM_DEF,
      STRUCT_DEF,
      IMPORT_PATH,
      DECORATOR,
      RETURN_TYPE,
      RETURN_NAMED_TYPE,
      TYPE_ANNOTATION,
      NAMED_TYPE,
      ENUM_ACCESS,
      SPEAKER,
      STRING,
      DICE,
      NUMBER,
      DICT_START,
      RANGE_OP,
      OPERATORS,
      WILDCARD,
    ],
  };
});

// Re-highlight any urd code blocks that were rendered before this script loaded.
document.querySelectorAll('code.language-urd').forEach(function(block) {
  hljs.highlightElement(block);
});
