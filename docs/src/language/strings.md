# Strings & Interpolation

Strings are one of the most frequently used types in Urd. They appear in dialogue lines, menu labels, interpolated messages, and general-purpose text manipulation. Urd strings are UTF-8, support embedded expressions via interpolation, and come with a rich set of built-in methods.

## String Literals

String literals are delimited by double quotes:

```urd
let greeting = "Hello, world!"
let empty = ""
```

Strings can span multiple lines. Newlines within the quotes are preserved as-is:

```urd
let poem = "Roses are red,
Violets are blue,
Urd is for dialogue,
And so are you."
```

## Escape Sequences

The following escape sequences are recognized inside string literals:

| Sequence | Meaning              |
|----------|----------------------|
| `\"`     | Literal double quote |
| `\\`     | Literal backslash    |
| `\n`     | Newline              |
| `\t`     | Tab                  |
| `\{`     | Literal open brace (suppresses interpolation) |

Example:

```urd
let escaped = "She said, \"Run!\""
let path = "C:\\Users\\hero"
let literal_brace = "Use \{curly braces\} for interpolation"
```

## String Interpolation

Embed any expression inside a string by wrapping it in curly braces `{…}`. The expression is evaluated at runtime and its result is converted to a string:

```urd
global gold = 50
narrator: "You have {gold} gold coins."
# → "You have 50 gold coins."
```

Interpolation supports arbitrary expressions, including arithmetic and function calls:

```urd
narrator: "You have {gold * 2} gold after doubling."
narrator: "Health: {health} / {max_health}"
```

### Field Access in Interpolation

You can access fields on maps and structs directly inside interpolation braces:

```urd
const hero = :{ name: "Aldric", class: "Paladin" }
narrator: "{hero.name} the {hero.class} enters the arena!"
# → "Aldric the Paladin enters the arena!"
```

### Nested Interpolation

Interpolation expressions can themselves contain strings, though this is rarely needed:

```urd
let label = "gold"
narrator: "Your {label} count is {gold}."
```

### Suppressing Interpolation

To include a literal `{` in a string without triggering interpolation, escape it with a backslash:

```urd
narrator: "Use \{braces\} for interpolation in Urd."
# → "Use {braces} for interpolation in Urd."
```

## Substring Checking with `in`

The `in` operator can test whether one string is a substring of another:

```urd
let greeting = "hello, world"

if "hello" in greeting {
    narrator: "The greeting starts friendly."
}

if "world" in greeting {
    narrator: "The world is mentioned."
}
```

Both the left-hand side and right-hand side must be `Str`. This is equivalent
to calling `.contains()`, but reads more naturally in conditions. For more
details on the `in` operator's full capabilities, see
[Operators](./operators.md#membership-in).

## String Methods

Urd strings come with a set of built-in methods. All methods are **pure** — they return new values and never mutate the receiver. Here are a few commonly used ones:

```urd
let name = "  Aldric the Bold  "
name.trim()                   # → "Aldric the Bold"
name.len()                    # → 20
name.trim().to_upper()        # → "ALDRIC THE BOLD"

"hello world".contains("world")  # → true
"a,b,c".split(",")              # → ["a", "b", "c"]
"ha".repeat(3)                   # → "hahaha"
"Hello".slice(1, 4)             # → "ell"
"42".to_int()                    # → 42
```

Strings also support predicates like `.starts_with()` and `.ends_with()`, character decomposition with `.chars()` and `.lines()`, whitespace trimming variants (`.trim_start()`, `.trim_end()`), and more.

For the complete list of string methods with signatures and descriptions, see the [String Methods](../reference/builtin-methods.md#string-methods) reference.

## Strings in Dialogue

Strings are the backbone of dialogue lines. Every line a speaker delivers is a string, and interpolation works naturally:

```urd
global gold = 50
const merchant = :{ name: "Elara", name_color: "yellow" }

merchant: "I see you have {gold} gold. That sword costs 100."

if gold >= 100 {
    merchant: "Pleasure doing business with you!"
    gold = gold - 100
} else {
    merchant: "Come back when you can afford it."
}
```

## Unicode Support

Urd strings are fully UTF-8. Methods like `.len()`, `.slice()`, and `.chars()` operate on Unicode characters, not raw bytes:

```urd
let cyrillic = "Привет"
cyrillic.len()    # → 6 (six Cyrillic characters)
cyrillic.chars()  # → ["П", "р", "и", "в", "е", "т"]
```
