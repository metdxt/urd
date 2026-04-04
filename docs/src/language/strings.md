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

## String Methods

Urd strings come with a set of built-in methods. All methods return new values — strings are never mutated in place.

### Querying

| Method        | Returns | Description                            |
|---------------|---------|----------------------------------------|
| `.len()`      | `int`   | Number of Unicode characters (not bytes) |
| `.is_empty()` | `bool`  | `true` if the string has zero length   |

```urd
let s = "Hello"
s.len()        # → 5
"".is_empty()  # → true
```

### Case Conversion

| Method        | Returns | Description            |
|---------------|---------|------------------------|
| `.to_upper()` | `str`   | Uppercase copy         |
| `.to_lower()` | `str`   | Lowercase copy         |

```urd
"hello".to_upper()  # → "HELLO"
"WORLD".to_lower()  # → "world"
```

### Whitespace Trimming

| Method          | Returns | Description                          |
|-----------------|---------|--------------------------------------|
| `.trim()`       | `str`   | Remove leading and trailing whitespace |
| `.trim_start()` | `str`   | Remove leading whitespace only       |
| `.trim_end()`   | `str`   | Remove trailing whitespace only      |

```urd
"  spaced  ".trim()        # → "spaced"
"  spaced  ".trim_start()  # → "spaced  "
"  spaced  ".trim_end()    # → "  spaced"
```

### Predicates

| Method             | Returns | Description                                 |
|--------------------|---------|---------------------------------------------|
| `.contains(s)`     | `bool`  | `true` if `s` is a substring                |
| `.starts_with(s)`  | `bool`  | `true` if the string begins with `s`        |
| `.ends_with(s)`    | `bool`  | `true` if the string ends with `s`          |

```urd
"Hello, world!".contains("world")      # → true
"Hello, world!".starts_with("Hello")   # → true
"Hello, world!".ends_with("!")         # → true
```

### Splitting & Replacing

| Method              | Returns    | Description                            |
|---------------------|------------|----------------------------------------|
| `.split(sep)`       | `list`     | Split into a list of strings on `sep`  |
| `.replace(from, to)`| `str`      | Replace all occurrences of `from` with `to` |

```urd
"a,b,c".split(",")             # → ["a", "b", "c"]
"hello world".replace("o", "0") # → "hell0 w0rld"
```

### Slicing

| Method                  | Returns | Description                                   |
|-------------------------|---------|-----------------------------------------------|
| `.slice(start)`         | `str`   | Substring from `start` to end (character-based) |
| `.slice(start, end)`    | `str`   | Substring from `start` to `end` (exclusive)   |

Both bounds are **character-based** (Unicode scalar values), not byte offsets. Negative indices count from the end. Out-of-bounds values are clamped silently.

```urd
"Hello".slice(1)      # → "ello"
"Hello".slice(1, 4)   # → "ell"
"Hello".slice(-3)     # → "llo"
```

### Character Decomposition

| Method     | Returns    | Description                                     |
|------------|------------|-------------------------------------------------|
| `.chars()` | `list`     | List of single-character strings                |
| `.lines()` | `list`     | Split on newline boundaries (`\n` and `\r\n`)   |

```urd
"abc".chars()              # → ["a", "b", "c"]
"line1\nline2".lines()     # → ["line1", "line2"]
```

### Repetition

| Method        | Returns | Description                              |
|---------------|---------|------------------------------------------|
| `.repeat(n)`  | `str`   | Repeat the string `n` times             |

```urd
"ha".repeat(3)  # → "hahaha"
"ha".repeat(0)  # → ""
```

### Type Coercion

| Method       | Returns | Description                                  |
|--------------|---------|----------------------------------------------|
| `.to_int()`  | `int`   | Parse the string as an integer (trims whitespace) |
| `.to_float()`| `float` | Parse the string as a float (trims whitespace)    |

These methods return an error at runtime if the string cannot be parsed:

```urd
"42".to_int()       # → 42
" 3.14 ".to_float() # → 3.14
"abc".to_int()      # → runtime error
```

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
