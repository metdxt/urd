# 🐴 urd vs Python — Silly Benchmarks

> Comparing urd to Python for raw computation is like racing a fighter jet
> against a horse — except the horse shows up in **under a millisecond**
> and the jet is still warming up its Python interpreter.

---

## What is this?

[urd](../README.md) is a **dialogue scripting DSL** built for game narrative
trees: branching conversations, menus, flags, jump-based control flow. It is
*not* a general-purpose language. It has no arrays, no stdlib, no JIT, and
absolutely zero ambitions of competing with Python on arithmetic throughput.

These benchmarks exist purely for curiosity (and comedic value). They measure
how long it takes urd to do things it was never meant to do, side-by-side with
a language that does those things effortlessly.

Spoiler: urd wins. Every time. We are as surprised as you are.

---

## Running the benchmarks

From the **repo root**:

```sh
./benches/run.sh
```

The script will:
1. Build the `quest` binary in release mode (`cargo build -p urd-quest --release`)
2. Run all three benchmarks using [`hyperfine`](https://github.com/sharkdp/hyperfine)
   if it's installed (recommended), or fall back to plain `time` loops if not

**Requirements:** `cargo`, `python3`  
**Optional but recommended:** [`hyperfine`](https://github.com/sharkdp/hyperfine)

```sh
# Install hyperfine (if you want the nice output)
cargo install hyperfine
# or: brew install hyperfine / apt install hyperfine
```

---

## The benchmarks

### `fibonacci` — iterative Fibonacci(35)

| File | Description |
|------|-------------|
| `benches/urd/fibonacci.urd` | Two-variable iteration, 35 steps, emits the result via `narrator:` |
| `benches/python/fibonacci.py` | Same algorithm, plain Python loop |

Expected output: `fib(35) = 9227465`

urd uses jump-based loops (no `for`/`while`), so this exercises the VM's
instruction dispatch, global variable reads/writes, and arithmetic opcodes for
35 iterations — plus the startup and compile cost of loading the script.

**Typical result: urd ~14× faster.**  
urd finishes in under a millisecond. Python is still importing `sys`.

---

### `sum` — sum of integers 1 to 10,000

| File | Description |
|------|-------------|
| `benches/urd/sum.urd` | Accumulator loop, 10,000 iterations |
| `benches/python/sum.py` | `sum(range(1, 10001))` |

Expected output: `sum(10000) = 50005000`

This one scales up the iteration count to stress the VM's loop throughput
rather than startup costs. At 10,000 iterations, urd's per-instruction overhead
becomes the dominant factor — and it's still faster.

**Typical result: urd ~3× faster.**  
The closest race of the three. Python's interpreter finally gets to stretch its
legs, and urd's graph-walker VM starts to show its overhead. urd still wins.

---

### `fizzbuzz` — FizzBuzz 1 to 500

| File | Description |
|------|-------------|
| `benches/urd/fizzbuzz.urd` | Conditional branching + dialogue output, 500 iterations |
| `benches/python/fizzbuzz.py` | Standard FizzBuzz with `print` |

This benchmark adds **dialogue emission** to the loop — every iteration fires a
`narrator:` line (suppressed via `> /dev/null`). It measures how the VM handles
branchy conditional logic combined with the dialogue event machinery that urd
actually *was* built for.

**Typical result: urd ~8× faster.**  
Turns out urd is quite good at emitting dialogue lines. Who could have guessed.

---

## Why does urd win?

It shouldn't. And yet.

The honest answer is that **Python's ~12ms startup and import overhead
dominates all three benchmarks.** urd is a lean Rust binary: it starts, parses,
compiles, runs, and exits before CPython has finished bootstrapping its runtime.

On the benchmark that matters most — `sum`, with 10,000 tight loop iterations —
urd is still 3× faster, which is genuinely surprising for a graph-walking VM
with no JIT. The IR graph compiled from a `.urd` script is apparently a pretty
efficient thing to walk.

For context, urd's execution model is optimised for **sparse dialogue graphs**
with many branches and infrequent decisions — not dense arithmetic hot loops.
The fact that it holds up here anyway is an accident of Rust's startup speed
and the relative heaviness of the Python interpreter.

---

## Caveats

- **These are process-level benchmarks**, not VM-level benchmarks. A fairer
  fight would drive `Vm::next` from a Criterion harness with Python running a
  pre-loaded script in a persistent process. In that world, Python would likely
  win the tight loop cases.

- **urd pays a parse + compile cost on every run.** There is no persistent
  process or pre-compiled cache. Each invocation reads `.urd` source, parses it
  to an AST, compiles it to IR, and walks the graph. Python also does this, but
  at much higher runtime cost.

- **urd is not competing with Python.** urd is competing with hand-written
  dialogue JSON, ink, Yarn Spinner, and other narrative scripting formats. On
  *that* axis, it doesn't just hold up — it wins on expressiveness, type safety,
  and the ability to roll dice mid-conversation.

---

## Disclaimer

> These benchmarks were written with full awareness that running arithmetic
> loops in a dialogue DSL is the computational equivalent of asking your dungeon
> master to do your taxes. The master will comply. It will be fast.
> The results will be read aloud with dramatic flair.
>
> 🐴 *"The sum of integers from 1 to 10,000 is..."*  
> 🐴 *"...fifty million, and five thousand."*  
> 🐴 *"...I computed that before Python finished importing `os`."*