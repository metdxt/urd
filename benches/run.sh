#!/usr/bin/env sh
# benches/run.sh — Silly benchmark: urd dialogue DSL vs Python
#
# Run from the repo root:
#   ./benches/run.sh
#
# Requires: cargo, python3
# Optional: hyperfine (https://github.com/sharkdp/hyperfine)

set -e

# ── Colours ───────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
RESET='\033[0m'

# ── Header ────────────────────────────────────────────────────────────────────
printf "${BOLD}${YELLOW}"
cat <<'BANNER'

  ██████╗ ██╗   ██╗████████╗██╗  ██╗ ██████╗ ███╗   ██╗
  ██╔══██╗╚██╗ ██╔╝╚══██╔══╝██║  ██║██╔═══██╗████╗  ██║
  ██████╔╝ ╚████╔╝    ██║   ███████║██║   ██║██╔██╗ ██║
  ██╔═══╝   ╚██╔╝     ██║   ██╔══██║██║   ██║██║╚██╗██║
  ██║        ██║      ██║   ██║  ██║╚██████╔╝██║ ╚████║
  ╚═╝        ╚═╝      ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

BANNER
printf "${RESET}"
printf "${BOLD}${CYAN}  🐍  Python  vs  urd  ⚔️   SILLY BENCHMARKS${RESET}\n"
printf "${CYAN}  ──────────────────────────────────────────────────────────${RESET}\n"
printf "  urd is a dialogue scripting DSL for game narrative trees.\n"
printf "  It was not designed for arithmetic. Not even slightly.\n"
printf "  And yet, here we are.\n"
printf "${CYAN}  ──────────────────────────────────────────────────────────${RESET}\n\n"

# ── Sanity: must be run from the repo root ────────────────────────────────────
if [ ! -f "Cargo.toml" ]; then
    printf "${RED}Error:${RESET} Run this script from the repo root, e.g.:\n"
    printf "  ./benches/run.sh\n"
    exit 1
fi

# ── Build ─────────────────────────────────────────────────────────────────────
printf "${BOLD}${GREEN}[1/2] Building quest (release)...${RESET}\n"
cargo build -p urd-quest --release 2>&1
printf "${GREEN}      ✓ done${RESET}\n\n"

QUEST="./target/release/quest"

# ── Detect hyperfine ──────────────────────────────────────────────────────────
if command -v hyperfine > /dev/null 2>&1; then
    HAS_HYPERFINE=1
    printf "${BOLD}${GREEN}[2/2] hyperfine found — using it for accurate measurements${RESET}\n\n"
else
    HAS_HYPERFINE=0
    printf "${BOLD}${YELLOW}[2/2] hyperfine not found — falling back to manual timing (5 runs each)${RESET}\n"
    printf "      Install hyperfine for nicer output: https://github.com/sharkdp/hyperfine\n\n"
fi

# ── Benchmark runner helpers ──────────────────────────────────────────────────

# run_hyperfine <label> <urd_script> <python_script>
run_hyperfine() {
    _label="$1"
    _urd_script="$2"
    _py_script="$3"

    printf "${BOLD}${CYAN}  ── %s ──${RESET}\n" "$_label"
    hyperfine \
        --warmup 3 \
        --runs 10 \
        "${QUEST} run ${_urd_script} < /dev/null > /dev/null" \
        "python3 ${_py_script} > /dev/null"
    printf "\n"
}

# run_timed <label> <urd_script> <python_script>
run_timed() {
    _label="$1"
    _urd_script="$2"
    _py_script="$3"
    _runs=5

    printf "${BOLD}${CYAN}  ── %s ──${RESET}\n" "$_label"

    # urd
    printf "  ${YELLOW}urd${RESET}  (%d runs):\n" "$_runs"
    _n=1
    while [ "$_n" -le "$_runs" ]; do
        _run=$_n
        { time ${QUEST} run "${_urd_script}" < /dev/null > /dev/null; } 2>&1 \
            | grep real \
            | awk -v run="$_run" '{printf "    run %d: %s\n", run, $2}' \
            || true
        _n=$((_n + 1))
    done

    printf "\n"

    # python
    printf "  ${GREEN}python3${RESET} (%d runs):\n" "$_runs"
    _n=1
    while [ "$_n" -le "$_runs" ]; do
        _run=$_n
        { time python3 "${_py_script}" > /dev/null; } 2>&1 \
            | grep real \
            | awk -v run="$_run" '{printf "    run %d: %s\n", run, $2}' \
            || true
        _n=$((_n + 1))
    done

    printf "\n"
}

# ── Run benchmarks ────────────────────────────────────────────────────────────

if [ "$HAS_HYPERFINE" = "1" ]; then
    run_hyperfine "fibonacci(35) — 35 arithmetic iterations" \
        "benches/urd/fibonacci.urd" \
        "benches/python/fibonacci.py"

    run_hyperfine "sum(1..10000) — 10,000 accumulator iterations" \
        "benches/urd/sum.urd" \
        "benches/python/sum.py"

    run_hyperfine "fizzbuzz(1..500) — 500 conditional + dialogue iterations" \
        "benches/urd/fizzbuzz.urd" \
        "benches/python/fizzbuzz.py"
else
    run_timed "fibonacci(35) — 35 arithmetic iterations" \
        "benches/urd/fibonacci.urd" \
        "benches/python/fibonacci.py"

    run_timed "sum(1..10000) — 10,000 accumulator iterations" \
        "benches/urd/sum.urd" \
        "benches/python/sum.py"

    run_timed "fizzbuzz(1..500) — 500 conditional + dialogue iterations" \
        "benches/urd/fizzbuzz.urd" \
        "benches/python/fizzbuzz.py"
fi

# ── Footer ────────────────────────────────────────────────────────────────────
printf "${BOLD}${CYAN}  ──────────────────────────────────────────────────────────${RESET}\n"
printf "  ${BOLD}Disclaimer:${RESET} Comparing urd to Python for raw computation is like\n"
printf "  racing a horse against a fighter jet — but the horse has\n"
printf "  ${BOLD}much better dialogue options.${RESET} 🐴✨\n"
printf "${CYAN}  ──────────────────────────────────────────────────────────${RESET}\n\n"
