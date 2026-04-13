#![allow(clippy::expect_used, missing_docs)]
//! In-process benchmarks for the core Urd pipeline stages.
//!
//! Run with: `cargo bench -p urd`
//! Compile-check only: `cargo bench --no-run -p urd`

use criterion::{Criterion, black_box, criterion_group, criterion_main};

use urd::compiler::loader::parse_source;
use urd::compiler::Compiler;
use urd::ir::VmStep;
use urd::vm::registry::DecoratorRegistry;
use urd::vm::Vm;

/// The example script used for all benchmarks.
const CAVE_SRC: &str = include_str!("../../../examples/quest/cave.urd");

// ─── Lexing ──────────────────────────────────────────────────────────────────

fn bench_lex(c: &mut Criterion) {
    use urd::lexer::lex_src;

    c.bench_function("lex/cave.urd", |b| {
        b.iter(|| {
            // Consume every token so the lexer actually does the work.
            let count: usize = lex_src(black_box(CAVE_SRC)).count();
            black_box(count);
        });
    });
}

// ─── Parsing ─────────────────────────────────────────────────────────────────

fn bench_parse(c: &mut Criterion) {
    c.bench_function("parse/cave.urd", |b| {
        b.iter(|| {
            let ast = parse_source(black_box(CAVE_SRC)).expect("parse failed");
            black_box(ast);
        });
    });
}

// ─── Compilation ─────────────────────────────────────────────────────────────

fn bench_compile(c: &mut Criterion) {
    let ast = parse_source(CAVE_SRC).expect("parse failed");

    c.bench_function("compile/cave.urd", |b| {
        b.iter(|| {
            let graph = Compiler::compile(black_box(&ast)).expect("compile failed");
            black_box(graph);
        });
    });
}

// ─── Full pipeline (lex → parse → compile) ───────────────────────────────────

fn bench_full_pipeline(c: &mut Criterion) {
    c.bench_function("pipeline/cave.urd", |b| {
        b.iter(|| {
            let ast = parse_source(black_box(CAVE_SRC)).expect("parse failed");
            let graph = Compiler::compile(&ast).expect("compile failed");
            black_box(graph);
        });
    });
}

// ─── VM step throughput ──────────────────────────────────────────────────────

/// Maximum number of `Vm::next` calls per run.  `cave.urd` loops infinitely
/// when always picking choice 0 (open_chest → cave_depths → open_chest …),
/// so we cap the run to keep the benchmark deterministic and finite.
const VM_STEP_CAP: usize = 10_000;

/// Drive the VM for up to [`VM_STEP_CAP`] steps, always picking the first
/// choice when prompted.  Returns the total number of `Vm::next` calls made.
fn run_vm_to_end(graph: urd::IrGraph) -> usize {
    let registry = DecoratorRegistry::new();
    let mut vm = Vm::new(graph, registry).expect("vm init failed");
    // Disable the built-in budget — we enforce our own cap below.
    vm.set_step_budget(None);
    let mut steps: usize = 0;
    let mut pending_choice: Option<usize> = None;

    loop {
        let step = vm.next(pending_choice.take());
        steps += 1;
        match step {
            VmStep::Event(urd::ir::Event::Choice { .. }) => {
                // Always pick the first option to keep the run deterministic.
                pending_choice = Some(0);
            }
            VmStep::Event(_) => {}
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("vm error: {e}"),
            _ => {}
        }
        if steps >= VM_STEP_CAP {
            break;
        }
    }
    steps
}

fn bench_vm_step(c: &mut Criterion) {
    let ast = parse_source(CAVE_SRC).expect("parse failed");
    let graph = Compiler::compile(&ast).expect("compile failed");

    // Sanity check: make sure the script actually runs to completion.
    let total_steps = run_vm_to_end(graph.clone());
    assert!(total_steps > 0, "cave.urd produced zero VM steps");

    c.bench_function("vm_step/cave.urd", |b| {
        b.iter(|| {
            let steps = run_vm_to_end(black_box(graph.clone()));
            black_box(steps);
        });
    });
}

// ─── Criterion harness ──────────────────────────────────────────────────────

criterion_group!(
    benches,
    bench_lex,
    bench_parse,
    bench_compile,
    bench_full_pipeline,
    bench_vm_step,
);
criterion_main!(benches);
