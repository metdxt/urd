# Dice Roller

Urd scripts can roll dice using the built-in dice syntax (e.g. `3d6`, `1d20`). Under the hood, every dice expression is resolved by a **`DiceRoller`** trait object owned by the VM's environment. The default implementation uses a standard random number generator, but you can replace it with your own — most commonly for deterministic testing.

## The `DiceRoller` Trait

```rust
pub trait DiceRoller: Send + Sync {
    /// Roll `count` dice each with `sides` faces and return each individual result.
    ///
    /// Each element is in the range `1..=sides`.
    fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64>;

    /// Roll and return the total (sum of all individual results).
    /// Defaults to summing `roll_individual`.
    fn roll(&self, count: u32, sides: u32) -> i64 {
        self.roll_individual(count, sides).iter().sum()
    }
}
```

The trait has two methods:

- **`roll_individual`** — the core method you must implement. Returns a `Vec<i64>` with one entry per die rolled, each in the range `1..=sides`.
- **`roll`** — a convenience method that sums the individual results. You generally don't need to override this unless you want to optimize the common case.

Both methods must be safe to call from any thread (`Send + Sync`).

## Default Implementation

The `DefaultDiceRoller` uses `fastrand` to produce random numbers:

```rust
pub struct DefaultDiceRoller;

impl DiceRoller for DefaultDiceRoller {
    fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64> {
        (0..count)
            .map(|_| fastrand::u32(1..=sides) as i64)
            .collect()
    }
}
```

This is used by default — you don't need to configure anything if standard random dice are all you need.

## Custom Dice Rollers

### Deterministic Roller for Testing

The most common use case for a custom roller is deterministic testing. If your game logic depends on dice outcomes, you need reproducible results in tests:

```rust
struct FixedRoller;

impl DiceRoller for FixedRoller {
    fn roll_individual(&self, count: u32, _sides: u32) -> Vec<i64> {
        vec![3; count as usize] // always rolls 3
    }
}
```

### Seeded Roller for Replays

For replay systems or save/load, you might want a seeded RNG:

```rust
use std::sync::Mutex;

struct SeededRoller {
    rng: Mutex<fastrand::Rng>,
}

impl SeededRoller {
    fn new(seed: u64) -> Self {
        SeededRoller {
            rng: Mutex::new(fastrand::Rng::with_seed(seed)),
        }
    }
}

impl DiceRoller for SeededRoller {
    fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64> {
        let mut rng = self.rng.lock().unwrap();
        (0..count)
            .map(|_| rng.u32(1..=sides) as i64)
            .collect()
    }
}
```

### Weighted / Narrative Roller

Some games intentionally skew dice in the player's favour for narrative pacing:

```rust
struct NarrativeRoller {
    bias: f64, // 0.0 = fair, 1.0 = always max
}

impl DiceRoller for NarrativeRoller {
    fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64> {
        (0..count)
            .map(|_| {
                let base = fastrand::u32(1..=sides) as f64;
                let max = sides as f64;
                let biased = base + (max - base) * self.bias;
                biased.round() as i64
            })
            .collect()
    }
}
```

## Setting the Dice Roller

The dice roller is set on the VM's `Environment` via the `set_dice_roller` method:

```rust
env.set_dice_roller(Box::new(FixedRoller));
```

> **Note:** The `set_dice_roller` method is `pub` on `Environment`, but the VM currently only exposes an immutable `env()` accessor. Within the crate (e.g. in tests), you can access it through `vm.state.env.set_dice_roller(...)`. A public `with_dice_roller` builder method on `Vm` may be added in a future release.

For now, the practical approach for setting a custom roller from outside the crate is to configure the environment before constructing the VM, or to use the internal test helper pattern:

```rust
fn build_vm_with_roller(ast: Ast, roller: impl DiceRoller + 'static) -> Vm {
    let graph = Compiler::compile(&ast).expect("compile failed");
    let mut vm = Vm::new(graph, DecoratorRegistry::new()).expect("vm init failed");
    // Internal access — available within the urd crate
    vm.state.env.set_dice_roller(Box::new(roller));
    vm
}
```

## Dice in Urd Scripts

From the script author's perspective, dice expressions work naturally:

```urd
let damage = 2d6 + 3
let check = 1d20

if check >= 15 {
    narrator: "Critical hit! You deal {damage} damage."
} else {
    narrator: "You miss."
}
```

The VM calls your `DiceRoller` implementation whenever it evaluates a dice literal. The `roll_individual` method is called first, and the individual results are available as a `Roll` value. When used in arithmetic, the roll is automatically summed via the `roll` method.

## Testing with Dice

When writing tests for scripts that involve dice, always inject a deterministic roller:

```rust
#[test]
fn test_critical_hit_path() {
    struct AlwaysCrit;
    impl DiceRoller for AlwaysCrit {
        fn roll_individual(&self, count: u32, sides: u32) -> Vec<i64> {
            vec![sides as i64; count as usize] // always max roll
        }
    }

    // Set up VM with the fixed roller, then assert the dialogue
    // follows the critical hit branch.
}
```

This pattern lets you test every branch of dice-dependent logic without relying on probability.