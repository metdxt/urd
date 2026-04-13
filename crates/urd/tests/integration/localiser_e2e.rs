#![allow(missing_docs)]

//! End-to-end localisation integration test.
//!
//! Exercises the full pipeline:
//!
//! 1. Parse + compile an Urd script with `@fluent` globals and `@id`-decorated dialogue.
//! 2. Call `generate_ftl()` and verify the output contains correct message IDs and
//!    interpolation variables.
//! 3. Feed the generated FTL into a `fluent_bundle::FluentBundle`.
//! 4. Create a `Localizer` impl backed by that bundle.
//! 5. Run the VM with `with_localizer(…)`.
//! 6. Verify `Event::Dialogue` events contain `localized_text` matching the Fluent
//!    translations, and `Event::Choice` options carry `localized_label`.
//!
//! This is the **only** way to prove the `@id` → compiler → FTL → VM → localised
//! output pipeline works end-to-end.

use std::collections::HashMap;
use std::sync::Arc;

use fluent_bundle::bundle::FluentBundle;
use fluent_bundle::{FluentArgs, FluentResource, FluentValue};
use unic_langid::langid;

use urd::{
    Event, RuntimeValue, VmStep,
    compiler::Compiler,
    compiler::loader::parse_source,
    loc::{Localizer, ftl::generate_ftl},
    vm::{Vm, registry::DecoratorRegistry},
};

// ─── FluentLocalizer: real Fluent-backed Localizer impl ──────────────────────

/// A [`Localizer`] backed by a real concurrent [`FluentBundle`].
///
/// Uses the concurrent intl-memoizer so that the bundle is `Send + Sync`,
/// satisfying the `Localizer` trait bounds.
///
/// Converts `RuntimeValue` vars into `FluentValue` args and formats messages
/// through the Fluent resolver — exactly as a game integration would.
struct FluentLocalizer {
    bundle: FluentBundle<FluentResource, intl_memoizer::concurrent::IntlLangMemoizer>,
}

impl FluentLocalizer {
    /// Build a localizer from raw FTL source.
    fn new(ftl_source: &str) -> Self {
        let resource =
            FluentResource::try_new(ftl_source.to_owned()).expect("FTL source should parse");
        let mut bundle = FluentBundle::new_concurrent(vec![langid!("en-US")]);
        bundle
            .add_resource(resource)
            .expect("bundle should accept the resource");
        Self { bundle }
    }
}

impl Localizer for FluentLocalizer {
    fn localize(&self, id: &str, vars: &HashMap<String, RuntimeValue>) -> Option<String> {
        let msg = self.bundle.get_message(id)?;
        let pattern = msg.value()?;

        let mut args = FluentArgs::new();
        for (key, val) in vars {
            match val {
                RuntimeValue::Int(n) => {
                    args.set(key.as_str(), FluentValue::from(*n as f64));
                }
                RuntimeValue::Float(f) => {
                    args.set(key.as_str(), FluentValue::from(*f));
                }
                RuntimeValue::Bool(b) => {
                    args.set(
                        key.as_str(),
                        FluentValue::from(if *b { "true" } else { "false" }),
                    );
                }
                RuntimeValue::Str(ps) => {
                    args.set(key.as_str(), FluentValue::from(ps.to_string()));
                }
                _ => {}
            }
        }

        let mut errors = vec![];
        let result = self.bundle.format_pattern(pattern, Some(&args), &mut errors);
        Some(result.into_owned())
    }
}

// ─── Test helpers ────────────────────────────────────────────────────────────

/// Build a [`DecoratorRegistry`] that accepts `@id` as a no-op passthrough.
///
/// The `@id("…")` decorator is consumed by the compiler at compile time to
/// override auto-generated loc_ids.  It remains in the IR's decorator list,
/// however, so the VM's validation pass must recognise it.  Registering it as
/// a passthrough is what a real game integration would do.
fn registry_with_id() -> DecoratorRegistry {
    let mut reg = DecoratorRegistry::new();
    reg.register_passthrough("id");
    reg
}

// ─── The script under test ───────────────────────────────────────────────────

/// A self-contained Urd script exercising:
/// - `@fluent` globals (gold, player_name)
/// - `@id`-decorated dialogue (custom loc key slug)
/// - string interpolation in dialogue (`{gold}`, `{player_name}`)
/// - a menu with interpolated option labels
/// - `compile_named` for loc_id generation
///
/// The `@id("merchant-welcome")` override causes the compiler to use
/// `merchant-welcome` as the slug segment, producing a full loc_id of
/// `shop-greet-merchant-welcome` (file_slug-label-slug).
const TEST_SCRIPT: &str = r#"
const narrator = :{ name: "Narrator", name_color: "white" }
const merchant = :{ name: "Elara",    name_color: "yellow" }

@fluent
global gold = 50

@fluent("player_name")
global name = "Hero"

@entry
label greet {
    narrator: "You approach the merchant's stall."

    @id("merchant-welcome")
    merchant: "Welcome, {player_name}! You have {gold} gold."

    menu {
        "Buy potion (10 gold)" {
            gold = gold - 10
            merchant: "Sold! You now have {gold} gold."
            end!()
        }
        "Leave" {
            narrator: "You walk away."
            end!()
        }
    }
}
"#;

// ═════════════════════════════════════════════════════════════════════════════
// Tests
// ═════════════════════════════════════════════════════════════════════════════

// ── Step 1 + 2: compile_named produces loc_ids, generate_ftl is correct ──────

#[test]
fn ftl_contains_expected_message_ids() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");

    // The @id("merchant-welcome") dialogue produces key: shop-greet-merchant-welcome
    // (file_slug + label + @id slug).
    assert!(
        ftl.contains("shop-greet-merchant-welcome ="),
        "FTL must contain the @id-derived key 'shop-greet-merchant-welcome'; got:\n{ftl}"
    );

    // The auto-generated dialogue for narrator's first line.
    assert!(
        ftl.contains("shop-greet-line_1 ="),
        "FTL must contain auto-generated key 'shop-greet-line_1'; got:\n{ftl}"
    );

    // The menu options.
    assert!(
        ftl.contains("buy_potion_10_gold"),
        "FTL must contain an option key derived from 'Buy potion (10 gold)'; got:\n{ftl}"
    );
    assert!(
        ftl.contains("leave"),
        "FTL must contain an option key derived from 'Leave'; got:\n{ftl}"
    );

    // @fluent variables must be documented in comments.
    assert!(
        ftl.contains("$gold"),
        "FTL must mention $gold in @fluent variables comment; got:\n{ftl}"
    );
    assert!(
        ftl.contains("$player_name"),
        "FTL must mention $player_name in @fluent variables comment; got:\n{ftl}"
    );
}

#[test]
fn ftl_contains_interpolation_placeholders() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");

    // The merchant line uses {player_name} and {gold} —
    // the FTL renderer converts these to Fluent syntax: { $player_name }, { $gold }.
    assert!(
        ftl.contains("{ $player_name }"),
        "FTL must convert interpolation to Fluent placeholder for player_name; got:\n{ftl}"
    );
    assert!(
        ftl.contains("{ $gold }"),
        "FTL must convert interpolation to Fluent placeholder for gold; got:\n{ftl}"
    );
}

// ── Step 3 + 4: FTL parses into FluentBundle, FluentLocalizer works ──────────

#[test]
fn generated_ftl_parses_into_fluent_bundle() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");

    // This must not panic — the generated FTL must be valid Fluent syntax.
    let _localizer = FluentLocalizer::new(&ftl);
}

#[test]
fn fluent_localizer_resolves_message_with_vars() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");
    let localizer = FluentLocalizer::new(&ftl);

    // Build a vars map matching what the VM would produce.
    // The interpolation path `player_name` stays as-is (no dots → no hyphens).
    let mut vars = HashMap::new();
    vars.insert("gold".to_string(), RuntimeValue::Int(50));
    vars.insert(
        "player_name".to_string(),
        RuntimeValue::Str(urd::lexer::strings::ParsedString::new_plain("Hero")),
    );

    // The full key is shop-greet-merchant-welcome (file-label-@id_slug).
    let result = localizer.localize("shop-greet-merchant-welcome", &vars);
    assert!(
        result.is_some(),
        "localizer must resolve 'shop-greet-merchant-welcome'; FTL:\n{}",
        ftl
    );
    let text = result.unwrap();
    assert!(
        text.contains("Hero"),
        "resolved text must contain player name 'Hero'; got: {text}"
    );
    assert!(
        text.contains("50"),
        "resolved text must contain gold amount '50'; got: {text}"
    );
}

// ── Step 5 + 6: full VM run with localizer, verify localized_text ────────────

#[test]
fn vm_dialogue_events_carry_localized_text() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");
    let localizer = Arc::new(FluentLocalizer::new(&ftl));

    let vm = Vm::new(graph, registry_with_id()).expect("vm");
    let mut vm = vm.with_localizer(localizer);

    // Collect dialogue events until we hit a Choice or end.
    let mut dialogues: Vec<(Option<String>, Option<String>, Vec<RuntimeValue>)> = Vec::new();
    for _ in 0..50 {
        match vm.next(None) {
            VmStep::Event(Event::Dialogue {
                loc_id,
                localized_text,
                lines,
                ..
            }) => {
                dialogues.push((loc_id, localized_text, lines));
            }
            VmStep::Event(Event::Choice { .. }) => break,
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {e:?}"),
            _ => {}
        }
    }

    // We expect at least 2 dialogue events: narrator intro + merchant welcome.
    assert!(
        dialogues.len() >= 2,
        "expected at least 2 dialogue events, got {}; dialogues: {dialogues:?}",
        dialogues.len()
    );

    // ── Narrator line: auto-generated loc_id ──
    let (ref loc_id_0, ref localized_0, _) = dialogues[0];
    assert!(
        loc_id_0.is_some(),
        "narrator dialogue must have a loc_id (auto-generated)"
    );
    assert!(
        localized_0.is_some(),
        "narrator dialogue must have localized_text when localizer is attached; \
         loc_id={loc_id_0:?}"
    );
    let text_0 = localized_0.as_deref().unwrap();
    assert!(
        text_0.contains("merchant") || text_0.contains("stall") || text_0.contains("approach"),
        "narrator localized text should contain fallback content; got: {text_0}"
    );

    // ── Merchant welcome: @id-overridden loc_id (slug segment, not full replacement) ──
    let (ref loc_id_1, ref localized_1, _) = dialogues[1];
    assert_eq!(
        loc_id_1.as_deref(),
        Some("shop-greet-merchant-welcome"),
        "merchant dialogue must use @id-derived key 'shop-greet-merchant-welcome'"
    );
    assert!(
        localized_1.is_some(),
        "merchant dialogue must have localized_text"
    );
    let text_1 = localized_1.as_deref().unwrap();
    assert!(
        text_1.contains("Hero"),
        "merchant localized text must contain interpolated player_name 'Hero'; got: {text_1}"
    );
    assert!(
        text_1.contains("50"),
        "merchant localized text must contain interpolated gold '50'; got: {text_1}"
    );
}

#[test]
fn vm_choice_options_carry_localized_labels() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");
    let localizer = Arc::new(FluentLocalizer::new(&ftl));

    let vm = Vm::new(graph, registry_with_id()).expect("vm");
    let mut vm = vm.with_localizer(localizer);

    // Advance past dialogue to the Choice event.
    let mut choice_event: Option<Event> = None;
    for _ in 0..50 {
        match vm.next(None) {
            VmStep::Event(evt @ Event::Choice { .. }) => {
                choice_event = Some(evt);
                break;
            }
            VmStep::Event(Event::Dialogue { .. }) => { /* skip */ }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {e:?}"),
            _ => {}
        }
    }

    let choice = choice_event.expect("expected a Choice event");
    if let Event::Choice { options, .. } = &choice {
        assert!(
            options.len() >= 2,
            "expected at least 2 options, got {}",
            options.len()
        );

        // Each option should have a loc_id and a localized_label.
        for opt in options {
            assert!(
                opt.loc_id.is_some(),
                "option '{}' must have a loc_id",
                opt.label
            );
            assert!(
                opt.localized_label.is_some(),
                "option '{}' must have a localized_label when localizer is attached; loc_id={:?}",
                opt.label,
                opt.loc_id
            );
        }

        // The "Buy potion" option label should exist.
        let buy_opt = options.iter().find(|o| o.label.contains("Buy"));
        assert!(buy_opt.is_some(), "expected a 'Buy' option");
        let buy_label = buy_opt.unwrap().localized_label.as_deref().unwrap();
        assert!(
            buy_label.contains("10"),
            "buy option localized label should contain '10' (from interpolation); got: {buy_label}"
        );

        // The "Leave" option.
        let leave_opt = options.iter().find(|o| o.label.contains("Leave"));
        assert!(leave_opt.is_some(), "expected a 'Leave' option");
        let leave_label = leave_opt.unwrap().localized_label.as_deref().unwrap();
        assert!(
            leave_label.contains("Leave"),
            "leave option localized label should contain 'Leave'; got: {leave_label}"
        );
    } else {
        panic!("expected Event::Choice, got: {choice:?}");
    }
}

// ── Full pipeline with translated FTL ────────────────────────────────────────

#[test]
fn vm_with_translated_ftl_produces_translated_output() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");

    // Generate the default FTL to verify the auto-generated keys exist.
    let default_ftl = generate_ftl(&graph, "shop");
    assert!(
        default_ftl.contains("shop-greet-line_1"),
        "must find narrator key in generated FTL; got:\n{default_ftl}"
    );

    // Build a "translated" FTL with overridden values (simulating a French translation).
    // We use the actual keys discovered above.
    let translated_ftl = concat!(
        "shop-greet-merchant-welcome = Bienvenue, { $player_name } ! Vous avez { $gold } or.\n",
        "shop-greet-line_1 = Vous approchez le marchand.\n",
    );

    let localizer = Arc::new(FluentLocalizer::new(translated_ftl));

    let vm = Vm::new(graph, registry_with_id()).expect("vm");
    let mut vm = vm.with_localizer(localizer);

    let mut dialogues: Vec<(Option<String>, Option<String>)> = Vec::new();
    for _ in 0..50 {
        match vm.next(None) {
            VmStep::Event(Event::Dialogue {
                loc_id,
                localized_text,
                ..
            }) => {
                dialogues.push((loc_id, localized_text));
            }
            VmStep::Event(Event::Choice { .. }) => break,
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {e:?}"),
            _ => {}
        }
    }

    assert!(
        dialogues.len() >= 2,
        "expected at least 2 dialogue events; got {}: {dialogues:?}",
        dialogues.len()
    );

    // Narrator line should be the French translation.
    let (_, ref text_0) = dialogues[0];
    let text_0 = text_0.as_deref().expect("narrator must have localized_text");
    assert!(
        text_0.contains("approchez") && text_0.contains("marchand"),
        "narrator line should be French; got: {text_0}"
    );

    // Merchant welcome should be the French translation with interpolated vars.
    let (_, ref text_1) = dialogues[1];
    let text_1 = text_1
        .as_deref()
        .expect("merchant must have localized_text");
    assert!(
        text_1.contains("Bienvenue"),
        "merchant line should start with 'Bienvenue'; got: {text_1}"
    );
    assert!(
        text_1.contains("Hero"),
        "merchant line must interpolate player_name 'Hero'; got: {text_1}"
    );
    assert!(
        text_1.contains("50"),
        "merchant line must interpolate gold '50'; got: {text_1}"
    );
}

// ── Post-mutation: @fluent vars reflect runtime state ────────────────────────

#[test]
fn vm_localizer_receives_mutated_fluent_vars() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");
    let localizer = Arc::new(FluentLocalizer::new(&ftl));

    let vm = Vm::new(graph, registry_with_id()).expect("vm");
    let mut vm = vm.with_localizer(localizer);

    // Advance to the Choice event.
    loop {
        match vm.next(None) {
            VmStep::Event(Event::Choice { .. }) => break,
            VmStep::Event(Event::Dialogue { .. }) => {}
            VmStep::Ended => panic!("VM ended before Choice"),
            VmStep::Error(e) => panic!("VM error: {e:?}"),
            _ => {}
        }
    }

    // Choose "Buy potion (10 gold)" — option index 0.
    // After this, gold should become 40.
    let mut post_buy_dialogue: Option<(Option<String>, Option<String>)> = None;
    for _ in 0..50 {
        match vm.next(Some(0)) {
            VmStep::Event(Event::Dialogue {
                loc_id,
                localized_text,
                ..
            }) => {
                post_buy_dialogue = Some((loc_id, localized_text));
                break;
            }
            VmStep::Event(Event::Choice { .. }) => {
                // Shouldn't happen, but keep going.
            }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error after choice: {e:?}"),
            _ => {}
        }
    }

    let (_, localized) = post_buy_dialogue.expect("expected a dialogue after buying");
    let text = localized.expect("post-buy dialogue must have localized_text");

    // The dialogue says "Sold! You now have {gold} gold." — gold should be 40.
    assert!(
        text.contains("40"),
        "post-buy dialogue must reflect mutated gold=40; got: {text}"
    );
}

// ── No localizer: localized_text is None ─────────────────────────────────────

#[test]
fn vm_without_localizer_has_no_localized_text() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");

    // No localizer attached.
    let mut vm = Vm::new(graph, registry_with_id()).expect("vm");

    for _ in 0..50 {
        match vm.next(None) {
            VmStep::Event(Event::Dialogue {
                localized_text, ..
            }) => {
                assert!(
                    localized_text.is_none(),
                    "without a localizer, localized_text must be None"
                );
            }
            VmStep::Event(Event::Choice { options, .. }) => {
                for opt in &options {
                    assert!(
                        opt.localized_label.is_none(),
                        "without a localizer, localized_label must be None"
                    );
                }
                break;
            }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {e:?}"),
            _ => {}
        }
    }
}

// ── Compile without compile_named: loc_ids are None ──────────────────────────

#[test]
fn compile_without_named_produces_no_loc_ids() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    // Use Compiler::compile (NOT compile_named) — no loc_id generation.
    let graph = Compiler::compile(&ast).expect("compile");

    let mut vm = Vm::new(graph, registry_with_id()).expect("vm");

    for _ in 0..50 {
        match vm.next(None) {
            VmStep::Event(Event::Dialogue { loc_id, .. }) => {
                assert!(
                    loc_id.is_none(),
                    "Compiler::compile must not generate loc_ids"
                );
            }
            VmStep::Event(Event::Choice {
                loc_id, options, ..
            }) => {
                assert!(
                    loc_id.is_none(),
                    "Compiler::compile must not generate menu loc_id"
                );
                for opt in &options {
                    assert!(
                        opt.loc_id.is_none(),
                        "Compiler::compile must not generate option loc_ids"
                    );
                }
                break;
            }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {e:?}"),
            _ => {}
        }
    }
}

// ── Roundtrip: generate FTL, parse it, feed back, all keys resolve ───────────

#[test]
fn generated_ftl_roundtrip_all_keys_resolve() {
    let ast = parse_source(TEST_SCRIPT).expect("parse");
    let graph = Compiler::compile_named(&ast, "shop").expect("compile");
    let ftl = generate_ftl(&graph, "shop");
    let localizer = Arc::new(FluentLocalizer::new(&ftl));

    let vm = Vm::new(graph, registry_with_id()).expect("vm");
    let mut vm = vm.with_localizer(localizer);

    // Walk every event and assert that every loc_id resolves to Some localized text.
    let mut seen_dialogue = false;
    let mut seen_choice = false;
    for _ in 0..50 {
        match vm.next(None) {
            VmStep::Event(Event::Dialogue {
                loc_id,
                localized_text,
                ..
            }) => {
                if let Some(ref id) = loc_id {
                    assert!(
                        localized_text.is_some(),
                        "dialogue loc_id '{id}' must resolve to Some(localized_text)"
                    );
                    assert!(
                        !localized_text.as_deref().unwrap().is_empty(),
                        "localized_text for '{id}' must not be empty"
                    );
                }
                seen_dialogue = true;
            }
            VmStep::Event(Event::Choice { options, .. }) => {
                for opt in &options {
                    if let Some(ref id) = opt.loc_id {
                        assert!(
                            opt.localized_label.is_some(),
                            "option loc_id '{id}' must resolve to Some(localized_label)"
                        );
                        assert!(
                            !opt.localized_label.as_deref().unwrap().is_empty(),
                            "localized_label for '{id}' must not be empty"
                        );
                    }
                }
                seen_choice = true;
                break;
            }
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("VM error: {e:?}"),
            _ => {}
        }
    }

    assert!(seen_dialogue, "test must have encountered at least one Dialogue event");
    assert!(seen_choice, "test must have encountered at least one Choice event");
}
