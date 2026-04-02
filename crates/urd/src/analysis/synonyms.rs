//! # Synonym store
//!
//! A zero-dependency, compile-time fallback for single-word semantic synonyms
//! that the 64-dim ONNX embedding model cannot reliably handle.
//!
//! ## Motivation
//!
//! [`crate::analysis::semantic_suggest::SemanticSuggest`] backed by
//! `potion-base-2M` works well for multi-word identifiers with shared
//! components (`go_to_woods` / `go_to_forest`), but reliably falls below
//! threshold for single-word pairs like `trader`/`seller` or `health`/`hp`.
//! Those pairs live in distributional zones that PCA compression from
//! 768→64 dims collapses.
//!
//! ## Matching semantics
//!
//! Two preprocessed (space-separated) identifier names are synonymous when:
//! - they have the same word count, and
//! - each positional word pair is either identical or maps to the same
//!   synonym cluster in [`SYNONYM_MAP`].
//!
//! Example: `"enemy_num"` ↔ `"foe_count"` — both words match their clusters
//! (`C_ENEMY` and `C_COUNT` respectively).
//!
//! Example: `"go_to_woods"` ↔ `"go_to_forest"` — `"go"` and `"to"` are
//! identical; `"woods"` and `"forest"` share [`C_FOREST`].

use crate::analysis::semantic_suggest::SemanticSuggest;

// ---------------------------------------------------------------------------
// Cluster ID constants
// ---------------------------------------------------------------------------

const C_HEALTH: u16 = 1;
const C_MANA: u16 = 2;
const C_ARMOR: u16 = 3;
const C_STRENGTH: u16 = 4;
const C_SPEED: u16 = 5;
const C_DAMAGE: u16 = 6;
const C_EXPERIENCE: u16 = 7;
const C_COUNT: u16 = 10;
const C_MAX: u16 = 11;
const C_MIN: u16 = 12;
const C_ENEMY: u16 = 20;
const C_PLAYER: u16 = 21;
const C_ALLY: u16 = 22;
const C_ITEM: u16 = 23;
const C_TRADER: u16 = 30;
const C_BUYER: u16 = 31;
const C_PRICE: u16 = 32;
const C_MONEY: u16 = 33;
const C_DEAD: u16 = 40;
const C_ALIVE: u16 = 41;
const C_STATE: u16 = 42;
const C_CURRENT: u16 = 43;
const C_PREVIOUS: u16 = 44;
const C_ID: u16 = 50;
const C_INDEX: u16 = 51;
const C_TEXT: u16 = 60;
const C_NAME: u16 = 61;
const C_POSITION: u16 = 70;
const C_DIRECTION: u16 = 71;
const C_MOVE: u16 = 72;
const C_ATTACK: u16 = 80;
const C_SPAWN: u16 = 81;
const C_DESTROY: u16 = 82;
const C_JUMP: u16 = 83;
const C_TIMER: u16 = 90;
// ── Terrain / nature ──────────────────────────────────────────────────────────
const C_FOREST: u16 = 100;
const C_CAVE: u16 = 101;
const C_FIELD: u16 = 102;
const C_MOUNTAIN: u16 = 103;
const C_RIVER: u16 = 104;
// ── Settlements / structures ──────────────────────────────────────────────────
const C_VILLAGE: u16 = 110;
const C_CASTLE: u16 = 111;
const C_TAVERN: u16 = 112;
const C_SHOP: u16 = 113;
// ── Navigation ────────────────────────────────────────────────────────────────
const C_PATH: u16 = 120;

// ---------------------------------------------------------------------------
// Lookup table — MUST remain sorted by word for binary_search_by_key.
// ---------------------------------------------------------------------------

/// Maps lowercase single words to synonym cluster IDs.
///
/// **Invariant**: entries must be in ascending lexicographic order by the
/// first field.  The [`tests::synonym_map_is_sorted`] test enforces this.
static SYNONYM_MAP: &[(&str, u16)] = &[
    ("adversary", C_ENEMY),
    ("agi", C_SPEED),
    ("agility", C_SPEED),
    ("alive", C_ALIVE),
    ("ally", C_ALLY),
    ("amount", C_COUNT),
    ("armor", C_ARMOR),
    ("armour", C_ARMOR),
    ("atk", C_ATTACK),
    ("attack", C_ATTACK),
    ("avatar", C_PLAYER),
    ("bar", C_TAVERN),
    ("bazaar", C_SHOP),
    ("brook", C_RIVER),
    ("buyer", C_BUYER),
    ("cap", C_MAX),
    ("cast", C_ATTACK),
    ("castle", C_CASTLE),
    ("cave", C_CAVE),
    ("cavern", C_CAVE),
    ("character", C_PLAYER),
    ("citadel", C_CASTLE),
    ("city", C_VILLAGE),
    ("client", C_BUYER),
    ("cliff", C_MOUNTAIN),
    ("coins", C_MONEY),
    ("companion", C_ALLY),
    ("coordinates", C_POSITION),
    ("coords", C_POSITION),
    ("cost", C_PRICE),
    ("count", C_COUNT),
    ("countdown", C_TIMER),
    ("creek", C_RIVER),
    ("cur", C_CURRENT),
    ("currency", C_MONEY),
    ("current", C_CURRENT),
    ("customer", C_BUYER),
    ("damage", C_DAMAGE),
    ("dead", C_DEAD),
    ("dealer", C_TRADER),
    ("defeated", C_DEAD),
    ("defence", C_ARMOR),
    ("defense", C_ARMOR),
    ("despawn", C_DESTROY),
    ("destroy", C_DESTROY),
    ("dir", C_DIRECTION),
    ("direction", C_DIRECTION),
    ("dmg", C_DAMAGE),
    ("dungeon", C_CAVE),
    ("elapsed", C_TIMER),
    ("enemy", C_ENEMY),
    ("energy", C_MANA),
    ("entity", C_ITEM),
    ("exp", C_EXPERIENCE),
    ("experience", C_EXPERIENCE),
    ("facing", C_DIRECTION),
    ("field", C_FIELD),
    ("fire", C_ATTACK),
    ("flag", C_STATE),
    ("floor", C_MIN),
    ("foe", C_ENEMY),
    ("force", C_STRENGTH),
    ("forest", C_FOREST),
    ("fort", C_CASTLE),
    ("fortress", C_CASTLE),
    ("friend", C_ALLY),
    ("funds", C_MONEY),
    ("go", C_MOVE),
    ("gold", C_MONEY),
    ("gone", C_DEAD),
    ("grassland", C_FIELD),
    ("grotto", C_CAVE),
    ("grove", C_FOREST),
    ("hamlet", C_VILLAGE),
    ("harm", C_DAMAGE),
    ("heading", C_DIRECTION),
    ("health", C_HEALTH),
    ("hearts", C_HEALTH),
    ("hero", C_PLAYER),
    ("hill", C_MOUNTAIN),
    ("hit", C_ATTACK),
    ("hop", C_JUMP),
    ("hostile", C_ENEMY),
    ("hp", C_HEALTH),
    ("hurt", C_DAMAGE),
    ("id", C_ID),
    ("identifier", C_ID),
    ("idx", C_INDEX),
    ("index", C_INDEX),
    ("inn", C_TAVERN),
    ("instantiate", C_SPAWN),
    ("item", C_ITEM),
    ("jump", C_JUMP),
    ("jungle", C_FOREST),
    ("keep", C_CASTLE),
    ("key", C_ID),
    ("killed", C_DEAD),
    ("label", C_NAME),
    ("lake", C_RIVER),
    ("last", C_PREVIOUS),
    ("launch", C_ATTACK),
    ("leap", C_JUMP),
    ("life", C_HEALTH),
    ("limit", C_MAX),
    ("living", C_ALIVE),
    ("loc", C_POSITION),
    ("location", C_POSITION),
    ("lower", C_MIN),
    ("mana", C_MANA),
    ("market", C_SHOP),
    ("max", C_MAX),
    ("maximum", C_MAX),
    ("meadow", C_FIELD),
    ("merchant", C_TRADER),
    ("message", C_TEXT),
    ("might", C_STRENGTH),
    ("min", C_MIN),
    ("minimum", C_MIN),
    ("mob", C_ENEMY),
    ("mode", C_STATE),
    ("money", C_MONEY),
    ("mountain", C_MOUNTAIN),
    ("move", C_MOVE),
    ("mp", C_MANA),
    ("name", C_NAME),
    ("navigate", C_MOVE),
    ("next", C_STATE),
    ("npc", C_ALLY),
    ("num", C_COUNT),
    ("number", C_COUNT),
    ("object", C_ITEM),
    ("ocean", C_RIVER),
    ("old", C_PREVIOUS),
    ("opponent", C_ENEMY),
    ("passage", C_PATH),
    ("pasture", C_FIELD),
    ("path", C_PATH),
    ("pc", C_PLAYER),
    ("peak", C_MOUNTAIN),
    ("phase", C_STATE),
    ("plain", C_FIELD),
    ("player", C_PLAYER),
    ("pond", C_RIVER),
    ("pos", C_POSITION),
    ("position", C_POSITION),
    ("power", C_STRENGTH),
    ("prev", C_PREVIOUS),
    ("previous", C_PREVIOUS),
    ("price", C_PRICE),
    ("protagonist", C_PLAYER),
    ("pub", C_TAVERN),
    ("purchaser", C_BUYER),
    ("quantity", C_COUNT),
    ("remove", C_DESTROY),
    ("resistance", C_ARMOR),
    ("ridge", C_MOUNTAIN),
    ("river", C_RIVER),
    ("road", C_PATH),
    ("route", C_PATH),
    ("run", C_MOVE),
    ("sea", C_RIVER),
    ("seller", C_TRADER),
    ("settlement", C_VILLAGE),
    ("shield", C_ARMOR),
    ("shoot", C_ATTACK),
    ("shop", C_SHOP),
    ("shopkeeper", C_TRADER),
    ("speed", C_SPEED),
    ("spring", C_JUMP),
    ("stamina", C_MANA),
    ("state", C_STATE),
    ("status", C_STATE),
    ("store", C_SHOP),
    ("str", C_STRENGTH),
    ("stream", C_RIVER),
    ("strength", C_STRENGTH),
    ("strike", C_ATTACK),
    ("stronghold", C_CASTLE),
    ("sum", C_COUNT),
    ("summit", C_MOUNTAIN),
    ("summon", C_SPAWN),
    ("survived", C_ALIVE),
    ("tavern", C_TAVERN),
    ("text", C_TEXT),
    ("thicket", C_FOREST),
    ("thing", C_ITEM),
    ("timer", C_TIMER),
    ("title", C_NAME),
    ("total", C_COUNT),
    ("town", C_VILLAGE),
    ("track", C_PATH),
    ("trader", C_TRADER),
    ("trail", C_PATH),
    ("travel", C_MOVE),
    ("tunnel", C_CAVE),
    ("uid", C_ID),
    ("uuid", C_ID),
    ("value", C_PRICE),
    ("velocity", C_SPEED),
    ("vendor", C_TRADER),
    ("village", C_VILLAGE),
    ("vitality", C_HEALTH),
    ("walk", C_MOVE),
    ("water", C_RIVER),
    ("way", C_PATH),
    ("wealth", C_MONEY),
    ("wilderness", C_FOREST),
    ("woodland", C_FOREST),
    ("woods", C_FOREST),
    ("worth", C_PRICE),
    ("xp", C_EXPERIENCE),
];

// ---------------------------------------------------------------------------
// SynonymStore
// ---------------------------------------------------------------------------

/// Zero-cost curated synonym checker.
///
/// Implements [`SemanticSuggest`] using a compile-time lookup table
/// ([`SYNONYM_MAP`]) keyed by synonym cluster IDs.  Two identifier names are
/// considered synonymous when they have the same word count (after
/// [`preprocess_identifier`] splitting) and every positional word pair
/// belongs to the same cluster or is identical.
///
/// This handles the cases the 64-dim ONNX model cannot — single-word pairs
/// like `trader`/`seller` or `health`/`hp` — at essentially zero runtime cost.
#[derive(Debug, Clone, Copy, Default)]
pub struct SynonymStore;

impl SynonymStore {
    /// Return the synonym cluster for a single lowercase word, or `None`.
    fn cluster(word: &str) -> Option<u16> {
        SYNONYM_MAP
            .binary_search_by_key(&word, |&(w, _)| w)
            .ok()
            .map(|i| SYNONYM_MAP[i].1)
    }

    /// Two individual words are synonymous iff they are equal or share a cluster.
    fn words_match(a: &str, b: &str) -> bool {
        if a == b {
            return true;
        }
        matches!(
            (Self::cluster(a), Self::cluster(b)),
            (Some(ca), Some(cb)) if ca == cb
        )
    }

    /// Two preprocessed (space-separated) names match iff they have the same
    /// word count and every positional pair satisfies [`words_match`].
    fn names_match(a: &str, b: &str) -> bool {
        let wa: Vec<&str> = a.split_whitespace().collect();
        let wb: Vec<&str> = b.split_whitespace().collect();
        wa.len() == wb.len()
            && wa
                .iter()
                .zip(wb.iter())
                .all(|(x, y)| Self::words_match(x, y))
    }
}

impl SemanticSuggest for SynonymStore {
    fn find_synonym(&self, query: &str, candidates: &[String]) -> Option<String> {
        let q = preprocess_identifier(query);
        candidates
            .iter()
            .find(|c| {
                let c_prep = preprocess_identifier(c);
                c_prep != q && Self::names_match(&q, &c_prep)
            })
            .cloned()
    }
}

// ---------------------------------------------------------------------------
// ChainedSuggest
// ---------------------------------------------------------------------------

/// Chains two [`SemanticSuggest`] implementations: tries `primary` first,
/// falls back to `secondary` on `None`.
///
/// Typical usage:
/// ```text
/// ChainedSuggest::new(PotionModel { … }, SynonymStore)
/// ```
pub struct ChainedSuggest<P, F> {
    primary: P,
    secondary: F,
}

impl<P, F> ChainedSuggest<P, F> {
    /// Construct a new chain.
    pub fn new(primary: P, secondary: F) -> Self {
        Self { primary, secondary }
    }
}

impl<P: SemanticSuggest, F: SemanticSuggest> SemanticSuggest for ChainedSuggest<P, F> {
    fn find_synonym(&self, query: &str, candidates: &[String]) -> Option<String> {
        self.primary
            .find_synonym(query, candidates)
            .or_else(|| self.secondary.find_synonym(query, candidates))
    }
}

// ---------------------------------------------------------------------------
// preprocess_identifier — shared utility
// ---------------------------------------------------------------------------

/// Convert a snake\_case or camelCase identifier to a space-separated
/// lowercase string for embedding or cluster lookup.
///
/// Examples:
/// - `"player_name"` → `"player name"`
/// - `"enemyHealth"` → `"enemy health"`
/// - `"go_to_woods"` → `"go to woods"`
pub fn preprocess_identifier(name: &str) -> String {
    let snake_parts: Vec<&str> = name.split('_').collect();
    let mut words: Vec<String> = Vec::new();
    for part in snake_parts {
        let mut word = String::with_capacity(part.len());
        let chars: Vec<char> = part.chars().collect();
        for (i, &c) in chars.iter().enumerate() {
            if i > 0 && c.is_uppercase() {
                let prev = chars[i - 1];
                if prev.is_lowercase() || (i + 1 < chars.len() && chars[i + 1].is_lowercase()) {
                    word.push(' ');
                }
            }
            word.push(c.to_ascii_lowercase());
        }
        if !word.is_empty() {
            words.push(word);
        }
    }
    words.join(" ")
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn synonym_map_is_sorted() {
        for w in SYNONYM_MAP.windows(2) {
            assert!(
                w[0].0 <= w[1].0,
                "SYNONYM_MAP out of order: {:?} before {:?}",
                w[0].0,
                w[1].0
            );
        }
    }

    #[test]
    fn trader_seller_single_word_synonym() {
        let store = SynonymStore;
        let candidates = vec!["seller".to_owned(), "jump_count".to_owned()];
        assert_eq!(
            store.find_synonym("trader", &candidates),
            Some("seller".to_owned())
        );
    }

    #[test]
    fn health_hp_abbreviation() {
        let store = SynonymStore;
        let candidates = vec!["hp".to_owned()];
        assert_eq!(
            store.find_synonym("health", &candidates),
            Some("hp".to_owned())
        );
    }

    #[test]
    fn enemy_foe_single_word() {
        let store = SynonymStore;
        let candidates = vec!["foe".to_owned()];
        assert_eq!(
            store.find_synonym("enemy", &candidates),
            Some("foe".to_owned())
        );
    }

    #[test]
    fn multiword_enemy_num_matches_foe_count() {
        let store = SynonymStore;
        let candidates = vec!["foe_count".to_owned()];
        assert_eq!(
            store.find_synonym("enemy_num", &candidates),
            Some("foe_count".to_owned())
        );
    }

    #[test]
    fn multiword_partial_mismatch_no_match() {
        // "enemy_speed" vs "foe_damage": position 0 matches (C_ENEMY) but
        // position 1 does not (C_SPEED != C_DAMAGE).
        let store = SynonymStore;
        let candidates = vec!["foe_damage".to_owned()];
        assert_eq!(store.find_synonym("enemy_speed", &candidates), None);
    }

    #[test]
    fn go_to_woods_matches_go_to_forest() {
        // The canonical label-synonym case: "woods" and "forest" share C_FOREST.
        // Levenshtein distance between the full names is 4 — well above the ≤2
        // threshold — so without the synonym store this would produce no suggestion.
        let store = SynonymStore;
        let candidates = vec!["go_to_forest".to_owned(), "player_health".to_owned()];
        assert_eq!(
            store.find_synonym("go_to_woods", &candidates),
            Some("go_to_forest".to_owned())
        );
    }

    #[test]
    fn go_to_village_matches_go_to_town() {
        let store = SynonymStore;
        let candidates = vec!["go_to_town".to_owned()];
        assert_eq!(
            store.find_synonym("go_to_village", &candidates),
            Some("go_to_town".to_owned())
        );
    }

    #[test]
    fn unrelated_words_no_match() {
        let store = SynonymStore;
        let candidates = vec!["jump_count".to_owned(), "dialogue_text".to_owned()];
        assert_eq!(store.find_synonym("trader", &candidates), None);
    }

    #[test]
    fn exact_match_is_not_returned_as_synonym() {
        // "enemy" should not suggest "enemy" as a synonym of itself.
        let store = SynonymStore;
        let candidates = vec!["enemy".to_owned()];
        assert_eq!(store.find_synonym("enemy", &candidates), None);
    }

    #[test]
    fn chained_suggest_tries_primary_first() {
        // Primary always returns Some; secondary should never be reached.
        struct AlwaysFoo;
        impl SemanticSuggest for AlwaysFoo {
            fn find_synonym(&self, _: &str, _: &[String]) -> Option<String> {
                Some("foo".to_owned())
            }
        }
        struct NeverCalled;
        impl SemanticSuggest for NeverCalled {
            fn find_synonym(&self, _: &str, _: &[String]) -> Option<String> {
                panic!("secondary should not be called when primary succeeds");
            }
        }
        let chain = ChainedSuggest::new(AlwaysFoo, NeverCalled);
        assert_eq!(
            chain.find_synonym("x", &["y".to_owned()]),
            Some("foo".to_owned())
        );
    }

    #[test]
    fn chained_suggest_falls_back_to_secondary() {
        struct AlwaysNone;
        impl SemanticSuggest for AlwaysNone {
            fn find_synonym(&self, _: &str, _: &[String]) -> Option<String> {
                None
            }
        }
        let chain = ChainedSuggest::new(AlwaysNone, SynonymStore);
        let candidates = vec!["seller".to_owned()];
        assert_eq!(
            chain.find_synonym("trader", &candidates),
            Some("seller".to_owned())
        );
    }

    #[test]
    fn preprocess_snake_case() {
        assert_eq!(preprocess_identifier("player_name"), "player name");
    }

    #[test]
    fn preprocess_camel_case() {
        assert_eq!(preprocess_identifier("enemyHealth"), "enemy health");
    }

    #[test]
    fn preprocess_empty() {
        assert_eq!(preprocess_identifier(""), "");
    }
}
