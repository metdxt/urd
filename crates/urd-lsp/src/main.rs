//! # Urd Language Server
//!
//! A Language Server Protocol implementation for the Urd dialogue scripting language.
//! Provides real-time diagnostics, code completion, hover information, go-to-definition,
//! find references, document symbols, and semantic token highlighting.

mod completion;
mod document;
mod semantic;
#[cfg(feature = "spellcheck")]
mod user_dict;
mod workspace;

use std::sync::Arc;

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{debug, info};

use document::{Document, byte_span_to_lsp_range, position_to_byte_offset, prefix_start_character};
use urd::analysis::AnalysisError;
#[cfg(feature = "spellcheck")]
use urd::analysis::SpellcheckLanguage;
use urd::analysis::semantic_suggest::SemanticSuggest;
use urd::analysis::synonyms::SynonymStore;

use completion::{TypeContext, completion_items};
use semantic::{
    SemanticTokenType as UrdTokenType, Symbol, SymbolKind as UrdSymbolKind, collect_symbols,
    find_definition, find_references, find_rename_spans, hover_info,
    semantic_tokens as compute_semantic_tokens,
};
#[cfg(feature = "spellcheck")]
use user_dict::UserDictionary;
use workspace::WorkspaceIndex;

// ── Semantic-token legend ────────────────────────────────────────────────────

/// The ordered list of token types we advertise to the client.
///
/// The index of each entry becomes the `token_type` field in `SemanticToken`.
const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,     // 0
    SemanticTokenType::NAMESPACE,   // 1  — labels
    SemanticTokenType::VARIABLE,    // 2
    SemanticTokenType::STRING,      // 3
    SemanticTokenType::NUMBER,      // 4
    SemanticTokenType::OPERATOR,    // 5
    SemanticTokenType::ENUM_MEMBER, // 6
    SemanticTokenType::STRUCT,      // 7
    SemanticTokenType::DECORATOR,   // 8
    SemanticTokenType::FUNCTION,    // 9
    SemanticTokenType::PROPERTY,    // 10
];

/// Map our internal semantic-token type to its index in [`TOKEN_TYPES`].
fn urd_token_type_to_index(tt: &UrdTokenType) -> u32 {
    match tt {
        UrdTokenType::Keyword => 0,
        UrdTokenType::Label => 1,
        UrdTokenType::Variable => 2,
        UrdTokenType::String => 3,
        UrdTokenType::Number => 4,
        UrdTokenType::Operator => 5,
        UrdTokenType::EnumMember => 6,
        UrdTokenType::Struct => 7,
        UrdTokenType::Decorator => 8,
        UrdTokenType::Function => 9,
        UrdTokenType::Property => 10,
    }
}

// ── Symbol-kind mapping ──────────────────────────────────────────────────────

/// Map our internal symbol kind to an LSP `SymbolKind` (used in document-symbols).
fn urd_symbol_kind_to_lsp(kind: &UrdSymbolKind) -> SymbolKind {
    match kind {
        UrdSymbolKind::Label => SymbolKind::FUNCTION,
        UrdSymbolKind::Variable => SymbolKind::VARIABLE,
        UrdSymbolKind::Constant => SymbolKind::CONSTANT,
        UrdSymbolKind::Global => SymbolKind::VARIABLE,
        UrdSymbolKind::Enum => SymbolKind::ENUM,
        UrdSymbolKind::EnumVariant => SymbolKind::ENUM_MEMBER,
        UrdSymbolKind::Struct => SymbolKind::STRUCT,
        UrdSymbolKind::Decorator => SymbolKind::FUNCTION,
        UrdSymbolKind::Import => SymbolKind::MODULE,
    }
}

/// Map our internal symbol kind to an LSP `CompletionItemKind`.
fn urd_symbol_kind_to_completion(kind: &UrdSymbolKind) -> CompletionItemKind {
    match kind {
        UrdSymbolKind::Label => CompletionItemKind::FUNCTION,
        UrdSymbolKind::Variable => CompletionItemKind::VARIABLE,
        UrdSymbolKind::Constant => CompletionItemKind::CONSTANT,
        UrdSymbolKind::Global => CompletionItemKind::VARIABLE,
        UrdSymbolKind::Enum => CompletionItemKind::ENUM,
        UrdSymbolKind::EnumVariant => CompletionItemKind::ENUM_MEMBER,
        UrdSymbolKind::Struct => CompletionItemKind::STRUCT,
        UrdSymbolKind::Decorator => CompletionItemKind::FUNCTION,
        UrdSymbolKind::Import => CompletionItemKind::MODULE,
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

/// Extract the identifier-like word surrounding `byte_offset` in `src`.
///
/// Urd identifiers are `[_a-zA-Z][_a-zA-Z0-9]*` optionally dot-separated.
/// We scan outward from the offset to capture the full word (including dots for
/// qualified paths like `module.label`).
fn word_at_offset(src: &str, byte_offset: usize) -> Option<&str> {
    if byte_offset > src.len() {
        return None;
    }
    let bytes = src.as_bytes();

    fn is_ident_char(b: u8) -> bool {
        b.is_ascii_alphanumeric() || b == b'_' || b == b'.'
    }

    // If the cursor is between characters (or at EOF), step back one byte.
    let probe = if byte_offset < bytes.len() && is_ident_char(bytes[byte_offset]) {
        byte_offset
    } else if byte_offset > 0 && is_ident_char(bytes[byte_offset - 1]) {
        byte_offset - 1
    } else {
        return None;
    };

    // Scan backwards.
    let mut start = probe;
    while start > 0 && is_ident_char(bytes[start - 1]) {
        start -= 1;
    }

    // Scan forwards.
    let mut end = probe + 1;
    while end < bytes.len() && is_ident_char(bytes[end]) {
        end += 1;
    }

    // Trim leading/trailing dots that aren't part of a real identifier.
    let word = &src[start..end];
    let word = word.trim_matches('.');
    if word.is_empty() { None } else { Some(word) }
}

/// Gather symbols from a [`Document`], returning an empty vec when no AST is
/// available.
fn symbols_from_doc(doc: &Document) -> Vec<Symbol> {
    match &doc.ast {
        Some(ast) => collect_symbols(ast),
        None => Vec::new(),
    }
}

// ── Backend ──────────────────────────────────────────────────────────────────

/// The LSP server backend.
///
/// Holds the `Client` handle (used to push diagnostics) and a concurrent map of
/// open documents keyed by URI.
struct Backend {
    client: Client,
    documents: DashMap<Url, Document>,
    /// Cross-file symbol index: tracks imported modules for every open file.
    workspace: Arc<WorkspaceIndex>,
    /// Synonym-based suggestion backend used to enrich undefined-variable and
    /// undefined-label diagnostics with "did you mean …?" hints.
    semantic: Arc<dyn SemanticSuggest + Send + Sync>,
    /// Language override for spell-checking.
    /// `None` (default) = automatic detection via whatlang on every document.
    /// `Some(lang)` = force this language for all files, bypassing detection.
    /// Set via `initializationOptions.spellcheckLanguage`.
    #[cfg(feature = "spellcheck")]
    spellcheck_language: std::sync::RwLock<Option<SpellcheckLanguage>>,
    /// Per-workspace user dictionary: words exempt from spell-checking.
    /// Loaded from `.urd-dict` in the workspace root on `initialize`.
    #[cfg(feature = "spellcheck")]
    user_dict: Arc<std::sync::RwLock<UserDictionary>>,
}

impl Backend {
    /// Publish diagnostics for the document at `uri`.
    async fn publish_diagnostics(&self, uri: Url) {
        let diags = {
            let doc = match self.documents.get(&uri) {
                Some(d) => d,
                None => return,
            };
            doc.diagnostics()
        };
        self.client.publish_diagnostics(uri, diags, None).await;
    }

    /// Re-parse a document, update internal state, and push diagnostics.
    async fn on_change(&self, uri: Url, text: String) {
        {
            let mut entry = self
                .documents
                .entry(uri.clone())
                .or_insert_with(|| Document::new(""));
            entry.update(&text);
        }

        // Re-index imports so cross-file features stay up to date.
        // This must happen before we gather the imported type context below.
        //
        // Use `last_clean_ast` — the most recent *fully valid* parse — rather
        // than `ast`, which may be a partial recovery tree produced during a
        // mid-edit parse error.  A partial recovery tree often contains no
        // import nodes, which would cause `workspace.update` to replace the
        // previously-valid import cache with an empty list, making
        // `imported_type_context` return nothing and breaking struct-field and
        // enum-variant completion for the rest of the edit session.
        if let Some(doc) = self.documents.get(&uri)
            && let Some(ast) = &doc.last_clean_ast
        {
            self.workspace.update(&uri, ast);
        }

        // Re-run analysis with cross-module struct/enum/label context so that
        // type-checking and label-resolution can see definitions from imported files.
        let (imported_structs, imported_enums, imported_labels) =
            self.workspace.imported_type_context(&uri);
        if let Some(mut doc) = self.documents.get_mut(&uri) {
            doc.reanalyze_with_imports(
                imported_structs,
                imported_enums,
                imported_labels,
                Some(Arc::clone(&self.semantic) as Arc<dyn SemanticSuggest>),
            );
        }

        // Run the spellcheck pass with the currently configured language.
        #[cfg(feature = "spellcheck")]
        {
            // SpellcheckLanguage is Copy, so we deref the guard to get the value
            // and drop the lock immediately before calling into the document map.
            let language = *self
                .spellcheck_language
                .read()
                .unwrap_or_else(|e| e.into_inner());
            // Hold the dict read-lock while calling run_spellcheck, then drop it.
            let dict = self.user_dict.read().unwrap_or_else(|e| e.into_inner());
            if let Some(mut doc) = self.documents.get_mut(&uri) {
                doc.run_spellcheck(language, dict.words());
            }
        }

        self.publish_diagnostics(uri).await;
    }
}

// ── LanguageServer trait ─────────────────────────────────────────────────────

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    #[cfg_attr(not(feature = "spellcheck"), allow(unused_variables))]
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        info!("urd-lsp initializing");

        #[cfg(feature = "spellcheck")]
        {
            // Apply spellcheckLanguage from client initializationOptions if provided.
            if let Some(opts) = params.initialization_options.as_ref()
                && let Some(lang_val) = opts.get("spellcheckLanguage")
                && let Ok(lang) = serde_json::from_value::<SpellcheckLanguage>(lang_val.clone())
                && let Ok(mut guard) = self.spellcheck_language.write()
            {
                *guard = Some(lang);
            }

            // Resolve the workspace root: prefer workspace_folders, fall back to root_uri.
            let workspace_root = params
                .workspace_folders
                .as_deref()
                .and_then(|folders| folders.first())
                .and_then(|f| f.uri.to_file_path().ok())
                .or_else(|| params.root_uri.as_ref().and_then(|u| u.to_file_path().ok()));

            if let Some(root) = workspace_root {
                let dict_path = root.join(".urd-dict");
                info!("user-dict: loading from {}", dict_path.display());
                if let Ok(mut guard) = self.user_dict.write() {
                    *guard = UserDictionary::load(dict_path);
                }
            }
        }

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "urd-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
            capabilities: ServerCapabilities {
                // ── Document sync ────────────────────────────────────────
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),

                // ── Hover ────────────────────────────────────────────────
                hover_provider: Some(HoverProviderCapability::Simple(true)),

                // ── Completion ────────────────────────────────────────────
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), ":".into(), " ".into(), "@".into()]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),

                // ── Go to definition ─────────────────────────────────────
                definition_provider: Some(OneOf::Left(true)),

                // ── Find references ──────────────────────────────────────
                references_provider: Some(OneOf::Left(true)),

                // ── Document symbols ─────────────────────────────────────
                document_symbol_provider: Some(OneOf::Left(true)),

                // ── Rename ───────────────────────────────────────────────
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),

                // ── Code actions ─────────────────────────────────────────
                code_action_provider: Some(CodeActionProviderCapability::Options(
                    CodeActionOptions {
                        code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
                        ..Default::default()
                    },
                )),

                // ── Execute command ──────────────────────────────────────
                execute_command_provider: if cfg!(feature = "spellcheck") {
                    Some(ExecuteCommandOptions {
                        commands: vec!["urd.addToDictionary".to_string()],
                        work_done_progress_options: Default::default(),
                    })
                } else {
                    None
                },

                // ── Semantic tokens ──────────────────────────────────────
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: TOKEN_TYPES.to_vec(),
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            work_done_progress_options: Default::default(),
                        },
                    ),
                ),

                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "urd-lsp ready")
            .await;
        info!("urd-lsp initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        info!("urd-lsp shutting down");
        Ok(())
    }

    // ── Document synchronisation ─────────────────────────────────────────

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        debug!("did_open: {uri}");
        self.on_change(uri, text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // With FULL sync the last element is the entire document.
        if let Some(change) = params.content_changes.into_iter().last() {
            debug!("did_change: {uri}");
            self.on_change(uri, change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!("did_close: {uri}");
        self.documents.remove(&uri);
        self.workspace.remove(&uri);
        // Clear diagnostics for the closed file.
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!("did_save: {uri}");
        // Re-publish to make sure the client sees the latest state.
        self.publish_diagnostics(uri).await;
    }

    // ── Hover ────────────────────────────────────────────────────────────

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return Ok(None),
        };

        let src = doc.rope.to_string();
        let byte_offset = position_to_byte_offset(&src, pos);

        // Build a combined symbol list: local symbols + aliased imported symbols.
        let mut symbols = collect_symbols(ast);
        symbols.extend(self.workspace.imported_symbols(&uri));

        // Try local hover first.
        if let Some(markdown) = hover_info(ast, &symbols, &src, byte_offset) {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: markdown,
                }),
                range: None,
            }));
        }

        // Fall back: if the word under the cursor looks like a qualified
        // reference (alias.name), try the workspace index.
        if let Some(word) = word_at_offset(&src, byte_offset)
            && word.contains('.')
            && let Some(markdown) = self.workspace.hover_info(&uri, word)
        {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: markdown,
                }),
                range: None,
            }));
        }

        Ok(None)
    }

    // ── Completion ───────────────────────────────────────────────────────

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        debug!(
            "completion: request received for {uri} at line={} char={}",
            pos.line, pos.character
        );

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => {
                debug!("completion: no document found for {uri} — returning None");
                return Ok(None);
            }
        };

        let has_parse_errors = !doc.parse_errors.is_empty();
        let ast = match &doc.ast {
            Some(a) => {
                debug!(
                    "completion: using {} AST (parse_errors={})",
                    if has_parse_errors { "stale" } else { "fresh" },
                    doc.parse_errors.len()
                );
                a
            }
            None => {
                debug!(
                    "completion: doc.ast is None (document was never successfully parsed) — returning None"
                );
                return Ok(None);
            }
        };

        let src = doc.rope.to_string();
        let byte_offset = position_to_byte_offset(&src, pos);

        debug!(
            "completion: src_len={} byte_offset={} src_tail={:?}",
            src.len(),
            byte_offset,
            src.get(byte_offset.saturating_sub(20)..byte_offset)
                .unwrap_or("")
        );

        // Combine local symbols with aliased symbols from imported modules.
        let mut symbols = collect_symbols(ast);
        let imported = self.workspace.imported_symbols(&uri);
        debug!(
            "completion: local_symbols={} imported_symbols={}",
            symbols.len(),
            imported.len()
        );
        symbols.extend(imported);

        // Build a TypeContext from the workspace so that struct fields and enum
        // variants resolve even when the defining module was only partially
        // imported (e.g. `import (narrator) from "characters.urd"`).
        let (imported_structs, imported_enums, _) = self.workspace.imported_type_context(&uri);
        debug!(
            "completion: type_ctx structs={} ({:?}) enums={}",
            imported_structs.len(),
            imported_structs.keys().collect::<Vec<_>>(),
            imported_enums.len(),
        );
        let type_ctx = TypeContext {
            structs: imported_structs,
            enums: imported_enums,
        };

        let candidates = completion_items(ast, &symbols, byte_offset, &src, &type_ctx);

        debug!("completion: {} candidates generated", candidates.len());

        // Compute the UTF-16 column where the typed prefix starts so that the
        // TextEdit replaces only the prefix under the cursor (e.g. "jum") and
        // not the surrounding whitespace / indentation.
        //
        // Using `text_edit` instead of `insert_text` is critical: the LSP spec
        // explicitly states that `insert_text` semantics are client-defined and
        // may cause editors (including Zed) to replace from column 0 rather
        // than from the cursor.  When `text_edit` is present, `insert_text` is
        // ignored, and the editor uses the provided range verbatim.
        let prefix_start_char = prefix_start_character(&src, byte_offset, pos);
        let replace_range = Range {
            start: Position {
                line: pos.line,
                character: prefix_start_char,
            },
            end: pos,
        };

        let items: Vec<CompletionItem> = candidates
            .into_iter()
            .map(|(name, kind)| CompletionItem {
                label: name.clone(),
                kind: Some(urd_symbol_kind_to_completion(&kind)),
                detail: Some(kind.to_string()),
                // `text_edit` takes precedence over `insert_text` per the LSP
                // spec.  The range covers exactly the typed prefix on the
                // current line so indentation is preserved.
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: replace_range,
                    new_text: name,
                })),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..Default::default()
            })
            .collect();

        if items.is_empty() {
            debug!("completion: returning None (empty list)");
            Ok(None)
        } else {
            debug!("completion: returning {} items", items.len());
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    // ── Go to definition ─────────────────────────────────────────────────

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return Ok(None),
        };

        let src = doc.rope.to_string();
        let byte_offset = position_to_byte_offset(&src, pos);

        // Try to find the word under the cursor.
        let name = match word_at_offset(&src, byte_offset) {
            Some(w) => w.to_owned(),
            None => return Ok(None),
        };

        debug!("goto_definition: looking for '{name}'");

        // 1. Try local definition first.
        if let Some(span) = find_definition(ast, &name) {
            let range = byte_span_to_lsp_range(&src, span);
            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                uri, range,
            ))));
        }

        // 2. Fall back to the workspace index (cross-file lookup).
        if let Some((target_uri, span, target_src)) = self.workspace.find_definition(&uri, &name) {
            debug!("goto_definition: found '{name}' in {target_uri}");
            let range = byte_span_to_lsp_range(&target_src, span);
            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                target_uri, range,
            ))));
        }

        Ok(None)
    }

    // ── Find references ──────────────────────────────────────────────────

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return Ok(None),
        };

        let src = doc.rope.to_string();
        let byte_offset = position_to_byte_offset(&src, pos);

        let name = match word_at_offset(&src, byte_offset) {
            Some(w) => w.to_owned(),
            None => return Ok(None),
        };

        debug!("references: looking for '{name}'");

        // Local references.
        let mut locations: Vec<Location> = find_references(ast, &name)
            .into_iter()
            .map(|sp| Location::new(uri.clone(), byte_span_to_lsp_range(&src, sp)))
            .collect();

        // Cross-file references from the workspace index.
        locations.extend(self.workspace.find_references(&uri, &name));

        if locations.is_empty() {
            return Ok(None);
        }

        Ok(Some(locations))
    }

    // ── Document symbols ─────────────────────────────────────────────────

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let symbols = symbols_from_doc(&doc);
        if symbols.is_empty() {
            return Ok(None);
        }

        let src = doc.rope.to_string();

        #[allow(deprecated)] // `deprecated` field is required but deprecated in the spec
        let items: Vec<DocumentSymbol> = symbols
            .iter()
            .map(|sym| {
                let range = byte_span_to_lsp_range(&src, sym.span);
                DocumentSymbol {
                    name: sym.name.clone(),
                    detail: sym.detail.clone(),
                    kind: urd_symbol_kind_to_lsp(&sym.kind),
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: None,
                }
            })
            .collect();

        Ok(Some(DocumentSymbolResponse::Nested(items)))
    }

    // ── Semantic tokens ──────────────────────────────────────────────────

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return Ok(None),
        };

        let src = doc.rope.to_string();
        let raw_tokens = compute_semantic_tokens(ast);

        if raw_tokens.is_empty() {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: vec![],
            })));
        }

        // Convert absolute byte-offset tokens to the delta-encoded form that
        // LSP expects.  Each token is relative to the previous one:
        //   delta_line, delta_start (on same line), length, type, modifiers.
        let mut data: Vec<SemanticToken> = Vec::with_capacity(raw_tokens.len());
        let mut prev_line: u32 = 0;
        let mut prev_start: u32 = 0;

        for tok in &raw_tokens {
            let pos = byte_offset_to_lsp_position(&src, tok.start);
            let delta_line = pos.line - prev_line;
            let delta_start = if delta_line == 0 {
                pos.character - prev_start
            } else {
                pos.character
            };

            data.push(SemanticToken {
                delta_line,
                delta_start,
                // LSP semantic token length is measured in UTF-16 code units.
                length: utf16_code_units_in_byte_range(&src, tok.start, tok.length),
                token_type: urd_token_type_to_index(&tok.token_type),
                token_modifiers_bitset: 0,
            });

            prev_line = pos.line;
            prev_start = pos.character;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let pos = params.position;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return Ok(None),
        };

        let src = doc.rope.to_string();
        let byte_offset = position_to_byte_offset(&src, pos);

        let word = match word_at_offset(&src, byte_offset) {
            Some(w) => w.to_owned(),
            None => return Ok(None),
        };

        // Only allow renaming if the word resolves to a known symbol.
        let symbols = collect_symbols(ast);
        let is_known = symbols.iter().any(|s| s.name == word);
        if !is_known {
            return Ok(None);
        }

        // Find the precise span of the word under the cursor so the editor
        // can highlight it.
        let spans = find_rename_spans(ast, &src, &word);
        let cursor_span = spans
            .iter()
            .find(|sp| byte_offset >= sp.start && byte_offset < sp.end);

        match cursor_span {
            Some(sp) => {
                let range = byte_span_to_lsp_range(&src, *sp);
                Ok(Some(PrepareRenameResponse::Range(range)))
            }
            None => Ok(None),
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let new_name = params.new_name;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return Ok(None),
        };

        let src = doc.rope.to_string();
        let byte_offset = position_to_byte_offset(&src, pos);

        let old_name = match word_at_offset(&src, byte_offset) {
            Some(w) => w.to_owned(),
            None => return Ok(None),
        };

        debug!("rename: '{old_name}' -> '{new_name}'");

        let spans = find_rename_spans(ast, &src, &old_name);
        if spans.is_empty() {
            return Ok(None);
        }

        let edits: Vec<TextEdit> = spans
            .iter()
            .map(|sp| TextEdit {
                range: byte_span_to_lsp_range(&src, *sp),
                new_text: new_name.clone(),
            })
            .collect();

        let mut changes = std::collections::HashMap::new();
        changes.insert(uri, edits);

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri;

        let doc = match self.documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let src = doc.rope.to_string();
        let mut actions: Vec<CodeActionOrCommand> = Vec::new();

        // Offer "Create label 'name'" for every UndefinedLabel diagnostic
        // whose span overlaps the requested range.
        for err in &doc.analysis_errors {
            if let AnalysisError::UndefinedLabel { label, span, .. } = err {
                let diag_range = byte_span_to_lsp_range(&src, *span);

                // Check that the diagnostic overlaps the editor's requested range.
                if diag_range.end < params.range.start || diag_range.start > params.range.end {
                    continue;
                }

                // Build the snippet to insert: a new label block after the end of the file.
                let eof_pos = byte_offset_to_lsp_position(&src, src.len());
                let insert_text = format!("\nlabel {label} {{\n    todo!()\n}}\n");

                let mut changes = std::collections::HashMap::new();
                changes.insert(
                    uri.clone(),
                    vec![TextEdit {
                        range: Range::new(eof_pos, eof_pos),
                        new_text: insert_text,
                    }],
                );

                let action = CodeAction {
                    title: format!("Create label '{label}'"),
                    kind: Some(CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![Diagnostic {
                        range: diag_range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        source: Some("urd".into()),
                        message: err.to_string(),
                        ..Default::default()
                    }]),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    }),
                    is_preferred: Some(true),
                    ..Default::default()
                };

                actions.push(CodeActionOrCommand::CodeAction(action));
            }
        }

        // Offer "Replace with '…'" and "Add '…' to dictionary" for every
        // spell-check diagnostic.  The replacement action is preferred (shown
        // first and triggered by the default quick-fix keybinding); the
        // dictionary action is always available as an alternative.
        #[cfg(feature = "spellcheck")]
        for diag in &params.context.diagnostics {
            if diag.source.as_deref() == Some("urd-spell") {
                let data = diag.data.as_ref();

                let word = data
                    .and_then(|d| d.get("word"))
                    .and_then(|v| v.as_str())
                    .map(str::to_owned);

                // `suggestion` is `null` in JSON when SymSpell had no candidate.
                let suggestion = data
                    .and_then(|d| d.get("suggestion"))
                    .and_then(|v| v.as_str())
                    .map(str::to_owned);

                // "Replace with 'X'" — only when a suggestion is available.
                if let (Some(w), Some(sug)) = (&word, &suggestion) {
                    let corrected = match_casing(w, sug);
                    let mut changes = std::collections::HashMap::new();
                    changes.insert(
                        uri.clone(),
                        vec![TextEdit {
                            range: diag.range,
                            new_text: corrected.clone(),
                        }],
                    );
                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Replace with '{corrected}'"),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diag.clone()]),
                        edit: Some(WorkspaceEdit {
                            changes: Some(changes),
                            ..Default::default()
                        }),
                        is_preferred: Some(true),
                        ..Default::default()
                    }));
                }

                // "Add '…' to dictionary" — always available regardless of
                // whether a suggestion exists.
                if let Some(word) = word {
                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Add '{word}' to dictionary"),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diag.clone()]),
                        command: Some(Command {
                            title: format!("Add '{word}' to dictionary"),
                            command: "urd.addToDictionary".to_string(),
                            arguments: Some(vec![serde_json::json!(word)]),
                        }),
                        is_preferred: Some(false),
                        ..Default::default()
                    }));
                }
            }
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    #[cfg(feature = "spellcheck")]
    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        if params.command == "urd.addToDictionary" {
            let word = params
                .arguments
                .first()
                .and_then(|v| v.as_str())
                .map(str::to_owned);

            let Some(word) = word else {
                return Ok(None);
            };

            // Persist to the user dictionary (in-memory + disk).
            {
                let mut dict = self.user_dict.write().unwrap_or_else(|e| e.into_inner());
                if let Err(e) = dict.add(&word) {
                    tracing::warn!("user-dict: failed to write '{}' to disk: {e}", word);
                }
            }

            self.client
                .log_message(
                    MessageType::INFO,
                    format!("urd-lsp: added '{word}' to user dictionary"),
                )
                .await;

            // Re-run spellcheck on every open document so the word is no longer flagged.
            let language = *self
                .spellcheck_language
                .read()
                .unwrap_or_else(|e| e.into_inner());
            let uris: Vec<Url> = self
                .documents
                .iter()
                .map(|entry| entry.key().clone())
                .collect();
            {
                let dict = self.user_dict.read().unwrap_or_else(|e| e.into_inner());
                for uri in &uris {
                    if let Some(mut doc) = self.documents.get_mut(uri) {
                        doc.run_spellcheck(language, dict.words());
                    }
                }
            }

            // Republish clean diagnostics for all documents.
            for uri in uris {
                self.publish_diagnostics(uri).await;
            }
        }

        Ok(None)
    }
}

/// Adjust the casing of `suggestion` to mirror the pattern of `original`.
///
/// Three patterns are recognised:
///
/// - **All-caps** — every alphabetic character in `original` is uppercase
///   (e.g. `"NASA"`, `"ROCKEET"`): the suggestion is uppercased.
/// - **Title-case** — `original` starts with an uppercase letter
///   (e.g. `"Rocket"`, `"Доргу"`): the first character of the suggestion is
///   uppercased and the rest are left as-is.
/// - **Lowercase / other** — `original` starts with a lowercase letter or
///   the pattern is not recognised: `suggestion` is returned unchanged.
#[cfg(feature = "spellcheck")]
fn match_casing(original: &str, suggestion: &str) -> String {
    let mut orig_chars = original.chars();
    let first = match orig_chars.next() {
        Some(c) => c,
        None => return suggestion.to_owned(),
    };

    // All-caps: every alphabetic character is uppercase.
    if first.is_uppercase()
        && original
            .chars()
            .all(|c| c.is_uppercase() || !c.is_alphabetic())
    {
        return suggestion.to_uppercase();
    }

    // Title-case: only the first character is uppercase.
    if first.is_uppercase() {
        let mut result = String::new();
        let mut sug = suggestion.chars();
        if let Some(c) = sug.next() {
            for upper in c.to_uppercase() {
                result.push(upper);
            }
        }
        result.push_str(sug.as_str());
        return result;
    }

    suggestion.to_owned()
}

/// Thin wrapper around [`document::byte_offset_to_position`] for use inside
/// the semantic-token encoder (avoids a public re-export clash).
fn byte_offset_to_lsp_position(src: &str, byte_offset: usize) -> Position {
    document::byte_offset_to_position(src, byte_offset)
}

/// Convert a byte range in `src` to a UTF-16 code-unit length.
///
/// LSP semantic-token lengths are specified in UTF-16 code units, not bytes.
fn utf16_code_units_in_byte_range(src: &str, start: usize, byte_len: usize) -> u32 {
    let start = start.min(src.len());
    let end = start.saturating_add(byte_len).min(src.len());
    src.get(start..end)
        .map(|s| s.encode_utf16().count() as u32)
        .unwrap_or(0)
}

// ── Entry point ──────────────────────────────────────────────────────────────

#[tokio::main]
async fn main() {
    // Keep the line number of the surrounding code stable.
    // Initialise tracing (logs go to stderr so they don't interfere with
    // the JSON-RPC channel on stdin/stdout).
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .with_writer(std::io::stderr)
        .init();

    info!("starting urd-lsp");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let semantic: Arc<dyn SemanticSuggest + Send + Sync> = Arc::new(SynonymStore);

    let (service, socket) = LspService::build(|client| Backend {
        client,
        documents: DashMap::new(),
        workspace: Arc::new(WorkspaceIndex::new()),
        semantic,
        #[cfg(feature = "spellcheck")]
        spellcheck_language: std::sync::RwLock::new(None),
        #[cfg(feature = "spellcheck")]
        user_dict: Arc::new(std::sync::RwLock::new(UserDictionary::new(
            std::path::PathBuf::from(".urd-dict"),
        ))),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── match_casing ─────────────────────────────────────────────────────────

    #[test]
    #[cfg(feature = "spellcheck")]
    fn match_casing_lowercase_unchanged() {
        assert_eq!(match_casing("rockeet", "rocket"), "rocket");
    }

    #[test]
    #[cfg(feature = "spellcheck")]
    fn match_casing_title_case_capitalises_first() {
        assert_eq!(match_casing("Rockeet", "rocket"), "Rocket");
    }

    #[test]
    #[cfg(feature = "spellcheck")]
    fn match_casing_all_caps_uppercases_suggestion() {
        assert_eq!(match_casing("ROCKEET", "rocket"), "ROCKET");
    }

    #[test]
    #[cfg(feature = "spellcheck")]
    fn match_casing_cyrillic_lowercase_unchanged() {
        assert_eq!(match_casing("доргу", "дорогу"), "дорогу");
    }

    #[test]
    #[cfg(feature = "spellcheck")]
    fn match_casing_cyrillic_title_case() {
        assert_eq!(match_casing("Доргу", "дорогу"), "Дорогу");
    }

    #[test]
    #[cfg(feature = "spellcheck")]
    fn match_casing_empty_original_returns_suggestion() {
        assert_eq!(match_casing("", "rocket"), "rocket");
    }

    // -- word_at_offset ----------------------------------------------------

    #[test]
    fn word_at_offset_simple_ident() {
        let src = "jump foo_bar";
        // Cursor on 'f' (byte 5)
        assert_eq!(word_at_offset(src, 5), Some("foo_bar"));
    }

    #[test]
    fn word_at_offset_dotted_path() {
        let src = "jump cave.start";
        assert_eq!(word_at_offset(src, 7), Some("cave.start"));
    }

    #[test]
    fn word_at_offset_at_boundary() {
        let src = "let x = 42";
        // Cursor right after 'x' (byte 5, which is ' ')
        assert_eq!(word_at_offset(src, 4), Some("x"));
    }

    #[test]
    fn word_at_offset_on_whitespace() {
        let src = "a  b";
        // Byte 1 is space
        assert_eq!(word_at_offset(src, 2), None);
    }

    #[test]
    fn word_at_offset_at_eof() {
        let src = "hello";
        assert_eq!(word_at_offset(src, 5), Some("hello"));
    }

    #[test]
    fn word_at_offset_empty_source() {
        assert_eq!(word_at_offset("", 0), None);
    }

    #[test]
    fn word_at_offset_keyword() {
        let src = "label start {";
        assert_eq!(word_at_offset(src, 0), Some("label"));
        assert_eq!(word_at_offset(src, 6), Some("start"));
    }

    // -- urd_token_type_to_index ------------------------------------------

    #[test]
    fn token_type_indices_are_in_range() {
        let types = [
            UrdTokenType::Keyword,
            UrdTokenType::Label,
            UrdTokenType::Variable,
            UrdTokenType::String,
            UrdTokenType::Number,
            UrdTokenType::Operator,
            UrdTokenType::EnumMember,
            UrdTokenType::Struct,
            UrdTokenType::Decorator,
            UrdTokenType::Function,
        ];
        for tt in &types {
            let idx = urd_token_type_to_index(tt) as usize;
            assert!(
                idx < TOKEN_TYPES.len(),
                "index {idx} out of range for {tt:?}"
            );
        }
    }

    #[test]
    fn utf16_code_units_in_byte_range_ascii_matches_bytes() {
        let src = "hello";
        assert_eq!(utf16_code_units_in_byte_range(src, 0, 5), 5);
    }

    #[test]
    fn utf16_code_units_in_byte_range_surrogate_pair_counts_two() {
        // '𐍈' is 4 bytes in UTF-8 and 2 UTF-16 code units.
        let src = "a𐍈b";
        let start = src.find('𐍈').unwrap();
        assert_eq!(
            utf16_code_units_in_byte_range(src, start, '𐍈'.len_utf8()),
            2
        );
    }

    #[test]
    fn utf16_code_units_in_byte_range_clamps_end() {
        let src = "ab";
        assert_eq!(utf16_code_units_in_byte_range(src, 1, 99), 1);
    }

    // -- symbol kind mapping -----------------------------------------------

    #[test]
    fn symbol_kinds_map_without_panic() {
        let kinds = [
            UrdSymbolKind::Label,
            UrdSymbolKind::Variable,
            UrdSymbolKind::Constant,
            UrdSymbolKind::Global,
            UrdSymbolKind::Enum,
            UrdSymbolKind::EnumVariant,
            UrdSymbolKind::Struct,
            UrdSymbolKind::Decorator,
            UrdSymbolKind::Import,
        ];
        for k in &kinds {
            let _lsp = urd_symbol_kind_to_lsp(k);
            let _comp = urd_symbol_kind_to_completion(k);
        }
    }
}
