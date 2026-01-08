//! RON Language Server
//!
//! A Language Server Protocol implementation for RON (Rusty Object Notation) files.
//! Provides auto-completion, diagnostics, and hover documentation based on schemas.

mod completion;
mod diagnostics;
mod document;
mod hover;
mod schema_resolver;

use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use document::Document;
use schema_resolver::SchemaResolver;

/// The RON Language Server.
pub struct RonLanguageServer {
    /// LSP client for sending notifications.
    client: Client,
    /// Cache of open documents.
    documents: Arc<RwLock<HashMap<Url, Document>>>,
    /// Schema resolver for loading schemas.
    schema_resolver: Arc<SchemaResolver>,
}

impl RonLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            schema_resolver: Arc::new(SchemaResolver::new()),
        }
    }

    /// Validate a document and publish diagnostics.
    async fn validate_document(&self, uri: &Url) {
        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(uri) {
            let diagnostics = diagnostics::validate_document(doc, &self.schema_resolver);
            self.client
                .publish_diagnostics(uri.clone(), diagnostics, Some(doc.version))
                .await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for RonLanguageServer {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ":".to_string(),
                        "(".to_string(),
                        ",".to_string(),
                        " ".to_string(),
                    ]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "ron-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "RON Language Server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let doc = Document::new(
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        );
        self.documents.write().await.insert(uri.clone(), doc);
        self.validate_document(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let mut documents = self.documents.write().await;

        if let Some(doc) = documents.get_mut(&uri) {
            // We use full sync, so take the last change
            if let Some(change) = params.content_changes.into_iter().last() {
                doc.update(change.text, params.text_document.version);
            }
        }
        drop(documents);
        self.validate_document(&uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.write().await.remove(&params.text_document.uri);
        // Clear diagnostics when document is closed
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(uri) {
            let items = completion::provide_completions(doc, position, &self.schema_resolver);
            if !items.is_empty() {
                return Ok(Some(CompletionResponse::Array(items)));
            }
        }
        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(uri) {
            return Ok(hover::provide_hover(doc, position, &self.schema_resolver));
        }
        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(RonLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
