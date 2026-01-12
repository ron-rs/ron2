//! RON Language Server
//!
//! A Language Server Protocol implementation for RON (Rusty Object Notation) files.
//! Provides auto-completion, diagnostics, and hover documentation based on schemas.

use std::{collections::HashMap, path::PathBuf, sync::Arc};

use ron2_lsp::{diagnostics, hover, provide_completions, Document, SchemaResolver};
use serde::Deserialize;
use tokio::sync::RwLock;
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};

/// LSP configuration settings.
///
/// These can be set via `initializationOptions` or `workspace/didChangeConfiguration`.
///
/// Example VS Code settings:
/// ```json
/// {
///   "ron.schemaDirs": ["./schemas", "/path/to/global/schemas"]
/// }
/// ```
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    /// Directories to search for schema files.
    /// Paths can be absolute or relative to the workspace root.
    #[serde(default)]
    pub schema_dirs: Vec<String>,
}

/// Wrapper for configuration that may be nested under "ron" key.
#[derive(Debug, Default, Deserialize)]
pub struct ConfigWrapper {
    #[serde(default)]
    pub ron: Config,
}

/// The RON Language Server.
pub struct RonLanguageServer {
    /// LSP client for sending notifications.
    client: Client,
    /// Cache of open documents.
    documents: Arc<RwLock<HashMap<Url, Document>>>,
    /// Schema resolver for loading schemas.
    schema_resolver: Arc<SchemaResolver>,
    /// Workspace root folders.
    workspace_roots: Arc<RwLock<Vec<PathBuf>>>,
}

impl RonLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            schema_resolver: Arc::new(SchemaResolver::new()),
            workspace_roots: Arc::new(RwLock::new(Vec::new())),
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

    /// Update configuration from JSON value.
    async fn update_config(&self, value: serde_json::Value) {
        // Try to parse as ConfigWrapper (settings nested under "ron" key)
        let config = if let Ok(wrapper) = serde_json::from_value::<ConfigWrapper>(value.clone()) {
            wrapper.ron
        } else if let Ok(config) = serde_json::from_value::<Config>(value) {
            // Or try to parse directly as Config
            config
        } else {
            return;
        };

        // Resolve schema directories relative to workspace roots
        let workspace_roots = self.workspace_roots.read().await;
        let mut resolved_dirs: Vec<PathBuf> = Vec::new();

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Processing schema_dirs config: {:?} with workspace roots: {:?}",
                    config.schema_dirs, *workspace_roots
                ),
            )
            .await;

        if workspace_roots.is_empty()
            && config
                .schema_dirs
                .iter()
                .any(|d| !PathBuf::from(d).is_absolute())
        {
            self.client
                .log_message(
                    MessageType::WARNING,
                    "No workspace root set - relative schema_dirs paths cannot be resolved. Use absolute paths or open a workspace folder.",
                )
                .await;
        }

        for dir in &config.schema_dirs {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!("Adding absolute schema dir: {:?}", path),
                    )
                    .await;
                resolved_dirs.push(path);
            } else {
                // Resolve relative paths against each workspace root
                for root in workspace_roots.iter() {
                    let resolved = root.join(&path);
                    if resolved.exists() {
                        self.client
                            .log_message(
                                MessageType::INFO,
                                format!("Adding resolved schema dir: {:?}", resolved),
                            )
                            .await;
                        resolved_dirs.push(resolved);
                    } else {
                        self.client
                            .log_message(
                                MessageType::WARNING,
                                format!("Schema dir not found: {:?} (tried {:?})", dir, resolved),
                            )
                            .await;
                    }
                }
            }
        }

        self.schema_resolver.set_schema_dirs(resolved_dirs);

        // Re-validate all open documents
        self.revalidate_all_documents().await;
    }

    /// Re-validate all open documents after config change.
    async fn revalidate_all_documents(&self) {
        let documents = self.documents.read().await;
        let uris: Vec<Url> = documents.keys().cloned().collect();
        drop(documents);

        for uri in uris {
            self.validate_document(&uri).await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for RonLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Store workspace roots
        let mut roots = Vec::new();
        if let Some(folders) = params.workspace_folders {
            for folder in folders {
                if let Ok(path) = folder.uri.to_file_path() {
                    roots.push(path);
                }
            }
        }
        // Fall back to root_uri if no workspace folders
        if roots.is_empty() {
            if let Some(root_uri) = params.root_uri {
                if let Ok(path) = root_uri.to_file_path() {
                    roots.push(path);
                }
            }
        }
        *self.workspace_roots.write().await = roots;

        // Process initialization options
        if let Some(options) = params.initialization_options {
            self.update_config(options).await;
        }

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
                name: "ron2-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "RON Language Server initialized")
            .await;

        // Log workspace roots for debugging
        let roots = self.workspace_roots.read().await;
        self.client
            .log_message(MessageType::INFO, format!("Workspace roots: {:?}", *roots))
            .await;

        // Log configured schema dirs
        let dirs = self.schema_resolver.schema_dirs();
        self.client
            .log_message(MessageType::INFO, format!("Schema directories: {:?}", dirs))
            .await;
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        self.update_config(params.settings).await;
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
        self.documents
            .write()
            .await
            .remove(&params.text_document.uri);
        // Clear diagnostics when document is closed
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(uri) {
            let items = provide_completions(doc, position, &self.schema_resolver);
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
