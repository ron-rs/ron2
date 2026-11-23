//! RON Language Server
//!
//! A Language Server Protocol implementation for RON (Rusty Object Notation) files.
//! Provides auto-completion, diagnostics, and hover documentation based on schemas.

use std::{path::PathBuf, sync::Arc};

use ahash::{HashMap, HashMapExt};
use ron2_lsp::{Document, SchemaResolver, diagnostics, hover, provide_completions};
use serde::Deserialize;
use tokio::sync::RwLock;
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result, lsp_types::*};

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
        // Release the documents lock before awaiting the client.
        let result = self.documents.read().await.get(uri).map(|doc| {
            (
                diagnostics::validate_document(doc, &self.schema_resolver),
                doc.version,
            )
        });
        if let Some((diagnostics, version)) = result {
            self.client
                .publish_diagnostics(uri.clone(), diagnostics, Some(version))
                .await;
        }
    }

    /// Apply settings from `initializationOptions` or `workspace/didChangeConfiguration`.
    async fn update_config(&self, value: serde_json::Value) {
        let Some(config) = parse_config(value) else {
            return;
        };
        let roots = self.workspace_roots.read().await.clone();
        let (dirs, unresolved) = resolve_schema_dirs(&config.schema_dirs, &roots);

        for dir in unresolved {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("Schema dir {dir:?} not found in workspace roots {roots:?}"),
                )
                .await;
        }
        self.client
            .log_message(MessageType::INFO, format!("Schema directories: {dirs:?}"))
            .await;

        self.schema_resolver.set_schema_dirs(dirs);
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
        if roots.is_empty()
            && let Some(root_uri) = params.root_uri
            && let Ok(path) = root_uri.to_file_path()
        {
            roots.push(path);
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
        let roots = self.workspace_roots.read().await.clone();
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "RON Language Server initialized (workspace roots: {roots:?}, schema directories: {:?})",
                    self.schema_resolver.schema_dirs()
                ),
            )
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

/// Read [`Config`] from settings, which may be nested under a `ron` key.
fn parse_config(value: serde_json::Value) -> Option<Config> {
    let settings = match value.get("ron") {
        Some(nested) => nested.clone(),
        None => value,
    };
    serde_json::from_value(settings).ok()
}

/// Resolve configured schema directories against the workspace roots.
///
/// Absolute paths are kept as-is; relative paths are kept for every root they
/// exist under. Returns the resolved directories and the relative paths that
/// matched no root.
fn resolve_schema_dirs(dirs: &[String], roots: &[PathBuf]) -> (Vec<PathBuf>, Vec<String>) {
    let mut resolved = Vec::new();
    let mut unresolved = Vec::new();
    for dir in dirs {
        let path = PathBuf::from(dir);
        if path.is_absolute() {
            resolved.push(path);
            continue;
        }
        let found: Vec<PathBuf> = roots
            .iter()
            .map(|root| root.join(&path))
            .filter(|p| p.exists())
            .collect();
        if found.is_empty() {
            unresolved.push(dir.clone());
        }
        resolved.extend(found);
    }
    (resolved, unresolved)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(RonLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_config_accepts_nested_and_flat_settings() {
        let nested = parse_config(serde_json::json!({"ron": {"schemaDirs": ["a"]}}));
        assert_eq!(nested.map(|c| c.schema_dirs), Some(vec!["a".to_string()]));
        // A flat object used to parse as an empty nested config and drop the dirs.
        let flat = parse_config(serde_json::json!({"schemaDirs": ["b"]}));
        assert_eq!(flat.map(|c| c.schema_dirs), Some(vec!["b".to_string()]));
    }

    #[test]
    fn resolve_schema_dirs_reports_missing_relative_dirs() {
        let root = std::env::temp_dir();
        let absolute = root.join("abs").to_string_lossy().into_owned();
        let (dirs, unresolved) = resolve_schema_dirs(
            &[
                absolute.clone(),
                ".".to_string(),
                "does-not-exist-ron2".to_string(),
            ],
            std::slice::from_ref(&root),
        );
        assert_eq!(dirs, [PathBuf::from(absolute), root.join(".")]);
        assert_eq!(unresolved, ["does-not-exist-ron2"]);
    }
}
