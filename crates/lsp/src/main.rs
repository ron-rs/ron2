//! RON Language Server
//!
//! A Language Server Protocol implementation for RON (Rusty Object Notation) files.
//! Provides auto-completion, diagnostics, and hover documentation based on schemas.
//!
//! Work that may touch the file system (schema lookup, which can stat and
//! read schema files when the cache revalidates) runs on Tokio's blocking
//! pool via [`off_executor`]; handlers hold no lock while awaiting it.

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
    /// Cache of open documents. Shared with blocking tasks, which get their
    /// own `Arc` so that no lock is held while they run.
    documents: Arc<RwLock<HashMap<Url, Arc<Document>>>>,
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

    /// The open document at `uri`; the documents lock is released on return.
    async fn document(&self, uri: &Url) -> Option<Arc<Document>> {
        self.documents.read().await.get(uri).cloned()
    }

    /// Run `f` on the document at `uri` and the schema resolver, off the
    /// async executor.
    async fn with_document<T: Send + 'static>(
        &self,
        uri: &Url,
        f: impl FnOnce(&Document, &SchemaResolver) -> T + Send + 'static,
    ) -> Option<T> {
        let doc = self.document(uri).await?;
        let resolver = Arc::clone(&self.schema_resolver);
        off_executor(move || f(&doc, &resolver)).await
    }

    /// Validate a document and publish diagnostics.
    async fn validate_document(&self, uri: &Url) {
        let Some((diagnostics, version)) = self
            .with_document(uri, |doc, resolver| {
                (diagnostics::validate_document(doc, resolver), doc.version)
            })
            .await
        else {
            return;
        };
        // A newer edit arrived while validating: its own validation publishes.
        if self.document(uri).await.map(|doc| doc.version) != Some(version) {
            return;
        }
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(version))
            .await;
    }

    /// Apply settings from `initializationOptions` or `workspace/didChangeConfiguration`.
    async fn update_config(&self, value: serde_json::Value) {
        let Some(config) = parse_config(value) else {
            return;
        };
        let roots = self.workspace_roots.read().await.clone();
        let (dirs, unresolved) = {
            let roots = roots.clone();
            off_executor(move || resolve_schema_dirs(&config.schema_dirs, &roots))
                .await
                .unwrap_or_default()
        };

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
        self.documents
            .write()
            .await
            .insert(uri.clone(), Arc::new(doc));
        self.validate_document(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // We use full sync, so take the last change
        let Some(change) = params.content_changes.into_iter().last() else {
            return;
        };
        let doc = Document::new(uri.clone(), change.text, params.text_document.version);
        let mut documents = self.documents.write().await;
        if let Some(entry) = documents.get_mut(&uri) {
            // Blocking tasks still reading the old version keep their `Arc`.
            *entry = Arc::new(doc);
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

        let items = self
            .with_document(uri, move |doc, resolver| {
                provide_completions(doc, position, resolver)
            })
            .await
            .unwrap_or_default();
        Ok((!items.is_empty()).then_some(CompletionResponse::Array(items)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        Ok(self
            .with_document(uri, move |doc, resolver| {
                hover::provide_hover(doc, position, resolver)
            })
            .await
            .flatten())
    }
}

/// Run blocking work (file system access, schema lookups) on Tokio's
/// blocking pool so it does not stall the executor. `None` if `f` panicked.
async fn off_executor<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> Option<T> {
    tokio::task::spawn_blocking(f).await.ok()
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
    use ron2::schema::{Field, Schema, TypeKind, write_schema};

    use super::*;

    /// On a single-threaded runtime, any blocking file system access in a
    /// handler would run on the executor's only thread.
    #[tokio::test(flavor = "current_thread")]
    async fn schema_work_runs_off_the_executor() {
        let executor = std::thread::current().id();
        let worker = off_executor(|| std::thread::current().id()).await;
        assert!(worker.is_some_and(|worker| worker != executor));

        let dir = std::env::temp_dir().join(format!("ron2-lsp-main-{}", std::process::id()));
        let schema = Schema::new(TypeKind::Struct {
            fields: vec![
                Field::new("port", TypeKind::U16).with_doc("Server port"),
                Field::new("host", TypeKind::String),
            ],
        });
        write_schema("app::Config", &schema, Some(&dir)).unwrap();

        let (service, _socket) = LspService::new(RonLanguageServer::new);
        let server = service.inner();
        server.schema_resolver.set_schema_dirs(vec![dir.clone()]);
        let uri = Url::parse("file:///config.ron").unwrap();
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem::new(
                    uri.clone(),
                    "ron".into(),
                    1,
                    "#![type = \"app::Config\"]\n(\n    port: 1,\n    \n)".into(),
                ),
            })
            .await;

        let at = |line, character| TextDocumentPositionParams {
            text_document: TextDocumentIdentifier::new(uri.clone()),
            position: Position::new(line, character),
        };
        let hover = server
            .hover(HoverParams {
                text_document_position_params: at(2, 6),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .unwrap()
            .unwrap();
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markup, got {:?}", hover.contents);
        };
        assert!(markup.value.contains("Server port"), "{}", markup.value);

        // `port` is used, so only `host` is left to complete.
        let completion = server
            .completion(CompletionParams {
                text_document_position: at(3, 4),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .unwrap();
        let Some(CompletionResponse::Array(items)) = completion else {
            panic!("expected completion items, got {completion:?}");
        };
        let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
        assert_eq!(labels, ["host"]);

        // An unknown document has no answers.
        let mut other = at(0, 0);
        other.text_document.uri = Url::parse("file:///other.ron").unwrap();
        let hover = server
            .hover(HoverParams {
                text_document_position_params: other,
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .unwrap();
        assert!(hover.is_none());

        let _ = std::fs::remove_dir_all(dir);
    }

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
