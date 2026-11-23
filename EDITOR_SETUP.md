# Editor Setup

## Helix

1. **Install the LSP server**

   ```bash
   cargo install --locked ron2-lsp
   ```

2. **Configure Helix languages** (`~/.config/helix/languages.toml`)

   ```toml
   [language-server.ron-lsp]
   command = "ron-lsp"

   [[language]]
   name = "ron"
   language-servers = ["ron-lsp"]
   ```

3. **Configure schema directories** (optional)

   Pass schema directories as initialization options (flat, or nested under a
   `ron` key as in VS Code settings):

   ```toml
   [language-server.ron-lsp]
   command = "ron-lsp"

   [language-server.ron-lsp.config]
   schemaDirs = ["./schemas", "/path/to/global/schemas"]
   ```

   Schema directories can be:
   - Relative paths (resolved from workspace root)
   - Absolute paths

4. **Link RON files to schemas**

   Add an attribute at the top of your RON files:

   ```ron
   #![type = "my_crate::config::AppConfig"]

   (
       port: 8080,
       host: "localhost",
   )
   ```

   Or use an explicit schema path, relative to the RON file:
   ```ron
   #![schema = "./schemas/app_config.schema.ron"]

   (
       port: 8080,
   )
   ```

5. **Generate schemas**

   Derive `Ron` (or `RonSchema`) and write the schemas with `write_schemas`,
   for example from a test or a small binary:

   ```rust
   use ron2::{Ron, schema::RonSchema};

   #[derive(Ron)]
   pub struct AppConfig {
       /// Server port
       port: u16,
       /// Hostname
       host: String,
   }

   fn main() -> Result<(), Box<dyn std::error::Error>> {
       // Writes schemas/my_crate/config/AppConfig.schema.ron and every type it references.
       AppConfig::write_schemas(Some("schemas"))?;
       Ok(())
   }
   ```

   `write_schemas(None)` writes to `RON_SCHEMA_DIR` or the XDG default
   (`~/.local/share/ron-schemas/`), which the LSP also searches.

## VS Code

Coming soon. The LSP server supports standard LSP protocol and can be integrated with any editor.
