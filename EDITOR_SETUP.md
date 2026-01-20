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

   Add initialization options to pass schema directories:

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

   Or use an explicit schema path:
   ```ron
   #![schema = "./schemas/app_config.schema.ron"]

   (
       port: 8080,
   )
   ```

5. **Generate schemas** (using derive macros)

   ```rust
   use ron2::Ron;

   #[derive(Ron)]
   #[ron(output = "schemas/")]  // Output directory relative to crate root
   pub struct AppConfig {
       /// Server port
       port: u16,
       /// Hostname
       host: String,
   }
   ```

   Build your crate to generate schemas:
   ```bash
   cargo build
   ```

## VS Code

Coming soon. The LSP server supports standard LSP protocol and can be integrated with any editor.
