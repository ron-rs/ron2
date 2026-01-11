## ron-extras

Extensions for RON (Rusty Object Notation):

- **ron2**: Standalone RON parser with full AST access, perfect round-trip fidelity, and zero-copy parsing (replaces the `ron` crate)
- **ron-derive**: Proc macro that generates schemas at compile time
- **ron-schema**: Core schema types, validation, and storage (like JSON schema but for RON)
- **ron-lsp**: Language server providing auto-completions for RON files

## How it works

We introduce two new attributes:

```ron
#![type = "crate::path::to::MyStruct"]
#![schema = "my_struct.schema.ron"]
```

We define a central location in the system where we store the latest schema for each type. The schema attribute overrides the location.
Our `ron-derive` can generate the schema automatically.

## Why ron2?

Unlike the main `ron` crate, ron2:
- **AST-first**: Parses to a complete AST preserving all source information
- **Perfect round-trip**: Comments, whitespace, and formatting are preserved exactly
- **Zero-copy**: Uses `Cow<'a, str>` for borrowing from source without allocation
- **No serde dependency**: Lightweight, works without serde
- **Std-only**: Always relies on `std`/`alloc`, so no `no_std` build is provided

## Build Commands

```bash
cargo build                              # Build all crates
cargo test                               # Run all tests
cargo test -p ron2                       # Test ron2
cargo test -p ron2 --features integer128 # Test with i128/u128 support
cargo run -p ron-lsp                     # Run the LSP server
```

## Editor Setup

### Helix

1. **Install the LSP server**

   ```bash
   cargo install --path crates/ron-lsp
   ```

   Or build and copy to your PATH:
   ```bash
   cargo build --release -p ron-lsp
   cp target/release/ron-lsp ~/.local/bin/
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

5. **Generate schemas** (if using `ron-derive`)

   ```rust
   use ron_derive::Ron;

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

### VS Code

Coming soon. The LSP server supports standard LSP protocol and can be integrated with any editor.

## License

Dual-licensed under Apache-2.0 and MIT.
