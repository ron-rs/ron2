## ron-extras

Alternative, AST-powered implementation of the Rusty Object Notation (RON).

- **ron2**: Standalone RON parser with full AST access, perfect round-trip fidelity and pretty printing with comment preservation
- **ron2-derive**: Proc macro for AST<->Type conversions and schemas at compile time
- **ron2-lsp**: Language server providing auto-completions for RON files
- **ron2-doc**: RON Schema to Markdown docs generation
- **ron2-cli**: Command Line Interface to format RON and generate markdown docs from RON schema

## Why ron2?

Unlike the main `ron` crate, ron2:

- **AST-first**: Parses to a complete AST preserving all source information
- **Rich errors**: Since we already have the AST structure, type mismatch errors are more accurate
- **Perfect round-trip**: Comments, whitespace, and formatting are preserved exactly
- **Full RON data model**: Do to using our own derive, we don't run into [serde limitations]

[serde limitations]: https://github.com/ron-rs/ron?tab=readme-ov-file#limitations

This implementation is best for projects relying on complex, human-edited config files or for building custom DSLs.

## Who is ron2 not for?

- You need to work with types that only support serde
- You need to parse huge amounts of data (ron2 is a lot slower)

You should consider using if these alternatives are a better fit for your use case:

### Reference implementation

Use the reference implementation at https://github.com/ron-rs/ron
if you want a mature and fast implementation of a RON parser,
while accepting the limitations of serde's data model.

### Minimal implementation

If you care about compile and lean dependency trees, use https://github.com/not-fl3/nanoserde
to derive non-pretty printing serialization for Rust types.

## Additions to the RON format

Our goal is to be 100% compatible with the reference implementation, however we introduce two new attributes:

```ron
#![type = "crate::path::to::MyStruct"]
```

This attribute allows to resolve which schema to load, and enables LSP completions and validation.

```ron
#![schema = "schemas/MyType.schema.ron"]
```

## Editor Setup

See [EDITOR_SETUP.md](EDITOR_SETUP.md) for instructions on integrating with Helix, VS Code, and other editors.

## Acknowledgments

The `ron2` crate is derived from [ron](https://github.com/ron-rs/ron) by the RON developers. We thank them for their work on the original RON format and implementation.

## License

Dual-licensed under Apache-2.0 and MIT, same as the original `ron` crate.
