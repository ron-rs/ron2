## ron-extras

Implements everything missing from RON (Rusty Object Notation):

- ron-derive (fix serde limitations)
- ron-schema (like JSON schema but for RON)
- ron-lsp (auto completions for RON)

## How it works:

We introduce two new attributes:

```ron
#![type = "crate::path::to::MyStruct"]
#![schema = "my_struct.schema.ron"]
```

We define a central location in the system where we store the latest schema for each type. The schema attribute overrides the location.
Our `ron-derive` can generate the schema automatically.

