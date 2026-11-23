//! Attribute misuse must be a compile error, not silently ignored.
//!
//! Regenerate the expected output with `TRYBUILD=overwrite cargo test -p ron2-derive --test compile_fail`.

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
