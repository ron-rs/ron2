// A field name that a struct and its `#[ron(flatten)]` fields share (after
// renames) can never be read and would be written twice.

#[derive(ron2::FromRon, ron2::ToRon)]
struct Inner {
    shared: i32,
}

// Outer field renamed onto a flattened field's name.
#[derive(ron2::FromRon)]
struct Outer {
    #[ron(rename = "shared")]
    text: String,
    #[ron(flatten)]
    inner: Inner,
}

#[derive(ron2::FromRon, ron2::ToRon)]
struct Other {
    shared: bool,
}

// Two flattened fields with a common name.
#[derive(ron2::ToRon)]
struct Siblings {
    #[ron(flatten)]
    inner: Inner,
    #[ron(flatten)]
    other: Option<Other>,
}

#[derive(ron2::FromRon, ron2::ToRon)]
struct Middle {
    #[ron(flatten)]
    inner: Inner,
}

// A conflict two levels down, in a struct variant.
#[derive(ron2::FromRon)]
enum Nested {
    Variant {
        shared: u8,
        #[ron(flatten)]
        middle: Middle,
    },
}

fn main() {}
