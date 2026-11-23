fn always(_: &u32) -> bool {
    true
}

#[derive(ron2::Ron)]
struct Conflicts {
    #[ron(opt, skip_serializing_if = "always")]
    a: u32,
    #[ron(flatten, rename = "x")]
    b: Inner,
    #[ron(skip, rename = "d")]
    d: u32,
}

#[derive(ron2::Ron, Default)]
struct Inner {
    x: u32,
}

#[derive(ron2::Ron)]
struct TupleField(#[ron(rename = "a")] u32, #[ron(default)] u32, #[ron(skip)] u32);

fn main() {}
