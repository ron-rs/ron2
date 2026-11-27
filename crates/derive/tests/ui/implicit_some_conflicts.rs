#[derive(ron2::Ron)]
struct Conflicts {
    #[ron(implicit_some, explicit)]
    a: Option<u32>,
    #[ron(skip, implicit_some)]
    b: Option<u32>,
    #[ron(skip_serializing, implicit_some)]
    c: Option<u32>,
    #[ron(flatten, implicit_some)]
    d: Option<Inner>,
}

#[derive(ron2::Ron, Default)]
struct Inner {
    x: u32,
}

#[derive(ron2::Ron)]
struct TupleField(#[ron(implicit_some)] Option<u32>);

fn main() {}
