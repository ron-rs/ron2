#[derive(ron2::ToRon)]
struct NotOption {
    #[ron(implicit_some)]
    value: u32,
}

fn main() {}
