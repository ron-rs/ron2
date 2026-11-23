#[derive(ron2::FromRon)]
struct NotOption {
    #[ron(explicit)]
    value: u32,
}

fn main() {}
