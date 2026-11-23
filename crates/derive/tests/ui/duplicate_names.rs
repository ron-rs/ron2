#[derive(ron2::Ron)]
struct Fields {
    #[ron(rename = "same")]
    a: u32,
    #[ron(rename = "same")]
    b: u32,
}

#[derive(ron2::Ron)]
#[ron(rename_all = "snake_case")]
enum Variants {
    MyVariant,
    #[ron(rename = "my_variant")]
    Other,
}

fn main() {}
