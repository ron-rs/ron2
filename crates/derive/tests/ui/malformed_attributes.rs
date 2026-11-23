#[derive(ron2::Ron)]
struct Duplicate {
    #[ron(rename = "a", rename = "b")]
    a: u32,
    #[ron(skip = true)]
    b: u32,
    #[ron(rename = c)]
    c: u32,
}

#[derive(ron2::Ron)]
#[ron(rename_all = "kebab-case")]
struct Kebab {
    user_name: u32,
}

#[derive(ron2::Ron)]
struct InvalidName {
    #[ron(rename = "user-name")]
    user_name: u32,
}

fn main() {}
