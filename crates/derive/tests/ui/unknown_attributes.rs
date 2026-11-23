#[derive(ron2::Ron)]
#[ron(deny_unknown_field, rename_al = "camelCase")]
struct Container {
    #[ron(defualt)]
    a: u32,
}

#[derive(ron2::Ron)]
enum Variants {
    #[ron(skipp)]
    A,
}

fn main() {}
