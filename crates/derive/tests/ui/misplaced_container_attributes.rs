#[derive(ron2::Ron)]
#[ron(transparent)]
enum TransparentEnum {
    A(u32),
}

#[derive(ron2::Ron)]
#[ron(rename = "Other")]
enum RenamedEnum {
    A,
}

#[derive(ron2::Ron)]
#[ron(transparent)]
struct TwoFields {
    a: u32,
    b: u32,
}

#[derive(ron2::Ron)]
#[ron(rename_all = "camelCase")]
struct Tuple(u32);

#[derive(ron2::Ron)]
enum VariantRenameAll {
    #[ron(rename_all = "camelCase")]
    Tuple(u32),
}

fn main() {}
