use example_showcase::GameConfig;
use ron2::schema::RonSchema;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/schemas");
    let written = GameConfig::write_schemas(Some(dir))?;
    println!("Wrote {} schemas to {dir}", written.len());
    Ok(())
}
