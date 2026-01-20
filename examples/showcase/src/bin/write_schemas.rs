use example_showcase::GameConfig;
use ron2::schema::RonSchema;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let written = GameConfig::write_schemas(Some("schemas"))?;
    println!("Wrote {} schemas to schemas/", written.len());
    Ok(())
}
