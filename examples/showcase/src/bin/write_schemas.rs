use ron2::schema::RonSchema;
use ron_showcase::GameConfig;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let written = GameConfig::write_schemas(Some("schemas"))?;
    println!("Wrote {} schemas to schemas/", written.len());
    Ok(())
}
