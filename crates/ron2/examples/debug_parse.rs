fn main() {
    // Test simple struct
    let simple = "Point { x: 1, y: 2 }";
    println!("Parsing: {:?}", simple);
    match ron2::ast::parse_document(simple) {
        Ok(doc) => println!("OK: {:?}", doc.value.map(|v| format!("{:?}", v))),
        Err(e) => println!("ERR: {:?}", e),
    }

    // Test with Config
    let config = "Config { name: \"test\" }";
    println!("\nParsing: {:?}", config);
    match ron2::ast::parse_document(config) {
        Ok(doc) => println!("OK: {:?}", doc.value.map(|v| format!("{:?}", v))),
        Err(e) => println!("ERR: {:?}", e),
    }

    // Compare with direct path
    println!("\nDirect path:");
    println!("  {:?}", ron2::from_str(simple));
    println!("  {:?}", ron2::from_str(config));
}
