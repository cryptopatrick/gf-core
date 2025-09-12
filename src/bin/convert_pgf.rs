use std::fs;
use std::env;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <input.pgf> <output.json>", args[0]);
        std::process::exit(1);
    }

    let input_path = &args[1];
    let output_path = &args[2];

    println!("Converting {} to {}", input_path, output_path);

    // Read the PGF file as binary data
    let pgf_data = fs::read(input_path)?;
    println!("Read {} bytes from {}", pgf_data.len(), input_path);

    // Parse PGF data
    let pgf = pgf2json::parse_pgf(pgf_data.into())?;
    
    // Convert to JSON
    let json_output = pgf2json::pgf_to_json(&pgf)?;
    
    // Write the JSON to file
    fs::write(output_path, json_output)?;
    println!("Successfully converted to {}", output_path);

    Ok(())
}