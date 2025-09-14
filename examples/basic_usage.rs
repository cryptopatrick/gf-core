use gf_core::*;
use std::fs;
use bytes::Bytes;

/*
This program will:
  1. Loads the Hello.pgf grammar file from grammars/Hello/
  2. Parses it and converts it to JSON
  3. Creates a GFGrammar from the JSON
  4. Parses "hello world" into an AST
  5. Prints the parsed AST tree: "hello world"

  You can enable debug output by uncommenting the set_debug(true) line below.
  This will show detailed information about parsing, linearization, and translation.
*/

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Uncomment the line below to see debug output from GF-Core operations
    // set_debug(true);
    
    // Load a PGF grammar from binary file and convert to JSON
    println!("LOG: Reading binary PGF file from disk...");
    let pgf_content = fs::read("grammars/Food/Food.pgf")?;
    
    println!("LOG: Attemp at parsing PGF file Food.pgf starting...");
    let pgf = pgf2json::parse_pgf(&Bytes::from(pgf_content))?;
    
    println!("LOG: Attemp at transforming pgf data to json...");
    let json_string = pgf2json::pgf_to_json(&pgf)?;
    
    println!("LOG: Result of parsing (json_string): {}", json_string);
    println!("TODO: Verify that json_string is json formated!");
    
    // Parse JSON string back into PGF struct for runtime usage
    let json_value: serde_json::Value = serde_json::from_str(&json_string)?;
    println!("TODO: Understand purpose of (json_value): {}", json_string);
    
    let pgf_struct: PGF = serde_json::from_value(json_value)?;
    println!("TODO: Understand content of (pgf_struct): {:?}", pgf_struct);
    
    // Convert to runtime grammar
    println!("TODO: Purpose of GFGrammar::from_json(pgf_struct)");
    let grammar: GFGrammar = GFGrammar::from_json(pgf_struct);
    
    
    // Parse an abstract syntax tree from string
    println!("LOG: Reached grammar reading abstact tree");
    let tree = grammar
    .abstract_grammar
    .parse_tree("hello world", None)
    .expect("Failed to parse tree");

println!("Parsed AST tree: {}", tree.print());

println!("TODO: Gain better understanding of what grammar.concretes.get(HelloEng) does.");
// Linearize the tree in English
if let Some(eng_concrete) = grammar.concretes.get("HelloEng") {
        println!("LOG: Reached linearization attempt of english AST: hello world");
        let english_output = eng_concrete.linearize(&tree);
        println!("English: {english_output}");
    }

    // Note: HelloIta concrete grammar not available due to PGF parsing issues
    println!("Available concrete grammars: {:?}", grammar.concretes.keys().collect::<Vec<_>>());

    Ok(())
}
