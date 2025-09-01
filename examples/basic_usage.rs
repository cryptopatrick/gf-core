use gf_core::*;
use std::collections::HashMap;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load a PGF grammar from JSON file
    let json_content = fs::read_to_string("tests/grammars/Zero.json")?;
    let json: serde_json::Value = serde_json::from_str(&json_content)?;
    let pgf: PGF = serde_json::from_value(json)?;
    
    // Convert to runtime grammar
    let grammar = GFGrammar::from_json(pgf);
    
    // Parse an abstract syntax tree from string
    let tree = grammar.abstract_grammar
        .parse_tree("eat apple", None)
        .expect("Failed to parse tree");
    
    println!("Parsed tree: {}", tree.print());
    
    // Linearize the tree in English
    if let Some(eng_concrete) = grammar.concretes.get("ZeroEng") {
        let english_output = eng_concrete.linearize(&tree);
        println!("English: {}", english_output);
    }
    
    // Linearize the tree in Swedish
    if let Some(swe_concrete) = grammar.concretes.get("ZeroSwe") {
        let swedish_output = swe_concrete.linearize(&tree);
        println!("Swedish: {}", swedish_output);
    }
    
    Ok(())
}