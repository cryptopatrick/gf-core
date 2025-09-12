use gf_core::*;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load a PGF grammar from JSON file
    let json_content = fs::read_to_string("tests/grammars/Hello/Hello.json")?;
    let json: serde_json::Value = serde_json::from_str(&json_content)?;
    let pgf: PGF = serde_json::from_value(json)?;

    // Convert to runtime grammar
    let grammar = GFGrammar::from_json(pgf);

    // Parse an abstract syntax tree from string
    let tree = grammar
        .abstract_grammar
        .parse_tree("hello world", None)
        .expect("Failed to parse tree");

    println!("Parsed AST tree: {}", tree.print());

    // Linearize the tree in English
    if let Some(eng_concrete) = grammar.concretes.get("HelloEng") {
        let english_output = eng_concrete.linearize(&tree);
        println!("English: {}", english_output);
    }

    // Linearize the tree in French
    if let Some(fre_concrete) = grammar.concretes.get("HelloFre") {
           let french_output = fre_concrete.linearize(&tree);
        println!("French: {}", french_output);
    }

    Ok(())
}
