use gf_core::*;
use std::fs;
use bytes::Bytes;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("GF-Core Simple Debug Demo");
    println!("=========================");
    
    // Load a PGF grammar from binary file and convert to JSON
    let pgf_content = fs::read("grammars/HelloFromGF-Core/Hello.pgf")?;
    let pgf = pgf2json::parse_pgf(&Bytes::from(pgf_content))?;
    let json_string = pgf2json::pgf_to_json(&pgf)?;
    
    // Parse JSON string back into PGF struct for runtime usage
    let json_value: serde_json::Value = serde_json::from_str(&json_string)?;
    let pgf: PGF = serde_json::from_value(json_value)?;
    
    println!("\n=== Without Debug Output ===");
    
    // Convert to runtime grammar (without debug)
    let grammar = GFGrammar::from_json(pgf);
    
    // Parse and linearize without debug
    let tree = grammar
        .abstract_grammar
        .parse_tree("hello world", None)
        .expect("Failed to parse tree");
    
    if let Some(eng_concrete) = grammar.concretes.get("HelloEng") {
        let english_output = eng_concrete.linearize(&tree);
        println!("English result: {}", english_output);
    }
    
    println!("\n=== With Debug Output ===");
    
    // Enable debug output
    set_debug(true);
    
    // Load PGF grammar again (since we need a fresh copy)
    let pgf_content = fs::read("grammars/HelloFromGF-Core/Hello.pgf")?;
    let pgf = pgf2json::parse_pgf(&Bytes::from(pgf_content))?;
    let json_string = pgf2json::pgf_to_json(&pgf)?;
    
    // Parse JSON string back into PGF struct for runtime usage
    let json_value: serde_json::Value = serde_json::from_str(&json_string)?;
    let pgf: PGF = serde_json::from_value(json_value)?;
    
    // Convert to runtime grammar (with debug)
    let grammar = GFGrammar::from_json(pgf);
    
    // Parse and linearize with debug
    let tree = grammar
        .abstract_grammar
        .parse_tree("hello world", None)
        .expect("Failed to parse tree");
    
    if let Some(eng_concrete) = grammar.concretes.get("HelloEng") {
        let english_output = eng_concrete.linearize(&tree);
        println!("English result: {}", english_output);
    }
    
    // Test French linearization
    if let Some(fre_concrete) = grammar.concretes.get("HelloFre") {
        let french_output = fre_concrete.linearize(&tree);
        println!("French result: {}", french_output);
    }
    
    // Test translation functionality with debug
    println!("\n--- Translation with Debug ---");
    let translations = grammar.translate("hello world", Some("HelloEng"), Some("HelloFre"));
    for (i, translation_set) in translations.iter().enumerate() {
        println!("Translation set {}:", i + 1);
        for (tree_name, lang_map) in translation_set {
            for (lang, text) in lang_map {
                println!("  {}: {}", lang, text);
            }
        }
    }
    
    // Disable debug output
    set_debug(false);
    println!("\n=== Debug Disabled ===");
    
    // This should not show debug output
    let _tree = grammar
        .abstract_grammar
        .parse_tree("hello world", None)
        .expect("Failed to parse tree");
    
    println!("Operations completed without debug output.");

    Ok(())
}