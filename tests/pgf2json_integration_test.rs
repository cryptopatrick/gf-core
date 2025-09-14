use gf_core::*;
use std::fs;

#[test]
fn test_pgf2json_conversion_and_basic_usage() {
    // Load a PGF grammar from binary file and convert to JSON
    let pgf_content = fs::read("grammars/Hello/Hello.pgf").expect("Failed to read PGF file");
    let pgf = pgf2json::parse_pgf(pgf_content.into()).expect("Failed to parse PGF");
    let json_string = pgf2json::pgf_to_json(&pgf).expect("Failed to convert PGF to JSON");
    
    // Verify JSON conversion worked
    assert!(!json_string.is_empty());
    assert!(json_string.contains("\"abstract\""));
    assert!(json_string.contains("\"concretes\""));
    
    // Parse JSON string back into PGF struct for runtime usage
    let json_value: serde_json::Value = serde_json::from_str(&json_string).expect("Failed to parse JSON");
    let pgf_struct: PGF = serde_json::from_value(json_value).expect("Failed to deserialize PGF");

    // Convert to runtime grammar
    let grammar = GFGrammar::from_json(pgf_struct);
    
    // Verify the grammar has the expected structure
    assert_eq!(grammar.abstract_grammar.startcat, "Greeting");
    assert!(grammar.concretes.contains_key("HelloEng"));
    assert!(grammar.concretes.contains_key("HelloFre"));
    
    // Test parsing an abstract syntax tree from string
    let tree = grammar
        .abstract_grammar
        .parse_tree("hello world", None)
        .expect("Failed to parse tree");

    let tree_str = tree.print();
    assert!(!tree_str.is_empty());
    println!("Parsed AST tree: {}", tree_str);

    // Test linearization in English
    if let Some(eng_concrete) = grammar.concretes.get("HelloEng") {
        let english_output = eng_concrete.linearize(&tree);
        assert!(!english_output.is_empty());
        println!("English: {}", english_output);
    }

    // Test linearization in French
    if let Some(fre_concrete) = grammar.concretes.get("HelloFre") {
        let french_output = fre_concrete.linearize(&tree);
        assert!(!french_output.is_empty());
        println!("French: {}", french_output);
    }
}

#[test]
fn test_save_json_and_load_from_file() {
    // Convert PGF to JSON and save to file
    let pgf_content = fs::read("grammars/Hello/Hello.pgf").expect("Failed to read PGF file");
    let pgf = pgf2json::parse_pgf(pgf_content.into()).expect("Failed to parse PGF");
    let json_string = pgf2json::pgf_to_json(&pgf).expect("Failed to convert PGF to JSON");
    
    // Save JSON to file
    let json_path = "grammars/Hello/Hello.json";
    fs::write(json_path, &json_string).expect("Failed to write JSON file");
    
    // Load JSON from file and verify it works
    let loaded_json = fs::read_to_string(json_path).expect("Failed to read JSON file");
    assert_eq!(json_string, loaded_json);
    
    let json_value: serde_json::Value = serde_json::from_str(&loaded_json).expect("Failed to parse JSON");
    let pgf_struct: PGF = serde_json::from_value(json_value).expect("Failed to deserialize PGF");
    let grammar = GFGrammar::from_json(pgf_struct);
    
    // Test that the loaded grammar works
    let tree = grammar
        .abstract_grammar
        .parse_tree("hello world", None)
        .expect("Failed to parse tree with loaded grammar");
        
    assert!(!tree.print().is_empty());
    
    // Clean up
    fs::remove_file(json_path).unwrap_or(());
}