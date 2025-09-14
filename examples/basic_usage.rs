use gf_core::*;
use std::fs;
use bytes::Bytes;

/*
This program will:
  1. Loads the Food.pgf grammar file from grammars/Food/
  2. Parses it and converts it to JSON
  3. Creates a GFGrammar from the JSON
  4. Parses "this fish is delicious" into an AST
  5. Linearizes the AST back to English text
  6. Shows available concrete grammars

  You can enable debug output by uncommenting the set_debug(true) line below.
  This will show detailed information about parsing, linearization, and translation.
*/

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Uncomment the line below to see debug output from GF-Core operations
    set_debug(true);
    
    // Load a PGF grammar from binary file and convert to JSON
    println!("LOG: Reading binary PGF file from disk...");
    let pgf_content = fs::read("grammars/Food/Food.pgf")?;
    
    println!("LOG: Attemp at parsing PGF file Food.pgf starting...");
    let pgf = pgf2json::parse_pgf(&Bytes::from(pgf_content))?;
    
    println!("LOG: Attemp at transforming pgf data to json...");
    let json_string = pgf2json::pgf_to_json(&pgf)?;
    
    println!("LOG: Result of parsing (json_string): {json_string}");
    println!("TODO: Verify that json_string is json formated!");
    
    // Parse JSON string back into PGF struct for runtime usage
    let json_value: serde_json::Value = serde_json::from_str(&json_string)?;
    println!("TODO: Understand purpose of (json_value): {json_string}" );
    
    let pgf_struct: PGF = serde_json::from_value(json_value)?;
    println!("TODO: Understand content of (pgf_struct): {:?}", pgf_struct);
    
    // Convert to runtime grammar
    println!("TODO: Purpose of GFGrammar::from_json(pgf_struct)");
    let grammar: GFGrammar = GFGrammar::from_json(pgf_struct);
    
    
    // Test our fixed abstract syntax parsing
    println!("LOG: Testing fixed abstract syntax parsing");
    
    let test_cases = [
        "Fish",
        "This(Fish)", 
        "Is(This(Fish), Delicious)",
        "MakeS (NP) (VP)",
    ];
    
    for test_case in test_cases {
        println!("\nTesting abstract syntax: '{}'", test_case);
        match grammar.abstract_grammar.parse_tree(test_case, None) {
            Some(tree) => {
                println!("✓ Parsed successfully: {}", tree.print());
                
                // Try to linearize this tree
                if let Some(eng_concrete) = grammar.concretes.get("FoodEng") {
                    println!("LOG: Attempting linearization with FoodEng concrete grammar");
                    let english_output = eng_concrete.linearize(&tree);
                    println!("  English: '{}'", english_output);
                }
            }
            None => {
                println!("✗ Failed to parse");
            }
        }
    }
    
    // Also test the natural language parsing for comparison
    println!("\n=== Natural Language Parsing (for comparison) ===");
    if let Some(eng_concrete) = grammar.concretes.get("FoodEng") {
        let trees = eng_concrete.parse_string("this fish is delicious", &grammar.abstract_grammar.startcat);
        
        if trees.is_empty() {
            println!("No valid parse found for 'this fish is delicious'");
        } else {
            println!("Found {} parse tree(s):", trees.len());
            for (i, tree) in trees.iter().enumerate() {
                println!("  Tree {}: {}", i + 1, tree.print());
                
                // Linearize back to English
                let english_output = eng_concrete.linearize(tree);
                println!("  English: {}", english_output);
            }
        }
    }

    // Note: FoodIta concrete grammar not available in this PGF file
    println!("Available concrete grammars: {:?}", grammar.concretes.keys().collect::<Vec<_>>());

    Ok(())
}