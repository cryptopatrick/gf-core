use gf_core::*;
use std::collections::HashMap;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let json_content = fs::read_to_string("tests/grammars/Zero.json")?;
    let json: serde_json::Value = serde_json::from_str(&json_content)?;
    let pgf: PGF = serde_json::from_value(json)?;
    let grammar = GFGrammar::from_json(pgf);
    
    // Parse multiple sentences and show all possible trees
    let sentences = vec!["eat apple", "drink water"];
    
    for sentence in sentences {
        println!("\n=== Parsing: '{}' ===", sentence);
        
        // Parse with English concrete
        if let Some(eng_concrete) = grammar.concretes.get("ZeroEng") {
            let trees = eng_concrete.parse_string(sentence, &grammar.abstract_grammar.startcat);
            
            if trees.is_empty() {
                println!("No valid parse found");
            } else {
                println!("Found {} parse tree(s):", trees.len());
                for (i, tree) in trees.iter().enumerate() {
                    println!("  Tree {}: {}", i + 1, tree.print());
                    
                    // Show linearizations in both languages
                    let eng_output = eng_concrete.linearize(tree);
                    println!("    English: {}", eng_output);
                    
                    if let Some(swe_concrete) = grammar.concretes.get("ZeroSwe") {
                        let swe_output = swe_concrete.linearize(tree);
                        println!("    Swedish: {}", swe_output);
                    }
                    
                    // Show tagged linearization
                    let tagged = eng_concrete.tag_and_linearize(tree);
                    println!("    Tagged tokens:");
                    for ts in tagged {
                        println!("      '{}' [tag: {}]", ts.token, ts.tag);
                    }
                }
            }
        }
    }
    
    // Demonstrate completion functionality
    println!("\n=== Completion Demo ===");
    if let Some(eng_concrete) = grammar.concretes.get("ZeroEng") {
        let partial_inputs = vec!["ea", "drink", ""];
        
        for partial in partial_inputs {
            println!("\nPartial input: '{}'", partial);
            let completion = eng_concrete.complete(partial, &grammar.abstract_grammar.startcat);
            
            println!("  Consumed tokens: {:?}", completion.consumed);
            println!("  Suggestions: {:?}", completion.suggestions);
        }
    }
    
    Ok(())
}