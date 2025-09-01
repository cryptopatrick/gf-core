use gf_core::*;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let json_content = fs::read_to_string("tests/grammars/Zero.json")?;
    let json: serde_json::Value = serde_json::from_str(&json_content)?;
    let pgf: PGF = serde_json::from_value(json)?;
    let grammar = GFGrammar::from_json(pgf);
    
    println!("GF Translation Service");
    println!("======================");
    println!("Available languages: {:?}", grammar.concretes.keys().collect::<Vec<_>>());
    println!("Start category: {}", grammar.abstract_grammar.startcat);
    println!("Type 'quit' to exit\n");
    
    loop {
        print!("Enter text to translate: ");
        io::stdout().flush()?;
        
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let input = input.trim();
        
        if input == "quit" {
            break;
        }
        
        if input.is_empty() {
            continue;
        }
        
        println!("\n--- Translation Results ---");
        
        // Translate from all languages to all languages
        let translations = grammar.translate(input, None, None);
        
        if translations.is_empty() {
            println!("No valid translations found for: '{}'", input);
            
            // Show completion suggestions
            println!("\nCompletion suggestions:");
            for (lang_code, concrete) in &grammar.concretes {
                let completion = concrete.complete(input, &grammar.abstract_grammar.startcat);
                if !completion.suggestions.is_empty() {
                    println!("  {} suggestions: {:?}", lang_code, completion.suggestions);
                }
            }
        } else {
            for (source_idx, translation_map) in translations.iter().enumerate() {
                println!("Source parse {}:", source_idx + 1);
                
                for (tree_str, target_map) in translation_map {
                    println!("  Abstract tree: {}", tree_str);
                    
                    for (lang_code, translation) in target_map {
                        println!("    {}: {}", lang_code, translation);
                    }
                }
            }
        }
        
        // Demonstrate specific language pair translation
        println!("\n--- English to Swedish Translation ---");
        let eng_to_swe = grammar.translate(input, Some("ZeroEng"), Some("ZeroSwe"));
        
        if !eng_to_swe.is_empty() {
            for translation_map in &eng_to_swe {
                for (tree_str, target_map) in translation_map {
                    if let Some(swedish) = target_map.get("ZeroSwe") {
                        println!("EN→SV: {} → {}", input, swedish);
                    }
                }
            }
        }
        
        println!();
    }
    
    println!("Translation service ended.");
    Ok(())
}