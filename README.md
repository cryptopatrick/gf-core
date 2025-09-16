gf-core
======

# GF Rust Runtime
Rust crate for working with Grammatical Framework.  
This crate contains a Rust implementation of the Grammatical Framework runtime.

> __Note:__ this is _not_ a full Grammatical Framework compiler, only a runtime.

## Applicability
This crate will enable the user to use GF in pure Rust source code bases.
This makes it possible to have GF-powered applications and services without the 
need for a Haskell driven-GF backend server.

## Quickstart
Here's a short example that illustrates the use of the runtime.
For longer examples, please consult the official GF documentation.

## Step 01: Write a grammar
To use this crate we need a grammar. Typically we want to write three grammars;
an _abstract_ grammar, and two _concrete_ grammars.

### Abstact Grammar File: Hello.gf

```haskell
 -- a "Hello World" grammar
    abstract Hello = {
  
      flags startcat = Greeting ;
  
      cat Greeting ; Recipient ;
  
      fun
        Hello : Recipient -> Greeting ;
        World, Mum, Friends : Recipient ;
    }
```

### Concrete English Grammar File: HelloEng.gf

```haskell
concrete HelloEng of Hello = {
      lincat Greeting, Recipient = {s : Str} ;
  
      lin
        Hello recip = {s = "hello" ++ recip.s} ;
        World = {s = "world"} ;
        Mum = {s = "mum"} ;
        Friends = {s = "friends"} ;
    }
```

### Concrete Italian Grammar File: HelloIta.gf

```haskell
concrete HelloIta of Hello = {
      lincat Greeting, Recipient = {s : Str} ;
      lin
        Hello recip = {s = "ciao" ++ recip.s} ;
        World = {s = "mondo"} ;
        Mum = {s = "mamma"} ;
        Friends = {s = "amici"} ;
    }

```

## Step 02: Compile the grammars into PGF
Once we have our grammars written, we need to compile them into PGF (Portable Grammar Format).
To do this, we need the GF CLI, which is part of the GF binary.

> The GF binary and cli can be downloaded from [this page](https://github.com/GrammaticalFramework/gf-core/releases/tag/release-3.12)

```
# Compile the grammar into Hello.pgf
gf -make HelloEng.gf HelloIta.gf
```
Running this command, GF will look at `Hello.gf` (since it's what the concrete syntaxes depend on) and then produce the PGF file: `Hello.pgf`.

### Step 03: use the runtime
Here's an example of using the gf-core runtime:  
This program will:
  1. Load the `Food.pgf` grammar file
  2. Parse it and convert it to JSON
  3. Create a GFGrammar from the JSON
  4. Parse the sentence "this fish is delicious" into an AST
  5. Linearizes the AST back to English text
  6. Shows available concrete grammars (English and Italian)

```rust
use gf_core::*;
use std::fs;
use bytes::Bytes;


fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Uncomment the line below to see debug output from GF-Core operations
    set_debug(true);
    
    // Read the binary file containing the PGF grammar.
    let pgf_content = fs::read("grammars/Food/Food.pgf")?;
    
    // Parse the binary content of the PGF file.
    let pgf = pgf2json::parse_pgf(&Bytes::from(pgf_content))?;
    
    // Convert the PGF data into JSON formatl
    let json_string = pgf2json::pgf_to_json(&pgf)?;

    // Parse JSON into JSON values.
    let json_value: serde_json::Value = serde_json::from_str(&json_string)?;
    
    // Create a JSON valued PGF struct.
    let pgf_struct: PGF = serde_json::from_value(json_value)?;
    println!("TODO: Understand content of (pgf_struct): {:?}", pgf_struct);
    
    // Create a GF grammar from the PGF struct.
    let grammar: GFGrammar = GFGrammar::from_json(pgf_struct);
    
    // Test our fixed abstract syntax parsing
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
```