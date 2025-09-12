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
Here's a very short example that illustrates the use of the runtime.
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

### Concrete Finnish Grammar File: HelloIta.gf

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

### Step 03: Generate JSON
Next, we need to convert the `Hello.pgf` file into JSON format. We can do that __using 
a tool provided by GF framework called `pgf2json`.

```shell

```haskell
# Translate Hello.pgf into Hello.json format
gf --run Hello.pgf <<< ":i --format=json" > Hello.json
```
echo "hello_world.json" | gf --run Hello.pgf --output-format=json
```
It's this generated json file (hello_world.json) that we can use in our Rust programs (see below).


## Step 04: Use the compiled grammar in code

```rust
use gf_core::*;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load a PGF grammar from JSON file
    let json_content = fs::read_to_string("tests/grammars/Zero.json")?;
    let json: serde_json::Value = serde_json::from_str(&json_content)?;
    let pgf: PGF = serde_json::from_value(json)?;

    // Convert to runtime grammar
    let grammar = GFGrammar::from_json(pgf);

    // Parse an abstract syntax tree from string
    let tree = grammar
        .abstract_grammar
        // .parse_tree("eat apple", None)
        .parse_tree("eat apple", None)
        .expect("Failed to parse tree");

    println!("Parsed AST tree: {}", tree.print());

    // Linearize the tree in English
    if let Some(eng_concrete) = grammar.concretes.get("ZeroEng") {
    // if let Some(eng_concrete) = grammar.concretes.get("HelloEng") {
        let english_output = eng_concrete.linearize(&tree);
        println!("English: {}", english_output);
    }

    // Linearize the tree in Swedish
    if let Some(swe_concrete) = grammar.concretes.get("ZeroSwe") {
    // if let Some(swe_concrete) = grammar.concretes.get("HelloFre") {
        let swedish_output = swe_concrete.linearize(&tree);
        println!("Swedish: {}", swedish_output);
    }

    Ok(())
}
```