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

### Concrete Finnish Grammar File: HelloFre.gf

```haskell
  concrete HelloFre of Hello = {
    lincat Greeting, Recipient = {s : Str} ;

    lin
      Hello recip = {s = "bonjour" ++ recip.s} ;
      World = {s = "le monde"} ;
      Mum = {s = "maman"} ;
      Friends = {s = "les amis"} ;
  }
```

## Step 02: Compile the grammars
Once we have our grammars written, we need to compile them. We do so using the GF
compiler, and choose JSON as the output format.
Usage with PGF JSON (recommended)
You need to compile them into PGF (Portable Grammar Format), which is GF’s binary format:

> The GF binary and cli can be downloaded from [this page](https://github.com/GrammaticalFramework/gf-core/releases/tag/release-3.12)

```
# json output requires a GF version of 3.10, or later.
% echo "l Hello World" | gf HelloEng.gf HelloFin.gf HelloIta.gf
gf --make helloEng.gf helloFre.gf 
```
Running this command, GF will look at `hello.gf` (since it's what the concrete syntaxes depend on) and then produce the PGF file: `hello.pgf`.

### Step 03: Generate JSON
Once we have our grammar in `.pgf` format, we can use it to generate a json version.

```shell
echo "hello_world.json" | gf --run hello.pgf --output-format=json
```


### Automatically export all funccitons to export_json.sh

```shell
#!/bin/bash
set -e

# Compile the grammars
gf --make helloEng.gf helloFre.gf

PGF="hello.pgf"
LANGS=("Eng" "Fre")

# Get list of all functions in the grammar
FUNCS=($(gf --functions "$PGF"))

echo '{'

first_func=true
for f in "${FUNCS[@]}"; do
  if [ "$first_func" = false ]; then
    echo ','
  fi
  first_func=false

  echo -n "  \"$f\": {"

  first_lang=true
  for l in "${LANGS[@]}"; do
    if [ "$first_lang" = false ]; then
      echo -n ','
    fi
    first_lang=false

    echo -n "\"$l\":"
    echo "linearize -lang=$l $f" | gf --run --output-format=json "$PGF" | tr -d '\n'
  done

  echo -n "}"
done

echo
echo '}'
```

```shell
# Run the script and store the result in a file.
./export_json.sh > results.json
```



## Step 03: Use the compiled grammar in code

```rust
use gf_core::*;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load a PGF grammar from JSON file
    let json_content = fs::read_to_string("<path-to-grammar-files.json>")?;
    let json: serde_json::Value = serde_json::from_str(&json_content)?;
    let pgf: PGF = serde_json::from_value(json)?;

    // Convert to runtime grammar
    let grammar = GFGrammar::from_json(pgf);

    // Parse an abstract syntax tree from string
    let tree = grammar
        .abstract_grammar
        .parse_tree("eat apple", None)
        .expect("Failed to parse tree");

    println!("Parsed tree: {}", tree.print());

    // Linearize the tree in English
    if let Some(eng_concrete) = grammar.concretes.get("HelloEng") {
        let english_output = eng_concrete.linearize(&tree);
        println!("English: {}", english_output);
    }

    // Linearize the tree in Swedish
    if let Some(fre_concrete) = grammar.concretes.get("HelloFre") {
        let french_output = fre_concrete.linearize(&tree);
        println!("French: {}", french_output);
    }

    Ok(())
}
```