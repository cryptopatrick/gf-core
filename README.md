gf-core
======

# GF Rust Runtime
Rust crate for working with Grammatical Framework.
This crate contains a Rust implementation of the Grammatical Framework runtime.

## Applicability
This crate will enable the user to use GF in pure Rust source code bases.
This makes it possible to have GF-powered applications and services without the need for a Haskell driven GF baskend server.

There is some discprepancy in version betveen this crate and the GF-library that is actively maintained.
Because of this, some of the latest features are surely missing, and the code
is not optimized in any way. It's our ambition to continue improving the speed of this crate, please check the PATH TO benchsuite, where we hope to compare this crate to other GF-runtimes.

## Usage with PGF JSON (recommended)
To use this crate we need a grammar. You can compile your grammar into JSON
which is the recommended format. The command for this is:

### Example of usage
1. Start with your GF grammar, let's say the following:

### Abstact Grammar File: Wizard.gf

```haskell
abstract Zero = {
  flags
    startcat = Utt ;

  cat
    Utt ;
    N ;

  fun
    eat: N -> Utt ;
    apple, banana: N ;
}
```

### Concrete English Grammar File: WizardEng.gf
```json
concrete ZeroEng of Zero = {
  lincat
    Utt, N, MassN = { s: Str };

  lin
    eat n = { s = "eat" ++ artIndef ++ n.s } ;

    apple = { s = "apple" } ;
    banana = { s = "banana" } ;

  oper
    artIndef : Str = pre {"a" ; "an" / strs {"a" ; "e" ; "i" ; "o"}} ;
    -- artIndef : Str = "a" ;
}
```

### Concrete French Grammar File: WizardFre.gf
```haskell
concrete ZeroSwe of Zero = {
  flags
    coding = utf8 ;
  
  param
    Gender = Utrum | Neutrum ;

  lincat
    Utt = { s: Str } ;
    N = { s: Str ; g: Gender } ;

  lin
    eat n = { s = "äta" ++ artIndef ! n.g ++ n.s } ;

    apple = { s = "äpple" ; g = Neutrum } ;
    banana = { s = "banan" ; g = Utrum } ;

  oper
    artIndef : Gender => Str = table {
      Utrum => "en" ;
      Neutrum => "ett"
    } ;
}
```

### Compile the grammars
Finally, we compile our GF grammar into JSON, using the gf cli (which you can install from here: PATH TO GF CLI).
This requires a GF version release of 3.10, or later.

```
gf --make --output-format=json
```

### Include the compiled grammar in your Rust code

```rust
use gf-core::{fromJSON, GFGrammar};
//File::io::readFile

// We read the generated grammar in json format, into memory.
let json = JSON.parse(readFileSync('./test/grammars/Zero.json').toString());
// Analyze the grammar and use it to create a GFGrammar object.
let grammar: GFGrammar = fromJSON(json);
```

### Use in Browser using WASM?

## Usage with PGF JS

**Compilation of GF to JavaScript will soon be deprecated, but these instructions are here
in case you can't/won't use the latest GF.**

Compile the grammar straight to Rust, without intermediate JSON step.

```shell
gf --make --output-format=rs
```
