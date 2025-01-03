gf-core
======

# GF Rust Runtime
Rust crate for working with Grammatical Framework.  
This crate contains a Rust implementation of the Grammatical Framework runtime.

> __Note:__ this is _not_ a full Grammatical Framework compiler, only a runtime.

## Applicability
This crate will enable the user to use GF in pure Rust source code bases.
This makes it possible to have GF-powered applications and services without the need for a Haskell driven-GF backend server.

## Usage with PGF JSON (recommended)
To use this crate we need a grammar. You can compile your grammar into JSON
which is the recommended format. The command for this is:

### Example of usage
...comming soon!

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

### Concrete Finnish Grammar File: HelloFin.gf

```haskell
    concrete HelloFin of Hello = {
      lincat Greeting, Recipient = {s : Str} ;
      lin
        Hello recip = {s = "terve" ++ recip.s} ;
        World = {s = "maailma"} ;
        Mum = {s = "äiti"} ;
        Friends = {s = "ystävät"} ;
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
// We read the generated grammar in json format, into memory.
// Analyze the grammar and use it to create a GFGrammar object.
```