mod pgf_json;
pub use pgf_json::*;

use serde::Serialize;
use std::collections::HashMap;
use regex::Regex;






#[derive(Serialize, Debug, Clone)]
pub struct Type {
    /// Input categories.
    pub args: Vec<String>,
    /// Output category.
    pub cat: String,
} 

/// Rust Runtime for Grammatical Framework
///
/// This crate provides a runtime for Grammatical Framework (GF) grammars,
/// allowing parsing, linearization, and translation between languages defined
/// in GF abstract and concrete syntax.

////////////////////////////////////////////////////////////////////////////////
/// Constants and type aliases

/// Type alias for nested HashMaps used in translation outputs.
type HMS3 = HashMap<String, HashMap<String, String>>;

/// Every constituent has a unique id. If the constituent is discontinuous,
/// it will be represented with several brackets sharing the same id.
pub type FId = i32;

////////////////////////////////////////////////////////////////////////////////
/// Accumulator for completion suggestions during parsing.
#[derive(Debug, Clone)]
pub struct CompletionAccumulator {
    /// Optional vector of active items for suggestions.
    pub value: Option<Vec<ActiveItem>>,
}

impl CompletionAccumulator {
    /// Creates a new empty accumulator.
    pub fn new() -> Self {
        CompletionAccumulator { value: None }
    }
}

/// Result of completion, including consumed tokens and suggestions.
#[derive(Debug, Clone)]
pub struct CompletionResult {
    /// Tokens already consumed.
    pub consumed: Vec<String>,
    /// Suggested completions.
    pub suggestions: Vec<String>,
}

////////////////////////////////////////////////////////////////////////////////
/// A Grammatical Framework grammar consisting of one abstract and multiple concretes.
///
/// The `GFGrammar` type encapsulates an abstract grammar (logical structure) and
/// one or more concrete grammars (linearizations). The abstract defines categories
/// and functions independently of form, while concretes provide manifestations
/// (e.g., natural language strings).
///
/// # Example
///
/// ```
/// use gf_core::{GFGrammar, GFAbstract};
/// use std::collections::HashMap;
/// let abstract_ = GFAbstract::new("S".to_string(), HashMap::new());
/// let concretes = HashMap::new();
/// let grammar = GFGrammar::new(abstract_, concretes);
/// ```
pub struct GFGrammar {
    /// The abstract grammar (only one per GFGrammar).
    pub abstract_grammar: GFAbstract,

    /// Map of language codes to concrete grammars.
    pub concretes: HashMap<String, GFConcrete>,
}

impl GFGrammar {
    /// Creates a new GFGrammar from an abstract and concretes.
    pub fn new(
        abstract_: GFAbstract,
        concretes: HashMap<String, GFConcrete>,
    ) -> Self {
        GFGrammar { abstract_grammar: abstract_, concretes }
    }

    /// Converts from PGF JSON format to GFGrammar.
    pub fn from_json(json: PGF) -> Self {
        let cncs: HashMap<String, GFConcrete> = json
            .concretes
            .into_iter()
            .map(|(key, concrete)| (key, GFConcrete::from_json(concrete)))
            .collect();
        GFGrammar {
            abstract_grammar: GFAbstract::from_json(json.abstract_),
            concretes: cncs,
        }
    }

    /// Translates input from one language to another (or all if unspecified).
    ///
    /// Parses the input in the source language(s) and linearizes to target language(s).
    ///
    /// # Arguments
    /// * `input` - The string to translate.
    /// * `from_lang` - Optional source language code.
    /// * `to_lang` - Optional target language code.
    ///
    /// # Returns
    /// Vector of translation maps (source -> tree -> target -> output).
    pub fn translate(
        &self,
        input: &str,
        from_lang: Option<&str>,
        to_lang: Option<&str>,
    ) -> Vec<HMS3> {
        let mut outputs: Vec<HMS3> = Vec::new();

        let from_cncs = if let Some(lang) = from_lang {
            let mut map = HashMap::new();
            if let Some(concrete) = self.concretes.get(lang) {
                map.insert(lang.to_string(), concrete.clone());
            }
            map
        } else {
            self.concretes.clone()
        };

        let to_cncs = if let Some(lang) = to_lang {
            let mut map = HashMap::new();
            if let Some(concrete) = self.concretes.get(lang) {
                map.insert(lang.to_string(), concrete.clone());
            }
            map
        } else {
            self.concretes.clone()
        };

        for (_c1, concrete) in &from_cncs {
            let trees = concrete.parse_string(input, &self.abstract_grammar.startcat);
            if !trees.is_empty() {
                let mut c1_outputs: HashMap<String, HashMap<String, String>> = HashMap::new();
                for tree in trees {
                    let mut translations: HashMap<String, String> = HashMap::new();
                    for (c2, to_concrete) in &to_cncs {
                        translations.insert(c2.clone(), to_concrete.linearize(&tree));
                    }
                    c1_outputs.insert(tree.name, translations);
                }
                outputs.push(c1_outputs);
            }
        }

        outputs
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Abstract grammar defining logical structure via functions, categories, and types.
///
/// # Example
///
/// ```gf
/// abstract Lang = {
///   cat Sentence;
///   fun MakeSentence : Noun -> Verb -> Sentence;
/// }
/// ```
#[derive(Debug, Clone)]
pub struct GFAbstract {
    /// Starting category for parsing/linearization.
    pub startcat: String,
    /// Map of function names to their types.
    types: HashMap<String, Type>,
}

impl GFAbstract {
    /// Creates a new abstract grammar.
    pub fn new(startcat: String, types: HashMap<String, Type>) -> Self {
        GFAbstract { startcat, types }
    }

    /// Converts from JSON abstract to GFAbstract.
    pub fn from_json(json: Abstract) -> Self {
        let types = json
            .funs
            .into_iter()
            .map(|(key, fun)| (key, Type::new(fun.args, fun.cat)))
            .collect();

        GFAbstract { startcat: json.startcat, types }
    }

    /// Adds a new function type to the abstract.
    pub fn add_type(&mut self, fun: String, args: Vec<String>, cat: String) {
        self.types.insert(fun, Type::new(args, cat));
    }

    /// Gets arguments for a function.
    pub fn get_args(&self, fun: &str) -> Option<&Vec<String>> {
        self.types.get(fun).map(|t| &t.args)
    }

    /// Gets category for a function.
    pub fn get_cat(&self, fun: &str) -> Option<&String> {
        self.types.get(fun).map(|t| &t.cat)
    }

    /// Annotates meta variables in a tree with types.
    fn annotate(&self, mut tree: Fun, r#type: Option<&String>) -> Fun {
        if tree.is_meta() {
            tree.type_ = r#type.cloned();
        } else if let Some(typ) = self.types.get(&tree.name) {
            for (arg, expected_type) in tree.args.iter_mut().zip(&typ.args) {
                *arg = self.annotate(arg.clone(), Some(expected_type));
            }
        }
        tree
    }

    /// Handles literal wrapping for strings, ints, floats.
    pub fn handle_literals(&self, mut tree: Fun, r#type: &str) -> Fun {
        if tree.name != "?" {
            if r#type == "String" || r#type == "Int" || r#type == "Float" {
                tree.name = format!("{}_Literal_{}", r#type, tree.name);
            } else if let Some(typ) = self.types.get(&tree.name) {
                for (arg, expected_type) in tree.args.iter_mut().zip(&typ.args) {
                    *arg = self.handle_literals(arg.clone(), expected_type);
                }
            }
        }
        tree
    }

    /// Deep copies a tree.
    pub fn copy_tree(&self, x: &Fun) -> Fun {
        let mut tree = Fun::new(x.name.clone(), vec![]);
        tree.type_ = x.type_.clone();
        for arg in &x.args {
            tree.args.push(self.copy_tree(arg));
        }
        tree
    }

    /// Parses a string into a tree.
    pub fn parse_tree(
        &self,
        str: &str,
        r#type: Option<&String>,
    ) -> Option<Fun> {
        let tokens: Vec<&str> = str
            .split_whitespace()
            .flat_map(|s| s.split(|c| "()?:".contains(c)))
            .filter(|s| !s.is_empty())
            .collect();

        if let Some(tree) = self.parse_tree_internal(&mut tokens.into_iter(), 0) {
            Some(self.annotate(tree, r#type))
        } else {
            None
        }
    }

    /// Internal recursive parser for trees.
    fn parse_tree_internal<'a, I>(
        &self,
        tokens: &mut I,
        _prec: usize,
    ) -> Option<Fun>
    where
        I: Iterator<Item = &'a str>,
    {
        if let Some(t) = tokens.next() {
            if t == "(" {
                let tree = self.parse_tree_internal(tokens, 0)?;
                if tokens.next() != Some(")") {
                    return None; // Mismatched parenthesis
                }
                Some(tree)
            } else if t == "?" {
                Some(Fun::new("?".to_string(), vec![]))
            } else {
                let mut tree = Fun::new(t.to_string(), vec![]);
                // Only parse arguments if precedence is 0 (top level)
                if _prec == 0 {
                    while let Some(arg) = self.parse_tree_internal(tokens, 1) {
                        tree.args.push(arg);
                    }
                }
                Some(tree)
            }
        } else {
            None
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/* ... rest of the code ... */

/// Concrete syntax for linearizing abstract trees.
///
/// Defines realizations (linearizations) of the abstract grammar, which can be
/// natural language, images, etc.
///
/// # Example
///
/// ```gf
/// concrete LangEng of Lang = {
///   lincat Sentence = Str;
///   lin MakeSentence n v = n ++ v;
/// }
/// ```
#[derive(Clone, Debug)]
pub struct GFConcrete {
    /// Flags for concrete-specific settings.
    pub flags: HashMap<String, String>,
    /// Concrete functions.
    functions: Vec<RuntimeCncFun>,
    /// Start category ranges.
    pub start_cats: HashMap<String, (i32, i32)>,
    /// Total number of FIds.
    pub total_fids: i32,
    /// Parameter productions.
    pub pproductions: HashMap<i32, Vec<Production>>,
    /// Label productions.
    lproductions: HashMap<String, Vec<LProduction>>,
}

impl GFConcrete {
    /// Creates a new concrete grammar.
    pub fn new(
        flags: HashMap<String, String>,
        functions: Vec<RuntimeCncFun>,
        productions: HashMap<i32, Vec<Production>>,
        start_cats: HashMap<String, (i32, i32)>,
        total_fids: i32,
    ) -> Self {
        let mut lproductions = HashMap::new();

        fn register_recursive(
            args: &[PArg], 
            key: String, 
            i: usize, 
            lproductions: &mut HashMap<String, Vec<LProduction>>, 
            productions: &HashMap<i32, Vec<Production>>,
            fun: &RuntimeCncFun,
            fid: FId,
            depth: usize
        ) {
            if depth > 100 { // Prevent stack overflow
                return;
            }
            if i < args.len() {
                let arg = args[i].fid;
                let mut count = 0;

                if let Some(rules) = productions.get(&arg) {
                    for rule in rules {
                        if let Production::Coerce(ref coerce_rule) = rule {
                            let new_key = format!("{}_{}", key, coerce_rule.arg);
                            register_recursive(args, new_key, i + 1, lproductions, productions, fun, fid, depth + 1);
                            count += 1;
                        }
                    }
                }

                if count == 0 {
                    let new_key = format!("{}_{}", key, arg);
                    register_recursive(args, new_key, i + 1, lproductions, productions, fun, fid, depth + 1);
                }
            } else {
                lproductions
                    .entry(key)
                    .or_insert_with(Vec::new)
                    .push(LProduction {
                        fun: fun.clone(),
                        fid,
                    });
            }
        }

        for (&fid, rules) in &productions {
            for rule in rules {
                if let Production::Apply(ref apply_rule) = rule {
                    match &apply_rule.fun {
                        ApplyFun::FId(fun_id) => {
                            // For FId, we need to find the corresponding function name
                            // This is a simplified approach - in real usage we'd need better mapping
                            if (*fun_id as usize) < functions.len() {
                                let fun = &functions[*fun_id as usize];
                                register_recursive(&apply_rule.args, fun.name.clone(), 0, &mut lproductions, &productions, fun, fid, 0);
                            }
                        }
                        ApplyFun::CncFun(json_fun) => {
                            // Create a temporary runtime function for registration
                            let runtime_fun = RuntimeCncFun::new(json_fun.name.clone(), LinType::FId(json_fun.lins.clone()));
                            register_recursive(&apply_rule.args, runtime_fun.name.clone(), 0, &mut lproductions, &productions, &runtime_fun, fid, 0);
                        }
                    }
                }
            }
        }

        GFConcrete {
            flags,
            pproductions: productions,
            functions,
            start_cats,
            total_fids,
            lproductions,
        }
    }

    /// Converts from JSON concrete to GFConcrete.
    pub fn from_json(json: Concrete) -> Self {
        let productions: HashMap<i32, Vec<Production>> = json
            .productions
            .into_iter()
            .map(|(k, v)| {
                (k, v) // Use the productions directly since they're already the right type
            })
            .collect();

        let sequences: Vec<Vec<Sym>> = json.sequences;

        let functions: Vec<RuntimeCncFun> = json
            .functions
            .into_iter()
            .map(|f| {
                // Convert Vec<i32> indices to actual symbol sequences
                let lins = if f.lins.is_empty() {
                    LinType::Sym(vec![])
                } else {
                    let symbol_sequences: Vec<Vec<Sym>> = f.lins
                        .into_iter()
                        .map(|seq_idx| {
                            if seq_idx as usize >= sequences.len() {
                                vec![]
                            } else {
                                sequences[seq_idx as usize].clone()
                            }
                        })
                        .collect();
                    LinType::Sym(symbol_sequences)
                };
                
                RuntimeCncFun {
                    name: f.name,
                    lins,
                }
            })
            .collect();

        let start_cats = json
            .categories
            .into_iter()
            .map(|(key, cat)| (key, (cat.start, cat.end)))
            .collect();

        GFConcrete::new(
            json.flags,
            functions,
            productions,
            start_cats,
            json.totalfids,
        )
    }

    /// Linearizes a tree into symbols with tags.
    pub fn linearize_syms(&self, tree: &Fun, tag: &str) -> Vec<LinearizedSym> {
        let mut res = Vec::new();

        if tree.is_string() {
            let mut sym = SymKS::new(vec![tree.name.clone()]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym { fid: -1, table: vec![vec![Sym::SymKS(sym)]] });
        } else if tree.is_int() {
            let mut sym = SymKS::new(vec![tree.name.clone()]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym { fid: -2, table: vec![vec![Sym::SymKS(sym)]] });
        } else if tree.is_float() {
            let mut sym = SymKS::new(vec![tree.name.clone()]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym { fid: -3, table: vec![vec![Sym::SymKS(sym)]] });
        } else if tree.is_meta() {
            let cat = self.start_cats.get(tree.type_.as_ref().unwrap_or(&String::new())).cloned().unwrap_or((0, 0));
            let sym = Sym::SymKS(SymKS {
                id: "KS".to_string(),
                tokens: vec![tree.name.clone()],
                tag: Some(tag.to_string()),
            });

            for fid in cat.0..=cat.1 {
                res.push(LinearizedSym {
                    fid,
                    table: vec![vec![sym.clone()]],
                });
            }
        } else {
            let cs: Vec<LinearizedSym> = tree.args.iter().enumerate().map(|(i, arg)| {
                self.linearize_syms(arg, &format!("{}-{}", tag, i))[0].clone()
            }).collect();

            let mut key = tree.name.clone();
            for c in &cs {
                if c.fid == -5 {
                    if let Some((matched_key, _)) = self.lproductions.iter().find(|(k, _)| k.contains(&tree.name)) {
                        key = matched_key.clone();
                    }
                    break;
                } else {
                    key = format!("{}_{}", key, c.fid);
                }
            }

            if let Some(rules) = self.lproductions.get(&key) {
                for rule in rules {
                    let mut row = LinearizedSym {
                        fid: rule.fid,
                        table: Vec::new(),
                    };

                    match &rule.fun.lins {
                        LinType::Sym(lins) => {
                            for (j, lin) in lins.iter().enumerate() {
                                let mut toks: Vec<Sym> = Vec::new();
                                if j >= row.table.len() {
                                    row.table.push(Vec::new());
                                }

                                for sym0 in lin {
                                    match sym0 {
                                        Sym::SymCat { i, .. } | Sym::SymLit { i, .. } => {
                                            if *i < cs.len() && j < cs[*i].table.len() {
                                                let ts = &cs[*i].table[j];
                                                toks.extend_from_slice(ts);
                                            }
                                        }
                                        Sym::SymKS(ks) => {
                                            toks.push(Sym::SymKS(ks.tag_with(tag)));
                                        }
                                        Sym::SymKP(kp) => {
                                            toks.push(Sym::SymKP(kp.tag_with(tag)));
                                        }
                                    }
                                }

                                row.table[j] = toks;
                            }
                        }
                        LinType::FId(_) => {
                            // Handle FId case - create empty table
                            row.table.push(Vec::new());
                        }
                    }

                    res.push(row);
                }
            }
        }

        res
    }

    /// Converts symbols to tagged tokens.
    pub fn syms2toks(&self, syms: &[Sym]) -> Vec<TaggedString> {
        let mut ts = Vec::new();

        for i in 0..syms.len() {
            match &syms[i] {
                Sym::SymKS(sym) => {
                    if let Some(tag) = &sym.tag {
                        for token in &sym.tokens {
                            ts.push(TaggedString::new(token, tag));
                        }
                    }
                }
                Sym::SymKP(sym) => {
                    let mut added_alt = false;

                    if i + 1 < syms.len() {
                        if let Sym::SymKS(next_sym) = &syms[i + 1] {
                            if let Some(next_token) = next_sym.tokens.first() {
                                for alt in &sym.alts {
                                    if alt.prefixes.iter().any(|p| next_token.starts_with(p)) {
                                        for symks in &alt.tokens {
                                            if let Some(tag) = &sym.tag {
                                                for token in &symks.tokens {
                                                    ts.push(TaggedString::new(token, tag));
                                                }
                                            }
                                        }
                                        added_alt = true;
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    if !added_alt {
                        if let Some(tag) = &sym.tag {
                            for symks in &sym.tokens {
                                for token in &symks.tokens {
                                    ts.push(TaggedString::new(token, tag));
                                }
                            }
                        }
                    }
                }
                _ => {}, // Ignore non-token symbols
            }
        }

        ts
    }

    /// Linearizes a tree to all possible strings.
    pub fn linearize_all(&self, tree: &Fun) -> Vec<String> {
        self.linearize_syms(tree, "0")
            .into_iter()
            .map(|r| self.unlex(&self.syms2toks(&r.table[0])))
            .collect()
    }

    /// Linearizes a tree to a single string (first variant).
    pub fn linearize(&self, tree: &Fun) -> String {
        let res = self.linearize_syms(tree, "0");
        if !res.is_empty() {
            self.unlex(&self.syms2toks(&res[0].table[0]))
        } else {
            String::new()
        }
    }

    /// Linearizes a tree with tags.
    pub fn tag_and_linearize(&self, tree: &Fun) -> Vec<TaggedString> {
        let res = self.linearize_syms(tree, "0");
        if !res.is_empty() {
            self.syms2toks(&res[0].table[0])
        } else {
            Vec::new()
        }
    }

    /// Joins tagged strings into a single string, handling spacing.
    fn unlex(&self, ts: &[TaggedString]) -> String {
        if ts.is_empty() {
            return String::new();
        }

        let no_space_after = Regex::new(r"^[\(\-\[]").unwrap();
        let no_space_before = Regex::new(r"^[\.\,\?\!\)\:\;\-\]]").unwrap();

        let mut s = String::new();

        for i in 0..ts.len() {
            let t = &ts[i].token;
            s.push_str(t);

            if i + 1 < ts.len() {
                let after = &ts[i + 1].token;
                if !no_space_after.is_match(t) && !no_space_before.is_match(after) {
                    s.push(' ');
                }
            }
        }

        s
    }

    /// Tokenizes input string by whitespace.
    fn tokenize(&self, input: &str) -> Vec<String> {
        input
            .trim()
            .split_whitespace()
            .map(String::from)
            .collect()
    }

    /// Parses a string into trees using the given start category.
    pub fn parse_string(&self, input: &str, cat: &str) -> Vec<Fun> {
        let tokens = self.tokenize(input);

        let mut ps = ParseState::new(self.clone(), cat.to_string());
        for token in tokens {
            if !ps.next(&token) {
                return Vec::new();
            }
        }

        ps.extract_trees()
    }

    /// Provides completions for partial input.
    pub fn complete(&self, input: &str, cat: &str) -> CompletionResult {
        let mut tokens: Vec<String> = input
            .trim()
            .split_whitespace()
            .filter(|t| !t.is_empty())
            .map(|t| t.to_string())
            .collect();

        let current = tokens.pop().unwrap_or_default();

        let mut ps = ParseState::new(self.clone(), cat.to_string());
        let mut ps2 = ParseState::new(self.clone(), cat.to_string());

        for token in &tokens {
            if !ps.next(token) {
                return CompletionResult {
                    consumed: vec![],
                    suggestions: vec![],
                };
            }
            ps2.next(token);
        }

        let mut current = current;
        if ps2.next(&current) {
            ps.next(&current);
            tokens.push(current.clone());
            current = String::new();
        }

        let acc = ps.complete(&current);
        let mut suggestions = Vec::new();

        if let Some(items) = acc.value {
            for a in items {
                for s in &a.seq {
                    match s {
                        Sym::SymKS(sym) => {
                            for t in &sym.tokens {
                                suggestions.push(t.clone());
                            }
                        }
                        Sym::SymKP(sym) => {
                            for symks in &sym.tokens {
                                for t in &symks.tokens {
                                    suggestions.push(t.clone());
                                }
                            }
                        }
                        _ => {},
                    }
                }
            }
        }

        CompletionResult { consumed: tokens, suggestions }
    }
}


////////////////////////////////////////////////////////////////////////////////
/// Prefix tree for parsing items.
#[derive(Debug, Clone)]
pub struct Trie<T> {
    /// Value at node.
    value: Option<Vec<T>>,
    /// Child nodes.
    items: HashMap<String, Trie<T>>,
}

impl<T: Clone> Trie<T> {
    /// Creates a new trie.
    pub fn new() -> Self {
        Trie { value: None, items: HashMap::new() }
    }

    /// Inserts a chain of keys with value.
    pub fn insert_chain(&mut self, keys: &[String], obj: Vec<T>) {
        let mut node = self;
        for key in keys {
            node = node.items.entry(key.clone()).or_insert_with(Trie::new);
        }
        node.value = Some(obj);
    }

    /// Inserts a chain with single item.
    pub fn insert_chain1(&mut self, keys: &[String], obj: T) {
        let mut node = self;
        for key in keys {
            node = node.items.entry(key.clone()).or_insert_with(Trie::new);
        }
        if let Some(value) = &mut node.value {
            value.push(obj);
        } else {
            node.value = Some(vec![obj]);
        }
    }

    /// Looks up a key.
    pub fn lookup(&self, key: &str) -> Option<&Trie<T>> {
        self.items.get(key)
    }

    /// Checks if trie is empty.
    pub fn is_empty(&self) -> bool {
        self.value.is_none() && self.items.is_empty()
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Chart for parsing.
#[derive(Debug, Clone)]
pub struct Chart {
    /// Active items by FId and label.
    active: HashMap<FId, HashMap<i32, Vec<ActiveItem>>>,
    /// Active items by offset.
    actives: Vec<HashMap<FId, HashMap<i32, Vec<ActiveItem>>>>,
    /// Passive FIds by key.
    passive: HashMap<String, FId>,
    /// Forest of productions.
    pub forest: HashMap<FId, Vec<Production>>,
    /// Next ID.
    pub next_id: FId,
    /// Current offset.
    pub offset: usize,
}

impl Chart {
    /// Creates a new chart from concrete.
    pub fn new(concrete: GFConcrete) -> Self {
        let mut forest = HashMap::new();
        for (&fid, prods) in &concrete.pproductions {
            forest.insert(fid, prods.clone());
        }
        Chart {
            active: HashMap::new(),
            actives: vec![],
            passive: HashMap::new(),
            forest,
            next_id: concrete.total_fids,
            offset: 0,
        }
    }

    /// Looks up active items.
    pub fn lookup_ac(&self, fid: FId, label: i32) -> Option<&Vec<ActiveItem>> {
        self.active.get(&fid).and_then(|m| m.get(&label))
    }

    /// Looks up active items by offset.
    pub fn lookup_aco(&self, offset: usize, fid: FId, label: i32) -> Option<&Vec<ActiveItem>> {
        if offset == self.offset {
            self.lookup_ac(fid, label)
        } else {
            self.actives.get(offset)?.get(&fid).and_then(|m| m.get(&label))
        }
    }

    /// Gets labels for active FId.
    pub fn labels_ac(&self, fid: FId) -> Option<Vec<i32>> {
        self.active.get(&fid).map(|m| m.keys().cloned().collect())
    }

    /// Inserts active items.
    pub fn insert_ac(&mut self, fid: FId, label: i32, items: Vec<ActiveItem>) {
        self.active.entry(fid).or_insert_with(HashMap::new).insert(label, items);
    }

    /// Looks up passive FId.
    pub fn lookup_pc(&self, fid: FId, label: i32, offset: usize) -> Option<FId> {
        let key = format!("{}.{}-{}", fid, label, offset);
        self.passive.get(&key).cloned()
    }

    /// Inserts passive FId.
    pub fn insert_pc(&mut self, fid: FId, label: i32, offset: usize, fid2: FId) {
        let key = format!("{}.{}-{}", fid, label, offset);
        self.passive.insert(key, fid2);
    }

    /// Shifts the chart to next offset.
    pub fn shift(&mut self) {
        self.actives.push(self.active.clone());
        self.active.clear();
        self.passive.clear();
        self.offset += 1;
    }

    /// Expands forest for FId.
    pub fn expand_forest(&self, fid: FId) -> Vec<Production> {
        let mut rules = Vec::new();

        fn go(forest: &HashMap<FId, Vec<Production>>, fid: FId, rules: &mut Vec<Production>) {
            if let Some(prods) = forest.get(&fid) {
                for prod in prods {
                    match prod {
                        Production::Apply(apply) => rules.push(Production::Apply(apply.clone())),
                        Production::Coerce(coerce) => go(forest, coerce.arg, rules),
                        Production::Const(const_) => rules.push(Production::Const(const_.clone())),
                    }
                }
            }
        }

        go(&self.forest, fid, &mut rules);
        rules
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Parsing state.
#[derive(Debug, Clone)]
pub struct ParseState {
    /// Concrete grammar.
    concrete: GFConcrete,
    /// Start category.
    start_cat: String,
    /// Trie of active items.
    items: Trie<ActiveItem>,
    /// Chart.
    chart: Chart,
}

impl ParseState {
    /// Processes items with callbacks.
    pub fn process<F, G>(
        &mut self,
        agenda: &mut Vec<ActiveItem>,
        literal_callback: F,
        mut token_callback: G,
    ) where
        F: Fn(FId) -> Option<Const>,
        G: FnMut(&[String], ActiveItem),
    {
        while let Some(item) = agenda.pop() {
            if item.dot < item.seq.len() {
                let sym = &item.seq[item.dot];
                match sym {
                    Sym::SymCat { i, label } => {
                        let fid = item.args[*i].fid;
                        if let Some(items) = self.chart.lookup_ac(fid, *label as i32) {
                            if !items.contains(&item) {
                                let mut items = items.clone();
                                items.push(item.clone());
                                self.chart.insert_ac(fid, *label as i32, items);
                                if let Some(fid2) = self.chart.lookup_pc(fid, *label as i32, self.chart.offset) {
                                    agenda.push(item.shift_over_arg(*i, fid2));
                                }
                            }
                        } else {
                            let rules = self.chart.expand_forest(fid);
                            for rule in rules {
                                if let Production::Apply(apply) = rule {
                                    let runtime_fun = match &apply.fun {
                                        ApplyFun::CncFun(fun) => {
                                            // Convert from JSON CncFun to RuntimeCncFun if needed
                                            // For now, assume it's already converted at construction
                                            RuntimeCncFun::new(fun.name.clone(), LinType::FId(fun.lins.clone()))
                                        }
                                        ApplyFun::FId(id) => {
                                            // Create a placeholder RuntimeCncFun for FId
                                            RuntimeCncFun::new(id.to_string(), LinType::FId(vec![]))
                                        }
                                    };
                                    
                                    let seq = match &runtime_fun.lins {
                                        LinType::Sym(syms) => {
                                            if (*label as usize) < syms.len() {
                                                syms[*label as usize].clone()
                                            } else {
                                                vec![]
                                            }
                                        }
                                        LinType::FId(_) => vec![],
                                    };
                                    
                                    agenda.push(ActiveItem::new(
                                        self.chart.offset,
                                        0,
                                        runtime_fun,
                                        seq,
                                        apply.args,
                                        fid,
                                        *label as i32,
                                    ));
                                }
                            }
                        }
                    }
                    Sym::SymKS(sym) => token_callback(&sym.tokens, item.shift_over_token()),
                    Sym::SymKP(sym) => {
                        let pitem = item.shift_over_token();
                        for symks in &sym.tokens {
                            token_callback(&symks.tokens, pitem.clone());
                        }
                        for alt in &sym.alts {
                            for symks in &alt.tokens {
                                token_callback(&symks.tokens, pitem.clone());
                            }
                        }
                    }
                    Sym::SymLit { i, .. } => {
                        let fid = item.args[*i].fid;
                        if let Some(rules) = self.chart.forest.get(&fid) {
                            if let Some(Production::Const(const_rule)) = rules.get(0) {
                                token_callback(&const_rule.toks, item.shift_over_token());
                            }
                        } else if let Some(rule) = literal_callback(fid) {
                            let new_fid = self.chart.next_id;
                            self.chart.next_id += 1;
                            self.chart.forest.insert(new_fid, vec![Production::Const(rule.clone())]);
                            token_callback(&rule.toks, item.shift_over_arg(*i, new_fid));
                        }
                    }
                }
            } else {
                let fid = self.chart.lookup_pc(item.fid, item.lbl, item.offset).unwrap_or_else(|| {
                    let new_fid = self.chart.next_id;
                    self.chart.next_id += 1;
                    self.chart.insert_pc(item.fid, item.lbl, item.offset, new_fid);
                    self.chart.forest.insert(new_fid, vec![Production::Apply(Apply::new(ApplyFun::CncFun(CncFun::new(item.fun.name.clone(), vec![])), item.args.clone()))]);
                    new_fid
                });

                if let Some(labels) = self.chart.labels_ac(fid) {
                    for lbl in labels {
                        let seq = match &item.fun.lins {
                            LinType::Sym(syms) => {
                                if (lbl as usize) < syms.len() {
                                    syms[lbl as usize].clone()
                                } else {
                                    vec![]
                                }
                            }
                            LinType::FId(_) => vec![],
                        };
                        agenda.push(ActiveItem::new(
                            self.chart.offset,
                            0,
                            item.fun.clone(),
                            seq,
                            item.args.clone(),
                            fid,
                            lbl,
                        ));
                    }
                }
            }
        }
    }

    /// Creates a new parse state.
    pub fn new(concrete: GFConcrete, start_cat: String) -> Self {
        let mut items = Trie::new();
        let chart = Chart::new(concrete.clone());

        let mut active_items = Vec::new();

        if let Some((start, end)) = concrete.start_cats.get(&start_cat) {
            for fid in *start..=*end {
                let rules = chart.expand_forest(fid);
                for rule in rules {
                    if let Production::Apply(apply) = rule {
                        match &apply.fun {
                            ApplyFun::CncFun(json_fun) => {
                                // Convert JSON CncFun to RuntimeCncFun
                                let runtime_fun = RuntimeCncFun::new(
                                    json_fun.name.clone(),
                                    LinType::FId(json_fun.lins.clone())
                                );
                                
                                // Create active items for each linearization
                                active_items.push(ActiveItem::new(
                                    0,
                                    0,
                                    runtime_fun,
                                    vec![], // Will be filled based on actual lintype
                                    apply.args.clone(),
                                    fid,
                                    0,
                                ));
                            }
                            ApplyFun::FId(fun_id) => {
                                if (*fun_id as usize) < concrete.functions.len() {
                                    let runtime_fun = concrete.functions[*fun_id as usize].clone();
                                    match &runtime_fun.lins {
                                        LinType::Sym(lins) => {
                                            for (lbl, lin) in lins.iter().enumerate() {
                                                active_items.push(ActiveItem::new(
                                                    0,
                                                    0,
                                                    runtime_fun.clone(),
                                                    lin.clone(),
                                                    apply.args.clone(),
                                                    fid,
                                                    lbl as i32,
                                                ));
                                            }
                                        }
                                        LinType::FId(_) => {
                                            active_items.push(ActiveItem::new(
                                                0,
                                                0,
                                                runtime_fun,
                                                vec![],
                                                apply.args.clone(),
                                                fid,
                                                0,
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        items.insert_chain(&[], active_items);

        ParseState { concrete, start_cat, items, chart }
    }

    /// Advances parsing with next token.
    pub fn next(&mut self, token: &str) -> bool {
        let mut acc = self.items.lookup(token).cloned().unwrap_or_else(Trie::new);

        let mut agenda = self.items.value.clone().unwrap_or_default();

        self.process(
            &mut agenda,
            |fid| {
                match fid {
                    -1 => Some(Const::new(Fun::new(format!("\"{}\"", token), vec![]), vec![token.to_string()])),
                    -2 if token.parse::<i32>().is_ok() => Some(Const::new(Fun::new(token.to_string(), vec![]), vec![token.to_string()])),
                    -3 if token.parse::<f64>().is_ok() => Some(Const::new(Fun::new(token.to_string(), vec![]), vec![token.to_string()])),
                    _ => None,
                }
            },
            |tokens, item| {
                if tokens.first() == Some(&token.to_string()) {
                    let tokens1 = tokens[1..].to_vec();
                    acc.insert_chain1(&tokens1, item);
                }
            },
        );

        self.items = acc;
        self.chart.shift();

        !self.items.is_empty()
    }

    /// Gets completions for partial token.
    pub fn complete(&self, current_token: &str) -> CompletionAccumulator {
        let mut acc = self.items.lookup(current_token).cloned().unwrap_or_else(Trie::new);

        let mut agenda = self.items.value.clone().unwrap_or_default();

        let mut clone_self = self.clone();
        clone_self.process(
            &mut agenda,
            |_| None,
            |tokens, item| {
                if current_token.is_empty() || tokens.first().map_or(false, |t| t.starts_with(current_token)) {
                    let tokens1 = tokens[1..].to_vec();
                    acc.insert_chain1(&tokens1, item);
                }
            },
        );

        CompletionAccumulator { value: acc.value }
    }

    /// Extracts parsed trees.
    pub fn extract_trees(&self) -> Vec<Fun> {
        let total_fids = self.concrete.total_fids;
        let forest = &self.chart.forest;

        fn go(fid: FId, total_fids: FId, forest: &HashMap<FId, Vec<Production>>) -> Vec<Fun> {
            if fid < total_fids {
                vec![Fun::new("?".to_string(), vec![])]
            } else if let Some(rules) = forest.get(&fid) {
                let mut trees = Vec::new();
                for rule in rules {
                    match rule {
                        Production::Const(c) => trees.push(c.lit.clone()),
                        Production::Apply(a) => {
                            let arg_trees: Vec<Vec<Fun>> = a.args.iter().map(|arg| go(arg.fid, total_fids, forest)).collect();
                            let mut indices = vec![0; a.args.len()];
                            loop {
                                let mut t = Fun::new(a.fun.get_name(), vec![]);
                                for (k, idx) in indices.iter().enumerate() {
                                    t.args.push(arg_trees[k][*idx].clone());
                                }
                                trees.push(t);

                                let mut carry = true;
                                for i in 0..indices.len() {
                                    if carry {
                                        indices[i] += 1;
                                        if indices[i] < arg_trees[i].len() {
                                            carry = false;
                                        } else {
                                            indices[i] = 0;
                                        }
                                    }
                                }
                                if carry {
                                    break;
                                }
                            }
                        }
                        _ => {},
                    }
                }
                trees
            } else {
                vec![]
            }
        }

        let mut trees = Vec::new();

        if let Some((start, end)) = self.concrete.start_cats.get(&self.start_cat) {
            for fid0 in *start..=*end {
                let rules = self.chart.expand_forest(fid0);
                let mut labels = vec![];
                for rule in &rules {
                    if let Production::Apply(a) = rule {
                        match &a.fun {
                            ApplyFun::CncFun(fun) => {
                                // JSON CncFun has Vec<i32> lins, each one is an index
                                labels.extend(0..fun.lins.len() as i32);
                            }
                            ApplyFun::FId(fun_id) => {
                                if (*fun_id as usize) < self.concrete.functions.len() {
                                    let runtime_fun = &self.concrete.functions[*fun_id as usize];
                                    match &runtime_fun.lins {
                                        LinType::Sym(lins) => {
                                            labels.extend(0..lins.len() as i32);
                                        }
                                        LinType::FId(indices) => {
                                            labels.extend(0..indices.len() as i32);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                for lbl in labels {
                    if let Some(fid) = self.chart.lookup_pc(fid0, lbl, 0) {
                        let arg_trees = go(fid, total_fids, forest);
                        for tree in arg_trees {
                            if !trees.contains(&tree) {
                                trees.push(tree);
                            }
                        }
                    }
                }
            }
        }

        trees
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Runtime concrete function with actual symbol sequences for linearization.
#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeCncFun {
    /// Function name.
    pub name: String,
    /// Linearization sequences (converted from indices to actual symbols).
    pub lins: LinType,
}

impl RuntimeCncFun {
    /// Creates a new runtime concrete function.
    pub fn new(name: String, lins: LinType) -> Self {
        RuntimeCncFun { name, lins }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Label production.
#[derive(Debug, Clone)]
struct LProduction {
    fid: FId,
    fun: RuntimeCncFun,
}

////////////////////////////////////////////////////////////////////////////////
/// Tagged string for linearization.
#[derive(Debug, Clone)]
pub struct TaggedString {
    /// Token.
    pub token: String,
    /// Tag.
    pub tag: String,
}

impl TaggedString {
    /// Creates a new tagged string.
    pub fn new(token: &str, tag: &str) -> Self {
        TaggedString { token: token.to_string(), tag: tag.to_string() }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Linearized symbol row.
#[derive(Debug, Clone)]
pub struct LinearizedSym {
    /// FId.
    pub fid: i32,
    /// Table of symbols.
    pub table: Vec<Vec<Sym>>,
}

////////////////////////////////////////////////////////////////////////////////
/// Active item for parsing.
#[derive(Debug, Clone, PartialEq)]
pub struct ActiveItem {
    /// Offset.
    pub offset: usize,
    /// Dot position.
    pub dot: usize,
    /// Function.
    pub fun: RuntimeCncFun,
    /// Sequence.
    pub seq: Vec<Sym>,
    /// Arguments.
    pub args: Vec<PArg>,
    /// FId.
    pub fid: FId,
    /// Label.
    pub lbl: i32,
}

impl ActiveItem {
    /// Creates a new active item.
    pub fn new(
        offset: usize,
        dot: usize,
        fun: RuntimeCncFun,
        seq: Vec<Sym>,
        args: Vec<PArg>,
        fid: FId,
        lbl: i32,
    ) -> Self {
        ActiveItem { offset, dot, fun, seq, args, fid, lbl }
    }

    /// Checks equality.
    pub fn is_equal(&self, other: &ActiveItem) -> bool {
        self.offset == other.offset
            && self.dot == other.dot
            && self.fun.name == other.fun.name
            && self.seq == other.seq
            && self.args == other.args
            && self.fid == other.fid
            && self.lbl == other.lbl
    }

    /// Shifts over argument.
    pub fn shift_over_arg(&self, i: usize, fid: FId) -> ActiveItem {
        let mut args = self.args.clone();
        args[i].fid = fid;
        ActiveItem {
            offset: self.offset,
            dot: self.dot + 1,
            fun: self.fun.clone(),
            seq: self.seq.clone(),
            args,
            fid: self.fid,
            lbl: self.lbl,
        }
    }

    /// Shifts over token.
    pub fn shift_over_token(&self) -> ActiveItem {
        ActiveItem {
            offset: self.offset,
            dot: self.dot + 1,
            fun: self.fun.clone(),
            seq: self.seq.clone(),
            args: self.args.clone(),
            fid: self.fid,
            lbl: self.lbl,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Utility to check if value is undefined.
pub fn is_undefined<T>(value: &Option<T>) -> bool {
    value.is_none()
}

////////////////////////////////////////////////////////////////////////////////
/// Maps a hashmap with a function.
pub fn map_object<K: Eq + std::hash::Hash + Clone, V, F: Fn(&V) -> U, U>(obj: &HashMap<K, V>, fun: F) -> HashMap<K, U> {
    obj.iter().map(|(k, v)| (k.clone(), fun(v))).collect()
}

impl Type {
    /// Creates a new type.
    pub fn new(args: Vec<String>, cat: String) -> Self {
        Type { args, cat }
    }
}


impl Apply {
    /// Shows the apply rule as string.
    pub fn show(&self, cat: &str) -> String {
        format!("{} -> {} [{:?}]", cat, self.fun.get_name(), self.args)
    }

    /// Checks equality with another apply.
    pub fn is_equal(&self, obj: &Apply) -> bool {
        self.id == obj.id && self.fun == obj.fun && self.args == obj.args
    }
}


impl Coerce {
    /// Shows the coerce rule as string.
    pub fn show(&self, cat: &str) -> String {
        format!("{} -> _ [{}]", cat, self.arg)
    }
}

impl PArg {
    /// Creates a new PArg.
    pub fn new(type_: String, hypos: Vec<FId>, fid: FId) -> Self {
        PArg { type_, hypos, fid }
    }
}

impl Const {
    /// Shows the const rule as string.
    pub fn show(&self, cat: &str) -> String {
        format!("{} -> {}", cat, self.lit.print())
    }

    /// Checks equality with another const.
    pub fn is_equal(&self, obj: &Const) -> bool {
        self.id == obj.id && self.lit.is_equal(&obj.lit) && self.toks == obj.toks
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    /// Test creating a GFGrammar.
    #[test]
    fn test_gfgrammar_new() {
        let abstract_ = GFAbstract::new("S".to_string(), HashMap::new());
        let concretes = HashMap::new();
        let grammar = GFGrammar::new(abstract_, concretes);
        assert_eq!(grammar.abstract_grammar.startcat, "S");
    }

    /// Test adding type to abstract.
    #[test]
    fn test_gfabstract_add_type() {
        let mut abstract_ = GFAbstract::new("S".to_string(), HashMap::new());
        abstract_.add_type("MakeS".to_string(), vec!["NP".to_string(), "VP".to_string()], "S".to_string());
        assert_eq!(abstract_.get_cat("MakeS"), Some(&"S".to_string()));
        assert_eq!(abstract_.get_args("MakeS"), Some(&vec!["NP".to_string(), "VP".to_string()]));
    }

    /// Test tree parsing.
    #[test]
    fn test_parse_tree() {
        let abstract_ = GFAbstract::new("S".to_string(), HashMap::new());
        let tree = abstract_.parse_tree("MakeS (NP) (VP)", None);
        assert!(tree.is_some());
        let tree = tree.unwrap();
        assert_eq!(tree.name, "MakeS");
        assert_eq!(tree.args.len(), 2);
    }

    /// Test literal handling.
    #[test]
    fn test_handle_literals() {
        let abstract_ = GFAbstract::new("S".to_string(), HashMap::new());
        let tree = Fun::new("\"test\"".to_string(), vec![]);
        let handled = abstract_.handle_literals(tree, "String");
        assert_eq!(handled.name, "String_Literal_\"test\"");
    }

    /// Test tree equality.
    #[test]
    fn test_tree_equality() {
        let tree1 = Fun::new("Test".to_string(), vec![Fun::new("Arg1".to_string(), vec![])]);
        let tree2 = Fun::new("Test".to_string(), vec![Fun::new("Arg1".to_string(), vec![])]);
        assert!(tree1.is_equal(&tree2));
        let tree3 = Fun::new("Test".to_string(), vec![Fun::new("Arg2".to_string(), vec![])]);
        assert!(!tree1.is_equal(&tree3));
    }

    // Add more tests as needed.
}