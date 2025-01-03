////////////////////////////////////////////////////////////////////////////////
// Rust Runtime for Grammatical Framework
// Neo: "I know Kung Fu."
// Morpheus: "Show me!"

#![deny(missing_docs)]
#![doc(html_root_url = "https://docs.rs/gf-core/0.1/")]

////////////////////////////////////////////////////////////////////////////////
mod GFAbstract;
mod GFConcrete;
mod ParseState;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

//use regex::Regex;
// use std::hash::Hash;
// use std::pin::Pin;

////////////////////////////////////////////////////////////////////////////////
// pub use crate::glob::{Glob, GlobBuilder, GlobMatcher};
// pub use crate::gf-core::pgf_json::*;
// use crate::pgf_json::*;
// use crate::pgf_json::{Abstract, Sym};
mod pgf_json;
use crate::pgf_json::*;

////////////////////////////////////////////////////////////////////////////////     
/// Constants and type alias
// We create a type alias to avoid extremely long HashMap types.
type HMS3 = HashMap<String, HashMap<String, String>>;
/// Every constituent has an unique id. If the constituent is
/// discontinuous then it will be represented with several brackets
/// where they all will have the same id.
pub type FId = i32;

////////////////////////////////////////////////////////////////////////////////     
////////////////////////////////////////////////////////////////////////////////     
// TODO. Simplify.
pub struct CompletionAccumulator {
    pub value: Option<Vec<ActiveItem>>,
}

pub struct CompletionResult {
    pub consumed: Vec<String>,
    pub suggestions: Vec<String>,
}

////////////////////////////////////////////////////////////////////////////////     
/// A Grammatical Framework grammar is one abstract and multiple concretes.
/// 
/// The GFGrammar type is an umbrella type for the two subtypes GFAbstract and
/// GFConcrete. A GF grammar is made up of an abstract grammar, which define
/// a language in a way that is independent from its manifestation, and one or
/// more _concrete_ grammars, which are manifestations of the abstract grammar.
/// In GF, the manifestation of an abstract grammar is denoted, a linearization.
/// 
// TODO: add documentation.
pub struct GFGrammar {
    // A GFGrammar can only have one single abstract grammar.
    pub abstract_: GFAbstract,
    // A GFGrammar can have one of more concrete grammars.
    pub concretes: HashMap<String, GFConcrete>,
}

impl GFGrammar {
    pub fn new(abstract_: GFAbstract, concretes: HashMap<String, GFConcrete>) -> Self {
        GFGrammar {
            abstract_,
            concretes,
        }
    }

    /// Convert from PGF JSON format into a GFGrammar structure.
    pub fn from_json(json: PGF) -> Self {
        let cncs: HashMap<String, GFConcrete> = json
            .concretes
            .into_iter()
            .map(|(key, concrete)| (key, GFConcrete::from_json(concrete)))
            .collect();
        GFGrammar {
            abstract_: GFAbstract::from_json(json.abstract_),
            concretes: cncs,
        }
    }

    pub fn translate(
        &self,
        input: &str,
        from_lang: Option<&str>,
        to_lang: Option<&str>,
        // ) -> HashMap<String, HashMap<String, Vec<String>>> {
    ) -> Vec<HMS3> {
        let mut outputs: Vec<HMS3> = Vec::new();

        // FROM
        // In an attempt to make the code clearer, concretes relates to self.concretes
        // while cncs are temporary variables.
        let from_cncs = if let Some(lang) = from_lang {
            let mut map = HashMap::new();
            if let Some(concrete) = self.concretes.get(lang) {
                map.insert(lang.to_string(), concrete.clone());
            }
            map
        } else {
            self.concretes.clone()
        };

        // TO
        let to_cncs = if let Some(lang) = to_lang {
            let mut map = HashMap::new();
            if let Some(concrete) = self.concretes.get(lang) {
                map.insert(lang.to_string(), concrete.clone());
            }
            map
        } else {
            self.concretes.clone()
        };

        for (c1, concrete) in &from_cncs {
            let trees = concrete.parse_string(input, &self.abstract_.startcat);
            if !trees.is_empty() {
                let mut c1_outputs: HMS3 = HashMap::new();
                for tree in trees {
                    let mut translations: HashMap<String, String> = HashMap::new();
                    for (c2, to_concrete) in &to_cncs {
                        translations.insert(c2.clone(), to_concrete.linearize(&tree));
                    }
                    c1_outputs.insert(tree.name, translations);
                }
                outputs.push(
                    HashMap::new()
                        .insert(c1.clone(), c1_outputs)
                        .expect("Error unpacking vector"),
                );
            }
        }

        outputs
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Abstract Syntax Tree
///
/// The Fun type is used to represent functions (mappings) between categories.
/// Functions are declared in the _abstract_ grammar and then linearized
/// in each _concrete grammar that implements the concrete grammar.
#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub name: String,
    pub args: Vec<Fun>,
    pub type_: Option<String>, // Optional type, used for meta variables
}

impl Fun {
    pub fn new(name: &str, args: Vec<Fun>) -> Self {
        Fun {
            name: name.to_string(),
            args,
            type_: None,
        }
    }

    // Print the Fun object
    pub fn print(&self) -> String {
        self.show(0)
    }

    // Show the Fun object with precedence
    pub fn show(&self, prec: usize) -> String {
        if self.is_meta() {
            if let Some(ref t) = self.type_ {
                let mut s = format!("?:{}", t);
                if prec > 0 {
                    s = format!("({})", s);
                }
                s
            } else {
                "?".to_string()
            }
        } else {
            let mut s = self.name.clone();
            for arg in &self.args {
                s.push(' ');
                s.push_str(&arg.show(1));
            }
            if prec > 0 && !self.args.is_empty() {
                s = format!("({})", s);
            }
            s
        }
    }

    // Get argument by index
    pub fn get_arg(&self, i: usize) -> Option<&Fun> {
        self.args.get(i)
    }

    // Set argument by index
    pub fn set_arg(&mut self, i: usize, c: Fun) {
        if i < self.args.len() {
            self.args[i] = c;
        }
    }

    // Check if Fun is a meta variable
    pub fn is_meta(&self) -> bool {
        self.name == "?"
    }

    // Check if Fun is complete
    pub fn is_complete(&self) -> bool {
        if self.is_meta() {
            return false;
        }
        for arg in &self.args {
            if !arg.is_complete() {
                return false;
            }
        }
        true
    }

    // Check if Fun is a literal
    pub fn is_literal(&self) -> bool {
        self.name.starts_with('"')
            || self.name.starts_with('-')
            || self.name.chars().next().unwrap_or(' ').is_digit(10)
    }

    // Check if Fun is a string
    pub fn is_string(&self) -> bool {
        self.name.starts_with('"') && self.name.ends_with('"')
    }

    // Check if Fun is an integer
    pub fn is_int(&self) -> bool {
        self.name.parse::<i32>().is_ok()
    }

    // Check if Fun is a float
    pub fn is_float(&self) -> bool {
        self.name.parse::<f64>().is_ok() && self.name != "." && self.name != "-."
    }

    // Check if two Fun objects are equal
    pub fn is_equal(&self, other: &Fun) -> bool {
        if self.name != other.name {
            return false;
        }
        if self.args.len() != other.args.len() {
            return false;
        }
        for (arg1, arg2) in self.args.iter().zip(&other.args) {
            if !arg1.is_equal(arg2) {
                return false;
            }
        }
        true
    }
}

// Implementing Display trait for Fun for easier printing
impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print())
    }
}


///////////////////////////////////////////////////////////////////////////////
/// Type System
/// 
/// The Type type is used to declare the type system of the grammar.
/// Each function, in the abstact grammar, has a type signature that is defined
/// by its input categories and output categoary.
/// 
/// # Example
/// 
/// ```rust
/// // A function which takes a Noun type and a Verb type. It then returns
/// // a Sentence type.
/// fun MakeSentence : Nount -> Verb -> Sentence;
/// ``` 
#[derive(Debug, Clone)]
pub struct Type {
    pub args: Vec<String>,
    pub cat: String,
}

impl Type {
    pub fn new(args: Vec<String>, cat: String) -> Self {
        Type { args, cat }
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The Apply type is used to _apply_ a function to the argumetns in the
/// concrete syntax of the abstarct grammar.
/// The each concrete grammar, functions are _applied_ to their arguments to
/// produce a final expression.
#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Apply {
    pub id: String,
    pub fun: ApplyFun,
    pub args: Vec<PArg>,
}

#[derive(Serialize, Debug, Clone, PartialEq)]
pub enum ApplyFun {
    FId(FId),
    CncFun(CncFun),
}

impl Apply {
    pub fn new(fun: ApplyFun, args: Vec<PArg>) -> Self {
        Apply {
            id: "Apply".to_string(),
            fun,
            args,
        }
    }

    pub fn show(&self, cat: &str) -> String {
        let fun_name = match &self.fun {
            ApplyFun::FId(_) => "Unknown Function".to_string(),
            ApplyFun::CncFun(cnc_fun) => cnc_fun.name.clone(),
        };

        format!("{} -> {} [{:?}]", cat, fun_name, self.args)
    }

    pub fn is_equal(&self, obj: &Apply) -> bool {
        self.id == obj.id
            && self.fun == obj.fun
            && self.args.len() == obj.args.len()
            && self.args.iter().zip(&obj.args).all(|(a, b)| a == b)
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The Coerce type is used to forcibly convert one type into another, in order
/// to make an expression match the expected (return) type of the expression.
/// Todo: improve explanation.
/// Typicaly involves _implicit_ type conversions during composition.
#[derive(Debug, Clone)]
pub struct Coerce {
    pub id: String,
    pub arg: FId,
}

impl Coerce {
    pub fn new(arg: FId) -> Self {
        Coerce {
            id: "Coerce".to_string(),
            arg,
        }
    }

    pub fn show(&self, cat: &str) -> String {
        format!("{} -> _ [{}]", cat, self.arg)
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The PArg type is used to provide _pattern arguments_ that can handle
/// parameters or arguments in the patterns of the grammar.
/// This increases the flexibility and reusability of the syntax that is used
/// to declare componentes in both the abstact and the concrete grammar.
#[derive(Debug, Clone, PartialEq)]
pub struct PArg {
    pub fid: FId,
    pub hypos: Vec<FId>,
}

impl PArg {
    pub fn new(hypos: Vec<FId>) -> Self {
        // TODO: look for better solution below.
        let fid = *hypos.last().unwrap_or(&0); // Default to 0 if no hypos provided
        let hypos = if hypos.len() > 1 {
            hypos[..hypos.len() - 1].to_vec()
        } else {
            Vec::new()
        };

        PArg { fid, hypos }
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The Const type represents a constant value or an expression in the grammar.
/// Constants are fixed values that are associated with specific elements in
/// in concrete grammars.
/// 
/// # Example
/// 
/// ```rust
/// line Hello = "hello";
/// ```
#[derive(Debug, Clone)]
pub struct Const {
    pub id: String,
    pub lit: Fun,
    pub toks: Vec<String>,
}

impl Const {
    pub fn new(lit: Fun, toks: Vec<String>) -> Self {
        Const {
            id: "Const".to_string(),
            lit,
            toks,
        }
    }

    pub fn show(&self, cat: &str) -> String {
        format!("{} -> {}", cat, self.lit.print())
    }

    pub fn is_equal(&self, obj: &Const) -> bool {
        if self.id != obj.id || !self.lit.is_equal(&obj.lit) || self.toks.len() != obj.toks.len() {
            return false;
        }

        self.toks.iter().zip(&obj.toks).all(|(a, b)| a == b)
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The CncFun type (short for ConcreteFunction) implements abstract functions
/// which are expressed in the abstract grammar, and stores them in a specific
/// concrete grammar. So, the CncFun type declares _how_ a function expressed
/// in the abstract grammar should be lineariezed in the concrete grammar.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CncFun {
    pub name: String,
    pub lins: LinType,
}

impl CncFun {
    pub fn new(name: &str, lins: Vec<FId>) -> Self {
        CncFun {
            name: name.to_string(),
            lins: LinType::FId(lins),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The SymCat type represents a _synctactic category_ symbol.
/// Categories liek Sentence, Noun, or Verb are defined in the abstract syntax.
// TODO: improve explanation.
// SymCat: Object to represent argument projections in grammar rules.
#[derive(Debug, Clone)]
pub struct SymCat {
    pub id: String,
    pub i: usize,
    pub label: usize,
}

impl SymCat {
    pub fn new(i: usize, label: usize) -> Self {
        SymCat {
            id: "Arg".to_string(),
            i,
            label,
        }
    }

    pub fn show(&self) -> String {
        format!("{}.{}", self.i, self.label)
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The SymKS type represents a _Syntactic Kernel Symbol_, a core syntactic 
/// element that is an essential component in GF's syntactic representation.
/// SymKS: Object to represent terminals in grammar rules
#[derive(Debug, Clone)]
pub struct SymKS {
    pub id: String,
    pub tokens: Vec<String>,
    pub tag: Option<String>,
}

impl SymKS {
    pub fn new(tokens: Vec<String>) -> Self {
        SymKS {
            id: "KS".to_string(),
            tokens,
            tag: None,
        }
    }

    pub fn show(&self) -> String {
        format!("\"{:?}\"", self.tokens)
    }
    
    pub fn tag_with(&self, tag: &str) -> Self {
        let mut s = SymKS::new(self.tokens.clone());
        s.tag = Some(tag.to_string());
        s
    }
}

impl From for SymKS {
    fn from() {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The SymKP type represents _Syntactic Kernel Phrases_. It combines kernel 
/// symbols into structured phrases, and is used to handle syntactic phrases
/// during parsing and generation.
#[derive(Debug, Clone)]
pub struct SymKP {
    pub id: String,
    pub tokens: Vec<SymKS>,
    pub alts: Vec<Alt>,
    pub tag: Option<String>, // Todo: tag might exit, wrap in Option<tag>?
}

impl SymKP {
    pub fn new(tokens: Vec<SymKS>, alts: Vec<Alt>) -> Self {
        SymKP {
            id: "KP".to_string(),
            tokens,
            alts,
            tag: None,
        }
    }

    pub fn show(&self) -> String {
        format!("\"{:?}\"", self.tokens)
    }

    pub fn tag_with(&self, tag: &str) -> Self {
        let mut s = SymKP::new(self.tokens.clone(), self.alts.clone());
        s.tag = Some(tag.to_string());
        s
    }
}

///////////////////////////////////////////////////////////////////////////////
/// The SymLit type is used to represent _literal symbols_ in the grammar, such 
/// as strings or numerical literals.
/// 
/// # Example
///
/// ```rust
/// lin NumLiteral n = show n;
/// ```
#[derive(Debug, Clone)]
pub struct SymLit {
    pub id: String,
    pub i: usize,
    pub label: usize,
}

impl SymLit {
    pub fn new(i: usize, label: usize) -> Self {
        SymLit {
            id: "Lit".to_string(),
            i,
            label,
        }
    }

    pub fn get_id(&self) -> &str {
        &self.id
    }

    pub fn show(&self) -> String {
        format!("{}.{}", self.i, self.label)
    }
}


///////////////////////////////////////////////////////////////////////////////
/// The Alt type represents _Alternatives_ and is used when we need to define
/// multiple possible realizations of a function or structure. It's useful in
/// both parsing and generation for handling ambiguity and variation.
/// 
/// # Example
/// 
/// ```rust
/// ling Greeting = "hello" | "hi";
/// ```
#[derive(Debug, Clone)]
pub struct Alt {
    pub tokens: Vec<SymKS>,
    pub prefixes: Vec<String>,
}

impl Alt {
    pub fn new(tokens: Vec<SymKS>, prefixes: Vec<String>) -> Self {
        Alt { tokens, prefixes }
    }
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// Handler an Wrapper Enums

// TODO: find a better solution.
#[derive(Debug, Clone)]
struct LProduction {
    fid: FId,
    fun: CncFun,
}

// TODO: find a better solution.
#[derive(Debug, Clone)]
pub struct TaggedString {
    pub token: String,
    pub tag: String,
}

impl TaggedString {
    pub fn new(s: &str, tag: &str) -> Self {
        TaggedString {
            token: s.to_string(),
            tag: tag.to_string(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Production {
    Apply(Apply),
    Coerce(Coerce),
    Const(Const),
}

// TODO: This function needs to be broken up.
pub fn production_from_json(json: &serde_json::Value) -> Option<Production> {
    match json["type"].as_str() {
        Some("Apply") => {
            let fid = json["fid"].as_u64().unwrap() as FId;
            let args = json["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|parg| {
                    let hypos = parg["hypos"]
                        .as_array()
                        .unwrap()
                        .iter()
                        .map(|h| h.as_u64().unwrap() as i32)
                        .collect();
                    let fid = parg["fid"].as_u64().unwrap() as FId;
                    PArg::new(hypos, fid)
                })
                .collect();
            Some(Production::Apply(Apply { id, fun, args }))
        }
        Some("Coerce") => {
            let arg = json["arg"].as_u64().unwrap() as FId;
            Some(Production::Coerce(Coerce { arg }))
        }
        _ => None,
    }
}



#[derive(serde::Serialize, Deserialize, Debug, Clone)]
pub enum LinType {
    FId(Vec<FId>),
    Sym(Vec<Vec<Sym>>),
}


/*
Trying to use an Enum until we find a way to use Generics here. 
pub enum Sym {
    SymCat,
    SymKS,
    SymKP,
    SymLit,
}
 */
#[derive(Debug, Clone)]
pub struct LinearizedSym {
    pub fid: i32,
    pub table: Vec<Vec<Sym>>,
}

////////////////////////////////////////////////////////////////////////////////
//* Sym: Definition of symbols present in linearization records
// TODO: sym_from_json should take a JSON.Sym.
pub fn sym_from_json(json: &serde_json::Value) -> Option<Sym> {
    match json["type"].as_str()? {
        "SymCat" => {
            let args = json["args"].as_array()?;
            let arg1 = args.get(0)?.as_u64()? as usize;
            let arg2 = args.get(1)?.as_u64()? as usize;
            // TODO: check if correct.
            // Some(Sym::SymCat(SymCat { arg1, arg2 }))
            Some(SymLit::new(arg1, arg2))
        }
        "SymLit" => {
            let args = json["args"].as_array()?;
            let arg1 = args.get(0)?.as_u64()? as usize;
            let arg2 = args.get(1)?.as_u64()? as usize;
            // TODO: check if correct.
            // Some(Sym::SymLit(SymLit { arg1, arg2 }))
            Some(SymLit::new(arg1, arg2))
        }
        "SymKS" => {
            let args = json["args"]
            .as_array()?
            .iter()
            .map(|arg| arg.as_str().unwrap_or_default().to_string())
            .collect();
        // TODO: check if correct.
            // Some(Sym::SymKS(SymKS { args }))
            Some(SymKP::new(args, alts))
        }
        "SymKP" => {
            let args = json["args"].as_array()?;

            // Parse the first argument as Vec<SymKS>
            let symks_args = args
                .get(0)?
                .as_array()?
                .iter()
                .filter_map(|sym| {
                    let inner_args = sym["args"]
                        .as_array()?
                        .iter()
                        .map(|arg| arg.as_str().unwrap_or_default().to_string())
                        .collect();
                    // TODO: check if correct:
                    // Some(SymKS { args: inner_args })
                    Some(SymKP::new(inner_args, alts))
                })
                .collect();

            // Parse the second argument as Vec<Alt>
            let alts = args
                .get(1)?
                .as_array()?
                .iter()
                .filter_map(|alt| {
                    let tokens = alt["args"]
                        .get(0)?
                        .as_array()?
                        .iter()
                        .filter_map(|sym| {
                            let inner_args = sym["args"]
                                .as_array()?
                                .iter()
                                .map(|arg| arg.as_str().unwrap_or_default().to_string())
                                .collect();
                            // Todo: check if correct.
                            // Some(SymKS { args: inner_args })
                            Some(SymKP::new(inner_args, alts))
                        })
                        .collect();
                    let prefixes = alt["args"]
                        .get(1)?
                        .as_array()?
                        .iter()
                        .map(|prefix| prefix.as_str().unwrap_or_default().to_string())
                        .collect();
                    Some(Alt::new(tokens, prefixes))
                })
                .collect();

            /*             Some(Sym::SymKP(SymKP {
            args: symks_args,
            alts,
            })) */
            // Todo: check if correct.
            Some(SymKP::new(symks_args, alts))
        }
        _ => None,
    }
}

////////////////////////////////////////////////////////////////////////////////
/// The Trie<T> type implements a prefix tree data structure which is used to 
/// store and retrieve tokens during the chart parser's operation.
/// It need to be generic, since we don't know what types it will store.
#[derive(Debug, Clone)]
pub struct Trie<T> {
    value: Option<Vec<T>>,
    items: HashMap<String, Trie<T>>,
}

impl<T> Trie<T> {
    pub fn new() -> Self {
        Trie {
            value: None,
            items: HashMap::new(),
        }
    }

    pub fn insert_chain(&mut self, keys: &[String], obj: Vec<T>) {
        let mut node = self;
        for key in keys {
            node = node.items.entry(key.clone()).or_insert_with(Trie::new);
        }
        node.value = Some(obj);
    }

    pub fn insert_chain1(&mut self, keys: &[String], obj: T) {
        let mut node = self;
        for key in keys {
            node = node.items.entry(key.clone()).or_insert_with(Trie::new);
        }
        match &mut node.value {
            Some(value) => value.push(obj),
            None => node.value = Some(vec![obj]),
        }
    }

    pub fn lookup(&self, key: &str) -> Option<&Trie<T>> {
        self.items.get(key)
    }

    pub fn is_empty(&self) -> bool {
        if self.value.is_some() {
            return false;
        }
        !self.items.is_empty()
    }
}

////////////////////////////////////////////////////////////////////////////////
/// The Chart type provides Chart Parsing functionality. Chart maintains a 
/// collection of `ActiveItem` instances, which represents grammatical items 
/// which the parser has partially recognized so far in its parsing sequence.
/// The Chart type works together with the ActiveItem type to track the progress
/// of a parsing operation.
pub struct Chart {
    // Maps FId to ActiveItemMap (label -> Vec<ActiveItem>)
    /// One active grammatical construct that the parser has recognized.
    active: HashMap<FId, HashMap<i32, Vec<ActiveItem>>>,
    
    // A list of ActiveItemMap, indexed by FId
    /// A group of active grammatical constructs that the parser has recognized.
    actives: Vec<HashMap<i32, Vec<ActiveItem>>>,

    // Maps a string key to FId
    passive: HashMap<String, FId>,

    // Maps FId to a list of Productions
    pub forest: HashMap<FId, Vec<Production>>,

    // The next available ID
    pub next_id: FId,

    // Offset value
    pub offset: usize,
}

impl Chart {
    /// Create a new Chart parser.
    pub fn new(concrete: &GFConcrete) -> Self {
        let mut forest = HashMap::new();

        // Populate the forest from concrete.pproductions
        for (fid, productions) in &concrete.pproductions {
            forest.insert(*fid, productions.clone());
        }

        Chart {
            active: HashMap::new(),
            actives: Vec::new(),
            passive: HashMap::new(),
            forest,
            next_id: concrete.total_fids as i32,
            offset: 0,
        }
    }

    pub fn lookup_ac(&self, fid: FId, label: i32) -> Option<&Vec<ActiveItem>> {
        self.active
            .get(&fid)
            .and_then(|item_map| item_map.get(&label))
    }

    pub fn lookup_aco(&self, offset: usize, fid: FId, label: i32) -> Option<&Vec<ActiveItem>> {
        let tmp = if offset == self.offset {
            self.active.get(&fid)
        } else {
            self.actives
                .get(offset)
                .and_then(|actives| actives.get(&fid))
        };

        tmp.and_then(|item_map| item_map.get(&label))
    }

    pub fn labels_ac(&self, fid: FId) -> Option<&HashMap<i32, Vec<ActiveItem>>> {
        self.active.get(&fid)
    }

    pub fn insert_ac(&mut self, fid: FId, label: i32, items: Vec<ActiveItem>) {
        let entry = self.active.entry(fid).or_insert_with(HashMap::new);
        entry.insert(label, items);
    }

    pub fn lookup_pc(&self, fid: FId, label: i32, offset: usize) -> Option<FId> {
        let key = format!("{}.{}-{}", fid, label, offset);
        self.passive.get(&key).cloned()
    }

    pub fn insert_pc(&mut self, fid1: FId, label: i32, offset: usize, fid2: FId) {
        let key = format!("{}.{}-{}", fid1, label, offset);
        self.passive.insert(key, fid2);
    }

    pub fn shift(&mut self) {
        self.actives.push(self.active.clone());
        self.active = HashMap::new();
        self.passive = HashMap::new();
        self.offset += 1;
    }

    pub fn expand_forest(&self, fid: FId) -> Vec<Apply> {
        let mut rules = Vec::new();

        fn go(
            rules: &mut Vec<Apply>,
            forest: &HashMap<FId, Vec<Production>>,
            rules0: &[Production],
        ) {
            for rule in rules0 {
                match rule {
                    Production::Apply(apply_rule) => rules.push(apply_rule.clone()),
                    Production::Coerce(coerce_rule) => {
                        if let Some(next_rules) = forest.get(&coerce_rule.arg) {
                            go(rules, forest, next_rules);
                        }
                    }
                }
            }
        }

        if let Some(rules0) = self.forest.get(&fid) {
            go(&mut rules, &self.forest, rules0);
        }

        rules
    }
}

pub type ActiveItemMap = HashMap<i32, Vec<ActiveItem>>;

////////////////////////////////////////////////////////////////////////////////
/// The ActiveItem type is used to represent partially completed parses.
/// It enables tracking how much of a grammatical rule that has been recognized.
/// We can use it to track _which_ grammatical rule that has been applied
/// and also _how much_ of the grammatical rule that has been matched so far.
/// The ActiveItem type and Chart type are used to together to manage the state
/// and track progress of the parsing operation. 
#[derive(Debug, Clone)]
pub struct ActiveItem {
    pub offset: usize,
    pub dot: usize,
    pub fun: CncFun,
    pub seq: Vec<Sym>,
    pub args: Vec<PArg>,
    pub fid: FId,
    pub lbl: i32,
}

impl ActiveItem {
    pub fn new(
        offset: usize,
        dot: usize,
        fun: CncFun,
        seq: Vec<Sym>,
        args: Vec<PArg>,
        fid: FId,
        lbl: i32,
    ) -> Self {
        ActiveItem {
            offset,
            dot,
            fun,
            seq,
            args,
            fid,
            lbl,
        }
    }

    pub fn is_equal(&self, obj: &ActiveItem) -> bool {
        self.offset == obj.offset
            && self.dot == obj.dot
            && self.fun == obj.fun
            && self.seq == obj.seq
            && self.args == obj.args
            && self.fid == obj.fid
            && self.lbl == obj.lbl
    }

    pub fn shift_over_arg(&self, i: usize, fid: FId) -> ActiveItem {
        let mut nargs = self.args.clone();
        let mut newfid: Vec<FId> = Vec::new();
        newfid.push(fid);
        nargs[i] = PArg::new(newfid);
        ActiveItem::new(
            self.offset,
            self.dot + 1,
            self.fun.clone(),
            self.seq.clone(),
            nargs,
            self.fid,
            self.lbl,
        )
    }

    pub fn shift_over_token(&self) -> ActiveItem {
        ActiveItem::new(
            self.offset,
            self.dot + 1,
            self.fun.clone(),
            self.seq.clone(),
            self.args.clone(),
            self.fid,
            self.lbl,
        )
    }
}

////////////////////////////////////////////////////////////////////////////////
// Utility functions
pub fn is_undefined<T>(value: Option<&T>) -> bool {
    value.is_none()
}

pub fn map_object<K, V, F, U>(obj: &HashMap<K, V>, fun: F) -> HashMap<K, U>
where
    K: Eq + std::hash::Hash + Clone,
    F: Fn(&V) -> U,
{
    obj.iter()
        .map(|(key, value)| (key.clone(), fun(value)))
        .collect()
}

// EOF
///////////////////////////////////////////////////////////////////////////////