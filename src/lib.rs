////////////////////////////////////////////////////////////////////////////////
// Rust Runtime for Grammatical Framework

////////////////////////////////////////////////////////////////////////////////
#![deny(missing_docs)]
#![doc(html_root_url = "https://docs.rs/gf-core/0.1/")]

////////////////////////////////////////////////////////////////////////////////
mod pgf_json;
use pgf_json::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

////////////////////////////////////////////////////////////////////////////////
/// Constants and type alias
// We create a type alias to avoid extremely long HashMap types.
type HMS3 = HashMap<String, HashMap<String, String>>;
/// Every constituent has an unique id. If the constituent is
/// discontinuous then it will be represented with several brackets
/// where they all will have the same id.
pub type FId = i32;

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
    pub fn new(
        abstract_: GFAbstract,
        concretes: HashMap<String, GFConcrete>,
    ) -> Self {
        GFGrammar { abstract_, concretes }
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
                    let mut translations: HashMap<String, String> =
                        HashMap::new();
                    for (c2, to_concrete) in &to_cncs {
                        translations
                            .insert(c2.clone(), to_concrete.linearize(&tree));
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

////////////////////////////////////////////////////////////////////////////////
/// GFAbstract
///
/// The GFAbstract type is used to describe the logical structure of a language
/// using the constructs; _function_, _category_, and _type_.
///
/// # Example
///
/// This example show how to create an abstract grammar.
///
/// ```no_run
/// abstract Lang = {
/// cat Sentence; -- catebory for a sentence.
/// fun MakeSentence : Noun -> Verb -> Sentence; -- function.
/// }
/// ```
#[derive(Debug, Clone)]
pub struct GFAbstract {
    pub startcat: String,
    types: HashMap<String, Type>,
}

impl GFAbstract {
    pub fn new(startcat: String, types: HashMap<String, Type>) -> Self {
        GFAbstract { startcat, types }
    }

    // Create GFAbstract from JSON-like data
    pub fn from_json(json: Abstract) -> Self {
        let types = json
            .funs
            .into_iter()
            .map(|(key, fun)| (key, Type::new(fun.args, fun.cat)))
            .collect();

        GFAbstract { startcat: json.startcat, types }
    }

    // Add a new type
    pub fn add_type(&mut self, fun: String, args: Vec<String>, cat: String) {
        self.types.insert(fun, Type::new(args, cat));
    }

    // Get the arguments of a function
    pub fn get_args(&self, fun: &str) -> Option<&Vec<String>> {
        self.types.get(fun).map(|t| &t.args)
    }

    // Get the category of a function
    pub fn get_cat(&self, fun: &str) -> Option<&String> {
        self.types.get(fun).map(|t| &t.cat)
    }

    // Annotate (only) meta variables
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

    // Handle literals
    pub fn handle_literals(&self, mut tree: Fun, r#type: &str) -> Fun {
        if tree.name != "?" {
            if r#type == "String" || r#type == "Int" || r#type == "Float" {
                tree.name = format!("{}_Literal_{}", r#type, tree.name);
            } else if let Some(typ) = self.types.get(&tree.name) {
                for (arg, expected_type) in tree.args.iter_mut().zip(&typ.args)
                {
                    *arg = self.handle_literals(arg.clone(), expected_type);
                }
            }
        }
        tree
    }

    // Copy a tree (deep copy)
    pub fn copy_tree(&self, x: &Fun) -> Fun {
        let mut tree = Fun::new(x.name.as_str().clone(), vec![]);
        tree.type_ = x.type_.clone();
        for arg in &x.args {
            tree.args.push(self.copy_tree(arg));
        }
        tree
    }

    // Parse a tree from a string
    pub fn parse_tree(
        &self,
        str: &str,
        r#type: Option<&String>,
    ) -> Option<Fun> {
        let tokens: Vec<&str> = str
            .split_whitespace()
            .flat_map(|s| s.split(|c| "()?:".contains(c)))
            .collect();

        if let Some(tree) =
            self.parse_tree_internal(&mut tokens.into_iter(), 0)
        {
            Some(self.annotate(tree, r#type))
        } else {
            None
        }
    }

    // Internal recursive tree parser
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
                tokens.next(); // Consume closing parenthesis
                Some(tree)
            } else if t == "?" {
                Some(Fun::new("?", vec![]))
            } else {
                let mut tree = Fun::new(t, vec![]);
                while let Some(arg) = self.parse_tree_internal(tokens, 1) {
                    tree.args.push(arg);
                }
                Some(tree)
            }
        } else {
            None
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// GFConcrete syntax.
///
/// The GFConcrete type is used to define one or more _realisations_, called
/// linearizations_ in GF, of the abstract grammar expressed in GFAbstract.
/// It's important to understand that a lineariztion of a GFAbstract does not
/// have to be in form of natural language, it can be anything from images, to
/// audio, to button presses in a user interface.
///
/// # Example
///
/// ```no_run
/// concrete LangEng of Lang = {
///     lincat Sentence = Str;
///     lin MakeSentence n v = n ++ v;
/// }
/// ```
#[derive(Clone)]
pub struct GFConcrete {
    pub flags: HashMap<String, String>,
    functions: Vec<CncFun>,
    pub start_cats: HashMap<String, (usize, usize)>,
    pub total_fids: usize,
    pub pproductions: HashMap<usize, Vec<Production>>,
    lproductions: HashMap<String, Vec<LProduction>>,
}

impl GFConcrete {
    pub fn new(
        flags: HashMap<String, String>,
        functions: Vec<CncFun>,
        productions: HashMap<usize, Vec<Production>>,
        start_cats: HashMap<String, (usize, usize)>,
        total_fids: usize,
    ) -> Self {
        let mut lproductions = HashMap::new();

        // TODO: Process productions to populate lproductions, but a better
        // solution is needed.
        for (&fid, rules) in &productions {
            for rule in rules {
                if let Production::Apply(ref apply_rule) = rule {
                    let fun: &CncFun = &functions[apply_rule.fun];
                    let register = |args: &[PArg],
                                    key: String,
                                    i: usize,
                                    lproductions: &mut HashMap<String,Vec<LProduction>>| 
                    {
                        if i < args.len() {
                            let arg = args[i].fid as usize;
                            let mut count = 0;

                            if let Some(rules) = productions.get(&arg) {
                                for rule in rules {
                                    if let Production::Coerce(
                                        ref coerce_rule,
                                    ) = rule
                                    {
                                        let new_key = format!(
                                            "{}_{}",
                                            key, coerce_rule.arg
                                        );
                                        register(
                                            args,
                                            new_key,
                                            i + 1,
                                            lproductions,
                                        );
                                        count += 1;
                                    }
                                }
                            }

                            if count == 0 {
                                let new_key = format!("{}_{}", key, arg);
                                register(args, new_key, i + 1, lproductions);
                            }
                        } else {
                            lproductions
                                .entry(key)
                                .or_insert_with(Vec::new)
                                .push(LProduction {
                                    fun: fun.clone(),
                                    fid: fid.try_into().unwrap(),
                                });
                        }
                    };

                    register(
                        &apply_rule.args,
                        fun.name.clone(),
                        0,
                        &mut lproductions,
                    );
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

    pub fn from_json(json: Concrete) -> Self {
        let productions: HashMap<usize, Vec<Production>> = json
            .productions
            .into_iter()
            .map(|(k, v)| {
                let prod = v
                    .into_iter()
                    .filter_map(production_from_json)
                    .collect::<Vec<_>>();
                (k.parse::<i32>().unwrap(), prod)
            })
            .collect();

        let functions = json
            .functions
            .into_iter()
            .map(|f| CncFun { name: f.name, lins: f.lins })
            .collect();

        let sequences = json
            .sequences
            .into_iter()
            .map(|syms| syms.into_iter().filter_map(sym_from_json).collect())
            .collect();

        let start_cats = json
            .categories
            .into_iter()
            .map(|(key, cat)| (key, (cat.start, cat.end)))
            .collect();

        GFConcrete::new(
            json.flags,
            productions,
            functions,
            sequences,
            start_cats,
            json.totalfids,
        )
    }

    pub fn linearize_syms(&self, tree: &Fun, tag: &str) -> Vec<LinearizedSym> {
        let mut res = Vec::new();

        // TODO: Lots of code duplication below. Look for a cleaner solution.
        // String
        if tree.is_string() {
            let sym = SymKS::new(vec![tree.name]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym { fid: -1, table: vec![vec![sym]] });
        }
        // INT
        else if tree.is_int() {
            let sym = SymKS::new(vec![tree.name]);
            sym.tag = Some(tag.to_string());
            /*
            let sym = Sym::SymKS {
                id: tree.name.clone(),
                tag: Some(tag.to_string()),
            });
            */
            res.push(LinearizedSym { fid: -2, table: vec![vec![sym]] });
        }
        // FLOAT
        else if tree.is_float() {
            let sym = SymKS::new(vec![tree.name]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym { fid: -3, table: vec![vec![sym]] });
        }
        // META
        else if tree.is_meta() {
            let cat =
                self.start_cats.get(tree.type_.as_ref().unwrap()).unwrap();
            let sym = Sym::KS(SymKS {
                id: tree.name.clone(),
                // TODO: need to add for fieled token
                tag: tag.to_string().unwrap(),
            });

            for fid in cat.0..=cat.1 {
                res.push(LinearizedSym {
                    fid: fid as i32,
                    table: vec![vec![sym.clone()]],
                });
            }
        }
        // MISC
        else {
            let mut cs: Vec<LinearizedSym> = Vec::new();
            for (i, arg) in tree.args.iter().enumerate() {
                let mut subs =
                    self.linearize_syms(arg, &format!("{}-{}", tag, i));
                if let Some(sub) = subs.get(0) {
                    cs.push(sub.clone());
                }
            }

            let mut key = tree.name.clone();
            for (i, c) in cs.iter().enumerate() {
                if c.fid == -5 {
                    if let Some((matched_key, _)) = self
                        .lproductions
                        .iter()
                        .find(|(k, _)| k.contains(&tree.name))
                    {
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
                        fid: rule.fid as i32,
                        table: Vec::new(),
                    };

                    for (j, lin) in rule.fun.lins.iter().enumerate() {
                        let mut toks: Vec<Sym> = Vec::new();
                        row.table.push(Vec::new());

                        for sym0 in lin {
                            match sym0 {
                                Sym::Arg(i) | Sym::Lit(i) => {
                                    let ts = &cs[*i].table[j];
                                    toks.extend_from_slice(ts);
                                }
                                Sym::KS(ks) => {
                                    toks.push(Sym::KS(
                                        ks.clone().tag_with(tag),
                                    ));
                                }
                                Sym::KP(kp) => {
                                    toks.push(Sym::KP(kp.clone()));
                                }
                            }
                        }

                        row.table[j] = toks;
                    }

                    res.push(row);
                }
            }
        } // End of MISC

        res
    }

    pub fn syms2toks(&self, syms: &[Sym]) -> Vec<TaggedString> {
        let mut ts = Vec::new();

        for i in 0..syms.len() {
            match &syms[i] {
                Sym::KS(sym) => {
                    if let Some(tag) = &sym.tag {
                        for token in &sym.tokens {
                            ts.push(TaggedString::new(token, tag));
                        }
                    }
                }
                Sym::KP(sym) => {
                    let mut added_alt = false;

                    // Terrible. Perhaps look at using mask instrinsics or a
                    // switch, anything to avoid these deeply nested ifs.
                    if i + 1 < syms.len() {
                        if let Sym::KS(next_sym) = &syms[i + 1] {
                            if let Some(next_token) = next_sym.tokens.first() {
                                for alt in &sym.alts {
                                    if alt
                                        .prefixes
                                        .iter()
                                        .any(|p| next_token.starts_with(p))
                                    {
                                        for symks in &alt.tokens {
                                            if let Some(tag) = &sym.tag {
                                                for token in &symks.tokens {
                                                    ts.push(
                                                        TaggedString::new(
                                                            token, tag,
                                                        ),
                                                    );
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
                        for symks in &sym.tokens {
                            if let Some(tag) = &sym.tag {
                                for token in &symks.tokens {
                                    ts.push(TaggedString::new(token, tag));
                                }
                            }
                        }
                    }
                }
            }
        }

        ts
    }

    pub fn linearize_all(&self, tree: &Fun) -> Vec<String> {
        self.linearize_syms(tree, "0")
            .into_iter()
            .map(|r| self.unlex(&self.syms2toks(&r.table[0])))
            .collect()
    }

    pub fn linearize(&self, tree: &Fun) -> String {
        let res = self.linearize_syms(tree, "0");
        if !res.is_empty() {
            self.unlex(&self.syms2toks(&res[0].table[0]))
        } else {
            String::new()
        }
    }

    pub fn tag_and_linearize(&self, tree: &Fun) -> Vec<TaggedString> {
        let res = self.linearize_syms(tree, "0");
        if !res.is_empty() {
            self.syms2toks(&res[0].table[0])
        } else {
            Vec::new()
        }
    }

    fn unlex(&self, ts: &[TaggedString]) -> String {
        if ts.is_empty() {
            return String::new();
        }

        let no_space_after = regex::Regex::new(r"^[\(\-\[]").unwrap();
        let no_space_before =
            regex::Regex::new(r"^[\.\,\?\!\)\:\;\-\]]").unwrap();

        let mut s = String::new();
        let mut s = String::new();

        for i in 0..ts.len() {
            let t = &ts[i].token;
            let after =
                if i + 1 < ts.len() { Some(&ts[i + 1].token) } else { None };

            s.push_str(t);

            if let Some(after) = after {
                if !no_space_after.is_match(t)
                    && !no_space_before.is_match(after)
                {
                    s.push(' ');
                }
            }
        }

        s
    }

    fn tokenize(&self, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut in_token = false;
        let mut start = 0;

        for (i, c) in input.chars().enumerate() {
            // if c == ' ' || c == '\f' || c == '\n' || c == '\r' || c == '\t' || c == '\u{000B}' || c == '\u{00A0}' {
            if c == ' '
                || c == '\n' // TODO: change this to '\f'
                || c == '\n'
                || c == '\r'
                || c == '\t'
                || c == '\u{000B}'
                || c == '\u{00A0}'
            {
                if in_token {
                    let end = i;
                    in_token = false;
                    tokens.push(input[start..end].to_string());
                }
            } else {
                if !in_token {
                    start = i;
                    in_token = true;
                }
            }
        }

        if in_token {
            tokens.push(input[start..].to_string());
        }

        tokens
    }

    /// Parses a string with a given start category.
    pub fn parse_string(&self, input: &str, cat: &str) -> Vec<Fun> {
        let tokens = self.tokenize(input);

        let mut ps = ParseState::new(self, cat.to_string());
        for token in tokens {
            if !ps.next(&token) {
                return Vec::new();
            }
        }

        ps.extract_trees()
    }

    pub fn complete(&self, input: &str, cat: &str) -> CompletionResult {
        let mut tokens: Vec<String> = input
            .trim()
            .split_whitespace()
            .filter(|t| !t.is_empty())
            .map(|t| t.to_string())
            .collect();

        // Capture the last token as it may be partial
        let current = tokens.pop().unwrap_or_default();

        // Initialize parse state objects
        let mut ps = ParseState::new(self, cat);
        let mut ps2 = ParseState::new(self, cat);

        // Iterate over tokens and feed one by one to the parser
        for token in &tokens {
            if !ps.next(token) {
                return CompletionResult {
                    consumed: vec![],
                    suggestions: vec![],
                };
            }
            ps2.next(token);
        }

        // Attempt to parse the current token, knowing it may be incomplete
        let mut current = current;
        if ps2.next(&current) {
            ps.next(&current);
            tokens.push(current.clone());
            current.clear();
        }

        // Get suggestions based on the parse state
        let acc = ps.complete(&current);
        let mut suggestions = Vec::new();

        if let Some(items) = acc.value {
            for a in items {
                for s in &a.seq {
                    match s {
                        Sym::KS(sym) => {
                            for t in &sym.tokens {
                                suggestions.push(t.clone());
                            }
                        }
                        Sym::KP(sym) => {
                            for symks in &sym.tokens {
                                for t in &symks.tokens {
                                    suggestions.push(t.clone());
                                }
                            }
                        }
                    }
                }
            }
        }

        CompletionResult { consumed: tokens, suggestions }
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
        Fun { name: name.to_string(), args, type_: None }
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
        self.name.parse::<f64>().is_ok()
            && self.name != "."
            && self.name != "-."
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

impl Apply {
    pub fn new(fun: ApplyFun, args: Vec<PArg>) -> Self {
        Apply { id: "Apply".to_string(), fun, args }
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

#[derive(Serialize, Debug, Clone, PartialEq)]
pub enum ApplyFun {
    FId(FId),
    CncFun(CncFun),
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
        Coerce { id: "Coerce".to_string(), arg }
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
        Const { id: "Const".to_string(), lit, toks }
    }

    pub fn show(&self, cat: &str) -> String {
        format!("{} -> {}", cat, self.lit.print())
    }

    pub fn is_equal(&self, obj: &Const) -> bool {
        if self.id != obj.id
            || !self.lit.is_equal(&obj.lit)
            || self.toks.len() != obj.toks.len()
        {
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
        CncFun { name: name.to_string(), lins: LinType::FId(lins) }
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
        SymCat { id: "Arg".to_string(), i, label }
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
        SymKS { id: "KS".to_string(), tokens, tag: None }
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
        SymKP { id: "KP".to_string(), tokens, alts, tag: None }
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
        SymLit { id: "Lit".to_string(), i, label }
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

////////////////////////////////////////////////////////////////////////////////
use crate::GFConcrete;
use std::collections::HashMap;

// pub type FId = i32;

///////////////////////////////////////////////////////////////////////////////
pub struct ParseState {
    concrete: GFConcrete,
    start_cat: String,
    items: Trie<ActiveItem>,
    chart: Chart,
}

impl ParseState {
    //The process function is responsible for iterating over items and applying the provided closures:
    pub fn process<F, G>(
        &self,
        value: Option<&Vec<ActiveItem>>,
        handle_fid: F,
        handle_item: G,
    ) where
        F: Fn(FId) -> Option<Const>,
        G: Fn(&[String], &ActiveItem),
    {
        if let Some(items) = value {
            for item in items {
                // Example usage of handle_fid
                handle_fid(item.fid);

                // Example usage of handle_item
                let tokens = vec!["example_token".to_string()]; // Placeholder for token extraction
                handle_item(&tokens, item);
            }
        }
    }

    pub fn new(concrete: GFConcrete, start_cat: String) -> Self {
        let mut items = Trie::new();
        let chart = Chart::new(concrete.clone());

        let mut active_items = Vec::new();

        if let Some(fids) = concrete.start_cats.get(&start_cat) {
            for fid in fids.0..=fids.1 {
                if let Some(ex_prods) = chart.expand_forest(fid as i32) {
                    for rule in ex_prods {
                        if let Production::Apply(apply_rule) = rule {
                            if let ApplyFun::CncFun(fun) = &apply_rule.fun {
                                for (lbl, lin) in fun.lins.iter().enumerate() {
                                    active_items.push(ActiveItem::new(
                                        0,
                                        0,
                                        fun.clone(),
                                        lin.clone(),
                                        apply_rule.args.clone(),
                                        fid as i32,
                                        lbl as i32,
                                    ));
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

    pub fn next(&mut self, token: &str) -> bool {
        let mut acc =
            self.items.lookup(token).cloned().unwrap_or_else(Trie::new);

        self.process(
            self.items.value.as_ref(),
            |fid| match fid {
                // String
                -1 => Some(Const::new(
                    Fun::new(&format!("\"{}\"", token), vec![]),
                    vec![token.to_string()],
                )),
                // Integer
                -2 => {
                    if token == "0"
                        || token
                            .parse::<i32>()
                            .ok()
                            .filter(|x| *x != 0)
                            .is_some()
                    {
                        Some(Const::new(
                            Fun::new(token),
                            vec![token.to_string()],
                        ))
                    } else {
                        None
                    }
                }
                // Float
                -3 => {
                    if token == "0"
                        || token == "0.0"
                        || token
                            .parse::<f64>()
                            .ok()
                            .filter(|x| *x != 0.0)
                            .is_some()
                    {
                        Some(Const::new(
                            Fun::new(token, vec![]),
                            vec![token.to_string()],
                        ))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            |tokens, item| {
                if tokens.get(0).map_or(false, |t| t == token) {
                    let tokens1: Vec<String> =
                        tokens.iter().skip(1).cloned().collect();
                    acc.insert_chain1(&tokens1, item.clone());
                }
            },
        );

        self.items = acc;
        self.chart.shift();

        !self.items.is_empty()
    }

    /**
     * For a ParseState and a partial input, return all possible completions
     * Based closely on ParseState.next()
     * currentToken could be empty or a partial string
     */
    pub fn complete(&mut self, current_token: &str) -> Trie<ActiveItem> {
        // Initialize accumulator for suggestions
        let mut acc = self
            .items
            .lookup(current_token)
            .cloned()
            .unwrap_or_else(Trie::new);

        self.process(
            // Items
            self.items.value.as_ref(),
            // Deal with literal categories
            |_fid| {
                // Always return None as per the TypeScript logic
                None
            },
            // Takes an array of tokens and populates the accumulator
            |tokens, item| {
                if current_token.is_empty()
                    || tokens
                        .get(0)
                        .map_or(false, |t| t.starts_with(current_token))
                {
                    let tokens1: Vec<String> =
                        tokens.iter().skip(1).cloned().collect();
                    acc.insert_chain1(&tokens1, item.clone());
                }
            },
        );

        // Return matches
        acc
    }

    pub fn extract_trees(&self) -> Vec<Fun> {
        // Process items with no meaningful side effects
        self.process(
            self.items.value.as_ref(),
            |_fid| None,         // First closure always returns None
            |_tokens, _item| {}, // Second closure does nothing
        );

        let total_fids = self.concrete.total_fids;
        let forest = &self.chart.forest;

        // Recursive helper function
        fn go(
            fid: FId,
            total_fids: FId,
            forest: &HashMap<FId, Vec<Production>>,
        ) -> Vec<Fun> {
            if fid < total_fids {
                return vec![Fun::new("?", vec![])];
            } else {
                let mut trees = Vec::new();

                if let Some(rules) = forest.get(&fid) {
                    for rule in rules {
                        match rule {
                            Production::Const(const_rule) => {
                                trees.push(const_rule.lit.clone());
                            }
                            Production::Apply(apply_rule) => {
                                let mut arg_indices =
                                    vec![0; apply_rule.args.len()];
                                let mut arg_trees: Vec<Vec<Fun>> = apply_rule
                                    .args
                                    .iter()
                                    .map(|arg| go(arg.fid, total_fids, forest))
                                    .collect();

                                loop {
                                    let mut t = Fun::new(
                                        &apply_rule.fun.get_name(),
                                        vec![],
                                    );
                                    for (k, arg_trees_k) in
                                        arg_trees.iter().enumerate()
                                    {
                                        t.set_arg(
                                            k,
                                            arg_trees_k[arg_indices[k]]
                                                .clone(),
                                        );
                                    }
                                    trees.push(t);

                                    let mut i = 0;
                                    while i < arg_trees.len() {
                                        arg_indices[i] += 1;
                                        if arg_indices[i] < arg_trees[i].len()
                                        {
                                            break;
                                        }
                                        arg_indices[i] = 0;
                                        i += 1;
                                    }

                                    if i >= arg_trees.len() {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }

                trees
            }
        }

        let mut trees = Vec::new();

        if let Some(fids) = self.concrete.start_cats.get(&self.start_cat) {
            for fid0 in fids.0..=fids.1 {
                let mut labels = HashMap::new();
                if let Some(rules) = self.chart.expand_forest(fid0 as i32) {
                    for rule in &rules {
                        if let Production::Apply(apply_rule) = rule {
                            if let ApplyFun::CncFun(fun) = &apply_rule.fun {
                                for lbl in 0..fun.lins.len() {
                                    labels.insert(lbl, true);
                                }
                            }
                        }
                    }
                }

                for lbl in labels.keys() {
                    let fid =
                        self.chart.lookup_pc(fid0 as i32, *lbl as i32, 0);
                    let arg_trees = go(fid, total_fids, forest);

                    for arg_tree in arg_trees {
                        if !trees.iter().any(|t| t.is_equal(&arg_tree)) {
                            trees.push(arg_tree);
                        }
                    }
                }
            }
        }

        trees
    }

    fn process<F, G>(
        &mut self,
        agenda: Option<&mut Vec<ActiveItem>>,
        literal_callback: F,
        token_callback: G,
    ) where
        F: Fn(FId) -> Option<Const>,
        G: Fn(&[String], ActiveItem),
    {
        if let Some(agenda) = agenda {
            while let Some(mut item) = agenda.pop() {
                let lin = &item.seq;

                if item.dot < lin.len() {
                    let sym0 = &lin[item.dot];
                    match sym0 {
                        Sym::SymCat(sym) => {
                            let fid = item.args[sym.i].fid;
                            let label = sym.label;

                            if let Some(items) =
                                self.chart.lookup_ac(fid, label)
                            {
                                if !items.iter().any(|existing_item| {
                                    existing_item.is_equal(&item)
                                }) {
                                    items.push(item.clone());
                                    if let Some(fid2) = self.chart.lookup_pc(
                                        fid,
                                        label,
                                        self.chart.offset,
                                    ) {
                                        agenda.push(
                                            item.shift_over_arg(sym.i, fid2),
                                        );
                                    }
                                }
                            } else {
                                if let Some(rules) =
                                    self.chart.expand_forest(fid)
                                {
                                    for rule in rules {
                                        if let Production::Apply(apply_rule) =
                                            rule
                                        {
                                            agenda.push(ActiveItem::new(
                                                self.chart.offset,
                                                0,
                                                apply_rule.fun.clone(),
                                                apply_rule.fun.lins[label]
                                                    .clone(),
                                                apply_rule.args.clone(),
                                                fid,
                                                label,
                                            ));
                                        }
                                    }
                                }
                                self.chart.insert_ac(
                                    fid,
                                    label,
                                    vec![item.clone()],
                                );
                            }
                        }
                        Sym::SymKS(sym) => {
                            token_callback(
                                &sym.tokens,
                                item.shift_over_token(),
                            );
                        }
                        Sym::SymKP(sym) => {
                            let pitem = item.shift_over_token();
                            for symks in &sym.tokens {
                                token_callback(&symks.tokens, pitem.clone());
                            }
                            for alt in &sym.alts {
                                for symks in &alt.tokens {
                                    token_callback(
                                        &symks.tokens,
                                        pitem.clone(),
                                    );
                                }
                            }
                        }
                        Sym::SymLit(sym) => {
                            let mut fid = item.args[sym.i].fid;

                            if let Some(rules) = self.chart.forest.get(&fid) {
                                if let Some(Production::Const(const_rule)) =
                                    rules.get(0)
                                {
                                    token_callback(
                                        &const_rule.toks,
                                        item.shift_over_token(),
                                    );
                                }
                            } else if let Some(rule) = literal_callback(fid) {
                                fid = self.chart.next_id;
                                self.chart.next_id += 1;
                                self.chart.forest.insert(
                                    fid,
                                    vec![Production::Const(rule.clone())],
                                );
                                token_callback(
                                    &rule.toks,
                                    item.shift_over_arg(sym.i, fid),
                                );
                            }
                        }
                    }
                } else {
                    let mut fid = self
                        .chart
                        .lookup_pc(item.fid, item.lbl, item.offset)
                        .unwrap_or_else(|| {
                            let new_fid = self.chart.next_id;
                            self.chart.next_id += 1;

                            if let Some(items) = self.chart.lookup_aco(
                                item.offset,
                                item.fid,
                                item.lbl,
                            ) {
                                for pitem in items {
                                    if let Sym::SymCat(sym_cat) =
                                        &pitem.seq[pitem.dot]
                                    {
                                        agenda.push(pitem.shift_over_arg(
                                            sym_cat.i, new_fid,
                                        ));
                                    }
                                }
                            }

                            self.chart.insert_pc(
                                item.fid,
                                item.lbl,
                                item.offset,
                                new_fid,
                            );
                            self.chart.forest.insert(
                                new_fid,
                                vec![Production::Apply(Apply::new(
                                    item.fun.clone(),
                                    item.args.clone(),
                                ))],
                            );

                            new_fid
                        });

                    if let Some(labels) = self.chart.labels_ac(fid) {
                        for lbl in labels {
                            agenda.push(ActiveItem::new(
                                self.chart.offset,
                                0,
                                item.fun.clone(),
                                item.fun.lins[lbl].clone(),
                                item.args.clone(),
                                fid,
                                lbl,
                            ));
                        }
                    }

                    if let Some(rules) = self.chart.forest.get_mut(&fid) {
                        let rule =
                            Apply::new(item.fun.clone(), item.args.clone());

                        if !rules.iter().any(|existing_rule| {
                            if let Production::Apply(existing_apply) =
                                existing_rule
                            {
                                existing_apply.is_equal(&rule)
                            } else {
                                false
                            }
                        }) {
                            rules.push(Production::Apply(rule));
                        }
                    }
                }
            }
        }
    }
} //ParseState impl ends.

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
        TaggedString { token: s.to_string(), tag: tag.to_string() }
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
                        .map(|arg| {
                            arg.as_str().unwrap_or_default().to_string()
                        })
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
                                .map(|arg| {
                                    arg.as_str()
                                        .unwrap_or_default()
                                        .to_string()
                                })
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
                        .map(|prefix| {
                            prefix.as_str().unwrap_or_default().to_string()
                        })
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
        Trie { value: None, items: HashMap::new() }
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
        self.active.get(&fid).and_then(|item_map| item_map.get(&label))
    }

    pub fn lookup_aco(
        &self,
        offset: usize,
        fid: FId,
        label: i32,
    ) -> Option<&Vec<ActiveItem>> {
        let tmp = if offset == self.offset {
            self.active.get(&fid)
        } else {
            self.actives.get(offset).and_then(|actives| actives.get(&fid))
        };

        tmp.and_then(|item_map| item_map.get(&label))
    }

    pub fn labels_ac(
        &self,
        fid: FId,
    ) -> Option<&HashMap<i32, Vec<ActiveItem>>> {
        self.active.get(&fid)
    }

    pub fn insert_ac(&mut self, fid: FId, label: i32, items: Vec<ActiveItem>) {
        let entry = self.active.entry(fid).or_insert_with(HashMap::new);
        entry.insert(label, items);
    }

    pub fn lookup_pc(
        &self,
        fid: FId,
        label: i32,
        offset: usize,
    ) -> Option<FId> {
        let key = format!("{}.{}-{}", fid, label, offset);
        self.passive.get(&key).cloned()
    }

    pub fn insert_pc(
        &mut self,
        fid1: FId,
        label: i32,
        offset: usize,
        fid2: FId,
    ) {
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
                    Production::Apply(apply_rule) => {
                        rules.push(apply_rule.clone())
                    }
                    Production::Coerce(coerce_rule) => {
                        if let Some(next_rules) = forest.get(&coerce_rule.arg)
                        {
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
        ActiveItem { offset, dot, fun, seq, args, fid, lbl }
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
    obj.iter().map(|(key, value)| (key.clone(), fun(value))).collect()
}

// EOF
///////////////////////////////////////////////////////////////////////////////
