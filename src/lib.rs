mod pgf_json;
pub use pgf_json::*;

use regex::Regex;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Serialize, Debug, Clone)]
pub struct Type {
    /// Input categories (arguments) for the function type.
    pub args: Vec<String>,
    /// Output category (result) for the function type.
    pub cat: String,
}

/// Rust Runtime for Grammatical Framework
///
/// This crate provides a runtime for Grammatical Framework (GF) grammars,
/// allowing parsing, linearization, and translation between languages defined
/// in GF abstract and concrete syntax.
///
/// For more information on GF, see [gf-dev.github.io](https://gf-dev.github.io/).
////////////////////////////////////////////////////////////////////////////////
// Constants and type aliases
/// Type alias for nested HashMaps used in translation outputs.
/// Represents source language -> abstract tree -> target language -> output string.
type HMS3 = HashMap<String, HashMap<String, String>>;

/// Every constituent has a unique id. If the constituent is discontinuous,
/// it will be represented with several brackets sharing the same id.
pub type FId = i32;

////////////////////////////////////////////////////////////////////////////////
/// Accumulator for completion suggestions during parsing.
///
/// Collects active parsing items that can suggest possible completions.
#[derive(Debug, Clone)]
pub struct CompletionAccumulator {
    /// Optional vector of active items for suggestions.
    pub value: Option<Vec<ActiveItem>>,
}

impl Default for CompletionAccumulator {
    fn default() -> Self {
        Self::new()
    }
}

impl CompletionAccumulator {
    /// Creates a new empty accumulator.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::CompletionAccumulator;
    /// let acc = CompletionAccumulator::new();
    /// assert!(acc.value.is_none());
    /// ```
    pub fn new() -> Self {
        CompletionAccumulator { value: None }
    }
}

/// Result of completion, including consumed tokens and suggestions.
///
/// This struct is returned by completion methods to provide feedback on
/// what has been parsed so far and possible ways to continue.
#[derive(Debug, Clone)]
pub struct CompletionResult {
    /// Tokens already consumed from the input.
    pub consumed: Vec<String>,
    /// Suggested completions for the next token(s).
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
/// # Examples
///
/// ```
/// use gf_core::{GFGrammar, GFAbstract};
/// use std::collections::HashMap;
/// let abstract_ = GFAbstract::new("S".to_string(), HashMap::new());
/// let concretes = HashMap::new();
/// let grammar = GFGrammar::new(abstract_, concretes);
/// assert_eq!(grammar.abstract_grammar.startcat, "S");
/// ```
pub struct GFGrammar {
    /// The abstract grammar (only one per GFGrammar).
    pub abstract_grammar: GFAbstract,

    /// Map of language codes to concrete grammars.
    pub concretes: HashMap<String, GFConcrete>,
}

impl GFGrammar {
    /// Creates a new GFGrammar from an abstract and concretes.
    ///
    /// # Arguments
    /// * `abstract_` - The abstract grammar.
    /// * `concretes` - A map of language codes to concrete grammars.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::{GFGrammar, GFAbstract};
    /// use std::collections::HashMap;
    /// let abstract_ = GFAbstract::new("S".to_string(), HashMap::new());
    /// let concretes = HashMap::new();
    /// let grammar = GFGrammar::new(abstract_, concretes);
    /// assert_eq!(grammar.abstract_grammar.startcat, "S");
    /// ```
    pub fn new(
        abstract_: GFAbstract,
        concretes: HashMap<String, GFConcrete>,
    ) -> Self {
        GFGrammar { abstract_grammar: abstract_, concretes }
    }

    /// Converts from PGF JSON format to GFGrammar.
    ///
    /// This method deserializes a PGF structure (typically from JSON) into a usable GFGrammar.
    ///
    /// # Arguments
    /// * `json` - The PGF structure to convert.
    ///
    /// # Examples
    ///
    /// Assuming a valid PGF instance, this creates a grammar:
    ///
    /// ```
    /// // Note: This doctest is illustrative; actual PGF creation may require deserialization.
    /// use gf_core::{GFGrammar, PGF, Abstract, Concrete};
    /// use std::collections::HashMap;
    /// let abstract_ = Abstract { name: "TestGrammar".to_string(), startcat: "S".to_string(), funs: HashMap::new() };
    /// let concretes = HashMap::new();
    /// let pgf = PGF { abstract_, concretes };
    /// let grammar = GFGrammar::from_json(pgf);
    /// assert_eq!(grammar.abstract_grammar.startcat, "S");
    /// ```
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

        for concrete in from_cncs.values() {
            let trees =
                concrete.parse_string(input, &self.abstract_grammar.startcat);
            if !trees.is_empty() {
                let mut c1_outputs: HashMap<String, HashMap<String, String>> =
                    HashMap::new();
                for tree in trees {
                    let mut translations: HashMap<String, String> =
                        HashMap::new();
                    for (c2, to_concrete) in &to_cncs {
                        translations
                            .insert(c2.clone(), to_concrete.linearize(&tree));
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
/// The abstract grammar specifies the semantic structure without tying it to specific
/// forms. It defines functions (fun) that combine categories (cat) into new categories.
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
    ///
    /// # Arguments
    /// * `startcat` - The starting category.
    /// * `types` - Map of function names to types.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::GFAbstract;
    /// use std::collections::HashMap;
    /// let abs = GFAbstract::new("S".to_string(), HashMap::new());
    /// assert_eq!(abs.startcat, "S");
    /// ```
    pub fn new(startcat: String, types: HashMap<String, Type>) -> Self {
        GFAbstract { startcat, types }
    }

    /// Converts from JSON abstract to GFAbstract.
    ///
    /// # Arguments
    /// * `json` - The Abstract structure from JSON.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::{GFAbstract, Abstract};
    /// use std::collections::HashMap;
    /// let json = Abstract { name: "TestGrammar".to_string(), startcat: "S".to_string(), funs: HashMap::new() };
    /// let abs = GFAbstract::from_json(json);
    /// assert_eq!(abs.startcat, "S");
    /// ```
    pub fn from_json(json: Abstract) -> Self {
        let types = json
            .funs
            .into_iter()
            .map(|(key, fun)| (key, Type::new(fun.args, fun.cat)))
            .collect();

        GFAbstract { startcat: json.startcat, types }
    }

    /// Adds a new function type to the abstract.
    ///
    /// # Arguments
    /// * `fun` - Function name.
    /// * `args` - Argument categories.
    /// * `cat` - Result category.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::GFAbstract;
    /// use std::collections::HashMap;
    /// let mut abs = GFAbstract::new("S".to_string(), HashMap::new());
    /// abs.add_type("MakeS".to_string(), vec!["NP".to_string(), "VP".to_string()], "S".to_string());
    /// assert_eq!(abs.get_cat("MakeS").unwrap(), "S");
    /// ```
    pub fn add_type(&mut self, fun: String, args: Vec<String>, cat: String) {
        self.types.insert(fun, Type::new(args, cat));
    }

    /// Gets arguments for a function.
    ///
    /// # Arguments
    /// * `fun` - Function name.
    ///
    /// # Returns
    /// Optional reference to vector of argument categories.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::GFAbstract;
    /// use std::collections::HashMap;
    /// let mut abs = GFAbstract::new("S".to_string(), HashMap::new());
    /// abs.add_type("MakeS".to_string(), vec!["NP".to_string(), "VP".to_string()], "S".to_string());
    /// assert_eq!(abs.get_args("MakeS").unwrap(), &vec!["NP".to_string(), "VP".to_string()]);
    /// ```
    pub fn get_args(&self, fun: &str) -> Option<&Vec<String>> {
        self.types.get(fun).map(|t| &t.args)
    }

    /// Gets category for a function.
    ///
    /// # Arguments
    /// * `fun` - Function name.
    ///
    /// # Returns
    /// Optional reference to result category.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::GFAbstract;
    /// use std::collections::HashMap;
    /// let mut abs = GFAbstract::new("S".to_string(), HashMap::new());
    /// abs.add_type("MakeS".to_string(), vec!["NP".to_string(), "VP".to_string()], "S".to_string());
    /// assert_eq!(abs.get_cat("MakeS").unwrap(), "S");
    /// ```
    pub fn get_cat(&self, fun: &str) -> Option<&String> {
        self.types.get(fun).map(|t| &t.cat)
    }

    /// Annotates meta variables in a tree with types.
    ///
    /// Recursively traverses the tree and assigns types to meta variables ('?') based on the abstract types.
    ///
    /// # Arguments
    /// * `tree` - The tree to annotate.
    /// * `type_` - Optional type to assign.
    ///
    /// # Returns
    /// Annotated tree.
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
    ///
    /// Modifies tree nodes representing literals to wrap them in appropriate literal functions.
    ///
    /// # Arguments
    /// * `tree` - The tree to process.
    /// * `type_` - The type of the current node.
    ///
    /// # Returns
    /// Processed tree.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::{GFAbstract, Fun};
    /// use std::collections::HashMap;
    /// let abs = GFAbstract::new("S".to_string(), HashMap::new());
    /// let tree = Fun::new("\"hello\"".to_string(), vec![]);
    /// let handled = abs.handle_literals(tree, "String");
    /// assert_eq!(handled.name, "String_Literal_\"hello\"");
    /// ```
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

    /// Deep copies a tree.
    ///
    /// # Arguments
    /// * `x` - The tree to copy.
    ///
    /// # Returns
    /// A deep clone of the tree.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::{GFAbstract, Fun};
    /// use std::collections::HashMap;
    /// let abs = GFAbstract::new("S".to_string(), HashMap::new());
    /// let tree = Fun::new("Test".to_string(), vec![Fun::new("Arg".to_string(), vec![])]);
    /// let copy = abs.copy_tree(&tree);
    /// assert_eq!(tree.name, copy.name);
    /// assert_eq!(tree.args.len(), copy.args.len());
    /// ```
    #[allow(clippy::only_used_in_recursion)]
    pub fn copy_tree(&self, x: &Fun) -> Fun {
        let mut tree = Fun::new(x.name.clone(), vec![]);
        tree.type_ = x.type_.clone();
        for arg in &x.args {
            tree.args.push(self.copy_tree(arg));
        }
        tree
    }

    /// Parses a string into a tree.
    ///
    /// # Arguments
    /// * `str` - The string representation of the tree.
    /// * `type_` - Optional type for annotation.
    ///
    /// # Returns
    /// Optional parsed and annotated tree.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::GFAbstract;
    /// use std::collections::HashMap;
    /// let abs = GFAbstract::new("S".to_string(), HashMap::new());
    /// let tree = abs.parse_tree("Test (Arg)", None);
    /// assert!(tree.is_some());
    /// let tree = tree.unwrap();
    /// assert_eq!(tree.name, "Test");
    /// assert_eq!(tree.args.len(), 1);
    /// assert_eq!(tree.args[0].name, "Arg");
    /// ```
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

        self.parse_tree_internal(&mut tokens.into_iter(), 0)
            .map(|tree| self.annotate(tree, r#type))
    }

    /// Internal recursive parser for trees.
    ///
    /// Parses tokens into a tree structure, handling parentheses and meta variables.
    #[allow(clippy::only_used_in_recursion)]
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
/// A concrete grammar maps abstract functions to linearization rules, which
/// produce output forms based on the arguments.
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
    ///
    /// # Arguments
    /// * `flags` - Concrete-specific flags.
    /// * `functions` - List of runtime concrete functions.
    /// * `productions` - Map of productions by FId.
    /// * `start_cats` - Start category ranges.
    /// * `total_fids` - Total number of FIds.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::{GFConcrete, RuntimeCncFun, LinType, Production};
    /// use std::collections::HashMap;
    /// let flags = HashMap::new();
    /// let functions = vec![];
    /// let productions = HashMap::new();
    /// let start_cats = HashMap::new();
    /// let total_fids = 0;
    /// let concrete = GFConcrete::new(flags, functions, productions, start_cats, total_fids);
    /// ```
    pub fn new(
        flags: HashMap<String, String>,
        functions: Vec<RuntimeCncFun>,
        productions: HashMap<i32, Vec<Production>>,
        start_cats: HashMap<String, (i32, i32)>,
        total_fids: i32,
    ) -> Self {
        let mut lproductions = HashMap::new();

        #[allow(clippy::too_many_arguments)]
        fn register_recursive(
            args: &[PArg],
            key: String,
            i: usize,
            lproductions: &mut HashMap<String, Vec<LProduction>>,
            productions: &HashMap<i32, Vec<Production>>,
            fun: &RuntimeCncFun,
            fid: FId,
            depth: usize,
        ) {
            if depth > 100 {
                // Prevent stack overflow
                return;
            }
            if i < args.len() {
                let arg = args[i].fid;
                let mut count = 0;

                if let Some(rules) = productions.get(&arg) {
                    for rule in rules {
                        if let Production::Coerce(ref coerce_rule) = rule {
                            let new_key =
                                format!("{}_{}", key, coerce_rule.arg);
                            register_recursive(
                                args,
                                new_key,
                                i + 1,
                                lproductions,
                                productions,
                                fun,
                                fid,
                                depth + 1,
                            );
                            count += 1;
                        }
                    }
                }

                if count == 0 {
                    let new_key = format!("{key}_{arg}");
                    register_recursive(
                        args,
                        new_key,
                        i + 1,
                        lproductions,
                        productions,
                        fun,
                        fid,
                        depth + 1,
                    );
                }
            } else {
                lproductions
                    .entry(key)
                    .or_default()
                    .push(LProduction { fun: fun.clone(), fid });
            }
        }

        for (&fid, rules) in &productions {
            for rule in rules {
                if let Production::Apply(ref apply_rule) = rule {
                    match apply_rule.to_apply_fun() {
                        ApplyFun::FId(fun_id) => {
                            // For FId, we need to find the corresponding function name
                            // This is a simplified approach - in real usage we'd need better mapping
                            if (fun_id as usize) < functions.len() {
                                let fun = &functions[fun_id as usize];
                                register_recursive(
                                    &apply_rule.args,
                                    fun.name.clone(),
                                    0,
                                    &mut lproductions,
                                    &productions,
                                    fun,
                                    fid,
                                    0,
                                );
                            }
                        }
                        ApplyFun::CncFun(json_fun) => {
                            // Create a temporary runtime function for registration
                            let runtime_fun = RuntimeCncFun::new(
                                json_fun.name.clone(),
                                LinType::FId(json_fun.lins.clone()),
                            );
                            register_recursive(
                                &apply_rule.args,
                                runtime_fun.name.clone(),
                                0,
                                &mut lproductions,
                                &productions,
                                &runtime_fun,
                                fid,
                                0,
                            );
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
    ///
    /// This method deserializes a Concrete structure into a usable GFConcrete,
    /// resolving sequence indices to actual symbols.
    ///
    /// # Arguments
    /// * `json` - The Concrete structure from JSON.
    pub fn from_json(json: Concrete) -> Self {
        let productions: HashMap<i32, Vec<Production>> = json.productions;

        let sequences: Vec<Vec<Sym>> = json.sequences;

        let functions: Vec<RuntimeCncFun> = json
            .functions
            .into_iter()
            .map(|f| {
                // Convert Vec<i32> indices to actual symbol sequences
                let lins = if f.lins.is_empty() {
                    LinType::Sym(vec![])
                } else {
                    let symbol_sequences: Vec<Vec<Sym>> = f
                        .lins
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

                RuntimeCncFun { name: f.name, lins }
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
    ///
    /// Produces a vector of linearized symbols for the tree, handling different types
    /// like metas, literals, and functions.
    ///
    /// # Arguments
    /// * `tree` - The abstract tree to linearize.
    /// * `tag` - The tag for tracking.
    ///
    /// # Returns
    /// Vector of LinearizedSym.
    pub fn linearize_syms(&self, tree: &Fun, tag: &str) -> Vec<LinearizedSym> {
        let mut res = Vec::new();

        if tree.is_string() {
            let mut sym = SymKS::new(vec![tree.name.clone()]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym {
                fid: -1,
                table: vec![vec![Sym::SymKS(sym)]],
            });
        } else if tree.is_int() {
            let mut sym = SymKS::new(vec![tree.name.clone()]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym {
                fid: -2,
                table: vec![vec![Sym::SymKS(sym)]],
            });
        } else if tree.is_float() {
            let mut sym = SymKS::new(vec![tree.name.clone()]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym {
                fid: -3,
                table: vec![vec![Sym::SymKS(sym)]],
            });
        } else if tree.is_meta() {
            let cat = self
                .start_cats
                .get(tree.type_.as_ref().unwrap_or(&String::new()))
                .cloned()
                .unwrap_or((0, 0));
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
            let cs: Vec<LinearizedSym> = tree
                .args
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                    self.linearize_syms(arg, &format!("{tag}-{i}"))[0].clone()
                })
                .collect();

            let mut key = tree.name.clone();
            for c in &cs {
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
                    let mut row =
                        LinearizedSym { fid: rule.fid, table: Vec::new() };

                    match &rule.fun.lins {
                        LinType::Sym(lins) => {
                            for (j, lin) in lins.iter().enumerate() {
                                let mut toks: Vec<Sym> = Vec::new();
                                if j >= row.table.len() {
                                    row.table.push(Vec::new());
                                }

                                for sym0 in lin {
                                    match sym0 {
                                        Sym::SymCat { i, .. }
                                        | Sym::SymLit { i, .. } => {
                                            if *i < cs.len()
                                                && j < cs[*i].table.len()
                                            {
                                                let ts = &cs[*i].table[j];
                                                toks.extend_from_slice(ts);
                                            }
                                        }
                                        Sym::SymKS(ks) => {
                                            toks.push(Sym::SymKS(
                                                ks.tag_with(tag),
                                            ));
                                        }
                                        Sym::SymKP(kp) => {
                                            toks.push(Sym::SymKP(
                                                kp.tag_with(tag),
                                            ));
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
    ///
    /// Processes a sequence of symbols into tagged strings, handling KS and KP symbols.
    ///
    /// # Arguments
    /// * `syms` - Slice of symbols.
    ///
    /// # Returns
    /// Vector of TaggedString.
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
                        if let Some(tag) = &sym.tag {
                            for symks in &sym.tokens {
                                for token in &symks.tokens {
                                    ts.push(TaggedString::new(token, tag));
                                }
                            }
                        }
                    }
                }
                _ => {} // Ignore non-token symbols
            }
        }

        ts
    }

    /// Linearizes a tree to all possible strings.
    ///
    /// # Arguments
    /// * `tree` - The tree to linearize.
    ///
    /// # Returns
    /// Vector of all possible linearizations.
    pub fn linearize_all(&self, tree: &Fun) -> Vec<String> {
        self.linearize_syms(tree, "0")
            .into_iter()
            .map(|r| self.unlex(&self.syms2toks(&r.table[0])))
            .collect()
    }

    /// Linearizes a tree to a single string (first variant).
    ///
    /// # Arguments
    /// * `tree` - The tree to linearize.
    ///
    /// # Returns
    /// The linearized string.
    pub fn linearize(&self, tree: &Fun) -> String {
        let res = self.linearize_syms(tree, "0");
        if !res.is_empty() {
            self.unlex(&self.syms2toks(&res[0].table[0]))
        } else {
            String::new()
        }
    }

    /// Linearizes a tree with tags.
    ///
    /// # Arguments
    /// * `tree` - The tree to linearize.
    ///
    /// # Returns
    /// Vector of tagged strings.
    pub fn tag_and_linearize(&self, tree: &Fun) -> Vec<TaggedString> {
        let res = self.linearize_syms(tree, "0");
        if !res.is_empty() {
            self.syms2toks(&res[0].table[0])
        } else {
            Vec::new()
        }
    }

    /// Joins tagged strings into a single string, handling spacing.
    ///
    /// Applies rules for when to insert spaces between tokens.
    ///
    /// # Arguments
    /// * `ts` - Slice of tagged strings.
    ///
    /// # Returns
    /// The joined string.
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
                if !no_space_after.is_match(t)
                    && !no_space_before.is_match(after)
                {
                    s.push(' ');
                }
            }
        }

        s
    }

    /// Tokenizes input string by whitespace.
    ///
    /// # Arguments
    /// * `input` - The input string.
    ///
    /// # Returns
    /// Vector of tokens.
    ///
    fn tokenize(&self, input: &str) -> Vec<String> {
        input.split_whitespace().map(String::from).collect()
    }

    /// Parses a string into trees using the given start category.
    ///
    /// # Arguments
    /// * `input` - The input string.
    /// * `cat` - The start category.
    ///
    /// # Returns
    /// Vector of parsed trees.
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
    ///
    /// # Arguments
    /// * `input` - The partial input string.
    /// * `cat` - The start category.
    ///
    /// # Returns
    /// CompletionResult with consumed tokens and suggestions.
    pub fn complete(&self, input: &str, cat: &str) -> CompletionResult {
        let mut tokens: Vec<String> = input
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
                        _ => {}
                    }
                }
            }
        }

        CompletionResult { consumed: tokens, suggestions }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Prefix tree for parsing items.
///
/// Used to efficiently store and retrieve sequences of active items during parsing.
#[derive(Debug, Clone)]
pub struct Trie<T> {
    /// Value at node.
    value: Option<Vec<T>>,
    /// Child nodes.
    items: HashMap<String, Trie<T>>,
}

impl<T: Clone> Default for Trie<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Trie<T> {
    /// Creates a new trie.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::Trie;
    /// let trie: Trie<i32> = Trie::new();
    /// assert!(trie.is_empty());
    /// ```
    pub fn new() -> Self {
        Trie { value: None, items: HashMap::new() }
    }

    /// Inserts a chain of keys with value.
    ///
    /// # Arguments
    /// * `keys` - Slice of keys.
    /// * `obj` - Value to insert.
    pub fn insert_chain(&mut self, keys: &[String], obj: Vec<T>) {
        let mut node = self;
        for key in keys {
            node = node.items.entry(key.clone()).or_default();
        }
        node.value = Some(obj);
    }

    /// Inserts a chain with single item.
    ///
    /// # Arguments
    /// * `keys` - Slice of keys.
    /// * `obj` - Single item to insert.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::Trie;
    /// let mut trie: Trie<i32> = Trie::new();
    /// trie.insert_chain1(&["a".to_string(), "b".to_string()], 42);
    /// // Verify the key path exists
    /// assert!(trie.lookup("a").is_some());
    /// assert!(trie.lookup("a").unwrap().lookup("b").is_some());
    /// ```
    pub fn insert_chain1(&mut self, keys: &[String], obj: T) {
        let mut node = self;
        for key in keys {
            node = node.items.entry(key.clone()).or_default();
        }
        if let Some(value) = &mut node.value {
            value.push(obj);
        } else {
            node.value = Some(vec![obj]);
        }
    }

    /// Looks up a key.
    ///
    /// # Arguments
    /// * `key` - The key to look up.
    ///
    /// # Returns
    /// Optional reference to the sub-trie.
    pub fn lookup(&self, key: &str) -> Option<&Trie<T>> {
        self.items.get(key)
    }

    /// Checks if trie is empty.
    ///
    /// # Returns
    /// True if no value and no items.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::Trie;
    /// let trie: Trie<i32> = Trie::new();
    /// assert!(trie.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.value.is_none() && self.items.is_empty()
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Chart for parsing.
///
/// Manages active and passive items during chart parsing.
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
    ///
    /// Initializes the forest with productions from the concrete grammar.
    ///
    /// # Arguments
    /// * `concrete` - The concrete grammar.
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
    ///
    /// # Arguments
    /// * `fid` - FId.
    /// * `label` - Label.
    ///
    /// # Returns
    /// Optional reference to vector of active items.
    pub fn lookup_ac(&self, fid: FId, label: i32) -> Option<&Vec<ActiveItem>> {
        self.active.get(&fid).and_then(|m| m.get(&label))
    }

    /// Looks up active items by offset.
    ///
    /// # Arguments
    /// * `offset` - Offset.
    /// * `fid` - FId.
    /// * `label` - Label.
    ///
    /// # Returns
    /// Optional reference to vector of active items.
    pub fn lookup_aco(
        &self,
        offset: usize,
        fid: FId,
        label: i32,
    ) -> Option<&Vec<ActiveItem>> {
        if offset == self.offset {
            self.lookup_ac(fid, label)
        } else {
            self.actives.get(offset)?.get(&fid).and_then(|m| m.get(&label))
        }
    }

    /// Gets labels for active FId.
    ///
    /// # Arguments
    /// * `fid` - FId.
    ///
    /// # Returns
    /// Optional vector of labels.
    pub fn labels_ac(&self, fid: FId) -> Option<Vec<i32>> {
        self.active.get(&fid).map(|m| m.keys().cloned().collect())
    }

    /// Inserts active items.
    ///
    /// # Arguments
    /// * `fid` - FId.
    /// * `label` - Label.
    /// * `items` - Vector of active items.
    pub fn insert_ac(&mut self, fid: FId, label: i32, items: Vec<ActiveItem>) {
        self.active.entry(fid).or_default().insert(label, items);
    }

    /// Looks up passive FId.
    ///
    /// # Arguments
    /// * `fid` - FId.
    /// * `label` - Label.
    /// * `offset` - Offset.
    ///
    /// # Returns
    /// Optional FId.
    pub fn lookup_pc(
        &self,
        fid: FId,
        label: i32,
        offset: usize,
    ) -> Option<FId> {
        let key = format!("{fid}.{label}-{offset}");
        self.passive.get(&key).cloned()
    }

    /// Inserts passive FId.
    ///
    /// # Arguments
    /// * `fid` - FId.
    /// * `label` - Label.
    /// * `offset` - Offset.
    /// * `fid2` - FId to insert.
    pub fn insert_pc(
        &mut self,
        fid: FId,
        label: i32,
        offset: usize,
        fid2: FId,
    ) {
        let key = format!("{fid}.{label}-{offset}");
        self.passive.insert(key, fid2);
    }

    /// Shifts the chart to next offset.
    ///
    /// Moves current active to actives and clears current active and passive.
    pub fn shift(&mut self) {
        self.actives.push(self.active.clone());
        self.active.clear();
        self.passive.clear();
        self.offset += 1;
    }

    /// Expands forest for FId.
    ///
    /// Recursively expands coercions to get all applicable productions.
    ///
    /// # Arguments
    /// * `fid` - FId.
    ///
    /// # Returns
    /// Vector of productions.
    pub fn expand_forest(&self, fid: FId) -> Vec<Production> {
        let mut rules = Vec::new();

        fn go(
            forest: &HashMap<FId, Vec<Production>>,
            fid: FId,
            rules: &mut Vec<Production>,
        ) {
            if let Some(prods) = forest.get(&fid) {
                for prod in prods {
                    match prod {
                        Production::Apply(apply) => {
                            rules.push(Production::Apply(apply.clone()))
                        }
                        Production::Coerce(coerce) => {
                            go(forest, coerce.arg, rules)
                        }
                        Production::Const(const_) => {
                            rules.push(Production::Const(const_.clone()))
                        }
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
///
/// Maintains the current state of parsing, including the trie of items and the chart.
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
    ///
    /// Advances the parsing agenda using provided callbacks for literals and tokens.
    ///
    /// # Arguments
    /// * `agenda` - Mutable vector of active items.
    /// * `literal_callback` - Callback for literals.
    /// * `token_callback` - Callback for tokens.
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
                        if let Some(items) =
                            self.chart.lookup_ac(fid, *label as i32)
                        {
                            if !items.contains(&item) {
                                let mut items = items.clone();
                                items.push(item.clone());
                                self.chart.insert_ac(
                                    fid,
                                    *label as i32,
                                    items,
                                );
                                if let Some(fid2) = self.chart.lookup_pc(
                                    fid,
                                    *label as i32,
                                    self.chart.offset,
                                ) {
                                    agenda.push(item.shift_over_arg(*i, fid2));
                                }
                            }
                        } else {
                            let rules = self.chart.expand_forest(fid);
                            for rule in rules {
                                if let Production::Apply(apply) = rule {
                                    let runtime_fun = match apply
                                        .to_apply_fun()
                                    {
                                        ApplyFun::CncFun(fun) => {
                                            // Convert from JSON CncFun to RuntimeCncFun if needed
                                            // For now, assume it's already converted at construction
                                            RuntimeCncFun::new(
                                                fun.name.clone(),
                                                LinType::FId(fun.lins.clone()),
                                            )
                                        }
                                        ApplyFun::FId(id) => {
                                            // Create a placeholder RuntimeCncFun for FId
                                            RuntimeCncFun::new(
                                                id.to_string(),
                                                LinType::FId(vec![]),
                                            )
                                        }
                                    };

                                    let seq = match &runtime_fun.lins {
                                        LinType::Sym(syms) => {
                                            if *label < syms.len() {
                                                syms[*label].clone()
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
                    Sym::SymKS(sym) => {
                        token_callback(&sym.tokens, item.shift_over_token())
                    }
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
                            if let Some(Production::Const(const_rule)) =
                                rules.first()
                            {
                                token_callback(
                                    &const_rule.toks,
                                    item.shift_over_token(),
                                );
                            }
                        } else if let Some(rule) = literal_callback(fid) {
                            let new_fid = self.chart.next_id;
                            self.chart.next_id += 1;
                            self.chart.forest.insert(
                                new_fid,
                                vec![Production::Const(rule.clone())],
                            );
                            token_callback(
                                &rule.toks,
                                item.shift_over_arg(*i, new_fid),
                            );
                        }
                    }
                }
            } else {
                let fid = self
                    .chart
                    .lookup_pc(item.fid, item.lbl, item.offset)
                    .unwrap_or_else(|| {
                        let new_fid = self.chart.next_id;
                        self.chart.next_id += 1;
                        self.chart.insert_pc(
                            item.fid,
                            item.lbl,
                            item.offset,
                            new_fid,
                        );
                        let apply = Apply {
                            fid: None,
                            fun: Some(CncFun::new(
                                item.fun.name.clone(),
                                vec![],
                            )),
                            args: item.args.clone(),
                        };
                        self.chart
                            .forest
                            .insert(new_fid, vec![Production::Apply(apply)]);
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
    ///
    /// Initializes the parse state with starting active items based on the start category.
    ///
    /// # Arguments
    /// * `concrete` - The concrete grammar.
    /// * `start_cat` - The start category.
    pub fn new(concrete: GFConcrete, start_cat: String) -> Self {
        let mut items = Trie::new();
        let chart = Chart::new(concrete.clone());

        let mut active_items = Vec::new();

        if let Some((start, end)) = concrete.start_cats.get(&start_cat) {
            for fid in *start..=*end {
                let rules = chart.expand_forest(fid);
                for rule in rules {
                    if let Production::Apply(apply) = rule {
                        match apply.to_apply_fun() {
                            ApplyFun::CncFun(json_fun) => {
                                // Convert JSON CncFun to RuntimeCncFun
                                let runtime_fun = RuntimeCncFun::new(
                                    json_fun.name.clone(),
                                    LinType::FId(json_fun.lins.clone()),
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
                                if (fun_id as usize) < concrete.functions.len()
                                {
                                    let runtime_fun = concrete.functions
                                        [fun_id as usize]
                                        .clone();
                                    match &runtime_fun.lins {
                                        LinType::Sym(lins) => {
                                            for (lbl, lin) in
                                                lins.iter().enumerate()
                                            {
                                                active_items.push(
                                                    ActiveItem::new(
                                                        0,
                                                        0,
                                                        runtime_fun.clone(),
                                                        lin.clone(),
                                                        apply.args.clone(),
                                                        fid,
                                                        lbl as i32,
                                                    ),
                                                );
                                            }
                                        }
                                        LinType::FId(_) => {
                                            active_items.push(
                                                ActiveItem::new(
                                                    0,
                                                    0,
                                                    runtime_fun,
                                                    vec![],
                                                    apply.args.clone(),
                                                    fid,
                                                    0,
                                                ),
                                            );
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
    ///
    /// # Arguments
    /// * `token` - The next token.
    ///
    /// # Returns
    /// True if parsing can continue, false if failed.
    pub fn next(&mut self, token: &str) -> bool {
        let mut acc =
            self.items.lookup(token).cloned().unwrap_or_else(Trie::new);

        let mut agenda = self.items.value.clone().unwrap_or_default();

        self.process(
            &mut agenda,
            |fid| match fid {
                -1 => Some(Const::new(
                    Fun::new(format!("\"{token}\""), vec![]),
                    vec![token.to_string()],
                )),
                -2 if token.parse::<i32>().is_ok() => Some(Const::new(
                    Fun::new(token.to_string(), vec![]),
                    vec![token.to_string()],
                )),
                -3 if token.parse::<f64>().is_ok() => Some(Const::new(
                    Fun::new(token.to_string(), vec![]),
                    vec![token.to_string()],
                )),
                _ => None,
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
    ///
    /// # Arguments
    /// * `current_token` - The partial current token.
    ///
    /// # Returns
    /// CompletionAccumulator with possible completions.
    pub fn complete(&self, current_token: &str) -> CompletionAccumulator {
        let mut acc = self
            .items
            .lookup(current_token)
            .cloned()
            .unwrap_or_else(Trie::new);

        let mut agenda = self.items.value.clone().unwrap_or_default();

        let mut clone_self = self.clone();
        clone_self.process(
            &mut agenda,
            |_| None,
            |tokens, item| {
                if current_token.is_empty()
                    || tokens
                        .first()
                        .is_some_and(|t| t.starts_with(current_token))
                {
                    let tokens1 = tokens[1..].to_vec();
                    acc.insert_chain1(&tokens1, item);
                }
            },
        );

        CompletionAccumulator { value: acc.value }
    }

    /// Extracts parsed trees.
    ///
    /// Reconstructs abstract trees from the chart after parsing.
    ///
    /// # Returns
    /// Vector of unique parsed trees.
    pub fn extract_trees(&self) -> Vec<Fun> {
        let total_fids = self.concrete.total_fids;
        let forest = &self.chart.forest;

        fn go(
            fid: FId,
            total_fids: FId,
            forest: &HashMap<FId, Vec<Production>>,
        ) -> Vec<Fun> {
            if fid < total_fids {
                vec![Fun::new("?".to_string(), vec![])]
            } else if let Some(rules) = forest.get(&fid) {
                let mut trees = Vec::new();
                for rule in rules {
                    match rule {
                        Production::Const(c) => trees.push(c.lit.clone()),
                        Production::Apply(a) => {
                            let arg_trees: Vec<Vec<Fun>> = a
                                .args
                                .iter()
                                .map(|arg| go(arg.fid, total_fids, forest))
                                .collect();
                            let mut indices = vec![0; a.args.len()];
                            loop {
                                let mut t = Fun::new(a.get_name(), vec![]);
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
                        _ => {}
                    }
                }
                trees
            } else {
                vec![]
            }
        }

        let mut trees = Vec::new();

        if let Some((start, end)) =
            self.concrete.start_cats.get(&self.start_cat)
        {
            for fid0 in *start..=*end {
                let rules = self.chart.expand_forest(fid0);
                let mut labels = vec![];
                for rule in &rules {
                    if let Production::Apply(a) = rule {
                        match a.to_apply_fun() {
                            ApplyFun::CncFun(fun) => {
                                // JSON CncFun has Vec<i32> lins, each one is an index
                                labels.extend(0..fun.lins.len() as i32);
                            }
                            ApplyFun::FId(fun_id) => {
                                if (fun_id as usize)
                                    < self.concrete.functions.len()
                                {
                                    let runtime_fun = &self.concrete.functions
                                        [fun_id as usize];
                                    match &runtime_fun.lins {
                                        LinType::Sym(lins) => {
                                            labels
                                                .extend(0..lins.len() as i32);
                                        }
                                        LinType::FId(indices) => {
                                            labels.extend(
                                                0..indices.len() as i32,
                                            );
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
    ///
    /// # Arguments
    /// * `name` - Function name.
    /// * `lins` - Linearization type.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::{RuntimeCncFun, LinType};
    /// let fun = RuntimeCncFun::new("Test".to_string(), LinType::Sym(vec![]));
    /// assert_eq!(fun.name, "Test");
    /// ```
    pub fn new(name: String, lins: LinType) -> Self {
        RuntimeCncFun { name, lins }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Label production.
///
/// Associates a function with an FId for label-based lookup.
#[derive(Debug, Clone)]
struct LProduction {
    fid: FId,
    fun: RuntimeCncFun,
}

////////////////////////////////////////////////////////////////////////////////
/// Tagged string for linearization.
///
/// Pairs a token with a tag for tracking origins in linearized output.
#[derive(Debug, Clone)]
pub struct TaggedString {
    /// Token.
    pub token: String,
    /// Tag.
    pub tag: String,
}

impl TaggedString {
    /// Creates a new tagged string.
    ///
    /// # Arguments
    /// * `token` - The token.
    /// * `tag` - The tag.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::TaggedString;
    /// let ts = TaggedString::new("hello", "0");
    /// assert_eq!(ts.token, "hello");
    /// assert_eq!(ts.tag, "0");
    /// ```
    pub fn new(token: &str, tag: &str) -> Self {
        TaggedString { token: token.to_string(), tag: tag.to_string() }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Linearized symbol row.
///
/// Represents a row in the linearization table with FId and symbol tables.
#[derive(Debug, Clone)]
pub struct LinearizedSym {
    /// FId.
    pub fid: i32,
    /// Table of symbols.
    pub table: Vec<Vec<Sym>>,
}

////////////////////////////////////////////////////////////////////////////////
/// Active item for parsing.
///
/// Represents an active parsing item in the chart, with position and components.
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
    ///
    /// # Arguments
    /// * `offset` - Offset.
    /// * `dot` - Dot position.
    /// * `fun` - Function.
    /// * `seq` - Sequence.
    /// * `args` - Arguments.
    /// * `fid` - FId.
    /// * `lbl` - Label.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::{ActiveItem, RuntimeCncFun, LinType, PArg, Sym};
    /// let fun = RuntimeCncFun::new("Test".to_string(), LinType::Sym(vec![]));
    /// let item = ActiveItem::new(0, 0, fun, vec![], vec![], 1, 0);
    /// assert_eq!(item.offset, 0);
    /// ```
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
    ///
    /// Compares all fields for equality.
    ///
    /// # Arguments
    /// * `other` - The other item.
    ///
    /// # Returns
    /// True if equal.
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
    ///
    /// Advances the dot and updates argument fid.
    ///
    /// # Arguments
    /// * `i` - Argument index.
    /// * `fid` - New fid.
    ///
    /// # Returns
    /// Updated active item.
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
    ///
    /// Advances the dot position.
    ///
    /// # Returns
    /// Updated active item.
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
///
/// # Arguments
/// * `value` - The optional value.
///
/// # Returns
/// True if None.
///
/// # Examples
///
/// ```
/// use gf_core::is_undefined;
/// assert!(is_undefined::<i32>(&None));
/// assert!(!is_undefined(&Some(42)));
/// ```
pub fn is_undefined<T>(value: &Option<T>) -> bool {
    value.is_none()
}

////////////////////////////////////////////////////////////////////////////////
/// Maps a hashmap with a function.
///
/// Applies a function to each value in the hashmap.
///
/// # Arguments
/// * `obj` - The hashmap.
/// * `fun` - The function to apply.
///
/// # Returns
/// New hashmap with transformed values.
///
/// # Examples
///
/// ```
/// use gf_core::map_object;
/// use std::collections::HashMap;
/// let mut map: HashMap<String, i32> = HashMap::new();
/// map.insert("a".to_string(), 1);
/// let new_map = map_object(&map, |&v| v * 2);
/// assert_eq!(new_map.get("a"), Some(&2));
/// ```
pub fn map_object<K: Eq + std::hash::Hash + Clone, V, F: Fn(&V) -> U, U>(
    obj: &HashMap<K, V>,
    fun: F,
) -> HashMap<K, U> {
    obj.iter().map(|(k, v)| (k.clone(), fun(v))).collect()
}

impl Type {
    /// Creates a new type.
    ///
    /// # Arguments
    /// * `args` - Argument categories.
    /// * `cat` - Result category.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::Type;
    /// let typ = Type::new(vec!["A".to_string()], "B".to_string());
    /// assert_eq!(typ.args, vec!["A".to_string()]);
    /// assert_eq!(typ.cat, "B");
    /// ```
    pub fn new(args: Vec<String>, cat: String) -> Self {
        Type { args, cat }
    }
}

impl Apply {
    /// Shows the apply rule as string.
    ///
    /// # Arguments
    /// * `cat` - Category.
    ///
    /// # Returns
    /// String representation.
    pub fn show(&self, cat: &str) -> String {
        format!("{} -> {} [{:?}]", cat, self.get_name(), self.args)
    }

    /// Checks equality with another apply.
    ///
    /// # Arguments
    /// * `obj` - Other Apply.
    ///
    /// # Returns
    /// True if equal.
    pub fn is_equal(&self, obj: &Apply) -> bool {
        self.fun == obj.fun && self.args == obj.args
    }
}

impl Coerce {
    /// Shows the coerce rule as string.
    ///
    /// # Arguments
    /// * `cat` - Category.
    ///
    /// # Returns
    /// String representation.
    pub fn show(&self, cat: &str) -> String {
        format!("{} -> _ [{}]", cat, self.arg)
    }
}

impl PArg {
    /// Creates a new PArg.
    ///
    /// # Arguments
    /// * `type_` - Type.
    /// * `hypos` - Hypos.
    /// * `fid` - FId.
    ///
    /// # Examples
    ///
    /// ```
    /// use gf_core::PArg;
    /// let parg = PArg::new("Type".to_string(), vec![1], 2);
    /// assert_eq!(parg.type_, "Type");
    /// ```
    pub fn new(type_: String, hypos: Vec<FId>, fid: FId) -> Self {
        PArg { type_, hypos, fid }
    }
}

impl Const {
    /// Shows the const rule as string.
    ///
    /// # Arguments
    /// * `cat` - Category.
    ///
    /// # Returns
    /// String representation.
    pub fn show(&self, cat: &str) -> String {
        format!("{} -> {}", cat, self.lit.print())
    }

    /// Checks equality with another const.
    ///
    /// # Arguments
    /// * `obj` - Other Const.
    ///
    /// # Returns
    /// True if equal.
    pub fn is_equal(&self, obj: &Const) -> bool {
        self.lit.is_equal(&obj.lit) && self.toks == obj.toks
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

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
        abstract_.add_type(
            "MakeS".to_string(),
            vec!["NP".to_string(), "VP".to_string()],
            "S".to_string(),
        );
        assert_eq!(abstract_.get_cat("MakeS"), Some(&"S".to_string()));
        assert_eq!(
            abstract_.get_args("MakeS"),
            Some(&vec!["NP".to_string(), "VP".to_string()])
        );
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
        let tree1 = Fun::new(
            "Test".to_string(),
            vec![Fun::new("Arg1".to_string(), vec![])],
        );
        let tree2 = Fun::new(
            "Test".to_string(),
            vec![Fun::new("Arg1".to_string(), vec![])],
        );
        assert!(tree1.is_equal(&tree2));
        let tree3 = Fun::new(
            "Test".to_string(),
            vec![Fun::new("Arg2".to_string(), vec![])],
        );
        assert!(!tree1.is_equal(&tree3));
    }

    // Port of TypeScript tests from tests/index.spec.ts

    /// Test importing from JSON (equivalent to TypeScript "imports from JSON" test).
    #[test]
    fn test_import_from_json() {
        let json_content = fs::read_to_string("tests/grammars/Zero.json")
            .expect("Failed to read Zero.json");
        let json: serde_json::Value =
            serde_json::from_str(&json_content).expect("Failed to parse JSON");
        let pgf: PGF =
            serde_json::from_value(json).expect("Failed to deserialize PGF");
        let grammar = GFGrammar::from_json(pgf);

        // Verify grammar was created successfully
        assert_eq!(grammar.abstract_grammar.startcat, "Utt");
        assert!(grammar.concretes.contains_key("ZeroEng"));
        assert!(grammar.concretes.contains_key("ZeroSwe"));
    }

    /// Test parsing tree (equivalent to TypeScript "parses tree" test).
    #[test]
    fn test_parse_tree_from_grammar() {
        let json_content = fs::read_to_string("tests/grammars/Zero.json")
            .expect("Failed to read Zero.json");
        let json: serde_json::Value =
            serde_json::from_str(&json_content).expect("Failed to parse JSON");
        let pgf: PGF =
            serde_json::from_value(json).expect("Failed to deserialize PGF");
        let grammar = GFGrammar::from_json(pgf);

        let tree = grammar.abstract_grammar.parse_tree("eat apple", None);
        assert!(tree.is_some(), "Should be able to parse 'eat apple'");
        let tree = tree.unwrap();
        assert_eq!(tree.name, "eat");
        assert_eq!(tree.args.len(), 1);
        assert_eq!(tree.args[0].name, "apple");
    }

    /// Test English linearization (equivalent to TypeScript "linearises in English" test).
    #[test]
    fn test_linearize_english() {
        let json_content = fs::read_to_string("tests/grammars/Zero.json")
            .expect("Failed to read Zero.json");
        let json: serde_json::Value =
            serde_json::from_str(&json_content).expect("Failed to parse JSON");
        let pgf: PGF =
            serde_json::from_value(json).expect("Failed to deserialize PGF");
        let grammar = GFGrammar::from_json(pgf);

        let tree = grammar
            .abstract_grammar
            .parse_tree("eat apple", None)
            .expect("Failed to parse tree");
        let linearized = grammar.concretes["ZeroEng"].linearize(&tree);
        assert_eq!(linearized, "eat an apple");
    }

    /// Test Swedish linearization (equivalent to TypeScript "linearises in Swedish" test).
    #[test]
    fn test_linearize_swedish() {
        let json_content = fs::read_to_string("tests/grammars/Zero.json")
            .expect("Failed to read Zero.json");
        let json: serde_json::Value =
            serde_json::from_str(&json_content).expect("Failed to parse JSON");
        let pgf: PGF =
            serde_json::from_value(json).expect("Failed to deserialize PGF");
        let grammar = GFGrammar::from_json(pgf);

        let tree = grammar
            .abstract_grammar
            .parse_tree("eat apple", None)
            .expect("Failed to parse tree");
        let linearized = grammar.concretes["ZeroSwe"].linearize(&tree);
        assert_eq!(linearized, "äta ett äpple");
    }
}
