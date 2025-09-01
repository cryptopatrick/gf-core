use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Fun {
    pub name: String,
    pub args: Vec<Fun>,
    pub type_: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct PGF {
    #[serde(rename = "abstract")]
    pub abstract_: Abstract,
    pub concretes: HashMap<String, Concrete>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Abstract {
    pub name: String,
    pub startcat: String,
    pub funs: HashMap<String, AbsFun>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Concrete {
    pub flags: HashMap<String, String>,
    pub productions: HashMap<i32, Vec<Production>>,
    pub functions: Vec<CncFun>,
    pub sequences: Vec<Vec<Sym>>,
    pub categories: HashMap<String, Category>,
    pub totalfids: i32,
}
/// The Abstract function name for a constituent in a parse tree.
#[derive(Serialize, Deserialize, Debug)]
pub struct AbsFun {
    pub args: Vec<String>,
    pub cat: String,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum Production {
    Apply(Apply),
    Coerce(Coerce),
    Const(Const),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Apply {
    #[serde(default)]
    pub fid: Option<i32>,
    #[serde(default)]  
    pub fun: Option<CncFun>,
    pub args: Vec<PArg>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ApplyFun {
    FId(i32),
    CncFun(CncFun),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Const {
    pub lit: Fun,
    pub toks: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Coerce {
    pub arg: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PArg {
    #[serde(rename = "type")]
    pub type_: String,
    pub hypos: Vec<i32>,
    pub fid: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct CncFun {
    pub name: String,
    pub lins: Vec<i32>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum LinType {
    FId(Vec<i32>),
    Sym(Vec<Vec<Sym>>),
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub enum Sym {
    SymCat { i: usize, label: usize },
    SymLit { i: usize, label: usize },
    SymKS(SymKS),
    SymKP(SymKP),
}

// Custom deserializer for Sym
impl<'de> Deserialize<'de> for Sym {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde_json::Value;
        let value: Value = Deserialize::deserialize(deserializer)?;
        
        let type_str = value["type"].as_str().ok_or_else(|| {
            serde::de::Error::missing_field("type")
        })?;
        
        match type_str {
            "SymCat" => {
                let args = value["args"].as_array().ok_or_else(|| {
                    serde::de::Error::missing_field("args")
                })?;
                if args.len() != 2 {
                    return Err(serde::de::Error::custom("SymCat args must have 2 elements"));
                }
                let i = args[0].as_u64().unwrap_or(0) as usize;
                let label = args[1].as_u64().unwrap_or(0) as usize;
                Ok(Sym::SymCat { i, label })
            }
            "SymLit" => {
                let args = value["args"].as_array().ok_or_else(|| {
                    serde::de::Error::missing_field("args")
                })?;
                if args.len() != 2 {
                    return Err(serde::de::Error::custom("SymLit args must have 2 elements"));
                }
                let i = args[0].as_u64().unwrap_or(0) as usize;
                let label = args[1].as_u64().unwrap_or(0) as usize;
                Ok(Sym::SymLit { i, label })
            }
            "SymKS" => {
                let args = value["args"].as_array().ok_or_else(|| {
                    serde::de::Error::missing_field("args")
                })?;
                let tokens: Vec<String> = args.iter()
                    .map(|v| v.as_str().unwrap_or("").to_string())
                    .collect();
                Ok(Sym::SymKS(SymKS::new(tokens)))
            }
            "SymKP" => {
                // SymKP is more complex - it has nested structure
                let args = value["args"].as_array().ok_or_else(|| {
                    serde::de::Error::missing_field("args")
                })?;
                
                let mut tokens = Vec::new();
                let mut alts = Vec::new();
                
                // Parse the nested structure
                for arg_group in args {
                    if let Some(arr) = arg_group.as_array() {
                        for item in arr {
                            if let Some(obj) = item.as_object() {
                                if obj.get("type").and_then(|v| v.as_str()) == Some("SymKS") {
                                    if let Some(item_args) = obj.get("args").and_then(|v| v.as_array()) {
                                        let item_tokens: Vec<String> = item_args.iter()
                                            .map(|v| v.as_str().unwrap_or("").to_string())
                                            .collect();
                                        tokens.push(SymKS::new(item_tokens));
                                    }
                                } else if obj.get("type").and_then(|v| v.as_str()) == Some("Alt") {
                                    if let Some(alt_args) = obj.get("args").and_then(|v| v.as_array()) {
                                        if alt_args.len() >= 2 {
                                            // First element is tokens array, second is prefixes
                                            let mut alt_tokens = Vec::new();
                                            if let Some(tokens_arr) = alt_args[0].as_array() {
                                                for token_item in tokens_arr {
                                                    if let Some(token_obj) = token_item.as_object() {
                                                        if let Some(token_args) = token_obj.get("args").and_then(|v| v.as_array()) {
                                                            let item_tokens: Vec<String> = token_args.iter()
                                                                .map(|v| v.as_str().unwrap_or("").to_string())
                                                                .collect();
                                                            alt_tokens.push(SymKS::new(item_tokens));
                                                        }
                                                    }
                                                }
                                            }
                                            let prefixes: Vec<String> = alt_args[1].as_array()
                                                .map(|arr| arr.iter().map(|v| v.as_str().unwrap_or("").to_string()).collect())
                                                .unwrap_or_default();
                                            alts.push(Alt::new(alt_tokens, prefixes));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                
                Ok(Sym::SymKP(SymKP::new(tokens, alts)))
            }
            _ => Err(serde::de::Error::custom(format!("Unknown symbol type: {type_str}")))
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SymKS {
    pub id: String,
    pub tokens: Vec<String>,
    pub tag: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SymKP {
    pub id: String,
    pub tokens: Vec<SymKS>,
    pub alts: Vec<Alt>,
    pub tag: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Alt {
    pub tokens: Vec<SymKS>,
    pub prefixes: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum SymArg {
    Number(i32),
    Text(String),
    TextVec(Vec<String>),
    SymVec(Vec<Sym>),
}

/// A syntactic constituent category in the a parse tree of a sentence.
#[derive(Serialize, Deserialize, Debug)]
pub struct Category {
    pub start: i32,
    pub end: i32,
}

impl Fun {
    /// Creates a new function tree.
    pub fn new(name: String, args: Vec<Fun>) -> Self {
        Fun { name, args, type_: None }
    }

    /// Prints the tree as a string.
    pub fn print(&self) -> String {
        self.show(0)
    }

    /// Shows the tree with precedence.
    fn show(&self, prec: usize) -> String {
        if self.is_meta() {
            if let Some(ref t) = self.type_ {
                let mut s = format!("?:{t}");
                if prec > 0 {
                    s = format!("({s})");
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
                s = format!("({s})");
            }
            s
        }
    }

    /// Gets argument by index.
    pub fn get_arg(&self, i: usize) -> Option<&Fun> {
        self.args.get(i)
    }

    /// Sets argument by index.
    pub fn set_arg(&mut self, i: usize, c: Fun) {
        if i < self.args.len() {
            self.args[i] = c;
        }
    }

    /// Checks if this is a meta variable.
    pub fn is_meta(&self) -> bool {
        self.name == "?"
    }

    /// Checks if the tree is complete (no metas).
    pub fn is_complete(&self) -> bool {
        if self.is_meta() {
            false
        } else {
            self.args.iter().all(|arg| arg.is_complete())
        }
    }

    /// Checks if this is a literal.
    pub fn is_literal(&self) -> bool {
        self.name.starts_with('"') || self.name.starts_with('-') || self.name.chars().next().is_some_and(|c| c.is_ascii_digit())
    }

    /// Checks if this is a string literal.
    pub fn is_string(&self) -> bool {
        self.name.starts_with('"') && self.name.ends_with('"')
    }

    /// Checks if this is an integer literal.
    pub fn is_int(&self) -> bool {
        self.name.parse::<i32>().is_ok()
    }

    /// Checks if this is a float literal.
    pub fn is_float(&self) -> bool {
        self.name.parse::<f64>().is_ok() && self.name != "." && self.name != "-."
    }

    /// Checks equality with another tree.
    pub fn is_equal(&self, other: &Fun) -> bool {
        if self.name != other.name || self.args.len() != other.args.len() {
            return false;
        }
        self.args.iter().zip(&other.args).all(|(a, b)| a.is_equal(b))
    }
}

impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print())
    }
}

impl Apply {
    /// Creates a new apply rule.
    pub fn new(fun: ApplyFun, args: Vec<PArg>) -> Self {
        match fun {
            ApplyFun::FId(id) => Apply { fid: Some(id), fun: None, args },
            ApplyFun::CncFun(cnc_fun) => Apply { fid: None, fun: Some(cnc_fun), args },
        }
    }

    /// Converts to runtime ApplyFun.
    pub fn to_apply_fun(&self) -> ApplyFun {
        if let Some(fid) = self.fid {
            ApplyFun::FId(fid)
        } else if let Some(ref fun) = self.fun {
            ApplyFun::CncFun(fun.clone())
        } else {
            // This should not happen in well-formed data
            ApplyFun::FId(-1) // Use sentinel value instead of panic
        }
    }

    /// Gets the name of the function for debugging.
    pub fn get_name(&self) -> String {
        self.to_apply_fun().get_name()
    }
}

impl ApplyFun {
    /// Gets the name of the function.
    pub fn get_name(&self) -> String {
        match self {
            ApplyFun::FId(id) => id.to_string(),
            ApplyFun::CncFun(fun) => fun.name.clone(),
        }
    }

    /// Gets the ID if FId.
    pub fn get_id(&self) -> i32 {
        match self {
            ApplyFun::FId(id) => *id,
            ApplyFun::CncFun(_) => -1, // CncFun doesn't have an ID, return sentinel
        }
    }
}

impl Const {
    /// Creates a new const rule.
    pub fn new(lit: Fun, toks: Vec<String>) -> Self {
        Const { lit, toks }
    }
}

impl Coerce {
    /// Creates a new coerce rule.
    pub fn new(arg: i32) -> Self {
        Coerce { arg }
    }
}

impl SymKS {
    /// Creates a new SymKS.
    pub fn new(tokens: Vec<String>) -> Self {
        SymKS { id: "KS".to_string(), tokens, tag: None }
    }

    /// Shows as string.
    pub fn show(&self) -> String {
        format!("\"{:?}\"", self.tokens)
    }

    /// Tags the symbol.
    pub fn tag_with(&self, tag: &str) -> SymKS {
        SymKS { id: self.id.clone(), tokens: self.tokens.clone(), tag: Some(tag.to_string()) }
    }
}

impl SymKP {
    /// Creates a new SymKP.
    pub fn new(tokens: Vec<SymKS>, alts: Vec<Alt>) -> Self {
        SymKP { id: "KP".to_string(), tokens, alts, tag: None }
    }

    /// Shows as string.
    pub fn show(&self) -> String {
        format!("\"{:?}\"", self.tokens)
    }

    /// Tags the phrase.
    pub fn tag_with(&self, tag: &str) -> SymKP {
        SymKP { id: self.id.clone(), tokens: self.tokens.clone(), alts: self.alts.clone(), tag: Some(tag.to_string()) }
    }
}

impl Alt {
    /// Creates a new Alt.
    pub fn new(tokens: Vec<SymKS>, prefixes: Vec<String>) -> Self {
        Alt { tokens, prefixes }
    }
}

impl CncFun {
    /// Creates a new CncFun.
    pub fn new(name: String, lins: Vec<i32>) -> Self {
        CncFun { name, lins }
    }
}
