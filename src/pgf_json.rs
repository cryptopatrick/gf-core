use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug)]
pub struct PGF {
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

#[derive(Serialize, Deserialize, Debug)]
pub enum Production {
    // TODO: look for a cleaner solution than wrapping.
    Apply(Apply),
    Coerce(Coerce),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Apply {
    pub type_: String,
    pub fid: i32,
    pub args: Vec<PArg>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Coerce {
    pub type_: String,
    pub arg: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PArg {
    pub type_: String,
    pub hypos: Vec<i32>,
    pub fid: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CncFun {
    pub name: String,
    pub lins: Vec<i32>,
}

#[derive(serde::Serialize, Deserialize, Clone, Debug)]
pub struct Sym {
    pub type_: String,
    pub args: Vec<SymArg>,
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
