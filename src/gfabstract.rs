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
/// 
// TODO: add documentation.    
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

        GFAbstract {
            startcat: json.startcat,
            types,
        }
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
                for (arg, expected_type) in tree.args.iter_mut().zip(&typ.args) {
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
    pub fn parse_tree(&self, str: &str, r#type: Option<&String>) -> Option<Fun> {
        let tokens: Vec<&str> = str
            .split_whitespace()
            .flat_map(|s| s.split(|c| "()?:".contains(c)))
            .collect();

        if let Some(tree) = self.parse_tree_internal(&mut tokens.into_iter(), 0) {
            Some(self.annotate(tree, r#type))
        } else {
            None
        }
    }

    // Internal recursive tree parser
    fn parse_tree_internal<'a, I>(&self, tokens: &mut I, _prec: usize) -> Option<Fun>
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
