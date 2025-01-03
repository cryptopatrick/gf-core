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
                    let fun = &functions[apply_rule.fun];
                    let register = |args: &[PArg], key: String, i: usize, lproductions: &mut HashMap<String, Vec<LProduction>>| {
                        if i < args.len() {
                            let arg = args[i].fid as usize;
                            let mut count = 0;

                            if let Some(rules) = productions.get(&arg ) {
                                for rule in rules {
                                    if let Production::Coerce(ref coerce_rule) = rule {
                                        let new_key = format!("{}_{}", key, coerce_rule.arg);
                                        register(args, new_key, i + 1, lproductions);
                                        count += 1;
                                    }
                                }
                            }

                            if count == 0 {
                                let new_key = format!("{}_{}", key, arg);
                                register(args, new_key, i + 1, lproductions);
                            }
                        } else {
                            lproductions.entry(key).or_insert_with(Vec::new).push(LProduction {
                                fun: fun.clone(),
                                fid: fid.try_into().unwrap(),
                            });
                        }
                    };

                    register(&apply_rule.args, fun.name.clone(), 0, &mut lproductions);
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
            .map(|f| CncFun {
                name: f.name,
                lins: f.lins,
            })
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
            res.push(LinearizedSym {
                fid: -1,
                table: vec![vec![sym]],
            });
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
            res.push(LinearizedSym {
                fid: -2,
                table: vec![vec![sym]],
            });
        }
        // FLOAT
        else if tree.is_float() {
            let sym = SymKS::new(vec![tree.name]);
            sym.tag = Some(tag.to_string());
            res.push(LinearizedSym {
                fid: -3,
                table: vec![vec![sym]],
            });
        }
        // META
        else if tree.is_meta() {
            let cat = self.start_cats.get(tree.type_.as_ref().unwrap()).unwrap();
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
                let mut subs = self.linearize_syms(arg, &format!("{}-{}", tag, i));
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
                                    toks.push(Sym::KS(ks.clone().tag_with(tag)));
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
        let no_space_before = regex::Regex::new(r"^[\.\,\?\!\)\:\;\-\]]").unwrap();

        let mut s = String::new();
        let mut s = String::new();

        for i in 0..ts.len() {
            let t = &ts[i].token;
            let after = if i + 1 < ts.len() {
                Some(&ts[i + 1].token)
            } else {
                None
            };

            s.push_str(t);

            if let Some(after) = after {
                if !no_space_after.is_match(t) && !no_space_before.is_match(after) {
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

        CompletionResult {
            consumed: tokens,
            suggestions,
        }
    }
}
