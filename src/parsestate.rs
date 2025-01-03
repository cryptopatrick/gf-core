///////////////////////////////////////////////////////////////////////////////
pub struct ParseState {
    concrete: GFConcrete,
    start_cat: String,
    items: Trie<ActiveItem>,
    chart: Chart,
}

impl ParseState {
    //The process function is responsible for iterating over items and applying the provided closures:
    pub fn process<F, G>(&self, value: Option<&Vec<ActiveItem>>, handle_fid: F, handle_item: G)
    where
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

        ParseState {
            concrete,
            start_cat,
            items,
            chart,
        }
    }

    pub fn next(&mut self, token: &str) -> bool {
        let mut acc = self.items.lookup(token).cloned().unwrap_or_else(Trie::new);

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
                    if token == "0" || token.parse::<i32>().ok().filter(|x| *x != 0).is_some() {
                        Some(Const::new(Fun::new(token), vec![token.to_string()]))
                    } else {
                        None
                    }
                }
                // Float
                -3 => {
                    if token == "0"
                        || token == "0.0"
                        || token.parse::<f64>().ok().filter(|x| *x != 0.0).is_some()
                    {
                        Some(Const::new(Fun::new(token, vec![]), vec![token.to_string()]))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            |tokens, item| {
                if tokens.get(0).map_or(false, |t| t == token) {
                    let tokens1: Vec<String> = tokens.iter().skip(1).cloned().collect();
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
                    let tokens1: Vec<String> = tokens.iter().skip(1).cloned().collect();
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
        fn go(fid: FId, total_fids: FId, forest: &HashMap<FId, Vec<Production>>) -> Vec<Fun> {
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
                                let mut arg_indices = vec![0; apply_rule.args.len()];
                                let mut arg_trees: Vec<Vec<Fun>> = apply_rule
                                    .args
                                    .iter()
                                    .map(|arg| go(arg.fid, total_fids, forest))
                                    .collect();

                                loop {
                                    let mut t = Fun::new(&apply_rule.fun.get_name(), vec![]);
                                    for (k, arg_trees_k) in arg_trees.iter().enumerate() {
                                        t.set_arg(k, arg_trees_k[arg_indices[k]].clone());
                                    }
                                    trees.push(t);

                                    let mut i = 0;
                                    while i < arg_trees.len() {
                                        arg_indices[i] += 1;
                                        if arg_indices[i] < arg_trees[i].len() {
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
                    let fid = self.chart.lookup_pc(fid0 as i32, *lbl as i32, 0);
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

                            if let Some(items) = self.chart.lookup_ac(fid, label) {
                                if !items
                                    .iter()
                                    .any(|existing_item| existing_item.is_equal(&item))
                                {
                                    items.push(item.clone());
                                    if let Some(fid2) =
                                        self.chart.lookup_pc(fid, label, self.chart.offset)
                                    {
                                        agenda.push(item.shift_over_arg(sym.i, fid2));
                                    }
                                }
                            } else {
                                if let Some(rules) = self.chart.expand_forest(fid) {
                                    for rule in rules {
                                        if let Production::Apply(apply_rule) = rule {
                                            agenda.push(ActiveItem::new(
                                                self.chart.offset,
                                                0,
                                                apply_rule.fun.clone(),
                                                apply_rule.fun.lins[label].clone(),
                                                apply_rule.args.clone(),
                                                fid,
                                                label,
                                            ));
                                        }
                                    }
                                }
                                self.chart.insert_ac(fid, label, vec![item.clone()]);
                            }
                        }
                        Sym::SymKS(sym) => {
                            token_callback(&sym.tokens, item.shift_over_token());
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
                        Sym::SymLit(sym) => {
                            let mut fid = item.args[sym.i].fid;

                            if let Some(rules) = self.chart.forest.get(&fid) {
                                if let Some(Production::Const(const_rule)) = rules.get(0) {
                                    token_callback(&const_rule.toks, item.shift_over_token());
                                }
                            } else if let Some(rule) = literal_callback(fid) {
                                fid = self.chart.next_id;
                                self.chart.next_id += 1;
                                self.chart
                                    .forest
                                    .insert(fid, vec![Production::Const(rule.clone())]);
                                token_callback(&rule.toks, item.shift_over_arg(sym.i, fid));
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

                            if let Some(items) =
                                self.chart.lookup_aco(item.offset, item.fid, item.lbl)
                            {
                                for pitem in items {
                                    if let Sym::SymCat(sym_cat) = &pitem.seq[pitem.dot] {
                                        agenda.push(pitem.shift_over_arg(sym_cat.i, new_fid));
                                    }
                                }
                            }

                            self.chart
                                .insert_pc(item.fid, item.lbl, item.offset, new_fid);
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
                        let rule = Apply::new(item.fun.clone(), item.args.clone());

                        if !rules.iter().any(|existing_rule| {
                            if let Production::Apply(existing_apply) = existing_rule {
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
