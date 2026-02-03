use std::{collections::{BTreeMap, BTreeSet, HashMap, HashSet}, marker::PhantomData};
use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::hash::Hash;

pub trait GrammarTypes {
    type Terminal: Debug + Copy + Eq + Hash + Display;
    type Rule: Debug + Copy + Eq + Hash + Display;
    type ExternalRuleType: Debug + Clone + Eq;
    type ExternalFunction: Debug + Clone + Eq;
    type ExternalLexMode: Debug + Clone + Eq;
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol {
    symbol_type: SymbolType,
    with_location: bool,
    discard: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SymbolType {
    Terminal(usize),
    NonTerminal(usize),
}



pub struct Rule<G: GrammarTypes> {
    symbols: Vec<Symbol>,
    value: RuleValue<G>,
}

impl <G: GrammarTypes> Rule<G> {
    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }

    pub fn value(&self) -> &RuleValue<G> {
        &self.value
    }
}


pub enum RuleType<G: GrammarTypes> {
    ExternalType(G::ExternalRuleType),
    TerminalType(usize),
    Function(Box<RuleType<G>>, Box<RuleType<G>>),
    Tuple(Vec<RuleType<G>>),
    WithLocation(Box<RuleType<G>>),
}

impl <G: GrammarTypes> Clone for RuleType<G> {
    fn clone(&self) -> Self {
        match self {
            RuleType::ExternalType(et) => RuleType::ExternalType(et.clone()),
            RuleType::TerminalType(tt) => RuleType::TerminalType(*tt),
            RuleType::Function(a, b) => RuleType::Function(a.clone(), b.clone()),
            RuleType::Tuple(items) => RuleType::Tuple(items.clone()),
            RuleType::WithLocation(inner) => RuleType::WithLocation(inner.clone()),
        }
    }
}

pub enum RuleValue<G: GrammarTypes> {
    SymbolValue(usize),
    ExternalFunction(G::ExternalFunction, Vec<RuleValue<G>>),
    MakeTuple(Vec<RuleValue<G>>),
    GetTuple(Box<RuleValue<G>>, usize),
    DropLocation(Box<RuleValue<G>>),
    BuildLocation {
        first: Box<RuleValue<G>>,
        second: Box<RuleValue<G>>,
        value: Box<RuleValue<G>>,
    },
    Lambda {
        discard_param: bool,
        param_type: RuleType<G>,
        body: Box<RuleValue<G>>,
    },

    // f, a
    Apply(Box<RuleValue<G>>, Box<RuleValue<G>>),
}

impl <G: GrammarTypes> RuleValue<G> {
    fn substitute_indexes(&mut self, f: &impl Fn(&mut usize)) {
        self.substitute_indexes_with(&|mut i| {
            f(&mut i);
            RuleValue::SymbolValue(i)
        });
    }
    
    fn substitute_indexes_with(&mut self, f: &impl Fn(usize) -> Self) {
        match self {
            RuleValue::SymbolValue(i) => *self = f(*i),
            RuleValue::ExternalFunction(_, args) => {
                for arg in args {
                    arg.substitute_indexes_with(f);
                }
            },
            
            RuleValue::MakeTuple(items) => {
                for item in items {
                    item.substitute_indexes_with(f);
                }
            },

            RuleValue::GetTuple(tuple_value, _) => tuple_value.substitute_indexes_with(f),

            RuleValue::DropLocation(value) => value.substitute_indexes_with(f),

            RuleValue::BuildLocation { first, second, value } => {
                first.substitute_indexes_with(f);
                second.substitute_indexes_with(f);
                value.substitute_indexes_with(f);
            }

            RuleValue::Lambda { body,  .. } => body.substitute_indexes_with(f),
            RuleValue::Apply(func, a) => {
                func.substitute_indexes_with(f);
                a.substitute_indexes_with(f);
            },
        }
    }
}


pub struct RuleSet<G: GrammarTypes> {
    rule_type: RuleType<G>,
    name: String,
    offshoot_index: usize,
    rules: Vec<Rule<G>>,
    lex_mode: Option<G::ExternalLexMode>,
}

impl <G: GrammarTypes> RuleSet<G> {
    pub fn rule_type(&self) -> &RuleType<G> {
        &self.rule_type
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn offshoot_index(&self) -> usize {
        self.offshoot_index
    }

    pub fn rules(&self) -> &[Rule<G>] {
        &self.rules
    }
}

pub struct Grammar<G: GrammarTypes> {
    terminals: Vec<G::Terminal>,
    rulesets: Vec<RuleSet<G>>,
}

impl <G: GrammarTypes> Grammar<G> {
    pub fn make_raw(f: impl GrammarBuilderCallback<G>) -> Grammar<G> {
        let mut builder = GrammarBuilder {
            grammar: Grammar {
                terminals: Vec::new(),
                rulesets: Vec::new(),
            },
            dummy: PhantomData::<()>,
        };

        f.accept(&mut builder);

        let mut grammar = builder.grammar;

        elim_left_rec(&mut grammar);
        left_factor(&mut grammar);

        grammar
    }

    pub fn make(factory: G) -> Self where G: GrammarFactory {

        struct Callback<G> {
            factory: G,
        }

        impl <G: GrammarFactory> GrammarBuilderCallback<G> for Callback<G> {
            fn accept<X>(self, builder: &mut GrammarBuilder<G, X>) {
                let mut rule_map = HashMap::new();
                let mut token_map: HashMap<G::Terminal, SymbolBuilder<X>> = HashMap::new();
                let mut rule_queue = VecDeque::new();
                let mut processed_rules = HashSet::new();

                fn get_non_terminal<G: GrammarFactory, X>(
                    factory: &G,
                    builder: &mut GrammarBuilder<G, X>,
                    rule_map: &mut HashMap<G::Rule, NonTerminalBuilder<X>>,
                    rule: G::Rule,
                ) -> NonTerminalBuilder<X> {
                    *rule_map.entry(rule).or_insert_with(|| {
                        let ruleset = factory.create_rule_set(rule);
                        builder.add_non_terminal(
                            rule.to_string(),
                            ruleset.rule_type,
                            ruleset.lex_mode,
                        )
                    })
                }
                fn get_terminal<G: GrammarFactory, X>(
                    builder: &mut GrammarBuilder<G, X>,
                    token_map: &mut HashMap<G::Terminal, SymbolBuilder<X>>,
                    token: G::Terminal,
                ) -> SymbolBuilder<X> {
                    *token_map.entry(token).or_insert_with(|| builder.add_terminal(
                        token,
                    ))
                }

                rule_queue.push_back(self.factory.start_rule());


                while let Some(rule) = rule_queue.pop_front() {
                    if !processed_rules.insert(rule) {
                        continue
                    }

                    let ruleset = self.factory.create_rule_set(rule);
                    let non_term = get_non_terminal(&self.factory, builder, &mut rule_map, rule);

                    for rule in ruleset.rules {
                        let symbols = rule.symbols.iter()
                            .map(|sym| {
                                let mut sb = match sym.symbol_type {
                                    SymbolInfoType::Terminal(t) =>
                                        get_terminal::<G, X>(builder, &mut token_map, t),

                                    SymbolInfoType::NonTerminal(nt) => {
                                        rule_queue.push_back(nt);
                                        get_non_terminal(&self.factory, builder, &mut rule_map, nt).sym()
                                    },
                                };

                                if sym.with_location {
                                    sb = sb.with_location();
                                }

                                if sym.discard {
                                    sb = sb.discard();
                                }

                                sb
                            })
                            .collect::<Vec<_>>();

                        builder.add_rule(non_term, rule.function.clone(), symbols);
                    }
                }
            }
        }

        Grammar::<G>::make_raw(Callback { factory })
    }

    pub fn table<'a>(&'a self) -> LLTable<'a, G> {
        let firsts = build_first(self);
        let follows = build_follow(self, &firsts);
        let table = build_table(self, &firsts, &follows);

        LLTable {
            grammar: self,
            table,
        }
    }

    pub fn terminals(&self) -> &[G::Terminal] {
        &self.terminals
    }

    pub fn rulesets(&self) -> &[RuleSet<G>] {
        &self.rulesets
    }
}



pub trait GrammarFactory: GrammarTypes + Clone {

    fn start_rule(&self) -> Self::Rule;

    fn create_rule_set(&self, rule: Self::Rule) -> RuleSetInfo<Self>;
}

pub struct RuleSetInfo<G: GrammarTypes> {
    rule_type: G::ExternalRuleType,
    rules: Vec<RuleInfo<G>>,
    lex_mode: Option<G::ExternalLexMode>,
}

impl <G: GrammarTypes> RuleSetInfo<G> {
    pub fn lex_mode(mut self, mode: impl Into<G::ExternalLexMode>) -> Self {
        self.lex_mode = Some(mode.into());
        self
    }
}

pub struct RuleInfo<G: GrammarTypes> {
    symbols: Vec<SymbolInfo<G>>,
    function: G::ExternalFunction,
}

pub struct SymbolInfo<G: GrammarTypes> {
    symbol_type: SymbolInfoType<G>,
    with_location: bool,
    discard: bool,
}

impl <G: GrammarTypes> SymbolInfo<G> {
    pub fn with_location(mut self) -> Self {
        self.with_location = true;
        self
    }

    pub fn discard(mut self) -> Self {
        self.discard = true;
        self
    }
}

enum SymbolInfoType<G: GrammarTypes> {
    Terminal(G::Terminal),
    NonTerminal(G::Rule),
}


pub fn ruleset<G: GrammarTypes>(rule_type: impl Into<G::ExternalRuleType>, rules: impl Into<Vec<RuleInfo<G>>>) -> RuleSetInfo<G> {
    RuleSetInfo {
        rule_type: rule_type.into(),
        rules: rules.into(),
        lex_mode: None,
    }
}

pub fn rule<G: GrammarTypes>(symbols: impl Into<Vec<SymbolInfo<G>>>, function: impl Into<G::ExternalFunction>) -> RuleInfo<G> {
    RuleInfo {
        symbols: symbols.into(),
        function: function.into(),
    }
}

pub fn term<G: GrammarTypes>(terminal: G::Terminal) -> SymbolInfo<G> {
    SymbolInfo {
        symbol_type: SymbolInfoType::Terminal(terminal),
        with_location: false,
        discard: false,
    }
}

pub fn nonterm<G: GrammarTypes>(rule: G::Rule) -> SymbolInfo<G> {
    SymbolInfo {
        symbol_type: SymbolInfoType::NonTerminal(rule),
        with_location: false,
        discard: false,
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FirstElem {
    Terminal(usize),
    Epsilon,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FollowElem {
    Terminal(usize),
    Epsilon,
    EndOfFile,
}


pub struct LLTable<'a, G: GrammarTypes> {
    grammar: &'a Grammar<G>,
    table: Vec<Vec<LLTransition>>,
}

impl <'a, G: GrammarTypes> LLTable<'a, G> {
    pub fn table(&self) -> &Vec<Vec<LLTransition>> {
        &self.table
    }

    pub fn to_ll1(self) -> Result<LL1Table<'a, G>, LL1Error> {
        Ok(LL1Table {
            grammar: self.grammar,
            table: self.table.into_iter()
                .map(|terminals| {
                    let mut rule_map: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();
                    for (terminal, trans) in terminals.into_iter().enumerate() {
                        let mut iter = trans.rules.iter().copied();
                        let e1 = iter.next();
                        let e2 = iter.next();

                        let rule = match (e1, e2) {
                            (Some(e1), None) => e1,
                            (None, _) => continue,
                            _ => return Err(LL1Error {
                                conflicts: trans.conflicts,
                            }),
                        };

                        rule_map.entry(rule).or_default().insert(terminal);
                    }
                    
                    Ok(rule_map)
                })
                .collect::<Result<Vec<_>, LL1Error>>()?,
        })
    }
}

pub struct LL1Table<'a, G: GrammarTypes> {
    grammar: &'a Grammar<G>,
    table: Vec<BTreeMap<usize, BTreeSet<usize>>>,
}

impl <'a, G: GrammarTypes> LL1Table<'a, G> {
    pub fn rulesets<'b>(&'b self) -> impl Iterator<Item=LL1RuleSet<'a, 'b, G>> {
        self.grammar.rulesets.iter()
            .zip(self.table.iter())
            .map(|(ruleset, rule_map)|
                LL1RuleSet {
                    grammar: self.grammar,
                    ruleset,
                    rule_map,
                }
            )
    }
}

pub struct LL1RuleSet<'a, 'b, G: GrammarTypes> {
    grammar: &'a Grammar<G>,

    ruleset: &'a RuleSet<G>,
    rule_map: &'b BTreeMap<usize, BTreeSet<usize>>,
}

impl <'a, 'b, G: GrammarTypes> LL1RuleSet<'a, 'b, G> {
    pub fn rule_type(&self) -> LL1RuleType<'a, G> {
        LL1RuleType::from_rule_type(self.grammar, &self.ruleset.rule_type)
    }

    pub fn rule_name(&self) -> &'a str {
        &self.ruleset.name
    }

    pub fn rule_offshoot_index(&self) -> usize {
        self.ruleset.offshoot_index
    }

    pub fn rules(&self) -> impl Iterator<Item=LL1RuleSetRule<'a, 'b, G>> {
        self.ruleset.rules.iter()
            .enumerate()
            .filter_map(|(rule_index, rule)| {
                Some(LL1RuleSetRule {
                    grammar: self.grammar,
                    rule,
                    terminals: self.rule_map.get(&rule_index)?,
                })
            })
    }
    
    pub fn lex_mode(&self) -> Option<&'a G::ExternalLexMode> {
        self.ruleset.lex_mode.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LL1RuleType<'a, G: GrammarTypes> {
    ExternalType(&'a G::ExternalRuleType),
    TerminalType(&'a G::Terminal),
    Function(Box<LL1RuleType<'a, G>>, Box<LL1RuleType<'a, G>>),
    Tuple(Vec<LL1RuleType<'a, G>>),
    WithLocation(Box<LL1RuleType<'a, G>>),
}

impl <'a, G: GrammarTypes> LL1RuleType<'a, G> {
    fn from_rule_type(grammar: &'a Grammar<G>, t: &'a RuleType<G>) -> Self {
        match t {
            RuleType::ExternalType(et) => LL1RuleType::ExternalType(et),
            RuleType::TerminalType(tt) => LL1RuleType::TerminalType(&grammar.terminals[*tt]),
            RuleType::Function(a, b) =>
                LL1RuleType::Function(
                    Box::new(Self::from_rule_type(grammar, a)), 
                    Box::new(Self::from_rule_type(grammar, b)),
                ),
            RuleType::Tuple(items) =>
                LL1RuleType::Tuple(
                    items.iter().map(|item| Self::from_rule_type(grammar, item)).collect()
                ),
            RuleType::WithLocation(inner) =>
                LL1RuleType::WithLocation(Box::new(Self::from_rule_type(grammar, inner))),
        }
    }
}

pub struct LL1RuleSetRule<'a, 'b, G: GrammarTypes> {
    grammar: &'a Grammar<G>,

    rule: &'a Rule<G>,
    terminals: &'b BTreeSet<usize>,
}

impl <'a, 'b, G: GrammarTypes> LL1RuleSetRule<'a, 'b, G> where 'a : 'b {
    pub fn symbols(&self) -> impl Iterator<Item=LL1Symbol<'a, G>> {
        self.rule.symbols.iter()
            .map(|sym| LL1Symbol::from_symbol(self.grammar, *sym))
    }

    pub fn value(&self) -> LL1RuleValue<'a, G> {
        LL1RuleValue::from_rule_value(&self.grammar, &self.rule.value)
    }

    pub fn terminals(&self) -> impl Iterator<Item=Option<&'a G::Terminal>> + 'b {
        let grammar = self.grammar;
        self.terminals.iter()
            .map(move |i| if *i == grammar.terminals.len() { None } else { Some(&grammar.terminals[*i]) })
    }
}


pub struct LL1Symbol<'a, G: GrammarTypes> {
    pub symbol_type: LL1SymbolType<'a, G>,
    pub with_location: bool,
    pub discard: bool,
}

pub enum LL1SymbolType<'a, G: GrammarTypes> {
    Terminal(&'a G::Terminal),
    NonTerminal {
        name: &'a str,
        offshoot_index: usize,
    },
}

impl <'a, G: GrammarTypes> LL1Symbol<'a, G> {
    fn from_symbol(grammar: &'a Grammar<G>, sb: Symbol) -> Self {
        let ll1sym_type = match sb.symbol_type {
            SymbolType::Terminal(i) => LL1SymbolType::Terminal(&grammar.terminals[i]),
            SymbolType::NonTerminal(i) => {
                let ruleset = &grammar.rulesets[i];
                LL1SymbolType::NonTerminal {
                    name: &ruleset.name,
                    offshoot_index: ruleset.offshoot_index,
                }
            },
        };

        LL1Symbol {
            symbol_type: ll1sym_type,
            with_location: sb.with_location,
            discard: sb.discard,
        }
    }
}

pub enum LL1RuleValue<'a, G: GrammarTypes> {
    SymbolValue(usize),
    ExternalFunction(&'a G::ExternalFunction, Vec<LL1RuleValue<'a, G>>),
    MakeTuple(Vec<LL1RuleValue<'a, G>>),
    GetTuple(Box<LL1RuleValue<'a, G>>, usize),
    DropLocation(Box<LL1RuleValue<'a, G>>),
    BuildLocation {
        first: Box<LL1RuleValue<'a, G>>,
        second: Box<LL1RuleValue<'a, G>>,
        value: Box<LL1RuleValue<'a, G>>,
    },
    Lambda {
        discard_param: bool,
        param_type: LL1RuleType<'a, G>,
        body: Box<LL1RuleValue<'a, G>>,
    },

    // f, a
    Apply(Box<LL1RuleValue<'a, G>>, Box<LL1RuleValue<'a, G>>),
}

impl <'a, G: GrammarTypes> LL1RuleValue<'a, G> {
    fn from_rule_value(grammar: &'a Grammar<G>, rule_value: &'a RuleValue<G>) -> Self {
        match rule_value {
            RuleValue::SymbolValue(sym) => LL1RuleValue::SymbolValue(*sym),
            RuleValue::ExternalFunction(f, args) =>
                LL1RuleValue::ExternalFunction(f, args.iter().map(|arg| Self::from_rule_value(grammar, arg)).collect()),
            RuleValue::MakeTuple(items) =>
                LL1RuleValue::MakeTuple(items.iter().map(|item| Self::from_rule_value(grammar, item)).collect()),
            RuleValue::GetTuple(tuple, index) =>
                LL1RuleValue::GetTuple(Box::new(Self::from_rule_value(grammar, tuple)), *index),
            RuleValue::DropLocation(inner) =>
                LL1RuleValue::DropLocation(Box::new(Self::from_rule_value(grammar, inner))),
            RuleValue::BuildLocation { first, second, value } =>
                LL1RuleValue::BuildLocation {
                    first: Box::new(Self::from_rule_value(grammar, first)),
                    second: Box::new(Self::from_rule_value(grammar, second)),
                    value: Box::new(Self::from_rule_value(grammar, value)),
                },

            RuleValue::Lambda { discard_param, param_type, body } =>
                LL1RuleValue::Lambda {
                    discard_param: *discard_param,
                    param_type: LL1RuleType::from_rule_type(grammar, param_type),
                    body: Box::new(Self::from_rule_value(grammar, body)),
                },

            RuleValue::Apply(f, a) =>
                LL1RuleValue::Apply(
                    Box::new(Self::from_rule_value(grammar, f)),
                    Box::new(Self::from_rule_value(grammar, a)),
                ),
        }
    }
}



#[derive(Debug, Clone)]
pub struct LL1Error {
    pub conflicts: Vec<LL1Conflict>,
}

#[derive(Debug, Clone)]
pub enum LL1Conflict {
    FirstFirst {
        ruleset: usize,
        rule: usize,
        terminal: usize,
    },
    FirstFollow {
        ruleset: usize,
        rule: usize,
        terminal: usize,
    },
    FirstFollowEnd {
        ruleset: usize,
        rule: usize,
    },
}


#[derive(Debug, Clone)]
pub struct LLTransition {
    rules: HashSet<usize>,
    conflicts: Vec<LL1Conflict>,
}



pub trait GrammarBuilderCallback<G: GrammarTypes> {
    fn accept<X>(self, builder: &mut GrammarBuilder<G, X>);
}



#[derive(Debug)]
pub struct SymbolBuilder<X> {
    sym: Symbol,
    dummy: PhantomData<X>,
}

impl <X> SymbolBuilder<X> {
    pub fn with_location(mut self) -> Self {
        self.sym.with_location = true;
        self
    }

    pub fn discard(mut self) -> Self {
        self.sym.discard = true;
        self
    }
}

impl <X> Clone for SymbolBuilder<X> {
    fn clone(&self) -> Self {
        SymbolBuilder {
            sym: self.sym,
            dummy: PhantomData,
        }
    }
}

impl <X> Copy for SymbolBuilder<X> {}

#[derive(Debug)]
pub struct NonTerminalBuilder<X> {
    non_terminal: usize,
    dummy: PhantomData<X>,
}

impl <X> NonTerminalBuilder<X> {
    pub fn sym(self) -> SymbolBuilder<X> {
        SymbolBuilder {
            sym: Symbol {
                symbol_type: SymbolType::NonTerminal(self.non_terminal),
                with_location: false,
                discard: false,
            },
            dummy: PhantomData,
        }
    }
}

impl <X> Clone for NonTerminalBuilder<X> {
    fn clone(&self) -> Self {
        NonTerminalBuilder {
            non_terminal: self.non_terminal,
            dummy: PhantomData,
        }
    }
}

impl <X> Copy for NonTerminalBuilder<X> {}


pub struct GrammarBuilder<G: GrammarTypes, X> {
    grammar: Grammar<G>,
    dummy: PhantomData<X>,
}

impl <G: GrammarTypes, X> GrammarBuilder<G, X> {
    pub fn add_terminal(&mut self, terminal: G::Terminal) -> SymbolBuilder<X> {
        let index = self.grammar.terminals.len();

        self.grammar.terminals.push(terminal);

        SymbolBuilder {
            sym: Symbol {
                symbol_type: SymbolType::Terminal(index),
                with_location: false,
                discard: false,
            },
            dummy: PhantomData,
        }
    }

    pub fn add_non_terminal(&mut self, name: String, rule_type: G::ExternalRuleType, lex_mode: Option<G::ExternalLexMode>) -> NonTerminalBuilder<X> {
        let index = self.grammar.rulesets.len();

        let ruleset = RuleSet {
            rule_type: RuleType::ExternalType(rule_type),
            name,
            offshoot_index: 0,
            rules: Vec::new(),
            lex_mode,
        };

        self.grammar.rulesets.push(ruleset);

        NonTerminalBuilder {
            non_terminal: index,
            dummy: PhantomData,
        }
    }

    pub fn add_rule(&mut self, non_terminal: NonTerminalBuilder<X>, function: G::ExternalFunction, symbols: Vec<SymbolBuilder<X>>) {
        let ruleset = &mut self.grammar.rulesets[non_terminal.non_terminal];

        let symbols: Vec<_> = symbols.into_iter().map(|s| s.sym).collect();
        let value = RuleValue::ExternalFunction(
            function,
            symbols.iter()
                .enumerate()
                .filter(|(_, s)| !s.discard)
                .map(|(i, _)| RuleValue::SymbolValue(i))
                .collect()
        );

        ruleset.rules.push(Rule {
            symbols,
            value,
        });
    }
}

fn find_next_offshoot_index<G: GrammarTypes>(grammar: &Grammar<G>, name: &str) -> usize {
    grammar.rulesets.iter()
        .filter(|rs| rs.name == name)
        .map(|rs| rs.offshoot_index)
        .max()
        .unwrap_or_default()
        + 1
}



fn elim_left_rec<G: GrammarTypes>(grammar: &mut Grammar<G>) {
    for i in 0..grammar.rulesets.len() {
        let ruleset = &grammar.rulesets[i];

        let has_left_rec = ruleset.rules.iter()
            .flat_map(|r| r.symbols.iter().next())
            .any(|s| s.symbol_type == SymbolType::NonTerminal(i));

        if !has_left_rec {
            continue;
        }

        let rule_type = ruleset.rule_type.clone();
        let ruleset_name = ruleset.name.clone();
        let prefix_offshoot_index = find_next_offshoot_index(grammar, &ruleset_name);
        let suffix_offshoot_index = prefix_offshoot_index + 1;
        let suffix_repeat_offshoot_index = prefix_offshoot_index + 2;

        let prefix_ruleset_index = grammar.rulesets.len();
        let suffix_ruleset_index = prefix_ruleset_index + 1;
        let suffix_repeat_ruleset_index = prefix_ruleset_index + 2;
        let ruleset = &mut grammar.rulesets[i];

        let mut suffix_rules = Vec::new();
        let mut prefix_rules = Vec::new();

        let mut has_location = false;
        let mut has_no_location = false;

        for mut r in ruleset.rules.drain(..) {
            let is_left_rec = r.symbols.iter().next()
                .map(|s| s.symbol_type == SymbolType::NonTerminal(i))
                .unwrap_or_default();

            if is_left_rec {
                let sym = r.symbols.remove(0);
                let lambda_arg_index = r.symbols.len();

                if sym.with_location {
                    has_location = true;
                }
                else {
                    has_no_location = true;
                }

                if has_no_location && has_location {
                    panic!("Left-recursive rule {} has both location and non-location arguments", ruleset_name);
                }


                let mut value = r.value;
                value.substitute_indexes(&|i| {
                    if *i == 0 {
                        *i = lambda_arg_index;
                    }
                    else {
                        *i -= 1;
                    }
                });


                let prefix_arg_type =
                    if has_location {
                        RuleType::WithLocation(Box::new(rule_type.clone()))
                    }
                    else {
                        rule_type.clone()
                    };

                r.value = RuleValue::Lambda {
                    discard_param: sym.discard,
                    param_type: prefix_arg_type,
                    body: Box::new(value),
                };

                suffix_rules.push(r);
            }
            else {
                prefix_rules.push(r);
            }
        }


        let prefix_arg_type =
            if has_location {
                RuleType::WithLocation(Box::new(rule_type.clone()))
            }
            else {
                rule_type.clone()
            };

        ruleset.rules.push(Rule {
            symbols: vec![
                Symbol {
                    symbol_type: SymbolType::NonTerminal(prefix_ruleset_index),
                    with_location: has_location,
                    discard: false,
                },
                Symbol {
                    symbol_type: SymbolType::NonTerminal(suffix_repeat_ruleset_index),
                    with_location: false,
                    discard: false,
                },
            ],
            value: RuleValue::Apply(
                Box::new(RuleValue::SymbolValue(1)),
                Box::new(RuleValue::SymbolValue(0)),
            ),
        });

        grammar.rulesets.push(RuleSet {
            rule_type: rule_type.clone(),
            name: ruleset_name.clone(),
            offshoot_index: prefix_offshoot_index,
            rules: prefix_rules,
            lex_mode: None,
        });

        grammar.rulesets.push(RuleSet {
            rule_type: RuleType::Function(Box::new(prefix_arg_type.clone()), Box::new(rule_type.clone())),
            name: ruleset_name.clone(),
            offshoot_index: suffix_offshoot_index,
            rules: suffix_rules,
            lex_mode: None,
        });

        grammar.rulesets.push(RuleSet {
            rule_type: RuleType::Function(Box::new(prefix_arg_type.clone()), Box::new(rule_type)),
            name: ruleset_name,
            offshoot_index: suffix_repeat_offshoot_index,
            rules: vec![
                Rule {
                    symbols: vec![
                        Symbol {
                            symbol_type: SymbolType::NonTerminal(suffix_ruleset_index),
                            with_location: true,
                            discard: false,
                        },
                        Symbol {
                            symbol_type: SymbolType::NonTerminal(suffix_repeat_ruleset_index),
                            with_location: false,
                            discard: false,
                        },
                    ],
                    // x2 => x1(x0(x2))
                    value: RuleValue::Lambda {
                        discard_param: false,
                        param_type: prefix_arg_type.clone(),
                        body: Box::new(RuleValue::Apply(
                            Box::new(RuleValue::SymbolValue(1)),
                            Box::new(RuleValue::BuildLocation {
                                first: Box::new(RuleValue::SymbolValue(2)),
                                second: Box::new(RuleValue::SymbolValue(0)),
                                value: Box::new(RuleValue::Apply(
                                    Box::new(RuleValue::DropLocation(Box::new(RuleValue::SymbolValue(0)))),
                                    Box::new(RuleValue::SymbolValue(2)),
                                )),
                            }),
                        )),
                    },
                },

                // Empty rule for no recursion.
                Rule {
                    symbols: vec![],
                    value: RuleValue::Lambda {
                        discard_param: false,
                        param_type: prefix_arg_type.clone(),
                        body: if has_location {
                            Box::new(RuleValue::DropLocation(Box::new(RuleValue::SymbolValue(0))))
                        }
                        else {
                            Box::new(RuleValue::SymbolValue(0))
                        },
                    },
                }
            ],
            lex_mode: None,
        });
    }
}

fn left_factor<G: GrammarTypes>(grammar: &mut Grammar<G>) {
    let mut needs_factor = true;

    let mut ruleset_types = Vec::new();

    while needs_factor {
        needs_factor = false;

        for i in ruleset_types.len()..grammar.rulesets.len() {
            ruleset_types.push(grammar.rulesets[i].rule_type.clone());
        }

        let type_for_sym = |sym: Symbol| {
            let mut t = match sym.symbol_type {
                SymbolType::Terminal(index) => RuleType::TerminalType(index),
                SymbolType::NonTerminal(index) => ruleset_types[index].clone(),
            };

            if sym.with_location {
                t = RuleType::WithLocation(Box::new(t));
            }

            t
        };

        for i in 0..grammar.rulesets.len() {
            let ruleset = &grammar.rulesets[i];
    
            let mut prefixes: HashMap<Vec<Symbol>, usize> = HashMap::new();
            for rule in &ruleset.rules {
                for j in 1..=rule.symbols.len() {
                    let mut entry_prefix_vec = rule.symbols[0..j].to_vec();
                    for sym in &mut entry_prefix_vec {
                        sym.discard = false;
                    }


                    *prefixes.entry(entry_prefix_vec).or_insert(0) += 1;
                }
            }
    
            let has_common_prefix = prefixes.values().copied().any(|n| n > 1);
    
            if !has_common_prefix {
                continue;
            }

            needs_factor = true;
    
            let ruleset_name = ruleset.name.clone();
            let mut new_offshoot_index = find_next_offshoot_index(grammar, &ruleset_name);
    
            let mut next_new_ruleset_index_offset = grammar.rulesets.len();
            let ruleset = &mut grammar.rulesets[i];
    
    
            
            let mut new_rulesets: HashMap<Vec<Symbol>, (usize, RuleSet<G>)> = HashMap::new();
            let mut adjusted_rules: Vec<Rule<G>> = Vec::new();
    
            for mut r in ruleset.rules.drain(..) {
                // Find matching prefixes.
                // Prefer the longest of the most common prefixes.
                if let Some(prefix) = prefixes.iter()
                    .filter(|(prefix, n)|
                        **n > 1 &&
                            prefix.len() <= r.symbols.len() &&
                            r.symbols
                                .iter()
                                .zip(prefix.iter())
                                .all(|(a, b)| {
                                    let mut a2 = a.clone();
                                    a2.discard = false;
                                    a2 == *b
                                })
                    )
                    .max_by_key(|(prefix, n)| (**n, prefix.len()))
                    .map(|(prefix, _)| prefix)
                {
                    let new_rule_arg_type =
                        if prefix.len() == 1 {
                            type_for_sym(prefix[0])
                        }
                        else {
                            RuleType::Tuple(
                                prefix.iter().copied().map(type_for_sym).collect()
                            )
                        };

                    let (_, new_ruleset) = new_rulesets.entry(prefix.clone())
                        .or_insert_with(|| {
                            let ruleset_index = next_new_ruleset_index_offset;
                            next_new_ruleset_index_offset += 1;

                            let offshoot_index = new_offshoot_index;
                            new_offshoot_index += 1;

                            let mut symbols = Vec::new();
                            symbols.reserve_exact(prefix.len() + 1);
                            symbols.extend(prefix);
                            symbols.push(Symbol {
                                symbol_type: SymbolType::NonTerminal(ruleset_index),
                                with_location: false,
                                discard: false,
                            });



                            let new_rule_type =
                                RuleType::Function(
                                    Box::new(new_rule_arg_type.clone()),
                                    Box::new(ruleset.rule_type.clone()),
                                );

                            let ruleset = RuleSet {
                                rule_type: new_rule_type,
                                name: ruleset_name.clone(),
                                offshoot_index,
                                rules: Vec::new(),
                                lex_mode: None,
                            };
                            
                            adjusted_rules.push(Rule {
                                symbols,
                                value: RuleValue::Apply(
                                    Box::new(RuleValue::SymbolValue(prefix.len())),
                                    Box::new(
                                        if prefix.len() == 1 {
                                            RuleValue::SymbolValue(0)
                                        }
                                        else {
                                            RuleValue::MakeTuple((0..prefix.len()).map(RuleValue::SymbolValue).collect())
                                        }
                                    )
                                ),
                            });
                            (ruleset_index, ruleset)
                        });

                    let all_elements_discarded = r.symbols.drain(0..prefix.len()).all(|sym| sym.discard);

                    r.value.substitute_indexes_with(&|index| {
                        if index < prefix.len() {
                            if prefix.len() == 1 {
                                RuleValue::SymbolValue(r.symbols.len())
                            }
                            else {
                                RuleValue::GetTuple(Box::new(RuleValue::SymbolValue(r.symbols.len())), index)
                            }
                        }
                        else if index < prefix.len() + r.symbols.len() {
                            RuleValue::SymbolValue(index - prefix.len())
                        }
                        else {
                            RuleValue::SymbolValue(index - prefix.len() + 1)
                        }
                    });

                    r.value = RuleValue::Lambda {
                        discard_param: all_elements_discarded,
                        param_type: new_rule_arg_type,
                        body: Box::new(r.value),
                    };

                    new_ruleset.rules.push(r);
                }
                else {
                    adjusted_rules.push(r);
                }
            }

            ruleset.rules.extend(adjusted_rules);

            let mut new_rulesets = new_rulesets.into_values().collect::<Vec<_>>();
            new_rulesets.sort_by_key(|(i, _)| *i);
            grammar.rulesets.extend(new_rulesets.into_iter().map(|(_, ruleset)| ruleset));
        }
    }
}




fn build_first<G: GrammarTypes>(grammar: &Grammar<G>) -> Vec<HashSet<FirstElem>> {
    let mut firsts = vec![ HashSet::new(); grammar.rulesets.len() ];
    let mut first = HashSet::new();

    let mut needs_build = true;
    while needs_build {
        needs_build = false;

        for (i, ruleset) in grammar.rulesets.iter().enumerate() {
            first.clear();

            for rule in &ruleset.rules {
                symbol_firsts(&rule.symbols, &firsts, &mut first);
            }

            let first2 = &mut firsts[i];
            for elem in &first {
                needs_build |= first2.insert(*elem);
            }
        }
    }

    firsts
}

fn symbol_firsts(symbols: &[Symbol], firsts: &[HashSet<FirstElem>], first: &mut HashSet<FirstElem>) {
    match symbols {
        [ Symbol { symbol_type: SymbolType::Terminal(a), .. }, .. ] => {
            first.insert(FirstElem::Terminal(*a));
        },
        [ Symbol { symbol_type: SymbolType::NonTerminal(a), .. }, .. ] => {
            let a_firsts = &firsts[*a];
            if a_firsts.contains(&FirstElem::Epsilon) {
                first.extend(a_firsts.iter().copied().filter(|s| *s != FirstElem::Epsilon));
                symbol_firsts(&symbols[1..], firsts, first);
            }
            else {
                first.extend(a_firsts);
            }
        },
        [] => {
            first.insert(FirstElem::Epsilon);
        },
    }
}

fn build_follow<G: GrammarTypes>(grammar: &Grammar<G>, firsts: &[HashSet<FirstElem>]) -> Vec<HashSet<FollowElem>> {
    let mut follows = vec![ HashSet::new(); grammar.rulesets.len() ];
    let mut follow = HashSet::new();
    let mut post_first = HashSet::new();

    follows[0].insert(FollowElem::EndOfFile);
    
    let mut needs_build = true;
    while needs_build {
        needs_build = false;

        for (j, ruleset) in grammar.rulesets.iter().enumerate() {
            for rule in &ruleset.rules {
                for (k, sym) in rule.symbols.iter().copied().enumerate() {
                    let SymbolType::NonTerminal(i) = sym.symbol_type else { continue; };
                    
                    post_first.clear();
                    symbol_firsts(&rule.symbols[k + 1..], firsts, &mut post_first);

                    for elem in &post_first {
                        match elem {
                            FirstElem::Terminal(a) => {
                                needs_build |= follows[i].insert(FollowElem::Terminal(*a));
                            },
                            FirstElem::Epsilon => {
                                follow.clear();
                                follow.extend(&follows[j]);

                                for elem in &follow {
                                    needs_build |= follows[i].insert(*elem);
                                }
                            },
                        }
                    }
                }
            }
        }
    }

    follows
}

fn build_table<G: GrammarTypes>(grammar: &Grammar<G>, firsts: &[HashSet<FirstElem>], follows: &[HashSet<FollowElem>]) -> Vec<Vec<LLTransition>> {
    let mut table = vec![
        vec![
            LLTransition {
                rules: HashSet::new(),
                conflicts: Vec::new(),
            };
            grammar.terminals.len() + 1
        ];
        grammar.rulesets.len()
    ];
    let eof_terminal_index = grammar.terminals.len();

    for (i, ruleset) in grammar.rulesets.iter().enumerate() {
        let follow = &follows[i];

        for (j, rule) in ruleset.rules.iter().enumerate() {
            let mut sym_first = HashSet::new();
            symbol_firsts(&rule.symbols, firsts, &mut sym_first);

            for k in 0..grammar.terminals.len() {
                if !sym_first.contains(&FirstElem::Terminal(k)) {
                    continue;
                }

                let cell = &mut table[i][k];
                if !cell.rules.is_empty() {
                    cell.conflicts.push(LL1Conflict::FirstFirst {
                        ruleset: i,
                        rule: j,
                        terminal: k,
                    });
                }
                
                cell.rules.insert(j);
            }

            if sym_first.contains(&FirstElem::Epsilon) {
                for k in 0..grammar.terminals.len() {
                    if !follow.contains(&FollowElem::Terminal(k)) {
                        continue;
                    }
                    
                    let cell = &mut table[i][k];
                    if !cell.rules.is_empty() {
                        cell.conflicts.push(LL1Conflict::FirstFollow {
                            ruleset: i,
                            rule: j,
                            terminal: k,
                        });
                    }
                    
                    cell.rules.insert(j);
                }
                
                if follow.contains(&FollowElem::EndOfFile) {
                    let cell = &mut table[i][eof_terminal_index];
                    if !cell.rules.is_empty() {
                        cell.conflicts.push(LL1Conflict::FirstFollowEnd {
                            ruleset: i,
                            rule: j,
                        });
                    }
                    
                    cell.rules.insert(j);
                }
            }
        }
    }

    table
}


