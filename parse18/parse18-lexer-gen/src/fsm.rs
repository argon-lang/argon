use std::collections::{BTreeSet, HashMap, HashSet};

use unicode_general_category::GeneralCategory;

use crate::regex::{CharClass, CharClassSet, Regex, UnicodePropertySet};


pub struct DFA<A> {
    pub states: Vec<DFAState<A>>
}

impl <A> DFA<A> {
    pub fn map<B>(self, mut f: impl FnMut(A) -> B) -> DFA<B> {
        DFA {
            states: self.states.into_iter().map(move |state| state.map(&mut f)).collect()
        }
    }
}

pub struct DFAState<A> {
    pub acceptance: Option<A>,
    pub is_reject: bool,
    pub transition: DFATransition,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DFATransition {
    Always(usize),
    IfCharacter(char, Box<DFATransition>, Box<DFATransition>),
    IfCharacterRange(char, char, Box<DFATransition>, Box<DFATransition>),
    ForCategory {
        categories: Vec<(Vec<GeneralCategory>, DFATransition)>,
        fallback: Box<DFATransition>,
    },
    IfProperty(UnicodePropertySet, Box<DFATransition>, Box<DFATransition>),
}

impl <A> DFAState<A> {
    pub fn map<B>(self, f: impl FnMut(A) -> B) -> DFAState<B> {
        DFAState {
            acceptance: self.acceptance.map(f),
            is_reject: self.is_reject,
            transition: self.transition,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NFA<A> {
    pub states: Vec<NFAState<A>>
}

impl <A> NFA<A> {
    fn validate_states(&self) {
        for state in &self.states {
            for t in &state.transitions {
                for &target in &t.target {
                    assert!(target < self.states.len(), "invalid state {}, # states = {}", target, self.states.len());
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NFAState<A> {
    pub acceptance: Option<A>,
    pub transitions: Vec<NFATransition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NFATransition {
    pub target: Vec<usize>,
    pub conditions: FSMTransitionConditionConjunct,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FSMTransitionConditionConjunct {
    pub conditions: Vec<FSMTransitionConditionLiteral>,
}

impl FSMTransitionConditionConjunct {
    fn contains_char(&self, c: char) -> bool {
        self.conditions.iter().all(|lit| lit.contains_char(c))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FSMTransitionConditionLiteral {
    pub condition_type: CharClass,
    pub negation: bool,
}

impl FSMTransitionConditionLiteral {
    fn contains_char(self, c: char) -> bool {
        self.condition_type.contains_char(c) != self.negation
    }
}


pub fn nfa_to_dfa<A: NFAAcceptancePriority + Clone>(nfa: NFA<A>) -> DFA<A> {
    let mut converter = NFAConverter {
        nfa,
        dfa: DFA {
            states: Vec::new(),
        },
        dfa_states: Vec::new(),
        state_mapping: HashMap::new(),

        unicode_prop_categories: HashMap::new(),
    };

    converter.build();

    converter.dfa
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct NFAStateSet {
    states: BTreeSet<usize>,
}


pub trait NFAAcceptancePriority {
    fn compare_priority(&self, rhs: &Self) -> std::cmp::Ordering;
}





struct NFAConverter<A> {
    nfa: NFA<A>,
    dfa: DFA<A>,

    dfa_states: Vec<NFAStateSet>,
    state_mapping: HashMap<NFAStateSet, usize>,

    unicode_prop_categories: HashMap<UnicodePropertySet, HashSet<GeneralCategory>>,
}

impl <A: NFAAcceptancePriority + Clone> NFAConverter<A> {
    fn build(&mut self) {
        // Force the initial state to be added to dfa_states.
        self.get_dfa_state(NFAStateSet { states: BTreeSet::from([ 0 ]) });
        
        while self.dfa.states.len() < self.dfa_states.len() {
            let state = self.build_dfa_state(&self.dfa_states[self.dfa.states.len()].clone());
            self.dfa.states.push(state);
        }
    }

    fn get_dfa_state(&mut self, nfa_states: NFAStateSet) -> usize {
        *self.state_mapping.entry(nfa_states)
            .or_insert_with_key(|nfa_states| {
                let index = self.dfa_states.len();
                self.dfa_states.push(nfa_states.clone());
                index
            })
    }

    fn get_prop_categories(&mut self, prop: UnicodePropertySet) -> &HashSet<GeneralCategory> {
        self.unicode_prop_categories.entry(prop).or_insert_with(|| {
            let mut categories = HashSet::new();
            for c in '\0'..=char::MAX {
                if prop.contains_char(c) {
                    categories.insert(unicode_general_category::get_general_category(c));
                }
            }
            categories
        })
    }

    fn build_dfa_state(&mut self, nfa_states: &NFAStateSet) -> DFAState<A> {
        let mut builder = DFAStateBuilder::new(self);
        builder.add_nfa_states(nfa_states);
        let transition = builder.generate_transition();
        let acceptance = builder.acceptance;

        DFAState {
            acceptance,
            is_reject: nfa_states.states.is_empty(),
            transition,
        }
    }
}

struct DFAStateBuilder<'a, A> {
    nfa_converter: &'a mut NFAConverter<A>,

    nfa_transitions: Vec<NFATransition>,

    acceptance: Option<A>,
    atoms: HashSet<CharClass>,
}

impl <'a, A: NFAAcceptancePriority + Clone> DFAStateBuilder<'a, A> {
    fn new(nfa_converter: &'a mut NFAConverter<A>) -> Self {
        DFAStateBuilder {
            nfa_converter,

            nfa_transitions: Vec::new(),

            acceptance: None,
            atoms: HashSet::new(),
        }
    }

    fn add_nfa_states(&mut self, nfa_states: &NFAStateSet) {
        for &state_id in &nfa_states.states {
            let state = &self.nfa_converter.nfa.states[state_id];

            for t in &state.transitions {
                for literal in &t.conditions.conditions {
                    self.atoms.insert(literal.condition_type.clone());
                }
                self.nfa_transitions.push(t.clone());
            }

            merge_acceptance(&mut self.acceptance, state.acceptance.clone());
        }
    }

    fn generate_transition(&mut self) -> DFATransition {
        let mut ranges = BTreeSet::new();
        let mut unicode_categories = HashSet::new();
        let mut unicode_property_groups = BTreeSet::new();
        
        for atom in self.atoms.iter().copied() {
            match atom {
                CharClass::Any => {},
                CharClass::Char(c) => { ranges.insert((c, c)); },
                CharClass::Range(a, b) => { ranges.insert((a, b)); },
                CharClass::Category(cat) => { unicode_categories.insert(cat); },
                CharClass::Property(prop) => { unicode_property_groups.insert(prop); },
            }
        }

        let mut range_builder = RangeClassBuilder::new(self);
        let mut prev_char = None;
        for (start, end) in ranges {
            let range = if let Some(pc) = &mut prev_char {
                    if start <= *pc && *pc <= end {
                        if let Some(next_char) = char::from_u32(start as u32 + 1) {
                            next_char..=end
                        }
                        else {
                            // Next value is not a valid character so we have to include the last value and then skip it.
                            let mut range = *pc..=end;
                            range.next();
                            range
                        }
                    }
                    else {
                        start..=end
                    }
            }
            else {
                start..=end
            };

            for c in range {
                range_builder.next(c);
            }

            prev_char = Some(end);
        }
        range_builder.finalize();
        let range_mapping = range_builder.range_mapping;


        let unicode_property_groups = unicode_property_groups.into_iter().collect::<Vec<_>>();

        let no_cat_trans = self.scan_properties(None, &unicode_property_groups);

        let mut cat_trans: Vec<(Vec<GeneralCategory>, DFATransition)> = Vec::new();

        'cat: for cat in unicode_categories {
            let prop_trans = self.scan_properties(Some(cat), &unicode_property_groups);

            if prop_trans == no_cat_trans {
                continue;
            }

            for (other_cats, other_cat_trans) in &mut cat_trans {
                if prop_trans == *other_cat_trans {
                    other_cats.push(cat);
                    continue 'cat;
                }
            }

            cat_trans.push((vec![ cat ], prop_trans));
        }

        
        let mut trans =
            if cat_trans.is_empty() {
                no_cat_trans
            }
            else {
                DFATransition::ForCategory { categories: cat_trans, fallback: Box::new(no_cat_trans) }
            };

        for (start, end, target) in range_mapping.into_iter().rev() {
            let range_trans = Box::new(DFATransition::Always(target));
            trans = if start == end {
                DFATransition::IfCharacter(start, range_trans, Box::new(trans))
            }
            else {
                DFATransition::IfCharacterRange(start, end, range_trans, Box::new(trans))
            };
        }

        trans
    }

    fn scan_properties(&mut self, cat: Option<GeneralCategory>, props: &[UnicodePropertySet]) -> DFATransition {
        self.scan_properties_rec(cat, &mut BTreeSet::new(), props)
    }

    fn scan_properties_rec(&mut self, cat: Option<GeneralCategory>, selected_props: &mut BTreeSet<UnicodePropertySet>, props: &[UnicodePropertySet]) -> DFATransition {
        if props.is_empty() {
            let targets = self.find_prop_target(cat, &selected_props);
            let target = self.nfa_converter.get_dfa_state(targets);
            
            DFATransition::Always(target)
        }
        else {
            let prop = props[0];

            let without_res = self.scan_properties_rec(cat, selected_props, &props[1..]);

            selected_props.insert(prop);

            let result = if self.is_absurd_or_obvious_property_set(cat, selected_props) {
                without_res
            }
            else {
                let with_res = self.scan_properties_rec(cat, selected_props, &props[1..]);

                if without_res == with_res {
                    without_res
                }
                else {
                    DFATransition::IfProperty(prop, Box::new(with_res), Box::new(without_res))
                }
            };

            selected_props.remove(&prop);
            
            result
        }
    }

    fn is_absurd_or_obvious_property_set(&mut self, cat: Option<GeneralCategory>, selected_props: &BTreeSet<UnicodePropertySet>) -> bool {
        if let Some(cat) = cat {
            if selected_props.iter().copied().any(|prop|
                !self.nfa_converter.get_prop_categories(prop).contains(&cat)
            ) {
                return true;
            }
        }

        // This works because the only supported properties define sets
        // that only have overlaps when one is a subset of the other.
        !selected_props.iter().copied().all(|p1|
            selected_props.iter().copied().all(|p2|
                p1.implies(p2) || p2.implies(p1)
            )
        )
    }

    fn find_char_target(&mut self, c: char) -> NFAStateSet {
        let mut states = BTreeSet::new();
        for t in &self.nfa_transitions {
            if t.conditions.contains_char(c) {
                states.extend(t.target.iter().copied());
            }
        }

        NFAStateSet {
            states
        }
    }

    fn find_prop_target(&mut self, cat: Option<GeneralCategory>, props: &BTreeSet<UnicodePropertySet>) -> NFAStateSet {
        let mut states = BTreeSet::new();
        for t in &self.nfa_transitions {
            if self.evaluate_prop_conjunct(cat, props, &t.conditions) {
                states.extend(t.target.iter().copied());
            }
        }

        NFAStateSet {
            states
        }
    }

    fn evaluate_prop_conjunct(&self, cat: Option<GeneralCategory>, props: &BTreeSet<UnicodePropertySet>, conj: &FSMTransitionConditionConjunct) -> bool {
        conj.conditions.iter()
            .all(|lit|
                self.evaluate_prop_atom(cat, props, lit.condition_type) != lit.negation
            )
    }

    fn evaluate_prop_atom(&self, cat: Option<GeneralCategory>, props: &BTreeSet<UnicodePropertySet>, cc: CharClass) -> bool {
        match cc {
            CharClass::Any => true,
            CharClass::Char(_) => false,
            CharClass::Range(_, _) => false,
            CharClass::Category(cat2) => cat == Some(cat2),
            CharClass::Property(prop) => props.contains(&prop),
        }
    }
}

struct RangeClassBuilder<'a, 'b, A> {
    builder: &'b mut DFAStateBuilder<'a, A>,

    prev: Option<(char, char, NFAStateSet)>,

    range_mapping: Vec<(char, char, usize)>,
}

impl <'a, 'b, A: NFAAcceptancePriority + Clone> RangeClassBuilder<'a, 'b, A> {
    fn new(builder: &'b mut DFAStateBuilder<'a, A>) -> Self {
        Self {
            builder,
            prev: None,
            range_mapping: Vec::new(),
        }
    }

    fn emit_range(&mut self) {
        if let Some((prev_range_start, prev_char, prev_target)) = self.prev.take() {
            let target = self.builder.nfa_converter.get_dfa_state(prev_target);
            self.range_mapping.push((prev_range_start, prev_char, target));
        }
    }

    fn next(&mut self, c: char) {
        let new_target = self.builder.find_char_target(c);
        if let Some((_, prev_char, prev_target)) = &mut self.prev {
            if (*prev_char as u32) + 1 == (c as u32) && *prev_target == new_target {
                *prev_char = c;
            }
            else {
                self.emit_range();
                self.prev = Some((c, c, new_target));
            }
        }
        else {
            self.prev = Some((c, c, new_target));
        }
    }

    fn finalize(&mut self) {
        self.emit_range();
    }
}



fn merge_acceptance<A: NFAAcceptancePriority>(a: &mut Option<A>, b: Option<A>) {
    match (a.as_mut(), b) {
        (Some(a), Some(b)) if a.compare_priority(&b) == std::cmp::Ordering::Less =>
            *a = b,

        (None, Some(b)) =>
            *a = Some(b),

        _ => {},
    }
}


pub fn nfa_from_regex_mapping<A: NFAAcceptancePriority + Clone + std::fmt::Debug>(alts: Vec<(Regex, A)>) -> NFA<A> {
    alts.into_iter()
        .map(|(r, value)| nfa_from_regex(r, value))
        .reduce(|mut a, b| {
            nfa_union(&mut a, b);
            a
        })
        .unwrap_or_else(reject_all_nfa)
}

fn nfa_from_regex<A: NFAAcceptancePriority + Clone + std::fmt::Debug>(r: Regex, value: A) -> NFA<A> {
    match r {
        Regex::Empty => {
            NFA {
                states: vec![
                    NFAState {
                        acceptance: Some(value),
                        transitions: vec![],
                    },
                ],
            }
        },

        Regex::CharClassSet(char_class_set) => {
            NFA {
                states: vec![
                    NFAState {
                        acceptance: None,
                        transitions: nfa_conditions_from_regex(char_class_set, vec![ 1 ]),
                    },
                    NFAState {
                        acceptance: Some(value),
                        transitions: vec![],
                    },
                ],
            }
        },

        Regex::Alternative(alts) => {
            alts.into_iter()
                .map(|r| nfa_from_regex(r, value.clone()))
                .reduce(|mut a, b| {
                    nfa_union(&mut a, b);
                    a
                })
                .unwrap_or_else(reject_all_nfa)
        },

        Regex::Concat(parts) => {
            parts.into_iter()
                .map(|r| nfa_from_regex(r, value.clone()))
                .reduce(|mut a, b| {
                    nfa_concat(&mut a, b);
                    a
                })
                .unwrap_or_else(|| nfa_from_regex(Regex::Empty, value))
        },


        Regex::Repeat(item_regex) => {
            let mut nfa = nfa_from_regex(*item_regex, value.clone());

            nfa.states[0].acceptance = Some(value);

            let start_state_transitions = nfa.states[0].transitions.clone();

            for state in &mut nfa.states[1..] {
                if state.acceptance.is_none() {
                    continue;
                }

                state.transitions.extend_from_slice(&start_state_transitions);
            }

            nfa
        },
    }
}

fn reject_all_nfa<A>() -> NFA<A> {
    NFA {
        states: vec![
            NFAState {
                acceptance: None,
                transitions: vec![],
            },
        ],
    }
}

fn adjust_states<A>(state: &mut NFAState<A>, adjust_state: impl Fn(&mut usize)) {
    for t in &mut state.transitions {
        for target in &mut t.target {
            adjust_state(target);
        }
    }
}

fn add_state<A>(nfa: &mut NFA<A>, mut state: NFAState<A>, adjust_state: impl Fn(&mut usize)) {
    adjust_states(&mut state, adjust_state);
    nfa.states.push(state);
}

fn nfa_union<A: NFAAcceptancePriority + std::fmt::Debug>(a: &mut NFA<A>, b: NFA<A>) {
    let offset = a.states.len() - 1;
    let adjust = |state: &mut usize| {
        if *state > 0 {
            *state += offset;
        }
    };

    for (i, mut b_state) in b.states.into_iter().enumerate() {
        if i == 0 {
            let start_state = &mut a.states[0];
            adjust_states(&mut b_state, adjust);
            merge_acceptance(&mut start_state.acceptance, b_state.acceptance);
            start_state.transitions.extend(b_state.transitions);
        }
        else {
            add_state(a, b_state, adjust);
        }
    }

    a.validate_states();
}

fn regex_cond_to_dnf(char_class_set: CharClassSet, negation: bool) -> Vec<Vec<FSMTransitionConditionLiteral>> {
    match char_class_set {
        CharClassSet::Empty =>
            if negation {
                vec![ vec![] ]
            }
            else {
                vec![]
            },

        CharClassSet::Class(char_class) => {
            vec![
                vec![
                    FSMTransitionConditionLiteral {
                        condition_type: char_class,
                        negation,
                    },
                ],
            ]
        },
        
        CharClassSet::Not(char_class_set) => {
            regex_cond_to_dnf(*char_class_set, !negation)
        },

        CharClassSet::Union(a, b) => {
            let a = *a;
            let b = *b;
            if negation {
                regex_cond_to_dnf(
                    CharClassSet::Intersection(
                        Box::new(CharClassSet::Not(Box::new(a))),
                        Box::new(CharClassSet::Not(Box::new(b))),
                    ),
                    false
                )
            }
            else {
                let mut dnf = regex_cond_to_dnf(a, false);
                dnf.extend(regex_cond_to_dnf(b, false));
                dnf
            }
        },
        CharClassSet::Intersection(a, b) => {
            let a = *a;
            let b = *b;
            if negation {
                regex_cond_to_dnf(
                    CharClassSet::Union(
                        Box::new(CharClassSet::Not(Box::new(a))),
                        Box::new(CharClassSet::Not(Box::new(b))),
                    ),
                    false
                )
            }
            else {
                let a2 = regex_cond_to_dnf(a, false);
                let b2 = regex_cond_to_dnf(b, false);

                a2.into_iter()
                    .flat_map(|ai|
                        b2.iter().map(move |bi| {
                            let mut conjunt = ai.clone();
                            conjunt.extend(bi.iter().cloned());
                            conjunt
                        })
                    )
                    .collect()
            }
        },
    }
}

fn nfa_conditions_from_regex(char_class_set: CharClassSet, target: Vec<usize>) -> Vec<NFATransition> {
    let dnf = regex_cond_to_dnf(char_class_set, false);

    dnf.into_iter()
        .map(|conjunct|
            NFATransition {
                conditions: FSMTransitionConditionConjunct {
                    conditions: conjunct,
                },
                target: target.clone(),
            }
        )
        .collect()
}

fn nfa_concat<A: NFAAcceptancePriority + Clone + std::fmt::Debug>(a: &mut NFA<A>, b: NFA<A>) {

    let offset = a.states.len();

    let adjust = |state: &mut usize| *state += offset;

    let b_start_state = &b.states[0];
    for state in &mut a.states {
        if state.acceptance.is_none() {
            continue;
        }

        let mut b_start_state = b_start_state.clone();
        adjust_states(&mut b_start_state, adjust);

        state.acceptance = b_start_state.acceptance;
        state.transitions.extend_from_slice(&b_start_state.transitions);
    }

    for state in b.states {
        add_state(a, state, adjust);
    }

    a.validate_states();
}





