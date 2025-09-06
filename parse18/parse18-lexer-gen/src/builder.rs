use crate::{fsm::{nfa_from_regex_mapping, nfa_to_dfa, NFAAcceptancePriority, DFA}, regex::Regex};



#[derive(Debug, Clone, PartialEq, Eq)]
struct WithIndex<A>(usize, A);

impl <A> NFAAcceptancePriority for WithIndex<A> {
    fn compare_priority(&self, rhs: &Self) -> std::cmp::Ordering {
        self.0.cmp(&rhs.0)
    }
}

pub struct DFABuilder<A> {
    regexes: Vec<(Regex, WithIndex<A>)>,
}

impl <A: Clone + std::fmt::Debug> DFABuilder<A> {
    pub fn new() -> Self {
        DFABuilder {
            regexes: Vec::new(),
        }
    }

    pub fn add<A2>(&mut self, r: Regex, value: A2)
        where A : From<A2>
    {
        self.regexes.push((r, WithIndex(self.regexes.len(), A::from(value))));
    }

    pub fn into_dfa(self) -> DFA<A> {
        let nfa = nfa_from_regex_mapping(self.regexes);
        let dfa = nfa_to_dfa(nfa);
        dfa.map(|wi: WithIndex<A>| wi.1)
    }
}


