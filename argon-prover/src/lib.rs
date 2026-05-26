#![no_std]

mod prolog;

extern crate alloc;

use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::vec::Vec;
use argon_util::{Fuel, UniqueIdentifier};
use core::hash::Hash;

pub use prolog::*;

pub trait ProverContext {
    type Syntax: ProverSyntax;
    type ProofAtom: Clone;
    type Model: Clone;

    fn fresh_assertions(
        &self,
        model: &Self::Model,
    ) -> Vec<(Proof<Self::ProofAtom>, Predicate<Self::Syntax>)>;
    fn unify_predicate_expression(
        &self,
        f1: &<Self::Syntax as ProverSyntax>::PredicateExpr,
        f2: &<Self::Syntax as ProverSyntax>::PredicateExpr,
        model: &mut Rc<Self::Model>,
        fuel: Fuel,
    ) -> bool;
}

pub trait Prover<'a>: 'a {
    type Context: ProverContext;
    fn new(context: &'a Self::Context, fuel: Fuel) -> Self;
    fn check(
        self,
        goal: Rc<Predicate<<Self::Context as ProverContext>::Syntax>>,
        model: &mut <Self::Context as ProverContext>::Model,
    ) -> ProofResult<Self::Context>;
}

pub enum ProofResult<PC: ProverContext + ?Sized> {
    Yes(Proof<PC::ProofAtom>),
    No(Proof<PC::ProofAtom>),
    Unknown,
}

pub enum PartialProofResult<PC: ProverContext + ?Sized> {
    Yes(Proof<PC::ProofAtom>, Rc<PC::Model>),
    No(Proof<PC::ProofAtom>, Rc<PC::Model>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Proof<A> {
    Atomic(A),
    TrueIsTrue,

    /// Reference an identifier bound by an implication abstraction.
    Identifier(UniqueIdentifier),

    /// Proves A -> B given an expression of B in terms of A.
    ImplicationAbstraction {
        id: UniqueIdentifier,
        body: Box<Proof<A>>,
    },

    /// Given A -> B and A, proves B.
    ModusPonens {
        implication: Box<Proof<A>>,
        premise: Box<Proof<A>>,
    },

    /// Given A -> B and not B, proves not A.
    ModusTollens {
        implication: Box<Proof<A>>,
        consequent_false: Box<Proof<A>>,
    },

    /// Given A and B, proves A & B.
    ConjunctIntro {
        a: Box<Proof<A>>,
        b: Box<Proof<A>>,
    },

    /// Given A, proves A | B.
    DisjunctIntroLeft(Box<Proof<A>>),

    /// Given B, proves A | B.
    DisjunctIntroRight(Box<Proof<A>>),

    /// Given A | B, proves B | A.
    DisjunctCommute(Box<Proof<A>>),

    /// Given A & B, proves B & A.
    ConjunctCommute(Box<Proof<A>>),

    /// Given (not A) & (not B), proves not (A | B).
    DeMorganAndPullNotOut(Box<Proof<A>>),

    /// Given (not A) | (not B), proves not (A & B).
    DeMorganOrPullNotOut(Box<Proof<A>>),

    /// Given not (A & B), proves (not A) | (not B).
    DeMorganAndPushNotIn(Box<Proof<A>>),

    /// Given not (A | B), proves (not A) & (not B).
    DeMorganOrPushNotIn(Box<Proof<A>>),

    /// Given A, proves not not A.
    DoubleNegIntro(Box<Proof<A>>),

    /// Given P -> Q and Q -> R, proves P -> R.
    HypotheticalSyllogism {
        p_implies_q: Box<Proof<A>>,
        q_implies_r: Box<Proof<A>>,
    },
}

pub trait ProverSyntax {
    type Variable;
    type PredicateExpr: Clone + Eq + Hash;
}

pub enum Predicate<S: ProverSyntax> {
    PredicateExpression(S::PredicateExpr),
    And(Rc<Predicate<S>>, Rc<Predicate<S>>),
    Or(Rc<Predicate<S>>, Rc<Predicate<S>>),
    Implies(Rc<Predicate<S>>, Rc<Predicate<S>>),
    True,
    False,
}

impl<S> Clone for Predicate<S>
where
    S: ProverSyntax,
    S::PredicateExpr: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Predicate::PredicateExpression(predicate) => {
                Predicate::PredicateExpression(predicate.clone())
            }
            Predicate::And(a, b) => Predicate::And(a.clone(), b.clone()),
            Predicate::Or(a, b) => Predicate::Or(a.clone(), b.clone()),
            Predicate::Implies(a, b) => Predicate::Implies(a.clone(), b.clone()),
            Predicate::True => Predicate::True,
            Predicate::False => Predicate::False,
        }
    }
}

impl<S> core::fmt::Debug for Predicate<S>
where
    S: ProverSyntax,
    S::PredicateExpr: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Predicate::PredicateExpression(predicate) => f
                .debug_tuple("PredicateExpression")
                .field(predicate)
                .finish(),
            Predicate::And(a, b) => f.debug_tuple("And").field(a).field(b).finish(),
            Predicate::Or(a, b) => f.debug_tuple("Or").field(a).field(b).finish(),
            Predicate::Implies(a, b) => f.debug_tuple("Implies").field(a).field(b).finish(),
            Predicate::True => f.write_str("True"),
            Predicate::False => f.write_str("False"),
        }
    }
}

impl<S> PartialEq for Predicate<S>
where
    S: ProverSyntax,
    S::PredicateExpr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Predicate::PredicateExpression(a), Predicate::PredicateExpression(b)) => a == b,
            (Predicate::And(a1, b1), Predicate::And(a2, b2))
            | (Predicate::Or(a1, b1), Predicate::Or(a2, b2))
            | (Predicate::Implies(a1, b1), Predicate::Implies(a2, b2)) => a1 == a2 && b1 == b2,
            (Predicate::True, Predicate::True) | (Predicate::False, Predicate::False) => true,
            _ => false,
        }
    }
}

impl<S> Eq for Predicate<S>
where
    S: ProverSyntax,
    S::PredicateExpr: Eq,
{
}

impl<S> core::hash::Hash for Predicate<S>
where
    S: ProverSyntax,
    S::PredicateExpr: core::hash::Hash,
{
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        match self {
            Predicate::PredicateExpression(predicate) => {
                core::hash::Hash::hash(&0_u8, state);
                core::hash::Hash::hash(predicate, state);
            }
            Predicate::And(a, b) => {
                core::hash::Hash::hash(&1_u8, state);
                core::hash::Hash::hash(a, state);
                core::hash::Hash::hash(b, state);
            }
            Predicate::Or(a, b) => {
                core::hash::Hash::hash(&2_u8, state);
                core::hash::Hash::hash(a, state);
                core::hash::Hash::hash(b, state);
            }
            Predicate::Implies(a, b) => {
                core::hash::Hash::hash(&3_u8, state);
                core::hash::Hash::hash(a, state);
                core::hash::Hash::hash(b, state);
            }
            Predicate::True => core::hash::Hash::hash(&4_u8, state),
            Predicate::False => core::hash::Hash::hash(&5_u8, state),
        }
    }
}
