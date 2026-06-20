use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::vec::Vec;
use core::convert::identity;
use core::hash::Hash;
use hashbrown::HashSet;

use crate::{
    PartialProofResult, Predicate, Proof, ProofResult, Prover, ProverContext, ProverSyntax,
};
use argon_util::{Fuel, UniqueIdentifier};

enum InferOneResult<'a, C: PrologProverContext + ?Sized> {
    Yes(
        Rc<dyn Fn(Proof<C::ProofAtom>) -> Proof<C::ProofAtom> + 'a>,
        Rc<C::Model>,
    ),
    No(
        Rc<dyn Fn(Proof<C::ProofAtom>) -> Proof<C::ProofAtom> + 'a>,
        Rc<C::Model>,
    ),
}

pub trait PrologProverContext: ProverContext
where
    <Self::Syntax as ProverSyntax>::PredicateExpr: Eq + Hash,
{
    fn intrinsic_predicate<'a>(
        &'a self,
        predicate: &<Self::Syntax as ProverSyntax>::PredicateExpr,
        model: Rc<Self::Model>,
        prover: PrologProver<Self>,
    ) -> Box<dyn Iterator<Item = PartialProofResult<Self>> + 'a>;
}

pub struct PrologProver<'a, C: PrologProverContext + ?Sized> {
    context: &'a C,

    fuel: Fuel,
    seen_predicates: Rc<HashSet<Rc<Predicate<C::Syntax>>>>,
    additional_givens: Rc<Vec<(Proof<C::ProofAtom>, Rc<Predicate<C::Syntax>>)>>,
}

impl<'a, C: PrologProverContext> Clone for PrologProver<'a, C> {
    fn clone(&self) -> Self {
        PrologProver {
            context: self.context,
            fuel: self.fuel.clone(),
            seen_predicates: Rc::clone(&self.seen_predicates),
            additional_givens: Rc::clone(&self.additional_givens),
        }
    }
}

impl<'a, C: PrologProverContext> Prover<'a> for PrologProver<'a, C> {
    type Context = C;

    fn new(context: &'a Self::Context, fuel: Fuel) -> Self {
        PrologProver {
            context,
            fuel,
            seen_predicates: Rc::new(HashSet::new()),
            additional_givens: Rc::new(Vec::new()),
        }
    }

    fn check(self, goal: Rc<Predicate<C::Syntax>>, model: &mut C::Model) -> ProofResult<C> {
        let model_ref = Rc::new(model.clone());
        let Some(result) = self.solve(goal, model_ref.clone()).next() else {
            return ProofResult::Unknown;
        };

        match result {
            PartialProofResult::Yes(proof, new_model) => {
                *model = (*new_model).clone();
                ProofResult::Yes(proof)
            }
            PartialProofResult::No(proof, new_model) => {
                *model = (*new_model).clone();
                ProofResult::No(proof)
            }
        }
    }
}

impl<'a, C: PrologProverContext> PrologProver<'a, C> {
    fn consume_fuel(mut self) -> Self {
        self.fuel.consume();
        self
    }

    fn add_given(mut self, proof: Proof<C::ProofAtom>, pred: Rc<Predicate<C::Syntax>>) -> Self {
        Rc::make_mut(&mut self.additional_givens).push((proof, pred));
        self
    }

    fn solve(
        mut self,
        goal: Rc<Predicate<C::Syntax>>,
        model: Rc<C::Model>,
    ) -> Box<dyn Iterator<Item = PartialProofResult<C>> + 'a> {
        if self.fuel.is_empty() || self.seen_predicates.contains(&goal) {
            return Box::new(core::iter::empty());
        }

        Rc::make_mut(&mut self.seen_predicates).insert(goal.clone());

        let goal = goal.clone();
        let inferred = self.clone().infer(goal.clone(), model.clone());
        let derived = core::iter::once_with(move || self.solve_derived(goal, model)).flatten();

        Box::new(inferred.chain(derived))
    }

    fn solve_derived(
        self,
        goal: Rc<Predicate<C::Syntax>>,
        model: Rc<C::Model>,
    ) -> Box<dyn Iterator<Item = PartialProofResult<C>> + 'a> {
        match &*goal {
            Predicate::And(a, b) => {
                let a = a.clone();
                let b = b.clone();
                let prover = self.consume_fuel();

                Box::new(prover.clone().solve(a.clone(), model).flat_map(
                    move |result| match result {
                        PartialProofResult::No(not_proof, model) => {
                            let proof = Proof::DeMorganOrPullNotOut(Box::new(
                                Proof::DisjunctIntroLeft(Box::new(not_proof)),
                            ));
                            Box::new(core::iter::once(PartialProofResult::No(proof, model)))
                                as Box<dyn Iterator<Item = PartialProofResult<C>>>
                        }

                        PartialProofResult::Yes(proof_a, model) => {
                            let proof_a = proof_a.clone();
                            let b = b.clone();
                            Box::new(prover.clone().solve(b, model).map(
                                move |result| match result {
                                    PartialProofResult::No(proof, model) => PartialProofResult::No(
                                        Proof::DeMorganOrPullNotOut(Box::new(
                                            Proof::DisjunctIntroRight(Box::new(proof)),
                                        )),
                                        model,
                                    ),

                                    PartialProofResult::Yes(proof_b, model) => {
                                        PartialProofResult::Yes(
                                            Proof::ConjunctIntro {
                                                a: Box::new(proof_a.clone()),
                                                b: Box::new(proof_b),
                                            },
                                            model,
                                        )
                                    }
                                },
                            ))
                        }
                    },
                ))
            }

            Predicate::Or(a, b) => {
                let a = a.clone();
                let b = b.clone();
                let commuted_goal = Rc::new(Predicate::Or(b.clone(), a.clone()));
                let commute_or = self
                    .clone()
                    .consume_fuel()
                    .infer(commuted_goal.clone(), model.clone())
                    .map(|result| match result {
                        PartialProofResult::No(proof, model) => PartialProofResult::No(
                            Proof::DeMorganAndPullNotOut(Box::new(Proof::ConjunctCommute(
                                Box::new(Proof::DeMorganOrPushNotIn(Box::new(proof))),
                            ))),
                            model,
                        ),

                        PartialProofResult::Yes(proof, model) => {
                            PartialProofResult::Yes(Proof::DisjunctCommute(Box::new(proof)), model)
                        }
                    });

                let prove_a_model = model.clone();
                let prove_a_prover = self.clone();
                let prove_a = core::iter::once_with(move || {
                    prove_a_prover
                        .clone()
                        .consume_fuel()
                        .solve(a.clone(), prove_a_model.clone())
                        .filter_map(|result| match result {
                            PartialProofResult::No(_, _) => None,
                            PartialProofResult::Yes(proof, model) => Some(PartialProofResult::Yes(
                                Proof::DisjunctIntroLeft(Box::new(proof)),
                                model,
                            )),
                        })
                })
                .flatten();

                let prove_b = core::iter::once_with(move || {
                    self.consume_fuel()
                        .solve(b.clone(), model)
                        .filter_map(|result| match result {
                            PartialProofResult::No(_, _) => None,
                            PartialProofResult::Yes(proof, model) => Some(PartialProofResult::Yes(
                                Proof::DisjunctIntroRight(Box::new(proof)),
                                model,
                            )),
                        })
                })
                .flatten();

                Box::new(commute_or.chain(prove_a).chain(prove_b))
            }

            Predicate::Implies(a, b) if matches!(**b, Predicate::False) => match &**a {
                Predicate::And(a, b) => {
                    let next_goal = Rc::new(Predicate::Or(
                        Rc::new(Predicate::Implies(a.clone(), Rc::new(Predicate::False))),
                        Rc::new(Predicate::Implies(b.clone(), Rc::new(Predicate::False))),
                    ));

                    Box::new(self.consume_fuel().solve(next_goal, model).map(
                        |result| match result {
                            PartialProofResult::No(proof, model) => {
                                let id = UniqueIdentifier::new();
                                PartialProofResult::No(
                                    Proof::HypotheticalSyllogism {
                                        p_implies_q: Box::new(Proof::ImplicationAbstraction {
                                            id: id.clone(),
                                            body: Box::new(Proof::DeMorganAndPushNotIn(Box::new(
                                                Proof::Identifier(id),
                                            ))),
                                        }),
                                        q_implies_r: Box::new(proof),
                                    },
                                    model,
                                )
                            }

                            PartialProofResult::Yes(proof, model) => PartialProofResult::Yes(
                                Proof::DeMorganOrPullNotOut(Box::new(proof)),
                                model,
                            ),
                        },
                    ))
                }

                Predicate::Or(a, b) => {
                    let next_goal = Rc::new(Predicate::And(
                        Rc::new(Predicate::Implies(a.clone(), Rc::new(Predicate::False))),
                        Rc::new(Predicate::Implies(b.clone(), Rc::new(Predicate::False))),
                    ));

                    Box::new(self.consume_fuel().solve(next_goal, model).map(
                        |result| match result {
                            PartialProofResult::No(not_proof, model) => {
                                let id = UniqueIdentifier::new();
                                PartialProofResult::No(
                                    Proof::HypotheticalSyllogism {
                                        p_implies_q: Box::new(Proof::ImplicationAbstraction {
                                            id: id.clone(),
                                            body: Box::new(Proof::DeMorganOrPushNotIn(Box::new(
                                                Proof::Identifier(id),
                                            ))),
                                        }),
                                        q_implies_r: Box::new(not_proof),
                                    },
                                    model,
                                )
                            }

                            PartialProofResult::Yes(proof, model) => PartialProofResult::Yes(
                                Proof::DeMorganAndPullNotOut(Box::new(proof)),
                                model,
                            ),
                        },
                    ))
                }

                Predicate::Implies(a, b) if matches!(**b, Predicate::False) => {
                    Box::new(self.consume_fuel().solve(a.clone(), model).map(
                        |result| match result {
                            PartialProofResult::No(not_proof, model) => PartialProofResult::No(
                                Proof::DoubleNegIntro(Box::new(not_proof)),
                                model,
                            ),

                            PartialProofResult::Yes(proof, model) => PartialProofResult::Yes(
                                Proof::DoubleNegIntro(Box::new(proof)),
                                model,
                            ),
                        },
                    ))
                }

                _ => Box::new(core::iter::empty()),
            },

            Predicate::Implies(a, b) => {
                let a_id = UniqueIdentifier::new();

                Box::new(
                    self.clone()
                        .add_given(Proof::Identifier(a_id.clone()), a.clone())
                        .solve(b.clone(), model)
                        .filter_map(move |result| match result {
                            PartialProofResult::Yes(proof, model) => Some(PartialProofResult::Yes(
                                Proof::ImplicationAbstraction {
                                    id: a_id.clone(),
                                    body: Box::new(proof),
                                },
                                model,
                            )),

                            PartialProofResult::No(_, _) => None,
                        }),
                )
            }

            Predicate::True => Box::new(core::iter::once(PartialProofResult::Yes(
                Proof::TrueIsTrue,
                model,
            ))),

            Predicate::False => Box::new(core::iter::empty()),

            Predicate::PredicateExpression(predicate) => self.context.intrinsic_predicate(
                predicate,
                model.clone(),
                self.clone().consume_fuel(),
            ),
        }
    }

    fn infer<'b>(
        self,
        goal: Rc<Predicate<C::Syntax>>,
        model: Rc<C::Model>,
    ) -> Box<dyn Iterator<Item = PartialProofResult<C>> + 'b>
    where
        'a: 'b,
    {
        if self.fuel.is_empty() {
            return Box::new(core::iter::empty());
        }

        let assertions: Vec<_> = self
            .context
            .fresh_assertions(&model)
            .into_iter()
            .map(|(proof, pred)| (proof, Rc::new(pred)))
            .chain(self.additional_givens.iter().cloned())
            .collect();

        Box::new(assertions.into_iter().flat_map(move |(proof, assertion)| {
            self.clone()
                .infer_one(goal.clone(), assertion, model.clone())
                .map(move |result| match result {
                    InferOneResult::No(not_proof_builder, model) => {
                        PartialProofResult::No(not_proof_builder(proof.clone()), model)
                    }

                    InferOneResult::Yes(proof_builder, model) => {
                        PartialProofResult::Yes(proof_builder(proof.clone()), model)
                    }
                })
        }))
    }

    fn infer_one(
        self,
        goal: Rc<Predicate<C::Syntax>>,
        assertion: Rc<Predicate<C::Syntax>>,
        model: Rc<C::Model>,
    ) -> Box<dyn Iterator<Item = InferOneResult<'a, C>> + 'a> {
        if let Predicate::Implies(..) = *goal {
            return Box::new(core::iter::empty());
        }

        let unify_assertion = self
            .unify_as_iterator(goal.clone(), assertion.clone(), model.clone())
            .map(|model| InferOneResult::Yes(Rc::new(identity), model));

        let assertion_specific = core::iter::once_with(move || match &*assertion {
            Predicate::Implies(premise, conclusion) => {
                if matches!(**premise, Predicate::False) {
                    Box::new(core::iter::empty()) as Box<dyn Iterator<Item = _>>
                } else if matches!(**conclusion, Predicate::False) {
                    Box::new(
                        self.unify_as_iterator(goal, assertion, model.clone())
                            .map(|model| InferOneResult::No(Rc::new(identity), model)),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    let premise = premise.clone();
                    let conclusion = conclusion.clone();

                    Box::new(
                        self.clone()
                            .infer_one(goal, conclusion.clone(), model.clone())
                            .flat_map(move |infer_res| match infer_res {
                                InferOneResult::No(not_proof_builder, model) => Box::new(
                                    self.clone()
                                        .consume_fuel()
                                        .solve(premise.clone(), model)
                                        .flat_map(move |proof_res| {
                                            let not_proof_builder = not_proof_builder.clone();
                                            match proof_res {
                                                PartialProofResult::Yes(premise_proof, model) => {
                                                    Box::new(core::iter::once(InferOneResult::No(
                                                        Rc::new(move |proof| Proof::ModusPonens {
                                                            implication: Box::new(
                                                                not_proof_builder(proof),
                                                            ),
                                                            premise: Box::new(
                                                                premise_proof.clone(),
                                                            ),
                                                        }),
                                                        model,
                                                    )))
                                                        as Box<dyn Iterator<Item = _>>
                                                }

                                                _ => Box::new(core::iter::empty()),
                                            }
                                        }),
                                )
                                    as Box<dyn Iterator<Item = _>>,
                                InferOneResult::Yes(proof_builder, model) => Box::new(
                                    self.clone()
                                        .consume_fuel()
                                        .solve(premise.clone(), model)
                                        .flat_map(move |proof_res| {
                                            let proof_builder = proof_builder.clone();
                                            match proof_res {
                                                PartialProofResult::Yes(premise_proof, model) => {
                                                    Box::new(core::iter::once(InferOneResult::Yes(
                                                        Rc::new(move |proof| Proof::ModusPonens {
                                                            implication: Box::new(proof_builder(
                                                                proof,
                                                            )),
                                                            premise: Box::new(
                                                                premise_proof.clone(),
                                                            ),
                                                        }),
                                                        model,
                                                    )))
                                                        as Box<dyn Iterator<Item = _>>
                                                }

                                                _ => Box::new(core::iter::empty()),
                                            }
                                        }),
                                )
                                    as Box<dyn Iterator<Item = _>>,
                            }),
                    ) as Box<dyn Iterator<Item = _>>
                }
            }
            _ => Box::new(core::iter::empty()),
        })
        .flatten();

        Box::new(unify_assertion.chain(assertion_specific))
    }

    fn unify_as_iterator(
        &self,
        goal: Rc<Predicate<C::Syntax>>,
        rule: Rc<Predicate<C::Syntax>>,
        mut model: Rc<C::Model>,
    ) -> Box<dyn Iterator<Item = Rc<C::Model>> + 'a> {
        if self.try_unify(goal, rule, &mut model) {
            Box::new(core::iter::once(model))
        } else {
            Box::new(core::iter::empty())
        }
    }

    fn try_unify(
        &self,
        goal: Rc<Predicate<C::Syntax>>,
        rule: Rc<Predicate<C::Syntax>>,
        model: &mut Rc<C::Model>,
    ) -> bool {
        let model2 = model.clone();

        let result = self.clone().unify(goal, rule, model);
        if result {
            *model = model2;
        }

        result
    }

    #[allow(clippy::match_same_arms, reason = "Readability")]
    fn unify(
        &mut self,
        goal: Rc<Predicate<C::Syntax>>,
        rule: Rc<Predicate<C::Syntax>>,
        model: &mut Rc<C::Model>,
    ) -> bool {
        if self.fuel.is_empty() {
            return false;
        }

        match (&*goal, &*rule) {
            (Predicate::PredicateExpression(e1), Predicate::PredicateExpression(e2)) => self
                .context
                .unify_predicate_expression(e1, e2, model, self.fuel.clone()),

            (Predicate::And(left1, right1), Predicate::And(left2, right2)) => {
                self.unify(left1.clone(), left2.clone(), model)
                    && self.unify(right1.clone(), right2.clone(), model)
            }

            (Predicate::Or(left1, right1), Predicate::Or(left2, right2)) => {
                self.unify(left1.clone(), left2.clone(), model)
                    && self.unify(right1.clone(), right2.clone(), model)
            }

            (Predicate::Implies(left1, right1), Predicate::Implies(left2, right2)) => {
                self.unify(left1.clone(), left2.clone(), model)
                    && self.unify(right1.clone(), right2.clone(), model)
            }

            (Predicate::True, Predicate::True) => true,
            (Predicate::False, Predicate::False) => true,
            _ => false,
        }
    }
}
