use super::ImplicitResolver;
use crate::type_checker::{
    ExprNormalizer, Hole, Model, TypeCheckExprContext, UnifyModel, default_to_type_check_shifter,
    unify_hole_impl,
};
use alloc::borrow::Cow;
use alloc::rc::Rc;
use argon_compiler::ImplicitValue;
use argon_expr::{
    Builtin, ClosureParameterVariable, Expr, ExprContext, ExprLocationExt, ExprScannerMut,
    ExpressionOwner, LocatedExpr, NormalizerScanner, SubstScanner, Unify, Variable,
};
use argon_parser::ast::FunctionParameterListType;
use argon_prover::{
    PartialProofResult, Predicate, PrologProver, PrologProverContext, Proof, ProofResult, Prover,
    ProverContext, ProverSyntax,
};
use argon_util::{Fuel, UniqueIdentifier};
use num_bigint::BigInt;
use parse18_runtime::Location;

pub(super) struct PrologImplicitResolver<'a> {
    pub location: &'a Location,
    pub fuel: Fuel,
    pub given_assertions: &'a [ImplicitValue<TypeCheckExprContext>],
}

impl PrologImplicitResolver<'_> {
    fn located(&self, expr: Expr<TypeCheckExprContext>) -> LocatedExpr<TypeCheckExprContext> {
        expr.with_location(self.location.clone())
    }

    fn expr_to_predicate(
        &self,
        mut expr: LocatedExpr<TypeCheckExprContext>,
        model: &mut Model,
    ) -> Rc<Predicate<ExprProverSyntax>> {
        {
            let mut norm = NormalizerScanner::new(self.fuel.clone(), ExprNormalizer { model });

            norm.normalize(&mut expr);
        }

        match expr.value {
            Expr::ConjunctionType { lhs, rhs } => {
                return Rc::new(Predicate::And(
                    self.expr_to_predicate(*lhs, model),
                    self.expr_to_predicate(*rhs, model),
                ));
            }

            Expr::DisjunctionType { lhs, rhs } => {
                return Rc::new(Predicate::Or(
                    self.expr_to_predicate(*lhs, model),
                    self.expr_to_predicate(*rhs, model),
                ));
            }

            Expr::FunctionType { a, r } => {
                return Rc::new(Predicate::Implies(
                    self.expr_to_predicate(a.var_type, model),
                    self.expr_to_predicate(*r, model),
                ));
            }

            Expr::EqualToType {
                r#type: mut t,
                lhs,
                rhs,
            } => {
                {
                    let mut norm =
                        NormalizerScanner::new(self.fuel.clone(), ExprNormalizer { model });

                    norm.normalize(&mut t);
                }

                match &t.value {
                    Expr::Builtin(Builtin::BoolType) => {
                        let a = self.expr_to_predicate_bool(lhs.value);
                        let b = self.expr_to_predicate_bool(rhs.value);

                        return match (a.as_ref(), b.as_ref()) {
                            (Predicate::True, Predicate::True) => Rc::new(Predicate::True),
                            (Predicate::True, Predicate::False) => Rc::new(Predicate::False),
                            (Predicate::False, Predicate::True) => Rc::new(Predicate::False),
                            (Predicate::False, Predicate::False) => Rc::new(Predicate::True),
                            (_, Predicate::True) => a,
                            (_, Predicate::False) => Rc::new(Predicate::Implies(a, b)),
                            (Predicate::True, _) => b,
                            (Predicate::False, _) => Rc::new(Predicate::Implies(b, a)),
                            _ => Rc::new(Predicate::And(
                                Rc::new(Predicate::Implies(a.clone(), b.clone())),
                                Rc::new(Predicate::Implies(b, a)),
                            )),
                        };
                    }
                    _ => {
                        expr.value = Expr::EqualToType {
                            r#type: t,
                            lhs,
                            rhs,
                        };
                    }
                }
            }

            _ => {}
        }

        Rc::new(Predicate::PredicateExpression(ExprWithExprType(
            ExprType::InhabitedType,
            expr.value,
        )))
    }

    fn expr_to_predicate_bool(
        &self,
        expr: Expr<TypeCheckExprContext>,
    ) -> Rc<Predicate<ExprProverSyntax>> {
        match expr {
            Expr::BoolLiteral(true) => Rc::new(Predicate::True),
            Expr::BoolLiteral(false) => Rc::new(Predicate::False),
            Expr::And(a, b) => Rc::new(Predicate::And(
                self.expr_to_predicate_bool(a.value),
                self.expr_to_predicate_bool(b.value),
            )),

            Expr::Or(a, b) => Rc::new(Predicate::Or(
                self.expr_to_predicate_bool(a.value),
                self.expr_to_predicate_bool(b.value),
            )),

            _ => Rc::new(Predicate::PredicateExpression(ExprWithExprType(
                ExprType::BoolValue,
                expr.clone(),
            ))),
        }
    }
}

impl ImplicitResolver for PrologImplicitResolver<'_> {
    fn try_resolve_implicit(
        self,
        t: &LocatedExpr<TypeCheckExprContext>,
        model: &mut Model,
    ) -> Option<LocatedExpr<TypeCheckExprContext>> {
        let prover = PrologProver::new(&self, self.fuel.clone());

        let goal = self.expr_to_predicate(t.clone(), model);

        let proof_result = prover.check(goal, model);

        match proof_result {
            ProofResult::Yes(proof) => {
                let e = proof_to_expr(proof, self.location).unwrap_or_else(|| {
                    Expr::Builtin(Builtin::UnsafeAssumeErased {
                        r#type: Box::new(t.clone()),
                    })
                    .with_location(self.location.clone())
                });
                Some(e)
            }

            _ => None,
        }
    }
}

impl ProverContext for PrologImplicitResolver<'_> {
    type Syntax = ExprProverSyntax;
    type ProofAtom = TCAtomicProof;
    type Model = Model;

    fn fresh_assertions(
        &self,
        model: &Self::Model,
    ) -> Vec<(Proof<Self::ProofAtom>, Predicate<Self::Syntax>)> {
        let mut assertions = Vec::new();

        // Add A == A assertion
        {
            // TODO: Pick better type for t
            let t = Hole::new(
                self.location.clone(),
                self.located(Expr::Type(Box::new(
                    self.located(Expr::IntLiteral(BigInt::ZERO)),
                ))),
            );
            let a = Hole::new(
                self.location.clone(),
                self.located(Expr::Type(Box::new(
                    self.located(Expr::IntLiteral(BigInt::ZERO)),
                ))),
            );
            assertions.push((
                Proof::Atomic(TCAtomicProof::ExprProof(self.located(Expr::Builtin(
                    Builtin::EqualToRefl {
                        r#type: Box::new(self.located(Expr::Hole(t.clone()))),
                        value: Box::new(self.located(Expr::Hole(a.clone()))),
                    },
                )))),
                Predicate::PredicateExpression(ExprWithExprType(
                    ExprType::InhabitedType,
                    Expr::EqualToType {
                        r#type: Box::new(self.located(Expr::Hole(t))),
                        lhs: Box::new(self.located(Expr::Hole(a.clone()))),
                        rhs: Box::new(self.located(Expr::Hole(a))),
                    },
                )),
            ));
        }

        assertions.extend(self.given_assertions.iter().filter_map(|v| {
            let proof_expr;
            let predicate_expr;

            let mut arguments = Vec::new();
            let mut closure_params = Vec::new();

            match v {
                ImplicitValue::OfVar(v) => {
                    proof_expr = Expr::Variable(v.clone()).with_location(self.location.clone());
                    predicate_expr = v.var_type().clone();
                }
                ImplicitValue::OfFunction(f) => {
                    let sig = f.clone().signature().as_ref().clone();
                    let sig = sig.shift(&mut default_to_type_check_shifter());

                    let mut subst = SubstScanner::new();

                    for (i, mut param) in sig.parameters.into_iter().enumerate() {
                        param.scan_mut(&mut subst);

                        let arg;

                        match param.list_type {
                            FunctionParameterListType::InferrableList(_) => {
                                let hole =
                                    Hole::new(self.location.clone(), param.param_type.clone());
                                arg = self.located(Expr::Hole(hole));
                            }
                            FunctionParameterListType::RequiresList => {
                                let var_id = UniqueIdentifier::new();
                                let closure_param = ClosureParameterVariable {
                                    id: var_id,
                                    var_type: param.param_type.clone(),
                                    name: param.name.clone(),
                                    is_mutable: false,
                                    erasure_mode: param.erasure_mode,
                                    is_witness: false,
                                };
                                closure_params.push(closure_param.clone());
                                arg = self.located(Expr::Variable(Variable::ClosureParameter(
                                    Box::new(closure_param),
                                )));
                            }

                            FunctionParameterListType::NormalList => return None,
                        }

                        let variable =
                            param.to_parameter_var(ExpressionOwner::Function(f.clone()), i);

                        subst.add_substitution(
                            Variable::Parameter(Box::new(variable.clone())),
                            Cow::Owned(arg.clone()),
                        );
                        arguments.push(arg);
                    }

                    let mut witness = self.located(Expr::FunctionCall {
                        function: f.clone(),
                        arguments,
                    });

                    let mut assertion_type = sig.return_type;
                    subst.scan(&mut assertion_type);

                    for param in closure_params.into_iter().rev() {
                        witness = self.located(Expr::Closure {
                            v: Box::new(param.clone()),
                            return_type: Box::new(assertion_type.clone()),
                            body: Box::new(witness),
                        });

                        assertion_type = self.located(Expr::FunctionType {
                            a: Box::new(param),
                            r: Box::new(assertion_type),
                        });
                    }

                    proof_expr = witness;
                    predicate_expr = assertion_type;
                }
            }

            let proof = Proof::Atomic(TCAtomicProof::ExprProof(proof_expr));

            let mut model = model.clone();
            let assertion_type =
                Rc::unwrap_or_clone(self.expr_to_predicate(predicate_expr, &mut model));

            Some((proof, assertion_type))
        }));

        assertions
    }

    fn unify_predicate_expression(
        &self,
        f1: &<Self::Syntax as ProverSyntax>::PredicateExpr,
        f2: &<Self::Syntax as ProverSyntax>::PredicateExpr,
        model: &mut Rc<Self::Model>,
        fuel: Fuel,
    ) -> bool {
        if f1.0 != f2.0 {
            return false;
        }

        let mut unify = PrologUnify {
            normalize_fuel: fuel,
            model: Rc::make_mut(model),
        };

        unify.unify(
            f1.1.clone().with_location(self.location.clone()),
            f2.1.clone().with_location(self.location.clone()),
        )
    }
}

impl<'a> PrologProverContext for PrologImplicitResolver<'a> {
    fn intrinsic_predicate<'b>(
        &'b self,
        _predicate: &<Self::Syntax as ProverSyntax>::PredicateExpr,
        _model: Rc<Self::Model>,
        _prover: PrologProver<Self>,
    ) -> Box<dyn Iterator<Item = PartialProofResult<Self>> + 'b> {
        Box::new(core::iter::empty())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum ExprType {
    InhabitedType,
    BoolValue,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct ExprWithExprType(pub ExprType, pub Expr<TypeCheckExprContext>);

#[derive(Debug, Clone)]
pub(super) enum TCAtomicProof {
    ExprProof(LocatedExpr<TypeCheckExprContext>),
}

pub(super) struct ExprProverSyntax;

impl ProverSyntax for ExprProverSyntax {
    type Variable = Hole;
    type PredicateExpr = ExprWithExprType;
}

fn proof_to_expr(
    p: Proof<TCAtomicProof>,
    location: &Location,
) -> Option<LocatedExpr<TypeCheckExprContext>> {
    Some(match p {
        Proof::Atomic(TCAtomicProof::ExprProof(expr)) => expr,
        Proof::ModusPonens {
            premise,
            implication,
        } => {
            let premise = proof_to_expr(*premise, location)?;
            let implication = proof_to_expr(*implication, location)?;
            Expr::FunctionObjectCall {
                function: Box::new(implication),
                argument: Box::new(premise),
            }
            .with_location(location.clone())
        }
        _ => return None,
    })
}

struct PrologUnify<'a> {
    normalize_fuel: Fuel,
    model: &'a mut Model,
}

impl Unify for PrologUnify<'_> {
    type EC = TypeCheckExprContext;
    type Model = Model;
    type Norm<'a>
        = ExprNormalizer<'a>
    where
        Self: 'a;

    fn model(&self) -> &Self::Model {
        &self.model
    }

    fn model_mut(&mut self) -> &mut Self::Model {
        &mut self.model
    }

    fn normalize_fuel(&self) -> Fuel {
        self.normalize_fuel.clone()
    }

    fn normalizer<'a>(model: &'a mut Self::Model) -> Self::Norm<'a>
    where
        Self: 'a,
    {
        ExprNormalizer { model }
    }

    fn unify_hole(&mut self, a: <Self::EC as ExprContext>::Hole, b: LocatedExpr<Self::EC>) -> bool {
        unify_hole_impl(self, a, b)
    }
}

impl UnifyModel for PrologUnify<'_> {
    fn model(&mut self) -> &mut Model {
        self.model
    }
}
