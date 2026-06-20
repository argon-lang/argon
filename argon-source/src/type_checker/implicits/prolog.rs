use super::ImplicitResolver;
use crate::type_checker::{
    ExprNormalizer, Hole, Model, TypeCheckExprContext, UnifyModel, default_to_type_check_shifter,
    unify_hole_impl,
};
use alloc::borrow::Cow;
use alloc::rc::Rc;
use argon_compiler::ImplicitValue;
use argon_expr::{
    Builtin, ClosureParameterVariable, Expr, ExprContext, ExprScannerMut, ExpressionOwner,
    SubstScanner, Unify, Variable,
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

impl ImplicitResolver for PrologImplicitResolver<'_> {
    fn try_resolve_implicit(
        self,
        t: &Expr<TypeCheckExprContext>,
        model: &mut Model,
    ) -> Option<Expr<TypeCheckExprContext>> {
        let prover = PrologProver::new(&self, self.fuel.clone());

        let goal = expr_to_predicate(t.clone());

        let proof_result = prover.check(Rc::new(goal), model);

        match proof_result {
            ProofResult::Yes(proof) => {
                let e = proof_to_expr(proof).unwrap_or_else(|| {
                    Expr::Builtin(Builtin::UnsafeAssumeErased {
                        r#type: Box::new(t.clone()),
                    })
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
        _model: &Self::Model,
    ) -> Vec<(Proof<Self::ProofAtom>, Predicate<Self::Syntax>)> {
        let mut assertions = Vec::new();

        // Add A == A assertion
        {
            // TODO: Pick better type for t
            let t = Hole::new(
                self.location.clone(),
                Expr::Type(Box::new(Expr::IntLiteral(BigInt::ZERO))),
            );
            let a = Hole::new(
                self.location.clone(),
                Expr::Type(Box::new(Expr::IntLiteral(BigInt::ZERO))),
            );
            assertions.push((
                Proof::Atomic(TCAtomicProof::ExprProof(Expr::Builtin(
                    Builtin::EqualToRefl {
                        r#type: Box::new(Expr::Hole(t.clone())),
                        value: Box::new(Expr::Hole(a.clone())),
                    },
                ))),
                Predicate::PredicateExpression(ExprWithExprType(
                    ExprType::InhabitedType,
                    Expr::EqualToType {
                        r#type: Box::new(Expr::Hole(t)),
                        lhs: Box::new(Expr::Hole(a.clone())),
                        rhs: Box::new(Expr::Hole(a)),
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
                    proof_expr = Expr::Variable(v.clone());
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
                                arg = Expr::Hole(hole);
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
                                arg = Expr::Variable(Variable::ClosureParameter(Box::new(
                                    closure_param,
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

                    let mut witness = Expr::FunctionCall {
                        function: f.clone(),
                        arguments,
                    };

                    let mut assertion_type = sig.return_type;
                    subst.scan(&mut assertion_type);

                    for param in closure_params.into_iter().rev() {
                        witness = Expr::Closure {
                            v: Box::new(param.clone()),
                            return_type: Box::new(assertion_type.clone()),
                            body: Box::new(witness),
                        };

                        assertion_type = Expr::FunctionType {
                            a: Box::new(param),
                            r: Box::new(assertion_type),
                        };
                    }

                    proof_expr = witness;
                    predicate_expr = assertion_type;
                }
            }

            let proof = Proof::Atomic(TCAtomicProof::ExprProof(proof_expr));
            let assertion_type = expr_to_predicate_bool(predicate_expr);

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

        unify.unify(f1.1.clone(), f2.1.clone())
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
    ExprProof(Expr<TypeCheckExprContext>),
}

pub(super) struct ExprProverSyntax;

impl ProverSyntax for ExprProverSyntax {
    type Variable = Hole;
    type PredicateExpr = ExprWithExprType;
}

fn expr_to_predicate(expr: Expr<TypeCheckExprContext>) -> Predicate<ExprProverSyntax> {
    match expr {
        Expr::ConjunctionType { lhs, rhs } => Predicate::And(
            Rc::new(expr_to_predicate(*lhs)),
            Rc::new(expr_to_predicate(*rhs)),
        ),

        Expr::DisjunctionType { lhs, rhs } => Predicate::Or(
            Rc::new(expr_to_predicate(*lhs)),
            Rc::new(expr_to_predicate(*rhs)),
        ),

        Expr::FunctionType { a, r } => Predicate::Implies(
            Rc::new(expr_to_predicate(a.var_type)),
            Rc::new(expr_to_predicate(*r)),
        ),

        Expr::EqualToType {
            r#type: _,
            lhs,
            rhs,
        } => {
            let a = Rc::new(expr_to_predicate(*lhs));
            let b = Rc::new(expr_to_predicate(*rhs));

            Predicate::And(
                Rc::new(Predicate::Implies(a.clone(), b.clone())),
                Rc::new(Predicate::Implies(b, a)),
            )
        }

        _ => {
            Predicate::PredicateExpression(ExprWithExprType(ExprType::InhabitedType, expr.clone()))
        }
    }
}

fn expr_to_predicate_bool(expr: Expr<TypeCheckExprContext>) -> Predicate<ExprProverSyntax> {
    match expr {
        Expr::BoolLiteral(true) => Predicate::True,
        Expr::BoolLiteral(false) => Predicate::False,
        Expr::And(a, b) => Predicate::And(
            Rc::new(expr_to_predicate_bool(*a)),
            Rc::new(expr_to_predicate_bool(*b)),
        ),

        Expr::Or(a, b) => Predicate::Or(
            Rc::new(expr_to_predicate_bool(*a)),
            Rc::new(expr_to_predicate_bool(*b)),
        ),

        _ => Predicate::PredicateExpression(ExprWithExprType(ExprType::BoolValue, expr.clone())),
    }
}

fn proof_to_expr(p: Proof<TCAtomicProof>) -> Option<Expr<TypeCheckExprContext>> {
    Some(match p {
        Proof::Atomic(TCAtomicProof::ExprProof(expr)) => expr,
        Proof::ModusPonens {
            premise,
            implication,
        } => {
            let premise = proof_to_expr(*premise)?;
            let implication = proof_to_expr(*implication)?;
            Expr::FunctionObjectCall {
                function: Box::new(implication),
                argument: Box::new(premise),
            }
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

    fn unify_hole(&mut self, a: <Self::EC as ExprContext>::Hole, b: Expr<Self::EC>) -> bool {
        unify_hole_impl(self, a, b)
    }
}

impl UnifyModel for PrologUnify<'_> {
    fn model(&mut self) -> &mut Model {
        self.model
    }
}
