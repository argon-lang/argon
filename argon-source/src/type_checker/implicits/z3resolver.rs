use crate::type_checker::implicits::ImplicitResolver;
use crate::type_checker::{
    ExprNormalizer, Hole, Model, TypeCheckExprContext, default_to_type_check_shifter,
};
use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::vec::Vec;
use argon_compiler::z3expr::Z3Expr;
use argon_compiler::{Context, ImplicitValue};
use argon_expr::{
    Builtin, Expr, ExprLocationExt, ExprScannerMut, ExpressionOwner, FullNormalizer, LocatedExpr,
    SubstScanner, Variable,
};
use argon_parser::ast::FunctionParameterListType;
use argon_z3::ast::{self, Ast, Bool, Dynamic};
use argon_z3::{Context as Z3Context, SatResult};
use hashbrown::HashMap;
use parse18_runtime::Location;

pub struct Z3ImplicitResolver<'a> {
    pub context: Context,
    pub location: &'a Location,
    pub given_assertions: &'a [ImplicitValue<TypeCheckExprContext>],
    pub known_var_values:
        &'a HashMap<Variable<TypeCheckExprContext>, LocatedExpr<TypeCheckExprContext>>,
}

impl Z3ImplicitResolver<'_> {
    fn loc(&self, expr: Expr<TypeCheckExprContext>) -> LocatedExpr<TypeCheckExprContext> {
        expr.with_location(self.location.clone())
    }

    fn add_known_var_value_assertions<'z3>(
        &self,
        z3expr: &mut Z3Expr<'z3, TypeCheckExprContext>,
        model: &mut Model,
    ) {
        for (variable, value) in self.known_var_values {
            let mut variable = self.loc(Expr::Variable(variable.clone()));
            let mut value = value.clone();

            let mut norm =
                FullNormalizer::new(self.context.normalize_fuel(), ExprNormalizer { model });
            norm.normalize(&mut variable);
            norm.normalize(&mut value);

            let variable = z3expr.expr_to_z3(&variable);
            let value = z3expr.expr_to_z3(&value);
            z3expr.solver().assert(variable.equals(&value));
        }
    }

    fn add_given_assertion<'z3>(
        &self,
        z3expr: &mut Z3Expr<'z3, TypeCheckExprContext>,
        assertion: &ImplicitValue<TypeCheckExprContext>,
        model: &mut Model,
    ) {
        let assertion = match assertion {
            ImplicitValue::OfVar(v) => self.convert_expr(z3expr, v.var_type().clone(), model),
            ImplicitValue::OfFunction(f) => {
                let sig = f
                    .clone()
                    .signature()
                    .as_ref()
                    .clone()
                    .shift(&mut default_to_type_check_shifter());
                let owner = ExpressionOwner::Function(f.clone());
                let mut subst = SubstScanner::new();
                let mut binders = Vec::new();

                for (i, mut param) in sig.parameters.into_iter().enumerate() {
                    param.scan_mut(&mut subst);

                    let variable = param.clone().to_parameter_var(owner.clone(), i);
                    let arg = self.loc(Expr::Hole(Hole::new(
                        self.location.clone(),
                        param.param_type.clone(),
                    )));
                    subst.add_substitution(
                        Variable::Parameter(Box::new(variable)),
                        Cow::Owned(arg.clone()),
                    );

                    match param.list_type {
                        FunctionParameterListType::InferrableList(_) => {
                            binders.push(AssertionBinder::ForAll(z3expr.expr_to_z3(&arg)));
                        }
                        FunctionParameterListType::RequiresList => {
                            subst.scan(&mut param.param_type);

                            binders.push(AssertionBinder::Implies(self.convert_expr(
                                z3expr,
                                param.param_type,
                                model,
                            )));
                        }
                        FunctionParameterListType::NormalList => return,
                    }
                }

                let mut assertion_type = sig.return_type;
                subst.scan(&mut assertion_type);

                let mut assertion = self.convert_expr(z3expr, assertion_type, model);
                for binder in binders.into_iter().rev() {
                    assertion = match binder {
                        AssertionBinder::ForAll(v) => ast::forall_const(&[&v], &[], &assertion),
                        AssertionBinder::Implies(premise) => premise.implies(&assertion),
                    };
                }
                assertion
            }
        };

        z3expr.solver().assert(assertion);
    }

    fn convert_expr<'z3>(
        &self,
        z3expr: &mut Z3Expr<'z3, TypeCheckExprContext>,
        mut expr: LocatedExpr<TypeCheckExprContext>,
        model: &mut Model,
    ) -> Bool<'z3> {
        {
            let mut norm =
                FullNormalizer::new(self.context.normalize_fuel(), ExprNormalizer { model });
            norm.normalize(&mut expr);
        }

        z3expr.inhabited(&expr)
    }
}

enum AssertionBinder<'z3> {
    ForAll(Dynamic<'z3>),
    Implies(Bool<'z3>),
}

impl ImplicitResolver for Z3ImplicitResolver<'_> {
    fn try_resolve_implicit(
        self,
        t: &LocatedExpr<TypeCheckExprContext>,
        model: &mut Model,
    ) -> Option<LocatedExpr<TypeCheckExprContext>> {
        let z3 = Z3Context::new();
        let mut z3expr = Z3Expr::new(&z3, self.context.clone());

        self.add_known_var_value_assertions(&mut z3expr, model);

        for assertion in self.given_assertions {
            self.add_given_assertion(&mut z3expr, assertion, model);
        }

        let goal = self.convert_expr(&mut z3expr, t.clone(), model);
        z3expr.solver().assert(goal.not());

        let sat_result = z3expr.solver().check();

        match sat_result {
            SatResult::Sat | SatResult::Unknown => None,
            SatResult::Unsat => Some(self.loc(Expr::Builtin(Builtin::UnsafeAssumeErased {
                r#type: Box::new(t.clone()),
            }))),
        }
    }
}
