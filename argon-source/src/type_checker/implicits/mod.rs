use crate::type_checker::implicits::prolog::PrologImplicitResolver;
use crate::type_checker::{Model, TypeCheckExprContext};
use argon_compiler::{Context, Function};
use argon_expr::{Expr, Variable};
use hashbrown::HashMap;
use parse18_runtime::Location;
use std::sync::Arc;

mod prolog;

pub struct ImplicitResolverInput<'a> {
    pub context: Context,
    pub resolve_location: &'a Location,
    pub model: &'a mut Model,
    pub given_assertions: Vec<ImplicitValue>,
    pub known_var_values: HashMap<Variable<TypeCheckExprContext>, Expr<TypeCheckExprContext>>,
}

impl<'a> ImplicitResolverInput<'a> {
    pub fn try_resolve_implicit(
        self,
        t: &Expr<TypeCheckExprContext>,
    ) -> Option<Expr<TypeCheckExprContext>> {
        PrologImplicitResolver {
            location: self.resolve_location,
            fuel: self.context.prolog_fuel(),
            given_assertions: &self.given_assertions,
        }
        .try_resolve_implicit(t, self.model)
    }
}

pub enum ImplicitValue {
    OfVar(Variable<TypeCheckExprContext>),
    OfFunction(Arc<dyn Function>),
}

trait ImplicitResolver {
    fn try_resolve_implicit(
        self,
        t: &Expr<TypeCheckExprContext>,
        model: &mut Model,
    ) -> Option<Expr<TypeCheckExprContext>>;
}
