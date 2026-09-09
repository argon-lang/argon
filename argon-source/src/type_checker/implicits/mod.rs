use crate::type_checker::implicits::prolog::PrologImplicitResolver;
use crate::type_checker::implicits::z3resolver::Z3ImplicitResolver;
use crate::type_checker::{Model, TypeCheckExprContext};
use alloc::vec::Vec;
use argon_compiler::{Context, ImplicitValue};
use argon_expr::{LocatedExpr, Variable};
use hashbrown::HashMap;
use parse18_runtime::Location;

mod prolog;
mod z3resolver;

pub struct ImplicitResolverInput<'a> {
    pub context: Context,
    pub resolve_location: &'a Location,
    pub model: &'a mut Model,
    pub given_assertions: Vec<ImplicitValue<TypeCheckExprContext>>,
    pub known_var_values:
        HashMap<Variable<TypeCheckExprContext>, LocatedExpr<TypeCheckExprContext>>,
}

impl<'a> ImplicitResolverInput<'a> {
    pub fn try_resolve_implicit(
        self,
        t: &LocatedExpr<TypeCheckExprContext>,
    ) -> Option<LocatedExpr<TypeCheckExprContext>> {
        let result = PrologImplicitResolver {
            location: self.resolve_location,
            fuel: self.context.prolog_fuel(),
            given_assertions: &self.given_assertions,
        }
        .try_resolve_implicit(t, self.model);

        if result.is_some() {
            return result;
        }

        Z3ImplicitResolver {
            context: self.context,
            location: self.resolve_location,
            given_assertions: &self.given_assertions,
            known_var_values: &self.known_var_values,
        }
        .try_resolve_implicit(t, self.model)
    }
}

trait ImplicitResolver {
    fn try_resolve_implicit(
        self,
        t: &LocatedExpr<TypeCheckExprContext>,
        model: &mut Model,
    ) -> Option<LocatedExpr<TypeCheckExprContext>>;
}
