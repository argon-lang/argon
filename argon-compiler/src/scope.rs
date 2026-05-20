use argon_expr::{ExprContext, Variable};
use argon_parser::ast::IdentifierExpr;

pub trait Scope {
    type ExprContext: ExprContext;

    fn lookup(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext>;
    fn lookup_assign(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext>;
}

pub trait LocalScope: Scope {
    fn add_variable(&mut self, variable: Variable<Self::ExprContext>);

    fn has_variable(&self, variable: &Variable<Self::ExprContext>) -> bool;
}

pub enum Lookup<EC: ExprContext + ?Sized> {
    Empty,
    Variable(Variable<EC>),
    Overloadable(OverloadLookup),
}

pub struct OverloadLookup {
    item_groups: Box<dyn Iterator<Item = Vec<Overloadable>>>,
}

pub enum Overloadable {}
