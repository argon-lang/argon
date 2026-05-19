use argon_expr::Variable;
use argon_parser::ast::IdentifierExpr;

pub trait Scope {
    fn lookup(&mut self, name: &IdentifierExpr) -> Lookup;
    fn lookup_assign(&mut self, name: &IdentifierExpr) -> Lookup;
}

pub trait LocalScope: Scope {
    fn add_variable(&mut self, variable: Variable);

    fn has_variable(&self, variable: &Variable) -> bool;
}

pub enum Lookup {
    Variable(Variable),
    Overloadable(OverloadLookup),
}

pub struct OverloadLookup {
    item_groups: Box<dyn Iterator<Item = Vec<Overloadable>>>,
}

pub enum Overloadable {}
