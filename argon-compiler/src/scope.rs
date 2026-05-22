use std::collections::{HashMap, HashSet};
use argon_expr::{ExprContext, ExprContextShifter, Variable};
use argon_parser::ast::IdentifierExpr;

pub trait Scope {
    type ExprContext: ExprContext + ?Sized;

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





pub struct LocalVariableScope<'a, EC: ExprContext + ?Sized> {
    parent: &'a mut dyn Scope<ExprContext = EC>,
    variable_lookup: HashMap<IdentifierExpr, Variable<EC>>,
    variables: HashSet<Variable<EC>>,
}

impl<'a, EC: ExprContext + ?Sized> LocalVariableScope<'a, EC> {
    pub fn new(parent: &'a mut dyn Scope<ExprContext = EC>) -> Self {
        Self {
            parent,
            variable_lookup: HashMap::new(),
            variables: HashSet::new(),
        }
    }

    fn lookup_variable(&mut self, name: &IdentifierExpr) -> Option<Lookup<EC>> {
        self.variable_lookup.get(name)
            .map(|variable| Lookup::Variable(variable.clone()))
    }
}

impl <'a, EC: ExprContext + ?Sized> Scope for LocalVariableScope<'a, EC> {
    type ExprContext = EC;

    fn lookup(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext> {
        self.lookup_variable(name)
            .unwrap_or_else(|| self.parent.lookup(name))
    }

    fn lookup_assign(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext> {
        self.lookup_variable(name)
            .unwrap_or_else(|| self.parent.lookup_assign(name))
    }
}

impl <'a, EC: ExprContext + ?Sized> LocalScope for LocalVariableScope<'a, EC> {
    fn add_variable(&mut self, variable: Variable<EC>) {
        if let Some(name) = &variable.name() {
            self.variable_lookup.insert((*name).clone(), variable.clone());
        }

        self.variables.insert(variable);
    }

    fn has_variable(&self, variable: &Variable<Self::ExprContext>) -> bool {
        self.variables.contains(variable)
    }
}


pub struct ShiftedScope<'a, Sc: ?Sized, Sh> {
    inner: &'a mut Sc,
    shifter: Sh,
}

impl <'a, Sc: ?Sized, Sh> ShiftedScope<'a, Sc, Sh> {
    pub fn new(inner: &'a mut Sc, shifter: Sh) -> Self {
        Self { inner, shifter }
    }
}

impl <'a, Sc, Sh> ShiftedScope<'a, Sc, Sh>
where
    Sc: Scope + ?Sized,
    Sh: ExprContextShifter<EC1 = Sc::ExprContext>,
{
    fn shift_lookup(&self, lookup: Lookup<Sh::EC1>) -> Lookup<Sh::EC2> {
        match lookup {
            Lookup::Empty => Lookup::Empty,
            Lookup::Overloadable(o) => Lookup::Overloadable(o),

            Lookup::Variable(v) =>
                Lookup::Variable(self.shifter.shift_variable(v)),
        }
    }
}

impl <'a, Sc, Sh> Scope for ShiftedScope<'a, Sc, Sh>
    where
        Sc: Scope + ?Sized,
        Sh: ExprContextShifter<EC1 = Sc::ExprContext>,
{
    type ExprContext = Sh::EC2;

    fn lookup(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext> {
        let lookup = self.inner.lookup(name);
        self.shift_lookup(lookup)
    }

    fn lookup_assign(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext> {
        let lookup = self.inner.lookup_assign(name);
        self.shift_lookup(lookup)
    }
}


