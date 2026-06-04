use crate::signature::SignatureParameter;
use crate::{DefaultExprContext, Enum, Function, FunctionSignature, Instance, Method, Record, Trait};
use alloc::boxed::Box;
use alloc::{sync::Arc, vec::Vec};
use argon_expr::{
    ExprContext, ExprContextShifter, ExpressionOwner, Variable, VariableTupleElement,
};
use argon_parser::ast::Identifier;
use hashbrown::{HashMap, HashSet};

pub trait Scope {
    type ExprContext: ExprContext + ?Sized;

    fn lookup(&self, name: &Identifier) -> Lookup<Self::ExprContext>;
    fn lookup_assign(&self, name: &Identifier) -> Lookup<Self::ExprContext>;
}

pub trait LocalScope: Scope {
    fn add_variable(&mut self, variable: Variable<Self::ExprContext>);

    fn has_variable(&self, variable: &Variable<Self::ExprContext>) -> bool;
}

pub enum Lookup<EC: ExprContext + ?Sized> {
    Empty,
    Variable(Variable<EC>),
    VariableTupleElement(VariableTupleElement<EC>),
    Overloadable(OverloadLookup),
}

pub struct OverloadLookup {
    pub item_groups: Vec<Vec<Overloadable>>,
}

impl OverloadLookup {
    pub fn new(item_groups: Vec<Vec<Overloadable>>) -> Self {
        Self { item_groups }
    }

    pub fn into_item_groups(self) -> Vec<Vec<Overloadable>> {
        self.item_groups
    }
}

pub enum Overloadable {
    Function(Arc<dyn Function>),
    Method(Arc<dyn Method>),
    Record(Arc<dyn Record>),
    Enum(Arc<dyn Enum>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
}

impl Overloadable {
    pub fn as_expression_owner<EC>(&self) -> ExpressionOwner<EC>
    where
        EC: ExprContext<
                Function = <DefaultExprContext as ExprContext>::Function,
                Record = <DefaultExprContext as ExprContext>::Record,
                Enum = <DefaultExprContext as ExprContext>::Enum,
                Trait = <DefaultExprContext as ExprContext>::Trait,
                EnumVariant = <DefaultExprContext as ExprContext>::EnumVariant,
                Method = <DefaultExprContext as ExprContext>::Method,
                Instance = <DefaultExprContext as ExprContext>::Instance,
            > + ?Sized,
    {
        match self {
            Overloadable::Function(f) => ExpressionOwner::Function(f.clone()),
            Overloadable::Method(m) => ExpressionOwner::Method(m.clone()),
            Overloadable::Record(r) => ExpressionOwner::Record(r.clone()),
            Overloadable::Enum(e) => ExpressionOwner::Enum(e.clone()),
            Overloadable::Trait(t) => ExpressionOwner::Trait(t.clone()),
            Overloadable::Instance(i) => ExpressionOwner::Instance(i.clone()),
        }
    }

    pub fn signature(&self) -> Arc<FunctionSignature<DefaultExprContext>> {
        match self {
            Overloadable::Function(f) => f.clone().signature(),
            Overloadable::Record(r) => r.clone().signature(),
            Overloadable::Method(_) => todo!(),
            Overloadable::Enum(e) => e.clone().signature(),
            Overloadable::Trait(t) => t.clone().signature(),
            Overloadable::Instance(i) => todo!(),
        }
    }
}

pub struct ParameterScope<'a, EC: ExprContext + ?Sized> {
    parent: &'a mut dyn Scope<ExprContext = EC>,
    variable_lookup: HashMap<Identifier, Variable<EC>>,
    binding_lookup: HashMap<Identifier, VariableTupleElement<EC>>,
}

impl<'a, EC: ExprContext + ?Sized> ParameterScope<'a, EC> {
    pub fn new(
        parent: &'a mut dyn Scope<ExprContext = EC>,
        owner: ExpressionOwner<EC>,
        parameters: &[SignatureParameter<EC>],
    ) -> Self {
        let mut variable_lookup = HashMap::new();
        let mut binding_lookup = HashMap::new();

        for (param_index, param) in parameters.iter().enumerate() {
            let param_var = Variable::Parameter(Box::new(
                param.clone().to_parameter_var(owner.clone(), param_index),
            ));

            if let Some(name) = param.name.as_ref() {
                variable_lookup.insert(name.clone(), param_var.clone());
            }

            for (binding_index, binding) in param.bindings.iter().enumerate() {
                let Some(name) = binding.name.clone() else {
                    continue;
                };

                let vte = VariableTupleElement {
                    variable: param_var.clone(),
                    index: binding_index,
                    binding_type: binding.param_type.clone(),
                };

                binding_lookup.insert(name, vte);
            }
        }

        Self {
            parent,
            variable_lookup,
            binding_lookup,
        }
    }

    fn lookup_name(&self, name: &Identifier) -> Option<Lookup<EC>> {
        self.variable_lookup
            .get(name)
            .map(|variable| Lookup::Variable(variable.clone()))
            .or_else(|| {
                self.binding_lookup
                    .get(name)
                    .map(|binding| Lookup::VariableTupleElement(binding.clone()))
            })
    }
}

impl<'a, EC: ExprContext + ?Sized> Scope for ParameterScope<'a, EC> {
    type ExprContext = EC;

    fn lookup(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        self.lookup_name(name)
            .unwrap_or_else(|| self.parent.lookup(name))
    }

    fn lookup_assign(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        self.lookup_name(name)
            .unwrap_or_else(|| self.parent.lookup_assign(name))
    }
}

pub struct LocalVariableScope<'a, EC: ExprContext + ?Sized> {
    parent: &'a mut dyn Scope<ExprContext = EC>,
    variable_lookup: HashMap<Identifier, Variable<EC>>,
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

    fn lookup_variable(&self, name: &Identifier) -> Option<Lookup<EC>> {
        self.variable_lookup
            .get(name)
            .map(|variable| Lookup::Variable(variable.clone()))
    }
}

impl<'a, EC: ExprContext + ?Sized> Scope for LocalVariableScope<'a, EC> {
    type ExprContext = EC;

    fn lookup(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        self.lookup_variable(name)
            .unwrap_or_else(|| self.parent.lookup(name))
    }

    fn lookup_assign(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        self.lookup_variable(name)
            .unwrap_or_else(|| self.parent.lookup_assign(name))
    }
}

impl<'a, EC: ExprContext + ?Sized> LocalScope for LocalVariableScope<'a, EC> {
    fn add_variable(&mut self, variable: Variable<EC>) {
        if let Some(name) = &variable.name() {
            self.variable_lookup
                .insert((*name).clone(), variable.clone());
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

impl<'a, Sc: ?Sized, Sh> ShiftedScope<'a, Sc, Sh> {
    pub fn new(inner: &'a mut Sc, shifter: Sh) -> Self {
        Self { inner, shifter }
    }
}

impl<'a, Sc, Sh> ShiftedScope<'a, Sc, Sh>
where
    Sc: Scope + ?Sized,
    Sh: ExprContextShifter<EC1 = Sc::ExprContext> + Copy,
{
    fn shift_lookup(&self, lookup: Lookup<Sh::EC1>) -> Lookup<Sh::EC2> {
        match lookup {
            Lookup::Empty => Lookup::Empty,
            Lookup::Overloadable(o) => Lookup::Overloadable(o),

            Lookup::Variable(v) => {
                let mut shifter = self.shifter;
                Lookup::Variable(shifter.shift_variable(v))
            }
            Lookup::VariableTupleElement(vte) => {
                let mut shifter = self.shifter;
                Lookup::VariableTupleElement(VariableTupleElement {
                    variable: shifter.shift_variable(vte.variable),
                    index: vte.index,
                    binding_type: shifter.shift(vte.binding_type),
                })
            }
        }
    }
}

impl<'a, Sc, Sh> Scope for ShiftedScope<'a, Sc, Sh>
where
    Sc: Scope + ?Sized,
    Sh: ExprContextShifter<EC1 = Sc::ExprContext> + Copy,
{
    type ExprContext = Sh::EC2;

    fn lookup(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        let lookup = self.inner.lookup(name);
        self.shift_lookup(lookup)
    }

    fn lookup_assign(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        let lookup = self.inner.lookup_assign(name);
        self.shift_lookup(lookup)
    }
}
