use crate::access::AccessToken;
use crate::implicits::ImplicitValue;
use crate::signature::SignatureParameter;
use crate::{
    DefaultExprContext, Enum, EnumVariant, Function, FunctionSignature, Instance, Record, Trait,
    Tube, TubeName,
};
use alloc::boxed::Box;
use alloc::{sync::Arc, vec::Vec};
use argon_expr::{
    BlockLabel, BlockLabelDeclaration, Expr, ExprContext, ExprContextShifter, ExpressionOwner,
    InstanceParameterVariable, LoopLabels, Variable, VariableTupleElement,
};
use argon_parser::ast::Identifier;
use hashbrown::{HashMap, HashSet};

pub trait Scope {
    type ExprContext: ExprContext + ?Sized;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext>;
    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext>;

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>>;

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>);

    fn lookup_block_label(
        &self,
        name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>>;

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>>;
    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>>;
    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>>;
}

pub trait LocalScope: Scope {
    fn add_variable(&mut self, variable: Variable<Self::ExprContext>);

    fn has_variable(&self, variable: &Variable<Self::ExprContext>) -> bool;

    fn add_block_label(&mut self, label: BlockLabelDeclaration<Self::ExprContext>);
}

pub trait ImplicitGivens {
    type ExprContext: ExprContext + ?Sized;

    fn register_variable(&mut self, variable: Variable<Self::ExprContext>);
    fn register_function(&mut self, function: Arc<dyn Function>);
}

impl<EC: ExprContext + ?Sized> ImplicitGivens for Vec<ImplicitValue<EC>> {
    type ExprContext = EC;

    fn register_variable(&mut self, variable: Variable<Self::ExprContext>) {
        self.push(ImplicitValue::OfVar(variable));
    }

    fn register_function(&mut self, function: Arc<dyn Function>) {
        self.push(ImplicitValue::OfFunction(function));
    }
}

pub enum Lookup<EC: ExprContext + ?Sized> {
    Empty,
    Variable(Variable<EC>),
    VariableTupleElement(VariableTupleElement<EC>),
    Overloadable(OverloadLookup),
}

impl<EC: ExprContext + ?Sized> Scope for &dyn Scope<ExprContext = EC> {
    type ExprContext = EC;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        (**self).lookup(name, access)
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        (**self).lookup_assign(name, access)
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        (**self).lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        (**self).given_assertions(givens)
    }

    fn lookup_block_label(
        &self,
        name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        (**self).lookup_block_label(name)
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        (**self).latest_loop_labels()
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        (**self).latest_block_label()
    }

    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>> {
        (**self).function_result_value_type()
    }
}

impl<EC: ExprContext + ?Sized> Scope for &mut dyn Scope<ExprContext = EC> {
    type ExprContext = EC;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        (**self).lookup(name, access)
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        (**self).lookup_assign(name, access)
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        (**self).lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        (**self).given_assertions(givens)
    }

    fn lookup_block_label(
        &self,
        name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        (**self).lookup_block_label(name)
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        (**self).latest_loop_labels()
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        (**self).latest_block_label()
    }

    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>> {
        (**self).function_result_value_type()
    }
}

pub struct FunctionResultValueScope<Sc: Scope> {
    parent: Sc,
    result_value_type: Expr<Sc::ExprContext>,
}

impl<Sc: Scope> FunctionResultValueScope<Sc> {
    pub fn new(parent: Sc, result_value_type: Expr<Sc::ExprContext>) -> Self {
        Self {
            parent,
            result_value_type,
        }
    }
}

impl<Sc: Scope> Scope for FunctionResultValueScope<Sc> {
    type ExprContext = Sc::ExprContext;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.parent.lookup(name, access)
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.parent.lookup_assign(name, access)
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        self.parent.lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        self.parent.given_assertions(givens);
    }

    fn lookup_block_label(
        &self,
        name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        self.parent.lookup_block_label(name)
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        self.parent.latest_loop_labels()
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        self.parent.latest_block_label()
    }

    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>> {
        Some(self.result_value_type.clone())
    }
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

#[derive(Debug)]
pub enum Overloadable {
    Function(Arc<dyn Function>),
    Record(Arc<dyn Record>),
    Enum(Arc<dyn Enum>),
    EnumVariant(Arc<dyn EnumVariant>),
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
                Instance = <DefaultExprContext as ExprContext>::Instance,
            > + ?Sized,
    {
        match self {
            Overloadable::Function(f) => ExpressionOwner::Function(f.clone()),
            Overloadable::Record(r) => ExpressionOwner::Record(r.clone()),
            Overloadable::Enum(e) => ExpressionOwner::Enum(e.clone()),
            Overloadable::EnumVariant(v) => ExpressionOwner::EnumVariant(v.clone()),
            Overloadable::Trait(t) => ExpressionOwner::Trait(t.clone()),
            Overloadable::Instance(i) => ExpressionOwner::Instance(i.clone()),
        }
    }

    pub fn signature(&self) -> Arc<FunctionSignature<DefaultExprContext>> {
        match self {
            Overloadable::Function(f) => f.clone().signature(),
            Overloadable::Record(r) => r.clone().signature(),
            Overloadable::Enum(e) => e.clone().signature(),
            Overloadable::EnumVariant(v) => v.clone().signature(),
            Overloadable::Trait(t) => t.clone().signature(),
            Overloadable::Instance(i) => i.clone().signature(),
        }
    }
}

pub struct ParameterScope<Sc: Scope> {
    parent: Sc,
    variable_lookup: HashMap<Identifier, Variable<Sc::ExprContext>>,
    binding_lookup: HashMap<Identifier, VariableTupleElement<Sc::ExprContext>>,
}

impl<Sc: Scope> ParameterScope<Sc> {
    pub fn new(
        parent: Sc,
        owner: ExpressionOwner<Sc::ExprContext>,
        parameters: &[SignatureParameter<Sc::ExprContext>],
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

    fn lookup_name(&self, name: &Identifier) -> Option<Lookup<Sc::ExprContext>> {
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

impl<Sc: Scope> Scope for ParameterScope<Sc> {
    type ExprContext = Sc::ExprContext;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.lookup_name(name)
            .unwrap_or_else(|| self.parent.lookup(name, access))
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.lookup_name(name)
            .unwrap_or_else(|| self.parent.lookup_assign(name, access))
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        self.parent.lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        for v in self.variable_lookup.values() {
            if v.is_witness() {
                givens.register_variable(v.clone());
            }
        }

        self.parent.given_assertions(givens);
    }

    fn lookup_block_label(
        &self,
        _name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        None
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        None
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        None
    }

    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>> {
        self.parent.function_result_value_type()
    }
}

pub struct InstanceParameterScope<Sc: Scope> {
    parent: Sc,
    variable: Variable<Sc::ExprContext>,
}

impl<Sc: Scope> InstanceParameterScope<Sc> {
    pub fn new(parent: Sc, variable: InstanceParameterVariable<Sc::ExprContext>) -> Self {
        Self {
            parent,
            variable: Variable::InstanceParameter(Box::new(variable)),
        }
    }

    fn lookup_name(&self, name: &Identifier) -> Option<Lookup<Sc::ExprContext>> {
        match self.variable.name() {
            Some(variable_name) if variable_name == name => {
                Some(Lookup::Variable(self.variable.clone()))
            }
            _ => None,
        }
    }
}

impl<Sc: Scope> Scope for InstanceParameterScope<Sc> {
    type ExprContext = Sc::ExprContext;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.lookup_name(name)
            .unwrap_or_else(|| self.parent.lookup(name, access))
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.lookup_name(name)
            .unwrap_or_else(|| self.parent.lookup_assign(name, access))
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        self.parent.lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        self.parent.given_assertions(givens);
    }

    fn lookup_block_label(
        &self,
        _name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        None
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        None
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        None
    }

    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>> {
        self.parent.function_result_value_type()
    }
}

pub struct LocalVariableScope<Sc: Scope> {
    parent: Sc,
    variable_lookup: HashMap<Identifier, Variable<Sc::ExprContext>>,
    variables: HashSet<Variable<Sc::ExprContext>>,
    block_labels: HashMap<Identifier, BlockLabelDeclaration<Sc::ExprContext>>,
    latest_loop_labels: Option<LoopLabels<Sc::ExprContext>>,
    latest_block_label: Option<BlockLabel<Sc::ExprContext>>,
}

impl<Sc: Scope> LocalVariableScope<Sc> {
    pub fn new(parent: Sc) -> Self {
        Self {
            parent,
            variable_lookup: HashMap::new(),
            variables: HashSet::new(),
            block_labels: HashMap::new(),
            latest_loop_labels: None,
            latest_block_label: None,
        }
    }

    fn lookup_variable(&self, name: &Identifier) -> Option<Lookup<Sc::ExprContext>> {
        self.variable_lookup
            .get(name)
            .map(|variable| Lookup::Variable(variable.clone()))
    }
}

impl<Sc: Scope> Scope for LocalVariableScope<Sc> {
    type ExprContext = Sc::ExprContext;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.lookup_variable(name)
            .unwrap_or_else(|| self.parent.lookup(name, access))
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.lookup_variable(name)
            .unwrap_or_else(|| self.parent.lookup_assign(name, access))
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        self.parent.lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        for v in &self.variables {
            if v.is_witness() {
                givens.register_variable(v.clone());
            }
        }

        self.parent.given_assertions(givens);
    }

    fn lookup_block_label(
        &self,
        name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        self.block_labels
            .get(name)
            .cloned()
            .or_else(|| self.parent.lookup_block_label(name))
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        self.latest_loop_labels
            .clone()
            .or_else(|| self.parent.latest_loop_labels())
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        self.latest_block_label
            .clone()
            .or_else(|| self.parent.latest_block_label())
    }

    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>> {
        self.parent.function_result_value_type()
    }
}

impl<Sc: Scope> LocalScope for LocalVariableScope<Sc> {
    fn add_variable(&mut self, variable: Variable<Self::ExprContext>) {
        if let Some(name) = &variable.name() {
            self.variable_lookup
                .insert((*name).clone(), variable.clone());
        }

        self.variables.insert(variable);
    }

    fn has_variable(&self, variable: &Variable<Self::ExprContext>) -> bool {
        self.variables.contains(variable)
    }

    fn add_block_label(&mut self, label: BlockLabelDeclaration<Self::ExprContext>) {
        match label {
            BlockLabelDeclaration::Block(label) => {
                if let Some(name) = &label.name {
                    self.block_labels
                        .insert(name.clone(), BlockLabelDeclaration::Block(label.clone()));
                }

                self.latest_block_label = Some(label);
            }
            BlockLabelDeclaration::Loop(labels) => {
                if let Some(name) = labels.name() {
                    self.block_labels
                        .insert(name.clone(), BlockLabelDeclaration::Loop(labels.clone()));
                }

                self.latest_loop_labels = Some(labels);
            }
        }
    }
}

pub struct ShiftedScope<'a, Sc: ?Sized, Sh> {
    inner: &'a Sc,
    shifter: Sh,
}

impl<'a, Sc: ?Sized, Sh> ShiftedScope<'a, Sc, Sh> {
    pub fn new(inner: &'a Sc, shifter: Sh) -> Self {
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

    fn shift_block_label_declaration(
        &self,
        label: BlockLabelDeclaration<Sh::EC1>,
    ) -> BlockLabelDeclaration<Sh::EC2> {
        match label {
            BlockLabelDeclaration::Block(label) => {
                BlockLabelDeclaration::Block(self.shift_block_label(label))
            }
            BlockLabelDeclaration::Loop(labels) => {
                BlockLabelDeclaration::Loop(self.shift_loop_labels(labels))
            }
        }
    }

    fn shift_block_label(&self, label: BlockLabel<Sh::EC1>) -> BlockLabel<Sh::EC2> {
        let mut shifter = self.shifter;
        BlockLabel {
            id: label.id,
            name: label.name,
            kind: label.kind,
            block_result_type: shifter.shift(label.block_result_type),
        }
    }

    fn shift_loop_labels(&self, labels: LoopLabels<Sh::EC1>) -> LoopLabels<Sh::EC2> {
        match labels {
            LoopLabels::Loop(label) => LoopLabels::Loop(self.shift_block_label(label)),
            LoopLabels::While { outer, inner } => LoopLabels::While {
                outer: self.shift_block_label(outer),
                inner: self.shift_block_label(inner),
            },
        }
    }
}

impl<'a, Sc, Sh> Scope for ShiftedScope<'a, Sc, Sh>
where
    Sc: Scope + ?Sized,
    Sh: ExprContextShifter<EC1 = Sc::ExprContext> + Copy,
{
    type ExprContext = Sh::EC2;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        let lookup = self.inner.lookup(name, access);
        self.shift_lookup(lookup)
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        let lookup = self.inner.lookup_assign(name, access);
        self.shift_lookup(lookup)
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        self.inner.lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        let mut shifted_givens = ShiftedImplicitGivens {
            inner: givens,
            shifter: self.shifter,
        };

        self.inner.given_assertions(&mut shifted_givens);
    }

    fn lookup_block_label(
        &self,
        name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        self.inner
            .lookup_block_label(name)
            .map(|label| self.shift_block_label_declaration(label))
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        self.inner
            .latest_loop_labels()
            .map(|labels| self.shift_loop_labels(labels))
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        self.inner
            .latest_block_label()
            .map(|label| self.shift_block_label(label))
    }

    fn function_result_value_type(&self) -> Option<Expr<Self::ExprContext>> {
        self.inner.function_result_value_type().map(|result_type| {
            let mut shifter = self.shifter;
            shifter.shift(result_type)
        })
    }
}

struct ShiftedImplicitGivens<'a, G: ?Sized, Sh> {
    inner: &'a mut G,
    shifter: Sh,
}

impl<G, Sh> ImplicitGivens for ShiftedImplicitGivens<'_, G, Sh>
where
    G: ImplicitGivens<ExprContext = Sh::EC2> + ?Sized,
    Sh: ExprContextShifter + Copy,
{
    type ExprContext = Sh::EC1;

    fn register_variable(&mut self, variable: Variable<Self::ExprContext>) {
        let mut shifter = self.shifter;
        self.inner
            .register_variable(shifter.shift_variable(variable));
    }

    fn register_function(&mut self, function: Arc<dyn Function>) {
        self.inner.register_function(function);
    }
}
