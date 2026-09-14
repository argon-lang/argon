use alloc::{sync::Arc, vec::Vec};
use argon_compiler::access::AccessToken;
use argon_compiler::{Enum, EnumVariant, Function, Instance, Record, Trait, Tube, TubeName};
use argon_expr::{ErasureMode, ExprContext};
use argon_ir::{
    BlockLabelKind, Region, Register, RegisterOrigin, default_shift_region,
    default_shift_register_origin,
};
use argon_parser::ast::Identifier;
use argon_util::UniqueIdentifier;
use core::hash::{Hash, Hasher};
use hashbrown::{HashMap, HashSet};

use super::TypeCheckExprContext;
use super::shifter::DefaultToTypeCheckContextShifter;

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
    fn function_result_value_type(&self) -> Option<Region<Self::ExprContext>>;
    fn known_variable_values(
        &self,
    ) -> HashMap<Variable<Self::ExprContext>, Region<Self::ExprContext>>;
}

pub trait LocalScope: Scope {
    fn add_variable(&mut self, variable: Variable<Self::ExprContext>);
    fn has_variable(&self, variable: &Variable<Self::ExprContext>) -> bool;
    fn set_known_variable_value(
        &mut self,
        variable: Variable<Self::ExprContext>,
        value: Region<Self::ExprContext>,
    );
    fn add_block_label(&mut self, label: BlockLabelDeclaration<Self::ExprContext>);
}

pub trait ImplicitGivens {
    type ExprContext: ExprContext + ?Sized;

    fn register_variable(&mut self, variable: Variable<Self::ExprContext>);
    fn register_function(&mut self, function: Arc<dyn Function>);
}

pub enum Lookup<EC: ExprContext + ?Sized> {
    Empty,
    Variable(Variable<EC>),
    VariableTupleElement(VariableTupleElement<EC>),
    Overloadable(OverloadLookup),
}

pub struct Variable<EC: ExprContext + ?Sized> {
    pub register: Register,
    pub r#type: Region<EC>,
    pub name: Option<Identifier>,
    pub is_mutable: bool,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
    pub origin: RegisterOrigin<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for Variable<EC> {
    fn clone(&self) -> Self {
        Self {
            register: self.register.clone(),
            r#type: self.r#type.clone(),
            name: self.name.clone(),
            is_mutable: self.is_mutable,
            erasure_mode: self.erasure_mode,
            is_witness: self.is_witness,
            origin: self.origin.clone(),
        }
    }
}

impl<EC: ExprContext + ?Sized> PartialEq for Variable<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.register == other.register
    }
}

impl<EC: ExprContext + ?Sized> Eq for Variable<EC> {}

impl<EC: ExprContext + ?Sized> Hash for Variable<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.register.hash(state);
    }
}

pub struct VariableTupleElement<EC: ExprContext + ?Sized> {
    pub variable: Variable<EC>,
    pub index: usize,
    pub binding_type: Region<EC>,
}

pub struct BlockLabel<EC: ExprContext + ?Sized> {
    pub id: UniqueIdentifier,
    pub name: Option<Identifier>,
    pub kind: BlockLabelKind,
    pub block_result_type: Region<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for BlockLabel<EC> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            name: self.name.clone(),
            kind: self.kind,
            block_result_type: self.block_result_type.clone(),
        }
    }
}

pub enum BlockLabelDeclaration<EC: ExprContext + ?Sized> {
    Block(BlockLabel<EC>),
    Loop(LoopLabels<EC>),
}

pub enum LoopLabels<EC: ExprContext + ?Sized> {
    Loop(BlockLabel<EC>),
    While {
        outer: BlockLabel<EC>,
        inner: BlockLabel<EC>,
    },
}

impl<EC: ExprContext + ?Sized> Clone for BlockLabelDeclaration<EC> {
    fn clone(&self) -> Self {
        match self {
            Self::Block(label) => Self::Block(label.clone()),
            Self::Loop(labels) => Self::Loop(labels.clone()),
        }
    }
}

impl<EC: ExprContext + ?Sized> Clone for LoopLabels<EC> {
    fn clone(&self) -> Self {
        match self {
            Self::Loop(label) => Self::Loop(label.clone()),
            Self::While { outer, inner } => Self::While {
                outer: outer.clone(),
                inner: inner.clone(),
            },
        }
    }
}

pub struct OverloadLookup {
    pub item_groups: Vec<Vec<Overloadable>>,
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

pub(super) struct ShiftedScope<'a> {
    inner: &'a dyn Scope<ExprContext = argon_compiler::DefaultExprContext>,
}

impl<'a> ShiftedScope<'a> {
    pub(super) fn new(
        inner: &'a dyn Scope<ExprContext = argon_compiler::DefaultExprContext>,
    ) -> Self {
        Self { inner }
    }

    fn shift_lookup(
        &self,
        lookup: Lookup<argon_compiler::DefaultExprContext>,
    ) -> Lookup<TypeCheckExprContext> {
        match lookup {
            Lookup::Empty => Lookup::Empty,
            Lookup::Variable(variable) => Lookup::Variable(shift_variable(variable)),
            Lookup::VariableTupleElement(element) => {
                Lookup::VariableTupleElement(VariableTupleElement {
                    variable: shift_variable(element.variable),
                    index: element.index,
                    binding_type: shift_region(element.binding_type),
                })
            }
            Lookup::Overloadable(overload) => Lookup::Overloadable(overload),
        }
    }
}

impl Scope for ShiftedScope<'_> {
    type ExprContext = TypeCheckExprContext;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.shift_lookup(self.inner.lookup(name, access))
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.shift_lookup(self.inner.lookup_assign(name, access))
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        self.inner.lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        let mut shifted = ShiftedImplicitGivens { inner: givens };
        self.inner.given_assertions(&mut shifted);
    }

    fn lookup_block_label(
        &self,
        name: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        self.inner
            .lookup_block_label(name)
            .map(shift_label_declaration)
    }

    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        self.inner.latest_loop_labels().map(shift_loop_labels)
    }

    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        self.inner.latest_block_label().map(shift_label)
    }

    fn function_result_value_type(&self) -> Option<Region<Self::ExprContext>> {
        self.inner.function_result_value_type().map(shift_region)
    }

    fn known_variable_values(
        &self,
    ) -> HashMap<Variable<Self::ExprContext>, Region<Self::ExprContext>> {
        self.inner
            .known_variable_values()
            .into_iter()
            .map(|(variable, value)| (shift_variable(variable), shift_region(value)))
            .collect()
    }
}

struct ShiftedImplicitGivens<'a> {
    inner: &'a mut dyn ImplicitGivens<ExprContext = TypeCheckExprContext>,
}

impl ImplicitGivens for ShiftedImplicitGivens<'_> {
    type ExprContext = argon_compiler::DefaultExprContext;

    fn register_variable(&mut self, variable: Variable<Self::ExprContext>) {
        self.inner.register_variable(shift_variable(variable));
    }

    fn register_function(&mut self, function: Arc<dyn Function>) {
        self.inner.register_function(function);
    }
}

pub struct LocalVariableScope<Sc: Scope> {
    parent: Sc,
    variable_lookup: HashMap<Identifier, Variable<Sc::ExprContext>>,
    variables: HashSet<Variable<Sc::ExprContext>>,
    block_labels: HashMap<Identifier, BlockLabelDeclaration<Sc::ExprContext>>,
    latest_loop_labels: Option<LoopLabels<Sc::ExprContext>>,
    latest_block_label: Option<BlockLabel<Sc::ExprContext>>,
    known_variable_values: HashMap<Variable<Sc::ExprContext>, Region<Sc::ExprContext>>,
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
            known_variable_values: HashMap::new(),
        }
    }
}

impl<Sc: Scope> Scope for LocalVariableScope<Sc> {
    type ExprContext = Sc::ExprContext;

    fn lookup(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.variable_lookup
            .get(name)
            .cloned()
            .map(Lookup::Variable)
            .unwrap_or_else(|| self.parent.lookup(name, access))
    }

    fn lookup_assign(&self, name: &Identifier, access: &AccessToken) -> Lookup<Self::ExprContext> {
        self.variable_lookup
            .get(name)
            .cloned()
            .map(Lookup::Variable)
            .unwrap_or_else(|| self.parent.lookup_assign(name, access))
    }

    fn lookup_tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        self.parent.lookup_tube(name)
    }

    fn given_assertions(&self, givens: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {
        for variable in &self.variables {
            if variable.is_witness {
                givens.register_variable(variable.clone());
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

    fn function_result_value_type(&self) -> Option<Region<Self::ExprContext>> {
        self.parent.function_result_value_type()
    }

    fn known_variable_values(
        &self,
    ) -> HashMap<Variable<Self::ExprContext>, Region<Self::ExprContext>> {
        let mut values = self.parent.known_variable_values();
        values.extend(self.known_variable_values.clone());
        values
    }
}

impl<Sc: Scope> LocalScope for LocalVariableScope<Sc> {
    fn add_variable(&mut self, variable: Variable<Self::ExprContext>) {
        if let Some(name) = &variable.name {
            self.variable_lookup.insert(name.clone(), variable.clone());
        }
        self.variables.insert(variable);
    }

    fn has_variable(&self, variable: &Variable<Self::ExprContext>) -> bool {
        self.variables.contains(variable)
    }

    fn set_known_variable_value(
        &mut self,
        variable: Variable<Self::ExprContext>,
        value: Region<Self::ExprContext>,
    ) {
        self.known_variable_values.insert(variable, value);
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
                let name = match &labels {
                    LoopLabels::Loop(label) => label.name.as_ref(),
                    LoopLabels::While { outer, .. } => outer.name.as_ref(),
                };
                if let Some(name) = name {
                    self.block_labels
                        .insert(name.clone(), BlockLabelDeclaration::Loop(labels.clone()));
                }
                self.latest_loop_labels = Some(labels);
            }
        }
    }
}

fn shift_region(
    region: Region<argon_compiler::DefaultExprContext>,
) -> Region<TypeCheckExprContext> {
    default_shift_region(&mut DefaultToTypeCheckContextShifter, region)
}

fn shift_variable(
    variable: Variable<argon_compiler::DefaultExprContext>,
) -> Variable<TypeCheckExprContext> {
    let mut shifter = DefaultToTypeCheckContextShifter;
    Variable {
        register: variable.register,
        r#type: default_shift_region(&mut shifter, variable.r#type),
        name: variable.name,
        is_mutable: variable.is_mutable,
        erasure_mode: variable.erasure_mode,
        is_witness: variable.is_witness,
        origin: default_shift_register_origin(&mut shifter, variable.origin),
    }
}

fn shift_label(
    label: BlockLabel<argon_compiler::DefaultExprContext>,
) -> BlockLabel<TypeCheckExprContext> {
    BlockLabel {
        id: label.id,
        name: label.name,
        kind: label.kind,
        block_result_type: shift_region(label.block_result_type),
    }
}

fn shift_loop_labels(
    labels: LoopLabels<argon_compiler::DefaultExprContext>,
) -> LoopLabels<TypeCheckExprContext> {
    match labels {
        LoopLabels::Loop(label) => LoopLabels::Loop(shift_label(label)),
        LoopLabels::While { outer, inner } => LoopLabels::While {
            outer: shift_label(outer),
            inner: shift_label(inner),
        },
    }
}

fn shift_label_declaration(
    declaration: BlockLabelDeclaration<argon_compiler::DefaultExprContext>,
) -> BlockLabelDeclaration<TypeCheckExprContext> {
    match declaration {
        BlockLabelDeclaration::Block(label) => BlockLabelDeclaration::Block(shift_label(label)),
        BlockLabelDeclaration::Loop(labels) => {
            BlockLabelDeclaration::Loop(shift_loop_labels(labels))
        }
    }
}
