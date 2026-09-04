use crate::access::{AccessModifier, AccessToken};
use crate::{
    Context, Declaration, DefaultExprComparer, DefaultExprContext, FunctionSignature, Method,
    MethodEntry, MethodOwner, MethodSlot, SubstFunctionSignature,
};
use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;
use argon_expr::{
    EnumType, Expr, ExprLocationExt, ExpressionOwner, InstanceType, LocatedExpr, RecordType,
    SubstScanner, TraitType, Unify,
};
use argon_expr::{ExprScannerMut, Variable};
use argon_parser::Location;
use argon_util::{CompileError, Unload};
use hashbrown::{HashMap, HashSet};

#[derive(Clone)]
pub struct VTable {
    entries: HashMap<VTableSlot, VTableSlotValue>,
}

impl Unload for VTable {
    fn unload(&self) {}
}

impl VTable {
    pub fn empty() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn scan_mut<S>(&mut self, scan: &mut S)
    where
        S: ExprScannerMut<EC = DefaultExprContext>,
    {
        for slot_value in self.entries.values_mut() {
            scan.scan(&mut slot_value.slot_instance_type);
            Arc::make_mut(&mut slot_value.signature).scan_mut(scan);
        }
    }
}

impl VTable {
    pub fn entries(&self) -> &HashMap<VTableSlot, VTableSlotValue> {
        &self.entries
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct VTableSlot {
    method: Arc<dyn Method>,
}

impl VTableSlot {
    pub fn method(&self) -> &Arc<dyn Method> {
        &self.method
    }
}

impl PartialEq for VTableSlot {
    fn eq(&self, other: &Self) -> bool {
        &self.method == &other.method
    }
}

#[derive(Clone)]
pub struct VTableSlotValue {
    signature: Arc<FunctionSignature<DefaultExprContext>>,
    slot_instance_type: LocatedExpr<DefaultExprContext>,
    slot_access: AccessModifier,
    target: VTableTarget,
}

impl VTableSlotValue {
    pub fn signature(&self) -> &Arc<FunctionSignature<DefaultExprContext>> {
        &self.signature
    }

    pub fn slot_access(&self) -> AccessModifier {
        self.slot_access
    }

    pub fn target(&self) -> &VTableTarget {
        &self.target
    }
}

#[derive(Clone, Debug)]
pub enum VTableTarget {
    Abstract,
    Implementation(Arc<dyn Method>),
    Ambiguous(HashSet<Arc<dyn Method>>),
}

pub fn build_vtable(
    context: Context,
    method_owner: MethodOwner,
    access_token: AccessToken,
    location: Location,
) -> VTable {
    let parent;
    let is_concrete;

    match &method_owner {
        MethodOwner::Record(_) => {
            is_concrete = true;
            parent = VTable {
                entries: HashMap::new(),
            };
        }
        MethodOwner::Enum(_) => {
            is_concrete = false;
            parent = VTable {
                entries: HashMap::new(),
            };
        }
        MethodOwner::EnumVariant(variant) => {
            is_concrete = true;
            let enum_ = variant.owning_enum();
            let mut enum_vtable = enum_.clone().vtable().as_ref().clone();
            if let Expr::EnumType(enum_type) = &variant.clone().signature().return_type.value {
                let mut subst = SubstScanner::new();
                subst.add_function_parameter_substitutions(
                    ExpressionOwner::Enum(enum_.clone()),
                    &enum_.signature(),
                    &enum_type.arguments,
                );
                enum_vtable.scan_mut(&mut subst);
            }
            parent = enum_vtable;
        }
        MethodOwner::Trait(_) => {
            is_concrete = false;
            parent = VTable {
                entries: HashMap::new(),
            };
        }
        MethodOwner::Instance(i) => {
            is_concrete = true;
            parent = match &i.clone().signature().return_type.value {
                Expr::TraitType(tt) => {
                    let mut parent = tt.trait_.clone().vtable().as_ref().clone();
                    let mut subst = SubstScanner::new();
                    subst.add_function_parameter_substitutions(
                        ExpressionOwner::Trait(tt.trait_.clone()),
                        &tt.trait_.clone().signature(),
                        &tt.arguments,
                    );
                    parent.scan_mut(&mut subst);
                    parent
                }
                _ => VTable {
                    entries: HashMap::new(),
                },
            }
        }
    };

    let mut builder = VTableBuilder {
        context,
        parent,
        current: VTable {
            entries: HashMap::new(),
        },
        access_token,
        concrete_location: location,
    };

    builder.build_single_type_vtable(method_owner);

    if is_concrete {
        builder.check_concrete();
    }

    builder.current
}

struct VTableBuilder {
    context: Context,
    parent: VTable,
    current: VTable,
    access_token: AccessToken,
    concrete_location: Location,
}

impl VTableBuilder {
    fn check_concrete(&self) {
        self.current
            .entries
            .iter()
            .chain(
                self.parent
                    .entries
                    .iter()
                    .filter(|(slot, _)| !self.current.entries.contains_key(*slot)),
            )
            .for_each(|(_, slot_value)| match slot_value.target {
                VTableTarget::Abstract | VTableTarget::Ambiguous(_) => self
                    .context
                    .reporter()
                    .report_error(CompileError::abstract_method_error(Some(
                        self.concrete_location.clone(),
                    ))),

                VTableTarget::Implementation(_) => {}
            })
    }

    fn build_single_type_vtable(&mut self, method_owner: MethodOwner) {
        let methods = match method_owner {
            MethodOwner::Record(record) => record.methods(),
            MethodOwner::Enum(enum_) => enum_.methods(),
            MethodOwner::EnumVariant(variant) => variant.methods(),
            MethodOwner::Instance(instance) => instance.methods(),
            MethodOwner::Trait(trait_) => trait_.methods(),
        };

        for method in methods.iter() {
            self.build_override(method.clone());
        }
    }

    fn build_override(&mut self, method_entry: MethodEntry) {
        let MethodEntry {
            method,
            access: method_access,
        } = method_entry;

        let sig = method.clone().signature();

        let slot = method.metadata().slot;

        let target = match slot {
            MethodSlot::Abstract | MethodSlot::AbstractOverride => VTableTarget::Abstract,

            MethodSlot::Virtual
            | MethodSlot::Override
            | MethodSlot::Final
            | MethodSlot::FinalOverride => VTableTarget::Implementation(method.clone()),
        };

        match slot {
            MethodSlot::Virtual | MethodSlot::Final | MethodSlot::Abstract => {}

            MethodSlot::Override | MethodSlot::FinalOverride | MethodSlot::AbstractOverride => {
                self.override_matching_slots(method.clone(), method_access, sig.clone(), &target);
            }
        }

        let slot_instance_type = self.build_slot_instance_type(method.clone());

        self.current.entries.insert(
            VTableSlot { method },
            VTableSlotValue {
                signature: sig,
                slot_instance_type,
                slot_access: method_access,
                target,
            },
        );
    }

    fn build_slot_instance_type(&self, method: Arc<dyn Method>) -> LocatedExpr<DefaultExprContext> {
        let method_owner = method.clone().owner();
        let sig = method_owner.signature().as_ref().clone();
        let owner = method_owner.clone().into_expression_owner();
        let location = self.concrete_location.clone();

        let arguments = sig
            .parameters
            .into_iter()
            .enumerate()
            .map(|(i, param)| {
                let param_var = param.to_parameter_var(owner.clone(), i);
                Expr::Variable(Variable::Parameter(Box::new(param_var)))
                    .with_location(location.clone())
            })
            .collect::<Vec<_>>();

        match method_owner {
            MethodOwner::Record(r) => Expr::RecordType(RecordType {
                record: r,
                arguments,
            }),
            MethodOwner::Enum(e) => Expr::EnumType(EnumType {
                enum_: e,
                arguments,
            }),
            MethodOwner::EnumVariant(v) => v.signature().return_type.value.clone(),
            MethodOwner::Trait(t) => Expr::TraitType(TraitType {
                trait_: t,
                arguments,
            }),
            MethodOwner::Instance(i) => Expr::InstanceType(InstanceType {
                instance: i,
                arguments,
            }),
        }
        .with_location(location)
    }

    fn override_matching_slots(
        &mut self,
        method: Arc<dyn Method>,
        method_access: AccessModifier,
        sig: Arc<FunctionSignature<DefaultExprContext>>,
        target: &VTableTarget,
    ) {
        let mut found_override = false;

        for (slot, slot_value) in &self.parent.entries {
            match &slot_value.target {
                VTableTarget::Abstract | VTableTarget::Ambiguous(_) => {}
                VTableTarget::Implementation(m) => match m.metadata().slot {
                    MethodSlot::Abstract
                    | MethodSlot::AbstractOverride
                    | MethodSlot::Virtual
                    | MethodSlot::Override => {}

                    MethodSlot::Final | MethodSlot::FinalOverride => {
                        continue;
                    }
                },
            }

            if slot.method.metadata().name != method.metadata().name {
                continue;
            }

            let slot_declaration = Declaration::Method(slot.method.clone());
            let override_accessible = match slot_value.slot_access {
                AccessModifier::Protected | AccessModifier::ProtectedOrInternal => true,
                AccessModifier::ProtectedAndInternal => self.access_token.allows_access(
                    &slot_declaration,
                    Some(&method.clone().owner()),
                    AccessModifier::Internal,
                ),
                access => self.access_token.allows_access(
                    &slot_declaration,
                    Some(&method.clone().owner()),
                    access,
                ),
            };
            if !override_accessible {
                continue;
            }

            if !self.signature_matches(
                ExpressionOwner::Method(slot.method.clone()),
                slot_value.signature.as_ref().clone(),
                ExpressionOwner::Method(method.clone()),
                sig.clone(),
            ) {
                continue;
            }

            if slot_value.slot_access.is_wider_than(method_access) {
                self.context
                    .reporter()
                    .report_error(CompileError::override_access_narrowing(
                        Some(self.concrete_location.clone()),
                        slot_value.slot_access.display(),
                        method_access.display(),
                    ));
            }

            let mut new_value = slot_value.clone();
            new_value.target = target.clone();

            found_override = true;

            if self
                .current
                .entries
                .insert(slot.clone(), new_value)
                .is_some()
            {
                todo!("multiple overrides for same slot")
            }
        }

        if !found_override {
            self.context
                .reporter()
                .report_error(CompileError::invalid_override(Some(
                    self.concrete_location.clone(),
                )))
        }
    }

    fn signature_matches(
        &self,
        parent_owner: ExpressionOwner<DefaultExprContext>,
        parent: FunctionSignature<DefaultExprContext>,
        child_owner: ExpressionOwner<DefaultExprContext>,
        child: Arc<FunctionSignature<DefaultExprContext>>,
    ) -> bool {
        if parent.parameters.len() != child.parameters.len() {
            return false;
        }

        let mut subst = SubstScanner::new();

        for (index, (parent_param, child_param)) in parent
            .parameters
            .iter()
            .zip(child.parameters.iter())
            .enumerate()
        {
            let mut parent_param_type = parent_param.param_type.clone();

            subst.scan(&mut parent_param_type);

            if !self.type_matches(parent_param_type, child_param.param_type.clone()) {
                return false;
            }

            subst.add_substitution(
                Variable::Parameter(Box::new(
                    parent_param
                        .clone()
                        .to_parameter_var(parent_owner.clone(), index),
                )),
                Cow::Owned(
                    Expr::Variable(Variable::Parameter(Box::new(
                        child_param
                            .clone()
                            .to_parameter_var(child_owner.clone(), index),
                    )))
                    .with_location(self.concrete_location.clone()),
                ),
            );
        }

        true
    }

    fn type_matches(
        &self,
        a: LocatedExpr<DefaultExprContext>,
        b: LocatedExpr<DefaultExprContext>,
    ) -> bool {
        let mut unify = DefaultExprComparer::new(self.context.clone());

        unify.unify(a, b)
    }
}
