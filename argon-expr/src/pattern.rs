use crate::{EnumType, ExprContext, LocalVariable, LocatedExpr};
use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use core::hash::Hash;
use derivative::Derivative;
use num_bigint::BigInt;
use parse18_runtime::Location;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub struct LocatedPattern<EC: ExprContext + ?Sized> {
    pub value: Pattern<EC>,
    pub location: Location,
}

impl<EC: ExprContext + ?Sized> PartialEq for LocatedPattern<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<EC: ExprContext + ?Sized> Eq for LocatedPattern<EC> {}

impl<EC: ExprContext + ?Sized> Hash for LocatedPattern<EC> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub enum Pattern<EC: ExprContext + ?Sized> {
    Error,
    Discard {
        t: Box<LocatedExpr<EC>>,
    },
    Tuple(Vec<LocatedPattern<EC>>),
    Binding(LocalVariable<EC>, Box<LocatedPattern<EC>>),
    EnumVariant {
        enum_type: EnumType<EC>,
        variant: EC::EnumVariant,
        args: Vec<LocatedPattern<EC>>,
        fields: Vec<RecordFieldPattern<EC>>,
    },
    String(String),
    Int(BigInt),
    Bool(bool),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct RecordFieldPattern<EC: ExprContext + ?Sized> {
    pub field: EC::RecordField,
    pub pattern: LocatedPattern<EC>,
}
