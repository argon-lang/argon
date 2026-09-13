use alloc::{boxed::Box, string::String, vec::Vec};
use argon_expr::ExprContext;
use derivative::Derivative;
use num_bigint::BigInt;
use parse18_runtime::Location;

use crate::{EnumType, Register, Value};

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct LocatedPattern<EC: ExprContext + ?Sized> {
    pub pattern: Pattern<EC>,
    pub location: Location,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum Pattern<EC: ExprContext + ?Sized> {
    Error,
    Discard {
        r#type: Box<Value<EC>>,
    },
    Tuple(Vec<LocatedPattern<EC>>),
    Binding(Register, Box<LocatedPattern<EC>>),
    EnumVariant {
        enum_type: EnumType<EC>,
        variant: EC::EnumVariant,
        arguments: Vec<LocatedPattern<EC>>,
        fields: Vec<FieldPattern<EC>>,
    },
    String(String),
    Int(BigInt),
    Bool(bool),
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct FieldPattern<EC: ExprContext + ?Sized> {
    pub field: EC::RecordField,
    pub pattern: LocatedPattern<EC>,
}
