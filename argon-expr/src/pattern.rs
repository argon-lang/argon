use crate::{EnumType, Expr, ExprContext, LocalVariable};
use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use derivative::Derivative;
use num_bigint::BigInt;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub enum Pattern<EC: ExprContext + ?Sized> {
    Error,
    Discard {
        t: Box<Expr<EC>>,
    },
    Tuple(Vec<Pattern<EC>>),
    Binding(LocalVariable<EC>, Box<Pattern<EC>>),
    EnumVariant {
        enum_type: EnumType<EC>,
        variant: EC::EnumVariant,
        args: Vec<Pattern<EC>>,
        fields: Vec<RecordFieldPattern<EC>>,
    },
    String(String),
    Int(BigInt),
    Bool(bool),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub struct RecordFieldPattern<EC: ExprContext + ?Sized> {
    pub field: EC::RecordField,
    pub pattern: Pattern<EC>,
}
