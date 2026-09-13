use alloc::{boxed::Box, vec::Vec};
use argon_expr::{ExprContext, IntegerType};
use derivative::Derivative;
use num_bigint::BigInt;

use crate::{ProgramPoint, Register};

/// A type argument is an unlocated, computation-free IR value.
pub type TypeArgument<EC> = Value<EC>;

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum Value<EC: ExprContext + ?Sized> {
    Error,
    Hole(EC::Hole),
    BoolLiteral(bool),
    IntLiteral(BigInt),
    I8Literal(i8),
    U8Literal(u8),
    I16Literal(i16),
    U16Literal(u16),
    I32Literal(i32),
    U32Literal(u32),
    I64Literal(i64),
    U64Literal(u64),
    StringLiteral(Box<str>),
    Tuple(Vec<Value<EC>>),
    Type(Box<Value<EC>>),
    BigType(BigInt),
    BuiltinType(BuiltinType<EC>),
    /// The register's value at the program point where this value is evaluated.
    Register(Register),
    FunctionResult {
        result_type: Box<Value<EC>>,
    },
    FunctionType {
        parameter: Register,
        result: Box<Value<EC>>,
    },
    ConjunctionType {
        lhs: Box<Value<EC>>,
        rhs: Box<Value<EC>>,
    },
    DisjunctionType {
        lhs: Box<Value<EC>>,
        rhs: Box<Value<EC>>,
    },
    EqualToType {
        r#type: Box<Value<EC>>,
        lhs: Box<Value<EC>>,
        rhs: Box<Value<EC>>,
    },
    RecordType(RecordType<EC>),
    EnumType(EnumType<EC>),
    TraitType(TraitType<EC>),
    InstanceType(InstanceType<EC>),
    RecordLiteral {
        record_type: RecordType<EC>,
        fields: Vec<FieldValue<EC>>,
    },
    EnumVariantLiteral {
        enum_type: EnumType<EC>,
        variant: EC::EnumVariant,
        arguments: Vec<Value<EC>>,
        fields: Vec<FieldValue<EC>>,
    },
    SharedType(Box<Value<EC>>),
    UseType {
        is_mutable: bool,
        inner: Box<Value<EC>>,
    },
    BoxedType(Box<Value<EC>>),
    /// The value of a mutable register at the specified program state.
    RegisterSnapshot {
        register: Register,
        at: ProgramPoint,
    },
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum BuiltinType<EC: ExprContext + ?Sized> {
    Int(IntegerType),
    Bool,
    String,
    Never,
    Array { element_type: Box<Value<EC>> },
}

macro_rules! nominal_type {
    ($name:ident, $member:ident, $assoc:ident) => {
        #[derive(Derivative)]
        #[derivative(
            Debug(bound = ""),
            Clone(bound = ""),
            PartialEq(bound = ""),
            Eq(bound = ""),
            Hash(bound = "")
        )]
        pub struct $name<EC: ExprContext + ?Sized> {
            pub $member: EC::$assoc,
            pub arguments: Vec<TypeArgument<EC>>,
        }
    };
}
nominal_type!(RecordType, record, Record);
nominal_type!(EnumType, enum_, Enum);
nominal_type!(TraitType, trait_, Trait);
nominal_type!(InstanceType, instance, Instance);

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum MethodInstanceType<EC: ExprContext + ?Sized> {
    Record(RecordType<EC>),
    Enum(EnumType<EC>),
    Trait(TraitType<EC>),
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct FieldValue<EC: ExprContext + ?Sized> {
    pub field: EC::RecordField,
    pub value: Value<EC>,
}
