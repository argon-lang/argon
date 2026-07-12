use crate::{BlockLabel, Pattern};
use alloc::{boxed::Box, vec, vec::Vec};
use argon_parser::ast::Identifier;
use argon_util::{UniqueIdentifier, Unload};
use core::fmt::Debug;
use core::hash::{Hash, Hasher};
use derivative::Derivative;
use mitsein::vec1::Vec1;
use num_bigint::BigInt;

pub trait ExprContext {
    type Hole: Clone + Debug + Eq + Hash;
    type Function: Clone + Debug + Eq + Hash;
    type Record: Clone + Debug + Eq + Hash;
    type Enum: Clone + Debug + Eq + Hash;
    type Trait: Clone + Debug + Eq + Hash;
    type RecordField: Clone + Debug + Eq + Hash;
    type EnumVariant: Clone + Debug + Eq + Hash;
    type Method: Clone + Debug + Eq + Hash;
    type Instance: Clone + Debug + Eq + Hash;
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum ExpressionOwner<EC: ExprContext + ?Sized> {
    Function(EC::Function),
    Record(EC::Record),
    Enum(EC::Enum),
    Trait(EC::Trait),
    EnumVariant(EC::EnumVariant),
    Method(EC::Method),
    Instance(EC::Instance),
    Field(EC::RecordField),
}

impl<EC: ExprContext + ?Sized> Clone for ExpressionOwner<EC> {
    fn clone(&self) -> Self {
        match self {
            ExpressionOwner::Function(f) => ExpressionOwner::Function(f.clone()),
            ExpressionOwner::Record(r) => ExpressionOwner::Record(r.clone()),
            ExpressionOwner::Enum(e) => ExpressionOwner::Enum(e.clone()),
            ExpressionOwner::Trait(t) => ExpressionOwner::Trait(t.clone()),
            ExpressionOwner::EnumVariant(v) => ExpressionOwner::EnumVariant(v.clone()),
            ExpressionOwner::Method(m) => ExpressionOwner::Method(m.clone()),
            ExpressionOwner::Instance(i) => ExpressionOwner::Instance(i.clone()),
            ExpressionOwner::Field(f) => ExpressionOwner::Field(f.clone()),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub enum Expr<EC: ExprContext + ?Sized> {
    Error,
    Hole(EC::Hole),
    And(Box<Expr<EC>>, Box<Expr<EC>>),
    BoolLiteral(bool),
    Block {
        label: Box<BlockLabel<EC>>,
        body: Box<Expr<EC>>,
    },
    Break {
        label: Box<BlockLabel<EC>>,
        value: Box<Expr<EC>>,
    },
    Builtin(Builtin<EC>),
    BindEnsures {
        value: Box<Expr<EC>>,
        variables: Vec<Box<LocalVariable<EC>>>,
    },
    BindErasedAlias {
        variable: Box<LocalVariable<EC>>,
        equality_witness: Option<Box<LocalVariable<EC>>>,
        value: Box<Expr<EC>>,
    },
    ConjunctionType {
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    Closure {
        v: Box<ClosureParameterVariable<EC>>,
        return_type: Box<Expr<EC>>,
        body: Box<Expr<EC>>,
    },
    DisjunctionType {
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    EqualToType {
        r#type: Box<Expr<EC>>,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    Condition {
        value: Box<Expr<EC>>,
        when_true_witness: Option<Box<LocalVariable<EC>>>,
        when_false_witness: Option<Box<LocalVariable<EC>>>,
    },
    EnumType(EnumType<EC>),
    EnumVariantLiteral {
        enum_type: EnumType<EC>,
        variant: EC::EnumVariant,
        arguments: Vec<Expr<EC>>,
        fields: Vec<RecordFieldLiteral<EC>>,
    },
    Finally {
        block_body: Box<Expr<EC>>,
        finally_body: Box<Expr<EC>>,
    },
    FunctionCall {
        function: EC::Function,
        arguments: Vec<Expr<EC>>,
    },
    FunctionObjectCall {
        function: Box<Expr<EC>>,
        argument: Box<Expr<EC>>,
    },
    FunctionResultValue {
        result_type: Box<Expr<EC>>,
    },
    FunctionType {
        a: Box<ClosureParameterVariable<EC>>,
        r: Box<Expr<EC>>,
    },
    IfElse {
        condition: Box<Expr<EC>>,
        when_true: Box<Expr<EC>>,
        when_false: Box<Expr<EC>>,
    },
    InstanceType(InstanceType<EC>),
    IntLiteral(BigInt),
    I8Literal(i8),
    U8Literal(u8),
    I16Literal(i16),
    U16Literal(u16),
    I32Literal(i32),
    U32Literal(u32),
    I64Literal(i64),
    U64Literal(u64),
    Is {
        value: Box<Expr<EC>>,
        pattern: Box<Pattern<EC>>,
    },
    Match {
        value: Box<Expr<EC>>,
        cases: Vec<MatchCase<EC>>,
    },
    MethodCall {
        method: EC::Method,
        instance_type: MethodInstanceType<EC>,
        receiver: Box<Expr<EC>>,
        arguments: Vec<Expr<EC>>,
    },
    NewInstance {
        instance: EC::Instance,
        arguments: Vec<Expr<EC>>,
    },
    Not(Box<Expr<EC>>),
    Or(Box<Expr<EC>>, Box<Expr<EC>>),
    Raise {
        ex: Box<Expr<EC>>,
    },
    RecordFieldLoad {
        record_type: Box<RecordType<EC>>,
        field: EC::RecordField,
        record_value: Box<Expr<EC>>,
    },
    RecordFieldStore {
        record_type: Box<RecordType<EC>>,
        field: EC::RecordField,
        record_value: Box<Expr<EC>>,
        new_value: Box<Expr<EC>>,
    },
    RecordLiteral {
        record_type: RecordType<EC>,
        fields: Vec<RecordFieldLiteral<EC>>,
    },
    RecordType(RecordType<EC>),
    Retry {
        label: Box<BlockLabel<EC>>,
    },
    Sequence(Vec1<Expr<EC>>),
    StringLiteral(Box<str>),
    TraitType(TraitType<EC>),
    Tuple {
        items: Vec<Expr<EC>>,
    },
    TupleElement(Box<Expr<EC>>, usize),
    Type(Box<Expr<EC>>),
    BigType(BigInt),
    Variable(Variable<EC>),
    VariableBinding(Box<LocalVariable<EC>>, Box<Expr<EC>>),
    VariableStore(Variable<EC>, Box<Expr<EC>>),
    BoxedType(Box<Expr<EC>>),
    Box {
        t: Box<Expr<EC>>,
        value: Box<Expr<EC>>,
    },
    Unbox {
        t: Box<Expr<EC>>,
        value: Box<Expr<EC>>,
    },
}

impl<EC: ExprContext + ?Sized> Unload for Expr<EC> {
    fn unload(&self) {}
}

impl<EC: ExprContext + ?Sized> Expr<EC> {
    pub fn bool_type() -> Expr<EC> {
        Expr::Builtin(Builtin::BoolType)
    }

    pub fn int_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::Int)
    }

    pub fn u8_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::U8)
    }

    pub fn i8_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::I8)
    }

    pub fn i16_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::I16)
    }

    pub fn u16_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::U16)
    }

    pub fn i32_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::I32)
    }

    pub fn u32_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::U32)
    }

    pub fn i64_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::I64)
    }

    pub fn u64_type() -> Expr<EC> {
        Expr::integer_type(IntegerType::U64)
    }

    pub fn integer_type(integer_type: IntegerType) -> Expr<EC> {
        Expr::Builtin(Builtin::IntType { integer_type })
    }

    pub fn string_type() -> Expr<EC> {
        Expr::Builtin(Builtin::StringType)
    }

    pub fn never_type() -> Expr<EC> {
        Expr::Builtin(Builtin::NeverType)
    }

    pub fn array_type(element_type: Expr<EC>) -> Expr<EC> {
        Expr::Builtin(Builtin::ArrayType {
            element_type: Box::new(element_type),
        })
    }

    pub fn unit() -> Expr<EC> {
        Expr::Tuple { items: vec![] }
    }

    pub fn type_n(level: impl Into<BigInt>) -> Expr<EC> {
        Expr::Type(Box::new(Expr::IntLiteral(level.into())))
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub enum Builtin<EC: ExprContext + ?Sized> {
    IntType {
        integer_type: IntegerType,
    },
    BoolType,
    StringType,
    NeverType,
    ArrayType {
        element_type: Box<Expr<EC>>,
    },
    IntNegate {
        integer_type: IntegerType,
        value: Box<Expr<EC>>,
    },
    IntBitNot {
        integer_type: IntegerType,
        value: Box<Expr<EC>>,
    },
    IntConvert {
        source_type: IntegerType,
        dest_type: IntegerType,
        value: Box<Expr<EC>>,
    },
    IntAdd {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntSub {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntMul {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntBitAnd {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntBitOr {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntBitXor {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntBitShiftLeft {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntBitShiftRight {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntEq {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntLt {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntLe {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntGt {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    IntGe {
        integer_type: IntegerType,
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    StringConcat {
        values: Vec<Expr<EC>>,
    },
    StringEq {
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    BoolEq {
        lhs: Box<Expr<EC>>,
        rhs: Box<Expr<EC>>,
    },
    ArrayCreateUnsafeUninitialized {
        element_type: Box<Expr<EC>>,
        length: Box<Expr<EC>>,
    },
    ArrayLength {
        element_type: Box<Expr<EC>>,
        array: Box<Expr<EC>>,
    },
    ArrayGet {
        element_type: Box<Expr<EC>>,
        array: Box<Expr<EC>>,
        index: Box<Expr<EC>>,
    },
    ArraySet {
        element_type: Box<Expr<EC>>,
        array: Box<Expr<EC>>,
        index: Box<Expr<EC>>,
        value: Box<Expr<EC>>,
    },
    EqualToRefl {
        r#type: Box<Expr<EC>>,
        value: Box<Expr<EC>>,
    },
    UnsafeAssumeErased {
        r#type: Box<Expr<EC>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntegerType {
    Int,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
}

impl IntegerType {
    pub fn type_name(self) -> &'static str {
        match self {
            IntegerType::Int => "int_type",
            IntegerType::I8 => "i8_type",
            IntegerType::U8 => "u8_type",
            IntegerType::I16 => "i16_type",
            IntegerType::U16 => "u16_type",
            IntegerType::I32 => "i32_type",
            IntegerType::U32 => "u32_type",
            IntegerType::I64 => "i64_type",
            IntegerType::U64 => "u64_type",
        }
    }
}

impl<EC: ExprContext + ?Sized> Builtin<EC> {
    pub fn as_str(&self) -> &'static str {
        match self {
            Builtin::IntType { integer_type } => integer_type.type_name(),
            Builtin::BoolType => "bool_type",
            Builtin::StringType => "string_type",
            Builtin::NeverType => "never_type",
            Builtin::ArrayType { .. } => "array_type",
            Builtin::IntNegate { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_negate",
                IntegerType::I8 => "i8_negate",
                IntegerType::U8 => "u8_negate",
                IntegerType::I16 => "i16_negate",
                IntegerType::U16 => "u16_negate",
                IntegerType::I32 => "i32_negate",
                IntegerType::U32 => "u32_negate",
                IntegerType::I64 => "i64_negate",
                IntegerType::U64 => "u64_negate",
            },
            Builtin::IntBitNot { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_bitnot",
                IntegerType::I8 => "i8_bitnot",
                IntegerType::U8 => "u8_bitnot",
                IntegerType::I16 => "i16_bitnot",
                IntegerType::U16 => "u16_bitnot",
                IntegerType::I32 => "i32_bitnot",
                IntegerType::U32 => "u32_bitnot",
                IntegerType::I64 => "i64_bitnot",
                IntegerType::U64 => "u64_bitnot",
            },
            Builtin::IntConvert {
                source_type,
                dest_type,
                ..
            } => match (source_type, dest_type) {
                (IntegerType::Int, IntegerType::Int) => "int_to_int",
                (IntegerType::Int, IntegerType::I8) => "int_to_i8",
                (IntegerType::Int, IntegerType::U8) => "int_to_u8",
                (IntegerType::Int, IntegerType::I16) => "int_to_i16",
                (IntegerType::Int, IntegerType::U16) => "int_to_u16",
                (IntegerType::Int, IntegerType::I32) => "int_to_i32",
                (IntegerType::Int, IntegerType::U32) => "int_to_u32",
                (IntegerType::Int, IntegerType::I64) => "int_to_i64",
                (IntegerType::Int, IntegerType::U64) => "int_to_u64",
                (IntegerType::I8, IntegerType::Int) => "i8_to_int",
                (IntegerType::I8, IntegerType::I8) => "i8_to_i8",
                (IntegerType::I8, IntegerType::U8) => "i8_to_u8",
                (IntegerType::I8, IntegerType::I16) => "i8_to_i16",
                (IntegerType::I8, IntegerType::U16) => "i8_to_u16",
                (IntegerType::I8, IntegerType::I32) => "i8_to_i32",
                (IntegerType::I8, IntegerType::U32) => "i8_to_u32",
                (IntegerType::I8, IntegerType::I64) => "i8_to_i64",
                (IntegerType::I8, IntegerType::U64) => "i8_to_u64",
                (IntegerType::U8, IntegerType::Int) => "u8_to_int",
                (IntegerType::U8, IntegerType::I8) => "u8_to_i8",
                (IntegerType::U8, IntegerType::U8) => "u8_to_u8",
                (IntegerType::U8, IntegerType::I16) => "u8_to_i16",
                (IntegerType::U8, IntegerType::U16) => "u8_to_u16",
                (IntegerType::U8, IntegerType::I32) => "u8_to_i32",
                (IntegerType::U8, IntegerType::U32) => "u8_to_u32",
                (IntegerType::U8, IntegerType::I64) => "u8_to_i64",
                (IntegerType::U8, IntegerType::U64) => "u8_to_u64",
                (IntegerType::I16, IntegerType::Int) => "i16_to_int",
                (IntegerType::I16, IntegerType::I8) => "i16_to_i8",
                (IntegerType::I16, IntegerType::U8) => "i16_to_u8",
                (IntegerType::I16, IntegerType::I16) => "i16_to_i16",
                (IntegerType::I16, IntegerType::U16) => "i16_to_u16",
                (IntegerType::I16, IntegerType::I32) => "i16_to_i32",
                (IntegerType::I16, IntegerType::U32) => "i16_to_u32",
                (IntegerType::I16, IntegerType::I64) => "i16_to_i64",
                (IntegerType::I16, IntegerType::U64) => "i16_to_u64",
                (IntegerType::U16, IntegerType::Int) => "u16_to_int",
                (IntegerType::U16, IntegerType::I8) => "u16_to_i8",
                (IntegerType::U16, IntegerType::U8) => "u16_to_u8",
                (IntegerType::U16, IntegerType::I16) => "u16_to_i16",
                (IntegerType::U16, IntegerType::U16) => "u16_to_u16",
                (IntegerType::U16, IntegerType::I32) => "u16_to_i32",
                (IntegerType::U16, IntegerType::U32) => "u16_to_u32",
                (IntegerType::U16, IntegerType::I64) => "u16_to_i64",
                (IntegerType::U16, IntegerType::U64) => "u16_to_u64",
                (IntegerType::I32, IntegerType::Int) => "i32_to_int",
                (IntegerType::I32, IntegerType::I8) => "i32_to_i8",
                (IntegerType::I32, IntegerType::U8) => "i32_to_u8",
                (IntegerType::I32, IntegerType::I16) => "i32_to_i16",
                (IntegerType::I32, IntegerType::U16) => "i32_to_u16",
                (IntegerType::I32, IntegerType::I32) => "i32_to_i32",
                (IntegerType::I32, IntegerType::U32) => "i32_to_u32",
                (IntegerType::I32, IntegerType::I64) => "i32_to_i64",
                (IntegerType::I32, IntegerType::U64) => "i32_to_u64",
                (IntegerType::U32, IntegerType::Int) => "u32_to_int",
                (IntegerType::U32, IntegerType::I8) => "u32_to_i8",
                (IntegerType::U32, IntegerType::U8) => "u32_to_u8",
                (IntegerType::U32, IntegerType::I16) => "u32_to_i16",
                (IntegerType::U32, IntegerType::U16) => "u32_to_u16",
                (IntegerType::U32, IntegerType::I32) => "u32_to_i32",
                (IntegerType::U32, IntegerType::U32) => "u32_to_u32",
                (IntegerType::U32, IntegerType::I64) => "u32_to_i64",
                (IntegerType::U32, IntegerType::U64) => "u32_to_u64",
                (IntegerType::I64, IntegerType::Int) => "i64_to_int",
                (IntegerType::I64, IntegerType::I8) => "i64_to_i8",
                (IntegerType::I64, IntegerType::U8) => "i64_to_u8",
                (IntegerType::I64, IntegerType::I16) => "i64_to_i16",
                (IntegerType::I64, IntegerType::U16) => "i64_to_u16",
                (IntegerType::I64, IntegerType::I32) => "i64_to_i32",
                (IntegerType::I64, IntegerType::U32) => "i64_to_u32",
                (IntegerType::I64, IntegerType::I64) => "i64_to_i64",
                (IntegerType::I64, IntegerType::U64) => "i64_to_u64",
                (IntegerType::U64, IntegerType::Int) => "u64_to_int",
                (IntegerType::U64, IntegerType::I8) => "u64_to_i8",
                (IntegerType::U64, IntegerType::U8) => "u64_to_u8",
                (IntegerType::U64, IntegerType::I16) => "u64_to_i16",
                (IntegerType::U64, IntegerType::U16) => "u64_to_u16",
                (IntegerType::U64, IntegerType::I32) => "u64_to_i32",
                (IntegerType::U64, IntegerType::U32) => "u64_to_u32",
                (IntegerType::U64, IntegerType::I64) => "u64_to_i64",
                (IntegerType::U64, IntegerType::U64) => "u64_to_u64",
            },
            Builtin::IntAdd { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_add",
                IntegerType::I8 => "i8_add",
                IntegerType::U8 => "u8_add",
                IntegerType::I16 => "i16_add",
                IntegerType::U16 => "u16_add",
                IntegerType::I32 => "i32_add",
                IntegerType::U32 => "u32_add",
                IntegerType::I64 => "i64_add",
                IntegerType::U64 => "u64_add",
            },
            Builtin::IntSub { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_sub",
                IntegerType::I8 => "i8_sub",
                IntegerType::U8 => "u8_sub",
                IntegerType::I16 => "i16_sub",
                IntegerType::U16 => "u16_sub",
                IntegerType::I32 => "i32_sub",
                IntegerType::U32 => "u32_sub",
                IntegerType::I64 => "i64_sub",
                IntegerType::U64 => "u64_sub",
            },
            Builtin::IntMul { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_mul",
                IntegerType::I8 => "i8_mul",
                IntegerType::U8 => "u8_mul",
                IntegerType::I16 => "i16_mul",
                IntegerType::U16 => "u16_mul",
                IntegerType::I32 => "i32_mul",
                IntegerType::U32 => "u32_mul",
                IntegerType::I64 => "i64_mul",
                IntegerType::U64 => "u64_mul",
            },
            Builtin::IntBitAnd { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_bitand",
                IntegerType::I8 => "i8_bitand",
                IntegerType::U8 => "u8_bitand",
                IntegerType::I16 => "i16_bitand",
                IntegerType::U16 => "u16_bitand",
                IntegerType::I32 => "i32_bitand",
                IntegerType::U32 => "u32_bitand",
                IntegerType::I64 => "i64_bitand",
                IntegerType::U64 => "u64_bitand",
            },
            Builtin::IntBitOr { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_bitor",
                IntegerType::I8 => "i8_bitor",
                IntegerType::U8 => "u8_bitor",
                IntegerType::I16 => "i16_bitor",
                IntegerType::U16 => "u16_bitor",
                IntegerType::I32 => "i32_bitor",
                IntegerType::U32 => "u32_bitor",
                IntegerType::I64 => "i64_bitor",
                IntegerType::U64 => "u64_bitor",
            },
            Builtin::IntBitXor { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_bitxor",
                IntegerType::I8 => "i8_bitxor",
                IntegerType::U8 => "u8_bitxor",
                IntegerType::I16 => "i16_bitxor",
                IntegerType::U16 => "u16_bitxor",
                IntegerType::I32 => "i32_bitxor",
                IntegerType::U32 => "u32_bitxor",
                IntegerType::I64 => "i64_bitxor",
                IntegerType::U64 => "u64_bitxor",
            },
            Builtin::IntBitShiftLeft { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_bitshiftleft",
                IntegerType::I8 => "i8_bitshiftleft",
                IntegerType::U8 => "u8_bitshiftleft",
                IntegerType::I16 => "i16_bitshiftleft",
                IntegerType::U16 => "u16_bitshiftleft",
                IntegerType::I32 => "i32_bitshiftleft",
                IntegerType::U32 => "u32_bitshiftleft",
                IntegerType::I64 => "i64_bitshiftleft",
                IntegerType::U64 => "u64_bitshiftleft",
            },
            Builtin::IntBitShiftRight { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_bitshiftright",
                IntegerType::I8 => "i8_bitshiftright",
                IntegerType::U8 => "u8_bitshiftright",
                IntegerType::I16 => "i16_bitshiftright",
                IntegerType::U16 => "u16_bitshiftright",
                IntegerType::I32 => "i32_bitshiftright",
                IntegerType::U32 => "u32_bitshiftright",
                IntegerType::I64 => "i64_bitshiftright",
                IntegerType::U64 => "u64_bitshiftright",
            },
            Builtin::IntEq { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_eq",
                IntegerType::I8 => "i8_eq",
                IntegerType::U8 => "u8_eq",
                IntegerType::I16 => "i16_eq",
                IntegerType::U16 => "u16_eq",
                IntegerType::I32 => "i32_eq",
                IntegerType::U32 => "u32_eq",
                IntegerType::I64 => "i64_eq",
                IntegerType::U64 => "u64_eq",
            },
            Builtin::IntLt { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_lt",
                IntegerType::I8 => "i8_lt",
                IntegerType::U8 => "u8_lt",
                IntegerType::I16 => "i16_lt",
                IntegerType::U16 => "u16_lt",
                IntegerType::I32 => "i32_lt",
                IntegerType::U32 => "u32_lt",
                IntegerType::I64 => "i64_lt",
                IntegerType::U64 => "u64_lt",
            },
            Builtin::IntLe { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_le",
                IntegerType::I8 => "i8_le",
                IntegerType::U8 => "u8_le",
                IntegerType::I16 => "i16_le",
                IntegerType::U16 => "u16_le",
                IntegerType::I32 => "i32_le",
                IntegerType::U32 => "u32_le",
                IntegerType::I64 => "i64_le",
                IntegerType::U64 => "u64_le",
            },
            Builtin::IntGt { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_gt",
                IntegerType::I8 => "i8_gt",
                IntegerType::U8 => "u8_gt",
                IntegerType::I16 => "i16_gt",
                IntegerType::U16 => "u16_gt",
                IntegerType::I32 => "i32_gt",
                IntegerType::U32 => "u32_gt",
                IntegerType::I64 => "i64_gt",
                IntegerType::U64 => "u64_gt",
            },
            Builtin::IntGe { integer_type, .. } => match integer_type {
                IntegerType::Int => "int_ge",
                IntegerType::I8 => "i8_ge",
                IntegerType::U8 => "u8_ge",
                IntegerType::I16 => "i16_ge",
                IntegerType::U16 => "u16_ge",
                IntegerType::I32 => "i32_ge",
                IntegerType::U32 => "u32_ge",
                IntegerType::I64 => "i64_ge",
                IntegerType::U64 => "u64_ge",
            },
            Builtin::StringConcat { .. } => "string_concat",
            Builtin::StringEq { .. } => "string_eq",
            Builtin::BoolEq { .. } => "bool_eq",
            Builtin::ArrayCreateUnsafeUninitialized { .. } => "array_create_unsafe_uninitialized",
            Builtin::ArrayLength { .. } => "array_length",
            Builtin::ArrayGet { .. } => "array_get",
            Builtin::ArraySet { .. } => "array_set",
            Builtin::EqualToRefl { .. } => "equal_to_refl",
            Builtin::UnsafeAssumeErased { .. } => "unsafe_assume_erased",
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct MatchCase<EC: ExprContext + ?Sized> {
    pub pattern: Pattern<EC>,
    pub body: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for MatchCase<EC> {
    fn clone(&self) -> Self {
        Self {
            pattern: self.pattern.clone(),
            body: self.body.clone(),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct RecordFieldLiteral<EC: ExprContext + ?Sized> {
    pub field: EC::RecordField,
    pub value: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for RecordFieldLiteral<EC> {
    fn clone(&self) -> Self {
        Self {
            field: self.field.clone(),
            value: self.value.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ErasureMode {
    Erased,
    Token,
    Concrete,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct RecordType<EC: ExprContext + ?Sized> {
    pub record: EC::Record,
    pub arguments: Vec<Expr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct EnumType<EC: ExprContext + ?Sized> {
    pub enum_: EC::Enum,
    pub arguments: Vec<Expr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct TraitType<EC: ExprContext + ?Sized> {
    pub trait_: EC::Trait,
    pub arguments: Vec<Expr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct InstanceType<EC: ExprContext + ?Sized> {
    pub instance: EC::Instance,
    pub arguments: Vec<Expr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub enum MethodInstanceType<EC: ExprContext + ?Sized> {
    Trait(TraitType<EC>),
}

impl<EC: ExprContext + ?Sized> MethodInstanceType<EC> {
    pub fn into_expr(self) -> Expr<EC> {
        match self {
            MethodInstanceType::Trait(trait_type) => Expr::TraitType(trait_type),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub enum Variable<EC: ExprContext + ?Sized> {
    Local(Box<LocalVariable<EC>>),
    Parameter(Box<ParameterVariable<EC>>),
    InstanceParameter(Box<InstanceParameterVariable<EC>>),
    ClosureParameter(Box<ClosureParameterVariable<EC>>),
}

impl<EC: ExprContext + ?Sized> Variable<EC> {
    pub fn name(&self) -> Option<&Identifier> {
        match self {
            Variable::Local(variable) => variable.name.as_ref(),
            Variable::Parameter(variable) => variable.name.as_ref(),
            Variable::InstanceParameter(variable) => variable.name.as_ref(),
            Variable::ClosureParameter(variable) => variable.name.as_ref(),
        }
    }

    pub fn var_type(&self) -> &Expr<EC> {
        match self {
            Variable::Local(variable) => &variable.var_type,
            Variable::Parameter(variable) => &variable.var_type,
            Variable::InstanceParameter(variable) => &variable.var_type,
            Variable::ClosureParameter(variable) => &variable.var_type,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Variable::Local(variable) => variable.is_mutable,
            Variable::Parameter(_) => false,
            Variable::InstanceParameter(_) => false,
            Variable::ClosureParameter(v) => v.is_mutable,
        }
    }

    pub fn erasure_mode(&self) -> ErasureMode {
        match self {
            Variable::Local(variable) => variable.erasure_mode,
            Variable::Parameter(variable) => variable.erasure_mode,
            Variable::InstanceParameter(_) => ErasureMode::Concrete,
            Variable::ClosureParameter(variable) => variable.erasure_mode,
        }
    }

    pub fn is_witness(&self) -> bool {
        match self {
            Variable::Local(variable) => variable.is_witness,
            Variable::Parameter(param) => param.is_witness,
            Variable::InstanceParameter(_) => false,
            Variable::ClosureParameter(v) => v.is_witness,
        }
    }
}

pub struct VariableTupleElement<EC: ExprContext + ?Sized> {
    pub variable: Variable<EC>,
    pub index: usize,
    pub binding_type: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for VariableTupleElement<EC> {
    fn clone(&self) -> Self {
        Self {
            variable: self.variable.clone(),
            index: self.index,
            binding_type: self.binding_type.clone(),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub struct LocalVariable<EC: ExprContext + ?Sized> {
    pub id: UniqueIdentifier,
    pub name: Option<Identifier>,
    pub var_type: Expr<EC>,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
    pub is_mutable: bool,
}

impl<EC: ExprContext + ?Sized> PartialEq for LocalVariable<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<EC: ExprContext + ?Sized> Eq for LocalVariable<EC> {}

impl<EC: ExprContext + ?Sized> Hash for LocalVariable<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub struct ParameterVariable<EC: ExprContext + ?Sized> {
    pub owner: ExpressionOwner<EC>,
    pub parameter_index: usize,
    pub var_type: Expr<EC>,
    pub name: Option<Identifier>,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
}

impl<EC: ExprContext + ?Sized> PartialEq for ParameterVariable<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.owner == other.owner && self.parameter_index == other.parameter_index
    }
}

impl<EC: ExprContext + ?Sized> Eq for ParameterVariable<EC> {}

impl<EC: ExprContext + ?Sized> Hash for ParameterVariable<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.owner.hash(state);
        self.parameter_index.hash(state);
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub struct InstanceParameterVariable<EC: ExprContext + ?Sized> {
    pub owner: ExpressionOwner<EC>,
    pub var_type: Expr<EC>,
    pub name: Option<Identifier>,
}

impl<EC: ExprContext + ?Sized> PartialEq for InstanceParameterVariable<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.owner == other.owner
    }
}

impl<EC: ExprContext + ?Sized> Eq for InstanceParameterVariable<EC> {}

impl<EC: ExprContext + ?Sized> Hash for InstanceParameterVariable<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.owner.hash(state);
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub struct ClosureParameterVariable<EC: ExprContext + ?Sized> {
    pub id: UniqueIdentifier,
    pub var_type: Expr<EC>,
    pub name: Option<Identifier>,
    pub is_mutable: bool,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
}

impl<EC: ExprContext + ?Sized> PartialEq for ClosureParameterVariable<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<EC: ExprContext + ?Sized> Eq for ClosureParameterVariable<EC> {}

impl<EC: ExprContext + ?Sized> Hash for ClosureParameterVariable<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::{Expr, ExprContext};
    use core::fmt::Debug;

    struct ContextWithoutDebug;

    impl ExprContext for ContextWithoutDebug {
        type Hole = u8;
        type Function = u8;
        type Record = u8;
        type RecordField = u8;
        type Enum = u8;
        type Trait = u8;
        type EnumVariant = u8;
        type Method = u8;
        type Instance = u8;
    }

    fn assert_debug<T: Debug>() {}

    #[test]
    fn expr_debug_does_not_require_context_debug() {
        assert_debug::<Expr<ContextWithoutDebug>>();
    }
}
