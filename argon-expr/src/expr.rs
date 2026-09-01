use crate::{BlockLabel, LocatedPattern};
use alloc::{boxed::Box, vec, vec::Vec};
use argon_parser::ast::Identifier;
use argon_util::{UniqueIdentifier, Unload};
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use derivative::Derivative;
use mitsein::vec1::Vec1;
use num_bigint::BigInt;
use parse18_runtime::Location;

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct LocatedExpr<EC: ExprContext + ?Sized> {
    pub value: Expr<EC>,
    pub location: Location,
}

impl<EC: ExprContext + ?Sized> LocatedExpr<EC> {
    pub fn new(value: Expr<EC>, location: Location) -> Self {
        LocatedExpr { value, location }
    }

    pub fn map<EC2: ExprContext + ?Sized>(
        self,
        f: impl FnOnce(Expr<EC>) -> Expr<EC2>,
    ) -> LocatedExpr<EC2> {
        LocatedExpr {
            value: f(self.value),
            location: self.location,
        }
    }

    pub fn error(location: Location) -> Self {
        LocatedExpr {
            value: Expr::Error,
            location,
        }
    }
}

impl<EC: ExprContext + ?Sized> Debug for LocatedExpr<EC>
where
    Expr<EC>: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LocatedExpr")
            .field("value", &self.value)
            .field("location", &self.location)
            .finish()
    }
}

impl<EC: ExprContext + ?Sized> PartialEq for LocatedExpr<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<EC: ExprContext + ?Sized> Eq for LocatedExpr<EC> {}

impl<EC: ExprContext + ?Sized> Hash for LocatedExpr<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

pub trait ExprLocationExt<EC: ExprContext + ?Sized>: Sized {
    fn with_location(self, location: Location) -> LocatedExpr<EC>;
}

impl<EC: ExprContext + ?Sized> ExprLocationExt<EC> for Expr<EC> {
    fn with_location(self, location: Location) -> LocatedExpr<EC> {
        LocatedExpr::new(self, location)
    }
}

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
    And(Box<LocatedExpr<EC>>, Box<LocatedExpr<EC>>),
    BoolLiteral(bool),
    Block {
        label: Box<BlockLabel<EC>>,
        body: Box<LocatedExpr<EC>>,
    },
    Break {
        label: Box<BlockLabel<EC>>,
        value: Box<LocatedExpr<EC>>,
    },
    Builtin(Builtin<EC>),
    BindEnsures {
        value: Box<LocatedExpr<EC>>,
        variables: Vec<Box<LocalVariable<EC>>>,
    },
    BindErasedAlias {
        variable: Box<LocalVariable<EC>>,
        equality_witness: Option<Box<LocalVariable<EC>>>,
        value: Box<LocatedExpr<EC>>,
    },
    ConjunctionType {
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    Closure {
        v: Box<ClosureParameterVariable<EC>>,
        return_type: Box<LocatedExpr<EC>>,
        body: Box<LocatedExpr<EC>>,
    },
    DisjunctionType {
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    EqualToType {
        r#type: Box<LocatedExpr<EC>>,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    Condition {
        value: Box<LocatedExpr<EC>>,
        when_true_witness: Option<Box<LocalVariable<EC>>>,
        when_false_witness: Option<Box<LocalVariable<EC>>>,
    },
    EnumType(EnumType<EC>),
    EnumVariantLiteral {
        enum_type: EnumType<EC>,
        variant: EC::EnumVariant,
        arguments: Vec<LocatedExpr<EC>>,
        fields: Vec<RecordFieldLiteral<EC>>,
    },
    Finally {
        block_body: Box<LocatedExpr<EC>>,
        finally_body: Box<LocatedExpr<EC>>,
    },
    FunctionCall {
        function: EC::Function,
        arguments: Vec<LocatedExpr<EC>>,
    },
    FunctionObjectCall {
        function: Box<LocatedExpr<EC>>,
        argument: Box<LocatedExpr<EC>>,
    },
    FunctionResultValue {
        result_type: Box<LocatedExpr<EC>>,
    },
    FunctionType {
        a: Box<ClosureParameterVariable<EC>>,
        r: Box<LocatedExpr<EC>>,
    },
    IfElse {
        condition: Box<LocatedExpr<EC>>,
        when_true: Box<LocatedExpr<EC>>,
        when_false: Box<LocatedExpr<EC>>,
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
        value: Box<LocatedExpr<EC>>,
        pattern: Box<LocatedPattern<EC>>,
    },
    Match {
        value: Box<LocatedExpr<EC>>,
        cases: Vec<MatchCase<EC>>,
    },
    MethodCall {
        method: EC::Method,
        instance_type: MethodInstanceType<EC>,
        receiver: Box<LocatedExpr<EC>>,
        arguments: Vec<LocatedExpr<EC>>,
    },
    NewInstance {
        instance: EC::Instance,
        arguments: Vec<LocatedExpr<EC>>,
    },
    Not(Box<LocatedExpr<EC>>),
    Or(Box<LocatedExpr<EC>>, Box<LocatedExpr<EC>>),
    Raise {
        ex: Box<LocatedExpr<EC>>,
    },
    RecordFieldLoad {
        record_type: Box<RecordType<EC>>,
        field: EC::RecordField,
        record_value: Box<LocatedExpr<EC>>,
    },
    RecordFieldStore {
        record_type: Box<RecordType<EC>>,
        field: EC::RecordField,
        record_value: Box<LocatedExpr<EC>>,
        new_value: Box<LocatedExpr<EC>>,
    },
    RecordLiteral {
        record_type: RecordType<EC>,
        fields: Vec<RecordFieldLiteral<EC>>,
    },
    Use {
        is_mutable: bool,
        inner: Box<LocatedExpr<EC>>,
    },
    Shared {
        inner: Box<LocatedExpr<EC>>,
    },
    Share {
        value: Box<LocatedExpr<EC>>,
    },
    Borrow {
        value: Box<LocatedExpr<EC>>,
    },
    BorrowMut {
        value: Box<LocatedExpr<EC>>,
    },
    RecordType(RecordType<EC>),
    Retry {
        label: Box<BlockLabel<EC>>,
    },
    Sequence(Vec1<LocatedExpr<EC>>),
    StringLiteral(Box<str>),
    TraitType(TraitType<EC>),
    Tuple {
        items: Vec<LocatedExpr<EC>>,
    },
    TupleElement(Box<LocatedExpr<EC>>, usize),
    Type(Box<LocatedExpr<EC>>),
    BigType(BigInt),
    Variable(Variable<EC>),
    VariableBinding(Box<LocalVariable<EC>>, Box<LocatedExpr<EC>>),
    VariableStore(Variable<EC>, Box<LocatedExpr<EC>>),
    BoxedType(Box<LocatedExpr<EC>>),
    Box {
        t: Box<LocatedExpr<EC>>,
        value: Box<LocatedExpr<EC>>,
    },
    Unbox {
        t: Box<LocatedExpr<EC>>,
        value: Box<LocatedExpr<EC>>,
    },
}

impl<EC: ExprContext + ?Sized> Unload for Expr<EC> {
    fn unload(&self) {}
}

impl<EC: ExprContext + ?Sized> Unload for LocatedExpr<EC> {
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

    pub fn array_type(element_type: LocatedExpr<EC>) -> Expr<EC> {
        Expr::Builtin(Builtin::ArrayType {
            element_type: Box::new(element_type),
        })
    }

    pub fn unit() -> Expr<EC> {
        Expr::Tuple { items: vec![] }
    }

    pub fn type_n(level: impl Into<BigInt>) -> Expr<EC> {
        Expr::BigType(level.into())
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
        element_type: Box<LocatedExpr<EC>>,
    },
    IntNegate {
        integer_type: IntegerType,
        value: Box<LocatedExpr<EC>>,
    },
    IntBitNot {
        integer_type: IntegerType,
        value: Box<LocatedExpr<EC>>,
    },
    IntConvert {
        source_type: IntegerType,
        dest_type: IntegerType,
        value: Box<LocatedExpr<EC>>,
    },
    IntAdd {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntSub {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntMul {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntBitAnd {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntBitOr {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntBitXor {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntBitShiftLeft {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntBitShiftRight {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntEq {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntLt {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntLe {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntGt {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    IntGe {
        integer_type: IntegerType,
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    StringConcat {
        values: Vec<LocatedExpr<EC>>,
    },
    StringEq {
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    BoolEq {
        lhs: Box<LocatedExpr<EC>>,
        rhs: Box<LocatedExpr<EC>>,
    },
    ArrayCreateUnsafeUninitialized {
        element_type: Box<LocatedExpr<EC>>,
        length: Box<LocatedExpr<EC>>,
    },
    ArrayLength {
        element_type: Box<LocatedExpr<EC>>,
        array: Box<LocatedExpr<EC>>,
    },
    ArrayGet {
        element_type: Box<LocatedExpr<EC>>,
        array: Box<LocatedExpr<EC>>,
        index: Box<LocatedExpr<EC>>,
    },
    ArraySet {
        element_type: Box<LocatedExpr<EC>>,
        array: Box<LocatedExpr<EC>>,
        index: Box<LocatedExpr<EC>>,
        value: Box<LocatedExpr<EC>>,
    },
    EqualToRefl {
        r#type: Box<LocatedExpr<EC>>,
        value: Box<LocatedExpr<EC>>,
    },
    UnsafeAssumeErased {
        r#type: Box<LocatedExpr<EC>>,
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
    pub pattern: LocatedPattern<EC>,
    pub body: LocatedExpr<EC>,
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
    pub value: LocatedExpr<EC>,
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
    pub arguments: Vec<LocatedExpr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct EnumType<EC: ExprContext + ?Sized> {
    pub enum_: EC::Enum,
    pub arguments: Vec<LocatedExpr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct TraitType<EC: ExprContext + ?Sized> {
    pub trait_: EC::Trait,
    pub arguments: Vec<LocatedExpr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct InstanceType<EC: ExprContext + ?Sized> {
    pub instance: EC::Instance,
    pub arguments: Vec<LocatedExpr<EC>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub enum MethodInstanceType<EC: ExprContext + ?Sized> {
    Record(RecordType<EC>),
    Enum(EnumType<EC>),
    Trait(TraitType<EC>),
}

impl<EC: ExprContext + ?Sized> MethodInstanceType<EC> {
    pub fn into_expr(self) -> Expr<EC> {
        match self {
            MethodInstanceType::Record(record_type) => Expr::RecordType(record_type),
            MethodInstanceType::Enum(enum_type) => Expr::EnumType(enum_type),
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

    pub fn var_type(&self) -> &LocatedExpr<EC> {
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
    pub binding_type: LocatedExpr<EC>,
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
    pub var_type: LocatedExpr<EC>,
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
    pub var_type: LocatedExpr<EC>,
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
    pub var_type: LocatedExpr<EC>,
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
    pub var_type: LocatedExpr<EC>,
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
