use alloc::{boxed::Box, vec::Vec};
use argon_expr::{ExprContext, IntegerType};
use derivative::Derivative;
use num_bigint::BigInt;

use crate::{
    BlockLabel, EnumType, LocatedPattern, MethodInstanceType, ProgramPoint, RecordType, Region,
    Register, RegisterDeclaration,
};

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct MatchCase<EC: ExprContext + ?Sized> {
    pub pattern: LocatedPattern<EC>,
    pub body: Region<EC>,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct FieldRegister<EC: ExprContext + ?Sized> {
    pub field: EC::RecordField,
    pub value: Register,
}

/// An executable operation. Every value consumed at runtime is named by a register.
#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum Instruction<EC: ExprContext + ?Sized> {
    /// Introduces a register and its source-level metadata into the function body.
    DeclareRegister {
        declaration: RegisterDeclaration<EC>,
    },
    Hole {
        destination: Register,
        hole: EC::Hole,
    },
    BoolLiteral {
        destination: Register,
        value: bool,
    },
    IntLiteral {
        destination: Register,
        value: BigInt,
    },
    I8Literal {
        destination: Register,
        value: i8,
    },
    U8Literal {
        destination: Register,
        value: u8,
    },
    I16Literal {
        destination: Register,
        value: i16,
    },
    U16Literal {
        destination: Register,
        value: u16,
    },
    I32Literal {
        destination: Register,
        value: i32,
    },
    U32Literal {
        destination: Register,
        value: u32,
    },
    I64Literal {
        destination: Register,
        value: i64,
    },
    U64Literal {
        destination: Register,
        value: u64,
    },
    StringLiteral {
        destination: Register,
        value: Box<str>,
    },
    Tuple {
        destination: Register,
        elements: Vec<Register>,
    },
    RecordLiteral {
        destination: Register,
        record_type: RecordType<EC>,
        fields: Vec<FieldRegister<EC>>,
    },
    EnumVariantLiteral {
        destination: Register,
        enum_type: EnumType<EC>,
        variant: EC::EnumVariant,
        arguments: Vec<Register>,
        fields: Vec<FieldRegister<EC>>,
    },
    Type {
        destination: Register,
        value: Register,
    },
    BigType {
        destination: Register,
        level: BigInt,
    },
    IntType {
        destination: Register,
        integer_type: IntegerType,
    },
    BoolType {
        destination: Register,
    },
    StringType {
        destination: Register,
    },
    NeverType {
        destination: Register,
    },
    ArrayType {
        destination: Register,
        element_type: Register,
    },
    FunctionResult {
        destination: Register,
        result_type: Register,
    },
    FunctionType {
        destination: Register,
        parameter: Register,
        result: Register,
    },
    ConjunctionType {
        destination: Register,
        lhs: Register,
        rhs: Register,
    },
    DisjunctionType {
        destination: Register,
        lhs: Register,
        rhs: Register,
    },
    EqualToType {
        destination: Register,
        r#type: Register,
        lhs: Register,
        rhs: Register,
    },
    RecordType {
        destination: Register,
        record: EC::Record,
        arguments: Vec<Register>,
    },
    EnumType {
        destination: Register,
        enum_: EC::Enum,
        arguments: Vec<Register>,
    },
    TraitType {
        destination: Register,
        trait_: EC::Trait,
        arguments: Vec<Register>,
    },
    InstanceType {
        destination: Register,
        instance: EC::Instance,
        arguments: Vec<Register>,
    },
    SharedType {
        destination: Register,
        inner: Register,
    },
    UseType {
        destination: Register,
        is_mutable: bool,
        inner: Register,
    },
    BoxedType {
        destination: Register,
        inner: Register,
    },
    /// Load the value held by `register` at `at`.
    ///
    /// This instruction is only valid in erased or token mode. That invariant is
    /// intentionally left to later IR validation.
    LoadRegisterSnapshot {
        destination: Register,
        register: Register,
        at: ProgramPoint,
    },
    /// Copy initializes or reassigns the destination register.
    Copy {
        destination: Register,
        source: Register,
    },
    FunctionCall {
        destination: Register,
        function: EC::Function,
        arguments: Vec<Register>,
    },
    FunctionObjectCall {
        destination: Register,
        function: Register,
        argument: Register,
    },
    MethodCall {
        destination: Register,
        method: EC::Method,
        instance_type: MethodInstanceType<EC>,
        receiver: Register,
        arguments: Vec<Register>,
    },
    StaticMethodCall {
        destination: Register,
        method: EC::StaticMethod,
        owner_type: MethodInstanceType<EC>,
        arguments: Vec<Register>,
    },
    NewInstance {
        destination: Register,
        instance: EC::Instance,
        arguments: Vec<Register>,
    },
    Builtin {
        destination: Register,
        builtin: Builtin<EC>,
    },
    TupleElement {
        destination: Register,
        tuple: Register,
        index: usize,
    },
    RecordFieldLoad {
        destination: Register,
        record_type: RecordType<EC>,
        field: EC::RecordField,
        record: Register,
    },
    RecordFieldStore {
        record_type: RecordType<EC>,
        field: EC::RecordField,
        record: Register,
        value: Register,
    },
    BindEnsures {
        value: Register,
        witnesses: Vec<Register>,
    },
    BindErasedAlias {
        destination: Register,
        equality_witness: Option<Register>,
        value: Register,
    },
    Share {
        destination: Register,
        value: Register,
    },
    Borrow {
        destination: Register,
        value: Register,
    },
    BorrowMut {
        destination: Register,
        value: Register,
    },
    Box {
        destination: Register,
        r#type: Register,
        value: Register,
    },
    Unbox {
        destination: Register,
        r#type: Register,
        value: Register,
    },
    Raise {
        exception: Register,
    },
    Break {
        label: BlockLabel<EC>,
        value: Register,
    },
    Retry {
        label: BlockLabel<EC>,
    },
    Condition {
        destination: Register,
        value: Register,
        when_true_witness: Option<Register>,
        when_false_witness: Option<Register>,
    },
    IfElse {
        destination: Register,
        condition: Register,
        when_true: Region<EC>,
        when_false: Region<EC>,
    },
    Match {
        destination: Register,
        value: Register,
        cases: Vec<MatchCase<EC>>,
    },
    Block {
        destination: Register,
        label: BlockLabel<EC>,
        body: Region<EC>,
    },
    Finally {
        destination: Register,
        body: Region<EC>,
        finally: Region<EC>,
    },
    Not {
        destination: Register,
        value: Register,
    },
    And {
        destination: Register,
        lhs: Register,
        rhs: Region<EC>,
    },
    Or {
        destination: Register,
        lhs: Register,
        rhs: Region<EC>,
    },
    Closure {
        destination: Register,
        parameter: Register,
        return_type: Register,
        body: Region<EC>,
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
pub enum Builtin<EC: ExprContext + ?Sized> {
    IntNegate {
        integer_type: IntegerType,
        value: Register,
    },
    IntBitNot {
        integer_type: IntegerType,
        value: Register,
    },
    IntConvert {
        source_type: IntegerType,
        destination_type: IntegerType,
        value: Register,
    },
    IntAdd {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntSub {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntMul {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntBitAnd {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntBitOr {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntBitXor {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntBitShiftLeft {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntBitShiftRight {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntEq {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntLt {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntLe {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntGt {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    IntGe {
        integer_type: IntegerType,
        lhs: Register,
        rhs: Register,
    },
    StringConcat {
        values: Vec<Register>,
    },
    StringEq {
        lhs: Register,
        rhs: Register,
    },
    BoolEq {
        lhs: Register,
        rhs: Register,
    },
    ArrayCreateUnsafeUninitialized {
        element_type: Register,
        length: Register,
    },
    ArrayLength {
        element_type: Register,
        array: Register,
    },
    ArrayGet {
        element_type: Register,
        array: Register,
        index: Register,
    },
    ArraySet {
        element_type: Register,
        array: Register,
        index: Register,
        value: Register,
    },
    EqualToRefl {
        r#type: Register,
        value: Register,
    },
    UnsafeAssumeErased {
        r#type: Register,
    },
    Is {
        value: Register,
        pattern: LocatedPattern<EC>,
    },
}
