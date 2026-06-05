use alloc::{boxed::Box, sync::Arc, vec, vec::Vec};
use argon_parser::ast::{Identifier, NewTraitObjectBodyStmt, Pattern};
use argon_util::UniqueIdentifier;
use core::convert::Infallible;
use core::fmt::Debug;
use core::hash::{Hash, Hasher};
use core::str::FromStr;
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
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub enum Expr<EC: ExprContext + ?Sized> {
    Error,
    Hole(EC::Hole),
    And(Box<Expr<EC>>, Box<Expr<EC>>),
    BoolLiteral(bool),
    Break {
        label: Option<Infallible>, // TODO: Use a proper label type
    },
    Builtin {
        builtin: Builtin,
        arguments: Vec<Expr<EC>>,
    },
    EnumType(EC::Enum, Vec<Expr<EC>>),
    Finally {
        block_body: Box<Expr<EC>>,
        finally_body: Box<Expr<EC>>,
    },
    FunctionLiteral {
        parameter_name: Option<Identifier>,
        body: Box<Expr<EC>>,
    },
    FunctionCall {
        function: EC::Function,
        arguments: Vec<Expr<EC>>,
    },
    FunctionObjectCall {
        function: Box<Expr<EC>>,
        argument: Box<Expr<EC>>,
    },
    FunctionResultValue,
    FunctionType {
        a: Box<Expr<EC>>,
        r: Box<Expr<EC>>,
    },
    IfElse {
        when_true_var: Option<Variable<EC>>,
        when_false_var: Option<Variable<EC>>,
        condition: Box<Expr<EC>>,
        when_true: Box<Expr<EC>>,
        when_false: Box<Expr<EC>>,
    },
    IntLiteral(BigInt),
    Is {
        value: Box<Expr<EC>>,
        pattern: Pattern,
    },
    Loop {
        label: Option<Infallible>, // TODO: Use a proper label type
        body: Box<Expr<EC>>,
    },
    Match {
        value: Box<Expr<EC>>,
        cases: Vec<MatchCase<EC>>,
    },
    MethodCall {
        method: EC::Method,
        receiver: Box<Expr<EC>>,
        arguments: Vec<Expr<EC>>,
    },
    NewTraitObject {
        trait_ec: EC::Trait,
        body: Vec<NewTraitObjectBodyStmt>,
    },
    Next {
        label: Option<Infallible>, // TODO: Use a proper label type
    },
    Or(Box<Expr<EC>>, Box<Expr<EC>>),
    Raise {
        ex: Box<Expr<EC>>,
    },
    RecordFieldLoad {
        record_type: Box<Expr<EC>>,
        field: EC::RecordField,
        record_value: Box<Expr<EC>>,
    },
    RecordFieldStore {
        record_type: Box<Expr<EC>>,
        field: EC::RecordField,
        record_value: Box<Expr<EC>>,
        new_value: Box<Expr<EC>>,
    },
    RecordLiteral {
        record_type: RecordType<EC>,
        fields: Vec<RecordFieldLiteral<EC>>,
    },
    RecordType(RecordType<EC>),
    Redo {
        label: Option<Infallible>, // TODO: Use a proper label type
    },
    Sequence(Vec1<Expr<EC>>),
    StoreVariable(Variable<EC>),
    StringLiteral(Box<str>),
    TraitType(EC::Trait, Vec<Expr<EC>>),
    Tuple {
        items: Vec<Expr<EC>>,
    },
    TupleElement(Box<Expr<EC>>, usize),
    Type(Box<Expr<EC>>),
    BigType(BigInt),
    Variable(Variable<EC>),
    VariableBinding(Variable<EC>, Box<Expr<EC>>),
    VariableStore(Variable<EC>, Box<Expr<EC>>),
    While {
        label: Option<Identifier>,
        condition: Box<Expr<EC>>,
        body: Box<Expr<EC>>,
    },
    BoxedType {
        t: Box<Expr<EC>>,
    },
    Box {
        t: Box<Expr<EC>>,
        value: Box<Expr<EC>>,
    },
    Unbox {
        t: Box<Expr<EC>>,
        value: Box<Expr<EC>>,
    },
}

impl<EC: ExprContext + ?Sized> Expr<EC> {
    pub fn bool_type() -> Expr<EC> {
        Expr::Builtin {
            builtin: Builtin::BoolType,
            arguments: vec![],
        }
    }

    pub fn int_type() -> Expr<EC> {
        Expr::Builtin {
            builtin: Builtin::IntType,
            arguments: vec![],
        }
    }

    pub fn string_type() -> Expr<EC> {
        Expr::Builtin {
            builtin: Builtin::StringType,
            arguments: vec![],
        }
    }

    pub fn never_type() -> Expr<EC> {
        Expr::Builtin {
            builtin: Builtin::NeverType,
            arguments: vec![],
        }
    }

    pub fn array_type(element_type: Expr<EC>) -> Expr<EC> {
        Expr::Builtin {
            builtin: Builtin::ArrayType,
            arguments: vec![element_type],
        }
    }

    pub fn unit_type() -> Expr<EC> {
        Expr::Tuple { items: vec![] }
    }

    pub fn type_n(level: impl Into<BigInt>) -> Expr<EC> {
        Expr::Type(Box::new(Expr::IntLiteral(level.into())))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    IntType,
    BoolType,
    StringType,
    NeverType,
    ArrayType,
    IntNegate,
    IntBitNot,
    IntAdd,
    IntSub,
    IntMul,
    IntBitAnd,
    IntBitOr,
    IntBitXor,
    IntBitShiftLeft,
    IntBitShiftRight,
    IntEq,
    IntNe,
    IntLt,
    IntLe,
    IntGt,
    IntGe,
    StringConcat,
    StringEq,
    StringNe,
    BoolNot,
    BoolEq,
    BoolNe,
    ArrayCreateUnsafeUninitialized,
    ArrayLength,
    ArrayGet,
    ArraySet,
    ConjunctionType,
    DisjunctionType,
    EqualToType,
}

impl Builtin {
    pub fn as_str(self) -> &'static str {
        match self {
            Builtin::IntType => "int_type",
            Builtin::BoolType => "bool_type",
            Builtin::StringType => "string_type",
            Builtin::NeverType => "never_type",
            Builtin::ArrayType => "array_type",
            Builtin::IntNegate => "int_negate",
            Builtin::IntBitNot => "int_bitnot",
            Builtin::IntAdd => "int_add",
            Builtin::IntSub => "int_sub",
            Builtin::IntMul => "int_mul",
            Builtin::IntBitAnd => "int_bitand",
            Builtin::IntBitOr => "int_bitor",
            Builtin::IntBitXor => "int_bitxor",
            Builtin::IntBitShiftLeft => "int_bitshiftleft",
            Builtin::IntBitShiftRight => "int_bitshiftright",
            Builtin::IntEq => "int_eq",
            Builtin::IntNe => "int_ne",
            Builtin::IntLt => "int_lt",
            Builtin::IntLe => "int_le",
            Builtin::IntGt => "int_gt",
            Builtin::IntGe => "int_ge",
            Builtin::StringConcat => "string_concat",
            Builtin::StringEq => "string_eq",
            Builtin::StringNe => "string_ne",
            Builtin::BoolNot => "bool_not",
            Builtin::BoolEq => "bool_eq",
            Builtin::BoolNe => "bool_ne",
            Builtin::ArrayCreateUnsafeUninitialized => "array_create_unsafe_uninitialized",
            Builtin::ArrayLength => "array_length",
            Builtin::ArrayGet => "array_get",
            Builtin::ArraySet => "array_set",
            Builtin::ConjunctionType => "conjunction_type",
            Builtin::DisjunctionType => "disjunction_type",
            Builtin::EqualToType => "equal_to_type",
        }
    }
}

impl FromStr for Builtin {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "int_type" => Ok(Builtin::IntType),
            "bool_type" => Ok(Builtin::BoolType),
            "string_type" => Ok(Builtin::StringType),
            "never_type" => Ok(Builtin::NeverType),
            "array_type" => Ok(Builtin::ArrayType),
            "int_negate" => Ok(Builtin::IntNegate),
            "int_bitnot" => Ok(Builtin::IntBitNot),
            "int_add" => Ok(Builtin::IntAdd),
            "int_sub" => Ok(Builtin::IntSub),
            "int_mul" => Ok(Builtin::IntMul),
            "int_bitand" => Ok(Builtin::IntBitAnd),
            "int_bitor" => Ok(Builtin::IntBitOr),
            "int_bitxor" => Ok(Builtin::IntBitXor),
            "int_bitshiftleft" => Ok(Builtin::IntBitShiftLeft),
            "int_bitshiftright" => Ok(Builtin::IntBitShiftRight),
            "int_eq" => Ok(Builtin::IntEq),
            "int_ne" => Ok(Builtin::IntNe),
            "int_lt" => Ok(Builtin::IntLt),
            "int_le" => Ok(Builtin::IntLe),
            "int_gt" => Ok(Builtin::IntGt),
            "int_ge" => Ok(Builtin::IntGe),
            "string_concat" => Ok(Builtin::StringConcat),
            "string_eq" => Ok(Builtin::StringEq),
            "string_ne" => Ok(Builtin::StringNe),
            "bool_not" => Ok(Builtin::BoolNot),
            "bool_eq" => Ok(Builtin::BoolEq),
            "bool_ne" => Ok(Builtin::BoolNe),
            "array_create_unsafe_uninitialized" => Ok(Builtin::ArrayCreateUnsafeUninitialized),
            "array_length" => Ok(Builtin::ArrayLength),
            "array_get" => Ok(Builtin::ArrayGet),
            "array_set" => Ok(Builtin::ArraySet),
            "conjunction_type" => Ok(Builtin::ConjunctionType),
            "disjunction_type" => Ok(Builtin::DisjunctionType),
            "equal_to_type" => Ok(Builtin::EqualToType),
            _ => Err(()),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct MatchCase<EC: ExprContext + ?Sized> {
    pub pattern: Pattern,
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
pub struct RecordFieldLiteral<EC: ExprContext + ?Sized> {
    pub name: Identifier,
    pub value: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for RecordFieldLiteral<EC> {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
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
pub enum Variable<EC: ExprContext + ?Sized> {
    Local(Box<LocalVariable<EC>>),
    Parameter(Box<ParameterVariable<EC>>),
}

impl<EC: ExprContext + ?Sized> Variable<EC> {
    pub fn name(&self) -> Option<&Identifier> {
        match self {
            Variable::Local(variable) => variable.name.as_ref(),
            Variable::Parameter(variable) => variable.name.as_ref(),
        }
    }

    pub fn var_type(&self) -> &Expr<EC> {
        match self {
            Variable::Local(variable) => &variable.var_type,
            Variable::Parameter(variable) => &variable.var_type,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Variable::Local(variable) => variable.is_mutable,
            Variable::Parameter(_) => false,
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
