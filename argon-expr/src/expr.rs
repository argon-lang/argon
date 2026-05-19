use argon_parser::ast::FunctionParameterListType;
use argon_parser::ast::{IdentifierExpr, NewTraitObjectBodyStmt, Pattern};
use nonempty_collections::NEVec;
use num_bigint::BigInt;
use std::convert::Infallible;
use std::fmt::{Arguments, Debug};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::str::FromStr;

pub trait ExprContext {
    type Hole: Clone + Debug + Eq + Hash;
    type Function: Clone + Debug + Eq + Hash;
    type Method: Clone + Debug + Eq + Hash;
    type Record: Clone + Debug + Eq + Hash;
    type Enum: Clone + Debug + Eq + Hash;
    type EnumCase: Clone + Debug + Eq + Hash;
    type Trait: Clone + Debug + Eq + Hash;
}

#[derive(Debug, Clone)]
pub enum Expr<EC: ExprContext + ?Sized> {
    Error,
    Hole(EC::Hole),
    As {
        value: Box<Expr<EC>>,
        value_type: Box<Expr<EC>>,
    },
    Assert {
        t: Box<Expr<EC>>,
    },
    Block {
        body: Box<Expr<EC>>,
        finally_body: Option<Box<Expr<EC>>>,
    },
    BoolLiteral(bool),
    Break {
        label: Option<Infallible>, // TODO: Use a proper label type
    },
    Builtin {
        builtin: Builtin,
        arguments: Vec<Expr<EC>>,
    },
    Dot {
        o: Box<Expr<EC>>,
        member: IdentifierExpr,
    },
    FunctionLiteral {
        parameter_name: Option<IdentifierExpr>,
        body: Box<Expr<EC>>,
    },
    FunctionCall {
        function: EC::Function,
        arguments: Vec<FunctionArgument<EC>>,
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
        arguments: Vec<FunctionArgument<EC>>,
    },
    NewTraitObject {
        trait_ec: EC::Trait,
        body: Vec<NewTraitObjectBodyStmt>,
    },
    Next {
        label: Option<Infallible>, // TODO: Use a proper label type
    },
    Raise {
        ex: Box<Expr<EC>>,
    },
    RecordLiteral {
        record: EC::Record,
        fields: Vec<RecordFieldLiteral<EC>>,
    },
    Redo {
        label: Option<Infallible>, // TODO: Use a proper label type
    },
    Sequence(NEVec<Expr<EC>>),
    StoreVariable(Variable),
    StringLiteral(Box<str>),
    Tuple {
        items: Vec<Expr<EC>>,
    },
    AnyType,
    Type(Box<Expr<EC>>),
    BigType(BigInt),
    Variable(Variable),
    While {
        label: Option<IdentifierExpr>,
        condition: Box<Expr<EC>>,
        body: Box<Expr<EC>>,
    },
    BoxedType {
        t: Box<Expr<EC>>,
    },
    Box {
        value: Box<Expr<EC>>,
    },
    Unbox {
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

#[derive(Debug, Clone)]
pub struct FunctionArgument<EC: ExprContext + ?Sized> {
    pub list_type: FunctionParameterListType,
    pub arg: Expr<EC>,
}

#[derive(Debug, Clone)]
pub struct MatchCase<EC: ExprContext + ?Sized> {
    pub pattern: Pattern,
    pub body: Expr<EC>,
}

#[derive(Debug, Clone)]
pub struct RecordFieldLiteral<EC: ExprContext + ?Sized> {
    pub name: IdentifierExpr,
    pub value: Expr<EC>,
}

#[derive(Debug, Clone)]
pub enum Variable {
    Local(Rc<LocalVariable>),
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Variable::Local(a), Variable::Local(b)) => Rc::ptr_eq(a, b),
        }
    }
}

impl Eq for Variable {}

impl Hash for Variable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (std::ptr::from_ref(self) as usize).hash(state);
    }
}

#[derive(Debug)]
pub struct LocalVariable {}
