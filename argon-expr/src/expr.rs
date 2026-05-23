use argon_parser::ast::FunctionParameterListType;
use argon_parser::ast::{Identifier, NewTraitObjectBodyStmt, Pattern};
use nonempty_collections::NEVec;
use num_bigint::BigInt;
use std::convert::Infallible;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::str::FromStr;
use std::sync::Arc;

pub trait ExprContext {
    type Hole: Clone + Debug + Eq + Hash;
    type Function: Clone + Debug + Eq + Hash;
    type Record: Clone + Debug + Eq + Hash;
    type Enum: Clone + Debug + Eq + Hash;
    type Trait: Clone + Debug + Eq + Hash;
    type EnumVariant: Clone + Debug + Eq + Hash;
    type Method: Clone + Debug + Eq + Hash;
    type Instance: Clone + Debug + Eq + Hash;
}

#[derive(Debug)]
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

pub trait ExprContextShifter {
    type EC1: ExprContext + ?Sized;
    type EC2: ExprContext<
            Function = <Self::EC1 as ExprContext>::Function,
            Record = <Self::EC1 as ExprContext>::Record,
            Enum = <Self::EC1 as ExprContext>::Enum,
            Trait = <Self::EC1 as ExprContext>::Trait,
            EnumVariant = <Self::EC1 as ExprContext>::EnumVariant,
            Method = <Self::EC1 as ExprContext>::Method,
            Instance = <Self::EC1 as ExprContext>::Instance,
        > + ?Sized;

    fn shift_hole(&self, hole: <Self::EC1 as ExprContext>::Hole) -> Expr<Self::EC2>;

    fn shift(&self, expr: Expr<Self::EC1>) -> Expr<Self::EC2> {
        match expr {
            Expr::Error => Expr::Error,
            Expr::Hole(hole) => self.shift_hole(hole),
            Expr::As { value, value_type } => Expr::As {
                value: Box::new(self.shift(*value)),
                value_type: Box::new(self.shift(*value_type)),
            },
            Expr::Assert { t } => Expr::Assert {
                t: Box::new(self.shift(*t)),
            },
            Expr::Ensures {
                block_body,
                ensures_body,
            } => Expr::Ensures {
                block_body: Box::new(self.shift(*block_body)),
                ensures_body: ensures_body.map(|body| Box::new(self.shift(*body))),
            },
            Expr::BoolLiteral(value) => Expr::BoolLiteral(value),
            Expr::Break { label } => Expr::Break { label },
            Expr::Builtin { builtin, arguments } => Expr::Builtin {
                builtin,
                arguments: arguments
                    .into_iter()
                    .map(|argument| self.shift(argument))
                    .collect(),
            },
            Expr::Dot { o, member } => Expr::Dot {
                o: Box::new(self.shift(*o)),
                member,
            },
            Expr::EnumType(enum_ec, arguments) => Expr::EnumType(
                enum_ec,
                arguments
                    .into_iter()
                    .map(|argument| self.shift(argument))
                    .collect(),
            ),
            Expr::FunctionLiteral {
                parameter_name,
                body,
            } => Expr::FunctionLiteral {
                parameter_name,
                body: Box::new(self.shift(*body)),
            },
            Expr::FunctionCall {
                function,
                arguments,
            } => Expr::FunctionCall {
                function,
                arguments: arguments
                    .into_iter()
                    .map(|argument| FunctionArgument {
                        list_type: argument.list_type,
                        arg: self.shift(argument.arg),
                    })
                    .collect(),
            },
            Expr::FunctionObjectCall { function, argument } => Expr::FunctionObjectCall {
                function: Box::new(self.shift(*function)),
                argument: Box::new(self.shift(*argument)),
            },
            Expr::FunctionResultValue => Expr::FunctionResultValue,
            Expr::FunctionType { a, r } => Expr::FunctionType {
                a: Box::new(self.shift(*a)),
                r: Box::new(self.shift(*r)),
            },
            Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => Expr::IfElse {
                condition: Box::new(self.shift(*condition)),
                when_true: Box::new(self.shift(*when_true)),
                when_false: Box::new(self.shift(*when_false)),
            },
            Expr::IntLiteral(value) => Expr::IntLiteral(value),
            Expr::Is { value, pattern } => Expr::Is {
                value: Box::new(self.shift(*value)),
                pattern,
            },
            Expr::Loop { label, body } => Expr::Loop {
                label,
                body: Box::new(self.shift(*body)),
            },
            Expr::Match { value, cases } => Expr::Match {
                value: Box::new(self.shift(*value)),
                cases: cases
                    .into_iter()
                    .map(|case| MatchCase {
                        pattern: case.pattern,
                        body: self.shift(case.body),
                    })
                    .collect(),
            },
            Expr::MethodCall {
                method,
                receiver,
                arguments,
            } => Expr::MethodCall {
                method,
                receiver: Box::new(self.shift(*receiver)),
                arguments: arguments
                    .into_iter()
                    .map(|argument| FunctionArgument {
                        list_type: argument.list_type,
                        arg: self.shift(argument.arg),
                    })
                    .collect(),
            },
            Expr::NewTraitObject { trait_ec, body } => Expr::NewTraitObject { trait_ec, body },
            Expr::Next { label } => Expr::Next { label },
            Expr::Raise { ex } => Expr::Raise {
                ex: Box::new(self.shift(*ex)),
            },
            Expr::RecordLiteral { record, fields } => Expr::RecordLiteral {
                record,
                fields: fields
                    .into_iter()
                    .map(|field| RecordFieldLiteral {
                        name: field.name,
                        value: self.shift(field.value),
                    })
                    .collect(),
            },
            Expr::RecordType(record, arguments) => Expr::RecordType(
                record,
                arguments
                    .into_iter()
                    .map(|argument| self.shift(argument))
                    .collect(),
            ),
            Expr::Redo { label } => Expr::Redo { label },
            Expr::Sequence(exprs) => Expr::Sequence(
                NEVec::try_from_vec(exprs.into_iter().map(|expr| self.shift(expr)).collect())
                    .expect("shifting a non-empty expression sequence preserves non-emptiness"),
            ),
            Expr::StoreVariable(variable) => Expr::StoreVariable(self.shift_variable(variable)),
            Expr::StringLiteral(value) => Expr::StringLiteral(value),
            Expr::TraitType(trait_ec, arguments) => Expr::TraitType(
                trait_ec,
                arguments
                    .into_iter()
                    .map(|argument| self.shift(argument))
                    .collect(),
            ),
            Expr::Tuple { items } => Expr::Tuple {
                items: items.into_iter().map(|item| self.shift(item)).collect(),
            },
            Expr::TupleElement(value, index) => Expr::TupleElement(Box::new(self.shift(*value)), index),
            Expr::AnyType => Expr::AnyType,
            Expr::Type(t) => Expr::Type(Box::new(self.shift(*t))),
            Expr::BigType(value) => Expr::BigType(value),
            Expr::Variable(variable) => Expr::Variable(self.shift_variable(variable)),
            Expr::VariableStore(variable, value) => {
                Expr::VariableStore(self.shift_variable(variable), Box::new(self.shift(*value)))
            }
            Expr::While {
                label,
                condition,
                body,
            } => Expr::While {
                label,
                condition: Box::new(self.shift(*condition)),
                body: Box::new(self.shift(*body)),
            },
            Expr::BoxedType { t } => Expr::BoxedType {
                t: Box::new(self.shift(*t)),
            },
            Expr::Box { value } => Expr::Box {
                value: Box::new(self.shift(*value)),
            },
            Expr::Unbox { value } => Expr::Unbox {
                value: Box::new(self.shift(*value)),
            },
        }
    }

    fn shift_variable(&self, v: Variable<Self::EC1>) -> Variable<Self::EC2> {
        match v {
            Variable::Local(variable) => Variable::Local(Arc::new(LocalVariable {
                name: variable.name.clone(),
                var_type: self.shift(variable.var_type.clone()),
                erasure_mode: variable.erasure_mode,
                is_witness: variable.is_witness,
                is_mutable: variable.is_mutable,
            })),
            Variable::Parameter(variable) => Variable::Parameter(Arc::new(ParameterVariable {
                owner: match &variable.owner {
                    ExpressionOwner::Function(function) => {
                        ExpressionOwner::Function(function.clone())
                    }
                    ExpressionOwner::Record(record) => ExpressionOwner::Record(record.clone()),
                    ExpressionOwner::Enum(enum_ec) => ExpressionOwner::Enum(enum_ec.clone()),
                    ExpressionOwner::Trait(trait_ec) => ExpressionOwner::Trait(trait_ec.clone()),
                    ExpressionOwner::EnumVariant(enum_variant) => {
                        ExpressionOwner::EnumVariant(enum_variant.clone())
                    }
                    ExpressionOwner::Method(method) => ExpressionOwner::Method(method.clone()),
                    ExpressionOwner::Instance(instance) => {
                        ExpressionOwner::Instance(instance.clone())
                    }
                },
                parameter_index: variable.parameter_index,
                var_type: self.shift(variable.var_type.clone()),
                name: variable.name.clone(),
                erasure_mode: variable.erasure_mode,
                is_witness: variable.is_witness,
            })),
        }
    }
}

#[derive(Debug)]
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
        member: Identifier,
    },
    EnumType(EC::Enum, Vec<Expr<EC>>),
    Ensures {
        block_body: Box<Expr<EC>>,
        ensures_body: Option<Box<Expr<EC>>>,
    },
    FunctionLiteral {
        parameter_name: Option<Identifier>,
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
    RecordType(EC::Record, Vec<Expr<EC>>),
    Redo {
        label: Option<Infallible>, // TODO: Use a proper label type
    },
    Sequence(NEVec<Expr<EC>>),
    StoreVariable(Variable<EC>),
    StringLiteral(Box<str>),
    TraitType(EC::Trait, Vec<Expr<EC>>),
    Tuple {
        items: Vec<Expr<EC>>,
    },
    TupleElement(Box<Expr<EC>>, usize),
    AnyType,
    Type(Box<Expr<EC>>),
    BigType(BigInt),
    Variable(Variable<EC>),
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
        value: Box<Expr<EC>>,
    },
    Unbox {
        value: Box<Expr<EC>>,
    },
}

impl<EC: ExprContext + ?Sized> Clone for Expr<EC> {
    fn clone(&self) -> Self {
        match self {
            Expr::Error => Expr::Error,
            Expr::Hole(hole) => Expr::Hole(hole.clone()),
            Expr::As { value, value_type } => Expr::As {
                value: value.clone(),
                value_type: value_type.clone(),
            },
            Expr::Assert { t } => Expr::Assert { t: t.clone() },
            Expr::Ensures {
                block_body,
                ensures_body,
            } => Expr::Ensures {
                block_body: block_body.clone(),
                ensures_body: ensures_body.clone(),
            },
            Expr::BoolLiteral(value) => Expr::BoolLiteral(*value),
            Expr::Break { label } => Expr::Break {
                label: label.clone(),
            },
            Expr::Builtin { builtin, arguments } => Expr::Builtin {
                builtin: *builtin,
                arguments: arguments.clone(),
            },
            Expr::Dot { o, member } => Expr::Dot {
                o: o.clone(),
                member: member.clone(),
            },
            Expr::EnumType(enum_ec, arguments) => {
                Expr::EnumType(enum_ec.clone(), arguments.clone())
            }
            Expr::FunctionLiteral {
                parameter_name,
                body,
            } => Expr::FunctionLiteral {
                parameter_name: parameter_name.clone(),
                body: body.clone(),
            },
            Expr::FunctionCall {
                function,
                arguments,
            } => Expr::FunctionCall {
                function: function.clone(),
                arguments: arguments.clone(),
            },
            Expr::FunctionObjectCall { function, argument } => Expr::FunctionObjectCall {
                function: function.clone(),
                argument: argument.clone(),
            },
            Expr::FunctionResultValue => Expr::FunctionResultValue,
            Expr::FunctionType { a, r } => Expr::FunctionType {
                a: a.clone(),
                r: r.clone(),
            },
            Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => Expr::IfElse {
                condition: condition.clone(),
                when_true: when_true.clone(),
                when_false: when_false.clone(),
            },
            Expr::IntLiteral(value) => Expr::IntLiteral(value.clone()),
            Expr::Is { value, pattern } => Expr::Is {
                value: value.clone(),
                pattern: pattern.clone(),
            },
            Expr::Loop { label, body } => Expr::Loop {
                label: label.clone(),
                body: body.clone(),
            },
            Expr::Match { value, cases } => Expr::Match {
                value: value.clone(),
                cases: cases.clone(),
            },
            Expr::MethodCall {
                method,
                receiver,
                arguments,
            } => Expr::MethodCall {
                method: method.clone(),
                receiver: receiver.clone(),
                arguments: arguments.clone(),
            },
            Expr::NewTraitObject { trait_ec, body } => Expr::NewTraitObject {
                trait_ec: trait_ec.clone(),
                body: body.clone(),
            },
            Expr::Next { label } => Expr::Next {
                label: label.clone(),
            },
            Expr::Raise { ex } => Expr::Raise { ex: ex.clone() },
            Expr::RecordLiteral { record, fields } => Expr::RecordLiteral {
                record: record.clone(),
                fields: fields.clone(),
            },
            Expr::RecordType(record, arguments) => {
                Expr::RecordType(record.clone(), arguments.clone())
            }
            Expr::Redo { label } => Expr::Redo {
                label: label.clone(),
            },
            Expr::Sequence(exprs) => Expr::Sequence(exprs.clone()),
            Expr::StoreVariable(variable) => Expr::StoreVariable(variable.clone()),
            Expr::StringLiteral(value) => Expr::StringLiteral(value.clone()),
            Expr::TraitType(trait_ec, arguments) => {
                Expr::TraitType(trait_ec.clone(), arguments.clone())
            }
            Expr::Tuple { items } => Expr::Tuple {
                items: items.clone(),
            },
            Expr::TupleElement(value, index) => Expr::TupleElement(value.clone(), *index),
            Expr::AnyType => Expr::AnyType,
            Expr::Type(t) => Expr::Type(t.clone()),
            Expr::BigType(value) => Expr::BigType(value.clone()),
            Expr::Variable(variable) => Expr::Variable(variable.clone()),
            Expr::VariableStore(variable, value) => {
                Expr::VariableStore(variable.clone(), value.clone())
            }
            Expr::While {
                label,
                condition,
                body,
            } => Expr::While {
                label: label.clone(),
                condition: condition.clone(),
                body: body.clone(),
            },
            Expr::BoxedType { t } => Expr::BoxedType { t: t.clone() },
            Expr::Box { value } => Expr::Box {
                value: value.clone(),
            },
            Expr::Unbox { value } => Expr::Unbox {
                value: value.clone(),
            },
        }
    }
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

#[derive(Debug)]
pub struct FunctionArgument<EC: ExprContext + ?Sized> {
    pub list_type: FunctionParameterListType,
    pub arg: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for FunctionArgument<EC> {
    fn clone(&self) -> Self {
        Self {
            list_type: self.list_type,
            arg: self.arg.clone(),
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Variable<EC: ExprContext + ?Sized> {
    Local(Arc<LocalVariable<EC>>),
    Parameter(Arc<ParameterVariable<EC>>),
}

impl<EC: ExprContext + ?Sized> Variable<EC> {
    pub fn name(&self) -> Option<&Identifier> {
        match self {
            Variable::Local(variable) => Some(&variable.name),
            Variable::Parameter(variable) => variable.name.as_ref(),
        }
    }

    pub fn var_type(&self) -> Expr<EC> {
        match self {
            Variable::Local(variable) => variable.var_type.clone(),
            Variable::Parameter(variable) => variable.var_type.clone(),
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Variable::Local(variable) => variable.is_mutable,
            Variable::Parameter(_) => false,
        }
    }
}

impl<EC: ExprContext + ?Sized> Clone for Variable<EC> {
    fn clone(&self) -> Self {
        match self {
            Variable::Local(variable) => Variable::Local(variable.clone()),
            Variable::Parameter(variable) => Variable::Parameter(variable.clone()),
        }
    }
}

impl<EC: ExprContext + ?Sized> PartialEq for Variable<EC> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Variable::Local(a), Variable::Local(b)) => Arc::ptr_eq(a, b),
            (Variable::Local(_), _) | (_, Variable::Local(_)) => false,

            (Variable::Parameter(a), Variable::Parameter(b)) => Arc::ptr_eq(a, b),
        }
    }
}

impl<EC: ExprContext + ?Sized> Eq for Variable<EC> {}

impl<EC: ExprContext + ?Sized> Hash for Variable<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (std::ptr::from_ref(self) as usize).hash(state);
    }
}

pub struct VariableTupleElement<EC: ExprContext + ?Sized> {
    pub variable: Variable<EC>,
    pub index: usize,
    pub binding_type: Expr<EC>,
}

impl <EC: ExprContext + ?Sized> Clone for VariableTupleElement<EC> {
    fn clone(&self) -> Self {
        Self {
            variable: self.variable.clone(),
            index: self.index,
            binding_type: self.binding_type.clone(),
        }
    }
}


#[derive(Debug)]
pub struct LocalVariable<EC: ExprContext + ?Sized> {
    pub name: Identifier,
    pub var_type: Expr<EC>,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
    pub is_mutable: bool,
}

#[derive(Debug)]
pub struct ParameterVariable<EC: ExprContext + ?Sized> {
    pub owner: ExpressionOwner<EC>,
    pub parameter_index: usize,
    pub var_type: Expr<EC>,
    pub name: Option<Identifier>,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
}
