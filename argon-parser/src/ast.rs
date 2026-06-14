use crate::Location;
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt::{Display, Formatter};
use num_bigint::{BigInt, BigUint};
use parse18_runtime::WithLocation;

#[derive(Debug, Clone)]
pub struct NonEmptyVec<T> {
    pub head: T,
    pub tail: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    pub fn new(head: T, tail: Vec<T>) -> Self {
        Self { head, tail }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Error,
    FunctionDeclaration(Box<FunctionDeclarationStmt>),
    VariableDeclaration(Box<VariableDeclarationStmt>),
    RecordDeclaration(Box<RecordDeclarationStmt>),
    EnumDeclaration(Box<EnumDeclarationStmt>),
    TraitDeclaration(Box<TraitDeclarationStmt>),
    MethodDeclaration(Box<MethodDeclarationStmt>),
    InstanceDeclaration(Box<InstanceDeclarationStmt>),
    Import(Box<ImportStmt>),
    Export(Box<ExportStmt>),
    Assert(Box<AssertStmt>),
    Expr(WithLocation<Expr>),
}

#[derive(Debug, Clone)]
pub enum RecordBodyStmt {
    FunctionDeclaration(Box<FunctionDeclarationStmt>),
    RecordField(Box<RecordField>),
    MethodDeclaration(Box<MethodDeclarationStmt>),
}

#[derive(Debug, Clone)]
pub enum EnumBodyStmt {
    FunctionDeclaration(Box<FunctionDeclarationStmt>),
    MethodDeclaration(Box<MethodDeclarationStmt>),
    EnumVariant(Box<EnumVariant>),
}

#[derive(Debug, Clone)]
pub enum TraitBodyStmt {
    FunctionDeclaration(Box<FunctionDeclarationStmt>),
    MethodDeclaration(Box<MethodDeclarationStmt>),
}

#[derive(Debug, Clone)]
pub enum NewTraitObjectBodyStmt {
    FunctionDeclaration(Box<FunctionDeclarationStmt>),
    MethodDeclaration(Box<MethodDeclarationStmt>),
}

#[derive(Debug, Clone)]
pub enum DeclarationStmt {
    Function(Box<FunctionDeclarationStmt>),
    Record(Box<RecordDeclarationStmt>),
    Enum(Box<EnumDeclarationStmt>),
    Trait(Box<TraitDeclarationStmt>),
    Method(Box<MethodDeclarationStmt>),
    Instance(Box<InstanceDeclarationStmt>),
}

impl DeclarationStmt {
    pub fn modifiers(&self) -> &[WithLocation<Modifier>] {
        match self {
            DeclarationStmt::Function(x) => &x.modifiers,
            DeclarationStmt::Record(x) => &x.modifiers,
            DeclarationStmt::Enum(x) => &x.modifiers,
            DeclarationStmt::Trait(x) => &x.modifiers,
            DeclarationStmt::Method(x) => &x.modifiers,
            DeclarationStmt::Instance(x) => &x.modifiers,
        }
    }

    pub fn name(&self) -> &WithLocation<Identifier> {
        match self {
            DeclarationStmt::Function(x) => &x.name,
            DeclarationStmt::Record(x) => &x.name,
            DeclarationStmt::Enum(x) => &x.name,
            DeclarationStmt::Trait(x) => &x.name,
            DeclarationStmt::Method(x) => &x.name,
            DeclarationStmt::Instance(x) => &x.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclarationStmt {
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub purity: bool,
    pub name: WithLocation<Identifier>,
    pub parameters: Vec<WithLocation<FunctionParameterList>>,
    pub return_type: WithLocation<ReturnTypeSpecifier>,
    pub body: FunctionBody,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationStmt {
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub is_mutable: bool,
    pub name: Option<Identifier>,
    pub var_type: Option<WithLocation<Expr>>,
    pub value: WithLocation<Expr>,
}

#[derive(Debug, Clone)]
pub struct RecordDeclarationStmt {
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub name: WithLocation<Identifier>,
    pub parameters: Vec<WithLocation<FunctionParameterList>>,
    pub return_type: Option<WithLocation<Expr>>,
    pub body: Vec<WithLocation<RecordBodyStmt>>,
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub is_mutable: bool,
    pub name: WithLocation<Identifier>,
    pub field_type: WithLocation<Expr>,
}

#[derive(Debug, Clone)]
pub struct EnumDeclarationStmt {
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub name: WithLocation<Identifier>,
    pub parameters: Vec<WithLocation<FunctionParameterList>>,
    pub return_type: Option<WithLocation<Expr>>,
    pub body: Vec<WithLocation<EnumBodyStmt>>,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Constructor {
        modifiers: Vec<WithLocation<Modifier>>,
        name: WithLocation<Identifier>,
        parameters: Vec<WithLocation<FunctionParameterList>>,
        return_type: Option<WithLocation<Expr>>,
    },
    Record(RecordDeclarationStmt),
}

#[derive(Debug, Clone)]
pub struct TraitDeclarationStmt {
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub name: WithLocation<Identifier>,
    pub parameters: Vec<WithLocation<FunctionParameterList>>,
    pub return_type: Option<WithLocation<Expr>>,
    pub body: Vec<WithLocation<TraitBodyStmt>>,
}

#[derive(Debug, Clone)]
pub struct MethodDeclarationStmt {
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub purity: bool,
    pub instance_name: WithLocation<Option<Identifier>>,
    pub instance_type: Option<WithLocation<Expr>>,
    pub name: WithLocation<Identifier>,
    pub parameters: Vec<WithLocation<FunctionParameterList>>,
    pub return_type: WithLocation<ReturnTypeSpecifier>,
    pub body: Option<FunctionBody>,
}

#[derive(Debug, Clone)]
pub struct InstanceDeclarationStmt {
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub name: WithLocation<Identifier>,
    pub parameters: Vec<WithLocation<FunctionParameterList>>,
    pub return_type: Option<WithLocation<Expr>>,
    pub body: Vec<WithLocation<TraitBodyStmt>>,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub param_type: WithLocation<Expr>,
    pub name: Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionParameterListType {
    NormalList,
    InferrableList(BigUint),
    RequiresList,
}

#[derive(Debug, Clone)]
pub struct FunctionParameterList {
    pub list_type: FunctionParameterListType,
    pub modifiers: Vec<WithLocation<Modifier>>,
    pub parameters: Vec<WithLocation<FunctionParameter>>,
    pub has_trailing_comma: bool,
}

#[derive(Debug, Clone)]
pub struct ReturnTypeSpecifier {
    pub return_type: WithLocation<Expr>,
    pub ensures_clauses: Vec<WithLocation<Expr>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Modifier {
    Public,
    Internal,
    Protected,
    Private,
    Erased,
    Token,
    Witness,
    Inline,
    Final,
    Override,
    Virtual,
}

impl Display for Modifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Modifier::Public => write!(f, "public"),
            Modifier::Internal => write!(f, "internal"),
            Modifier::Protected => write!(f, "protected"),
            Modifier::Private => write!(f, "private"),
            Modifier::Erased => write!(f, "erased"),
            Modifier::Token => write!(f, "token"),
            Modifier::Witness => write!(f, "witness"),
            Modifier::Inline => write!(f, "inline"),
            Modifier::Final => write!(f, "final"),
            Modifier::Override => write!(f, "override"),
            Modifier::Virtual => write!(f, "virtual"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ImportStmt {
    Absolute(ImportPathSegment),
    Relative {
        up_count: usize,
        path: ImportPathSegment,
    },
    Tube {
        tube_name: NonEmptyVec<String>,
        path: ImportPathSegment,
    },
    Member {
        member_path: ImportPathSegment,
    },
}

#[derive(Debug, Clone)]
pub enum ImportPathSegment {
    Cons {
        id: String,
        sub_path: Box<ImportPathSegment>,
    },
    Many {
        segments: Vec<ImportPathSegment>,
    },
    Renaming {
        importing: WithLocation<Identifier>,
        viewed_name: WithLocation<Option<Identifier>>,
    },
    Imported {
        id: WithLocation<Identifier>,
    },
    Wildcard {
        location: Location,
    },
}

#[derive(Debug, Clone)]
pub struct ExportStmt {
    pub from_import: WithLocation<ImportStmt>,
}

#[derive(Debug, Clone)]
pub struct AssertStmt {
    pub t: WithLocation<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Error,
    As {
        value: Box<WithLocation<Expr>>,
        value_type: Box<WithLocation<Expr>>,
    },
    Assert {
        t: Box<WithLocation<Expr>>,
    },
    BinaryOperation {
        a: Box<WithLocation<Expr>>,
        op: WithLocation<BinaryOperator>,
        b: Box<WithLocation<Expr>>,
    },
    Block {
        body: WithLocation<Vec<WithLocation<Stmt>>>,
        finally_body: Option<WithLocation<Vec<WithLocation<Stmt>>>>,
    },
    BoolLiteral(bool),
    Break {
        label: Option<WithLocation<Identifier>>,
        value: Option<Box<WithLocation<Expr>>>,
    },
    Builtin(String),
    Dot {
        o: Box<WithLocation<Expr>>,
        member: WithLocation<Identifier>,
    },
    FunctionLiteral(FunctionLiteral),
    FunctionCall {
        func: Box<WithLocation<Expr>>,
        list_type: FunctionParameterListType,
        arg: Box<WithLocation<Expr>>,
    },
    FunctionResultValue,
    FunctionType {
        a: Box<WithLocation<Expr>>,
        r: Box<WithLocation<Expr>>,
    },
    IfElse {
        condition: Box<WithLocation<Expr>>,
        when_true: WithLocation<Vec<WithLocation<Stmt>>>,
        when_false: WithLocation<Vec<WithLocation<Stmt>>>,
    },
    IntLiteral(BigInt),
    Is {
        value: Box<WithLocation<Expr>>,
        pattern: WithLocation<Pattern>,
    },
    Loop {
        label: Option<WithLocation<Identifier>>,
        body: WithLocation<Vec<WithLocation<Stmt>>>,
    },
    Match {
        value: Box<WithLocation<Expr>>,
        cases: Vec<WithLocation<MatchCase>>,
    },
    NewTraitObject {
        trait_expr: Box<WithLocation<Expr>>,
        body: Vec<WithLocation<NewTraitObjectBodyStmt>>,
    },
    Next {
        label: Option<WithLocation<Identifier>>,
    },
    Paren(Box<WithLocation<Expr>>),
    Raise {
        ex: Box<WithLocation<Expr>>,
    },
    RecordLiteral {
        record_expr: Box<WithLocation<Expr>>,
        fields: WithLocation<Vec<WithLocation<RecordFieldLiteral>>>,
    },
    Redo {
        label: Option<WithLocation<Identifier>>,
    },
    Retry {
        label: Option<WithLocation<Identifier>>,
    },
    StringLiteral(StringLiteral),
    Summon {
        t: Box<WithLocation<Expr>>,
    },
    Tuple {
        items: Vec<WithLocation<Expr>>,
    },
    Type,
    BigType(BigInt),
    UnaryOperation {
        op: WithLocation<UnaryOperator>,
        a: Box<WithLocation<Expr>>,
    },
    While {
        label: Option<WithLocation<Identifier>>,
        condition: WithLocation<Vec<WithLocation<Stmt>>>,
        body: WithLocation<Vec<WithLocation<Stmt>>>,
    },
    BoxedType {
        t: Box<WithLocation<Expr>>,
    },
    Box {
        value: Box<WithLocation<Expr>>,
    },
    Unbox {
        value: Box<WithLocation<Expr>>,
    },
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Identifier {
    Named(String),
    BinaryOp(BinaryOperatorIdentifier),
    UnaryOp(UnaryOperatorIdentifier),
    Index,
    Extension(Box<Identifier>),
    Inverse(Box<Identifier>),
    Update(Box<Identifier>),
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Identifier::Named(name) => write!(f, "{}", name),
            Identifier::BinaryOp(op) => write!(f, "binary operator {}", op.symbol()),
            Identifier::UnaryOp(op) => write!(f, "unary operator {}", op.symbol()),
            Identifier::Index => write!(f, "index"),
            Identifier::Extension(inner) => write!(f, "extension {}", inner),
            Identifier::Inverse(inner) => write!(f, "inverse {}", inner),
            Identifier::Update(inner) => write!(f, "update {}", inner),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    ExprBody(Box<WithLocation<Expr>>),
    ExternBody(WithLocation<String>),
}

#[derive(Debug, Clone)]
pub struct RecordFieldLiteral {
    pub name: WithLocation<Identifier>,
    pub value: WithLocation<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub parameter_name: Option<Identifier>,
    pub body: Box<WithLocation<Expr>>,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub parts: Vec<StringFragment>,
}

#[derive(Debug, Clone)]
pub enum StringFragment {
    Text(String),
    Interpolate { value: Box<WithLocation<Expr>> },
}

pub trait Operator: Copy {
    fn symbol(self) -> &'static str;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperatorIdentifier {
    Plus,
    Minus,
    Mul,
    Div,
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    BitOr,
    BitXOr,
    BitAnd,
    ShiftLeft,
    ShiftRight,
    Concat,
}

impl TryFrom<BinaryOperator> for BinaryOperatorIdentifier {
    type Error = ();

    fn try_from(op: BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            BinaryOperator::Plus => Ok(BinaryOperatorIdentifier::Plus),
            BinaryOperator::Minus => Ok(BinaryOperatorIdentifier::Minus),
            BinaryOperator::Mul => Ok(BinaryOperatorIdentifier::Mul),
            BinaryOperator::Div => Ok(BinaryOperatorIdentifier::Div),
            BinaryOperator::Equal => Ok(BinaryOperatorIdentifier::Equal),
            BinaryOperator::NotEqual => Ok(BinaryOperatorIdentifier::NotEqual),
            BinaryOperator::LessThan => Ok(BinaryOperatorIdentifier::LessThan),
            BinaryOperator::LessThanEq => Ok(BinaryOperatorIdentifier::LessThanEq),
            BinaryOperator::GreaterThan => Ok(BinaryOperatorIdentifier::GreaterThan),
            BinaryOperator::GreaterThanEq => Ok(BinaryOperatorIdentifier::GreaterThanEq),
            BinaryOperator::BitOr => Ok(BinaryOperatorIdentifier::BitOr),
            BinaryOperator::BitXOr => Ok(BinaryOperatorIdentifier::BitXOr),
            BinaryOperator::BitAnd => Ok(BinaryOperatorIdentifier::BitAnd),
            BinaryOperator::ShiftLeft => Ok(BinaryOperatorIdentifier::ShiftLeft),
            BinaryOperator::ShiftRight => Ok(BinaryOperatorIdentifier::ShiftRight),
            BinaryOperator::Concat => Ok(BinaryOperatorIdentifier::Concat),
            BinaryOperator::Assign
            | BinaryOperator::LogicalOr
            | BinaryOperator::LogicalAnd
            | BinaryOperator::PropEqual
            | BinaryOperator::PropDisjunction
            | BinaryOperator::PropConjunction => Err(()),
        }
    }
}

impl From<BinaryOperatorIdentifier> for BinaryOperator {
    fn from(op: BinaryOperatorIdentifier) -> Self {
        match op {
            BinaryOperatorIdentifier::Plus => BinaryOperator::Plus,
            BinaryOperatorIdentifier::Minus => BinaryOperator::Minus,
            BinaryOperatorIdentifier::Mul => BinaryOperator::Mul,
            BinaryOperatorIdentifier::Div => BinaryOperator::Div,
            BinaryOperatorIdentifier::Equal => BinaryOperator::Equal,
            BinaryOperatorIdentifier::NotEqual => BinaryOperator::NotEqual,
            BinaryOperatorIdentifier::LessThan => BinaryOperator::LessThan,
            BinaryOperatorIdentifier::LessThanEq => BinaryOperator::LessThanEq,
            BinaryOperatorIdentifier::GreaterThan => BinaryOperator::GreaterThan,
            BinaryOperatorIdentifier::GreaterThanEq => BinaryOperator::GreaterThanEq,
            BinaryOperatorIdentifier::BitOr => BinaryOperator::BitOr,
            BinaryOperatorIdentifier::BitXOr => BinaryOperator::BitXOr,
            BinaryOperatorIdentifier::BitAnd => BinaryOperator::BitAnd,
            BinaryOperatorIdentifier::ShiftLeft => BinaryOperator::ShiftLeft,
            BinaryOperatorIdentifier::ShiftRight => BinaryOperator::ShiftRight,
            BinaryOperatorIdentifier::Concat => BinaryOperator::Concat,
        }
    }
}

impl Operator for BinaryOperatorIdentifier {
    fn symbol(self) -> &'static str {
        BinaryOperator::from(self).symbol()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOperatorIdentifier {
    Plus,
    Minus,
    BitNot,
}

impl TryFrom<UnaryOperator> for UnaryOperatorIdentifier {
    type Error = ();

    fn try_from(value: UnaryOperator) -> Result<Self, Self::Error> {
        Ok(match value {
            UnaryOperator::Plus => UnaryOperatorIdentifier::Plus,
            UnaryOperator::Minus => UnaryOperatorIdentifier::Minus,
            UnaryOperator::BitNot => UnaryOperatorIdentifier::BitNot,
            UnaryOperator::LogicalNot => return Err(()),
        })
    }
}

impl From<UnaryOperatorIdentifier> for UnaryOperator {
    fn from(op: UnaryOperatorIdentifier) -> Self {
        match op {
            UnaryOperatorIdentifier::Plus => UnaryOperator::Plus,
            UnaryOperatorIdentifier::Minus => UnaryOperator::Minus,
            UnaryOperatorIdentifier::BitNot => UnaryOperator::BitNot,
        }
    }
}

impl Operator for UnaryOperatorIdentifier {
    fn symbol(self) -> &'static str {
        UnaryOperator::from(self).symbol()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Assign,
    Plus,
    Minus,
    Mul,
    Div,
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    BitOr,
    BitXOr,
    BitAnd,
    LogicalOr,
    LogicalAnd,
    ShiftLeft,
    ShiftRight,
    Concat,
    PropEqual,
    PropDisjunction,
    PropConjunction,
}

impl BinaryOperator {
    #[allow(clippy::match_same_arms, reason = "Readability")]
    pub fn is_valid_identifier(self) -> bool {
        match self {
            BinaryOperator::Assign
            | BinaryOperator::LogicalOr
            | BinaryOperator::LogicalAnd
            | BinaryOperator::PropEqual
            | BinaryOperator::PropDisjunction
            | BinaryOperator::PropConjunction => false,

            BinaryOperator::Plus
            | BinaryOperator::Minus
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanEq
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEq
            | BinaryOperator::BitOr
            | BinaryOperator::BitXOr
            | BinaryOperator::BitAnd
            | BinaryOperator::ShiftLeft
            | BinaryOperator::ShiftRight
            | BinaryOperator::Concat => true,
        }
    }
}

impl Operator for BinaryOperator {
    fn symbol(self) -> &'static str {
        match self {
            BinaryOperator::Assign => ":=",
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Mul => "×",
            BinaryOperator::Div => "÷",
            BinaryOperator::Equal => "=",
            BinaryOperator::NotEqual => "≠",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEq => "≤",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEq => "≥",
            BinaryOperator::BitOr => "|||",
            BinaryOperator::BitXOr => "^^^",
            BinaryOperator::BitAnd => "&&&",
            BinaryOperator::LogicalOr => "||",
            BinaryOperator::LogicalAnd => "&&",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",
            BinaryOperator::Concat => "++",
            BinaryOperator::PropEqual => "==",
            BinaryOperator::PropDisjunction => "\\/",
            BinaryOperator::PropConjunction => "/\\",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Plus,
    Minus,
    BitNot,
    LogicalNot,
}

impl UnaryOperator {
    pub fn is_valid_identifier(self) -> bool {
        self != UnaryOperator::LogicalNot
    }
}

impl Operator for UnaryOperator {
    fn symbol(self) -> &'static str {
        match self {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::BitNot => "~~~",
            UnaryOperator::LogicalNot => "!",
        }
    }
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pattern: WithLocation<Pattern>,
    pub body: WithLocation<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Discard,
    Tuple {
        elements: Vec<WithLocation<Pattern>>,
    },
    Binding {
        is_mutable: bool,
        name: WithLocation<Identifier>,
        pattern: Box<WithLocation<Pattern>>,
    },
    Constructor {
        path: WithLocation<PatternPath>,
        args: Vec<PatternArgument>,
    },
    Record {
        path: WithLocation<PatternPath>,
        args: Vec<PatternArgument>,
        record_field_patterns: Vec<WithLocation<RecordFieldPattern>>,
    },
    String(StringLiteral),
    Int(BigInt),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum PatternPath {
    Member {
        base: Box<WithLocation<PatternPath>>,
        member: WithLocation<Identifier>,
    },
    Base {
        name: WithLocation<Identifier>,
    },
}

#[derive(Debug, Clone)]
pub struct RecordFieldPattern {
    pub field_name: WithLocation<Identifier>,
    pub pattern: WithLocation<Pattern>,
}

#[derive(Debug, Clone)]
pub struct PatternArgument {
    pub function_parameter_list_type: FunctionParameterListType,
    pub arg: WithLocation<Pattern>,
}

#[derive(Debug, Clone)]
pub struct TubeSpec {
    pub modules: Vec<ModulePatternMapping>,
}

#[derive(Debug, Clone)]
pub struct ModulePatternMapping {
    pub module: Vec<WithLocation<ModulePatternSegment>>,
    pub file_name_template: StringLiteral,
}

#[derive(Debug, Clone)]
pub enum ModulePatternSegment {
    Named { name: String },
    Star { bound_name: Identifier },
    DoubleStar { bound_name: Identifier },
}

#[derive(Debug, Clone)]
pub struct ModuleDeclaration {
    pub module_path: Vec<String>,
    pub stmts: Vec<WithLocation<Stmt>>,
}
