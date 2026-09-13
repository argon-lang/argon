use argon_expr::{ErasureMode, ExprContext};
use argon_parser::ast::Identifier;
use argon_util::UniqueIdentifier;
use core::hash::{Hash, Hasher};
use derivative::Derivative;

use crate::Value;

/// An opaque identity for a state in one function body.
///
/// Program points do not encode instruction order. A value in a region may snapshot
/// its entry, a preceding instruction exit, or a dominating ancestor point, but not
/// a point belonging only to a sibling region. Values escaping structured control
/// flow use the owning instruction node's exit point.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProgramPoint(UniqueIdentifier);

impl ProgramPoint {
    pub fn new() -> Self {
        Self(UniqueIdentifier::new())
    }
}

impl Default for ProgramPoint {
    fn default() -> Self {
        Self::new()
    }
}

/// An opaque register identity local to a function body.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Register(UniqueIdentifier);

impl Register {
    pub fn new() -> Self {
        Self(UniqueIdentifier::new())
    }
}

impl Default for Register {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
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
    StaticMethod(EC::StaticMethod),
    Instance(EC::Instance),
    Field(EC::RecordField),
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum RegisterOrigin<EC: ExprContext + ?Sized> {
    Temporary,
    Local,
    Parameter {
        owner: ExpressionOwner<EC>,
        parameter_index: usize,
    },
    InstanceParameter {
        owner: ExpressionOwner<EC>,
    },
    ClosureParameter,
}

/// The declaration and source-level metadata for a register in a function body.
#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct RegisterDeclaration<EC: ExprContext + ?Sized> {
    pub register: Register,
    pub r#type: Value<EC>,
    pub name: Option<Identifier>,
    pub is_mutable: bool,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
    pub origin: RegisterOrigin<EC>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct BlockLabel<EC: ExprContext + ?Sized> {
    pub id: UniqueIdentifier,
    pub name: Option<Identifier>,
    pub kind: BlockLabelKind,
    pub block_result_type: Value<EC>,
}

impl<EC: ExprContext + ?Sized> PartialEq for BlockLabel<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<EC: ExprContext + ?Sized> Eq for BlockLabel<EC> {}

impl<EC: ExprContext + ?Sized> Hash for BlockLabel<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockLabelKind {
    Block,
    Loop,
    WhileOuter,
    WhileInner,
}
