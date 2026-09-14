use alloc::vec::Vec;
use argon_expr::{ErasureMode, ExprContext};
use derivative::Derivative;
use parse18_runtime::Location;

use crate::{Instruction, ProgramPoint, Register};

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct FunctionBody<EC: ExprContext + ?Sized> {
    pub region: Region<EC>,
}

/// An ordered instruction region. `entry` addresses the state before its first
/// instruction, including when `instructions` is empty, and `result` identifies
/// the register containing the region's value.
#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Region<EC: ExprContext + ?Sized> {
    pub entry: ProgramPoint,
    pub instructions: Vec<InstructionNode<EC>>,
    pub result: Register,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct InstructionNode<EC: ExprContext + ?Sized> {
    pub instruction: Instruction<EC>,
    pub location: Location,
    /// The state immediately after this instruction; for structured instructions,
    /// this is the common join point of all nested regions.
    pub exit: ProgramPoint,
    pub erasure_mode: ErasureMode,
}
