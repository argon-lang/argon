pub mod branches;
pub mod control_flow;
pub mod control_path;
pub mod liveness;
pub mod purity;
pub mod reaching_defs;
pub mod use_def;

use argon_format_vm::vm as vf;
use std::collections::HashSet;

pub type InstructionPointer = *const vf::Instruction;

pub type InstructionSet = HashSet<InstructionPointer>;
pub type RegionPointer = *const vf::Region;
pub type RegionSet = HashSet<RegionPointer>;
pub type VariableSet = HashSet<vf::RegisterId>;
