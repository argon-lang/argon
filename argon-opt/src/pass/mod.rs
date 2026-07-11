mod copy_propagation;
mod dead_code_elimination;
mod flatten_regions;
mod function_inlining;
mod trailing_block_branch_elimination;
mod unused_register_elimination;

pub use copy_propagation::COPY_PROPAGATION;
pub use dead_code_elimination::DEAD_CODE_ELIMINATION;
pub use flatten_regions::FLATTEN_REGIONS;
pub use function_inlining::FUNCTION_INLINING;
pub use trailing_block_branch_elimination::TRAILING_BLOCK_BRANCH_ELIMINATION;
pub use unused_register_elimination::UNUSED_REGISTER_ELIMINATION;

use argon_format_vm::vm as vf;

use crate::optimizer::OptimizationState;

pub static ALL_OPTIMIZATIONS: &[&'static dyn OptimizationPass] = &[
    &COPY_PROPAGATION,
    &FUNCTION_INLINING,
    &DEAD_CODE_ELIMINATION,
    &UNUSED_REGISTER_ELIMINATION,
    &TRAILING_BLOCK_BRANCH_ELIMINATION,
    &FLATTEN_REGIONS,
];

pub trait OptimizationPass: Sync {
    fn name(&self) -> &'static str;

    fn max_iterations(&self) -> Option<usize>;

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody);
}

pub fn pass_by_name(name: &str) -> Option<&'static dyn OptimizationPass> {
    ALL_OPTIMIZATIONS
        .iter()
        .copied()
        .find(|pass| pass.name() == name)
}
