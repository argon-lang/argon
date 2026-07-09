mod copy_propagation;
mod dead_code_elimination;
mod unused_register_elimination;

pub use copy_propagation::COPY_PROPAGATION;
pub use dead_code_elimination::DEAD_CODE_ELIMINATION;
pub use unused_register_elimination::UNUSED_REGISTER_ELIMINATION;

use argon_format_vm::vm as vf;

use crate::optimizer::OptimizationState;

pub trait OptimizationPass {
    fn max_iterations(&self) -> Option<usize>;

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody);
}

pub fn pass_by_name(name: &str) -> Option<&'static dyn OptimizationPass> {
    match name {
        "copy-propagation" => Some(&COPY_PROPAGATION),
        "dead-code-elimination" => Some(&DEAD_CODE_ELIMINATION),
        "unused-register-elimination" => Some(&UNUSED_REGISTER_ELIMINATION),
        _ => None,
    }
}
