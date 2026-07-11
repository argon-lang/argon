mod analysis;
mod mutator;
pub mod optimizer;
pub mod pass;

use argon_vm::model::TubeModel;

pub fn optimize_tube<'a>(
    optimizer: &optimizer::Optimizer,
    tube: &mut TubeModel,
    referenced_tubes: impl IntoIterator<Item = &'a TubeModel>,
) {
    optimizer.optimize(tube, referenced_tubes);
}
