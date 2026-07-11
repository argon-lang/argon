use argon_format_vm::vm as vf;
use argon_vm::analysis::BlockJumpScan;

use super::OptimizationPass;
use crate::optimizer::OptimizationState;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum BranchKind {
    Break,
    Retry,
}

#[derive(Debug, Default)]
pub struct TrailingBlockBranchElimination;

pub static TRAILING_BLOCK_BRANCH_ELIMINATION: TrailingBlockBranchElimination =
    TrailingBlockBranchElimination;

impl OptimizationPass for TrailingBlockBranchElimination {
    fn name(&self) -> &'static str {
        "trailing-block-branch-elimination"
    }

    fn max_iterations(&self) -> Option<usize> {
        None
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        optimize_region(state, &mut function.region);
    }
}

fn optimize_region(state: &mut OptimizationState, region: &mut Box<vf::Region>) {
    match region.as_mut() {
        vf::Region::BasicBlock { .. } => {}

        vf::Region::Sequence { regions } => {
            for region in regions {
                optimize_region(state, region);
            }
        }

        vf::Region::Block {
            block_id,
            flags,
            region,
        } => {
            optimize_region(state, region);

            if !can_exit_normally(region)
                && let Some(branch_kind) = remove_trailing_branch(state, region, block_id)
            {
                flags.is_loop = branch_kind == BranchKind::Retry;
                update_jump_flags(flags, block_id, region);
            }
        }

        vf::Region::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => {
            optimize_region(state, condition);
            optimize_region(state, when_true);
            optimize_region(state, when_false);
        }

        vf::Region::Finally { action, ensuring } => {
            optimize_region(state, action);
            optimize_region(state, ensuring);
        }
    }
}

fn update_jump_flags(flags: &mut vf::BlockFlags, block_id: &vf::BlockId, region: &vf::Region) {
    let mut scan = BlockJumpScan::new(block_id);
    scan.scan_region(region);
    flags.has_break = scan.has_break;
    flags.has_retry = scan.has_retry;
}

fn remove_trailing_branch(
    state: &mut OptimizationState,
    region: &mut vf::Region,
    block_id: &vf::BlockId,
) -> Option<BranchKind> {
    match region {
        vf::Region::BasicBlock { instructions } => {
            let branch_kind = trailing_branch_kind(instructions.last()?.as_ref(), block_id)?;
            instructions.pop();
            state.mark_changed();
            Some(branch_kind)
        }

        vf::Region::Sequence { regions } => regions
            .iter_mut()
            .rev()
            .find(|region| can_enter_region(region))
            .and_then(|region| remove_trailing_branch(state, region, block_id)),

        vf::Region::IfElse {
            when_true,
            when_false,
            ..
        } => {
            let true_branch = trailing_branch_in_region(when_true, block_id);
            let false_branch = trailing_branch_in_region(when_false, block_id);

            if true_branch.is_some() && true_branch == false_branch {
                remove_trailing_branch(state, when_true, block_id);
                remove_trailing_branch(state, when_false, block_id);
                true_branch
            } else {
                None
            }
        }

        vf::Region::Block { .. } | vf::Region::Finally { .. } => None,
    }
}

fn trailing_branch_in_region(region: &vf::Region, block_id: &vf::BlockId) -> Option<BranchKind> {
    match region {
        vf::Region::BasicBlock { instructions } => {
            trailing_branch_kind(instructions.last()?.as_ref(), block_id)
        }

        vf::Region::Sequence { regions } => regions
            .iter()
            .rev()
            .find(|region| can_enter_region(region))
            .and_then(|region| trailing_branch_in_region(region, block_id)),

        vf::Region::IfElse {
            when_true,
            when_false,
            ..
        } => {
            let true_branch = trailing_branch_in_region(when_true, block_id);
            let false_branch = trailing_branch_in_region(when_false, block_id);

            if true_branch.is_some() && true_branch == false_branch {
                true_branch
            } else {
                None
            }
        }

        vf::Region::Block { .. } | vf::Region::Finally { .. } => None,
    }
}

fn trailing_branch_kind(
    instruction: &vf::Instruction,
    block_id: &vf::BlockId,
) -> Option<BranchKind> {
    match instruction {
        vf::Instruction::BlockBreak {
            block_id: target_block_id,
        } if target_block_id.id == block_id.id => Some(BranchKind::Break),

        vf::Instruction::BlockRetry {
            block_id: target_block_id,
        } if target_block_id.id == block_id.id => Some(BranchKind::Retry),

        _ => None,
    }
}

fn can_exit_normally(region: &vf::Region) -> bool {
    match region {
        vf::Region::BasicBlock { instructions } => instructions
            .last()
            .is_none_or(|instruction| !is_unconditional_branch(instruction)),

        vf::Region::Sequence { regions } => {
            let mut can_reach_next_region = true;

            for region in regions {
                if !can_reach_next_region {
                    return false;
                }

                can_reach_next_region = can_exit_normally(region);
            }

            can_reach_next_region
        }

        vf::Region::Block { flags, region, .. } => {
            if flags.is_loop {
                false
            } else {
                can_exit_normally(region)
            }
        }

        vf::Region::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => {
            can_exit_normally(condition)
                && (can_exit_normally(when_true) || can_exit_normally(when_false))
        }

        vf::Region::Finally { action, ensuring } => {
            can_exit_normally(action) && can_exit_normally(ensuring)
        }
    }
}

fn can_enter_region(region: &vf::Region) -> bool {
    match region {
        vf::Region::BasicBlock { instructions } => !instructions.is_empty(),
        vf::Region::Sequence { regions } => regions.iter().any(|region| can_enter_region(region)),
        vf::Region::Block { .. } => false,
        vf::Region::IfElse { .. } | vf::Region::Finally { .. } => true,
    }
}

fn is_unconditional_branch(instruction: &vf::Instruction) -> bool {
    matches!(
        instruction,
        vf::Instruction::BlockBreak { .. }
            | vf::Instruction::BlockRetry { .. }
            | vf::Instruction::Raise { .. }
            | vf::Instruction::Return { .. }
            | vf::Instruction::Unreachable {}
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn removes_trailing_break_to_current_block() {
        let block_id = block_id(0);
        let mut function = function_body(block(
            block_id.clone(),
            flags(true, false, true),
            sequence(vec![basic_block(vec![Box::new(
                vf::Instruction::BlockBreak {
                    block_id: Box::new(block_id.clone()),
                },
            )])]),
        ));
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        TRAILING_BLOCK_BRANCH_ELIMINATION.optimize(&mut state, &mut function);

        assert!(state.changed());
        let (flags, region) = root_block(&function);
        assert!(!flags.has_break);
        assert!(!flags.has_retry);
        assert!(!flags.is_loop);
        assert_basic_block_len(region, 0);
    }

    #[test]
    fn removes_trailing_retry_to_current_block_and_marks_loop() {
        let block_id = block_id(0);
        let mut function = function_body(block(
            block_id.clone(),
            flags(false, true, false),
            basic_block(vec![Box::new(vf::Instruction::BlockRetry {
                block_id: Box::new(block_id.clone()),
            })]),
        ));
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        TRAILING_BLOCK_BRANCH_ELIMINATION.optimize(&mut state, &mut function);

        assert!(state.changed());
        let (flags, region) = root_block(&function);
        assert!(!flags.has_break);
        assert!(!flags.has_retry);
        assert!(flags.is_loop);
        assert_basic_block_len(region, 0);
    }

    #[test]
    fn keeps_trailing_branch_to_other_block() {
        let current_block_id = block_id(0);
        let other_block_id = block_id(1);
        let mut function = function_body(block(
            current_block_id,
            flags(true, false, false),
            basic_block(vec![Box::new(vf::Instruction::BlockBreak {
                block_id: Box::new(other_block_id),
            })]),
        ));
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        TRAILING_BLOCK_BRANCH_ELIMINATION.optimize(&mut state, &mut function);

        assert!(!state.changed());
        let (flags, region) = root_block(&function);
        assert!(flags.has_break);
        assert_basic_block_len(region, 1);
    }

    #[test]
    fn keeps_branch_when_block_can_exit_normally() {
        let block_id = block_id(0);
        let mut function = function_body(block(
            block_id.clone(),
            flags(true, false, false),
            if_else(
                basic_block(Vec::new()),
                basic_block(Vec::new()),
                basic_block(vec![Box::new(vf::Instruction::BlockBreak {
                    block_id: Box::new(block_id),
                })]),
            ),
        ));
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        TRAILING_BLOCK_BRANCH_ELIMINATION.optimize(&mut state, &mut function);

        assert!(!state.changed());
        let (flags, _) = root_block(&function);
        assert!(flags.has_break);
        assert!(!flags.is_loop);
    }

    fn function_body(region: vf::Region) -> vf::FunctionBody {
        vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: Vec::new(),
            }),
            region: Box::new(region),
        }
    }

    fn block(block_id: vf::BlockId, flags: vf::BlockFlags, region: vf::Region) -> vf::Region {
        vf::Region::Block {
            block_id: Box::new(block_id),
            flags,
            region: Box::new(region),
        }
    }

    fn if_else(condition: vf::Region, when_true: vf::Region, when_false: vf::Region) -> vf::Region {
        vf::Region::IfElse {
            when_true_block_id: Box::new(block_id(1)),
            when_false_block_id: Box::new(block_id(2)),
            condition: Box::new(condition),
            when_true: Box::new(when_true),
            when_false: Box::new(when_false),
        }
    }

    fn sequence(regions: Vec<vf::Region>) -> vf::Region {
        vf::Region::Sequence {
            regions: regions.into_iter().map(Box::new).collect(),
        }
    }

    fn basic_block(instructions: Vec<Box<vf::Instruction>>) -> vf::Region {
        vf::Region::BasicBlock { instructions }
    }

    fn block_id(id: u32) -> vf::BlockId {
        vf::BlockId { id: id.into() }
    }

    fn flags(has_break: bool, has_retry: bool, is_loop: bool) -> vf::BlockFlags {
        vf::BlockFlags {
            has_break,
            has_retry,
            is_loop,
        }
    }

    fn root_block(function: &vf::FunctionBody) -> (&vf::BlockFlags, &vf::Region) {
        let vf::Region::Block { flags, region, .. } = function.region.as_ref() else {
            unreachable!();
        };

        (flags, region)
    }

    fn assert_basic_block_len(region: &vf::Region, expected_len: usize) {
        let region = match region {
            vf::Region::Sequence { regions } => regions.first().unwrap().as_ref(),
            region => region,
        };
        let vf::Region::BasicBlock { instructions } = region else {
            unreachable!();
        };

        assert_eq!(instructions.len(), expected_len);
    }
}
