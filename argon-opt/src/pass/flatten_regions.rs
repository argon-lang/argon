use argon_format_vm::vm as vf;

use super::OptimizationPass;
use crate::optimizer::OptimizationState;

#[derive(Debug, Default)]
pub struct FlattenRegions;

pub static FLATTEN_REGIONS: FlattenRegions = FlattenRegions;

impl OptimizationPass for FlattenRegions {
    fn name(&self) -> &'static str {
        "flatten-regions"
    }

    fn max_iterations(&self) -> Option<usize> {
        Some(1)
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        if flatten_region(&mut function.region) {
            state.mark_changed();
        }
    }
}

fn flatten_region(region: &mut Box<vf::Region>) -> bool {
    match region.as_mut() {
        vf::Region::BasicBlock { instructions } if instructions.is_empty() => {
            *region = Box::new(vf::Region::Sequence {
                regions: Vec::new(),
            });
            true
        }

        vf::Region::BasicBlock { .. } => false,

        vf::Region::Sequence { regions } => flatten_sequence(regions),

        vf::Region::Block { region, .. } => flatten_region(region),

        vf::Region::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => flatten_region(condition) | flatten_region(when_true) | flatten_region(when_false),

        vf::Region::Finally { action, ensuring } => {
            flatten_region(action) | flatten_region(ensuring)
        }
    }
}

fn flatten_sequence(regions: &mut Vec<Box<vf::Region>>) -> bool {
    let mut changed = false;
    let old_regions = core::mem::take(regions);

    for mut region in old_regions {
        changed |= flatten_region(&mut region);

        match *region {
            vf::Region::Sequence {
                regions: nested_regions,
            } => {
                regions.extend(nested_regions);
                changed = true;
            }

            vf::Region::BasicBlock { instructions } if instructions.is_empty() => {
                changed = true;
            }

            region => regions.push(Box::new(region)),
        }
    }

    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn flattens_nested_sequences_and_removes_empty_basic_blocks() {
        let mut function = function_body(vf::Region::Sequence {
            regions: vec![
                Box::new(basic_block(vec![constant(1)])),
                Box::new(vf::Region::Sequence {
                    regions: vec![
                        Box::new(basic_block(Vec::new())),
                        Box::new(basic_block(vec![constant(2)])),
                    ],
                }),
                Box::new(basic_block(Vec::new())),
            ],
        });
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        FLATTEN_REGIONS.optimize(&mut state, &mut function);

        assert!(state.changed());
        let vf::Region::Sequence { regions } = function.region.as_ref() else {
            unreachable!();
        };

        assert_eq!(regions.len(), 2);
        assert_eq!(first_const_value(&regions[0]), 1_u32.into());
        assert_eq!(first_const_value(&regions[1]), 2_u32.into());
    }

    #[test]
    fn recurses_into_structural_regions() {
        let mut function = function_body(vf::Region::Finally {
            action: Box::new(vf::Region::Block {
                block_id: Box::new(vf::BlockId { id: 0_u32.into() }),
                flags: vf::BlockFlags {
                    has_break: false,
                    has_retry: false,
                    is_loop: false,
                },
                region: Box::new(vf::Region::Sequence {
                    regions: vec![
                        Box::new(vf::Region::Sequence {
                            regions: vec![Box::new(basic_block(vec![constant(1)]))],
                        }),
                        Box::new(basic_block(Vec::new())),
                    ],
                }),
            }),
            ensuring: Box::new(basic_block(Vec::new())),
        });
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        FLATTEN_REGIONS.optimize(&mut state, &mut function);

        assert!(state.changed());
        let vf::Region::Finally { action, .. } = function.region.as_ref() else {
            unreachable!();
        };
        let vf::Region::Finally { ensuring, .. } = function.region.as_ref() else {
            unreachable!();
        };
        let vf::Region::Block { region, .. } = action.as_ref() else {
            unreachable!();
        };
        let vf::Region::Sequence { regions } = region.as_ref() else {
            unreachable!();
        };

        assert_eq!(regions.len(), 1);
        assert_eq!(first_const_value(&regions[0]), 1_u32.into());
        assert_empty_sequence(ensuring);
    }

    #[test]
    fn replaces_empty_basic_block_with_empty_sequence() {
        let mut function = function_body(basic_block(Vec::new()));
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        FLATTEN_REGIONS.optimize(&mut state, &mut function);

        assert!(state.changed());
        assert_empty_sequence(&function.region);
    }

    fn function_body(region: vf::Region) -> vf::FunctionBody {
        vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: Vec::new(),
            }),
            region: Box::new(region),
        }
    }

    fn basic_block(instructions: Vec<Box<vf::Instruction>>) -> vf::Region {
        vf::Region::BasicBlock { instructions }
    }

    fn constant(value: u32) -> Box<vf::Instruction> {
        Box::new(vf::Instruction::ConstInt {
            dest: Box::new(vf::RegisterId { id: value.into() }),
            value: value.into(),
        })
    }

    fn first_const_value(region: &vf::Region) -> num_bigint::BigInt {
        let vf::Region::BasicBlock { instructions } = region else {
            unreachable!();
        };
        let Some(vf::Instruction::ConstInt { value, .. }) = instructions.first().map(Box::as_ref)
        else {
            unreachable!();
        };
        value.clone()
    }

    fn assert_empty_sequence(region: &vf::Region) {
        let vf::Region::Sequence { regions } = region else {
            unreachable!();
        };
        assert!(regions.is_empty());
    }
}
use alloc::{boxed::Box, vec::Vec};
