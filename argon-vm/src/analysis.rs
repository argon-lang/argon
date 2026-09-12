use alloc::boxed::Box;
use alloc::vec::Vec;
use argon_format_vm::vm as vf;
use core::slice;

pub fn instructions(region: &vf::Region) -> impl Iterator<Item = &vf::Instruction> + '_ {
    RegionInstructionIter::new(region)
}

pub fn basic_blocks(region: &vf::Region) -> impl Iterator<Item = &vf::Region> + '_ {
    BasicBlockIter::new(region)
}

pub fn basic_blocks_foreach_mut(
    region: &mut vf::Region,
    mut callback: impl FnMut(&mut vf::Region),
) {
    basic_blocks_foreach_mut_impl(region, &mut callback);
}

pub fn instruction_foreach_mut(
    region: &mut vf::Region,
    mut callback: impl FnMut(&mut vf::Instruction),
) {
    instruction_foreach_mut_impl(region, &mut callback);
}

pub fn instruction_retain(
    region: &mut vf::Region,
    mut callback: impl FnMut(&mut vf::Instruction) -> bool,
) {
    instruction_retain_impl(region, &mut callback);
}

fn instruction_retain_impl(
    region: &mut vf::Region,
    callback: &mut impl FnMut(&mut vf::Instruction) -> bool,
) {
    match region {
        vf::Region::BasicBlock { instructions, .. } => {
            instructions.retain_mut(|instruction| callback(instruction));
        }
        vf::Region::Sequence { regions } => {
            for region in regions {
                instruction_retain_impl(region, callback);
            }
        }
        vf::Region::Block { region, .. } => {
            instruction_retain_impl(region, callback);
        }
        vf::Region::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => {
            instruction_retain_impl(condition, callback);
            instruction_retain_impl(when_true, callback);
            instruction_retain_impl(when_false, callback);
        }
        vf::Region::Finally { action, ensuring } => {
            instruction_retain_impl(action, callback);
            instruction_retain_impl(ensuring, callback);
        }
    }
}

fn basic_blocks_foreach_mut_impl(
    region: &mut vf::Region,
    callback: &mut impl FnMut(&mut vf::Region),
) {
    match region {
        vf::Region::BasicBlock { .. } => callback(region),
        vf::Region::Sequence { regions } => {
            for region in regions {
                basic_blocks_foreach_mut_impl(region, callback);
            }
        }
        vf::Region::Block { region, .. } => {
            basic_blocks_foreach_mut_impl(region, callback);
        }
        vf::Region::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => {
            basic_blocks_foreach_mut_impl(condition, callback);
            basic_blocks_foreach_mut_impl(when_true, callback);
            basic_blocks_foreach_mut_impl(when_false, callback);
        }
        vf::Region::Finally { action, ensuring } => {
            basic_blocks_foreach_mut_impl(action, callback);
            basic_blocks_foreach_mut_impl(ensuring, callback);
        }
    }
}

fn instruction_foreach_mut_impl(
    region: &mut vf::Region,
    callback: &mut impl FnMut(&mut vf::Instruction),
) {
    match region {
        vf::Region::BasicBlock { instructions, .. } => {
            for instruction in instructions {
                callback(instruction);
            }
        }
        vf::Region::Sequence { regions } => {
            for region in regions {
                instruction_foreach_mut_impl(region, callback);
            }
        }
        vf::Region::Block { region, .. } => {
            instruction_foreach_mut_impl(region, callback);
        }
        vf::Region::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => {
            instruction_foreach_mut_impl(condition, callback);
            instruction_foreach_mut_impl(when_true, callback);
            instruction_foreach_mut_impl(when_false, callback);
        }
        vf::Region::Finally { action, ensuring } => {
            instruction_foreach_mut_impl(action, callback);
            instruction_foreach_mut_impl(ensuring, callback);
        }
    }
}

enum RegionInstructionIterFrame<'a> {
    Region(&'a vf::Region),
    Instructions(slice::Iter<'a, Box<vf::Instruction>>),
}

struct RegionInstructionIter<'a> {
    stack: Vec<RegionInstructionIterFrame<'a>>,
}

impl<'a> RegionInstructionIter<'a> {
    fn new(region: &'a vf::Region) -> Self {
        RegionInstructionIter {
            stack: alloc::vec![RegionInstructionIterFrame::Region(region)],
        }
    }
}

impl<'a> Iterator for RegionInstructionIter<'a> {
    type Item = &'a vf::Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(frame) = self.stack.pop() {
            match frame {
                RegionInstructionIterFrame::Region(region) => match region {
                    vf::Region::BasicBlock { instructions, .. } => {
                        self.stack.push(RegionInstructionIterFrame::Instructions(
                            instructions.iter(),
                        ));
                    }
                    vf::Region::Sequence { regions } => {
                        self.stack.extend(
                            regions
                                .iter()
                                .rev()
                                .map(|region| RegionInstructionIterFrame::Region(region)),
                        );
                    }
                    vf::Region::Block { region, .. } => {
                        self.stack.push(RegionInstructionIterFrame::Region(region));
                    }
                    vf::Region::IfElse {
                        condition,
                        when_true,
                        when_false,
                        ..
                    } => {
                        self.stack
                            .push(RegionInstructionIterFrame::Region(when_false));
                        self.stack
                            .push(RegionInstructionIterFrame::Region(when_true));
                        self.stack
                            .push(RegionInstructionIterFrame::Region(condition));
                    }
                    vf::Region::Finally { action, ensuring } => {
                        self.stack
                            .push(RegionInstructionIterFrame::Region(ensuring));
                        self.stack.push(RegionInstructionIterFrame::Region(action));
                    }
                },
                RegionInstructionIterFrame::Instructions(mut instructions) => {
                    if let Some(instruction) = instructions.next() {
                        self.stack
                            .push(RegionInstructionIterFrame::Instructions(instructions));

                        return Some(instruction.as_ref());
                    }
                }
            }
        }

        None
    }
}

struct BasicBlockIter<'a> {
    stack: Vec<&'a vf::Region>,
}

impl<'a> BasicBlockIter<'a> {
    fn new(region: &'a vf::Region) -> Self {
        BasicBlockIter {
            stack: alloc::vec![region],
        }
    }
}

impl<'a> Iterator for BasicBlockIter<'a> {
    type Item = &'a vf::Region;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(region) = self.stack.pop() {
            match region {
                vf::Region::BasicBlock { .. } => return Some(region),
                vf::Region::Sequence { regions } => {
                    self.stack.extend(regions.iter().rev().map(Box::as_ref));
                }
                vf::Region::Block { region, .. } => {
                    self.stack.push(region);
                }
                vf::Region::IfElse {
                    condition,
                    when_true,
                    when_false,
                    ..
                } => {
                    self.stack.push(when_false);
                    self.stack.push(when_true);
                    self.stack.push(condition);
                }
                vf::Region::Finally { action, ensuring } => {
                    self.stack.push(ensuring);
                    self.stack.push(action);
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct BlockJumpScan<'a> {
    block_id: &'a vf::BlockId,
    pub has_break: bool,
    pub has_retry: bool,
}

impl<'a> BlockJumpScan<'a> {
    pub fn new(block_id: &'a vf::BlockId) -> Self {
        BlockJumpScan {
            block_id,
            has_break: false,
            has_retry: false,
        }
    }

    fn found_both(&self) -> bool {
        self.has_break && self.has_retry
    }

    pub fn scan_region(&mut self, region: &vf::Region) {
        for instruction in instructions(region) {
            self.scan_instruction(instruction);

            if self.found_both() {
                break;
            }
        }
    }

    fn scan_instruction(&mut self, instruction: &vf::Instruction) {
        match instruction {
            vf::Instruction::BlockBreak {
                block_id: target_block_id,
            }
            | vf::Instruction::BlockBreakIf {
                block_id: target_block_id,
                ..
            }
            | vf::Instruction::BlockBreakUnless {
                block_id: target_block_id,
                ..
            } if target_block_id.id == self.block_id.id => {
                self.has_break = true;
            }
            vf::Instruction::BlockRetry {
                block_id: target_block_id,
            } if target_block_id.id == self.block_id.id => {
                self.has_retry = true;
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn mutably_visits_top_level_and_nested_instructions() {
        let mut region = vf::Region::Finally {
            action: Box::new(basic_block(vec![constant(1), constant(2)])),
            ensuring: Box::new(basic_block(vec![constant(3)])),
        };
        let mut visited = 0;
        instruction_foreach_mut(&mut region, |instruction| {
            visited += 1;
            if let vf::Instruction::ConstInt { value, .. } = instruction {
                *value = 0.into();
            }
        });

        assert_eq!(visited, 3);
        assert!(
            instructions(&region)
                .filter_map(|instruction| match instruction {
                    vf::Instruction::ConstInt { value, .. } => Some(value),
                    _ => None,
                })
                .all(|value| *value == 0.into())
        );
    }

    #[test]
    fn retains_top_level_and_nested_instructions_matching_predicate() {
        let mut region = vf::Region::Sequence {
            regions: vec![
                Box::new(basic_block(vec![constant(1)])),
                Box::new(vf::Region::Block {
                    block_id: Box::new(vf::BlockId { id: 0_u32.into() }),
                    flags: vf::BlockFlags {
                        has_break: false,
                        has_retry: false,
                        is_loop: false,
                    },
                    region: Box::new(basic_block(vec![constant(2), constant(3)])),
                }),
            ]
            .into(),
        };

        instruction_retain(&mut region, |instruction| {
            !matches!(
                instruction,
                vf::Instruction::ConstInt { value, .. } if *value == 2.into()
            )
        });

        assert_eq!(instructions(&region).count(), 2);
        assert!(!instructions(&region).any(|instruction| matches!(
            instruction,
            vf::Instruction::ConstInt { value, .. } if *value == 2.into()
        )));
    }

    #[test]
    fn iterates_basic_blocks_in_region_order() {
        let region = vf::Region::Sequence {
            regions: vec![
                Box::new(basic_block(vec![constant(1)])),
                Box::new(vf::Region::IfElse {
                    when_true_block_id: Box::new(vf::BlockId { id: 0_u32.into() }),
                    when_false_block_id: Box::new(vf::BlockId { id: 1_u32.into() }),
                    condition: Box::new(basic_block(vec![constant(2)])),
                    when_true: Box::new(vf::Region::Block {
                        block_id: Box::new(vf::BlockId { id: 2_u32.into() }),
                        flags: vf::BlockFlags {
                            has_break: false,
                            has_retry: false,
                            is_loop: true,
                        },
                        region: Box::new(basic_block(vec![constant(3)])),
                    }),
                    when_false: Box::new(basic_block(vec![constant(4)])),
                }),
            ]
            .into(),
        };

        let values: Vec<_> = basic_blocks(&region)
            .map(|region| {
                let vf::Region::BasicBlock { instructions, .. } = region else {
                    unreachable!();
                };
                let Some(vf::Instruction::ConstInt { value, .. }) =
                    instructions.first().map(Box::as_ref)
                else {
                    unreachable!();
                };
                value.clone()
            })
            .collect();

        assert_eq!(
            values,
            vec![1_u32.into(), 2_u32.into(), 3_u32.into(), 4_u32.into()]
        );
    }

    #[test]
    fn mutably_visits_basic_blocks_in_region_order() {
        let mut region = vf::Region::Finally {
            action: Box::new(vf::Region::Sequence {
                regions: vec![
                    Box::new(basic_block(vec![constant(1)])),
                    Box::new(basic_block(vec![constant(2)])),
                ]
                .into(),
            }),
            ensuring: Box::new(basic_block(vec![constant(3)])),
        };

        let mut next = 10_u32;
        basic_blocks_foreach_mut(&mut region, |region| {
            let vf::Region::BasicBlock { instructions, .. } = region else {
                unreachable!();
            };

            instructions.push(constant(next));
            next += 1;
        });

        let last_values: Vec<_> = basic_blocks(&region)
            .map(|region| {
                let vf::Region::BasicBlock { instructions, .. } = region else {
                    unreachable!();
                };
                let Some(vf::Instruction::ConstInt { value, .. }) =
                    instructions.last().map(Box::as_ref)
                else {
                    unreachable!();
                };
                value.clone()
            })
            .collect();

        assert_eq!(
            last_values,
            vec![10_u32.into(), 11_u32.into(), 12_u32.into()]
        );
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
}
