use alloc::boxed::Box;
use alloc::vec::Vec;
use argon_format_vm::vm as vf;
use core::slice;

pub fn instructions(block: &vf::Block) -> impl Iterator<Item = &vf::Instruction> + '_ {
    BlockInstructionIter::new(block)
}

pub fn instruction_foreach_mut(
    block: &mut vf::Block,
    mut callback: impl FnMut(&mut vf::Instruction),
) {
    instruction_foreach_mut_impl(block, &mut callback);
}

pub fn instruction_retain(
    block: &mut vf::Block,
    mut callback: impl FnMut(&mut vf::Instruction) -> bool,
) {
    instruction_retain_impl(block, &mut callback);
}

fn instruction_retain_impl(
    block: &mut vf::Block,
    callback: &mut impl FnMut(&mut vf::Instruction) -> bool,
) {
    block.instructions.retain_mut(|instruction| {
        match instruction.as_mut() {
            vf::Instruction::Block { body, .. } => {
                instruction_retain_impl(body, callback);
            }
            vf::Instruction::Finally { action, ensuring } => {
                instruction_retain_impl(action, callback);
                instruction_retain_impl(ensuring, callback);
            }
            vf::Instruction::IfElse {
                condition,
                when_true,
                when_false,
                ..
            } => {
                instruction_retain_impl(condition, callback);
                instruction_retain_impl(when_true, callback);
                instruction_retain_impl(when_false, callback);
            }
            _ => {}
        }

        callback(instruction)
    });
}

fn instruction_foreach_mut_impl(
    block: &mut vf::Block,
    callback: &mut impl FnMut(&mut vf::Instruction),
) {
    for instruction in &mut block.instructions {
        callback(instruction);

        match instruction.as_mut() {
            vf::Instruction::Block { body, .. } => {
                instruction_foreach_mut_impl(body, callback);
            }
            vf::Instruction::Finally { action, ensuring } => {
                instruction_foreach_mut_impl(action, callback);
                instruction_foreach_mut_impl(ensuring, callback);
            }
            vf::Instruction::IfElse {
                condition,
                when_true,
                when_false,
                ..
            } => {
                instruction_foreach_mut_impl(when_true, callback);
                instruction_foreach_mut_impl(when_false, callback);
                instruction_foreach_mut_impl(condition, callback);
            }
            _ => {}
        }
    }
}

struct BlockInstructionIter<'a> {
    stack: Vec<slice::Iter<'a, Box<vf::Instruction>>>,
}

impl<'a> BlockInstructionIter<'a> {
    fn new(block: &'a vf::Block) -> Self {
        BlockInstructionIter {
            stack: alloc::vec![block.instructions.iter()],
        }
    }

    fn push_nested_blocks(&mut self, instruction: &'a vf::Instruction) {
        match instruction {
            vf::Instruction::Block { body, .. } => {
                self.stack.push(body.instructions.iter());
            }
            vf::Instruction::Finally { action, ensuring } => {
                self.stack.push(ensuring.instructions.iter());
                self.stack.push(action.instructions.iter());
            }
            vf::Instruction::IfElse {
                condition,
                when_true,
                when_false,
                ..
            } => {
                self.stack.push(condition.instructions.iter());
                self.stack.push(when_false.instructions.iter());
                self.stack.push(when_true.instructions.iter());
            }
            _ => {}
        }
    }
}

impl<'a> Iterator for BlockInstructionIter<'a> {
    type Item = &'a vf::Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let instructions = self.stack.last_mut()?;

            if let Some(instruction) = instructions.next() {
                let instruction = instruction.as_ref();
                self.push_nested_blocks(instruction);
                return Some(instruction);
            }

            self.stack.pop();
        }
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

    pub fn scan_block(&mut self, block: &vf::Block) {
        for instruction in instructions(block) {
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

    #[test]
    fn mutably_visits_top_level_and_nested_instructions() {
        let mut block = vf::Block {
            instructions: vec![
                constant(1),
                Box::new(vf::Instruction::Finally {
                    action: Box::new(vf::Block {
                        instructions: vec![constant(2)].into(),
                    }),
                    ensuring: Box::new(vf::Block {
                        instructions: vec![constant(3)].into(),
                    }),
                }),
            ]
            .into(),
        };
        let mut visited = 0;
        instruction_foreach_mut(&mut block, |instruction| {
            visited += 1;
            if let vf::Instruction::ConstInt { value, .. } = instruction {
                *value = 0.into();
            }
        });

        assert_eq!(visited, 4);
        assert!(
            instructions(&block)
                .filter_map(|instruction| match instruction {
                    vf::Instruction::ConstInt { value, .. } => Some(value),
                    _ => None,
                })
                .all(|value| *value == 0.into())
        );
    }

    #[test]
    fn retains_top_level_and_nested_instructions_matching_predicate() {
        let mut block = vf::Block {
            instructions: vec![
                constant(1),
                Box::new(vf::Instruction::Block {
                    block_id: Box::new(vf::BlockId { id: 0_u32.into() }),
                    flags: vf::BlockFlags {
                        has_break: false,
                        has_retry: false,
                        is_loop: false,
                    },
                    body: Box::new(vf::Block {
                        instructions: vec![constant(2), constant(3)].into(),
                    }),
                }),
            ]
            .into(),
        };

        instruction_retain(&mut block, |instruction| {
            !matches!(
                instruction,
                vf::Instruction::ConstInt { value, .. } if *value == 2.into()
            )
        });

        assert_eq!(instructions(&block).count(), 3);
        assert!(!instructions(&block).any(|instruction| matches!(
            instruction,
            vf::Instruction::ConstInt { value, .. } if *value == 2.into()
        )));
    }

    fn constant(value: u32) -> Box<vf::Instruction> {
        Box::new(vf::Instruction::ConstInt {
            dest: Box::new(vf::RegisterId { id: value.into() }),
            value: value.into(),
        })
    }
}
