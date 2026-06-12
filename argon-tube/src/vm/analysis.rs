use alloc::boxed::Box;
use alloc::vec::Vec;
use argon_format::vm as vf;
use core::slice;

pub fn instructions(block: &vf::Block) -> impl Iterator<Item = &vf::Instruction> + '_ {
    BlockInstructionIter::new(block)
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
