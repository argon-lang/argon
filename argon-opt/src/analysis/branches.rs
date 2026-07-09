use std::collections::{HashMap, HashSet};

use argon_format_vm::vm as vf;
use num_bigint::BigUint;

pub type InstructionPointer = *const vf::Instruction;

#[derive(Debug, Default)]
pub struct BlockBranches {
    pub breaks: HashSet<InstructionPointer>,
    pub retries: HashSet<InstructionPointer>,
}

#[derive(Debug, Default)]
pub struct BranchAnalysis {
    pub blocks: HashMap<BigUint, BlockBranches>,
}

impl BranchAnalysis {
    pub fn analyze(block: &vf::Block) -> Self {
        let mut analysis = Self::default();
        analysis.scan_block(block);
        analysis
    }

    fn scan_block(&mut self, block: &vf::Block) {
        for instruction in &block.instructions {
            self.scan_instruction(instruction);
        }
    }

    fn scan_instruction(&mut self, instruction: &vf::Instruction) {
        let instruction_pointer = instruction as InstructionPointer;

        match instruction {
            vf::Instruction::Block { block_id, body, .. } => {
                self.blocks.entry(block_id.id.clone()).or_default();
                self.scan_block(body);
            }

            vf::Instruction::BlockBreak { block_id }
            | vf::Instruction::BlockBreakIf { block_id, .. }
            | vf::Instruction::BlockBreakUnless { block_id, .. } => {
                self.blocks
                    .entry(block_id.id.clone())
                    .or_default()
                    .breaks
                    .insert(instruction_pointer);
            }

            vf::Instruction::IsEnumVariantOrBreak {
                not_variant_block_id,
                ..
            } => {
                self.blocks
                    .entry(not_variant_block_id.id.clone())
                    .or_default()
                    .breaks
                    .insert(instruction_pointer);
            }

            vf::Instruction::BlockRetry { block_id } => {
                self.blocks
                    .entry(block_id.id.clone())
                    .or_default()
                    .retries
                    .insert(instruction_pointer);
            }

            vf::Instruction::Finally { action, ensuring } => {
                self.scan_block(action);
                self.scan_block(ensuring);
            }

            vf::Instruction::IfElse {
                when_true_block_id,
                when_false_block_id,
                condition,
                when_true,
                when_false,
            } => {
                self.blocks
                    .entry(when_true_block_id.id.clone())
                    .or_default();
                self.blocks
                    .entry(when_false_block_id.id.clone())
                    .or_default();
                self.scan_block(condition);
                self.scan_block(when_true);
                self.scan_block(when_false);
            }

            _ => {}
        }
    }
}
