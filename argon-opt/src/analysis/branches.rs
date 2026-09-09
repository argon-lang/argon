use hashbrown::{HashMap, HashSet};

use argon_format_vm::vm as vf;

use super::RegionPointer;

#[derive(Debug, Default)]
pub struct BlockBranches {
    pub breaks: HashSet<RegionPointer>,
    pub retries: HashSet<RegionPointer>,
}

#[derive(Debug, Default)]
pub struct BranchAnalysis {
    pub blocks: HashMap<vf::BlockId, BlockBranches>,
    pub always_branches: HashSet<RegionPointer>,
}

impl BranchAnalysis {
    pub fn analyze(region: &vf::Region) -> Self {
        let mut analysis = Self::default();
        analysis.scan_region(region);
        analysis
    }

    fn scan_region(&mut self, region: &vf::Region) {
        match region {
            vf::Region::BasicBlock { instructions } => {
                if let Some(instruction) = instructions.last() {
                    self.scan_instruction(region, instruction);
                }
            }
            vf::Region::Sequence { regions } => {
                for region in regions {
                    self.scan_region(region);
                }
            }
            vf::Region::Block { region, .. } => {
                self.scan_region(region);
            }
            vf::Region::IfElse {
                condition,
                when_true,
                when_false,
                ..
            } => {
                self.scan_region(condition);
                self.scan_region(when_true);
                self.scan_region(when_false);
            }
            vf::Region::Finally { action, ensuring } => {
                self.scan_region(action);
                self.scan_region(ensuring);
            }
        }
    }

    fn scan_instruction(&mut self, region: &vf::Region, instruction: &vf::Instruction) {
        let region_pointer = region as RegionPointer;

        match instruction {
            vf::Instruction::BlockBreak { block_id }
            | vf::Instruction::BlockBreakIf { block_id, .. }
            | vf::Instruction::BlockBreakUnless { block_id, .. } => {
                self.blocks
                    .entry((**block_id).clone())
                    .or_default()
                    .breaks
                    .insert(region_pointer);
            }

            vf::Instruction::IsEnumVariantOrBreak {
                not_variant_block_id,
                ..
            } => {
                self.blocks
                    .entry((**not_variant_block_id).clone())
                    .or_default()
                    .breaks
                    .insert(region_pointer);
            }

            vf::Instruction::BlockRetry { block_id } => {
                self.blocks
                    .entry((**block_id).clone())
                    .or_default()
                    .retries
                    .insert(region_pointer);
            }

            _ => {}
        }
    }
}
