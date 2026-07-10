use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

use argon_format_vm::vm as vf;

use super::RegionPointer;
use super::branches::BranchAnalysis;

#[derive(Debug, Default)]
pub struct RegionFlow {
    pub predecessors: HashSet<RegionPointer>,
    pub successors: HashSet<RegionPointer>,
}

#[derive(Debug, Default)]
pub struct ControlFlowAnalysis {
    pub regions: HashMap<RegionPointer, RegionFlow>,
}

impl ControlFlowAnalysis {
    pub fn analyze(region: &vf::Region) -> Self {
        let branches = BranchAnalysis::analyze(region);
        let mut analysis = Self::default();
        analysis.scan_region(region, &HashSet::new(), &HashSet::new(), &branches);
        analysis
    }

    fn scan_region(
        &mut self,
        region: &vf::Region,
        incoming: &HashSet<RegionPointer>,
        always_predecessor_to: &HashSet<RegionPointer>,
        branches: &BranchAnalysis,
    ) -> HashSet<RegionPointer> {
        match region {
            vf::Region::BasicBlock { .. } => {
                let region_pointer = region as RegionPointer;
                self.regions.entry(region_pointer).or_default();

                for incoming_region in incoming {
                    self.add_edge(*incoming_region, region_pointer);
                }

                for other in always_predecessor_to {
                    self.add_edge(region_pointer, *other);
                }

                if branches.always_branches.contains(&region_pointer) {
                    HashSet::new()
                } else {
                    HashSet::from([region_pointer])
                }
            }
            vf::Region::Sequence { regions } => {
                let mut current_blocks = incoming.clone();
                for region in regions {
                    current_blocks =
                        self.scan_region(region, &current_blocks, always_predecessor_to, branches);
                }
                current_blocks
            }
            vf::Region::Block {
                block_id,
                flags,
                region,
            } => {
                let mut current_blocks =
                    self.scan_region(region, incoming, always_predecessor_to, branches);

                if flags.is_loop {
                    self.scan_region(region, &current_blocks, always_predecessor_to, branches);
                    current_blocks.clear();
                }

                if let Some(block_branches) = branches.blocks.get(&block_id) {
                    self.scan_region(
                        region,
                        &block_branches.retries,
                        always_predecessor_to,
                        branches,
                    );
                    current_blocks.extend(&block_branches.breaks);
                }

                current_blocks
            }
            vf::Region::IfElse {
                when_true_block_id,
                when_false_block_id,
                condition,
                when_true,
                when_false,
            } => {
                let mut entering_true_body =
                    self.scan_region(condition, incoming, always_predecessor_to, branches);

                if let Some(block_branches) = branches.blocks.get(&when_true_block_id) {
                    entering_true_body.extend(&block_branches.breaks);
                }

                let mut exiting = self.scan_region(
                    when_true,
                    &entering_true_body,
                    always_predecessor_to,
                    branches,
                );

                let entering_false_body =
                    if let Some(block_branches) = branches.blocks.get(&when_false_block_id) {
                        Cow::Borrowed(&block_branches.breaks)
                    } else {
                        Cow::Owned(HashSet::new())
                    };

                let exiting_false = self.scan_region(
                    when_false,
                    &entering_false_body,
                    always_predecessor_to,
                    branches,
                );

                exiting.extend(exiting_false);

                exiting
            }
            vf::Region::Finally { action, ensuring } => {
                let mut always_predecessor_to = always_predecessor_to.clone();
                always_predecessor_to.insert(ensuring.as_ref() as RegionPointer);

                self.scan_region(action, incoming, &always_predecessor_to, branches);
                self.scan_region(ensuring, incoming, &always_predecessor_to, branches)
            }
        }
    }

    fn add_edge(&mut self, source: RegionPointer, target: RegionPointer) {
        self.regions
            .entry(source)
            .or_default()
            .successors
            .insert(target);
        self.regions
            .entry(target)
            .or_default()
            .predecessors
            .insert(source);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use argon_vm::analysis::basic_blocks;

    #[test]
    fn finally_is_reachable_from_every_action_basic_block() {
        let region = vf::Region::Finally {
            action: Box::new(vf::Region::Sequence {
                regions: vec![
                    Box::new(basic_block(vec![Box::new(vf::Instruction::ConstInt {
                        dest: register(0),
                        value: 1.into(),
                    })])),
                    Box::new(basic_block(vec![Box::new(vf::Instruction::Return {
                        src: register(0),
                    })])),
                ],
            }),
            ensuring: Box::new(basic_block(vec![Box::new(vf::Instruction::ConstInt {
                dest: register(1),
                value: 2.into(),
            })])),
        };
        let vf::Region::Finally { action, ensuring } = &region else {
            unreachable!()
        };
        let ensuring_pointer = ensuring.as_ref() as RegionPointer;

        let analysis = ControlFlowAnalysis::analyze(&region);

        for action_basic_block in basic_blocks(action) {
            let action_pointer = action_basic_block as RegionPointer;
            assert!(
                analysis.regions[&action_pointer]
                    .successors
                    .contains(&ensuring_pointer)
            );
        }
    }

    fn basic_block(instructions: Vec<Box<vf::Instruction>>) -> vf::Region {
        vf::Region::BasicBlock { instructions }
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
