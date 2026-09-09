use alloc::collections::VecDeque;
use hashbrown::HashSet;

use crate::analysis::control_flow::ControlFlowAnalysis;
use crate::analysis::{InstructionPointer, InstructionSet, RegionPointer, RegionSet};
use argon_format_vm::vm as vf;
use argon_vm::analysis::basic_blocks;

pub struct ControlPathAnalysis {
    pub reachable_regions: RegionSet,
    pub current_basic_block_reachable: InstructionSet,
}

impl ControlPathAnalysis {
    pub fn analyze(
        region: &vf::Region,
        instruction_region: RegionPointer,
        instruction: InstructionPointer,
        use_successor: bool,
    ) -> Self {
        let flow = ControlFlowAnalysis::analyze(region);

        let current_basic_block_reachable: InstructionSet = if use_successor {
            let mut found_instruction = false;

            basic_blocks(region)
                .find(|bb| *bb as RegionPointer == instruction_region)
                .and_then(|bb| match bb {
                    vf::Region::BasicBlock { instructions } => Some(instructions),
                    _ => None,
                })
                .into_iter()
                .flat_map(|instructions| instructions.iter())
                .map(|insn| insn.as_ref() as InstructionPointer)
                .filter(|insn| {
                    if found_instruction {
                        true
                    } else {
                        found_instruction = *insn == instruction;
                        false
                    }
                })
                .collect()
        } else {
            basic_blocks(region)
                .find(|bb| *bb as RegionPointer == instruction_region)
                .and_then(|bb| match bb {
                    vf::Region::BasicBlock { instructions } => Some(instructions),
                    _ => None,
                })
                .into_iter()
                .flat_map(|instructions| instructions.iter())
                .map(|insn| insn.as_ref() as InstructionPointer)
                .take_while(|insn| *insn != instruction)
                .collect()
        };

        let mut reachable = HashSet::new();
        let mut queue = VecDeque::new();

        queue.push_back(instruction_region);

        while let Some(insn) = queue.pop_front() {
            let Some(flow_state) = flow.regions.get(&insn) else {
                continue;
            };

            let next = if use_successor {
                &flow_state.successors
            } else {
                &flow_state.predecessors
            };

            for next in next.iter().copied() {
                if reachable.insert(next) {
                    queue.push_back(next);
                }
            }
        }

        Self {
            reachable_regions: reachable,
            current_basic_block_reachable,
        }
    }
}
