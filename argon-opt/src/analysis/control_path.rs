use crate::analysis::reaching_defs::InstructionSet;
use std::collections::{HashSet, VecDeque};

use crate::analysis::control_flow::ControlFlowAnalysis;
use argon_format_vm::vm as vf;

pub struct ControlPathAnalysis {
    pub reachable: InstructionSet,
}

impl ControlPathAnalysis {
    pub fn analyze(
        block: &vf::Block,
        instruction: *const vf::Instruction,
        use_successor: bool,
    ) -> Self {
        let flow = ControlFlowAnalysis::analyze(block);

        let mut reachable = HashSet::new();
        let mut queue = VecDeque::new();

        queue.push_back(instruction);

        while let Some(insn) = queue.pop_front() {
            let Some(flow_state) = flow.instructions.get(&insn) else {
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

        Self { reachable }
    }
}
