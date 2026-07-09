use std::collections::HashMap;

use argon_format_vm::vm as vf;

use super::branches::InstructionPointer;
use super::control_flow::ControlFlowAnalysis;
use super::use_def::{collect_use_def, VariableSet};

#[derive(Debug, Default)]
pub struct InstructionLiveness {
    pub live_in: VariableSet,
    pub live_out: VariableSet,
}

#[derive(Debug, Default)]
pub struct LivenessAnalysis {
    pub instructions: HashMap<InstructionPointer, InstructionLiveness>,
}

impl LivenessAnalysis {
    pub fn analyze(block: &vf::Block) -> Self {
        let control_flow = ControlFlowAnalysis::analyze(block);
        let mut use_def = HashMap::new();
        collect_use_def(block, &mut use_def);

        let mut analysis = Self {
            instructions: control_flow
                .instructions
                .keys()
                .copied()
                .map(|instruction| (instruction, InstructionLiveness::default()))
                .collect(),
        };

        let mut changed = true;
        while changed {
            changed = false;
            for (&instruction, flow) in &control_flow.instructions {
                let live_out = flow
                    .successors
                    .iter()
                    .filter_map(|successor| analysis.instructions.get(successor))
                    .flat_map(|successor| successor.live_in.iter().cloned())
                    .collect::<VariableSet>();
                let instruction_use_def = &use_def[&instruction];
                let live_in = instruction_use_def
                    .uses
                    .iter()
                    .cloned()
                    .chain(
                        live_out
                            .iter()
                            .filter(|variable| !instruction_use_def.definitions.contains(*variable))
                            .cloned(),
                    )
                    .collect::<VariableSet>();
                let liveness = analysis
                    .instructions
                    .get_mut(&instruction)
                    .expect("control-flow instruction must have liveness data");
                changed |= liveness.live_in != live_in || liveness.live_out != live_out;
                liveness.live_in = live_in;
                liveness.live_out = live_out;
            }
        }
        analysis
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn computes_live_in_and_live_out() {
        let block = vf::Block {
            instructions: vec![
                Box::new(vf::Instruction::ConstInt {
                    dest: register(0),
                    value: 1.into(),
                }),
                Box::new(vf::Instruction::Move {
                    dest: register(1),
                    src: register(0),
                }),
                Box::new(vf::Instruction::Return { src: register(1) }),
            ]
            .into(),
        };
        let pointers = block
            .instructions
            .iter()
            .map(|instruction| instruction.as_ref() as InstructionPointer)
            .collect::<Vec<_>>();
        let analysis = LivenessAnalysis::analyze(&block);
        assert_eq!(analysis.instructions[&pointers[0]].live_in, variables([]));
        assert_eq!(analysis.instructions[&pointers[0]].live_out, variables([0]));
        assert_eq!(analysis.instructions[&pointers[1]].live_in, variables([0]));
        assert_eq!(analysis.instructions[&pointers[1]].live_out, variables([1]));
        assert_eq!(analysis.instructions[&pointers[2]].live_in, variables([1]));
        assert_eq!(analysis.instructions[&pointers[2]].live_out, variables([]));
    }

    #[test]
    fn loop_block_fallthrough_keeps_values_live_for_next_iteration() {
        let block = vf::Block {
            instructions: vec![Box::new(vf::Instruction::Block {
                block_id: Box::new(vf::BlockId { id: 0_u32.into() }),
                flags: vf::BlockFlags {
                    has_break: false,
                    has_retry: false,
                    is_loop: true,
                },
                body: Box::new(vf::Block {
                    instructions: vec![
                        Box::new(vf::Instruction::Move {
                            dest: register(0),
                            src: register(1),
                        }),
                        Box::new(vf::Instruction::ConstInt {
                            dest: register(1),
                            value: 1.into(),
                        }),
                    ]
                    .into(),
                }),
            })]
            .into(),
        };
        let vf::Instruction::Block { body, .. } = block.instructions[0].as_ref() else {
            unreachable!()
        };
        let loop_body_pointers = body
            .instructions
            .iter()
            .map(|instruction| instruction.as_ref() as InstructionPointer)
            .collect::<Vec<_>>();

        let analysis = LivenessAnalysis::analyze(&block);

        assert_eq!(
            analysis.instructions[&loop_body_pointers[1]].live_out,
            variables([1])
        );
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }

    fn variables(ids: impl IntoIterator<Item = u32>) -> VariableSet {
        ids.into_iter()
            .map(|id| vf::RegisterId { id: id.into() })
            .collect()
    }
}
