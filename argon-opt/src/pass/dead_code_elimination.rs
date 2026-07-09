use std::collections::HashMap;

use argon_format_vm::vm as vf;
use argon_vm::analysis::instruction_retain;

use super::OptimizationPass;
use crate::analysis::branches::InstructionPointer;
use crate::analysis::liveness::LivenessAnalysis;
use crate::analysis::purity::is_instruction_pure;
use crate::analysis::use_def::{build_use_def, InstructionUseDef, VariableSet};
use crate::optimizer::OptimizationState;

#[derive(Debug, Default)]
pub struct DeadCodeElimination;

pub static DEAD_CODE_ELIMINATION: DeadCodeElimination = DeadCodeElimination;

impl OptimizationPass for DeadCodeElimination {
    fn max_iterations(&self) -> Option<usize> {
        None
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        remove_dead_instructions(state, &mut function.block);
    }
}

fn remove_dead_instructions(state: &mut OptimizationState, block: &mut vf::Block) {
    let liveness = LivenessAnalysis::analyze(block);
    let use_def = build_use_def(block);
    remove_dead_from_block(state, block, &liveness, &use_def);
}

fn remove_dead_from_block(
    state: &mut OptimizationState,
    block: &mut vf::Block,
    liveness: &LivenessAnalysis,
    use_def: &HashMap<InstructionPointer, InstructionUseDef>,
) {
    instruction_retain(block, |instruction| {
        let instruction_pointer = instruction as InstructionPointer;
        let definitions = &use_def[&instruction_pointer].definitions;
        let live_out = &liveness.instructions[&instruction_pointer].live_out;

        if !definitions.is_disjoint(live_out) {
            return true;
        }

        discard_unused_function_result(state, instruction, live_out);
        let keep = !is_instruction_pure(instruction);
        if !keep {
            state.mark_changed();
        }
        keep
    });
}

fn discard_unused_function_result(
    state: &mut OptimizationState,
    instruction: &mut vf::Instruction,
    live_out: &VariableSet,
) {
    let result = match instruction {
        vf::Instruction::FunctionCall { dest, .. }
        | vf::Instruction::FunctionObjectCall { dest, .. }
        | vf::Instruction::FunctionObjectTokenCall { dest, .. }
        | vf::Instruction::FunctionObjectErasedCall { dest, .. }
        | vf::Instruction::InstanceMethodCall { dest, .. } => dest,
        _ => return,
    };

    let vf::FunctionResult::Register { id } = result.as_ref() else {
        return;
    };
    if !live_out.contains(id.as_ref()) {
        **result = vf::FunctionResult::Discard {};
        state.mark_changed();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn removes_pure_instruction_with_no_live_definitions() {
        let mut block = vf::Block {
            instructions: vec![constant(0)].into(),
        };
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut block);
        assert!(state.changed());
        assert!(block.instructions.is_empty());
    }

    #[test]
    fn retains_instruction_with_a_live_definition() {
        let mut block = vf::Block {
            instructions: vec![
                constant(0),
                Box::new(vf::Instruction::Return { src: register(0) }),
            ]
            .into(),
        };
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut block);
        assert!(!state.changed());
        assert_eq!(block.instructions.len(), 2);
    }

    #[test]
    fn discards_an_unused_function_result() {
        let mut block = vf::Block {
            instructions: vec![function_call(0)].into(),
        };
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut block);

        assert!(state.changed());
        let vf::Instruction::FunctionObjectErasedCall { dest, .. } = block.instructions[0].as_ref()
        else {
            unreachable!()
        };
        assert!(matches!(dest.as_ref(), vf::FunctionResult::Discard {}));
    }

    #[test]
    fn retains_a_live_function_result() {
        let mut block = vf::Block {
            instructions: vec![
                function_call(0),
                Box::new(vf::Instruction::Return { src: register(0) }),
            ]
            .into(),
        };
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut block);

        assert!(!state.changed());
        let vf::Instruction::FunctionObjectErasedCall { dest, .. } = block.instructions[0].as_ref()
        else {
            unreachable!()
        };
        assert!(matches!(dest.as_ref(), vf::FunctionResult::Register { .. }));
    }

    fn function_call(dest: u32) -> Box<vf::Instruction> {
        Box::new(vf::Instruction::FunctionObjectErasedCall {
            dest: Box::new(vf::FunctionResult::Register { id: register(dest) }),
            function: register(100),
        })
    }

    fn constant(id: u32) -> Box<vf::Instruction> {
        Box::new(vf::Instruction::ConstInt {
            dest: register(id),
            value: 1.into(),
        })
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
