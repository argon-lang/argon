use argon_format_vm::vm as vf;
use argon_vm::analysis::basic_blocks_foreach_mut;

use super::OptimizationPass;
use crate::analysis::liveness::LivenessAnalysis;
use crate::analysis::purity::is_instruction_pure;
use crate::analysis::use_def::UseDefAnalysis;
use crate::analysis::{InstructionPointer, VariableSet};
use crate::optimizer::OptimizationState;

#[derive(Debug, Default)]
pub struct DeadCodeElimination;

pub static DEAD_CODE_ELIMINATION: DeadCodeElimination = DeadCodeElimination;

impl OptimizationPass for DeadCodeElimination {
    fn name(&self) -> &'static str {
        "dead-code-elimination"
    }

    fn max_iterations(&self) -> Option<usize> {
        None
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        remove_dead_instructions(state, &mut function.region);
    }
}

fn remove_dead_instructions(state: &mut OptimizationState, region: &mut vf::Region) {
    let liveness = LivenessAnalysis::analyze(region);
    let use_def = UseDefAnalysis::analyze(region);
    remove_dead_from_block(state, region, &liveness, &use_def);
}

fn remove_dead_from_block(
    state: &mut OptimizationState,
    region: &mut vf::Region,
    liveness: &LivenessAnalysis,
    use_def: &UseDefAnalysis,
) {
    basic_blocks_foreach_mut(region, |bb| {
        let vf::Region::BasicBlock { instructions } = bb else {
            return;
        };

        instructions.retain_mut(|instruction| {
            let instruction_pointer = instruction.as_ref() as InstructionPointer;
            let definitions = &use_def.instructions[&instruction_pointer].definitions;
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
        })
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
        let mut region = basic_block(vec![constant(0)]);
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut region);
        assert!(state.changed());
        assert!(basic_block_instructions(&region).is_empty());
    }

    #[test]
    fn retains_instruction_with_a_live_definition() {
        let mut region = basic_block(vec![
            constant(0),
            Box::new(vf::Instruction::Return { src: register(0) }),
        ]);
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut region);
        assert!(!state.changed());
        assert_eq!(basic_block_instructions(&region).len(), 2);
    }

    #[test]
    fn discards_an_unused_function_result() {
        let mut region = basic_block(vec![function_call(0)]);
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut region);

        assert!(state.changed());
        let vf::Instruction::FunctionObjectErasedCall { dest, .. } =
            basic_block_instructions(&region)[0].as_ref()
        else {
            unreachable!()
        };
        assert!(matches!(dest.as_ref(), vf::FunctionResult::Discard {}));
    }

    #[test]
    fn retains_a_live_function_result() {
        let mut region = basic_block(vec![
            function_call(0),
            Box::new(vf::Instruction::Return { src: register(0) }),
        ]);
        let mut state = OptimizationState::new(&[]);

        remove_dead_instructions(&mut state, &mut region);

        assert!(!state.changed());
        let vf::Instruction::FunctionObjectErasedCall { dest, .. } =
            basic_block_instructions(&region)[0].as_ref()
        else {
            unreachable!()
        };
        assert!(matches!(dest.as_ref(), vf::FunctionResult::Register { .. }));
    }

    fn basic_block(instructions: Vec<Box<vf::Instruction>>) -> vf::Region {
        vf::Region::BasicBlock { instructions }
    }

    fn basic_block_instructions(region: &vf::Region) -> &[Box<vf::Instruction>] {
        let vf::Region::BasicBlock { instructions } = region else {
            unreachable!()
        };
        instructions
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
