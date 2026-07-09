use std::collections::{HashMap, HashSet};

use argon_format_vm::vm as vf;

use super::branches::InstructionPointer;
use super::control_flow::ControlFlowAnalysis;
use super::use_def::{collect_use_def, InstructionUseDef};

pub type RegisterDefinition = (InstructionPointer, vf::RegisterId);
pub type InstructionSet = HashSet<InstructionPointer>;

#[derive(Debug, Default)]
pub struct ReachingDefinitionsAnalysis {
    definitions: HashMap<RegisterDefinition, InstructionSet>,
}

impl ReachingDefinitionsAnalysis {
    pub fn analyze(block: &vf::Block) -> Self {
        let control_flow = ControlFlowAnalysis::analyze(block);
        let mut use_def = HashMap::new();
        collect_use_def(block, &mut use_def);
        let mut analysis = Self::default();

        for (&instruction, instruction_use_def) in &use_def {
            for register in &instruction_use_def.definitions {
                let uses = reachable_uses(&control_flow, &use_def, instruction, register);
                analysis
                    .definitions
                    .insert((instruction, register.clone()), uses);
            }
        }

        analysis
    }

    #[must_use]
    pub fn reachable_uses(
        &self,
        definition: InstructionPointer,
        register: &vf::RegisterId,
    ) -> Option<&InstructionSet> {
        self.definitions.get(&(definition, register.clone()))
    }
}

fn reachable_uses(
    control_flow: &ControlFlowAnalysis,
    use_def: &HashMap<InstructionPointer, InstructionUseDef>,
    definition: InstructionPointer,
    register: &vf::RegisterId,
) -> InstructionSet {
    let mut uses = InstructionSet::new();
    let mut visited = InstructionSet::new();
    let mut pending = control_flow.instructions[&definition]
        .successors
        .iter()
        .copied()
        .collect::<Vec<_>>();

    while let Some(instruction) = pending.pop() {
        if !visited.insert(instruction) {
            continue;
        }

        let instruction_use_def = &use_def[&instruction];
        if instruction_use_def.uses.contains(register) {
            uses.insert(instruction);
        }

        if !instruction_use_def.definitions.contains(register) {
            pending.extend(
                control_flow.instructions[&instruction]
                    .successors
                    .iter()
                    .copied(),
            );
        }
    }

    uses
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_uses_until_the_register_is_redefined() {
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
                Box::new(vf::Instruction::ConstInt {
                    dest: register(0),
                    value: 2.into(),
                }),
                Box::new(vf::Instruction::Return { src: register(0) }),
            ]
            .into(),
        };
        let pointers = block
            .instructions
            .iter()
            .map(|instruction| instruction.as_ref() as InstructionPointer)
            .collect::<Vec<_>>();

        let analysis = ReachingDefinitionsAnalysis::analyze(&block);
        let queried_register = vf::RegisterId { id: 0_u32.into() };

        assert_eq!(
            analysis.reachable_uses(pointers[0], &queried_register),
            Some(&instructions([pointers[1]]))
        );
        assert_eq!(
            analysis.reachable_uses(pointers[2], &queried_register),
            Some(&instructions([pointers[3]]))
        );
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }

    fn instructions(instructions: impl IntoIterator<Item = InstructionPointer>) -> InstructionSet {
        instructions.into_iter().collect()
    }
}
