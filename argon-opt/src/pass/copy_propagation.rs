use argon_format_vm::vm as vf;
use std::collections::HashMap;

use super::OptimizationPass;
use crate::analysis::branches::InstructionPointer;
use crate::analysis::control_path::ControlPathAnalysis;
use crate::analysis::reaching_defs::ReachingDefinitionsAnalysis;
use crate::analysis::use_def::{build_use_def, InstructionUseDef};
use crate::mutator::{instruction_registers_mut, InstructionRegisterType};
use crate::optimizer::OptimizationState;
use argon_vm::analysis::{instruction_foreach_mut, instructions};

struct CopyDefinition {
    copied_from: vf::RegisterId,
    copy_instruction: InstructionPointer,
    copy_flow_path: ControlPathAnalysis,
}

#[derive(Debug, Default)]
pub struct CopyPropagation;

pub static COPY_PROPAGATION: CopyPropagation = CopyPropagation;

impl OptimizationPass for CopyPropagation {
    fn max_iterations(&self) -> Option<usize> {
        None
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        let mut copy_definitions: HashMap<vf::RegisterId, Vec<CopyDefinition>> = HashMap::new();
        let use_def = build_use_def(&function.block);
        let reaching_defs = ReachingDefinitionsAnalysis::analyze(&function.block);

        for instruction in instructions(&function.block) {
            match instruction {
                vf::Instruction::Move { dest, src } => {
                    let flow_path = ControlPathAnalysis::analyze(
                        &function.block,
                        instruction as *const vf::Instruction,
                        true,
                    );

                    copy_definitions
                        .entry((**dest).clone())
                        .or_default()
                        .push(CopyDefinition {
                            copied_from: (**src).clone(),
                            copy_instruction: instruction as *const vf::Instruction,
                            copy_flow_path: flow_path,
                        });
                }

                _ => {}
            }
        }

        let mut propagations = HashMap::new();

        for instruction in instructions(&function.block) {
            let Some(instruction_use_def) = use_def.get(&(instruction as *const vf::Instruction))
            else {
                return;
            };

            let mut replacements = HashMap::new();

            let mut in_flow_path_cell = None;

            for use_reg in &instruction_use_def.uses {
                for definition in copy_definitions
                    .get(use_reg)
                    .iter()
                    .flat_map(|definitions| definitions.iter())
                {
                    let in_flow_path = in_flow_path_cell.get_or_insert_with(|| {
                        ControlPathAnalysis::analyze(
                            &function.block,
                            instruction as *const vf::Instruction,
                            false,
                        )
                    });

                    if is_safe_propagation(
                        instruction,
                        use_reg,
                        function,
                        definition,
                        &use_def,
                        &reaching_defs,
                        in_flow_path,
                    ) {
                        replacements.insert(use_reg, &definition.copied_from);
                    }
                }
            }

            if !replacements.is_empty() {
                propagations.insert(instruction as *const vf::Instruction, replacements);
            }
        }

        if !propagations.is_empty() {
            state.mark_changed();

            instruction_foreach_mut(&mut function.block, |instruction| {
                let Some(prop) = propagations.get(&(instruction as *const vf::Instruction)) else {
                    return;
                };

                instruction_registers_mut(instruction, InstructionRegisterType::Use, |r| {
                    if let Some(new_reg) = prop.get(r) {
                        *r = (*new_reg).clone();
                    }
                })
            });
        }
    }
}

fn is_safe_propagation(
    instruction: &vf::Instruction,
    use_reg: &vf::RegisterId,
    function: &vf::FunctionBody,
    definition: &CopyDefinition,
    use_def: &HashMap<InstructionPointer, InstructionUseDef>,
    reaching_defs: &ReachingDefinitionsAnalysis,
    in_flow_path: &ControlPathAnalysis,
) -> bool {
    let Some(found_uses) = reaching_defs.reachable_uses(definition.copy_instruction, use_reg)
    else {
        return false;
    };

    if found_uses.len() != 1 || !found_uses.contains(&(instruction as *const vf::Instruction)) {
        return false;
    }

    !instructions(&function.block).any(|insn| {
        definition
            .copy_flow_path
            .reachable
            .contains(&(insn as *const vf::Instruction))
            && in_flow_path
                .reachable
                .contains(&(insn as *const vf::Instruction))
            && use_def
                .get(&(insn as *const vf::Instruction))
                .is_none_or(|insn_use_def| {
                    insn_use_def.definitions.contains(use_reg)
                        || insn_use_def.uses.contains(&definition.copied_from)
                })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn propagates_copy_into_use() {
        let mut function = vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: vec![declaration(), declaration()],
            }),
            block: Box::new(vf::Block {
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
                ],
            }),
        };
        let mut state = OptimizationState::new(&[]);

        COPY_PROPAGATION.optimize(&mut state, &mut function);

        assert!(state.changed());
        let vf::Instruction::Return { src } = function.block.instructions[2].as_ref() else {
            unreachable!()
        };
        assert_eq!(src.id, 0_u32.into());
    }

    fn declaration() -> Box<vf::VariableDeclaration> {
        Box::new(vf::VariableDeclaration {
            r#type: Box::new(vf::Token::Boxed {}),
        })
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
