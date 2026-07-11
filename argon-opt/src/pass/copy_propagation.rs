use argon_format_vm::vm as vf;
use std::collections::HashMap;

use super::OptimizationPass;
use crate::analysis::control_path::ControlPathAnalysis;
use crate::analysis::reaching_defs::ReachingDefinitionsAnalysis;
use crate::analysis::use_def::UseDefAnalysis;
use crate::analysis::{InstructionPointer, RegionPointer};
use crate::mutator::{InstructionRegisterType, instruction_registers_mut};
use crate::optimizer::OptimizationState;
use argon_vm::analysis::{basic_blocks, basic_blocks_foreach_mut};

struct CopyDefinition {
    copied_from: vf::RegisterId,
    copy_basic_block: RegionPointer,
    copy_instruction: InstructionPointer,
    copy_flow_path: ControlPathAnalysis,
}

#[derive(Debug, Default)]
pub struct CopyPropagation;

pub static COPY_PROPAGATION: CopyPropagation = CopyPropagation;

impl OptimizationPass for CopyPropagation {
    fn name(&self) -> &'static str {
        "copy-propagation"
    }

    fn max_iterations(&self) -> Option<usize> {
        None
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        let mut copy_definitions: HashMap<vf::RegisterId, Vec<CopyDefinition>> = HashMap::new();
        let use_def = UseDefAnalysis::analyze(&function.region);
        let reaching_defs = ReachingDefinitionsAnalysis::analyze(&function.region);

        for bb in basic_blocks(&function.region) {
            let vf::Region::BasicBlock { instructions } = bb else {
                continue;
            };
            let region_pointer = bb as RegionPointer;

            for instruction in instructions {
                match instruction.as_ref() {
                    vf::Instruction::Move { dest, src } => {
                        let flow_path = ControlPathAnalysis::analyze(
                            &function.region,
                            region_pointer,
                            instruction.as_ref() as InstructionPointer,
                            true,
                        );

                        copy_definitions.entry((**dest).clone()).or_default().push(
                            CopyDefinition {
                                copied_from: (**src).clone(),
                                copy_basic_block: region_pointer,
                                copy_instruction: instruction.as_ref() as InstructionPointer,
                                copy_flow_path: flow_path,
                            },
                        );
                    }

                    _ => {}
                }
            }
        }

        let mut propagations: HashMap<
            RegionPointer,
            HashMap<InstructionPointer, HashMap<&vf::RegisterId, &vf::RegisterId>>,
        > = HashMap::new();

        for bb in basic_blocks(&function.region) {
            let vf::Region::BasicBlock { instructions } = bb else {
                continue;
            };
            let region_pointer = bb as RegionPointer;

            for instruction in instructions {
                let instruction_pointer = instruction.as_ref() as InstructionPointer;

                let instruction_use_def = &use_def.instructions[&instruction_pointer];

                let mut replacements: HashMap<
                    InstructionPointer,
                    HashMap<&vf::RegisterId, &vf::RegisterId>,
                > = HashMap::new();

                let mut in_flow_path_cell = None;

                for use_reg in &instruction_use_def.uses {
                    for definition in copy_definitions
                        .get(use_reg)
                        .iter()
                        .flat_map(|definitions| definitions.iter())
                    {
                        let in_flow_path = in_flow_path_cell.get_or_insert_with(|| {
                            ControlPathAnalysis::analyze(
                                &function.region,
                                region_pointer,
                                instruction_pointer,
                                false,
                            )
                        });

                        if is_safe_propagation(
                            region_pointer,
                            instruction,
                            use_reg,
                            function,
                            definition,
                            &use_def,
                            &reaching_defs,
                            in_flow_path,
                        ) {
                            replacements
                                .entry(instruction.as_ref() as InstructionPointer)
                                .or_default()
                                .insert(use_reg, &definition.copied_from);
                        }
                    }
                }

                if !replacements.is_empty() {
                    propagations.insert(region_pointer, replacements);
                }
            }
        }

        if !propagations.is_empty() {
            state.mark_changed();

            basic_blocks_foreach_mut(&mut function.region, |bb| {
                let region_pointer = bb as RegionPointer;
                let vf::Region::BasicBlock { instructions } = bb else {
                    return;
                };

                let Some(bb_propagations) = propagations.get(&region_pointer) else {
                    return;
                };

                for instruction in instructions {
                    let Some(prop) =
                        bb_propagations.get(&(instruction.as_ref() as InstructionPointer))
                    else {
                        continue;
                    };

                    instruction_registers_mut(instruction, InstructionRegisterType::Use, |r| {
                        if let Some(new_reg) = prop.get(r) {
                            *r = (*new_reg).clone();
                        }
                    })
                }
            });
        }
    }
}

fn is_safe_propagation(
    instruction_region: RegionPointer,
    instruction: &vf::Instruction,
    use_reg: &vf::RegisterId,
    function: &vf::FunctionBody,
    definition: &CopyDefinition,
    use_def: &UseDefAnalysis,
    reaching_defs: &ReachingDefinitionsAnalysis,
    in_flow_path: &ControlPathAnalysis,
) -> bool {
    let Some(found_uses) = reaching_defs.reachable_uses(definition.copy_instruction, use_reg)
    else {
        return false;
    };

    if found_uses.len() != 1 || !found_uses.contains(&(instruction as InstructionPointer)) {
        return false;
    }

    !basic_blocks(&function.region).any(|bb| {
        let vf::Region::BasicBlock { instructions } = bb else {
            return false;
        };
        let bb_region = bb as RegionPointer;

        if (!definition
            .copy_flow_path
            .reachable_regions
            .contains(&bb_region)
            && bb_region != definition.copy_basic_block)
            || (!in_flow_path.reachable_regions.contains(&bb_region)
                && bb_region != instruction_region)
        {
            return false;
        }

        instructions.iter().any(|insn| {
            let insn_pointer = insn.as_ref() as InstructionPointer;

            (definition
                .copy_flow_path
                .reachable_regions
                .contains(&bb_region)
                || definition
                    .copy_flow_path
                    .current_basic_block_reachable
                    .contains(&insn_pointer))
                && (in_flow_path.reachable_regions.contains(&bb_region)
                    || in_flow_path
                        .current_basic_block_reachable
                        .contains(&insn_pointer))
                && use_def
                    .instructions
                    .get(&insn_pointer)
                    .is_none_or(|insn_use_def| {
                        insn_use_def.definitions.contains(use_reg)
                            || insn_use_def.definitions.contains(&definition.copied_from)
                    })
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
            region: Box::new(basic_block(vec![
                Box::new(vf::Instruction::ConstInt {
                    dest: register(0),
                    value: 1.into(),
                }),
                Box::new(vf::Instruction::Move {
                    dest: register(1),
                    src: register(0),
                }),
                Box::new(vf::Instruction::Return { src: register(1) }),
            ])),
        };
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        COPY_PROPAGATION.optimize(&mut state, &mut function);

        assert!(state.changed());
        let vf::Instruction::Return { src } =
            basic_block_instructions(&function.region)[2].as_ref()
        else {
            unreachable!()
        };
        assert_eq!(src.id, 0_u32.into());
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

    fn declaration() -> Box<vf::VariableDeclaration> {
        Box::new(vf::VariableDeclaration {
            r#type: Box::new(vf::Token::Boxed {}),
        })
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
