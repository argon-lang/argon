use hashbrown::{HashMap, HashSet};

use super::control_flow::ControlFlowAnalysis;
use super::{InstructionPointer, RegionPointer, VariableSet};
use crate::analysis::use_def::UseDefAnalysis;
use argon_format_vm::vm as vf;
use argon_vm::analysis::basic_blocks;

pub type RegisterDefinition = (InstructionPointer, vf::RegisterId);

#[derive(Debug)]
pub struct ReachingDefinitions {
    pub reach_in: HashSet<RegisterDefinition>,

    #[allow(unused)]
    pub reach_out: HashSet<RegisterDefinition>,
}

#[derive(Debug, Default)]
pub struct ReachingDefinitionsAnalysis {
    #[allow(unused)]
    definitions: HashMap<RegionPointer, ReachingDefinitions>,
    uses: HashMap<RegisterDefinition, HashSet<InstructionPointer>>,
}

struct GenKill {
    pub gen_: HashSet<RegisterDefinition>,
    pub kill: VariableSet,
}

impl ReachingDefinitionsAnalysis {
    pub fn analyze(region: &vf::Region) -> Self {
        let control_flow = ControlFlowAnalysis::analyze(region);
        let use_def = UseDefAnalysis::analyze(region);

        // Compute gen and kill sets for each basic block.
        let mut genkill = HashMap::new();
        for bb in basic_blocks(region) {
            let vf::Region::BasicBlock { instructions } = bb else {
                continue;
            };
            let region_pointer = bb as RegionPointer;

            let mut gen_: HashSet<RegisterDefinition> = HashSet::new();
            let mut kill = VariableSet::new();

            for instruction in instructions {
                let instruction_use_def =
                    &use_def.instructions[&(instruction.as_ref() as InstructionPointer)];

                gen_.extend(
                    instruction_use_def
                        .definitions
                        .iter()
                        .cloned()
                        .filter(|variable| !kill.contains(variable))
                        .map(|variable| (instruction.as_ref() as InstructionPointer, variable)),
                );
                kill.extend(instruction_use_def.uses.iter().cloned());
            }

            genkill.insert(region_pointer, GenKill { gen_, kill });
        }

        let mut reaching_in: HashMap<RegionPointer, HashSet<RegisterDefinition>> = HashMap::new();
        let mut reaching_out: HashMap<RegionPointer, HashSet<RegisterDefinition>> = HashMap::new();

        for (region, GenKill { gen_, kill: _ }) in &genkill {
            reaching_out.insert(*region, gen_.clone());
        }

        let mut changed = true;
        while changed {
            changed = false;

            for (region, GenKill { gen_: _, kill }) in &genkill {
                let region = *region;

                let reach_in = reaching_in.entry(region).or_default();
                let old_in_len = reach_in.len();
                reach_in.extend(
                    control_flow.regions[&region]
                        .successors
                        .iter()
                        .copied()
                        .flat_map(|succ| reaching_out.get(&succ))
                        .flat_map(|v| v.iter())
                        .cloned(),
                );
                if old_in_len != reach_in.len() {
                    changed = true;
                }

                let reach_out = reaching_out.entry(region).or_default();
                let old_out_len = reach_out.len();
                reach_out.extend(reach_in.iter().cloned().filter(|(_, v)| !kill.contains(v)));
                if old_out_len != reach_out.len() {
                    changed = true;
                }
            }
        }

        let mut definitions = HashMap::new();
        for (region, reach_in) in reaching_in {
            let reach_out = reaching_out.remove(&region).unwrap_or_default();
            definitions.insert(
                region,
                ReachingDefinitions {
                    reach_in,
                    reach_out,
                },
            );
        }

        let mut uses: HashMap<RegisterDefinition, HashSet<InstructionPointer>> = HashMap::new();
        for bb in basic_blocks(region) {
            let vf::Region::BasicBlock { instructions } = bb else {
                continue;
            };
            let region_pointer = bb as RegionPointer;

            let mut reaching = definitions[&region_pointer].reach_in.clone();

            for instruction in instructions {
                let instruction_pointer = instruction.as_ref() as InstructionPointer;

                let instruction_use_def = &use_def.instructions[&instruction_pointer];

                for (def_insn, def_v) in &reaching {
                    if !instruction_use_def.uses.contains(def_v) {
                        continue;
                    }

                    uses.entry((*def_insn, def_v.clone()))
                        .or_default()
                        .insert(instruction_pointer);
                }

                reaching.retain(|(_, v)| !instruction_use_def.definitions.contains(v));
                reaching.extend(
                    instruction_use_def
                        .definitions
                        .iter()
                        .cloned()
                        .map(|variable| (instruction_pointer, variable)),
                );
            }
        }

        ReachingDefinitionsAnalysis { definitions, uses }
    }

    pub fn reachable_uses(
        &self,
        instruction: InstructionPointer,
        reg: &vf::RegisterId,
    ) -> Option<&HashSet<InstructionPointer>> {
        self.uses.get(&(instruction, reg.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::InstructionSet;
    use alloc::vec;

    #[test]
    fn finds_uses_until_the_register_is_redefined() {
        let region = basic_block(vec![
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
        ]);
        let vf::Region::BasicBlock { instructions } = &region else {
            unreachable!()
        };
        let pointers = instructions
            .iter()
            .map(|instruction| instruction.as_ref() as InstructionPointer)
            .collect::<Vec<_>>();

        let analysis = ReachingDefinitionsAnalysis::analyze(&region);
        let queried_register = vf::RegisterId { id: 0_u32.into() };

        assert_eq!(
            analysis.reachable_uses(pointers[0], &queried_register),
            Some(&instruction_set([pointers[1]]))
        );
        assert_eq!(
            analysis.reachable_uses(pointers[2], &queried_register),
            Some(&instruction_set([pointers[3]]))
        );
    }

    fn basic_block(instructions: Vec<Box<vf::Instruction>>) -> vf::Region {
        vf::Region::BasicBlock { instructions }
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }

    fn instruction_set(
        instructions: impl IntoIterator<Item = InstructionPointer>,
    ) -> InstructionSet {
        instructions.into_iter().collect()
    }
}
#[cfg(test)]
use alloc::{boxed::Box, vec::Vec};
