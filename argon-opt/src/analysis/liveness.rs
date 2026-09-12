use super::control_flow::ControlFlowAnalysis;
use super::use_def::UseDefAnalysis;
use super::{InstructionPointer, RegionPointer, VariableSet};
use argon_format_vm::vm as vf;
use argon_vm::analysis::basic_blocks;
use hashbrown::HashMap;

#[derive(Debug, Default)]
pub struct Liveness {
    #[allow(unused)]
    pub live_in: VariableSet,
    pub live_out: VariableSet,
}

#[derive(Debug, Default)]
pub struct LivenessAnalysis {
    #[allow(unused)]
    pub basic_blocks: HashMap<RegionPointer, Liveness>,
    pub instructions: HashMap<InstructionPointer, Liveness>,
}

struct GenKill {
    pub gen_: VariableSet,
    pub kill: VariableSet,
}

impl LivenessAnalysis {
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

            let mut gen_ = VariableSet::new();
            let mut kill = VariableSet::new();

            for instruction in instructions {
                let instruction_use_def =
                    &use_def.instructions[&(instruction.as_ref() as InstructionPointer)];

                gen_.extend(
                    instruction_use_def
                        .uses
                        .iter()
                        .cloned()
                        .filter(|variable| !kill.contains(variable)),
                );
                kill.extend(instruction_use_def.definitions.iter().cloned());
            }

            genkill.insert(region_pointer, GenKill { gen_, kill });
        }

        // Compute liveness for each basic block.
        let mut liveness_in: HashMap<RegionPointer, VariableSet> = HashMap::new();
        let mut liveness_out: HashMap<RegionPointer, VariableSet> = HashMap::new();

        for (region, GenKill { gen_, kill: _ }) in &genkill {
            liveness_in.insert(*region, gen_.clone());
        }

        let mut changed = true;
        while changed {
            changed = false;

            for (region, GenKill { gen_, kill }) in &genkill {
                let region = *region;

                let live_out = liveness_out.entry(region).or_default();
                let old_out_len = live_out.len();
                live_out.extend(
                    control_flow.regions[&region]
                        .successors
                        .iter()
                        .copied()
                        .flat_map(|succ| liveness_in.get(&succ))
                        .flat_map(|v| v.iter())
                        .cloned(),
                );
                if old_out_len != live_out.len() {
                    changed = true;
                }

                let live_in = liveness_in.entry(region).or_insert_with(|| gen_.clone());
                let old_in_len = live_in.len();
                live_in.extend(live_out.iter().cloned().filter(|v| !kill.contains(v)));
                if old_in_len != live_in.len() {
                    changed = true;
                }
            }
        }

        // Combine liveness sets into a single map.
        let mut liveness = HashMap::new();
        for (region, live_in) in liveness_in {
            let live_out = liveness_out.remove(&region).unwrap_or_default();
            liveness.insert(region, Liveness { live_in, live_out });
        }

        // Compute liveness for each instruction.
        let mut liveness_insn = HashMap::new();
        for bb in basic_blocks(region) {
            let vf::Region::BasicBlock { instructions } = bb else {
                continue;
            };

            let mut live = liveness[&(bb as RegionPointer)].live_out.clone();
            for instruction in instructions.iter().rev() {
                let instruction_pointer = instruction.as_ref() as InstructionPointer;
                let instruction_use_def = &use_def.instructions[&instruction_pointer];

                let live_out = live;
                live = live_out
                    .difference(&instruction_use_def.definitions)
                    .cloned()
                    .collect();
                live.extend(instruction_use_def.uses.iter().cloned());

                let live_in = live.clone();

                liveness_insn.insert(instruction_pointer, Liveness { live_in, live_out });
            }
        }

        LivenessAnalysis {
            basic_blocks: liveness,
            instructions: liveness_insn,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn computes_live_in_and_live_out() {
        let region = basic_block(vec![
            Box::new(vf::Instruction::ConstInt {
                dest: register(0),
                value: 1.into(),
            }),
            Box::new(vf::Instruction::Move {
                dest: register(1),
                src: register(0),
            }),
            Box::new(vf::Instruction::Return { src: register(1) }),
        ]);
        let vf::Region::BasicBlock { instructions } = &region else {
            unreachable!()
        };
        let pointers = instructions
            .iter()
            .map(|instruction| instruction.as_ref() as InstructionPointer)
            .collect::<Vec<_>>();
        let analysis = LivenessAnalysis::analyze(&region);
        assert_eq!(analysis.instructions[&pointers[0]].live_in, variables([]));
        assert_eq!(analysis.instructions[&pointers[0]].live_out, variables([0]));
        assert_eq!(analysis.instructions[&pointers[1]].live_in, variables([0]));
        assert_eq!(analysis.instructions[&pointers[1]].live_out, variables([1]));
        assert_eq!(analysis.instructions[&pointers[2]].live_in, variables([1]));
        assert_eq!(analysis.instructions[&pointers[2]].live_out, variables([]));
    }

    #[test]
    fn loop_block_fallthrough_keeps_values_live_for_next_iteration() {
        let region = vf::Region::Block {
            block_id: Box::new(vf::BlockId { id: 0_u32.into() }),
            flags: vf::BlockFlags {
                has_break: false,
                has_retry: false,
                is_loop: true,
            },
            region: Box::new(basic_block(vec![
                Box::new(vf::Instruction::Move {
                    dest: register(0),
                    src: register(1),
                }),
                Box::new(vf::Instruction::ConstInt {
                    dest: register(1),
                    value: 1.into(),
                }),
            ])),
        };
        let vf::Region::Block {
            region: loop_body, ..
        } = &region
        else {
            unreachable!()
        };
        let vf::Region::BasicBlock { instructions } = loop_body.as_ref() else {
            unreachable!()
        };
        let loop_body_pointers = instructions
            .iter()
            .map(|instruction| instruction.as_ref() as InstructionPointer)
            .collect::<Vec<_>>();

        let analysis = LivenessAnalysis::analyze(&region);

        assert_eq!(
            analysis.instructions[&loop_body_pointers[1]].live_out,
            variables([1])
        );
    }

    fn basic_block(instructions: Vec<Box<vf::Instruction>>) -> vf::Region {
        vf::Region::BasicBlock { instructions }
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
#[cfg(test)]
use alloc::{boxed::Box, vec::Vec};
