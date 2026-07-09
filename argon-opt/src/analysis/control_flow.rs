use std::collections::{HashMap, HashSet};

use argon_format_vm::vm as vf;

use super::branches::{BranchAnalysis, InstructionPointer};

#[derive(Debug, Default)]
pub struct InstructionFlow {
    pub predecessors: HashSet<InstructionPointer>,
    pub successors: HashSet<InstructionPointer>,
}

#[derive(Debug, Default)]
pub struct ControlFlowAnalysis {
    pub instructions: HashMap<InstructionPointer, InstructionFlow>,
}

impl ControlFlowAnalysis {
    pub fn analyze(block: &vf::Block) -> Self {
        let branches = BranchAnalysis::analyze(block);
        let mut analysis = Self::default();
        analysis.scan_block(block, &HashSet::new(), &HashSet::new(), &branches);
        analysis
    }

    fn scan_block(
        &mut self,
        block: &vf::Block,
        continuation: &HashSet<InstructionPointer>,
        always_reachable: &HashSet<InstructionPointer>,
        branches: &BranchAnalysis,
    ) {
        for (index, instruction) in block.instructions.iter().enumerate() {
            let next = block
                .instructions
                .get(index + 1)
                .map_or_else(|| continuation.clone(), |next| pointers([next.as_ref()]));

            self.scan_instruction(instruction, &next, always_reachable, branches);
        }
    }

    fn scan_instruction(
        &mut self,
        instruction: &vf::Instruction,
        next: &HashSet<InstructionPointer>,
        always_reachable: &HashSet<InstructionPointer>,
        branches: &BranchAnalysis,
    ) {
        let instruction_pointer = instruction as InstructionPointer;
        self.instructions.entry(instruction_pointer).or_default();
        self.add_edges(instruction_pointer, always_reachable);

        match instruction {
            vf::Instruction::Block {
                block_id,
                flags,
                body,
            } => {
                let body_start = first_instruction(body);
                self.add_edges(
                    instruction_pointer,
                    body_start.as_ref().map_or(next, |start| start),
                );
                let body_continuation = if flags.is_loop {
                    body_start
                        .as_ref()
                        .map_or_else(|| next.clone(), Clone::clone)
                } else {
                    next.clone()
                };
                self.scan_block(body, &body_continuation, always_reachable, branches);

                if let Some(block_branches) = branches.blocks.get(&block_id.id) {
                    self.add_branch_edges(&block_branches.breaks, next);
                    if let Some(body_start) = body_start {
                        self.add_branch_edges(&block_branches.retries, &body_start);
                    }
                }
            }

            vf::Instruction::Finally { action, ensuring } => {
                let ensuring_start = first_instruction(ensuring);
                let action_continuation = ensuring_start.as_ref().map_or(next, |start| start);
                let action_start = first_instruction(action);
                let action_always_reachable = always_reachable
                    .iter()
                    .chain(ensuring_start.iter().flatten())
                    .copied()
                    .collect();

                self.add_edges(
                    instruction_pointer,
                    action_start
                        .as_ref()
                        .map_or(action_continuation, |start| start),
                );
                self.scan_block(
                    action,
                    action_continuation,
                    &action_always_reachable,
                    branches,
                );
                self.scan_block(ensuring, next, always_reachable, branches);
            }

            vf::Instruction::IfElse {
                when_true_block_id,
                when_false_block_id,
                condition,
                when_true,
                when_false,
            } => {
                let when_true_start = first_instruction(when_true);
                let when_false_start = first_instruction(when_false);
                let condition_continuation = when_true_start
                    .as_ref()
                    .unwrap_or(next)
                    .iter()
                    .chain(when_false_start.as_ref().unwrap_or(next))
                    .copied()
                    .collect();
                let condition_start = first_instruction(condition);

                self.add_edges(
                    instruction_pointer,
                    condition_start
                        .as_ref()
                        .map_or(&condition_continuation, |start| start),
                );
                self.scan_block(
                    condition,
                    &condition_continuation,
                    always_reachable,
                    branches,
                );
                self.scan_block(when_true, next, always_reachable, branches);
                self.scan_block(when_false, next, always_reachable, branches);

                self.add_block_branches(
                    branches,
                    &when_true_block_id.id,
                    when_true_start.as_ref(),
                    next,
                );
                self.add_block_branches(
                    branches,
                    &when_false_block_id.id,
                    when_false_start.as_ref(),
                    next,
                );
            }

            vf::Instruction::BlockBreak { .. }
            | vf::Instruction::BlockRetry { .. }
            | vf::Instruction::Raise { .. }
            | vf::Instruction::Return { .. }
            | vf::Instruction::Unreachable {} => {}

            _ => self.add_edges(instruction_pointer, next),
        }
    }

    fn add_block_branches(
        &mut self,
        branches: &BranchAnalysis,
        block_id: &num_bigint::BigUint,
        block_start: Option<&HashSet<InstructionPointer>>,
        continuation: &HashSet<InstructionPointer>,
    ) {
        if let Some(block_branches) = branches.blocks.get(block_id) {
            self.add_branch_edges(&block_branches.breaks, continuation);
            if let Some(block_start) = block_start {
                self.add_branch_edges(&block_branches.retries, block_start);
            }
        }
    }

    fn add_branch_edges(
        &mut self,
        sources: &HashSet<InstructionPointer>,
        targets: &HashSet<InstructionPointer>,
    ) {
        for &source in sources {
            self.add_edges(source, targets);
        }
    }

    fn add_edges(&mut self, source: InstructionPointer, targets: &HashSet<InstructionPointer>) {
        for &target in targets {
            self.instructions
                .entry(source)
                .or_default()
                .successors
                .insert(target);
            self.instructions
                .entry(target)
                .or_default()
                .predecessors
                .insert(source);
        }
    }
}

fn first_instruction(block: &vf::Block) -> Option<HashSet<InstructionPointer>> {
    block
        .instructions
        .first()
        .map(|instruction| pointers([instruction.as_ref()]))
}

fn pointers<'a>(
    instructions: impl IntoIterator<Item = &'a vf::Instruction>,
) -> HashSet<InstructionPointer> {
    instructions
        .into_iter()
        .map(|instruction| instruction as InstructionPointer)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finally_is_reachable_from_every_action_instruction() {
        let block = vf::Block {
            instructions: vec![Box::new(vf::Instruction::Finally {
                action: Box::new(vf::Block {
                    instructions: vec![
                        Box::new(vf::Instruction::ConstInt {
                            dest: register(0),
                            value: 1.into(),
                        }),
                        Box::new(vf::Instruction::Return { src: register(0) }),
                    ]
                    .into(),
                }),
                ensuring: Box::new(vf::Block {
                    instructions: vec![Box::new(vf::Instruction::ConstInt {
                        dest: register(1),
                        value: 2.into(),
                    })]
                    .into(),
                }),
            })]
            .into(),
        };
        let vf::Instruction::Finally { action, ensuring } = block.instructions[0].as_ref() else {
            unreachable!()
        };
        let ensuring_pointer = ensuring.instructions[0].as_ref() as InstructionPointer;

        let analysis = ControlFlowAnalysis::analyze(&block);

        for instruction in &action.instructions {
            let instruction_pointer = instruction.as_ref() as InstructionPointer;
            assert!(analysis.instructions[&instruction_pointer]
                .successors
                .contains(&ensuring_pointer));
        }
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
