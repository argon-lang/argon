use hashbrown::{HashMap, HashSet};

use argon_format_vm::vm as vf;
use num_bigint::BigUint;

use super::OptimizationPass;
use crate::mutator::{InstructionRegisterType, registers_mut};
use crate::optimizer::OptimizationState;

#[derive(Debug, Default)]
pub struct UnusedRegisterElimination;

pub static UNUSED_REGISTER_ELIMINATION: UnusedRegisterElimination = UnusedRegisterElimination;

impl OptimizationPass for UnusedRegisterElimination {
    fn name(&self) -> &'static str {
        "unused-register-elimination"
    }

    fn max_iterations(&self) -> Option<usize> {
        None
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        let mut referenced_registers = HashSet::new();
        registers_mut(
            &mut function.region,
            InstructionRegisterType::Both,
            |register| {
                referenced_registers.insert(register.id.clone());
            },
        );

        let register_offset = state.argument_types().len();
        let mut replacements = HashMap::new();
        let mut next_register = register_offset;
        let mut register = register_offset;

        function.variables.variables.retain(|_| {
            let register_id = BigUint::from(register);
            register += 1;

            if referenced_registers.contains(&register_id) {
                replacements.insert(register_id, BigUint::from(next_register));
                next_register += 1;
                true
            } else {
                state.mark_changed();
                false
            }
        });

        if !replacements.is_empty() {
            registers_mut(
                &mut function.region,
                InstructionRegisterType::Both,
                |register| {
                    if let Some(replacement) = replacements.get(&register.id) {
                        register.id = replacement.clone();
                    }
                },
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn removes_unreferenced_register_declarations_and_renumbers_references() {
        let mut function = vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: vec![declaration(), declaration(), declaration()],
            }),
            region: Box::new(basic_block(vec![
                constant(0),
                constant(2),
                Box::new(vf::Instruction::Return { src: register(2) }),
            ])),
        };
        let mut state = OptimizationState::without_referenced_tubes(&[]);

        UNUSED_REGISTER_ELIMINATION.optimize(&mut state, &mut function);

        assert!(state.changed());
        assert_eq!(function.variables.variables.len(), 2);
        let vf::Instruction::ConstInt { dest, .. } =
            &*basic_block_instructions(&function.region)[1]
        else {
            unreachable!()
        };
        assert_eq!(dest.id, 1_u32.into());
        let vf::Instruction::Return { src } = &*basic_block_instructions(&function.region)[2]
        else {
            unreachable!()
        };
        assert_eq!(src.id, 1_u32.into());
    }

    #[test]
    fn preserves_register_numbering_after_receiver_and_parameters() {
        let mut function = vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: vec![declaration()],
            }),
            region: Box::new(basic_block(vec![
                Box::new(vf::Instruction::ConstInt {
                    dest: register(2),
                    value: 1.into(),
                }),
                Box::new(vf::Instruction::Return { src: register(2) }),
            ])),
        };
        let mut state = OptimizationState::without_referenced_tubes(&[
            vf::Token::Boxed {},
            vf::Token::Boxed {},
        ]);

        UNUSED_REGISTER_ELIMINATION.optimize(&mut state, &mut function);

        assert!(!state.changed());
        assert_eq!(function.variables.variables.len(), 1);
        let vf::Instruction::ConstInt { dest, .. } =
            &*basic_block_instructions(&function.region)[0]
        else {
            unreachable!()
        };
        assert_eq!(dest.id, 2_u32.into());
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
#[cfg(test)]
use alloc::{boxed::Box, vec::Vec};
