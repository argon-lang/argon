use std::collections::HashMap;

use argon_format_vm::vm as vf;

use super::{InstructionPointer, RegionPointer, VariableSet};

pub struct UseDefAnalysis {
    pub basic_blocks: HashMap<RegionPointer, UseDef>,
    pub instructions: HashMap<InstructionPointer, UseDef>,
}

#[derive(Debug, Default, Clone)]
pub struct UseDef {
    pub uses: VariableSet,
    pub definitions: VariableSet,
}

impl UseDefAnalysis {
    pub fn analyze(region: &vf::Region) -> Self {
        let mut result = Self {
            basic_blocks: HashMap::new(),
            instructions: HashMap::new(),
        };
        result.scan_region(region);
        result
    }

    pub fn scan_region(&mut self, region: &vf::Region) {
        match region {
            vf::Region::BasicBlock { instructions } => {
                let mut basic_block_usedef = UseDef::default();
                for instruction in instructions {
                    let usedef = instruction_use_def(instruction);
                    basic_block_usedef
                        .definitions
                        .extend(usedef.definitions.iter().cloned());
                    basic_block_usedef.uses.extend(usedef.uses.iter().cloned());
                    self.instructions
                        .insert(instruction.as_ref() as InstructionPointer, usedef);
                }
                self.basic_blocks
                    .insert(region as RegionPointer, basic_block_usedef);
            }
            vf::Region::Sequence { regions } => {
                for region in regions {
                    self.scan_region(region);
                }
            }
            vf::Region::Block { region, .. } => {
                self.scan_region(region);
            }
            vf::Region::IfElse {
                condition,
                when_true,
                when_false,
                ..
            } => {
                self.scan_region(condition);
                self.scan_region(when_true);
                self.scan_region(when_false);
            }
            vf::Region::Finally { action, ensuring } => {
                self.scan_region(action);
                self.scan_region(ensuring);
            }
        }
    }
}

fn instruction_use_def(instruction: &vf::Instruction) -> UseDef {
    let mut uses = VariableSet::new();
    let mut definitions = VariableSet::new();

    match instruction {
        vf::Instruction::BlockBreak { .. }
        | vf::Instruction::BlockRetry { .. }
        | vf::Instruction::Unreachable {} => {}

        vf::Instruction::BlockBreakIf { condition, .. }
        | vf::Instruction::BlockBreakUnless { condition, .. } => use_register(&mut uses, condition),

        vf::Instruction::Box { dest, value }
        | vf::Instruction::Move { dest, src: value }
        | vf::Instruction::NewReference { dest, value }
        | vf::Instruction::Unbox { dest, value, .. } => {
            define_register(&mut definitions, dest);
            use_register(&mut uses, value);
        }

        vf::Instruction::Builtin { op } => builtin_use_def(op, &mut uses, &mut definitions),

        vf::Instruction::ConstBool { dest, .. }
        | vf::Instruction::ConstI8 { dest, .. }
        | vf::Instruction::ConstInt { dest, .. }
        | vf::Instruction::ConstU8 { dest, .. }
        | vf::Instruction::ConstI16 { dest, .. }
        | vf::Instruction::ConstU16 { dest, .. }
        | vf::Instruction::ConstI32 { dest, .. }
        | vf::Instruction::ConstU32 { dest, .. }
        | vf::Instruction::ConstI64 { dest, .. }
        | vf::Instruction::ConstU64 { dest, .. }
        | vf::Instruction::ConstString { dest, .. }
        | vf::Instruction::LoadToken { dest, .. } => define_register(&mut definitions, dest),

        vf::Instruction::EnumVariantLiteral {
            dest, args, fields, ..
        } => {
            define_register(&mut definitions, dest);
            use_registers(&mut uses, args);
            uses.extend(fields.iter().map(|field| (*field.value).clone()));
        }

        vf::Instruction::FunctionCall { dest, args, .. } => {
            define_result(&mut definitions, dest);
            use_registers(&mut uses, args);
        }
        vf::Instruction::StaticMethodCall { dest, args, .. } => {
            define_result(&mut definitions, dest);
            use_registers(&mut uses, args);
        }

        vf::Instruction::FunctionObjectCall {
            dest,
            function,
            arg,
        } => {
            define_result(&mut definitions, dest);
            use_register(&mut uses, function);
            use_register(&mut uses, arg);
        }

        vf::Instruction::FunctionObjectTokenCall { dest, function, .. }
        | vf::Instruction::FunctionObjectErasedCall { dest, function } => {
            define_result(&mut definitions, dest);
            use_register(&mut uses, function);
        }

        vf::Instruction::InstanceMethodCall {
            dest,
            instance_object,
            args,
            ..
        } => {
            define_result(&mut definitions, dest);
            use_register(&mut uses, instance_object);
            use_registers(&mut uses, args);
        }

        vf::Instruction::IsEnumVariantOrBreak {
            value,
            args,
            field_extractors,
            ..
        } => {
            use_register(&mut uses, value);
            use_registers(&mut uses, args);
            definitions.extend(
                field_extractors
                    .iter()
                    .map(|extractor| (*extractor.r).clone()),
            );
        }

        vf::Instruction::LoadReference { dest, r#ref } => {
            define_register(&mut definitions, dest);
            use_register(&mut uses, r#ref);
        }

        vf::Instruction::LoadInstanceField {
            dest,
            instance_object,
            ..
        } => {
            define_register(&mut definitions, dest);
            use_register(&mut uses, instance_object);
        }

        vf::Instruction::NewInstance { dest, args, .. }
        | vf::Instruction::PartiallyAppliedFunction { dest, args, .. }
        | vf::Instruction::PartiallyAppliedTokenFunction { dest, args, .. }
        | vf::Instruction::PartiallyAppliedFunctionErased { dest, args, .. } => {
            define_register(&mut definitions, dest);
            use_registers(&mut uses, args);
        }

        vf::Instruction::Raise { exception } => use_register(&mut uses, exception),

        vf::Instruction::RecordFieldLoad {
            dest, record_value, ..
        } => {
            define_register(&mut definitions, dest);
            use_register(&mut uses, record_value);
        }

        vf::Instruction::RecordFieldStore {
            record_value,
            field_value,
            ..
        } => {
            use_register(&mut uses, record_value);
            use_register(&mut uses, field_value);
        }

        vf::Instruction::RecordLiteral { dest, fields, .. } => {
            define_register(&mut definitions, dest);
            uses.extend(fields.iter().map(|field| (*field.value).clone()));
        }

        vf::Instruction::Return { src } => use_register(&mut uses, src),

        vf::Instruction::Tuple { dest, values } => {
            define_register(&mut definitions, dest);
            use_registers(&mut uses, values);
        }

        vf::Instruction::TupleElement { dest, src, .. } => {
            define_register(&mut definitions, dest);
            use_register(&mut uses, src);
        }

        vf::Instruction::UpdateReference { r#ref, value } => {
            use_register(&mut uses, r#ref);
            use_register(&mut uses, value);
        }
    }

    UseDef { uses, definitions }
}

fn builtin_use_def(op: &vf::BuiltinOp, uses: &mut VariableSet, definitions: &mut VariableSet) {
    match op {
        vf::BuiltinOp::IntNegate { dest, value, .. }
        | vf::BuiltinOp::IntBitNot { dest, value, .. }
        | vf::BuiltinOp::IntConvert { dest, value, .. }
        | vf::BuiltinOp::BoolNot { dest, value }
        | vf::BuiltinOp::ArrayCreateUnsafeUninitialized {
            dest,
            length: value,
            ..
        }
        | vf::BuiltinOp::ArrayLength {
            dest, array: value, ..
        } => {
            define_register(definitions, dest);
            use_register(uses, value);
        }

        vf::BuiltinOp::IntAdd { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntSub { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntMul { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntBitAnd { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntBitOr { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntBitXor { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntBitShiftLeft { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntBitShiftRight { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntEq { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntLt { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntLe { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntGt { dest, lhs, rhs, .. }
        | vf::BuiltinOp::IntGe { dest, lhs, rhs, .. }
        | vf::BuiltinOp::StringEq { dest, lhs, rhs }
        | vf::BuiltinOp::BoolEq { dest, lhs, rhs }
        | vf::BuiltinOp::ArrayGet {
            dest,
            array: lhs,
            index: rhs,
            ..
        } => {
            define_register(definitions, dest);
            use_register(uses, lhs);
            use_register(uses, rhs);
        }

        vf::BuiltinOp::StringConcat { dest, args } => {
            define_register(definitions, dest);
            use_registers(uses, args);
        }

        vf::BuiltinOp::ArraySet {
            array,
            index,
            value,
            ..
        } => {
            use_register(uses, array);
            use_register(uses, index);
            use_register(uses, value);
        }
    }
}

fn define_result(definitions: &mut VariableSet, result: &vf::FunctionResult) {
    if let vf::FunctionResult::Register { id } = result {
        define_register(definitions, id);
    }
}

fn define_register(definitions: &mut VariableSet, register: &vf::RegisterId) {
    definitions.insert(register.clone());
}

fn use_register(uses: &mut VariableSet, register: &vf::RegisterId) {
    uses.insert(register.clone());
}

fn use_registers<'a>(
    uses: &mut VariableSet,
    registers: impl IntoIterator<Item = &'a Box<vf::RegisterId>>,
) {
    uses.extend(registers.into_iter().map(|register| (**register).clone()));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn computes_instruction_uses_and_definitions() {
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

        let analysis = UseDefAnalysis::analyze(&region);

        assert_eq!(analysis.instructions[&pointers[0]].uses, variables([]));
        assert_eq!(
            analysis.instructions[&pointers[0]].definitions,
            variables([0])
        );
        assert_eq!(analysis.instructions[&pointers[1]].uses, variables([0]));
        assert_eq!(
            analysis.instructions[&pointers[1]].definitions,
            variables([1])
        );
        assert_eq!(analysis.instructions[&pointers[2]].uses, variables([1]));
        assert_eq!(
            analysis.instructions[&pointers[2]].definitions,
            variables([])
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
