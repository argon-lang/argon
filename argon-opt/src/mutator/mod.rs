use argon_format_vm::vm as vf;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum InstructionRegisterType {
    Both,
    Use,
    Def,
}

pub(crate) fn registers_mut(
    block: &mut vf::Block,
    register_type: InstructionRegisterType,
    mut mutate: impl FnMut(&mut vf::RegisterId),
) {
    visit_block(block, register_type, &mut mutate);
}

fn visit_block(
    block: &mut vf::Block,
    register_type: InstructionRegisterType,
    mutate: &mut impl FnMut(&mut vf::RegisterId),
) {
    for instruction in &mut block.instructions {
        visit_instruction(instruction, register_type, mutate);
    }
}

fn visit_instruction(
    instruction: &mut vf::Instruction,
    register_type: InstructionRegisterType,
    mutate: &mut impl FnMut(&mut vf::RegisterId),
) {
    match instruction {
        vf::Instruction::Block { body, .. } => visit_block(body, register_type, mutate),
        vf::Instruction::BlockBreak { .. }
        | vf::Instruction::BlockRetry { .. }
        | vf::Instruction::Unreachable {} => {}
        vf::Instruction::BlockBreakIf { condition, .. }
        | vf::Instruction::BlockBreakUnless { condition, .. }
        | vf::Instruction::Raise {
            exception: condition,
        }
        | vf::Instruction::Return { src: condition } => {
            visit_register(
                condition,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
        }
        vf::Instruction::Box { dest, value }
        | vf::Instruction::Move { dest, src: value }
        | vf::Instruction::NewReference { dest, value }
        | vf::Instruction::Unbox { dest, value, .. } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_register(value, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::Builtin { op } => visit_builtin(op, register_type, mutate),
        vf::Instruction::ConstBool { dest, .. }
        | vf::Instruction::ConstInt { dest, .. }
        | vf::Instruction::ConstString { dest, .. }
        | vf::Instruction::LoadToken { dest, .. } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
        }
        vf::Instruction::EnumVariantLiteral {
            dest, args, fields, ..
        } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_registers(args, InstructionRegisterType::Use, register_type, mutate);
            for field in fields {
                visit_register(
                    &mut field.value,
                    InstructionRegisterType::Use,
                    register_type,
                    mutate,
                );
            }
        }
        vf::Instruction::Finally { action, ensuring } => {
            visit_block(action, register_type, mutate);
            visit_block(ensuring, register_type, mutate);
        }
        vf::Instruction::FunctionCall { dest, args, .. } => {
            visit_result(dest, register_type, mutate);
            visit_registers(args, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::FunctionObjectCall {
            dest,
            function,
            arg,
        } => {
            visit_result(dest, register_type, mutate);
            visit_register(
                function,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
            visit_register(arg, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::FunctionObjectTokenCall { dest, function, .. }
        | vf::Instruction::FunctionObjectErasedCall { dest, function } => {
            visit_result(dest, register_type, mutate);
            visit_register(
                function,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
        }
        vf::Instruction::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => {
            visit_block(condition, register_type, mutate);
            visit_block(when_true, register_type, mutate);
            visit_block(when_false, register_type, mutate);
        }
        vf::Instruction::InstanceMethodCall {
            dest,
            instance_object,
            args,
            ..
        } => {
            visit_result(dest, register_type, mutate);
            visit_register(
                instance_object,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
            visit_registers(args, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::IsEnumVariantOrBreak {
            value,
            args,
            field_extractors,
            ..
        } => {
            visit_register(value, InstructionRegisterType::Use, register_type, mutate);
            visit_registers(args, InstructionRegisterType::Use, register_type, mutate);
            for extractor in field_extractors {
                visit_register(
                    &mut extractor.r,
                    InstructionRegisterType::Def,
                    register_type,
                    mutate,
                );
            }
        }
        vf::Instruction::LoadReference { dest, r#ref } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_register(r#ref, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::LoadInstanceField {
            dest,
            instance_object,
            ..
        } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_register(
                instance_object,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
        }
        vf::Instruction::NewInstance { dest, args, .. }
        | vf::Instruction::PartiallyAppliedFunction { dest, args, .. }
        | vf::Instruction::PartiallyAppliedTokenFunction { dest, args, .. }
        | vf::Instruction::PartiallyAppliedFunctionErased { dest, args, .. } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_registers(args, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::RecordFieldLoad {
            dest, record_value, ..
        } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_register(
                record_value,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
        }
        vf::Instruction::RecordFieldStore {
            record_value,
            field_value,
            ..
        } => {
            visit_register(
                record_value,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
            visit_register(
                field_value,
                InstructionRegisterType::Use,
                register_type,
                mutate,
            );
        }
        vf::Instruction::RecordLiteral { dest, fields, .. } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            for field in fields {
                visit_register(
                    &mut field.value,
                    InstructionRegisterType::Use,
                    register_type,
                    mutate,
                );
            }
        }
        vf::Instruction::Tuple { dest, values } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_registers(values, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::TupleElement { dest, src, .. } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_register(src, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::Instruction::UpdateReference { r#ref, value } => {
            visit_register(r#ref, InstructionRegisterType::Use, register_type, mutate);
            visit_register(value, InstructionRegisterType::Use, register_type, mutate);
        }
    }
}

pub(crate) fn instruction_registers_mut(
    instruction: &mut vf::Instruction,
    register_type: InstructionRegisterType,
    mut mutate: impl FnMut(&mut vf::RegisterId),
) {
    visit_instruction(instruction, register_type, &mut mutate);
}

fn visit_builtin(
    op: &mut vf::BuiltinOp,
    register_type: InstructionRegisterType,
    mutate: &mut impl FnMut(&mut vf::RegisterId),
) {
    match op {
        vf::BuiltinOp::IntNegate { dest, value }
        | vf::BuiltinOp::IntBitNot { dest, value }
        | vf::BuiltinOp::BoolNot { dest, value }
        | vf::BuiltinOp::ArrayCreateUnsafeUninitialized {
            dest,
            length: value,
            ..
        }
        | vf::BuiltinOp::ArrayLength {
            dest, array: value, ..
        } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_register(value, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::BuiltinOp::IntAdd { dest, lhs, rhs }
        | vf::BuiltinOp::IntSub { dest, lhs, rhs }
        | vf::BuiltinOp::IntMul { dest, lhs, rhs }
        | vf::BuiltinOp::IntBitAnd { dest, lhs, rhs }
        | vf::BuiltinOp::IntBitOr { dest, lhs, rhs }
        | vf::BuiltinOp::IntBitXor { dest, lhs, rhs }
        | vf::BuiltinOp::IntBitShiftLeft { dest, lhs, rhs }
        | vf::BuiltinOp::IntBitShiftRight { dest, lhs, rhs }
        | vf::BuiltinOp::IntEq { dest, lhs, rhs }
        | vf::BuiltinOp::IntLt { dest, lhs, rhs }
        | vf::BuiltinOp::IntLe { dest, lhs, rhs }
        | vf::BuiltinOp::IntGt { dest, lhs, rhs }
        | vf::BuiltinOp::IntGe { dest, lhs, rhs }
        | vf::BuiltinOp::StringEq { dest, lhs, rhs }
        | vf::BuiltinOp::BoolEq { dest, lhs, rhs }
        | vf::BuiltinOp::ArrayGet {
            dest,
            array: lhs,
            index: rhs,
            ..
        } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_register(lhs, InstructionRegisterType::Use, register_type, mutate);
            visit_register(rhs, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::BuiltinOp::StringConcat { dest, args } => {
            visit_register(dest, InstructionRegisterType::Def, register_type, mutate);
            visit_registers(args, InstructionRegisterType::Use, register_type, mutate);
        }
        vf::BuiltinOp::ArraySet {
            array,
            index,
            value,
            ..
        } => {
            visit_register(array, InstructionRegisterType::Use, register_type, mutate);
            visit_register(index, InstructionRegisterType::Use, register_type, mutate);
            visit_register(value, InstructionRegisterType::Use, register_type, mutate);
        }
    }
}

fn visit_result(
    result: &mut vf::FunctionResult,
    register_type: InstructionRegisterType,
    mutate: &mut impl FnMut(&mut vf::RegisterId),
) {
    if let vf::FunctionResult::Register { id } = result {
        visit_register(id, InstructionRegisterType::Def, register_type, mutate);
    }
}

fn visit_registers(
    registers: &mut [Box<vf::RegisterId>],
    actual_type: InstructionRegisterType,
    register_type: InstructionRegisterType,
    mutate: &mut impl FnMut(&mut vf::RegisterId),
) {
    for register in registers {
        visit_register(register, actual_type, register_type, mutate);
    }
}

fn visit_register(
    register: &mut vf::RegisterId,
    actual_type: InstructionRegisterType,
    register_type: InstructionRegisterType,
    mutate: &mut impl FnMut(&mut vf::RegisterId),
) {
    if register_type == InstructionRegisterType::Both || register_type == actual_type {
        mutate(register);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filters_instruction_registers_by_use_and_definition() {
        let mut instruction = vf::Instruction::Move {
            dest: register(1),
            src: register(2),
        };

        instruction_registers_mut(&mut instruction, InstructionRegisterType::Use, |register| {
            register.id += 10_u32
        });
        instruction_registers_mut(&mut instruction, InstructionRegisterType::Def, |register| {
            register.id += 100_u32
        });

        let vf::Instruction::Move { dest, src } = instruction else {
            unreachable!()
        };
        assert_eq!(dest.id, 101_u32.into());
        assert_eq!(src.id, 12_u32.into());
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
