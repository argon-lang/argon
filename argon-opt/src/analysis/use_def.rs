use std::collections::{HashMap, HashSet};

use argon_format_vm::vm as vf;

use super::branches::InstructionPointer;

pub type VariableSet = HashSet<vf::RegisterId>;

#[derive(Debug, Default)]
pub struct InstructionUseDef {
    pub uses: VariableSet,
    pub definitions: VariableSet,
}

pub fn build_use_def(block: &vf::Block) -> HashMap<InstructionPointer, InstructionUseDef> {
    let mut result = HashMap::new();
    collect_use_def(block, &mut result);
    result
}

pub(crate) fn collect_use_def(
    block: &vf::Block,
    result: &mut HashMap<InstructionPointer, InstructionUseDef>,
) {
    for instruction in &block.instructions {
        result.insert(
            instruction.as_ref() as InstructionPointer,
            instruction_use_def(instruction),
        );
        match instruction.as_ref() {
            vf::Instruction::Block { body, .. } => collect_use_def(body, result),
            vf::Instruction::Finally { action, ensuring } => {
                collect_use_def(action, result);
                collect_use_def(ensuring, result);
            }
            vf::Instruction::IfElse {
                condition,
                when_true,
                when_false,
                ..
            } => {
                collect_use_def(condition, result);
                collect_use_def(when_true, result);
                collect_use_def(when_false, result);
            }
            _ => {}
        }
    }
}

fn instruction_use_def(instruction: &vf::Instruction) -> InstructionUseDef {
    let mut uses = VariableSet::new();
    let mut definitions = VariableSet::new();

    match instruction {
        vf::Instruction::Block { .. }
        | vf::Instruction::BlockBreak { .. }
        | vf::Instruction::BlockRetry { .. }
        | vf::Instruction::Finally { .. }
        | vf::Instruction::IfElse { .. }
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
        | vf::Instruction::ConstInt { dest, .. }
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

    InstructionUseDef { uses, definitions }
}

fn builtin_use_def(op: &vf::BuiltinOp, uses: &mut VariableSet, definitions: &mut VariableSet) {
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
            define_register(definitions, dest);
            use_register(uses, value);
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
                Box::new(vf::Instruction::Return { src: register(1) }),
            ]
            .into(),
        };
        let pointers = block
            .instructions
            .iter()
            .map(|instruction| instruction.as_ref() as InstructionPointer)
            .collect::<Vec<_>>();

        let analysis = build_use_def(&block);

        assert_eq!(analysis[&pointers[0]].uses, variables([]));
        assert_eq!(analysis[&pointers[0]].definitions, variables([0]));
        assert_eq!(analysis[&pointers[1]].uses, variables([0]));
        assert_eq!(analysis[&pointers[1]].definitions, variables([1]));
        assert_eq!(analysis[&pointers[2]].uses, variables([1]));
        assert_eq!(analysis[&pointers[2]].definitions, variables([]));
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
