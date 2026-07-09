use argon_format_vm::vm as vf;

#[must_use]
pub fn is_instruction_pure(instruction: &vf::Instruction) -> bool {
    match instruction {
        vf::Instruction::Block { body, .. } => is_block_pure(body),
        vf::Instruction::Finally { action, ensuring } => {
            is_block_pure(action) && is_block_pure(ensuring)
        }
        vf::Instruction::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => is_block_pure(condition) && is_block_pure(when_true) && is_block_pure(when_false),

        vf::Instruction::BlockBreak { .. }
        | vf::Instruction::BlockBreakIf { .. }
        | vf::Instruction::BlockBreakUnless { .. }
        | vf::Instruction::BlockRetry { .. }
        | vf::Instruction::FunctionCall { .. }
        | vf::Instruction::FunctionObjectCall { .. }
        | vf::Instruction::FunctionObjectTokenCall { .. }
        | vf::Instruction::FunctionObjectErasedCall { .. }
        | vf::Instruction::InstanceMethodCall { .. }
        | vf::Instruction::IsEnumVariantOrBreak { .. }
        | vf::Instruction::Raise { .. }
        | vf::Instruction::RecordFieldStore { .. }
        | vf::Instruction::Return { .. }
        | vf::Instruction::Unreachable {}
        | vf::Instruction::UpdateReference { .. } => false,

        vf::Instruction::Builtin { op } => is_builtin_pure(op),

        vf::Instruction::Box { .. }
        | vf::Instruction::ConstBool { .. }
        | vf::Instruction::ConstInt { .. }
        | vf::Instruction::ConstString { .. }
        | vf::Instruction::EnumVariantLiteral { .. }
        | vf::Instruction::LoadInstanceField { .. }
        | vf::Instruction::LoadReference { .. }
        | vf::Instruction::LoadToken { .. }
        | vf::Instruction::Move { .. }
        | vf::Instruction::NewInstance { .. }
        | vf::Instruction::NewReference { .. }
        | vf::Instruction::PartiallyAppliedFunction { .. }
        | vf::Instruction::PartiallyAppliedFunctionErased { .. }
        | vf::Instruction::PartiallyAppliedTokenFunction { .. }
        | vf::Instruction::RecordFieldLoad { .. }
        | vf::Instruction::RecordLiteral { .. }
        | vf::Instruction::Tuple { .. }
        | vf::Instruction::TupleElement { .. }
        | vf::Instruction::Unbox { .. } => true,
    }
}

#[must_use]
pub fn is_block_pure(block: &vf::Block) -> bool {
    block.instructions.is_empty()
}

fn is_builtin_pure(op: &vf::BuiltinOp) -> bool {
    !matches!(op, vf::BuiltinOp::ArraySet { .. })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_value_computation_as_pure() {
        assert!(is_instruction_pure(&vf::Instruction::ConstInt {
            dest: register(0),
            value: 1.into(),
        }));
    }

    #[test]
    fn classifies_mutation_as_impure() {
        assert!(!is_instruction_pure(&vf::Instruction::UpdateReference {
            r#ref: register(0),
            value: register(1),
        }));
        assert!(!is_instruction_pure(&vf::Instruction::Builtin {
            op: Box::new(vf::BuiltinOp::ArraySet {
                element_type: Box::new(vf::Token::Builtin {
                    b: Box::new(vf::BuiltinType::Int {}),
                }),
                array: register(0),
                index: register(1),
                value: register(2),
            }),
        }));
    }

    #[test]
    fn classifies_non_empty_blocks_as_impure() {
        let block = vf::Block {
            instructions: vec![Box::new(vf::Instruction::ConstInt {
                dest: register(0),
                value: 1.into(),
            })]
            .into(),
        };

        assert!(!is_block_pure(&block));
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
