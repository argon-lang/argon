use argon_format_vm::vm as vf;

#[must_use]
pub fn is_instruction_pure(instruction: &vf::Instruction) -> bool {
    match instruction {
        vf::Instruction::BlockBreak { .. }
        | vf::Instruction::BlockBreakIf { .. }
        | vf::Instruction::BlockBreakUnless { .. }
        | vf::Instruction::BlockRetry { .. }
        | vf::Instruction::FunctionCall { .. }
        | vf::Instruction::FunctionObjectCall { .. }
        | vf::Instruction::FunctionObjectTokenCall { .. }
        | vf::Instruction::FunctionObjectErasedCall { .. }
        | vf::Instruction::InstanceMethodCall { .. }
        | vf::Instruction::StaticMethodCall { .. }
        | vf::Instruction::IsEnumVariantOrBreak { .. }
        | vf::Instruction::Raise { .. }
        | vf::Instruction::RecordFieldStore { .. }
        | vf::Instruction::Return { .. }
        | vf::Instruction::Unreachable {}
        | vf::Instruction::UpdateReference { .. } => false,

        vf::Instruction::Builtin { op } => is_builtin_pure(op),

        vf::Instruction::Box { .. }
        | vf::Instruction::ConstBool { .. }
        | vf::Instruction::ConstI8 { .. }
        | vf::Instruction::ConstInt { .. }
        | vf::Instruction::ConstU8 { .. }
        | vf::Instruction::ConstI16 { .. }
        | vf::Instruction::ConstU16 { .. }
        | vf::Instruction::ConstI32 { .. }
        | vf::Instruction::ConstU32 { .. }
        | vf::Instruction::ConstI64 { .. }
        | vf::Instruction::ConstU64 { .. }
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
                    b: Box::new(vf::BuiltinType::Int {
                        integer_type: vf::IntegerType::Int {},
                    }),
                }),
                array: register(0),
                index: register(1),
                value: register(2),
            }),
        }));
    }

    #[test]
    fn classifies_control_flow_instruction_as_impure() {
        assert!(!is_instruction_pure(&vf::Instruction::BlockBreak {
            block_id: Box::new(vf::BlockId { id: 0_u32.into() }),
        }));
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId { id: id.into() })
    }
}
