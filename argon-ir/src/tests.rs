use alloc::{boxed::Box, vec, vec::Vec};
use argon_expr::{ErasureMode, ExprContext};
use argon_parser::ast::Identifier;
use argon_util::UniqueIdentifier;
use core::{fmt::Debug, hash::Hash};
use parse18_runtime::Location;

use crate::*;
use parse18_runtime::FilePosition;

struct ContextWithoutTraits;
impl ExprContext for ContextWithoutTraits {
    type Hole = u8;
    type Function = u8;
    type Record = u8;
    type Enum = u8;
    type Trait = u8;
    type RecordField = u8;
    type EnumVariant = u8;
    type Method = u8;
    type StaticMethod = u8;
    type Instance = u8;
}

struct ShiftedContext;
impl ExprContext for ShiftedContext {
    type Hole = u16;
    type Function = u8;
    type Record = u8;
    type Enum = u8;
    type Trait = u8;
    type RecordField = u8;
    type EnumVariant = u8;
    type Method = u8;
    type StaticMethod = u8;
    type Instance = u8;
}

struct TestIrShifter;

impl IrContextShifter for TestIrShifter {
    type EC1 = ContextWithoutTraits;
    type EC2 = ShiftedContext;

    fn shift_hole(&mut self, hole: u8) -> Value<Self::EC2> {
        Value::Hole(u16::from(hole))
    }

    fn shift_instruction_hole(
        &mut self,
        destination: Register,
        hole: u8,
    ) -> Instruction<Self::EC2> {
        Instruction::Hole {
            destination,
            hole: u16::from(hole),
        }
    }
}

fn location() -> Location {
    let position = FilePosition { line: 1, column: 2 };
    Location {
        file: "test.argon".into(),
        start: position,
        end: position,
    }
}

fn assert_model_traits<T: Clone + Debug + Eq + Hash>() {}

#[test]
fn model_traits_do_not_require_traits_on_context() {
    assert_model_traits::<FunctionBody<ContextWithoutTraits>>();
}

#[test]
fn not_is_a_standalone_instruction_and_shifts_unchanged() {
    let destination = Register::new();
    let value = Register::new();
    let instruction = Instruction::<ContextWithoutTraits>::Not {
        destination: destination.clone(),
        value: value.clone(),
    };

    let shifted = TestIrShifter.shift_instruction(instruction);

    assert!(matches!(
        shifted,
        Instruction::Not {
            destination: shifted_destination,
            value: shifted_value,
        } if shifted_destination == destination && shifted_value == value
    ));
}

#[test]
fn snapshots_include_the_program_point() {
    let register = Register::new();
    let first = Value::<ContextWithoutTraits>::RegisterSnapshot {
        register: register.clone(),
        at: ProgramPoint::new(),
    };
    let second = Value::<ContextWithoutTraits>::RegisterSnapshot {
        register,
        at: ProgramPoint::new(),
    };
    assert_ne!(first, second);
}

#[test]
fn node_preserves_metadata_and_structured_join() {
    let true_entry = ProgramPoint::new();
    let false_entry = ProgramPoint::new();
    let join = ProgramPoint::new();
    let node = InstructionNode::<ContextWithoutTraits> {
        instruction: Instruction::IfElse {
            destination: Register::new(),
            condition: Register::new(),
            when_true: Region {
                entry: true_entry.clone(),
                instructions: Vec::new(),
                result: Register::new(),
            },
            when_false: Region {
                entry: false_entry.clone(),
                instructions: Vec::new(),
                result: Register::new(),
            },
        },
        location: location(),
        exit: join.clone(),
        erasure_mode: ErasureMode::Token,
    };
    assert_ne!(true_entry, false_entry);
    assert_eq!(node.exit, join);
    assert_eq!(node.erasure_mode, ErasureMode::Token);
    assert_eq!(node.location, location());
}

#[test]
fn empty_region_has_an_addressable_entry() {
    let entry = ProgramPoint::new();
    let region = Region::<ContextWithoutTraits> {
        entry: entry.clone(),
        instructions: Vec::new(),
        result: Register::new(),
    };
    assert_eq!(region.entry, entry);
    assert!(region.instructions.is_empty());
}

#[test]
fn context_shifter_traverses_values_and_function_bodies() {
    let shifted_value = TestIrShifter.shift_value(Value::BuiltinType(BuiltinType::Array {
        element_type: Box::new(Value::Hole(7)),
    }));
    assert!(matches!(
        shifted_value,
        Value::BuiltinType(BuiltinType::Array { element_type })
            if matches!(*element_type, Value::Hole(7_u16))
    ));

    let destination = Register::new();
    let declaration = RegisterDeclaration {
        register: destination.clone(),
        r#type: Value::Hole(1),
        name: None,
        is_mutable: false,
        erasure_mode: ErasureMode::Concrete,
        is_witness: false,
        origin: RegisterOrigin::Temporary,
    };
    let body = FunctionBody::<ContextWithoutTraits> {
        region: Region {
            entry: ProgramPoint::new(),
            instructions: vec![
                InstructionNode {
                    instruction: Instruction::DeclareRegister { declaration },
                    location: location(),
                    exit: ProgramPoint::new(),
                    erasure_mode: ErasureMode::Concrete,
                },
                InstructionNode {
                    instruction: Instruction::Hole {
                        destination: destination.clone(),
                        hole: 2,
                    },
                    location: location(),
                    exit: ProgramPoint::new(),
                    erasure_mode: ErasureMode::Concrete,
                },
            ],
            result: destination.clone(),
        },
    };

    let shifted_body = TestIrShifter.shift_function_body(body);
    assert_eq!(shifted_body.region.result, destination);
    assert!(matches!(
        &shifted_body.region.instructions[0].instruction,
        Instruction::DeclareRegister {
            declaration: RegisterDeclaration {
                r#type: Value::Hole(1_u16),
                ..
            },
        }
    ));
    assert!(matches!(
        &shifted_body.region.instructions[1].instruction,
        Instruction::Hole {
            destination: actual_destination,
            hole: 2_u16,
        } if actual_destination == &destination
    ));
}

#[test]
fn every_erasure_mode_can_be_recorded_on_a_node() {
    for mode in [
        ErasureMode::Erased,
        ErasureMode::Token,
        ErasureMode::Concrete,
    ] {
        let node = InstructionNode::<ContextWithoutTraits> {
            instruction: Instruction::Retry {
                label: BlockLabel {
                    id: UniqueIdentifier::new(),
                    name: None,
                    kind: BlockLabelKind::Loop,
                    block_result_type: Value::Tuple(Vec::new()),
                },
            },
            location: location(),
            exit: ProgramPoint::new(),
            erasure_mode: mode,
        };
        assert_eq!(node.erasure_mode, mode);
    }
}

#[test]
fn register_declarations_preserve_variable_metadata_and_origins() {
    let owner = ExpressionOwner::Function(7);
    let declarations = [
        RegisterDeclaration::<ContextWithoutTraits> {
            register: Register::new(),
            r#type: Value::BuiltinType(BuiltinType::Bool),
            name: Some(Identifier::Named("local".into())),
            is_mutable: true,
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
            origin: RegisterOrigin::Local,
        },
        RegisterDeclaration {
            register: Register::new(),
            r#type: Value::BuiltinType(BuiltinType::Bool),
            name: Some(Identifier::Named("argument".into())),
            is_mutable: false,
            erasure_mode: ErasureMode::Token,
            is_witness: true,
            origin: RegisterOrigin::Parameter {
                owner: owner.clone(),
                parameter_index: 2,
            },
        },
        RegisterDeclaration {
            register: Register::new(),
            r#type: Value::BuiltinType(BuiltinType::Bool),
            name: None,
            is_mutable: false,
            erasure_mode: ErasureMode::Erased,
            is_witness: false,
            origin: RegisterOrigin::InstanceParameter {
                owner: owner.clone(),
            },
        },
        RegisterDeclaration {
            register: Register::new(),
            r#type: Value::BuiltinType(BuiltinType::Bool),
            name: Some(Identifier::Named("closure argument".into())),
            is_mutable: true,
            erasure_mode: ErasureMode::Concrete,
            is_witness: true,
            origin: RegisterOrigin::ClosureParameter,
        },
        RegisterDeclaration {
            register: Register::new(),
            r#type: Value::BuiltinType(BuiltinType::Bool),
            name: None,
            is_mutable: false,
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
            origin: RegisterOrigin::Temporary,
        },
    ];

    assert!(declarations[0].is_mutable);
    assert!(declarations[1].is_witness);
    assert!(matches!(
        &declarations[1].origin,
        RegisterOrigin::Parameter { owner: actual_owner, parameter_index: 2 }
            if actual_owner == &owner
    ));
    assert!(matches!(
        declarations[2].origin,
        RegisterOrigin::InstanceParameter { .. }
    ));
    assert!(matches!(
        declarations[3].origin,
        RegisterOrigin::ClosureParameter
    ));
    assert!(matches!(declarations[4].origin, RegisterOrigin::Temporary));
}

#[test]
fn bare_registers_and_snapshots_have_distinct_state_semantics() {
    let register = Register::new();
    let bare = Value::<ContextWithoutTraits>::Register(register.clone());
    let first = Value::<ContextWithoutTraits>::RegisterSnapshot {
        register: register.clone(),
        at: ProgramPoint::new(),
    };
    let second = Value::RegisterSnapshot {
        register,
        at: ProgramPoint::new(),
    };

    assert_ne!(first, second);
    assert!(matches!(bare, Value::Register(_)));
}

#[test]
fn register_snapshot_can_be_loaded_at_a_program_point() {
    let destination = Register::new();
    let register = Register::new();
    let at = ProgramPoint::new();
    let instruction = Instruction::<ContextWithoutTraits>::LoadRegisterSnapshot {
        destination: destination.clone(),
        register: register.clone(),
        at: at.clone(),
    };
    let node = InstructionNode {
        instruction,
        location: location(),
        exit: ProgramPoint::new(),
        erasure_mode: ErasureMode::Token,
    };

    assert!(matches!(
        node.instruction,
        Instruction::LoadRegisterSnapshot {
            destination: actual_destination,
            register: actual_register,
            at: actual_at,
        } if actual_destination == destination && actual_register == register && actual_at == at
    ));
}

#[test]
fn copy_handles_initialization_and_reassignment() {
    let destination = Register::new();
    let first_source = Register::new();
    let second_source = Register::new();
    let initialization = Instruction::<ContextWithoutTraits>::Copy {
        destination: destination.clone(),
        source: first_source.clone(),
    };
    let reassignment = Instruction::<ContextWithoutTraits>::Copy {
        destination: destination.clone(),
        source: second_source.clone(),
    };

    assert!(
        matches!(initialization, Instruction::Copy { destination: d, source: s } if d == destination && s == first_source)
    );
    assert!(
        matches!(reassignment, Instruction::Copy { destination: d, source: s } if d == destination && s == second_source)
    );
}

#[test]
fn patterns_witnesses_and_closures_bind_registers() {
    let binding = Register::new();
    let pattern = Pattern::<ContextWithoutTraits>::Binding(
        binding.clone(),
        Box::new(LocatedPattern {
            pattern: Pattern::Bool(true),
            location: location(),
        }),
    );
    let witness = Register::new();
    let condition = Instruction::<ContextWithoutTraits>::Condition {
        destination: Register::new(),
        value: Register::new(),
        when_true_witness: Some(witness.clone()),
        when_false_witness: None,
    };
    let closure = Instruction::<ContextWithoutTraits>::Closure {
        destination: Register::new(),
        parameter: binding.clone(),
        return_type: Register::new(),
        body: Region {
            entry: ProgramPoint::new(),
            instructions: Vec::new(),
            result: Register::new(),
        },
    };

    assert!(matches!(pattern, Pattern::Binding(register, _) if register == binding));
    assert!(
        matches!(condition, Instruction::Condition { when_true_witness: Some(register), .. } if register == witness)
    );
    assert!(matches!(closure, Instruction::Closure { parameter, .. } if parameter == binding));
}

#[test]
fn values_are_created_by_specific_register_instructions() {
    let literal = Register::new();
    let bool_literal = Instruction::<ContextWithoutTraits>::BoolLiteral {
        destination: literal.clone(),
        value: true,
    };
    let tuple = Instruction::<ContextWithoutTraits>::Tuple {
        destination: Register::new(),
        elements: vec![literal.clone()],
    };
    let record = Instruction::<ContextWithoutTraits>::RecordLiteral {
        destination: Register::new(),
        record_type: RecordType {
            record: 1,
            arguments: Vec::new(),
        },
        fields: vec![FieldRegister {
            field: 3,
            value: literal.clone(),
        }],
    };
    let enum_variant = Instruction::<ContextWithoutTraits>::EnumVariantLiteral {
        destination: Register::new(),
        enum_type: EnumType {
            enum_: 2,
            arguments: Vec::new(),
        },
        variant: 4,
        arguments: vec![literal.clone()],
        fields: vec![FieldRegister {
            field: 5,
            value: literal.clone(),
        }],
    };

    assert!(
        matches!(bool_literal, Instruction::BoolLiteral { destination, value: true } if destination == literal)
    );
    assert!(
        matches!(tuple, Instruction::Tuple { elements, .. } if elements == vec![literal.clone()])
    );
    assert!(
        matches!(record, Instruction::RecordLiteral { fields, .. } if fields[0].value == literal)
    );
    assert!(
        matches!(enum_variant, Instruction::EnumVariantLiteral { arguments, .. } if arguments == vec![literal])
    );
}
