use alloc::boxed::Box;
use argon_expr::ExprContext;

use crate::*;

pub trait IrContextShifter {
    type EC1: ExprContext + ?Sized;
    type EC2: ExprContext<
            Function = <Self::EC1 as ExprContext>::Function,
            Record = <Self::EC1 as ExprContext>::Record,
            Enum = <Self::EC1 as ExprContext>::Enum,
            Trait = <Self::EC1 as ExprContext>::Trait,
            RecordField = <Self::EC1 as ExprContext>::RecordField,
            EnumVariant = <Self::EC1 as ExprContext>::EnumVariant,
            Method = <Self::EC1 as ExprContext>::Method,
            StaticMethod = <Self::EC1 as ExprContext>::StaticMethod,
            Instance = <Self::EC1 as ExprContext>::Instance,
        > + ?Sized;

    fn shift_hole(&mut self, hole: <Self::EC1 as ExprContext>::Hole) -> Value<Self::EC2>;

    fn shift_instruction_hole(
        &mut self,
        destination: Register,
        hole: <Self::EC1 as ExprContext>::Hole,
    ) -> Instruction<Self::EC2>;

    fn shift_value(&mut self, value: Value<Self::EC1>) -> Value<Self::EC2> {
        default_shift_value(self, value)
    }

    fn shift_function_body(&mut self, body: FunctionBody<Self::EC1>) -> FunctionBody<Self::EC2> {
        default_shift_function_body(self, body)
    }

    fn shift_region(&mut self, region: Region<Self::EC1>) -> Region<Self::EC2> {
        default_shift_region(self, region)
    }

    fn shift_instruction(&mut self, instruction: Instruction<Self::EC1>) -> Instruction<Self::EC2> {
        default_shift_instruction(self, instruction)
    }
}

pub fn default_shift_value<S>(shifter: &mut S, value: Value<S::EC1>) -> Value<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match value {
        Value::Error => Value::Error,
        Value::Hole(hole) => shifter.shift_hole(hole),
        Value::BoolLiteral(value) => Value::BoolLiteral(value),
        Value::IntLiteral(value) => Value::IntLiteral(value),
        Value::I8Literal(value) => Value::I8Literal(value),
        Value::U8Literal(value) => Value::U8Literal(value),
        Value::I16Literal(value) => Value::I16Literal(value),
        Value::U16Literal(value) => Value::U16Literal(value),
        Value::I32Literal(value) => Value::I32Literal(value),
        Value::U32Literal(value) => Value::U32Literal(value),
        Value::I64Literal(value) => Value::I64Literal(value),
        Value::U64Literal(value) => Value::U64Literal(value),
        Value::StringLiteral(value) => Value::StringLiteral(value),
        Value::Tuple(values) => Value::Tuple(
            values
                .into_iter()
                .map(|value| shifter.shift_value(value))
                .collect(),
        ),
        Value::Type(value) => Value::Type(Box::new(shifter.shift_value(*value))),
        Value::BigType(value) => Value::BigType(value),
        Value::BuiltinType(value) => Value::BuiltinType(default_shift_builtin_type(shifter, value)),
        Value::Register(register) => Value::Register(register),
        Value::FunctionResult { result_type } => Value::FunctionResult {
            result_type: Box::new(shifter.shift_value(*result_type)),
        },
        Value::FunctionType { parameter, result } => Value::FunctionType {
            parameter,
            result: Box::new(shifter.shift_value(*result)),
        },
        Value::ConjunctionType { lhs, rhs } => Value::ConjunctionType {
            lhs: Box::new(shifter.shift_value(*lhs)),
            rhs: Box::new(shifter.shift_value(*rhs)),
        },
        Value::DisjunctionType { lhs, rhs } => Value::DisjunctionType {
            lhs: Box::new(shifter.shift_value(*lhs)),
            rhs: Box::new(shifter.shift_value(*rhs)),
        },
        Value::EqualToType { r#type, lhs, rhs } => Value::EqualToType {
            r#type: Box::new(shifter.shift_value(*r#type)),
            lhs: Box::new(shifter.shift_value(*lhs)),
            rhs: Box::new(shifter.shift_value(*rhs)),
        },
        Value::RecordType(value) => Value::RecordType(default_shift_record_type(shifter, value)),
        Value::EnumType(value) => Value::EnumType(default_shift_enum_type(shifter, value)),
        Value::TraitType(value) => Value::TraitType(default_shift_trait_type(shifter, value)),
        Value::InstanceType(value) => {
            Value::InstanceType(default_shift_instance_type(shifter, value))
        }
        Value::RecordLiteral {
            record_type,
            fields,
        } => Value::RecordLiteral {
            record_type: default_shift_record_type(shifter, record_type),
            fields: fields
                .into_iter()
                .map(|field| FieldValue {
                    field: field.field,
                    value: shifter.shift_value(field.value),
                })
                .collect(),
        },
        Value::EnumVariantLiteral {
            enum_type,
            variant,
            arguments,
            fields,
        } => Value::EnumVariantLiteral {
            enum_type: default_shift_enum_type(shifter, enum_type),
            variant,
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift_value(argument))
                .collect(),
            fields: fields
                .into_iter()
                .map(|field| FieldValue {
                    field: field.field,
                    value: shifter.shift_value(field.value),
                })
                .collect(),
        },
        Value::SharedType(value) => Value::SharedType(Box::new(shifter.shift_value(*value))),
        Value::UseType { is_mutable, inner } => Value::UseType {
            is_mutable,
            inner: Box::new(shifter.shift_value(*inner)),
        },
        Value::BoxedType(value) => Value::BoxedType(Box::new(shifter.shift_value(*value))),
        Value::RegisterSnapshot { register, at } => Value::RegisterSnapshot { register, at },
    }
}

pub fn default_shift_function_body<S>(
    shifter: &mut S,
    body: FunctionBody<S::EC1>,
) -> FunctionBody<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    FunctionBody {
        region: default_shift_region(shifter, body.region),
    }
}

pub fn default_shift_region<S>(shifter: &mut S, region: Region<S::EC1>) -> Region<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    Region {
        entry: region.entry,
        instructions: region
            .instructions
            .into_iter()
            .map(|node| InstructionNode {
                instruction: shifter.shift_instruction(node.instruction),
                location: node.location,
                exit: node.exit,
                erasure_mode: node.erasure_mode,
            })
            .collect(),
        result: region.result,
    }
}

pub fn default_shift_register_declaration<S>(
    shifter: &mut S,
    declaration: RegisterDeclaration<S::EC1>,
) -> RegisterDeclaration<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    RegisterDeclaration {
        register: declaration.register,
        r#type: shifter.shift_value(declaration.r#type),
        name: declaration.name,
        is_mutable: declaration.is_mutable,
        erasure_mode: declaration.erasure_mode,
        is_witness: declaration.is_witness,
        origin: default_shift_register_origin(shifter, declaration.origin),
    }
}

pub fn default_shift_register_origin<S>(
    shifter: &mut S,
    origin: RegisterOrigin<S::EC1>,
) -> RegisterOrigin<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match origin {
        RegisterOrigin::Temporary => RegisterOrigin::Temporary,
        RegisterOrigin::Local => RegisterOrigin::Local,
        RegisterOrigin::Parameter {
            owner,
            parameter_index,
        } => RegisterOrigin::Parameter {
            owner: default_shift_expression_owner(shifter, owner),
            parameter_index,
        },
        RegisterOrigin::InstanceParameter { owner } => RegisterOrigin::InstanceParameter {
            owner: default_shift_expression_owner(shifter, owner),
        },
        RegisterOrigin::ClosureParameter => RegisterOrigin::ClosureParameter,
    }
}

pub fn default_shift_expression_owner<S>(
    _shifter: &mut S,
    owner: ExpressionOwner<S::EC1>,
) -> ExpressionOwner<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match owner {
        ExpressionOwner::Function(value) => ExpressionOwner::Function(value),
        ExpressionOwner::Record(value) => ExpressionOwner::Record(value),
        ExpressionOwner::Enum(value) => ExpressionOwner::Enum(value),
        ExpressionOwner::Trait(value) => ExpressionOwner::Trait(value),
        ExpressionOwner::EnumVariant(value) => ExpressionOwner::EnumVariant(value),
        ExpressionOwner::Method(value) => ExpressionOwner::Method(value),
        ExpressionOwner::StaticMethod(value) => ExpressionOwner::StaticMethod(value),
        ExpressionOwner::Instance(value) => ExpressionOwner::Instance(value),
        ExpressionOwner::Field(value) => ExpressionOwner::Field(value),
    }
}

pub fn default_shift_block_label<S>(
    shifter: &mut S,
    label: BlockLabel<S::EC1>,
) -> BlockLabel<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    BlockLabel {
        id: label.id,
        name: label.name,
        kind: label.kind,
        block_result_type: shifter.shift_value(label.block_result_type),
    }
}

pub fn default_shift_located_pattern<S>(
    shifter: &mut S,
    pattern: LocatedPattern<S::EC1>,
) -> LocatedPattern<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    LocatedPattern {
        pattern: default_shift_pattern(shifter, pattern.pattern),
        location: pattern.location,
    }
}

pub fn default_shift_pattern<S>(shifter: &mut S, pattern: Pattern<S::EC1>) -> Pattern<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match pattern {
        Pattern::Error => Pattern::Error,
        Pattern::Discard { r#type } => Pattern::Discard {
            r#type: Box::new(shifter.shift_value(*r#type)),
        },
        Pattern::Tuple(patterns) => Pattern::Tuple(
            patterns
                .into_iter()
                .map(|pattern| default_shift_located_pattern(shifter, pattern))
                .collect(),
        ),
        Pattern::Binding(register, pattern) => Pattern::Binding(
            register,
            Box::new(default_shift_located_pattern(shifter, *pattern)),
        ),
        Pattern::EnumVariant {
            enum_type,
            variant,
            arguments,
            fields,
        } => Pattern::EnumVariant {
            enum_type: default_shift_enum_type(shifter, enum_type),
            variant,
            arguments: arguments
                .into_iter()
                .map(|pattern| default_shift_located_pattern(shifter, pattern))
                .collect(),
            fields: fields
                .into_iter()
                .map(|field| FieldPattern {
                    field: field.field,
                    pattern: default_shift_located_pattern(shifter, field.pattern),
                })
                .collect(),
        },
        Pattern::String(value) => Pattern::String(value),
        Pattern::Int(value) => Pattern::Int(value),
        Pattern::Bool(value) => Pattern::Bool(value),
    }
}

fn default_shift_builtin_type<S>(shifter: &mut S, value: BuiltinType<S::EC1>) -> BuiltinType<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match value {
        BuiltinType::Int(value) => BuiltinType::Int(value),
        BuiltinType::Bool => BuiltinType::Bool,
        BuiltinType::String => BuiltinType::String,
        BuiltinType::Never => BuiltinType::Never,
        BuiltinType::Array { element_type } => BuiltinType::Array {
            element_type: Box::new(shifter.shift_value(*element_type)),
        },
    }
}

macro_rules! shift_nominal_type {
    ($function:ident, $type:ident, $member:ident) => {
        pub fn $function<S>(shifter: &mut S, value: $type<S::EC1>) -> $type<S::EC2>
        where
            S: IrContextShifter + ?Sized,
        {
            $type {
                $member: value.$member,
                arguments: value
                    .arguments
                    .into_iter()
                    .map(|argument| shifter.shift_value(argument))
                    .collect(),
            }
        }
    };
}

shift_nominal_type!(default_shift_record_type, RecordType, record);
shift_nominal_type!(default_shift_enum_type, EnumType, enum_);
shift_nominal_type!(default_shift_trait_type, TraitType, trait_);
shift_nominal_type!(default_shift_instance_type, InstanceType, instance);

fn default_shift_method_instance_type<S>(
    shifter: &mut S,
    value: MethodInstanceType<S::EC1>,
) -> MethodInstanceType<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match value {
        MethodInstanceType::Record(value) => {
            MethodInstanceType::Record(default_shift_record_type(shifter, value))
        }
        MethodInstanceType::Enum(value) => {
            MethodInstanceType::Enum(default_shift_enum_type(shifter, value))
        }
        MethodInstanceType::Trait(value) => {
            MethodInstanceType::Trait(default_shift_trait_type(shifter, value))
        }
    }
}

pub fn default_shift_instruction<S>(
    shifter: &mut S,
    instruction: Instruction<S::EC1>,
) -> Instruction<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match instruction {
        Instruction::DeclareRegister { declaration } => Instruction::DeclareRegister {
            declaration: default_shift_register_declaration(shifter, declaration),
        },
        Instruction::Hole { destination, hole } => {
            shifter.shift_instruction_hole(destination, hole)
        }
        Instruction::BoolLiteral { destination, value } => {
            Instruction::BoolLiteral { destination, value }
        }
        Instruction::IntLiteral { destination, value } => {
            Instruction::IntLiteral { destination, value }
        }
        Instruction::I8Literal { destination, value } => {
            Instruction::I8Literal { destination, value }
        }
        Instruction::U8Literal { destination, value } => {
            Instruction::U8Literal { destination, value }
        }
        Instruction::I16Literal { destination, value } => {
            Instruction::I16Literal { destination, value }
        }
        Instruction::U16Literal { destination, value } => {
            Instruction::U16Literal { destination, value }
        }
        Instruction::I32Literal { destination, value } => {
            Instruction::I32Literal { destination, value }
        }
        Instruction::U32Literal { destination, value } => {
            Instruction::U32Literal { destination, value }
        }
        Instruction::I64Literal { destination, value } => {
            Instruction::I64Literal { destination, value }
        }
        Instruction::U64Literal { destination, value } => {
            Instruction::U64Literal { destination, value }
        }
        Instruction::StringLiteral { destination, value } => {
            Instruction::StringLiteral { destination, value }
        }
        Instruction::Tuple {
            destination,
            elements,
        } => Instruction::Tuple {
            destination,
            elements,
        },
        Instruction::RecordLiteral {
            destination,
            record_type,
            fields,
        } => Instruction::RecordLiteral {
            destination,
            record_type: default_shift_record_type(shifter, record_type),
            fields: fields
                .into_iter()
                .map(|field| FieldRegister {
                    field: field.field,
                    value: field.value,
                })
                .collect(),
        },
        Instruction::EnumVariantLiteral {
            destination,
            enum_type,
            variant,
            arguments,
            fields,
        } => Instruction::EnumVariantLiteral {
            destination,
            enum_type: default_shift_enum_type(shifter, enum_type),
            variant,
            arguments,
            fields: fields
                .into_iter()
                .map(|field| FieldRegister {
                    field: field.field,
                    value: field.value,
                })
                .collect(),
        },
        Instruction::Type { destination, value } => Instruction::Type { destination, value },
        Instruction::BigType { destination, level } => Instruction::BigType { destination, level },
        Instruction::IntType {
            destination,
            integer_type,
        } => Instruction::IntType {
            destination,
            integer_type,
        },
        Instruction::BoolType { destination } => Instruction::BoolType { destination },
        Instruction::StringType { destination } => Instruction::StringType { destination },
        Instruction::NeverType { destination } => Instruction::NeverType { destination },
        Instruction::ArrayType {
            destination,
            element_type,
        } => Instruction::ArrayType {
            destination,
            element_type,
        },
        Instruction::FunctionResult {
            destination,
            result_type,
        } => Instruction::FunctionResult {
            destination,
            result_type,
        },
        Instruction::FunctionType {
            destination,
            parameter,
            result,
        } => Instruction::FunctionType {
            destination,
            parameter,
            result,
        },
        Instruction::ConjunctionType {
            destination,
            lhs,
            rhs,
        } => Instruction::ConjunctionType {
            destination,
            lhs,
            rhs,
        },
        Instruction::DisjunctionType {
            destination,
            lhs,
            rhs,
        } => Instruction::DisjunctionType {
            destination,
            lhs,
            rhs,
        },
        Instruction::EqualToType {
            destination,
            r#type,
            lhs,
            rhs,
        } => Instruction::EqualToType {
            destination,
            r#type,
            lhs,
            rhs,
        },
        Instruction::RecordType {
            destination,
            record,
            arguments,
        } => Instruction::RecordType {
            destination,
            record,
            arguments,
        },
        Instruction::EnumType {
            destination,
            enum_,
            arguments,
        } => Instruction::EnumType {
            destination,
            enum_,
            arguments,
        },
        Instruction::TraitType {
            destination,
            trait_,
            arguments,
        } => Instruction::TraitType {
            destination,
            trait_,
            arguments,
        },
        Instruction::InstanceType {
            destination,
            instance,
            arguments,
        } => Instruction::InstanceType {
            destination,
            instance,
            arguments,
        },
        Instruction::SharedType { destination, inner } => {
            Instruction::SharedType { destination, inner }
        }
        Instruction::UseType {
            destination,
            is_mutable,
            inner,
        } => Instruction::UseType {
            destination,
            is_mutable,
            inner,
        },
        Instruction::BoxedType { destination, inner } => {
            Instruction::BoxedType { destination, inner }
        }
        Instruction::LoadRegisterSnapshot {
            destination,
            register,
            at,
        } => Instruction::LoadRegisterSnapshot {
            destination,
            register,
            at,
        },
        Instruction::Copy {
            destination,
            source,
        } => Instruction::Copy {
            destination,
            source,
        },
        Instruction::FunctionCall {
            destination,
            function,
            arguments,
        } => Instruction::FunctionCall {
            destination,
            function,
            arguments,
        },
        Instruction::FunctionObjectCall {
            destination,
            function,
            argument,
        } => Instruction::FunctionObjectCall {
            destination,
            function,
            argument,
        },
        Instruction::MethodCall {
            destination,
            method,
            instance_type,
            receiver,
            arguments,
        } => Instruction::MethodCall {
            destination,
            method,
            instance_type: default_shift_method_instance_type(shifter, instance_type),
            receiver,
            arguments,
        },
        Instruction::StaticMethodCall {
            destination,
            method,
            owner_type,
            arguments,
        } => Instruction::StaticMethodCall {
            destination,
            method,
            owner_type: default_shift_method_instance_type(shifter, owner_type),
            arguments,
        },
        Instruction::NewInstance {
            destination,
            instance,
            arguments,
        } => Instruction::NewInstance {
            destination,
            instance,
            arguments,
        },
        Instruction::Builtin {
            destination,
            builtin,
        } => Instruction::Builtin {
            destination,
            builtin: default_shift_builtin(shifter, builtin),
        },
        Instruction::TupleElement {
            destination,
            tuple,
            index,
        } => Instruction::TupleElement {
            destination,
            tuple,
            index,
        },
        Instruction::RecordFieldLoad {
            destination,
            record_type,
            field,
            record,
        } => Instruction::RecordFieldLoad {
            destination,
            record_type: default_shift_record_type(shifter, record_type),
            field,
            record,
        },
        Instruction::RecordFieldStore {
            record_type,
            field,
            record,
            value,
        } => Instruction::RecordFieldStore {
            record_type: default_shift_record_type(shifter, record_type),
            field,
            record,
            value,
        },
        Instruction::BindEnsures { value, witnesses } => {
            Instruction::BindEnsures { value, witnesses }
        }
        Instruction::BindErasedAlias {
            destination,
            equality_witness,
            value,
        } => Instruction::BindErasedAlias {
            destination,
            equality_witness,
            value,
        },
        Instruction::Share { destination, value } => Instruction::Share { destination, value },
        Instruction::Borrow { destination, value } => Instruction::Borrow { destination, value },
        Instruction::BorrowMut { destination, value } => {
            Instruction::BorrowMut { destination, value }
        }
        Instruction::Box {
            destination,
            r#type,
            value,
        } => Instruction::Box {
            destination,
            r#type,
            value,
        },
        Instruction::Unbox {
            destination,
            r#type,
            value,
        } => Instruction::Unbox {
            destination,
            r#type,
            value,
        },
        Instruction::Raise { exception } => Instruction::Raise { exception },
        Instruction::Break { label, value } => Instruction::Break {
            label: default_shift_block_label(shifter, label),
            value,
        },
        Instruction::Retry { label } => Instruction::Retry {
            label: default_shift_block_label(shifter, label),
        },
        Instruction::Condition {
            destination,
            value,
            when_true_witness,
            when_false_witness,
        } => Instruction::Condition {
            destination,
            value,
            when_true_witness,
            when_false_witness,
        },
        Instruction::IfElse {
            destination,
            condition,
            when_true,
            when_false,
        } => Instruction::IfElse {
            destination,
            condition,
            when_true: default_shift_region(shifter, when_true),
            when_false: default_shift_region(shifter, when_false),
        },
        Instruction::Match {
            destination,
            value,
            cases,
        } => Instruction::Match {
            destination,
            value,
            cases: cases
                .into_iter()
                .map(|case| MatchCase {
                    pattern: default_shift_located_pattern(shifter, case.pattern),
                    body: default_shift_region(shifter, case.body),
                })
                .collect(),
        },
        Instruction::Block {
            destination,
            label,
            body,
        } => Instruction::Block {
            destination,
            label: default_shift_block_label(shifter, label),
            body: default_shift_region(shifter, body),
        },
        Instruction::Finally {
            destination,
            body,
            finally,
        } => Instruction::Finally {
            destination,
            body: default_shift_region(shifter, body),
            finally: default_shift_region(shifter, finally),
        },
        Instruction::Not { destination, value } => Instruction::Not { destination, value },
        Instruction::And {
            destination,
            lhs,
            rhs,
        } => Instruction::And {
            destination,
            lhs,
            rhs: default_shift_region(shifter, rhs),
        },
        Instruction::Or {
            destination,
            lhs,
            rhs,
        } => Instruction::Or {
            destination,
            lhs,
            rhs: default_shift_region(shifter, rhs),
        },
        Instruction::Closure {
            destination,
            parameter,
            return_type,
            body,
        } => Instruction::Closure {
            destination,
            parameter,
            return_type,
            body: default_shift_region(shifter, body),
        },
    }
}

fn default_shift_builtin<S>(shifter: &mut S, builtin: Builtin<S::EC1>) -> Builtin<S::EC2>
where
    S: IrContextShifter + ?Sized,
{
    match builtin {
        Builtin::IntNegate {
            integer_type,
            value,
        } => Builtin::IntNegate {
            integer_type,
            value,
        },
        Builtin::IntBitNot {
            integer_type,
            value,
        } => Builtin::IntBitNot {
            integer_type,
            value,
        },
        Builtin::IntConvert {
            source_type,
            destination_type,
            value,
        } => Builtin::IntConvert {
            source_type,
            destination_type,
            value,
        },
        Builtin::IntAdd {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntAdd {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntSub {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntSub {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntMul {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntMul {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntBitAnd {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntBitAnd {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntBitOr {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntBitOr {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntBitXor {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntBitXor {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntBitShiftLeft {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntBitShiftLeft {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntBitShiftRight {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntBitShiftRight {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntEq {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntEq {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntLt {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntLt {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntLe {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntLe {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntGt {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntGt {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::IntGe {
            integer_type,
            lhs,
            rhs,
        } => Builtin::IntGe {
            integer_type,
            lhs,
            rhs,
        },
        Builtin::StringConcat { values } => Builtin::StringConcat { values },
        Builtin::StringEq { lhs, rhs } => Builtin::StringEq { lhs, rhs },
        Builtin::BoolEq { lhs, rhs } => Builtin::BoolEq { lhs, rhs },
        Builtin::ArrayCreateUnsafeUninitialized {
            element_type,
            length,
        } => Builtin::ArrayCreateUnsafeUninitialized {
            element_type,
            length,
        },
        Builtin::ArrayLength {
            element_type,
            array,
        } => Builtin::ArrayLength {
            element_type,
            array,
        },
        Builtin::ArrayGet {
            element_type,
            array,
            index,
        } => Builtin::ArrayGet {
            element_type,
            array,
            index,
        },
        Builtin::ArraySet {
            element_type,
            array,
            index,
            value,
        } => Builtin::ArraySet {
            element_type,
            array,
            index,
            value,
        },
        Builtin::EqualToRefl { r#type, value } => Builtin::EqualToRefl { r#type, value },
        Builtin::UnsafeAssumeErased { r#type } => Builtin::UnsafeAssumeErased { r#type },
        Builtin::Is { value, pattern } => Builtin::Is {
            value,
            pattern: default_shift_located_pattern(shifter, pattern),
        },
    }
}
