use argon_expr::ExprContext;

use crate::*;

/// Read-only traversal of an IR tree.
///
/// Returning `false` stops the traversal, which makes the scanner useful for
/// searches as well as full-tree validation.
pub trait IrScanner {
    type EC: ExprContext + ?Sized;

    fn scan_hole(&mut self, _hole: &<Self::EC as ExprContext>::Hole) -> bool {
        true
    }

    fn scan_value(&mut self, value: &Value<Self::EC>) -> bool {
        default_scan_value(self, value)
    }

    fn scan_instruction(&mut self, instruction: &Instruction<Self::EC>) -> bool {
        default_scan_instruction(self, instruction)
    }

    fn scan_region(&mut self, region: &Region<Self::EC>) -> bool {
        default_scan_region(self, region)
    }

    fn scan_pattern(&mut self, pattern: &LocatedPattern<Self::EC>) -> bool {
        default_scan_pattern(self, pattern)
    }
}

pub fn default_scan_value<S>(scanner: &mut S, value: &Value<S::EC>) -> bool
where
    S: IrScanner + ?Sized,
{
    match value {
        Value::Hole(hole) => scanner.scan_hole(hole),
        Value::Tuple(values) => values.iter().all(|value| scanner.scan_value(value)),
        Value::Type(value)
        | Value::SharedType(value)
        | Value::BoxedType(value)
        | Value::BuiltinType(BuiltinType::Array {
            element_type: value,
        }) => scanner.scan_value(value),
        Value::FunctionResult { result_type } => scanner.scan_value(result_type),
        Value::FunctionType { result, .. } => scanner.scan_value(result),
        Value::ConjunctionType { lhs, rhs } | Value::DisjunctionType { lhs, rhs } => {
            scanner.scan_value(lhs) && scanner.scan_value(rhs)
        }
        Value::EqualToType { r#type, lhs, rhs } => {
            scanner.scan_value(r#type) && scanner.scan_value(lhs) && scanner.scan_value(rhs)
        }
        Value::RecordType(value) => scan_values(scanner, &value.arguments),
        Value::EnumType(value) => scan_values(scanner, &value.arguments),
        Value::TraitType(value) => scan_values(scanner, &value.arguments),
        Value::InstanceType(value) => scan_values(scanner, &value.arguments),
        Value::RecordLiteral {
            record_type,
            fields,
        } => {
            scan_values(scanner, &record_type.arguments)
                && fields.iter().all(|field| scanner.scan_value(&field.value))
        }
        Value::EnumVariantLiteral {
            enum_type,
            arguments,
            fields,
            ..
        } => {
            scan_values(scanner, &enum_type.arguments)
                && scan_values(scanner, arguments)
                && fields.iter().all(|field| scanner.scan_value(&field.value))
        }
        Value::UseType { inner, .. } => scanner.scan_value(inner),
        Value::Error
        | Value::BoolLiteral(_)
        | Value::IntLiteral(_)
        | Value::I8Literal(_)
        | Value::U8Literal(_)
        | Value::I16Literal(_)
        | Value::U16Literal(_)
        | Value::I32Literal(_)
        | Value::U32Literal(_)
        | Value::I64Literal(_)
        | Value::U64Literal(_)
        | Value::StringLiteral(_)
        | Value::BigType(_)
        | Value::BuiltinType(_)
        | Value::Register(_)
        | Value::RegisterSnapshot { .. } => true,
    }
}

pub fn default_scan_instruction<S>(scanner: &mut S, instruction: &Instruction<S::EC>) -> bool
where
    S: IrScanner + ?Sized,
{
    match instruction {
        Instruction::DeclareRegister { declaration } => scanner.scan_value(&declaration.r#type),
        Instruction::Hole { hole, .. } => scanner.scan_hole(hole),
        Instruction::RecordLiteral { record_type, .. } => {
            scan_values(scanner, &record_type.arguments)
        }
        Instruction::EnumVariantLiteral { enum_type, .. } => {
            scan_values(scanner, &enum_type.arguments)
        }
        Instruction::MethodCall { instance_type, .. }
        | Instruction::StaticMethodCall {
            owner_type: instance_type,
            ..
        } => scan_method_instance_type(scanner, instance_type),
        Instruction::RecordFieldLoad { record_type, .. }
        | Instruction::RecordFieldStore { record_type, .. } => {
            scan_values(scanner, &record_type.arguments)
        }
        Instruction::Break { label, .. } | Instruction::Retry { label } => {
            scanner.scan_value(&label.block_result_type)
        }
        Instruction::IfElse {
            when_true,
            when_false,
            ..
        } => scanner.scan_region(when_true) && scanner.scan_region(when_false),
        Instruction::Match { cases, .. } => cases
            .iter()
            .all(|case| scanner.scan_pattern(&case.pattern) && scanner.scan_region(&case.body)),
        Instruction::Block { label, body, .. } => {
            scanner.scan_value(&label.block_result_type) && scanner.scan_region(body)
        }
        Instruction::Finally { body, finally, .. } => {
            scanner.scan_region(body) && scanner.scan_region(finally)
        }
        Instruction::And { rhs, .. } | Instruction::Or { rhs, .. } => scanner.scan_region(rhs),
        Instruction::Closure { body, .. } => scanner.scan_region(body),
        Instruction::Builtin { builtin, .. } => match builtin {
            Builtin::Is { pattern, .. } => scanner.scan_pattern(pattern),
            _ => true,
        },
        _ => true,
    }
}

pub fn default_scan_region<S>(scanner: &mut S, region: &Region<S::EC>) -> bool
where
    S: IrScanner + ?Sized,
{
    region
        .instructions
        .iter()
        .all(|node| scanner.scan_instruction(&node.instruction))
}

pub fn default_scan_pattern<S>(scanner: &mut S, pattern: &LocatedPattern<S::EC>) -> bool
where
    S: IrScanner + ?Sized,
{
    match &pattern.pattern {
        Pattern::Discard { r#type } => scanner.scan_value(r#type),
        Pattern::Tuple(patterns) => patterns.iter().all(|pattern| scanner.scan_pattern(pattern)),
        Pattern::Binding(_, pattern) => scanner.scan_pattern(pattern),
        Pattern::EnumVariant {
            enum_type,
            arguments,
            fields,
            ..
        } => {
            scan_values(scanner, &enum_type.arguments)
                && arguments
                    .iter()
                    .all(|pattern| scanner.scan_pattern(pattern))
                && fields
                    .iter()
                    .all(|field| scanner.scan_pattern(&field.pattern))
        }
        Pattern::Error | Pattern::String(_) | Pattern::Int(_) | Pattern::Bool(_) => true,
    }
}

fn scan_method_instance_type<S>(scanner: &mut S, value: &MethodInstanceType<S::EC>) -> bool
where
    S: IrScanner + ?Sized,
{
    match value {
        MethodInstanceType::Record(value) => scan_values(scanner, &value.arguments),
        MethodInstanceType::Enum(value) => scan_values(scanner, &value.arguments),
        MethodInstanceType::Trait(value) => scan_values(scanner, &value.arguments),
    }
}

fn scan_values<S>(scanner: &mut S, values: &[Value<S::EC>]) -> bool
where
    S: IrScanner + ?Sized,
{
    values.iter().all(|value| scanner.scan_value(value))
}
