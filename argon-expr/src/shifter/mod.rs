use crate::{
    BlockLabel, ClosureParameterVariable, EnumType, Expr, ExprContext, ExpressionOwner,
    InstanceParameterVariable, InstanceType, LocalVariable, LoopLabels, MatchCase,
    MethodInstanceType, ParameterVariable, Pattern, RecordFieldLiteral, RecordFieldPattern,
    RecordType, TraitType, Variable,
};
use alloc::{boxed::Box, vec::Vec};
use mitsein::vec1::Vec1;

mod fresh_var;

pub use fresh_var::FreshVariableShifter;

pub trait ExprContextShifter {
    type EC1: ExprContext + ?Sized;
    type EC2: ExprContext<
            Function = <Self::EC1 as ExprContext>::Function,
            Record = <Self::EC1 as ExprContext>::Record,
            Enum = <Self::EC1 as ExprContext>::Enum,
            Trait = <Self::EC1 as ExprContext>::Trait,
            RecordField = <Self::EC1 as ExprContext>::RecordField,
            EnumVariant = <Self::EC1 as ExprContext>::EnumVariant,
            Method = <Self::EC1 as ExprContext>::Method,
            Instance = <Self::EC1 as ExprContext>::Instance,
        > + ?Sized;

    fn shift_hole(&mut self, hole: <Self::EC1 as ExprContext>::Hole) -> Expr<Self::EC2>;

    fn shift(&mut self, expr: Expr<Self::EC1>) -> Expr<Self::EC2> {
        default_shift(self, expr)
    }

    fn shift_variable(&mut self, v: Variable<Self::EC1>) -> Variable<Self::EC2> {
        default_shift_variable(self, v)
    }
}

pub fn default_shift<S>(shifter: &mut S, expr: Expr<S::EC1>) -> Expr<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    match expr {
        Expr::Error => Expr::Error,
        Expr::Hole(hole) => shifter.shift_hole(hole),
        Expr::And(a, b) => Expr::And(Box::new(shifter.shift(*a)), Box::new(shifter.shift(*b))),
        Expr::Finally {
            block_body,
            finally_body: ensures_body,
        } => Expr::Finally {
            block_body: Box::new(shifter.shift(*block_body)),
            finally_body: Box::new(shifter.shift(*ensures_body)),
        },
        Expr::BoolLiteral(value) => Expr::BoolLiteral(value),
        Expr::Block { label, body } => Expr::Block {
            label: Box::new(default_shift_label(shifter, *label)),
            body: Box::new(shifter.shift(*body)),
        },
        Expr::Break { label, value } => Expr::Break {
            label: Box::new(default_shift_label(shifter, *label)),
            value: Box::new(shifter.shift(*value)),
        },
        Expr::BreakIf {
            label,
            value,
            condition,
        } => Expr::BreakIf {
            label: Box::new(default_shift_label(shifter, *label)),
            value: Box::new(shifter.shift(*value)),
            condition: Box::new(shifter.shift(*condition)),
        },
        Expr::Builtin { builtin, arguments } => Expr::Builtin {
            builtin,
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        },
        Expr::Condition {
            value,
            when_true_witness,
            when_false_witness,
        } => Expr::Condition {
            value: Box::new(shifter.shift(*value)),
            when_true_witness: when_true_witness.map(|v| shifter.shift_variable(v)),
            when_false_witness: when_false_witness.map(|v| shifter.shift_variable(v)),
        },
        Expr::EnumType(enum_type) => Expr::EnumType(default_shift_enum_type(shifter, enum_type)),
        Expr::EnumVariantLiteral {
            enum_type,
            variant,
            arguments,
            fields,
        } => Expr::EnumVariantLiteral {
            enum_type: default_shift_enum_type(shifter, enum_type),
            variant,
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
            fields: fields
                .into_iter()
                .map(|field| RecordFieldLiteral {
                    name: field.name,
                    value: shifter.shift(field.value),
                })
                .collect(),
        },
        Expr::Closure {
            v,
            return_type,
            body,
        } => Expr::Closure {
            v: Box::new(default_shift_closure_parameter_variable(shifter, *v)),
            return_type: Box::new(shifter.shift(*return_type)),
            body: Box::new(shifter.shift(*body)),
        },
        Expr::FunctionCall {
            function,
            arguments,
        } => Expr::FunctionCall {
            function,
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        },
        Expr::FunctionObjectCall { function, argument } => Expr::FunctionObjectCall {
            function: Box::new(shifter.shift(*function)),
            argument: Box::new(shifter.shift(*argument)),
        },
        Expr::FunctionResultValue => Expr::FunctionResultValue,
        Expr::FunctionType { a, r } => Expr::FunctionType {
            a: Box::new(default_shift_closure_parameter_variable(shifter, *a)),
            r: Box::new(shifter.shift(*r)),
        },
        Expr::IfElse {
            condition,
            when_true,
            when_false,
        } => Expr::IfElse {
            condition: Box::new(shifter.shift(*condition)),
            when_true: Box::new(shifter.shift(*when_true)),
            when_false: Box::new(shifter.shift(*when_false)),
        },
        Expr::InstanceType(instance_type) => {
            Expr::InstanceType(default_shift_instance_type(shifter, instance_type))
        }
        Expr::IntLiteral(value) => Expr::IntLiteral(value),
        Expr::Is { value, pattern } => Expr::Is {
            value: Box::new(shifter.shift(*value)),
            pattern: Box::new(default_shift_pattern(shifter, *pattern)),
        },
        Expr::Match { value, cases } => Expr::Match {
            value: Box::new(shifter.shift(*value)),
            cases: cases
                .into_iter()
                .map(|case| MatchCase {
                    pattern: default_shift_pattern(shifter, case.pattern),
                    body: shifter.shift(case.body),
                })
                .collect(),
        },
        Expr::MethodCall {
            method,
            instance_type,
            receiver,
            arguments,
        } => Expr::MethodCall {
            method,
            instance_type: default_shift_method_instance_type(shifter, instance_type),
            receiver: Box::new(shifter.shift(*receiver)),
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        },
        Expr::NewInstance {
            instance,
            arguments,
        } => Expr::NewInstance {
            instance,
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        },
        Expr::Not(value) => Expr::Not(Box::new(shifter.shift(*value))),
        Expr::Or(a, b) => Expr::Or(Box::new(shifter.shift(*a)), Box::new(shifter.shift(*b))),
        Expr::Raise { ex } => Expr::Raise {
            ex: Box::new(shifter.shift(*ex)),
        },
        Expr::RecordFieldLoad {
            record_type,
            field,
            record_value,
        } => Expr::RecordFieldLoad {
            record_type: Box::new(shifter.shift(*record_type)),
            field,
            record_value: Box::new(shifter.shift(*record_value)),
        },
        Expr::RecordFieldStore {
            record_type,
            field,
            record_value,
            new_value,
        } => Expr::RecordFieldStore {
            record_type: Box::new(shifter.shift(*record_type)),
            field,
            record_value: Box::new(shifter.shift(*record_value)),
            new_value: Box::new(shifter.shift(*new_value)),
        },
        Expr::RecordLiteral {
            record_type,
            fields,
        } => Expr::RecordLiteral {
            record_type: default_shift_record_type(shifter, record_type),
            fields: fields
                .into_iter()
                .map(|field| RecordFieldLiteral {
                    name: field.name,
                    value: shifter.shift(field.value),
                })
                .collect(),
        },
        Expr::RecordType(record_type) => {
            Expr::RecordType(default_shift_record_type(shifter, record_type))
        }
        Expr::Retry { label } => Expr::Retry {
            label: Box::new(default_shift_label(shifter, *label)),
        },
        Expr::Sequence(exprs) => Expr::Sequence(
            Vec1::try_from(
                exprs
                    .into_iter()
                    .map(|expr| shifter.shift(expr))
                    .collect::<Vec<_>>(),
            )
            .expect("shifting a non-empty expression sequence preserves non-emptiness"),
        ),
        Expr::StringLiteral(value) => Expr::StringLiteral(value),
        Expr::TraitType(trait_type) => {
            Expr::TraitType(default_shift_trait_type(shifter, trait_type))
        }
        Expr::Tuple { items } => Expr::Tuple {
            items: items.into_iter().map(|item| shifter.shift(item)).collect(),
        },
        Expr::TupleElement(value, index) => {
            Expr::TupleElement(Box::new(shifter.shift(*value)), index)
        }
        Expr::Type(t) => Expr::Type(Box::new(shifter.shift(*t))),
        Expr::BigType(value) => Expr::BigType(value),
        Expr::Variable(variable) => Expr::Variable(shifter.shift_variable(variable)),
        Expr::VariableBinding(variable, value) => Expr::VariableBinding(
            shifter.shift_variable(variable),
            Box::new(shifter.shift(*value)),
        ),
        Expr::VariableStore(variable, value) => Expr::VariableStore(
            shifter.shift_variable(variable),
            Box::new(shifter.shift(*value)),
        ),
        Expr::BoxedType { t } => Expr::BoxedType {
            t: Box::new(shifter.shift(*t)),
        },
        Expr::Box { t, value } => Expr::Box {
            t: Box::new(shifter.shift(*t)),
            value: Box::new(shifter.shift(*value)),
        },
        Expr::Unbox { t, value } => Expr::Unbox {
            t: Box::new(shifter.shift(*t)),
            value: Box::new(shifter.shift(*value)),
        },
    }
}

fn default_shift_label<S>(shifter: &mut S, label: BlockLabel<S::EC1>) -> BlockLabel<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    BlockLabel {
        id: label.id,
        name: label.name,
        kind: label.kind,
        block_result_type: shifter.shift(label.block_result_type),
    }
}

pub fn default_shift_loop_labels<S>(
    shifter: &mut S,
    labels: LoopLabels<S::EC1>,
) -> LoopLabels<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    match labels {
        LoopLabels::Loop(label) => LoopLabels::Loop(default_shift_label(shifter, label)),
        LoopLabels::While { outer, inner } => LoopLabels::While {
            outer: default_shift_label(shifter, outer),
            inner: default_shift_label(shifter, inner),
        },
    }
}

fn default_shift_record_type<S>(
    shifter: &mut S,
    record_type: RecordType<S::EC1>,
) -> RecordType<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    RecordType {
        record: record_type.record,
        arguments: record_type
            .arguments
            .into_iter()
            .map(|argument| shifter.shift(argument))
            .collect(),
    }
}

fn default_shift_enum_type<S>(shifter: &mut S, enum_type: EnumType<S::EC1>) -> EnumType<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    EnumType {
        enum_: enum_type.enum_,
        arguments: enum_type
            .arguments
            .into_iter()
            .map(|argument| shifter.shift(argument))
            .collect(),
    }
}

fn default_shift_trait_type<S>(shifter: &mut S, trait_type: TraitType<S::EC1>) -> TraitType<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    TraitType {
        trait_: trait_type.trait_,
        arguments: trait_type
            .arguments
            .into_iter()
            .map(|argument| shifter.shift(argument))
            .collect(),
    }
}

fn default_shift_method_instance_type<S>(
    shifter: &mut S,
    instance_type: MethodInstanceType<S::EC1>,
) -> MethodInstanceType<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    match instance_type {
        MethodInstanceType::Trait(trait_type) => {
            MethodInstanceType::Trait(default_shift_trait_type(shifter, trait_type))
        }
    }
}

fn default_shift_instance_type<S>(
    shifter: &mut S,
    instance_type: InstanceType<S::EC1>,
) -> InstanceType<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    InstanceType {
        instance: instance_type.instance,
        arguments: instance_type
            .arguments
            .into_iter()
            .map(|argument| shifter.shift(argument))
            .collect(),
    }
}

fn default_shift_pattern<S>(shifter: &mut S, pattern: Pattern<S::EC1>) -> Pattern<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    match pattern {
        Pattern::Error => Pattern::Error,
        Pattern::Discard { t } => Pattern::Discard {
            t: Box::new(shifter.shift(*t)),
        },
        Pattern::Tuple(items) => Pattern::Tuple(
            items
                .into_iter()
                .map(|item| default_shift_pattern(shifter, item))
                .collect(),
        ),
        Pattern::Binding(variable, pattern) => Pattern::Binding(
            default_shift_local_variable(shifter, variable),
            Box::new(default_shift_pattern(shifter, *pattern)),
        ),
        Pattern::EnumVariant {
            enum_type,
            variant,
            args,
            fields,
        } => Pattern::EnumVariant {
            enum_type: default_shift_enum_type(shifter, enum_type),
            variant,
            args: args
                .into_iter()
                .map(|arg| default_shift_pattern(shifter, arg))
                .collect(),
            fields: fields
                .into_iter()
                .map(|field| RecordFieldPattern {
                    field: field.field,
                    pattern: default_shift_pattern(shifter, field.pattern),
                })
                .collect(),
        },
        Pattern::String(value) => Pattern::String(value),
        Pattern::Int(value) => Pattern::Int(value),
        Pattern::Bool(value) => Pattern::Bool(value),
    }
}

fn default_shift_local_variable<S>(
    shifter: &mut S,
    variable: LocalVariable<S::EC1>,
) -> LocalVariable<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    LocalVariable {
        id: variable.id,
        name: variable.name,
        var_type: shifter.shift(variable.var_type),
        erasure_mode: variable.erasure_mode,
        is_witness: variable.is_witness,
        is_mutable: variable.is_mutable,
    }
}

fn default_shift_closure_parameter_variable<S>(
    shifter: &mut S,
    variable: ClosureParameterVariable<S::EC1>,
) -> ClosureParameterVariable<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    ClosureParameterVariable {
        id: variable.id,
        var_type: shifter.shift(variable.var_type),
        name: variable.name,
        is_mutable: variable.is_mutable,
        erasure_mode: variable.erasure_mode,
        is_witness: variable.is_witness,
    }
}

fn default_shift_expression_owner<S>(owner: ExpressionOwner<S::EC1>) -> ExpressionOwner<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    match owner {
        ExpressionOwner::Function(function) => ExpressionOwner::Function(function),
        ExpressionOwner::Record(record) => ExpressionOwner::Record(record),
        ExpressionOwner::Enum(enum_ec) => ExpressionOwner::Enum(enum_ec),
        ExpressionOwner::Trait(trait_ec) => ExpressionOwner::Trait(trait_ec),
        ExpressionOwner::EnumVariant(enum_variant) => ExpressionOwner::EnumVariant(enum_variant),
        ExpressionOwner::Method(method) => ExpressionOwner::Method(method),
        ExpressionOwner::Instance(instance) => ExpressionOwner::Instance(instance),
    }
}

pub fn default_shift_variable<S>(shifter: &mut S, v: Variable<S::EC1>) -> Variable<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    match v {
        Variable::Local(variable) => {
            Variable::Local(Box::new(default_shift_local_variable(shifter, *variable)))
        }
        Variable::Parameter(variable) => Variable::Parameter(Box::new(ParameterVariable {
            owner: default_shift_expression_owner::<S>(variable.owner),
            parameter_index: variable.parameter_index,
            var_type: shifter.shift(variable.var_type),
            name: variable.name,
            erasure_mode: variable.erasure_mode,
            is_witness: variable.is_witness,
        })),
        Variable::InstanceParameter(variable) => {
            Variable::InstanceParameter(Box::new(InstanceParameterVariable {
                owner: default_shift_expression_owner::<S>(variable.owner),
                var_type: shifter.shift(variable.var_type),
                name: variable.name,
            }))
        }
        Variable::ClosureParameter(variable) => Variable::ClosureParameter(Box::new(
            default_shift_closure_parameter_variable(shifter, *variable),
        )),
    }
}
