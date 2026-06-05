use crate::{
    Expr, ExprContext, ExpressionOwner, LocalVariable, MatchCase, ParameterVariable,
    RecordFieldLiteral, RecordType, Variable,
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
        Expr::Break { label } => Expr::Break { label },
        Expr::Builtin { builtin, arguments } => Expr::Builtin {
            builtin,
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        },
        Expr::EnumType(enum_ec, arguments) => Expr::EnumType(
            enum_ec,
            arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        ),
        Expr::FunctionLiteral {
            parameter_name,
            body,
        } => Expr::FunctionLiteral {
            parameter_name,
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
            a: Box::new(shifter.shift(*a)),
            r: Box::new(shifter.shift(*r)),
        },
        Expr::IfElse {
            when_true_var,
            when_false_var,
            condition,
            when_true,
            when_false,
        } => Expr::IfElse {
            when_true_var: when_true_var.map(|v| shifter.shift_variable(v)),
            when_false_var: when_false_var.map(|v| shifter.shift_variable(v)),
            condition: Box::new(shifter.shift(*condition)),
            when_true: Box::new(shifter.shift(*when_true)),
            when_false: Box::new(shifter.shift(*when_false)),
        },
        Expr::IntLiteral(value) => Expr::IntLiteral(value),
        Expr::Is { value, pattern } => Expr::Is {
            value: Box::new(shifter.shift(*value)),
            pattern,
        },
        Expr::Loop { label, body } => Expr::Loop {
            label,
            body: Box::new(shifter.shift(*body)),
        },
        Expr::Match { value, cases } => Expr::Match {
            value: Box::new(shifter.shift(*value)),
            cases: cases
                .into_iter()
                .map(|case| MatchCase {
                    pattern: case.pattern,
                    body: shifter.shift(case.body),
                })
                .collect(),
        },
        Expr::MethodCall {
            method,
            receiver,
            arguments,
        } => Expr::MethodCall {
            method,
            receiver: Box::new(shifter.shift(*receiver)),
            arguments: arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        },
        Expr::NewTraitObject { trait_ec, body } => Expr::NewTraitObject { trait_ec, body },
        Expr::Next { label } => Expr::Next { label },
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
        Expr::Redo { label } => Expr::Redo { label },
        Expr::Sequence(exprs) => Expr::Sequence(
            Vec1::try_from(
                exprs
                    .into_iter()
                    .map(|expr| shifter.shift(expr))
                    .collect::<Vec<_>>(),
            )
            .expect("shifting a non-empty expression sequence preserves non-emptiness"),
        ),
        Expr::StoreVariable(variable) => Expr::StoreVariable(shifter.shift_variable(variable)),
        Expr::StringLiteral(value) => Expr::StringLiteral(value),
        Expr::TraitType(trait_ec, arguments) => Expr::TraitType(
            trait_ec,
            arguments
                .into_iter()
                .map(|argument| shifter.shift(argument))
                .collect(),
        ),
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
        Expr::While {
            label,
            condition,
            body,
        } => Expr::While {
            label,
            condition: Box::new(shifter.shift(*condition)),
            body: Box::new(shifter.shift(*body)),
        },
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

pub fn default_shift_variable<S>(shifter: &mut S, v: Variable<S::EC1>) -> Variable<S::EC2>
where
    S: ExprContextShifter + ?Sized,
{
    match v {
        Variable::Local(variable) => Variable::Local(Box::new(LocalVariable {
            id: variable.id,
            name: variable.name,
            var_type: shifter.shift(variable.var_type),
            erasure_mode: variable.erasure_mode,
            is_witness: variable.is_witness,
            is_mutable: variable.is_mutable,
        })),
        Variable::Parameter(variable) => Variable::Parameter(Box::new(ParameterVariable {
            owner: match variable.owner {
                ExpressionOwner::Function(function) => ExpressionOwner::Function(function),
                ExpressionOwner::Record(record) => ExpressionOwner::Record(record),
                ExpressionOwner::Enum(enum_ec) => ExpressionOwner::Enum(enum_ec),
                ExpressionOwner::Trait(trait_ec) => ExpressionOwner::Trait(trait_ec),
                ExpressionOwner::EnumVariant(enum_variant) => {
                    ExpressionOwner::EnumVariant(enum_variant)
                }
                ExpressionOwner::Method(method) => ExpressionOwner::Method(method),
                ExpressionOwner::Instance(instance) => ExpressionOwner::Instance(instance),
            },
            parameter_index: variable.parameter_index,
            var_type: shifter.shift(variable.var_type),
            name: variable.name,
            erasure_mode: variable.erasure_mode,
            is_witness: variable.is_witness,
        })),
    }
}
