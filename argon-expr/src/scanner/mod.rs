use crate::{
    BlockLabel, Builtin, EnumType, Expr, ExprContext, InstanceType, LoopLabels, MatchCase,
    MethodInstanceType, Pattern, RecordFieldLiteral, RecordFieldPattern, RecordType, TraitType,
    Variable,
};

mod normalizer;
mod subst;

pub use normalizer::{Normalizer, NormalizerScanner};
pub use subst::SubstScanner;

pub trait ExprScanner {
    type EC: ExprContext + ?Sized;

    fn scan_hole(&mut self, _hole: &<Self::EC as ExprContext>::Hole) -> bool {
        true
    }

    fn scan(&mut self, expr: &Expr<Self::EC>) -> bool {
        default_scan(self, expr)
    }

    fn scan_variable(&mut self, v: &Variable<Self::EC>) -> bool {
        default_scan_variable(self, v)
    }

    fn scan_pattern(&mut self, pattern: &Pattern<Self::EC>) -> bool {
        default_scan_pattern(self, pattern)
    }
}

pub trait ExprScannerMut {
    type EC: ExprContext + ?Sized;

    fn scan_hole(&mut self, _hole: &mut <Self::EC as ExprContext>::Hole) -> bool {
        true
    }

    fn scan(&mut self, expr: &mut Expr<Self::EC>) -> bool {
        default_scan_mut(self, expr)
    }

    fn scan_variable(&mut self, v: &mut Variable<Self::EC>) -> bool {
        default_scan_variable_mut(self, v)
    }

    fn scan_pattern(&mut self, pattern: &mut Pattern<Self::EC>) -> bool {
        default_scan_pattern_mut(self, pattern)
    }
}

pub fn default_scan<S>(scanner: &mut S, expr: &Expr<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    match expr {
        Expr::Error => true,
        Expr::Hole(hole) => scanner.scan_hole(hole),
        Expr::And(a, b) => scanner.scan(a) && scanner.scan(b),
        Expr::Finally {
            block_body,
            finally_body: ensures_body,
        } => scanner.scan(block_body) && scanner.scan(ensures_body),
        Expr::BoolLiteral(_) => true,
        Expr::Block { label, body } => default_scan_label(scanner, label) && scanner.scan(body),
        Expr::Break { label, value } => default_scan_label(scanner, label) && scanner.scan(value),
        Expr::Builtin(builtin) => default_scan_builtin(scanner, builtin),
        Expr::Condition {
            value,
            when_true_witness,
            when_false_witness,
        } => {
            scanner.scan(value)
                && when_true_witness
                    .as_ref()
                    .is_none_or(|variable| scanner.scan(&variable.var_type))
                && when_false_witness
                    .as_ref()
                    .is_none_or(|variable| scanner.scan(&variable.var_type))
        }
        Expr::EnumType(enum_type) => default_scan_enum_type(scanner, enum_type),
        Expr::EnumVariantLiteral {
            enum_type,
            arguments,
            fields,
            ..
        } => {
            default_scan_enum_type(scanner, enum_type)
                && arguments.iter().all(|argument| scanner.scan(argument))
                && fields
                    .iter()
                    .all(|field: &RecordFieldLiteral<S::EC>| scanner.scan(&field.value))
        }
        Expr::Closure {
            v,
            return_type,
            body,
        } => scanner.scan(&v.var_type) && scanner.scan(return_type) && scanner.scan(body),
        Expr::FunctionCall { arguments, .. } => {
            arguments.iter().all(|argument| scanner.scan(argument))
        }
        Expr::FunctionObjectCall { function, argument } => {
            scanner.scan(function) && scanner.scan(argument)
        }
        Expr::FunctionResultValue => true,
        Expr::FunctionType { a, r } => scanner.scan(&a.var_type) && scanner.scan(r),
        Expr::IfElse {
            condition,
            when_true,
            when_false,
        } => scanner.scan(condition) && scanner.scan(when_true) && scanner.scan(when_false),
        Expr::InstanceType(instance_type) => default_scan_instance_type(scanner, instance_type),
        Expr::IntLiteral(_) => true,
        Expr::Is { value, pattern } => scanner.scan(value) && scanner.scan_pattern(pattern),
        Expr::Match { value, cases } => {
            scanner.scan(value)
                && cases.iter().all(|case: &MatchCase<S::EC>| {
                    scanner.scan_pattern(&case.pattern) && scanner.scan(&case.body)
                })
        }
        Expr::MethodCall {
            instance_type,
            receiver,
            arguments,
            ..
        } => {
            default_scan_method_instance_type(scanner, instance_type)
                && scanner.scan(receiver)
                && arguments.iter().all(|argument| scanner.scan(argument))
        }
        Expr::NewInstance { arguments, .. } => {
            arguments.iter().all(|argument| scanner.scan(argument))
        }
        Expr::Not(value) => scanner.scan(value),
        Expr::Or(a, b) => scanner.scan(a) && scanner.scan(b),
        Expr::Raise { ex } => scanner.scan(ex),
        Expr::RecordFieldLoad {
            record_type,
            record_value,
            ..
        } => scanner.scan(record_type) && scanner.scan(record_value),
        Expr::RecordFieldStore {
            record_type,
            record_value,
            new_value,
            ..
        } => scanner.scan(record_type) && scanner.scan(record_value) && scanner.scan(new_value),
        Expr::RecordLiteral {
            record_type,
            fields,
        } => {
            default_scan_record_type(scanner, record_type)
                && fields
                    .iter()
                    .all(|field: &RecordFieldLiteral<S::EC>| scanner.scan(&field.value))
        }
        Expr::RecordType(record_type) => default_scan_record_type(scanner, record_type),
        Expr::Retry { label } => default_scan_label(scanner, label),
        Expr::Sequence(exprs) => exprs.iter().all(|expr| scanner.scan(expr)),
        Expr::StringLiteral(_) => true,
        Expr::TraitType(trait_type) => default_scan_trait_type(scanner, trait_type),
        Expr::Tuple { items } => items.iter().all(|item| scanner.scan(item)),
        Expr::TupleElement(value, _) => scanner.scan(value),
        Expr::Type(t) => scanner.scan(t),
        Expr::BigType(_) => true,
        Expr::Variable(variable) => scanner.scan_variable(variable),
        Expr::VariableBinding(variable, value) => {
            scanner.scan(&variable.var_type) && scanner.scan(value)
        }
        Expr::VariableStore(variable, value) => {
            scanner.scan_variable(variable) && scanner.scan(value)
        }
        Expr::BoxedType(t) => scanner.scan(t),
        Expr::Box { t, value } => scanner.scan(t) && scanner.scan(value),
        Expr::Unbox { t, value } => scanner.scan(t) && scanner.scan(value),
    }
}

fn default_scan_label<S>(scanner: &mut S, label: &BlockLabel<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    scanner.scan(&label.block_result_type)
}

fn default_scan_builtin<S>(scanner: &mut S, builtin: &Builtin<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    match builtin {
        Builtin::IntType | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => true,
        Builtin::ArrayType { element_type } => scanner.scan(element_type),
        Builtin::IntNegate { value } | Builtin::IntBitNot { value } => scanner.scan(value),
        Builtin::IntAdd { lhs, rhs }
        | Builtin::IntSub { lhs, rhs }
        | Builtin::IntMul { lhs, rhs }
        | Builtin::IntBitAnd { lhs, rhs }
        | Builtin::IntBitOr { lhs, rhs }
        | Builtin::IntBitXor { lhs, rhs }
        | Builtin::IntBitShiftLeft { lhs, rhs }
        | Builtin::IntBitShiftRight { lhs, rhs }
        | Builtin::IntEq { lhs, rhs }
        | Builtin::IntNe { lhs, rhs }
        | Builtin::IntLt { lhs, rhs }
        | Builtin::IntLe { lhs, rhs }
        | Builtin::IntGt { lhs, rhs }
        | Builtin::IntGe { lhs, rhs }
        | Builtin::StringConcat { lhs, rhs }
        | Builtin::StringEq { lhs, rhs }
        | Builtin::StringNe { lhs, rhs }
        | Builtin::BoolEq { lhs, rhs }
        | Builtin::BoolNe { lhs, rhs }
        | Builtin::ConjunctionType { lhs, rhs }
        | Builtin::DisjunctionType { lhs, rhs }
        | Builtin::EqualToType { lhs, rhs } => scanner.scan(lhs) && scanner.scan(rhs),
        Builtin::ArrayCreateUnsafeUninitialized {
            element_type,
            length,
        } => scanner.scan(element_type) && scanner.scan(length),
        Builtin::ArrayLength {
            element_type,
            array,
        } => scanner.scan(element_type) && scanner.scan(array),
        Builtin::ArrayGet {
            element_type,
            array,
            index,
        } => scanner.scan(element_type) && scanner.scan(array) && scanner.scan(index),
        Builtin::ArraySet {
            element_type,
            array,
            index,
            value,
        } => {
            scanner.scan(element_type)
                && scanner.scan(array)
                && scanner.scan(index)
                && scanner.scan(value)
        }
    }
}

pub fn default_scan_loop_labels<S>(scanner: &mut S, labels: &LoopLabels<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    match labels {
        LoopLabels::Loop(label) => default_scan_label(scanner, label),
        LoopLabels::While { outer, inner } => {
            default_scan_label(scanner, outer) && default_scan_label(scanner, inner)
        }
    }
}

fn default_scan_record_type<S>(scanner: &mut S, record_type: &RecordType<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    record_type
        .arguments
        .iter()
        .all(|argument| scanner.scan(argument))
}

fn default_scan_enum_type<S>(scanner: &mut S, enum_type: &EnumType<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    enum_type
        .arguments
        .iter()
        .all(|argument| scanner.scan(argument))
}

fn default_scan_trait_type<S>(scanner: &mut S, trait_type: &TraitType<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    trait_type
        .arguments
        .iter()
        .all(|argument| scanner.scan(argument))
}

fn default_scan_method_instance_type<S>(
    scanner: &mut S,
    instance_type: &MethodInstanceType<S::EC>,
) -> bool
where
    S: ExprScanner + ?Sized,
{
    match instance_type {
        MethodInstanceType::Trait(trait_type) => default_scan_trait_type(scanner, trait_type),
    }
}

fn default_scan_instance_type<S>(scanner: &mut S, instance_type: &InstanceType<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    instance_type
        .arguments
        .iter()
        .all(|argument| scanner.scan(argument))
}

pub fn default_scan_variable<S>(scanner: &mut S, v: &Variable<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    match v {
        Variable::Local(variable) => scanner.scan(&variable.var_type),
        Variable::Parameter(variable) => scanner.scan(&variable.var_type),
        Variable::InstanceParameter(variable) => scanner.scan(&variable.var_type),
        Variable::ClosureParameter(variable) => scanner.scan(&variable.var_type),
    }
}

pub fn default_scan_pattern<S>(scanner: &mut S, pattern: &Pattern<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    match pattern {
        Pattern::Error | Pattern::String(_) | Pattern::Int(_) | Pattern::Bool(_) => true,
        Pattern::Discard { t } => scanner.scan(t),
        Pattern::Tuple(patterns) => patterns.iter().all(|pattern| scanner.scan_pattern(pattern)),
        Pattern::Binding(variable, pattern) => {
            scanner.scan(&variable.var_type) && scanner.scan_pattern(pattern)
        }
        Pattern::EnumVariant {
            enum_type,
            args,
            fields,
            ..
        } => {
            default_scan_enum_type(scanner, enum_type)
                && args.iter().all(|arg| scanner.scan_pattern(arg))
                && fields
                    .iter()
                    .all(|field: &RecordFieldPattern<S::EC>| scanner.scan_pattern(&field.pattern))
        }
    }
}

pub fn default_scan_mut<S>(scanner: &mut S, expr: &mut Expr<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    match expr {
        Expr::Error => true,
        Expr::Hole(hole) => scanner.scan_hole(hole),
        Expr::And(a, b) => scanner.scan(a.as_mut()) && scanner.scan(b.as_mut()),
        Expr::Finally {
            block_body,
            finally_body: ensures_body,
        } => scanner.scan(block_body.as_mut()) && scanner.scan(ensures_body.as_mut()),
        Expr::BoolLiteral(_) => true,
        Expr::Block { label, body } => {
            default_scan_label_mut(scanner, label) && scanner.scan(body.as_mut())
        }
        Expr::Break { label, value } => {
            default_scan_label_mut(scanner, label) && scanner.scan(value.as_mut())
        }
        Expr::Builtin(builtin) => default_scan_builtin_mut(scanner, builtin),
        Expr::Condition {
            value,
            when_true_witness,
            when_false_witness,
        } => {
            scanner.scan(value.as_mut())
                && when_true_witness
                    .as_mut()
                    .is_none_or(|variable| scanner.scan(&mut variable.var_type))
                && when_false_witness
                    .as_mut()
                    .is_none_or(|variable| scanner.scan(&mut variable.var_type))
        }
        Expr::EnumType(enum_type) => default_scan_enum_type_mut(scanner, enum_type),
        Expr::EnumVariantLiteral {
            enum_type,
            arguments,
            fields,
            ..
        } => {
            default_scan_enum_type_mut(scanner, enum_type)
                && arguments.iter_mut().all(|argument| scanner.scan(argument))
                && fields
                    .iter_mut()
                    .all(|field: &mut RecordFieldLiteral<S::EC>| scanner.scan(&mut field.value))
        }
        Expr::Closure {
            v,
            return_type,
            body,
        } => {
            scanner.scan(&mut v.var_type)
                && scanner.scan(return_type.as_mut())
                && scanner.scan(body.as_mut())
        }
        Expr::FunctionCall { arguments, .. } => {
            arguments.iter_mut().all(|argument| scanner.scan(argument))
        }
        Expr::FunctionObjectCall { function, argument } => {
            scanner.scan(function.as_mut()) && scanner.scan(argument.as_mut())
        }
        Expr::FunctionResultValue => true,
        Expr::FunctionType { a, r } => scanner.scan(&mut a.var_type) && scanner.scan(r.as_mut()),
        Expr::IfElse {
            condition,
            when_true,
            when_false,
        } => {
            scanner.scan(condition.as_mut())
                && scanner.scan(when_true.as_mut())
                && scanner.scan(when_false.as_mut())
        }
        Expr::InstanceType(instance_type) => default_scan_instance_type_mut(scanner, instance_type),
        Expr::IntLiteral(_) => true,
        Expr::Is { value, pattern } => {
            scanner.scan(value.as_mut()) && scanner.scan_pattern(pattern.as_mut())
        }
        Expr::Match { value, cases } => {
            scanner.scan(value.as_mut())
                && cases.iter_mut().all(|case: &mut MatchCase<S::EC>| {
                    scanner.scan_pattern(&mut case.pattern) && scanner.scan(&mut case.body)
                })
        }
        Expr::MethodCall {
            instance_type,
            receiver,
            arguments,
            ..
        } => {
            default_scan_method_instance_type_mut(scanner, instance_type)
                && scanner.scan(receiver.as_mut())
                && arguments.iter_mut().all(|argument| scanner.scan(argument))
        }
        Expr::NewInstance { arguments, .. } => {
            arguments.iter_mut().all(|argument| scanner.scan(argument))
        }
        Expr::Not(value) => scanner.scan(value.as_mut()),
        Expr::Or(a, b) => scanner.scan(a.as_mut()) && scanner.scan(b.as_mut()),
        Expr::Raise { ex } => scanner.scan(ex.as_mut()),
        Expr::RecordFieldLoad {
            record_type,
            record_value,
            ..
        } => scanner.scan(record_type.as_mut()) && scanner.scan(record_value.as_mut()),
        Expr::RecordFieldStore {
            record_type,
            record_value,
            new_value,
            ..
        } => {
            scanner.scan(record_type.as_mut())
                && scanner.scan(record_value.as_mut())
                && scanner.scan(new_value.as_mut())
        }
        Expr::RecordLiteral {
            record_type,
            fields,
        } => {
            default_scan_record_type_mut(scanner, record_type)
                && fields
                    .iter_mut()
                    .all(|field: &mut RecordFieldLiteral<S::EC>| scanner.scan(&mut field.value))
        }
        Expr::RecordType(record_type) => default_scan_record_type_mut(scanner, record_type),
        Expr::Retry { label } => default_scan_label_mut(scanner, label),
        Expr::Sequence(exprs) => exprs.iter_mut().all(|expr| scanner.scan(expr)),
        Expr::StringLiteral(_) => true,
        Expr::TraitType(trait_type) => default_scan_trait_type_mut(scanner, trait_type),
        Expr::Tuple { items } => items.iter_mut().all(|item| scanner.scan(item)),
        Expr::TupleElement(value, _) => scanner.scan(value.as_mut()),
        Expr::Type(t) => scanner.scan(t.as_mut()),
        Expr::BigType(_) => true,
        Expr::Variable(variable) => scanner.scan_variable(variable),
        Expr::VariableBinding(variable, value) => {
            scanner.scan(&mut variable.var_type) && scanner.scan(value.as_mut())
        }
        Expr::VariableStore(variable, value) => {
            scanner.scan_variable(variable) && scanner.scan(value.as_mut())
        }
        Expr::BoxedType(t) => scanner.scan(t.as_mut()),
        Expr::Box { t, value } => scanner.scan(t.as_mut()) && scanner.scan(value.as_mut()),
        Expr::Unbox { t, value } => scanner.scan(t.as_mut()) && scanner.scan(value.as_mut()),
    }
}

fn default_scan_label_mut<S>(scanner: &mut S, label: &mut BlockLabel<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    scanner.scan(&mut label.block_result_type)
}

fn default_scan_builtin_mut<S>(scanner: &mut S, builtin: &mut Builtin<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    match builtin {
        Builtin::IntType | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => true,
        Builtin::ArrayType { element_type } => scanner.scan(element_type),
        Builtin::IntNegate { value } | Builtin::IntBitNot { value } => scanner.scan(value),
        Builtin::IntAdd { lhs, rhs }
        | Builtin::IntSub { lhs, rhs }
        | Builtin::IntMul { lhs, rhs }
        | Builtin::IntBitAnd { lhs, rhs }
        | Builtin::IntBitOr { lhs, rhs }
        | Builtin::IntBitXor { lhs, rhs }
        | Builtin::IntBitShiftLeft { lhs, rhs }
        | Builtin::IntBitShiftRight { lhs, rhs }
        | Builtin::IntEq { lhs, rhs }
        | Builtin::IntNe { lhs, rhs }
        | Builtin::IntLt { lhs, rhs }
        | Builtin::IntLe { lhs, rhs }
        | Builtin::IntGt { lhs, rhs }
        | Builtin::IntGe { lhs, rhs }
        | Builtin::StringConcat { lhs, rhs }
        | Builtin::StringEq { lhs, rhs }
        | Builtin::StringNe { lhs, rhs }
        | Builtin::BoolEq { lhs, rhs }
        | Builtin::BoolNe { lhs, rhs }
        | Builtin::ConjunctionType { lhs, rhs }
        | Builtin::DisjunctionType { lhs, rhs }
        | Builtin::EqualToType { lhs, rhs } => scanner.scan(lhs) && scanner.scan(rhs),
        Builtin::ArrayCreateUnsafeUninitialized {
            element_type,
            length,
        } => scanner.scan(element_type) && scanner.scan(length),
        Builtin::ArrayLength {
            element_type,
            array,
        } => scanner.scan(element_type) && scanner.scan(array),
        Builtin::ArrayGet {
            element_type,
            array,
            index,
        } => scanner.scan(element_type) && scanner.scan(array) && scanner.scan(index),
        Builtin::ArraySet {
            element_type,
            array,
            index,
            value,
        } => {
            scanner.scan(element_type)
                && scanner.scan(array)
                && scanner.scan(index)
                && scanner.scan(value)
        }
    }
}

pub fn default_scan_loop_labels_mut<S>(scanner: &mut S, labels: &mut LoopLabels<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    match labels {
        LoopLabels::Loop(label) => default_scan_label_mut(scanner, label),
        LoopLabels::While { outer, inner } => {
            default_scan_label_mut(scanner, outer) && default_scan_label_mut(scanner, inner)
        }
    }
}

fn default_scan_record_type_mut<S>(scanner: &mut S, record_type: &mut RecordType<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    record_type
        .arguments
        .iter_mut()
        .all(|argument| scanner.scan(argument))
}

fn default_scan_enum_type_mut<S>(scanner: &mut S, enum_type: &mut EnumType<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    enum_type
        .arguments
        .iter_mut()
        .all(|argument| scanner.scan(argument))
}

fn default_scan_trait_type_mut<S>(scanner: &mut S, trait_type: &mut TraitType<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    trait_type
        .arguments
        .iter_mut()
        .all(|argument| scanner.scan(argument))
}

fn default_scan_method_instance_type_mut<S>(
    scanner: &mut S,
    instance_type: &mut MethodInstanceType<S::EC>,
) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    match instance_type {
        MethodInstanceType::Trait(trait_type) => default_scan_trait_type_mut(scanner, trait_type),
    }
}

fn default_scan_instance_type_mut<S>(
    scanner: &mut S,
    instance_type: &mut InstanceType<S::EC>,
) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    instance_type
        .arguments
        .iter_mut()
        .all(|argument| scanner.scan(argument))
}

pub fn default_scan_variable_mut<S>(scanner: &mut S, v: &mut Variable<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    match v {
        Variable::Local(variable) => scanner.scan(&mut variable.var_type),
        Variable::Parameter(variable) => scanner.scan(&mut variable.var_type),
        Variable::InstanceParameter(variable) => scanner.scan(&mut variable.var_type),
        Variable::ClosureParameter(variable) => scanner.scan(&mut variable.var_type),
    }
}

pub fn default_scan_pattern_mut<S>(scanner: &mut S, pattern: &mut Pattern<S::EC>) -> bool
where
    S: ExprScannerMut + ?Sized,
{
    match pattern {
        Pattern::Error | Pattern::String(_) | Pattern::Int(_) | Pattern::Bool(_) => true,
        Pattern::Discard { t } => scanner.scan(t.as_mut()),
        Pattern::Tuple(patterns) => patterns
            .iter_mut()
            .all(|pattern| scanner.scan_pattern(pattern)),
        Pattern::Binding(variable, pattern) => {
            scanner.scan(&mut variable.var_type) && scanner.scan_pattern(pattern.as_mut())
        }
        Pattern::EnumVariant {
            enum_type,
            args,
            fields,
            ..
        } => {
            default_scan_enum_type_mut(scanner, enum_type)
                && args.iter_mut().all(|arg| scanner.scan_pattern(arg))
                && fields
                    .iter_mut()
                    .all(|field: &mut RecordFieldPattern<S::EC>| {
                        scanner.scan_pattern(&mut field.pattern)
                    })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ExprScanner, ExprScannerMut};
    use crate::{Builtin, Expr, ExprContext};
    use alloc::vec;
    use alloc::vec::Vec;

    #[derive(Debug, Eq, Hash, PartialEq)]
    struct TestContext;

    impl ExprContext for TestContext {
        type Hole = u8;
        type Function = ();
        type Record = ();
        type RecordField = ();
        type Enum = ();
        type Trait = ();
        type EnumVariant = ();
        type Method = ();
        type Instance = ();
    }

    struct StopAtHole {
        stop_at: u8,
        visited: Vec<u8>,
    }

    impl ExprScanner for StopAtHole {
        type EC = TestContext;

        fn scan_hole(&mut self, hole: &u8) -> bool {
            self.visited.push(*hole);
            *hole != self.stop_at
        }
    }

    #[test]
    fn scanner_short_circuits_when_scan_returns_false() {
        let expr = Expr::Builtin(Builtin::BoolEq {
            lhs: Box::new(Expr::Hole(1)),
            rhs: Box::new(Expr::Hole(2)),
        });
        let mut scanner = StopAtHole {
            stop_at: 2,
            visited: Vec::new(),
        };

        assert!(!scanner.scan(&expr));
        assert_eq!(scanner.visited, vec![1, 2]);
    }

    struct ReplaceHoles;

    impl ExprScannerMut for ReplaceHoles {
        type EC = TestContext;

        fn scan_hole(&mut self, hole: &mut u8) -> bool {
            *hole += 10;
            true
        }
    }

    #[test]
    fn mutable_scanner_can_modify_exprs() {
        let mut expr = Expr::Builtin(Builtin::BoolEq {
            lhs: Box::new(Expr::Hole(1)),
            rhs: Box::new(Expr::Hole(2)),
        });

        assert!(ReplaceHoles.scan(&mut expr));

        assert!(matches!(
            expr,
            Expr::Builtin(Builtin::BoolEq { lhs, rhs })
                if matches!((&*lhs, &*rhs), (Expr::Hole(11), Expr::Hole(12)))
        ));
    }
}
