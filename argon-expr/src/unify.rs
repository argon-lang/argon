use crate::{
    Builtin, Expr, ExprContext, MatchCase, MethodInstanceType, Normalizer, NormalizerScanner,
    Pattern, RecordFieldLiteral, RecordFieldPattern, SubstScanner, Variable,
};
use alloc::{borrow::Cow, vec::Vec};
use argon_util::Fuel;

pub trait Unify {
    type EC: ExprContext;

    type Model: Clone;

    type Norm<'a>: Normalizer<EC = Self::EC>
    where
        Self: 'a;

    fn model(&self) -> &Self::Model;
    fn model_mut(&mut self) -> &mut Self::Model;

    fn normalize_fuel(&self) -> Fuel;
    fn normalizer<'a>(model: &'a mut Self::Model) -> Self::Norm<'a>
    where
        Self: 'a;

    fn unify_hole(&mut self, a: <Self::EC as ExprContext>::Hole, b: Expr<Self::EC>) -> bool;

    fn unify(&mut self, mut a: Expr<Self::EC>, mut b: Expr<Self::EC>) -> bool {
        {
            let mut norm =
                NormalizerScanner::new(self.normalize_fuel(), Self::normalizer(self.model_mut()));
            norm.normalize(&mut a);
        }
        {
            let mut norm =
                NormalizerScanner::new(self.normalize_fuel(), Self::normalizer(self.model_mut()));
            norm.normalize(&mut b);
        }

        match (a, b) {
            (Expr::Error, _) | (_, Expr::Error) => true,
            (Expr::Hole(a), b) => self.unify_hole(a, b),

            (a, Expr::Hole(b)) => self.unify_hole(b, a),

            (Expr::And(a1, b1), Expr::And(a2, b2)) | (Expr::Or(a1, b1), Expr::Or(a2, b2)) => {
                self.unify(*a1, *a2) && self.unify(*b1, *b2)
            }
            (Expr::BoolLiteral(a), Expr::BoolLiteral(b)) => a == b,
            (
                Expr::Block {
                    label: a_label,
                    body: a_body,
                },
                Expr::Block {
                    label: b_label,
                    body: b_body,
                },
            ) => {
                a_label == b_label
                    && self.unify(
                        a_label.block_result_type.clone(),
                        b_label.block_result_type.clone(),
                    )
                    && self.unify(*a_body, *b_body)
            }
            (
                Expr::Break {
                    label: a_label,
                    value: a_value,
                },
                Expr::Break {
                    label: b_label,
                    value: b_value,
                },
            ) => a_label == b_label && self.unify(*a_value, *b_value),
            (Expr::Builtin(a), Expr::Builtin(b)) => self.unify_builtin(a, b),
            (Expr::BoxedType(a), Expr::BoxedType(b)) => self.unify(*a, *b),
            (
                Expr::Box {
                    t: a_type,
                    value: a_value,
                },
                Expr::Box {
                    t: b_type,
                    value: b_value,
                },
            )
            | (
                Expr::Unbox {
                    t: a_type,
                    value: a_value,
                },
                Expr::Unbox {
                    t: b_type,
                    value: b_value,
                },
            ) => self.unify(*a_type, *b_type) && self.unify(*a_value, *b_value),
            (
                Expr::ConjunctionType {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Expr::ConjunctionType {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Expr::DisjunctionType {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Expr::DisjunctionType {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            ) => self.unify(*a_lhs, *b_lhs) && self.unify(*a_rhs, *b_rhs),
            (
                Expr::EqualToType {
                    r#type: a_type,
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Expr::EqualToType {
                    r#type: b_type,
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            ) => {
                self.unify(*a_type, *b_type)
                    && self.unify(*a_lhs, *b_lhs)
                    && self.unify(*a_rhs, *b_rhs)
            }
            (Expr::EnumType(a), Expr::EnumType(b)) => {
                a.enum_ == b.enum_ && self.unify_all(a.arguments, b.arguments)
            }
            (
                Expr::FunctionType {
                    a: a_arg,
                    r: a_result,
                },
                Expr::FunctionType {
                    a: b_arg,
                    r: b_result,
                },
            ) => {
                let mut b_result = *b_result;
                SubstScanner::subst(
                    Variable::ClosureParameter(b_arg.clone()),
                    Cow::Owned(Expr::Variable(Variable::ClosureParameter(a_arg.clone()))),
                    &mut b_result,
                );

                self.unify(a_arg.var_type.clone(), b_arg.var_type.clone())
                    && self.unify(*a_result, b_result)
            }
            (
                Expr::Closure {
                    v: a_arg,
                    return_type: a_return_type,
                    body: a_body,
                },
                Expr::Closure {
                    v: b_arg,
                    return_type: b_return_type,
                    body: b_body,
                },
            ) => {
                let mut b_body = *b_body;
                SubstScanner::subst(
                    Variable::ClosureParameter(b_arg.clone()),
                    Cow::Owned(Expr::Variable(Variable::ClosureParameter(a_arg.clone()))),
                    &mut b_body,
                );

                self.unify(a_arg.var_type.clone(), b_arg.var_type.clone())
                    && self.unify(*a_return_type, *b_return_type)
                    && self.unify(*a_body, b_body)
            }
            (
                Expr::Condition {
                    value: a_value,
                    when_true_witness: a_when_true_witness,
                    when_false_witness: a_when_false_witness,
                },
                Expr::Condition {
                    value: b_value,
                    when_true_witness: b_when_true_witness,
                    when_false_witness: b_when_false_witness,
                },
            ) => {
                a_when_true_witness == b_when_true_witness
                    && a_when_false_witness == b_when_false_witness
                    && self.unify(*a_value, *b_value)
            }
            (
                Expr::EnumVariantLiteral {
                    enum_type: a_enum_type,
                    variant: a_variant,
                    arguments: a_arguments,
                    fields: a_fields,
                },
                Expr::EnumVariantLiteral {
                    enum_type: b_enum_type,
                    variant: b_variant,
                    arguments: b_arguments,
                    fields: b_fields,
                },
            ) => {
                a_enum_type.enum_ == b_enum_type.enum_
                    && a_variant == b_variant
                    && self.unify_all(a_enum_type.arguments, b_enum_type.arguments)
                    && self.unify_all(a_arguments, b_arguments)
                    && self.unify_record_fields(a_fields, b_fields)
            }
            (
                Expr::Finally {
                    block_body: a_block_body,
                    finally_body: a_finally_body,
                },
                Expr::Finally {
                    block_body: b_block_body,
                    finally_body: b_finally_body,
                },
            ) => {
                self.unify(*a_block_body, *b_block_body)
                    && self.unify(*a_finally_body, *b_finally_body)
            }
            (
                Expr::FunctionCall {
                    function: a_function,
                    arguments: a_arguments,
                },
                Expr::FunctionCall {
                    function: b_function,
                    arguments: b_arguments,
                },
            ) => a_function == b_function && self.unify_all(a_arguments, b_arguments),
            (
                Expr::FunctionObjectCall {
                    function: a_function,
                    argument: a_argument,
                },
                Expr::FunctionObjectCall {
                    function: b_function,
                    argument: b_argument,
                },
            ) => self.unify(*a_function, *b_function) && self.unify(*a_argument, *b_argument),
            (Expr::FunctionResultValue, Expr::FunctionResultValue) => true,
            (
                Expr::IfElse {
                    condition: a_condition,
                    when_true: a_when_true,
                    when_false: a_when_false,
                },
                Expr::IfElse {
                    condition: b_condition,
                    when_true: b_when_true,
                    when_false: b_when_false,
                },
            ) => {
                self.unify(*a_condition, *b_condition)
                    && self.unify(*a_when_true, *b_when_true)
                    && self.unify(*a_when_false, *b_when_false)
            }
            (Expr::InstanceType(a), Expr::InstanceType(b)) => {
                a.instance == b.instance && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::IntLiteral(a), Expr::IntLiteral(b)) => a == b,
            (
                Expr::Is {
                    value: a_value,
                    pattern: a_pattern,
                },
                Expr::Is {
                    value: b_value,
                    pattern: b_pattern,
                },
            ) => self.unify(*a_value, *b_value) && self.unify_pattern(*a_pattern, *b_pattern),
            (
                Expr::Match {
                    value: a_value,
                    cases: a_cases,
                },
                Expr::Match {
                    value: b_value,
                    cases: b_cases,
                },
            ) => self.unify(*a_value, *b_value) && self.unify_match_cases(a_cases, b_cases),
            (
                Expr::MethodCall {
                    method: a_method,
                    instance_type: a_instance_type,
                    receiver: a_receiver,
                    arguments: a_arguments,
                },
                Expr::MethodCall {
                    method: b_method,
                    instance_type: b_instance_type,
                    receiver: b_receiver,
                    arguments: b_arguments,
                },
            ) => {
                a_method == b_method
                    && self.unify_method_instance_type(a_instance_type, b_instance_type)
                    && self.unify(*a_receiver, *b_receiver)
                    && self.unify_all(a_arguments, b_arguments)
            }
            (
                Expr::NewInstance {
                    instance: a_instance,
                    arguments: a_arguments,
                },
                Expr::NewInstance {
                    instance: b_instance,
                    arguments: b_arguments,
                },
            ) => a_instance == b_instance && self.unify_all(a_arguments, b_arguments),
            (Expr::Not(a), Expr::Not(b)) => self.unify(*a, *b),
            (Expr::Raise { ex: a }, Expr::Raise { ex: b }) => self.unify(*a, *b),
            (
                Expr::RecordFieldLoad {
                    record_type: a_record_type,
                    field: a_field,
                    record_value: a_record_value,
                },
                Expr::RecordFieldLoad {
                    record_type: b_record_type,
                    field: b_field,
                    record_value: b_record_value,
                },
            ) => {
                a_field == b_field
                    && self.unify(*a_record_type, *b_record_type)
                    && self.unify(*a_record_value, *b_record_value)
            }
            (
                Expr::RecordFieldStore {
                    record_type: a_record_type,
                    field: a_field,
                    record_value: a_record_value,
                    new_value: a_new_value,
                },
                Expr::RecordFieldStore {
                    record_type: b_record_type,
                    field: b_field,
                    record_value: b_record_value,
                    new_value: b_new_value,
                },
            ) => {
                a_field == b_field
                    && self.unify(*a_record_type, *b_record_type)
                    && self.unify(*a_record_value, *b_record_value)
                    && self.unify(*a_new_value, *b_new_value)
            }
            (
                Expr::RecordLiteral {
                    record_type: a_record_type,
                    fields: a_fields,
                },
                Expr::RecordLiteral {
                    record_type: b_record_type,
                    fields: b_fields,
                },
            ) => {
                a_record_type.record == b_record_type.record
                    && self.unify_all(a_record_type.arguments, b_record_type.arguments)
                    && self.unify_record_fields(a_fields, b_fields)
            }
            (Expr::RecordType(a), Expr::RecordType(b)) => {
                &a.record == &b.record && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::Retry { label: a }, Expr::Retry { label: b }) => a == b,
            (Expr::Sequence(a), Expr::Sequence(b)) => {
                a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| self.unify(a, b))
            }
            (Expr::StringLiteral(a), Expr::StringLiteral(b)) => a == b,
            (Expr::TraitType(a), Expr::TraitType(b)) => {
                a.trait_ == b.trait_ && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::Tuple { items: a }, Expr::Tuple { items: b }) => self.unify_all(a, b),
            (Expr::TupleElement(a, a_index), Expr::TupleElement(b, b_index)) => {
                a_index == b_index && self.unify(*a, *b)
            }
            (Expr::Type(a), Expr::Type(b)) => self.unify(*a, *b),
            (Expr::BigType(a), Expr::BigType(b)) => a == b,
            (Expr::Variable(a), Expr::Variable(b)) => a == b,
            (
                Expr::VariableBinding(a_variable, a_value),
                Expr::VariableBinding(b_variable, b_value),
            ) => a_variable == b_variable && self.unify(*a_value, *b_value),
            (
                Expr::VariableStore(a_variable, a_value),
                Expr::VariableStore(b_variable, b_value),
            ) => a_variable == b_variable && self.unify(*a_value, *b_value),
            _ => false,
        }
    }

    fn unify_all(&mut self, a: Vec<Expr<Self::EC>>, b: Vec<Expr<Self::EC>>) -> bool {
        a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| self.unify(a, b))
    }

    fn unify_record_fields(
        &mut self,
        a: Vec<RecordFieldLiteral<Self::EC>>,
        b: Vec<RecordFieldLiteral<Self::EC>>,
    ) -> bool {
        a.len() == b.len()
            && a.into_iter()
                .zip(b)
                .all(|(a, b)| a.field == b.field && self.unify(a.value, b.value))
    }

    fn unify_match_cases(
        &mut self,
        a: Vec<MatchCase<Self::EC>>,
        b: Vec<MatchCase<Self::EC>>,
    ) -> bool {
        a.len() == b.len()
            && a.into_iter().zip(b).all(|(a, b)| {
                self.unify_pattern(a.pattern, b.pattern) && self.unify(a.body, b.body)
            })
    }

    fn unify_method_instance_type(
        &mut self,
        a: MethodInstanceType<Self::EC>,
        b: MethodInstanceType<Self::EC>,
    ) -> bool {
        match (a, b) {
            (MethodInstanceType::Trait(a), MethodInstanceType::Trait(b)) => {
                a.trait_ == b.trait_ && self.unify_all(a.arguments, b.arguments)
            }
        }
    }

    fn unify_pattern(&mut self, a: Pattern<Self::EC>, b: Pattern<Self::EC>) -> bool {
        match (a, b) {
            (Pattern::Error, _) | (_, Pattern::Error) => true,
            (Pattern::Discard { t: a }, Pattern::Discard { t: b }) => self.unify(*a, *b),
            (Pattern::Tuple(a), Pattern::Tuple(b)) => {
                a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| self.unify_pattern(a, b))
            }
            (Pattern::Binding(a_variable, a_pattern), Pattern::Binding(b_variable, b_pattern)) => {
                a_variable == b_variable && self.unify_pattern(*a_pattern, *b_pattern)
            }
            (
                Pattern::EnumVariant {
                    enum_type: a_enum_type,
                    variant: a_variant,
                    args: a_args,
                    fields: a_fields,
                },
                Pattern::EnumVariant {
                    enum_type: b_enum_type,
                    variant: b_variant,
                    args: b_args,
                    fields: b_fields,
                },
            ) => {
                a_enum_type.enum_ == b_enum_type.enum_
                    && a_variant == b_variant
                    && self.unify_all(a_enum_type.arguments, b_enum_type.arguments)
                    && self.unify_patterns(a_args, b_args)
                    && self.unify_record_field_patterns(a_fields, b_fields)
            }
            (Pattern::String(a), Pattern::String(b)) => a == b,
            (Pattern::Int(a), Pattern::Int(b)) => a == b,
            (Pattern::Bool(a), Pattern::Bool(b)) => a == b,
            _ => false,
        }
    }

    fn unify_patterns(&mut self, a: Vec<Pattern<Self::EC>>, b: Vec<Pattern<Self::EC>>) -> bool {
        a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| self.unify_pattern(a, b))
    }

    fn unify_record_field_patterns(
        &mut self,
        a: Vec<RecordFieldPattern<Self::EC>>,
        b: Vec<RecordFieldPattern<Self::EC>>,
    ) -> bool {
        a.len() == b.len()
            && a.into_iter()
                .zip(b)
                .all(|(a, b)| a.field == b.field && self.unify_pattern(a.pattern, b.pattern))
    }

    fn unify_builtin(&mut self, a: Builtin<Self::EC>, b: Builtin<Self::EC>) -> bool {
        match (a, b) {
            (Builtin::IntType, Builtin::IntType)
            | (Builtin::BoolType, Builtin::BoolType)
            | (Builtin::StringType, Builtin::StringType)
            | (Builtin::NeverType, Builtin::NeverType) => true,
            (
                Builtin::ArrayType {
                    element_type: a_element_type,
                },
                Builtin::ArrayType {
                    element_type: b_element_type,
                },
            ) => self.unify(*a_element_type, *b_element_type),
            (Builtin::IntNegate { value: a }, Builtin::IntNegate { value: b })
            | (Builtin::IntBitNot { value: a }, Builtin::IntBitNot { value: b }) => {
                self.unify(*a, *b)
            }
            (
                Builtin::IntAdd {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntAdd {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntSub {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntSub {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntMul {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntMul {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntBitAnd {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntBitAnd {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntBitOr {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntBitOr {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntBitXor {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntBitXor {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntBitShiftLeft {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntBitShiftLeft {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntBitShiftRight {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntBitShiftRight {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntEq {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntEq {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntLt {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntLt {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntLe {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntLe {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntGt {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntGt {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::IntGe {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntGe {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::StringConcat {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::StringConcat {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::StringEq {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::StringEq {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::BoolEq {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::BoolEq {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            ) => self.unify(*a_lhs, *b_lhs) && self.unify(*a_rhs, *b_rhs),
            (
                Builtin::ArrayCreateUnsafeUninitialized {
                    element_type: a_element_type,
                    length: a_length,
                },
                Builtin::ArrayCreateUnsafeUninitialized {
                    element_type: b_element_type,
                    length: b_length,
                },
            ) => self.unify(*a_element_type, *b_element_type) && self.unify(*a_length, *b_length),
            (
                Builtin::ArrayLength {
                    element_type: a_element_type,
                    array: a_array,
                },
                Builtin::ArrayLength {
                    element_type: b_element_type,
                    array: b_array,
                },
            ) => self.unify(*a_element_type, *b_element_type) && self.unify(*a_array, *b_array),
            (
                Builtin::ArrayGet {
                    element_type: a_element_type,
                    array: a_array,
                    index: a_index,
                },
                Builtin::ArrayGet {
                    element_type: b_element_type,
                    array: b_array,
                    index: b_index,
                },
            ) => {
                self.unify(*a_element_type, *b_element_type)
                    && self.unify(*a_array, *b_array)
                    && self.unify(*a_index, *b_index)
            }
            (
                Builtin::ArraySet {
                    element_type: a_element_type,
                    array: a_array,
                    index: a_index,
                    value: a_value,
                },
                Builtin::ArraySet {
                    element_type: b_element_type,
                    array: b_array,
                    index: b_index,
                    value: b_value,
                },
            ) => {
                self.unify(*a_element_type, *b_element_type)
                    && self.unify(*a_array, *b_array)
                    && self.unify(*a_index, *b_index)
                    && self.unify(*a_value, *b_value)
            }
            (
                Builtin::EqualToRefl {
                    r#type: a_type,
                    value: a_value,
                },
                Builtin::EqualToRefl {
                    r#type: b_type,
                    value: b_value,
                },
            ) => self.unify(*a_type, *b_type) && self.unify(*a_value, *b_value),
            (
                Builtin::UnsafeAssumeErased { r#type: a_type },
                Builtin::UnsafeAssumeErased { r#type: b_type },
            ) => self.unify(*a_type, *b_type),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Unify;
    use crate::{ClosureParameterVariable, ErasureMode, Expr, ExprContext, Normalizer, Variable};
    use alloc::{boxed::Box, vec::Vec};
    use argon_util::{Fuel, UniqueIdentifier};

    #[derive(Debug, Eq, Hash, PartialEq)]
    struct TestContext;

    impl ExprContext for TestContext {
        type Hole = ();
        type Function = ();
        type Record = ();
        type RecordField = ();
        type Enum = ();
        type Trait = ();
        type EnumVariant = ();
        type Method = ();
        type Instance = ();
    }

    struct NoopNormalizer;

    impl Normalizer for NoopNormalizer {
        type EC = TestContext;

        fn resolve_hole(&mut self, _hole: &()) -> Option<Expr<Self::EC>> {
            None
        }

        fn get_function_body(
            &mut self,
            _function: &(),
            _arguments: &mut Vec<Expr<Self::EC>>,
        ) -> Option<Expr<Self::EC>> {
            None
        }
    }

    #[derive(Clone)]
    struct TestModel;

    struct TestUnifier {
        model: TestModel,
    }

    impl Unify for TestUnifier {
        type EC = TestContext;
        type Model = TestModel;
        type Norm<'a> = NoopNormalizer;

        fn model(&self) -> &Self::Model {
            &self.model
        }

        fn model_mut(&mut self) -> &mut Self::Model {
            &mut self.model
        }

        fn normalize_fuel(&self) -> Fuel {
            Fuel::new(0)
        }

        fn normalizer<'a>(_model: &'a mut Self::Model) -> Self::Norm<'a>
        where
            Self: 'a,
        {
            NoopNormalizer
        }

        fn unify_hole(&mut self, _a: (), _b: Expr<Self::EC>) -> bool {
            false
        }
    }

    fn closure_parameter() -> Box<ClosureParameterVariable<TestContext>> {
        Box::new(ClosureParameterVariable {
            id: UniqueIdentifier::new(),
            var_type: Expr::Type(Box::new(Expr::BigType(0.into()))),
            name: None,
            is_mutable: false,
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
        })
    }

    #[test]
    fn function_type_unification_substitutes_result_parameter() {
        let a = closure_parameter();
        let b = closure_parameter();

        let left = Expr::FunctionType {
            a: a.clone(),
            r: Box::new(Expr::Variable(Variable::ClosureParameter(a))),
        };
        let right = Expr::FunctionType {
            a: b.clone(),
            r: Box::new(Expr::Variable(Variable::ClosureParameter(b))),
        };

        assert!(TestUnifier { model: TestModel }.unify(left, right));
    }
}
