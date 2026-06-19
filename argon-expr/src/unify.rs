use crate::{Builtin, Expr, ExprContext, Normalizer, NormalizerScanner, SubstScanner, Variable};
use alloc::{borrow::Cow, vec::Vec};
use argon_util::Fuel;

pub trait Unify {
    type EC: ExprContext;
    type Norm<'a>: Normalizer<EC = Self::EC>
    where
        Self: 'a;
    fn normalize_fuel(&self) -> Fuel;
    fn normalizer<'a>(&'a mut self) -> Self::Norm<'a>;

    fn unify_hole(&mut self, a: <Self::EC as ExprContext>::Hole, b: Expr<Self::EC>) -> bool;

    fn unify(&mut self, mut a: Expr<Self::EC>, mut b: Expr<Self::EC>) -> bool {
        {
            let mut norm = NormalizerScanner::new(self.normalize_fuel(), self.normalizer());
            norm.normalize(&mut a);
        }
        {
            let mut norm = NormalizerScanner::new(self.normalize_fuel(), self.normalizer());
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
            (Expr::Builtin(a), Expr::Builtin(b)) => self.unify_builtin(a, b),
            (Expr::BoxedType(a), Expr::BoxedType(b)) => self.unify(*a, *b),
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
            (Expr::IntLiteral(a), Expr::IntLiteral(b)) => a == b,
            (Expr::RecordType(a), Expr::RecordType(b)) => {
                &a.record == &b.record && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::StringLiteral(a), Expr::StringLiteral(b)) => a == b,
            (Expr::TraitType(a), Expr::TraitType(b)) => {
                a.trait_ == b.trait_ && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::Tuple { items: a }, Expr::Tuple { items: b }) => self.unify_all(a, b),
            (Expr::Type(a), Expr::Type(b)) => self.unify(*a, *b),
            (Expr::BigType(a), Expr::BigType(b)) => a == b,
            (Expr::Variable(a), Expr::Variable(b)) => a == b,
            _ => false,
        }
    }

    fn unify_all(&mut self, a: Vec<Expr<Self::EC>>, b: Vec<Expr<Self::EC>>) -> bool {
        a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| self.unify(a, b))
    }

    fn unify_builtin(&mut self, a: Builtin<Self::EC>, b: Builtin<Self::EC>) -> bool {
        macro_rules! unary {
            ($a:expr, $b:expr) => {
                self.unify(*$a, *$b)
            };
        }

        macro_rules! binary {
            ($a_lhs:expr, $a_rhs:expr, $b_lhs:expr, $b_rhs:expr) => {
                self.unify(*$a_lhs, *$b_lhs) && self.unify(*$a_rhs, *$b_rhs)
            };
        }

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
            ) => unary!(a_element_type, b_element_type),
            (Builtin::IntNegate { value: a }, Builtin::IntNegate { value: b })
            | (Builtin::IntBitNot { value: a }, Builtin::IntBitNot { value: b }) => unary!(a, b),
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
                Builtin::IntNe {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::IntNe {
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
                Builtin::StringNe {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::StringNe {
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
            )
            | (
                Builtin::BoolNe {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::BoolNe {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::ConjunctionType {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::ConjunctionType {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::DisjunctionType {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::DisjunctionType {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            )
            | (
                Builtin::EqualToType {
                    lhs: a_lhs,
                    rhs: a_rhs,
                },
                Builtin::EqualToType {
                    lhs: b_lhs,
                    rhs: b_rhs,
                },
            ) => binary!(a_lhs, a_rhs, b_lhs, b_rhs),
            (
                Builtin::ArrayCreateUnsafeUninitialized {
                    element_type: a_element_type,
                    length: a_length,
                },
                Builtin::ArrayCreateUnsafeUninitialized {
                    element_type: b_element_type,
                    length: b_length,
                },
            ) => binary!(a_element_type, a_length, b_element_type, b_length),
            (
                Builtin::ArrayLength {
                    element_type: a_element_type,
                    array: a_array,
                },
                Builtin::ArrayLength {
                    element_type: b_element_type,
                    array: b_array,
                },
            ) => binary!(a_element_type, a_array, b_element_type, b_array),
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

    struct TestUnifier;

    impl Unify for TestUnifier {
        type EC = TestContext;
        type Norm<'a> = NoopNormalizer;

        fn normalize_fuel(&self) -> Fuel {
            Fuel::new(0)
        }

        fn normalizer<'a>(&'a mut self) -> Self::Norm<'a> {
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

        assert!(TestUnifier.unify(left, right));
    }
}
