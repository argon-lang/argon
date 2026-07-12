use crate::{
    Builtin, Expr, ExprContext, ExprScannerMut, IntegerType, SubstScanner, Variable,
    default_scan_mut,
};
use alloc::borrow::Cow;
use alloc::vec::Vec;
use argon_util::Fuel;
use num_traits::ToPrimitive;
use std::mem;

pub struct NormalizerScanner<S> {
    fuel: Fuel,
    state: S,
}

impl<S> NormalizerScanner<S> {
    pub fn new(fuel: Fuel, state: S) -> Self {
        Self { fuel, state }
    }
}

pub struct FullNormalizer<S> {
    normalizer: NormalizerScanner<S>,
}

impl<S> FullNormalizer<S> {
    pub fn new(fuel: Fuel, state: S) -> Self {
        Self {
            normalizer: NormalizerScanner::new(fuel, state),
        }
    }
}

pub trait Normalizer {
    type EC: ExprContext;

    fn resolve_hole(&mut self, hole: &<Self::EC as ExprContext>::Hole) -> Option<Expr<Self::EC>>;

    fn get_function_body(
        &mut self,
        function: &<Self::EC as ExprContext>::Function,
        arguments: &mut Vec<Expr<Self::EC>>,
    ) -> Option<Expr<Self::EC>>;
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> NormalizerScanner<S> {
    pub fn normalize(&mut self, expr: &mut Expr<S::EC>) {
        let e2 = mem::replace(expr, Expr::Error);
        *expr = self.normalize_impl(e2);
    }

    pub fn normalize_impl(&mut self, expr: Expr<S::EC>) -> Expr<S::EC> {
        let mut expr = expr;

        'updated_no_growth: while !self.fuel.is_empty() {
            'updated: {
                match expr {
                    Expr::Hole(ref hole) => {
                        if let Some(resolved) = self.state.resolve_hole(hole) {
                            expr = resolved;
                            break 'updated;
                        }
                    }

                    Expr::FunctionCall {
                        ref function,
                        ref mut arguments,
                    } => {
                        if let Some(body) = self.state.get_function_body(function, arguments) {
                            expr = body;
                            break 'updated;
                        }
                    }

                    Expr::Condition { value, .. } => {
                        expr = *value;
                        continue 'updated_no_growth;
                    }

                    Expr::Not(value) => {
                        let value = self.normalize_impl(*value);
                        match value {
                            Expr::BoolLiteral(b) => {
                                expr = Expr::BoolLiteral(!b);
                                continue 'updated_no_growth;
                            }
                            Expr::Not(v) => {
                                expr = *v;
                                continue 'updated_no_growth;
                            }
                            Expr::And(mut a, mut b) => {
                                a = Box::new(Expr::Not(a));
                                b = Box::new(Expr::Not(b));
                                expr = Expr::Or(a, b);
                                continue 'updated_no_growth;
                            }
                            Expr::Or(mut a, mut b) => {
                                a = Box::new(Expr::Not(a));
                                b = Box::new(Expr::Not(b));
                                expr = Expr::And(a, b);
                                continue 'updated_no_growth;
                            }
                            _ => {
                                expr = Expr::Not(Box::new(value));
                            }
                        }
                    }

                    Expr::FunctionObjectCall { function, argument } => match *function {
                        Expr::Closure { v, mut body, .. } => {
                            let mut subst = SubstScanner::new();
                            subst.add_substitution(
                                Variable::ClosureParameter(v),
                                Cow::Owned(*argument),
                            );
                            subst.scan(&mut *body);
                            expr = *body;
                            continue 'updated_no_growth;
                        }

                        _ => {
                            expr = Expr::FunctionObjectCall { function, argument };
                        }
                    },

                    Expr::Builtin(builtin) => {
                        expr = normalize_builtin(builtin);
                        if !matches!(expr, Expr::Builtin(_)) {
                            continue 'updated_no_growth;
                        }
                    }

                    _ => {}
                }

                break 'updated_no_growth;
            }

            self.fuel.consume();
        }

        expr
    }
}

fn normalize_builtin<EC: ExprContext + ?Sized>(builtin: Builtin<EC>) -> Expr<EC> {
    match builtin {
        Builtin::IntNegate {
            integer_type,
            value,
        } => match (integer_type, *value) {
            (IntegerType::Int, Expr::IntLiteral(value)) => Expr::IntLiteral(-value),
            (IntegerType::I8, Expr::I8Literal(value)) => Expr::I8Literal(value.wrapping_neg()),
            (IntegerType::U8, Expr::U8Literal(value)) => Expr::U8Literal(value.wrapping_neg()),
            (IntegerType::I16, Expr::I16Literal(value)) => Expr::I16Literal(value.wrapping_neg()),
            (IntegerType::U16, Expr::U16Literal(value)) => Expr::U16Literal(value.wrapping_neg()),
            (integer_type, value) => Expr::Builtin(Builtin::IntNegate {
                integer_type,
                value: Box::new(value),
            }),
        },

        Builtin::IntBitNot {
            integer_type,
            value,
        } => match (integer_type, *value) {
            (IntegerType::Int, Expr::IntLiteral(value)) => Expr::IntLiteral(!value),
            (IntegerType::I8, Expr::I8Literal(value)) => Expr::I8Literal(!value),
            (IntegerType::U8, Expr::U8Literal(value)) => Expr::U8Literal(!value),
            (IntegerType::I16, Expr::I16Literal(value)) => Expr::I16Literal(!value),
            (IntegerType::U16, Expr::U16Literal(value)) => Expr::U16Literal(!value),
            (integer_type, value) => Expr::Builtin(Builtin::IntBitNot {
                integer_type,
                value: Box::new(value),
            }),
        },

        Builtin::IntConvert {
            source_type,
            dest_type,
            value,
        } => match (source_type, dest_type, *value) {
            (IntegerType::Int, IntegerType::Int, value) => value,
            (IntegerType::I8, IntegerType::I8, value) => value,
            (IntegerType::U8, IntegerType::U8, value) => value,
            (IntegerType::I16, IntegerType::I16, value) => value,
            (IntegerType::U16, IntegerType::U16, value) => value,
            (IntegerType::Int, IntegerType::I8, Expr::IntLiteral(value)) => {
                Expr::I8Literal(value.to_i8().unwrap_or_else(|| {
                    let bytes = value.to_signed_bytes_le();
                    i8::from_le_bytes([bytes.first().copied().unwrap_or(0)])
                }))
            }
            (IntegerType::Int, IntegerType::U8, Expr::IntLiteral(value)) => {
                Expr::U8Literal(value.to_u8().unwrap_or_else(|| {
                    let bytes = value.to_signed_bytes_le();
                    bytes.first().copied().unwrap_or(0)
                }))
            }
            (IntegerType::Int, IntegerType::I16, Expr::IntLiteral(value)) => {
                Expr::I16Literal(value.to_i16().unwrap_or_else(|| {
                    let bytes = value.to_signed_bytes_le();
                    i16::from_le_bytes([
                        bytes.first().copied().unwrap_or(0),
                        bytes.get(1).copied().unwrap_or(0),
                    ])
                }))
            }
            (IntegerType::Int, IntegerType::U16, Expr::IntLiteral(value)) => {
                Expr::U16Literal(value.to_u16().unwrap_or_else(|| {
                    let bytes = value.to_signed_bytes_le();
                    u16::from_le_bytes([
                        bytes.first().copied().unwrap_or(0),
                        bytes.get(1).copied().unwrap_or(0),
                    ])
                }))
            }
            (IntegerType::I8, IntegerType::Int, Expr::I8Literal(value)) => {
                Expr::IntLiteral(value.into())
            }
            (IntegerType::I8, IntegerType::U8, Expr::I8Literal(value)) => {
                Expr::U8Literal(value.to_le_bytes()[0])
            }
            (IntegerType::I8, IntegerType::I16, Expr::I8Literal(value)) => {
                Expr::I16Literal(value.into())
            }
            (IntegerType::I8, IntegerType::U16, Expr::I8Literal(value)) => {
                Expr::U16Literal((value as i16) as u16)
            }
            (IntegerType::U8, IntegerType::I8, Expr::U8Literal(value)) => {
                Expr::I8Literal(i8::from_le_bytes([value]))
            }
            (IntegerType::U8, IntegerType::Int, Expr::U8Literal(value)) => {
                Expr::IntLiteral(value.into())
            }
            (IntegerType::U8, IntegerType::I16, Expr::U8Literal(value)) => {
                Expr::I16Literal(value.into())
            }
            (IntegerType::U8, IntegerType::U16, Expr::U8Literal(value)) => {
                Expr::U16Literal(value.into())
            }
            (IntegerType::I16, IntegerType::Int, Expr::I16Literal(value)) => {
                Expr::IntLiteral(value.into())
            }
            (IntegerType::I16, IntegerType::I8, Expr::I16Literal(value)) => {
                Expr::I8Literal(value as i8)
            }
            (IntegerType::I16, IntegerType::U8, Expr::I16Literal(value)) => {
                Expr::U8Literal(value as u8)
            }
            (IntegerType::I16, IntegerType::U16, Expr::I16Literal(value)) => {
                Expr::U16Literal(value as u16)
            }
            (IntegerType::U16, IntegerType::Int, Expr::U16Literal(value)) => {
                Expr::IntLiteral(value.into())
            }
            (IntegerType::U16, IntegerType::I8, Expr::U16Literal(value)) => {
                Expr::I8Literal(value as i8)
            }
            (IntegerType::U16, IntegerType::U8, Expr::U16Literal(value)) => {
                Expr::U8Literal(value as u8)
            }
            (IntegerType::U16, IntegerType::I16, Expr::U16Literal(value)) => {
                Expr::I16Literal(value as i16)
            }
            (source_type, dest_type, value) => Expr::Builtin(Builtin::IntConvert {
                source_type,
                dest_type,
                value: Box::new(value),
            }),
        },

        Builtin::IntAdd {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::IntLiteral(lhs + rhs),
            |lhs, rhs| Expr::I8Literal(lhs.wrapping_add(rhs)),
            |lhs, rhs| Expr::U8Literal(lhs.wrapping_add(rhs)),
            |lhs, rhs| Expr::I16Literal(lhs.wrapping_add(rhs)),
            |lhs, rhs| Expr::U16Literal(lhs.wrapping_add(rhs)),
            |integer_type, lhs, rhs| Builtin::IntAdd {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntSub {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::IntLiteral(lhs - rhs),
            |lhs, rhs| Expr::I8Literal(lhs.wrapping_sub(rhs)),
            |lhs, rhs| Expr::U8Literal(lhs.wrapping_sub(rhs)),
            |lhs, rhs| Expr::I16Literal(lhs.wrapping_sub(rhs)),
            |lhs, rhs| Expr::U16Literal(lhs.wrapping_sub(rhs)),
            |integer_type, lhs, rhs| Builtin::IntSub {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntMul {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::IntLiteral(lhs * rhs),
            |lhs, rhs| Expr::I8Literal(lhs.wrapping_mul(rhs)),
            |lhs, rhs| Expr::U8Literal(lhs.wrapping_mul(rhs)),
            |lhs, rhs| Expr::I16Literal(lhs.wrapping_mul(rhs)),
            |lhs, rhs| Expr::U16Literal(lhs.wrapping_mul(rhs)),
            |integer_type, lhs, rhs| Builtin::IntMul {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntBitAnd {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::IntLiteral(lhs & rhs),
            |lhs, rhs| Expr::I8Literal(lhs & rhs),
            |lhs, rhs| Expr::U8Literal(lhs & rhs),
            |lhs, rhs| Expr::I16Literal(lhs & rhs),
            |lhs, rhs| Expr::U16Literal(lhs & rhs),
            |integer_type, lhs, rhs| Builtin::IntBitAnd {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntBitOr {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::IntLiteral(lhs | rhs),
            |lhs, rhs| Expr::I8Literal(lhs | rhs),
            |lhs, rhs| Expr::U8Literal(lhs | rhs),
            |lhs, rhs| Expr::I16Literal(lhs | rhs),
            |lhs, rhs| Expr::U16Literal(lhs | rhs),
            |integer_type, lhs, rhs| Builtin::IntBitOr {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntBitXor {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::IntLiteral(lhs ^ rhs),
            |lhs, rhs| Expr::I8Literal(lhs ^ rhs),
            |lhs, rhs| Expr::U8Literal(lhs ^ rhs),
            |lhs, rhs| Expr::I16Literal(lhs ^ rhs),
            |lhs, rhs| Expr::U16Literal(lhs ^ rhs),
            |integer_type, lhs, rhs| Builtin::IntBitXor {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntBitShiftLeft {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| match rhs.to_usize() {
                Some(rhs) => Expr::IntLiteral(lhs << rhs),
                None => Expr::Builtin(Builtin::IntBitShiftLeft {
                    integer_type: IntegerType::Int,
                    lhs: Box::new(Expr::IntLiteral(lhs)),
                    rhs: Box::new(Expr::IntLiteral(rhs)),
                }),
            },
            |lhs, rhs| {
                Expr::I8Literal(if !(0..8).contains(&rhs) {
                    0
                } else {
                    lhs.wrapping_shl(rhs as u32)
                })
            },
            |lhs, rhs| {
                Expr::U8Literal(if rhs >= 8 {
                    0
                } else {
                    lhs.wrapping_shl(rhs.into())
                })
            },
            |lhs, rhs| {
                Expr::I16Literal(if !(0..16).contains(&rhs) {
                    0
                } else {
                    lhs.wrapping_shl(rhs as u32)
                })
            },
            |lhs, rhs| {
                Expr::U16Literal(if rhs >= 16 {
                    0
                } else {
                    lhs.wrapping_shl(rhs.into())
                })
            },
            |integer_type, lhs, rhs| Builtin::IntBitShiftLeft {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntBitShiftRight {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| match rhs.to_usize() {
                Some(rhs) => Expr::IntLiteral(lhs >> rhs),
                None => Expr::Builtin(Builtin::IntBitShiftRight {
                    integer_type: IntegerType::Int,
                    lhs: Box::new(Expr::IntLiteral(lhs)),
                    rhs: Box::new(Expr::IntLiteral(rhs)),
                }),
            },
            |lhs, rhs| {
                Expr::I8Literal(if !(0..8).contains(&rhs) {
                    if lhs < 0 { -1 } else { 0 }
                } else {
                    lhs.wrapping_shr(rhs as u32)
                })
            },
            |lhs, rhs| {
                Expr::U8Literal(if rhs >= 8 {
                    0
                } else {
                    lhs.wrapping_shr(rhs.into())
                })
            },
            |lhs, rhs| {
                Expr::I16Literal(if !(0..16).contains(&rhs) {
                    if lhs < 0 { -1 } else { 0 }
                } else {
                    lhs.wrapping_shr(rhs as u32)
                })
            },
            |lhs, rhs| {
                Expr::U16Literal(if rhs >= 16 {
                    0
                } else {
                    lhs.wrapping_shr(rhs.into())
                })
            },
            |integer_type, lhs, rhs| Builtin::IntBitShiftRight {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntEq {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::BoolLiteral(lhs == rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs == rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs == rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs == rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs == rhs),
            |integer_type, lhs, rhs| Builtin::IntEq {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntLt {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::BoolLiteral(lhs < rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs < rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs < rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs < rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs < rhs),
            |integer_type, lhs, rhs| Builtin::IntLt {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntLe {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::BoolLiteral(lhs <= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs <= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs <= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs <= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs <= rhs),
            |integer_type, lhs, rhs| Builtin::IntLe {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntGt {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::BoolLiteral(lhs > rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs > rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs > rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs > rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs > rhs),
            |integer_type, lhs, rhs| Builtin::IntGt {
                integer_type,
                lhs,
                rhs,
            },
        ),

        Builtin::IntGe {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Expr::BoolLiteral(lhs >= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs >= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs >= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs >= rhs),
            |lhs, rhs| Expr::BoolLiteral(lhs >= rhs),
            |integer_type, lhs, rhs| Builtin::IntGe {
                integer_type,
                lhs,
                rhs,
            },
        ),

        builtin => Expr::Builtin(builtin),
    }
}

fn normalize_integer_binary_op<EC: ExprContext + ?Sized>(
    integer_type: IntegerType,
    lhs: Expr<EC>,
    rhs: Expr<EC>,
    int_op: impl FnOnce(num_bigint::BigInt, num_bigint::BigInt) -> Expr<EC>,
    i8_op: impl FnOnce(i8, i8) -> Expr<EC>,
    u8_op: impl FnOnce(u8, u8) -> Expr<EC>,
    i16_op: impl FnOnce(i16, i16) -> Expr<EC>,
    u16_op: impl FnOnce(u16, u16) -> Expr<EC>,
    rebuild: impl FnOnce(IntegerType, Box<Expr<EC>>, Box<Expr<EC>>) -> Builtin<EC>,
) -> Expr<EC> {
    match (integer_type, lhs, rhs) {
        (IntegerType::Int, Expr::IntLiteral(lhs), Expr::IntLiteral(rhs)) => int_op(lhs, rhs),
        (IntegerType::I8, Expr::I8Literal(lhs), Expr::I8Literal(rhs)) => i8_op(lhs, rhs),
        (IntegerType::U8, Expr::U8Literal(lhs), Expr::U8Literal(rhs)) => u8_op(lhs, rhs),
        (IntegerType::I16, Expr::I16Literal(lhs), Expr::I16Literal(rhs)) => i16_op(lhs, rhs),
        (IntegerType::U16, Expr::U16Literal(lhs), Expr::U16Literal(rhs)) => u16_op(lhs, rhs),
        (integer_type, lhs, rhs) => {
            Expr::Builtin(rebuild(integer_type, Box::new(lhs), Box::new(rhs)))
        }
    }
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> FullNormalizer<S> {
    pub fn normalize(&mut self, expr: &mut Expr<S::EC>) {
        self.scan(expr);
    }
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> ExprScannerMut for FullNormalizer<S> {
    type EC = EC;

    fn scan(&mut self, expr: &mut Expr<Self::EC>) -> bool {
        if !default_scan_mut(self, expr) {
            return false;
        }

        let fuel = self.normalizer.fuel.clone();
        self.normalizer.normalize(expr);
        self.normalizer.fuel = fuel;
        true
    }
}

#[cfg(test)]
mod tests {
    use super::{FullNormalizer, Normalizer};
    use crate::{Expr, ExprContext, ExprScannerMut};
    use alloc::vec;
    use alloc::vec::Vec;
    use argon_util::Fuel;

    #[derive(Debug, Eq, Hash, PartialEq)]
    struct TestContext;

    impl ExprContext for TestContext {
        type Hole = u8;
        type Function = u8;
        type Record = ();
        type RecordField = ();
        type Enum = ();
        type Trait = ();
        type EnumVariant = ();
        type Method = ();
        type Instance = ();
    }

    struct TestNormalizer;

    impl Normalizer for TestNormalizer {
        type EC = TestContext;

        fn resolve_hole(&mut self, hole: &u8) -> Option<Expr<Self::EC>> {
            (*hole == 1).then_some(Expr::BoolLiteral(false))
        }

        fn get_function_body(
            &mut self,
            function: &u8,
            arguments: &mut Vec<Expr<Self::EC>>,
        ) -> Option<Expr<Self::EC>> {
            if *function != 1 {
                return None;
            }

            match arguments.as_slice() {
                [Expr::BoolLiteral(false)] => Some(Expr::BoolLiteral(true)),
                [_] => Some(Expr::Error),
                [] => None,
                _ => None,
            }
        }
    }

    #[test]
    fn full_normalizer_normalizes_nested_expressions() {
        let mut expr = Expr::Tuple {
            items: vec![Expr::Hole(1)],
        };
        let mut normalizer = FullNormalizer::new(Fuel::new(10), TestNormalizer);

        normalizer.normalize(&mut expr);

        assert_eq!(
            expr,
            Expr::Tuple {
                items: vec![Expr::BoolLiteral(false)]
            }
        );
    }

    #[test]
    fn full_normalizer_updates_children_before_current_expression() {
        let mut expr = Expr::FunctionCall {
            function: 1,
            arguments: vec![Expr::Hole(1)],
        };
        let mut normalizer = FullNormalizer::new(Fuel::new(10), TestNormalizer);

        assert!(normalizer.scan(&mut expr));

        assert_eq!(expr, Expr::BoolLiteral(true));
    }
}
