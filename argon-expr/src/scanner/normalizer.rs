use crate::{
    Builtin, Expr, ExprContext, ExprLocationExt, ExprScannerMut, IntegerType, LocatedExpr,
    SubstScanner, Variable, default_scan_mut, ownership::is_shared_type,
};
use alloc::borrow::Cow;
use alloc::vec::Vec;
use argon_util::Fuel;
use core::mem;
use num_bigint::ToBigInt;
use num_traits::ToPrimitive;

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

    fn resolve_hole(
        &mut self,
        hole: &<Self::EC as ExprContext>::Hole,
    ) -> Option<LocatedExpr<Self::EC>>;

    fn get_function_body(
        &mut self,
        function: &<Self::EC as ExprContext>::Function,
        arguments: &mut Vec<LocatedExpr<Self::EC>>,
    ) -> Option<LocatedExpr<Self::EC>>;
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> NormalizerScanner<S> {
    pub fn normalize(&mut self, expr: &mut LocatedExpr<S::EC>) {
        let e2 = mem::replace(
            expr,
            LocatedExpr {
                value: Expr::Error,
                location: expr.location.clone(),
            },
        );
        *expr = self.normalize_impl(e2);
    }

    pub fn normalize_impl(&mut self, expr: LocatedExpr<S::EC>) -> LocatedExpr<S::EC> {
        let mut expr = expr;

        'updated_no_growth: while !self.fuel.is_empty() {
            'updated: {
                match expr.value {
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
                        let mut value_box = value;
                        let mut value = self.normalize_impl(*value_box);
                        match value.value {
                            Expr::BoolLiteral(b) => {
                                value.value = Expr::BoolLiteral(!b);
                                expr = value;
                                continue 'updated_no_growth;
                            }
                            Expr::Not(v) => {
                                expr = *v;
                                continue 'updated_no_growth;
                            }
                            Expr::And(mut a, mut b) => {
                                let a_location = a.location.clone();
                                let b_location = b.location.clone();
                                a = Box::new(Expr::Not(a).with_location(a_location));
                                b = Box::new(Expr::Not(b).with_location(b_location));
                                value.value = Expr::Or(a, b);
                                expr = value;
                                continue 'updated_no_growth;
                            }
                            Expr::Or(mut a, mut b) => {
                                let a_location = a.location.clone();
                                let b_location = b.location.clone();
                                a = Box::new(Expr::Not(a).with_location(a_location));
                                b = Box::new(Expr::Not(b).with_location(b_location));
                                value.value = Expr::And(a, b);
                                expr = value;
                                continue 'updated_no_growth;
                            }
                            _ => {
                                *value_box = value;
                                expr.value = Expr::Not(value_box);
                            }
                        }
                    }

                    Expr::FunctionObjectCall { function, argument } => {
                        let function = *function;
                        match function.value {
                            Expr::Closure { v, mut body, .. } => {
                                let mut subst = SubstScanner::new();
                                subst.add_substitution(
                                    Variable::ClosureParameter(v),
                                    Cow::Owned(*argument),
                                );
                                subst.scan(&mut body);
                                expr = *body;
                                continue 'updated_no_growth;
                            }

                            _ => {
                                expr.value = Expr::FunctionObjectCall {
                                    function: Box::new(function),
                                    argument,
                                };
                            }
                        }
                    }

                    Expr::Builtin(builtin) => {
                        expr.value = normalize_builtin(builtin);
                        if !matches!(expr.value, Expr::Builtin(_)) {
                            continue 'updated_no_growth;
                        }
                    }

                    Expr::Use {
                        is_mutable,
                        inner: value,
                    } => {
                        match value.value {
                            Expr::Use {
                                inner: value,
                                is_mutable: is_mutable2,
                            } => {
                                expr.value = Expr::Use {
                                    is_mutable: is_mutable && is_mutable2,
                                    inner: value,
                                };
                                continue 'updated_no_growth;
                            }

                            _ if !is_mutable && is_shared_type(&value) => {
                                expr = *value;
                                continue 'updated_no_growth;
                            }

                            _ => {}
                        }

                        expr.value = Expr::Use {
                            is_mutable,
                            inner: value,
                        };
                    }

                    Expr::Shared { inner: value } => {
                        match value.value {
                            Expr::Shared { .. } => {
                                expr = *value;
                                continue 'updated_no_growth;
                            }

                            _ if is_shared_type(&value) => {
                                expr = *value;
                                continue 'updated_no_growth;
                            }

                            _ => {}
                        }

                        expr.value = Expr::Shared { inner: value };
                    }

                    Expr::Share { value } => {
                        match value.value {
                            Expr::Share { .. } => {
                                expr = *value;
                                continue 'updated_no_growth;
                            }

                            _ => {}
                        }

                        expr.value = Expr::Share { value };
                    }

                    Expr::Borrow { value } => {
                        match value.value {
                            Expr::Borrow { .. } => {
                                expr = *value;
                                continue 'updated_no_growth;
                            }

                            _ => {}
                        }

                        expr.value = Expr::Borrow { value };
                    }

                    Expr::BorrowMut { value } => {
                        match value.value {
                            Expr::BorrowMut { .. } => {
                                expr = *value;
                                continue 'updated_no_growth;
                            }

                            _ => {}
                        }

                        expr.value = Expr::BorrowMut { value };
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
        } => normalize_integer_unary_op(
            integer_type,
            *value,
            |value| Expr::IntLiteral(-value),
            |value| Expr::I8Literal(value.wrapping_neg()),
            |value| Expr::U8Literal(value.wrapping_neg()),
            |value| Expr::I16Literal(value.wrapping_neg()),
            |value| Expr::U16Literal(value.wrapping_neg()),
            |value| Expr::I32Literal(value.wrapping_neg()),
            |value| Expr::U32Literal(value.wrapping_neg()),
            |value| Expr::I64Literal(value.wrapping_neg()),
            |value| Expr::U64Literal(value.wrapping_neg()),
            |integer_type, value| Builtin::IntNegate {
                integer_type,
                value,
            },
        ),

        Builtin::IntBitNot {
            integer_type,
            value,
        } => normalize_integer_unary_op(
            integer_type,
            *value,
            |value| Expr::IntLiteral(!value),
            |value| Expr::I8Literal(!value),
            |value| Expr::U8Literal(!value),
            |value| Expr::I16Literal(!value),
            |value| Expr::U16Literal(!value),
            |value| Expr::I32Literal(!value),
            |value| Expr::U32Literal(!value),
            |value| Expr::I64Literal(!value),
            |value| Expr::U64Literal(!value),
            |integer_type, value| Builtin::IntBitNot {
                integer_type,
                value,
            },
        ),

        Builtin::IntConvert {
            source_type,
            dest_type,
            value,
        } => normalize_integer_conversion(source_type, dest_type, *value),

        Builtin::IntAdd {
            integer_type,
            lhs,
            rhs,
        } => normalize_integer_binary_op(
            integer_type,
            *lhs,
            *rhs,
            |lhs, rhs| Some(Expr::IntLiteral(lhs + rhs)),
            |lhs, rhs| Some(Expr::I8Literal(lhs.wrapping_add(rhs))),
            |lhs, rhs| Some(Expr::U8Literal(lhs.wrapping_add(rhs))),
            |lhs, rhs| Some(Expr::I16Literal(lhs.wrapping_add(rhs))),
            |lhs, rhs| Some(Expr::U16Literal(lhs.wrapping_add(rhs))),
            |lhs, rhs| Some(Expr::I32Literal(lhs.wrapping_add(rhs))),
            |lhs, rhs| Some(Expr::U32Literal(lhs.wrapping_add(rhs))),
            |lhs, rhs| Some(Expr::I64Literal(lhs.wrapping_add(rhs))),
            |lhs, rhs| Some(Expr::U64Literal(lhs.wrapping_add(rhs))),
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
            |lhs, rhs| Some(Expr::IntLiteral(lhs - rhs)),
            |lhs, rhs| Some(Expr::I8Literal(lhs.wrapping_sub(rhs))),
            |lhs, rhs| Some(Expr::U8Literal(lhs.wrapping_sub(rhs))),
            |lhs, rhs| Some(Expr::I16Literal(lhs.wrapping_sub(rhs))),
            |lhs, rhs| Some(Expr::U16Literal(lhs.wrapping_sub(rhs))),
            |lhs, rhs| Some(Expr::I32Literal(lhs.wrapping_sub(rhs))),
            |lhs, rhs| Some(Expr::U32Literal(lhs.wrapping_sub(rhs))),
            |lhs, rhs| Some(Expr::I64Literal(lhs.wrapping_sub(rhs))),
            |lhs, rhs| Some(Expr::U64Literal(lhs.wrapping_sub(rhs))),
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
            |lhs, rhs| Some(Expr::IntLiteral(lhs * rhs)),
            |lhs, rhs| Some(Expr::I8Literal(lhs.wrapping_mul(rhs))),
            |lhs, rhs| Some(Expr::U8Literal(lhs.wrapping_mul(rhs))),
            |lhs, rhs| Some(Expr::I16Literal(lhs.wrapping_mul(rhs))),
            |lhs, rhs| Some(Expr::U16Literal(lhs.wrapping_mul(rhs))),
            |lhs, rhs| Some(Expr::I32Literal(lhs.wrapping_mul(rhs))),
            |lhs, rhs| Some(Expr::U32Literal(lhs.wrapping_mul(rhs))),
            |lhs, rhs| Some(Expr::I64Literal(lhs.wrapping_mul(rhs))),
            |lhs, rhs| Some(Expr::U64Literal(lhs.wrapping_mul(rhs))),
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
            |lhs, rhs| Some(Expr::IntLiteral(lhs & rhs)),
            |lhs, rhs| Some(Expr::I8Literal(lhs & rhs)),
            |lhs, rhs| Some(Expr::U8Literal(lhs & rhs)),
            |lhs, rhs| Some(Expr::I16Literal(lhs & rhs)),
            |lhs, rhs| Some(Expr::U16Literal(lhs & rhs)),
            |lhs, rhs| Some(Expr::I32Literal(lhs & rhs)),
            |lhs, rhs| Some(Expr::U32Literal(lhs & rhs)),
            |lhs, rhs| Some(Expr::I64Literal(lhs & rhs)),
            |lhs, rhs| Some(Expr::U64Literal(lhs & rhs)),
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
            |lhs, rhs| Some(Expr::IntLiteral(lhs | rhs)),
            |lhs, rhs| Some(Expr::I8Literal(lhs | rhs)),
            |lhs, rhs| Some(Expr::U8Literal(lhs | rhs)),
            |lhs, rhs| Some(Expr::I16Literal(lhs | rhs)),
            |lhs, rhs| Some(Expr::U16Literal(lhs | rhs)),
            |lhs, rhs| Some(Expr::I32Literal(lhs | rhs)),
            |lhs, rhs| Some(Expr::U32Literal(lhs | rhs)),
            |lhs, rhs| Some(Expr::I64Literal(lhs | rhs)),
            |lhs, rhs| Some(Expr::U64Literal(lhs | rhs)),
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
            |lhs, rhs| Some(Expr::IntLiteral(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::I8Literal(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::U8Literal(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::I16Literal(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::U16Literal(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::I32Literal(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::U32Literal(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::I64Literal(lhs ^ rhs)),
            |lhs, rhs| Some(Expr::U64Literal(lhs ^ rhs)),
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
            |lhs, rhs| rhs.to_usize().map(|rhs| Expr::IntLiteral(lhs << rhs)),
            |lhs, rhs| {
                Some(Expr::I8Literal(if !(0..8).contains(&rhs) {
                    0
                } else {
                    lhs.wrapping_shl(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U8Literal(if rhs >= 8 {
                    0
                } else {
                    lhs.wrapping_shl(rhs.into())
                }))
            },
            |lhs, rhs| {
                Some(Expr::I16Literal(if !(0..16).contains(&rhs) {
                    0
                } else {
                    lhs.wrapping_shl(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U16Literal(if rhs >= 16 {
                    0
                } else {
                    lhs.wrapping_shl(rhs.into())
                }))
            },
            |lhs, rhs| {
                Some(Expr::I32Literal(if !(0..32).contains(&rhs) {
                    0
                } else {
                    lhs.wrapping_shl(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U32Literal(if rhs >= 32 {
                    0
                } else {
                    lhs.wrapping_shl(rhs)
                }))
            },
            |lhs, rhs| {
                Some(Expr::I64Literal(if !(0..64).contains(&rhs) {
                    0
                } else {
                    lhs.wrapping_shl(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U64Literal(if rhs >= 64 {
                    0
                } else {
                    lhs.wrapping_shl(rhs as u32)
                }))
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
            |lhs, rhs| rhs.to_usize().map(|rhs| Expr::IntLiteral(lhs >> rhs)),
            |lhs, rhs| {
                Some(Expr::I8Literal(if !(0..8).contains(&rhs) {
                    if lhs < 0 { -1 } else { 0 }
                } else {
                    lhs.wrapping_shr(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U8Literal(if rhs >= 8 {
                    0
                } else {
                    lhs.wrapping_shr(rhs.into())
                }))
            },
            |lhs, rhs| {
                Some(Expr::I16Literal(if !(0..16).contains(&rhs) {
                    if lhs < 0 { -1 } else { 0 }
                } else {
                    lhs.wrapping_shr(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U16Literal(if rhs >= 16 {
                    0
                } else {
                    lhs.wrapping_shr(rhs.into())
                }))
            },
            |lhs, rhs| {
                Some(Expr::I32Literal(if !(0..32).contains(&rhs) {
                    if lhs < 0 { -1 } else { 0 }
                } else {
                    lhs.wrapping_shr(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U32Literal(if rhs >= 32 {
                    0
                } else {
                    lhs.wrapping_shr(rhs)
                }))
            },
            |lhs, rhs| {
                Some(Expr::I64Literal(if !(0..64).contains(&rhs) {
                    if lhs < 0 { -1 } else { 0 }
                } else {
                    lhs.wrapping_shr(rhs as u32)
                }))
            },
            |lhs, rhs| {
                Some(Expr::U64Literal(if rhs >= 64 {
                    0
                } else {
                    lhs.wrapping_shr(rhs as u32)
                }))
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
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs == rhs)),
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
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs < rhs)),
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
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs <= rhs)),
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
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs > rhs)),
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
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |lhs, rhs| Some(Expr::BoolLiteral(lhs >= rhs)),
            |integer_type, lhs, rhs| Builtin::IntGe {
                integer_type,
                lhs,
                rhs,
            },
        ),

        builtin => Expr::Builtin(builtin),
    }
}

fn normalize_integer_unary_op<EC: ExprContext + ?Sized>(
    integer_type: IntegerType,
    value: LocatedExpr<EC>,
    int_op: impl FnOnce(num_bigint::BigInt) -> Expr<EC>,
    i8_op: impl FnOnce(i8) -> Expr<EC>,
    u8_op: impl FnOnce(u8) -> Expr<EC>,
    i16_op: impl FnOnce(i16) -> Expr<EC>,
    u16_op: impl FnOnce(u16) -> Expr<EC>,
    i32_op: impl FnOnce(i32) -> Expr<EC>,
    u32_op: impl FnOnce(u32) -> Expr<EC>,
    i64_op: impl FnOnce(i64) -> Expr<EC>,
    u64_op: impl FnOnce(u64) -> Expr<EC>,
    rebuild: impl FnOnce(IntegerType, Box<LocatedExpr<EC>>) -> Builtin<EC>,
) -> Expr<EC> {
    match (integer_type, value.value) {
        (IntegerType::Int, Expr::IntLiteral(value)) => int_op(value),
        (IntegerType::I8, Expr::I8Literal(value)) => i8_op(value),
        (IntegerType::U8, Expr::U8Literal(value)) => u8_op(value),
        (IntegerType::I16, Expr::I16Literal(value)) => i16_op(value),
        (IntegerType::U16, Expr::U16Literal(value)) => u16_op(value),
        (IntegerType::I32, Expr::I32Literal(value)) => i32_op(value),
        (IntegerType::U32, Expr::U32Literal(value)) => u32_op(value),
        (IntegerType::I64, Expr::I64Literal(value)) => i64_op(value),
        (IntegerType::U64, Expr::U64Literal(value)) => u64_op(value),
        (integer_type, value_expr) => Expr::Builtin(rebuild(
            integer_type,
            Box::new(value_expr.with_location(value.location)),
        )),
    }
}

fn normalize_integer_conversion<EC: ExprContext + ?Sized>(
    source_type: IntegerType,
    dest_type: IntegerType,
    value: LocatedExpr<EC>,
) -> Expr<EC> {
    match (source_type, dest_type, value.value) {
        (IntegerType::Int, IntegerType::Int, value) => value,
        (IntegerType::I8, IntegerType::I8, value) => value,
        (IntegerType::U8, IntegerType::U8, value) => value,
        (IntegerType::I16, IntegerType::I16, value) => value,
        (IntegerType::U16, IntegerType::U16, value) => value,
        (IntegerType::I32, IntegerType::I32, value) => value,
        (IntegerType::U32, IntegerType::U32, value) => value,
        (IntegerType::I64, IntegerType::I64, value) => value,
        (IntegerType::U64, IntegerType::U64, value) => value,
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
        (IntegerType::Int, IntegerType::I32, Expr::IntLiteral(value)) => {
            Expr::I32Literal(value.to_i32().unwrap_or_else(|| {
                let bytes = value.to_signed_bytes_le();
                i32::from_le_bytes([
                    bytes.first().copied().unwrap_or(0),
                    bytes.get(1).copied().unwrap_or(0),
                    bytes.get(2).copied().unwrap_or(0),
                    bytes.get(3).copied().unwrap_or(0),
                ])
            }))
        }
        (IntegerType::Int, IntegerType::U32, Expr::IntLiteral(value)) => {
            Expr::U32Literal(value.to_u32().unwrap_or_else(|| {
                let bytes = value.to_signed_bytes_le();
                u32::from_le_bytes([
                    bytes.first().copied().unwrap_or(0),
                    bytes.get(1).copied().unwrap_or(0),
                    bytes.get(2).copied().unwrap_or(0),
                    bytes.get(3).copied().unwrap_or(0),
                ])
            }))
        }
        (IntegerType::Int, IntegerType::I64, Expr::IntLiteral(value)) => {
            Expr::I64Literal(value.to_i64().unwrap_or_else(|| {
                let bytes = value.to_signed_bytes_le();
                i64::from_le_bytes([
                    bytes.first().copied().unwrap_or(0),
                    bytes.get(1).copied().unwrap_or(0),
                    bytes.get(2).copied().unwrap_or(0),
                    bytes.get(3).copied().unwrap_or(0),
                    bytes.get(4).copied().unwrap_or(0),
                    bytes.get(5).copied().unwrap_or(0),
                    bytes.get(6).copied().unwrap_or(0),
                    bytes.get(7).copied().unwrap_or(0),
                ])
            }))
        }
        (IntegerType::Int, IntegerType::U64, Expr::IntLiteral(value)) => {
            Expr::U64Literal(value.to_u64().unwrap_or_else(|| {
                let bytes = value.to_signed_bytes_le();
                u64::from_le_bytes([
                    bytes.first().copied().unwrap_or(0),
                    bytes.get(1).copied().unwrap_or(0),
                    bytes.get(2).copied().unwrap_or(0),
                    bytes.get(3).copied().unwrap_or(0),
                    bytes.get(4).copied().unwrap_or(0),
                    bytes.get(5).copied().unwrap_or(0),
                    bytes.get(6).copied().unwrap_or(0),
                    bytes.get(7).copied().unwrap_or(0),
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
        (IntegerType::I8, IntegerType::I32, Expr::I8Literal(value)) => {
            Expr::I32Literal(value.into())
        }
        (IntegerType::I8, IntegerType::U32, Expr::I8Literal(value)) => {
            Expr::U32Literal((value as i32) as u32)
        }
        (IntegerType::I8, IntegerType::I64, Expr::I8Literal(value)) => {
            Expr::I64Literal(value.into())
        }
        (IntegerType::I8, IntegerType::U64, Expr::I8Literal(value)) => {
            Expr::U64Literal((value as i64) as u64)
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
        (IntegerType::U8, IntegerType::I32, Expr::U8Literal(value)) => {
            Expr::I32Literal(value.into())
        }
        (IntegerType::U8, IntegerType::U32, Expr::U8Literal(value)) => {
            Expr::U32Literal(value.into())
        }
        (IntegerType::U8, IntegerType::I64, Expr::U8Literal(value)) => {
            Expr::I64Literal(value.into())
        }
        (IntegerType::U8, IntegerType::U64, Expr::U8Literal(value)) => {
            Expr::U64Literal(value.into())
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
        (IntegerType::I16, IntegerType::I32, Expr::I16Literal(value)) => {
            Expr::I32Literal(value.into())
        }
        (IntegerType::I16, IntegerType::U32, Expr::I16Literal(value)) => {
            Expr::U32Literal((value as i32) as u32)
        }
        (IntegerType::I16, IntegerType::I64, Expr::I16Literal(value)) => {
            Expr::I64Literal(value.into())
        }
        (IntegerType::I16, IntegerType::U64, Expr::I16Literal(value)) => {
            Expr::U64Literal((value as i64) as u64)
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
        (IntegerType::U16, IntegerType::I32, Expr::U16Literal(value)) => {
            Expr::I32Literal(value.into())
        }
        (IntegerType::U16, IntegerType::U32, Expr::U16Literal(value)) => {
            Expr::U32Literal(value.into())
        }
        (IntegerType::U16, IntegerType::I64, Expr::U16Literal(value)) => {
            Expr::I64Literal(value.into())
        }
        (IntegerType::U16, IntegerType::U64, Expr::U16Literal(value)) => {
            Expr::U64Literal(value.into())
        }
        (IntegerType::I32, IntegerType::Int, Expr::I32Literal(value)) => {
            Expr::IntLiteral(value.into())
        }
        (IntegerType::I32, IntegerType::I8, Expr::I32Literal(value)) => {
            Expr::I8Literal(value as i8)
        }
        (IntegerType::I32, IntegerType::U8, Expr::I32Literal(value)) => {
            Expr::U8Literal(value as u8)
        }
        (IntegerType::I32, IntegerType::I16, Expr::I32Literal(value)) => {
            Expr::I16Literal(value as i16)
        }
        (IntegerType::I32, IntegerType::U16, Expr::I32Literal(value)) => {
            Expr::U16Literal(value as u16)
        }
        (IntegerType::I32, IntegerType::U32, Expr::I32Literal(value)) => {
            Expr::U32Literal(value as u32)
        }
        (IntegerType::I32, IntegerType::I64, Expr::I32Literal(value)) => {
            Expr::I64Literal(value.into())
        }
        (IntegerType::I32, IntegerType::U64, Expr::I32Literal(value)) => {
            Expr::U64Literal((value as i64) as u64)
        }
        (IntegerType::U32, IntegerType::Int, Expr::U32Literal(value)) => {
            Expr::IntLiteral(value.into())
        }
        (IntegerType::U32, IntegerType::I8, Expr::U32Literal(value)) => {
            Expr::I8Literal(value as i8)
        }
        (IntegerType::U32, IntegerType::U8, Expr::U32Literal(value)) => {
            Expr::U8Literal(value as u8)
        }
        (IntegerType::U32, IntegerType::I16, Expr::U32Literal(value)) => {
            Expr::I16Literal(value as i16)
        }
        (IntegerType::U32, IntegerType::U16, Expr::U32Literal(value)) => {
            Expr::U16Literal(value as u16)
        }
        (IntegerType::U32, IntegerType::I32, Expr::U32Literal(value)) => {
            Expr::I32Literal(value as i32)
        }
        (IntegerType::U32, IntegerType::I64, Expr::U32Literal(value)) => {
            Expr::I64Literal(value.into())
        }
        (IntegerType::U32, IntegerType::U64, Expr::U32Literal(value)) => {
            Expr::U64Literal(value.into())
        }
        (IntegerType::I64, IntegerType::Int, Expr::I64Literal(value)) => {
            Expr::IntLiteral(value.into())
        }
        (IntegerType::I64, IntegerType::I8, Expr::I64Literal(value)) => {
            Expr::I8Literal(value as i8)
        }
        (IntegerType::I64, IntegerType::U8, Expr::I64Literal(value)) => {
            Expr::U8Literal(value as u8)
        }
        (IntegerType::I64, IntegerType::I16, Expr::I64Literal(value)) => {
            Expr::I16Literal(value as i16)
        }
        (IntegerType::I64, IntegerType::U16, Expr::I64Literal(value)) => {
            Expr::U16Literal(value as u16)
        }
        (IntegerType::I64, IntegerType::I32, Expr::I64Literal(value)) => {
            Expr::I32Literal(value as i32)
        }
        (IntegerType::I64, IntegerType::U32, Expr::I64Literal(value)) => {
            Expr::U32Literal(value as u32)
        }
        (IntegerType::I64, IntegerType::U64, Expr::I64Literal(value)) => {
            Expr::U64Literal(value as u64)
        }
        (IntegerType::U64, IntegerType::Int, Expr::U64Literal(value)) => {
            Expr::IntLiteral(value.to_bigint().unwrap())
        }
        (IntegerType::U64, IntegerType::I8, Expr::U64Literal(value)) => {
            Expr::I8Literal(value as i8)
        }
        (IntegerType::U64, IntegerType::U8, Expr::U64Literal(value)) => {
            Expr::U8Literal(value as u8)
        }
        (IntegerType::U64, IntegerType::I16, Expr::U64Literal(value)) => {
            Expr::I16Literal(value as i16)
        }
        (IntegerType::U64, IntegerType::U16, Expr::U64Literal(value)) => {
            Expr::U16Literal(value as u16)
        }
        (IntegerType::U64, IntegerType::I32, Expr::U64Literal(value)) => {
            Expr::I32Literal(value as i32)
        }
        (IntegerType::U64, IntegerType::U32, Expr::U64Literal(value)) => {
            Expr::U32Literal(value as u32)
        }
        (IntegerType::U64, IntegerType::I64, Expr::U64Literal(value)) => {
            Expr::I64Literal(value as i64)
        }
        (source_type, dest_type, value_expr) => Expr::Builtin(Builtin::IntConvert {
            source_type,
            dest_type,
            value: Box::new(value_expr.with_location(value.location)),
        }),
    }
}

fn normalize_integer_binary_op<EC: ExprContext + ?Sized>(
    integer_type: IntegerType,
    lhs: LocatedExpr<EC>,
    rhs: LocatedExpr<EC>,
    int_op: impl FnOnce(num_bigint::BigInt, num_bigint::BigInt) -> Option<Expr<EC>>,
    i8_op: impl FnOnce(i8, i8) -> Option<Expr<EC>>,
    u8_op: impl FnOnce(u8, u8) -> Option<Expr<EC>>,
    i16_op: impl FnOnce(i16, i16) -> Option<Expr<EC>>,
    u16_op: impl FnOnce(u16, u16) -> Option<Expr<EC>>,
    i32_op: impl FnOnce(i32, i32) -> Option<Expr<EC>>,
    u32_op: impl FnOnce(u32, u32) -> Option<Expr<EC>>,
    i64_op: impl FnOnce(i64, i64) -> Option<Expr<EC>>,
    u64_op: impl FnOnce(u64, u64) -> Option<Expr<EC>>,
    rebuild: impl FnOnce(IntegerType, Box<LocatedExpr<EC>>, Box<LocatedExpr<EC>>) -> Builtin<EC>,
) -> Expr<EC> {
    let lhs_location = lhs.location;
    let rhs_location = rhs.location;
    match (integer_type, lhs.value, rhs.value) {
        (IntegerType::Int, Expr::IntLiteral(lhs), Expr::IntLiteral(rhs)) => {
            match int_op(lhs.clone(), rhs.clone()) {
                Some(expr) => expr,
                None => Expr::Builtin(rebuild(
                    IntegerType::Int,
                    Box::new(Expr::IntLiteral(lhs).with_location(lhs_location)),
                    Box::new(Expr::IntLiteral(rhs).with_location(rhs_location)),
                )),
            }
        }
        (IntegerType::I8, Expr::I8Literal(lhs), Expr::I8Literal(rhs)) => i8_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::I8,
                    Box::new(Expr::I8Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::I8Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (IntegerType::U8, Expr::U8Literal(lhs), Expr::U8Literal(rhs)) => u8_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::U8,
                    Box::new(Expr::U8Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::U8Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (IntegerType::I16, Expr::I16Literal(lhs), Expr::I16Literal(rhs)) => i16_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::I16,
                    Box::new(Expr::I16Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::I16Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (IntegerType::U16, Expr::U16Literal(lhs), Expr::U16Literal(rhs)) => u16_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::U16,
                    Box::new(Expr::U16Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::U16Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (IntegerType::I32, Expr::I32Literal(lhs), Expr::I32Literal(rhs)) => i32_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::I32,
                    Box::new(Expr::I32Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::I32Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (IntegerType::U32, Expr::U32Literal(lhs), Expr::U32Literal(rhs)) => u32_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::U32,
                    Box::new(Expr::U32Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::U32Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (IntegerType::I64, Expr::I64Literal(lhs), Expr::I64Literal(rhs)) => i64_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::I64,
                    Box::new(Expr::I64Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::I64Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (IntegerType::U64, Expr::U64Literal(lhs), Expr::U64Literal(rhs)) => u64_op(lhs, rhs)
            .unwrap_or_else(|| {
                Expr::Builtin(rebuild(
                    IntegerType::U64,
                    Box::new(Expr::U64Literal(lhs).with_location(lhs_location)),
                    Box::new(Expr::U64Literal(rhs).with_location(rhs_location)),
                ))
            }),
        (integer_type, lhs, rhs) => Expr::Builtin(rebuild(
            integer_type,
            Box::new(lhs.with_location(lhs_location)),
            Box::new(rhs.with_location(rhs_location)),
        )),
    }
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> FullNormalizer<S> {
    pub fn normalize(&mut self, expr: &mut LocatedExpr<S::EC>) {
        self.scan(expr);
    }
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> ExprScannerMut for FullNormalizer<S> {
    type EC = EC;

    fn scan(&mut self, expr: &mut LocatedExpr<Self::EC>) -> bool {
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
    use crate::{Expr, ExprContext, ExprLocationExt, LocatedExpr};
    use alloc::vec;
    use alloc::vec::Vec;
    use argon_util::Fuel;
    use parse18_runtime::{FilePosition, Location};

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
        type StaticMethod = ();
        type Instance = ();
    }

    fn location() -> Location {
        Location {
            file: Default::default(),
            start: FilePosition { line: 0, column: 0 },
            end: FilePosition { line: 0, column: 0 },
        }
    }

    struct TestNormalizer;

    impl Normalizer for TestNormalizer {
        type EC = TestContext;

        fn resolve_hole(&mut self, hole: &u8) -> Option<LocatedExpr<Self::EC>> {
            (*hole == 1).then_some(Expr::BoolLiteral(false).with_location(location()))
        }

        fn get_function_body(
            &mut self,
            function: &u8,
            arguments: &mut Vec<LocatedExpr<Self::EC>>,
        ) -> Option<LocatedExpr<Self::EC>> {
            if *function != 1 {
                return None;
            }

            match arguments
                .iter()
                .map(|argument| &argument.value)
                .collect::<Vec<_>>()
                .as_slice()
            {
                [Expr::BoolLiteral(false)] => {
                    Some(Expr::BoolLiteral(true).with_location(location()))
                }
                [_] => Some(Expr::Error.with_location(location())),
                [] => None,
                _ => None,
            }
        }
    }

    #[test]
    fn full_normalizer_normalizes_nested_expressions() {
        let mut expr = Expr::Tuple {
            items: vec![Expr::Hole(1).with_location(location())],
        }
        .with_location(location());
        let mut normalizer = FullNormalizer::new(Fuel::new(10), TestNormalizer);

        normalizer.normalize(&mut expr);

        assert_eq!(
            expr.value,
            Expr::Tuple {
                items: vec![Expr::BoolLiteral(false).with_location(location())]
            }
        );
    }

    #[test]
    fn full_normalizer_updates_children_before_current_expression() {
        let mut expr = Expr::FunctionCall {
            function: 1,
            arguments: vec![Expr::Hole(1).with_location(location())],
        }
        .with_location(location());
        let mut normalizer = FullNormalizer::new(Fuel::new(10), TestNormalizer);

        normalizer.normalize(&mut expr);

        assert_eq!(expr.value, Expr::BoolLiteral(true));
    }
}
use alloc::boxed::Box;
