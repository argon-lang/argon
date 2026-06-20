use crate::{Expr, ExprContext};
use alloc::vec::Vec;
use argon_util::Fuel;
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

    pub fn normalize_impl(&mut self, mut expr: Expr<S::EC>) -> Expr<S::EC> {
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

                    _ => {}
                }

                break 'updated_no_growth;
            }

            self.fuel.consume();
        }

        expr
    }
}
