use crate::{Expr, ExprContext, ExprScannerMut};
use alloc::vec::Vec;
use argon_util::Fuel;

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
        while !self.fuel.is_empty() {
            'updated: {
                match expr {
                    Expr::Hole(hole) => {
                        if let Some(resolved) = self.state.resolve_hole(hole) {
                            *expr = resolved;
                            break 'updated;
                        }
                    }

                    Expr::FunctionCall {
                        function,
                        arguments,
                    } => {
                        if let Some(body) = self.state.get_function_body(function, arguments) {
                            *expr = body;
                            break 'updated;
                        }
                    }
                    _ => {}
                }

                return;
            }

            self.fuel.consume();
            self.normalize(expr);
        }
    }
}
