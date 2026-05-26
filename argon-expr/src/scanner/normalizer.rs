use crate::{Expr, ExprContext, ExprScannerMut, FunctionArgument, default_scan_mut};
use alloc::vec::Vec;
use argon_util::Fuel;
use core::marker::PhantomData;

pub struct NormalizerScanner<EC: ?Sized, S> {
    fuel: Fuel,
    state: S,
    dummy: PhantomData<*const EC>,
}

impl<EC: ?Sized, S> NormalizerScanner<EC, S> {
    pub fn new(fuel: Fuel, state: S) -> Self {
        Self {
            fuel,
            state,
            dummy: PhantomData,
        }
    }
}

pub trait Normalizer<EC: ExprContext + ?Sized> {
    fn get_function_body(
        &mut self,
        function: &EC::Function,
        arguments: &mut Vec<FunctionArgument<EC>>,
    ) -> Option<Expr<EC>>;
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC>> ExprScannerMut for NormalizerScanner<EC, S> {
    type EC = EC;

    fn scan(&mut self, expr: &mut Expr<Self::EC>) -> bool {
        if !self.fuel.is_empty() {
            match expr {
                Expr::FunctionCall {
                    function,
                    arguments,
                } => {
                    if let Some(body) = self.state.get_function_body(function, arguments) {
                        *expr = body;

                        let old_fuel = self.fuel.clone();
                        self.fuel.consume();
                        self.scan(expr);
                        self.fuel = old_fuel;
                    }
                }
                _ => {}
            }
        }

        default_scan_mut(self, expr)
    }
}
