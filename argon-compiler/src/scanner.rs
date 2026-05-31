use crate::{EffectInfo, Function};
use alloc::sync::Arc;
use argon_expr::{Expr, ExprContext, ExprScanner};
use core::marker::PhantomData;

pub struct PurityScanner<EC: ?Sized> {
    found_impure_function_call: bool,
    _phantom: PhantomData<*const EC>,
}

impl<EC> PurityScanner<EC>
where
    EC: ExprContext<Function = Arc<dyn Function>> + ?Sized,
{
    pub fn new() -> Self {
        Self {
            found_impure_function_call: false,
            _phantom: PhantomData,
        }
    }

    pub fn contains_impure_function_call(expr: &Expr<EC>) -> bool {
        let mut scanner = Self::new();
        scanner.scan(expr);
        scanner.found_impure_function_call()
    }

    pub fn found_impure_function_call(&self) -> bool {
        self.found_impure_function_call
    }
}

impl<EC> ExprScanner for PurityScanner<EC>
where
    EC: ExprContext<Function = Arc<dyn Function>> + ?Sized,
{
    type EC = EC;

    fn scan(&mut self, expr: &Expr<Self::EC>) -> bool {
        match expr {
            Expr::FunctionCall {
                function,
                arguments,
            } => {
                if function.metadata().effect_info == EffectInfo::Effectful {
                    self.found_impure_function_call = true;
                    return false;
                }

                arguments
                    .iter()
                    .all(|argument| self.scan(argument))
            }
            _ => argon_expr::default_scan(self, expr),
        }
    }
}
