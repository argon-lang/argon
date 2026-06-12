use crate::{
    EffectInfo, Enum, EnumVariant, Function, Instance, Method, Record, RecordField, Trait,
};
use alloc::sync::Arc;
use argon_expr::{Expr, ExprContext, ExprScanner};
use core::marker::PhantomData;

pub struct PurityScanner<EC: ?Sized> {
    found_impure_function_call: bool,
    _phantom: PhantomData<*const EC>,
}

impl<EC> PurityScanner<EC>
where
    EC: ExprContext<
            Function = Arc<dyn Function>,
            Record = Arc<dyn Record>,
            Enum = Arc<dyn Enum>,
            Trait = Arc<dyn Trait>,
            RecordField = Arc<dyn RecordField>,
            EnumVariant = Arc<dyn EnumVariant>,
            Method = Arc<dyn Method>,
            Instance = Arc<dyn Instance>,
        > + ?Sized,
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
    EC: ExprContext<
            Function = Arc<dyn Function>,
            Record = Arc<dyn Record>,
            Enum = Arc<dyn Enum>,
            Trait = Arc<dyn Trait>,
            RecordField = Arc<dyn RecordField>,
            EnumVariant = Arc<dyn EnumVariant>,
            Method = Arc<dyn Method>,
            Instance = Arc<dyn Instance>,
        > + ?Sized,
{
    type EC = EC;

    fn scan(&mut self, expr: &Expr<Self::EC>) -> bool {
        match expr {
            Expr::FunctionCall { function, .. } => {
                if function.metadata().effect_info == EffectInfo::Effectful {
                    self.found_impure_function_call = true;
                    return false;
                }
            }

            Expr::MethodCall { .. } => todo!(),

            Expr::RecordFieldLoad { field, .. } => {
                if field.metadata().is_mutable {
                    return false;
                }
            }

            Expr::Error
            | Expr::Break { .. }
            | Expr::BreakIf { .. }
            | Expr::Retry { .. }
            | Expr::Finally { .. }
            | Expr::Raise { .. }
            | Expr::RecordFieldStore { .. }
            | Expr::Sequence(_)
            | Expr::VariableBinding(..)
            | Expr::VariableStore(..) => {
                return false;
            }

            _ => {}
        }

        argon_expr::default_scan(self, expr)
    }
}
