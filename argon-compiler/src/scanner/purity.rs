use crate::{
    EffectInfo, Enum, EnumVariant, Function, Instance, Method, Record, RecordField, Trait,
};
use alloc::sync::Arc;
use argon_expr::{Expr, ExprContext, ExprScanner, LocatedExpr};
use core::marker::PhantomData;

pub struct PurityScanner<EC: ?Sized> {
    found_impure: bool,
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
            found_impure: false,
            _phantom: PhantomData,
        }
    }

    pub fn contains_impure_function_call(expr: &LocatedExpr<EC>) -> bool {
        let mut scanner = Self::new();
        scanner.scan(expr);
        scanner.found_impure()
    }

    pub fn found_impure(&self) -> bool {
        self.found_impure
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

    fn scan_expr(&mut self, expr: &Expr<Self::EC>) -> bool {
        match expr {
            Expr::FunctionCall { function, .. } => {
                if function.metadata().effect_info == EffectInfo::Effectful {
                    self.found_impure = true;
                    return false;
                }
            }

            Expr::MethodCall { method, .. } => {
                if method.metadata().effect_info == EffectInfo::Effectful {
                    self.found_impure = true;
                    return false;
                }
            }

            Expr::RecordFieldLoad { field, .. } => {
                if field.metadata().is_mutable {
                    self.found_impure = true;
                    return false;
                }
            }

            Expr::Break { .. }
            | Expr::Retry { .. }
            | Expr::Finally { .. }
            | Expr::Raise { .. }
            | Expr::RecordFieldStore { .. }
            | Expr::VariableStore(..) => {
                self.found_impure = true;
                return false;
            }

            Expr::Error => {
                return false;
            }

            _ => {}
        }

        argon_expr::default_scan_expr(self, expr)
    }
}
