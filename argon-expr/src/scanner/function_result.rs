use crate::{Expr, ExprContext, ExprScannerMut};
use alloc::borrow::Cow;

pub struct FunctionResultValueSubstScanner<'a, EC: ExprContext + ?Sized> {
    replacement: Cow<'a, Expr<EC>>,
}

impl<'a, EC: ExprContext + ?Sized> FunctionResultValueSubstScanner<'a, EC> {
    pub fn new(replacement: Cow<'a, Expr<EC>>) -> Self {
        Self { replacement }
    }

    pub fn subst(replacement: Cow<'a, Expr<EC>>, expr: &mut Expr<EC>) -> bool {
        let mut scanner = Self::new(replacement);
        scanner.scan(expr)
    }
}

impl<EC: ExprContext + ?Sized> ExprScannerMut for FunctionResultValueSubstScanner<'_, EC> {
    type EC = EC;

    fn scan(&mut self, expr: &mut Expr<Self::EC>) -> bool {
        if matches!(expr, Expr::FunctionResultValue { .. }) {
            *expr = self.replacement.as_ref().clone();
            true
        } else {
            crate::default_scan_mut(self, expr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::FunctionResultValueSubstScanner;
    use crate::{Expr, ExprContext, ExprScannerMut};
    use alloc::{borrow::Cow, boxed::Box, vec};

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

    #[test]
    fn substitutes_function_result_value_occurrences() {
        let mut expr: Expr<TestContext> = Expr::Tuple {
            items: vec![
                Expr::FunctionResultValue {
                    result_type: Box::new(Expr::<TestContext>::int_type()),
                },
                Expr::BoolLiteral(false),
                Expr::FunctionResultValue {
                    result_type: Box::new(Expr::<TestContext>::int_type()),
                },
            ],
        };
        let replacement = Expr::IntLiteral(123.into());

        assert!(FunctionResultValueSubstScanner::subst(
            Cow::Borrowed(&replacement),
            &mut expr
        ));

        assert!(matches!(
            expr,
            Expr::Tuple {
                items,
            } if matches!(
                items.as_slice(),
                [
                    Expr::IntLiteral(first),
                    Expr::BoolLiteral(false),
                    Expr::IntLiteral(second),
                ] if first == &123.into() && second == &123.into()
            )
        ));
    }

    #[test]
    fn does_not_rescan_replacement() {
        let mut expr: Expr<TestContext> = Expr::FunctionResultValue {
            result_type: Box::new(Expr::<TestContext>::int_type()),
        };
        let replacement = Expr::FunctionResultValue {
            result_type: Box::new(Expr::<TestContext>::bool_type()),
        };

        let mut scanner = FunctionResultValueSubstScanner::new(Cow::Borrowed(&replacement));
        assert!(scanner.scan(&mut expr));

        assert!(matches!(
            expr,
            Expr::FunctionResultValue {
                result_type,
            } if matches!(*result_type, Expr::Builtin(crate::Builtin::BoolType))
        ));
    }
}
