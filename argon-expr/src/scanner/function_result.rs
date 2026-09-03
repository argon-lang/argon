use crate::{Expr, ExprContext, ExprScannerMut, LocatedExpr, default_scan_mut};
use alloc::borrow::Cow;

pub struct FunctionResultValueSubstScanner<'a, EC: ExprContext + ?Sized> {
    replacement: Cow<'a, LocatedExpr<EC>>,
}

impl<'a, EC: ExprContext + ?Sized> FunctionResultValueSubstScanner<'a, EC> {
    pub fn new(replacement: Cow<'a, LocatedExpr<EC>>) -> Self {
        Self { replacement }
    }

    pub fn subst(replacement: Cow<'a, LocatedExpr<EC>>, expr: &mut LocatedExpr<EC>) -> bool {
        let mut scanner = Self::new(replacement);
        scanner.scan(expr)
    }
}

impl<EC: ExprContext + ?Sized> ExprScannerMut for FunctionResultValueSubstScanner<'_, EC> {
    type EC = EC;

    fn scan(&mut self, expr: &mut LocatedExpr<Self::EC>) -> bool {
        if matches!(expr.value, Expr::FunctionResultValue { .. }) {
            *expr = self.replacement.as_ref().clone();
            true
        } else {
            default_scan_mut(self, expr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::FunctionResultValueSubstScanner;
    use crate::{Expr, ExprContext, ExprLocationExt, ExprScannerMut};
    use alloc::{borrow::Cow, boxed::Box, vec};
    use parse18_runtime::{FilePosition, Location};

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

    #[test]
    fn substitutes_function_result_value_occurrences() {
        let mut expr = Expr::Tuple {
            items: vec![
                Expr::FunctionResultValue {
                    result_type: Box::new(
                        Expr::<TestContext>::int_type().with_location(location()),
                    ),
                }
                .with_location(location()),
                Expr::BoolLiteral(false).with_location(location()),
                Expr::FunctionResultValue {
                    result_type: Box::new(
                        Expr::<TestContext>::int_type().with_location(location()),
                    ),
                }
                .with_location(location()),
            ],
        }
        .with_location(location());
        let replacement = Expr::IntLiteral(123.into()).with_location(location());

        assert!(FunctionResultValueSubstScanner::subst(
            Cow::Borrowed(&replacement),
            &mut expr
        ));

        assert!(matches!(
            expr,
            crate::LocatedExpr {
                value: Expr::Tuple {
                    items,
                },
                ..
            } if matches!(
                items.as_slice(),
                [
                    first,
                    second,
                    third,
                ] if matches!(
                    (&first.value, &second.value, &third.value),
                    (Expr::IntLiteral(first), Expr::BoolLiteral(false), Expr::IntLiteral(second))
                        if first == &123.into() && second == &123.into()
                )
            )
        ));
    }

    #[test]
    fn substitutes_root_function_result_value() {
        let mut expr = Expr::FunctionResultValue {
            result_type: Box::new(Expr::<TestContext>::int_type().with_location(location())),
        }
        .with_location(location());
        let replacement = Expr::IntLiteral(123.into()).with_location(location());

        assert!(FunctionResultValueSubstScanner::subst(
            Cow::Borrowed(&replacement),
            &mut expr
        ));

        assert!(matches!(expr.value, Expr::IntLiteral(value) if value == 123.into()));
    }

    #[test]
    fn does_not_rescan_replacement() {
        let mut expr = Expr::FunctionResultValue {
            result_type: Box::new(Expr::<TestContext>::int_type().with_location(location())),
        }
        .with_location(location());
        let replacement = Expr::FunctionResultValue {
            result_type: Box::new(Expr::<TestContext>::bool_type().with_location(location())),
        }
        .with_location(location());

        let mut scanner = FunctionResultValueSubstScanner::new(Cow::Borrowed(&replacement));
        assert!(scanner.scan(&mut expr));

        assert!(matches!(
            expr,
            crate::LocatedExpr {
                value: Expr::FunctionResultValue {
                    result_type,
                },
                ..
            } if matches!(result_type.value, Expr::Builtin(crate::Builtin::BoolType))
        ));
    }
}
