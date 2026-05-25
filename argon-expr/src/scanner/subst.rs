use crate::{Expr, ExprContext, ExprScannerMut, Variable};

pub struct SubstScanner<'a, EC: ExprContext + ?Sized> {
    variable: Variable<EC>,
    replacement: &'a Expr<EC>,
}

impl<'a, EC: ExprContext + ?Sized> SubstScanner<'a, EC> {
    pub fn new(variable: Variable<EC>, replacement: &'a Expr<EC>) -> Self {
        Self {
            variable,
            replacement,
        }
    }

    pub fn subst(variable: Variable<EC>, replacement: &'a Expr<EC>, expr: &mut Expr<EC>) -> bool {
        Self::new(variable, replacement).scan(expr)
    }
}

impl<EC: ExprContext + ?Sized> ExprScannerMut for SubstScanner<'_, EC> {
    type EC = EC;

    fn scan(&mut self, expr: &mut Expr<Self::EC>) -> bool {
        let replace = matches!(expr, Expr::Variable(variable) if variable == &self.variable);

        if replace {
            *expr = self.replacement.clone();
            true
        } else {
            crate::default_scan_mut(self, expr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SubstScanner;
    use crate::{ErasureMode, Expr, ExprContext, LocalVariable, Variable};
    use std::sync::Arc;

    #[derive(Debug, Eq, Hash, PartialEq)]
    struct TestContext;

    impl ExprContext for TestContext {
        type Hole = ();
        type Function = ();
        type Record = ();
        type Enum = ();
        type Trait = ();
        type EnumVariant = ();
        type Method = ();
        type Instance = ();
    }

    fn variable() -> Variable<TestContext> {
        Variable::Local(Arc::new(LocalVariable {
            name: None,
            var_type: Expr::Error,
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
            is_mutable: false,
        }))
    }

    #[test]
    fn substitutes_variable_occurrences() {
        let variable = variable();
        let mut expr = Expr::Tuple {
            items: vec![
                Expr::Variable(variable.clone()),
                Expr::BoolLiteral(false),
                Expr::Variable(variable.clone()),
            ],
        };
        let replacement = Expr::BoolLiteral(true);

        assert!(SubstScanner::subst(variable, &replacement, &mut expr));

        assert!(matches!(
            expr,
            Expr::Tuple {
                items,
            } if matches!(
                items.as_slice(),
                [
                    Expr::BoolLiteral(true),
                    Expr::BoolLiteral(false),
                    Expr::BoolLiteral(true),
                ]
            )
        ));
    }

    #[test]
    fn does_not_substitute_store_targets() {
        let variable = variable();
        let mut expr = Expr::StoreVariable(variable.clone());
        let replacement = Expr::BoolLiteral(true);

        assert!(SubstScanner::subst(variable, &replacement, &mut expr));

        assert!(matches!(expr, Expr::StoreVariable(_)));
    }
}
