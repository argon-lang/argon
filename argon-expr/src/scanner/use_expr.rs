use crate::{Expr, ExprContext, ExprScanner};

pub struct UseExprScanner<EC: ExprContext + ?Sized> {
    found_use: bool,
    found_use_mut: bool,
    _phantom: core::marker::PhantomData<*const EC>,
}

impl<EC: ExprContext + ?Sized> Default for UseExprScanner<EC> {
    fn default() -> Self {
        Self::new()
    }
}

impl<EC: ExprContext + ?Sized> UseExprScanner<EC> {
    pub fn new() -> Self {
        Self {
            found_use: false,
            found_use_mut: false,
            _phantom: core::marker::PhantomData,
        }
    }

    pub fn found_use(&self) -> bool {
        self.found_use
    }

    pub fn found_use_mut(&self) -> bool {
        self.found_use_mut
    }

    pub fn contains_use(expr: &Expr<EC>) -> bool {
        let mut scanner = Self::new();
        scanner.scan(expr);
        scanner.found_use()
    }

    pub fn contains_use_mut(expr: &Expr<EC>) -> bool {
        let mut scanner = Self::new();
        scanner.scan(expr);
        scanner.found_use_mut()
    }
}

impl<EC: ExprContext + ?Sized> ExprScanner for UseExprScanner<EC> {
    type EC = EC;

    fn scan(&mut self, expr: &Expr<Self::EC>) -> bool {
        if let Expr::Use { is_mutable, .. } = expr {
            self.found_use = true;
            self.found_use_mut |= *is_mutable;
        }

        crate::default_scan(self, expr)
    }
}

#[cfg(test)]
mod tests {
    use super::UseExprScanner;
    use crate::{Expr, ExprContext};
    use alloc::{boxed::Box, vec};

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
    fn detects_use_expression() {
        let expr = Expr::<TestContext>::Use {
            is_mutable: false,
            inner: Box::new(Expr::IntLiteral(1.into())),
        };

        assert!(UseExprScanner::contains_use(&expr));
        assert!(!UseExprScanner::contains_use_mut(&expr));
    }

    #[test]
    fn detects_use_mut_expression() {
        let expr = Expr::<TestContext>::Use {
            is_mutable: true,
            inner: Box::new(Expr::IntLiteral(1.into())),
        };

        assert!(UseExprScanner::contains_use(&expr));
        assert!(UseExprScanner::contains_use_mut(&expr));
    }

    #[test]
    fn detects_nested_use_expression() {
        let expr = Expr::<TestContext>::Shared {
            inner: Box::new(Expr::Tuple {
                items: vec![Expr::Use {
                    is_mutable: false,
                    inner: Box::new(Expr::IntLiteral(1.into())),
                }],
            }),
        };

        assert!(UseExprScanner::contains_use(&expr));
    }

    #[test]
    fn ignores_non_use_expressions() {
        let expr = Expr::<TestContext>::Shared {
            inner: Box::new(Expr::Shared {
                inner: Box::new(Expr::IntLiteral(1.into())),
            }),
        };

        assert!(!UseExprScanner::contains_use(&expr));
        assert!(!UseExprScanner::contains_use_mut(&expr));
    }
}
