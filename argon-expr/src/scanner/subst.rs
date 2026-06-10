use crate::{Expr, ExprContext, ExprScannerMut, Variable};
use alloc::borrow::Cow;
use hashbrown::HashMap;

pub struct SubstScanner<'a, EC: ExprContext + ?Sized> {
    substitutions: HashMap<Variable<EC>, Cow<'a, Expr<EC>>>,
}

impl<'a, EC: ExprContext + ?Sized> SubstScanner<'a, EC> {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
        }
    }

    pub fn add_substitution(&mut self, variable: Variable<EC>, replacement: Cow<'a, Expr<EC>>) {
        self.substitutions.insert(variable, replacement);
    }

    pub fn subst(
        variable: Variable<EC>,
        replacement: Cow<'a, Expr<EC>>,
        expr: &mut Expr<EC>,
    ) -> bool {
        let mut scanner = Self::new();
        scanner.add_substitution(variable, replacement);
        scanner.scan(expr)
    }
}

impl<EC: ExprContext + ?Sized> ExprScannerMut for SubstScanner<'_, EC> {
    type EC = EC;

    fn scan(&mut self, expr: &mut Expr<Self::EC>) -> bool {
        if let Expr::Variable(variable) = expr {
            if let Some(replacement) = self.substitutions.get(variable) {
                *expr = replacement.as_ref().clone();
                true
            } else {
                crate::default_scan_mut(self, expr)
            }
        } else {
            crate::default_scan_mut(self, expr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SubstScanner;
    use crate::{ErasureMode, Expr, ExprContext, ExprScannerMut, LocalVariable, Variable};
    use alloc::{boxed::Box, vec};
    use argon_util::UniqueIdentifier;

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

    fn variable() -> Variable<TestContext> {
        Variable::Local(Box::new(LocalVariable {
            id: UniqueIdentifier::new(),
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
    fn substitutes_multiple_variable_occurrences() {
        let first_variable = variable();
        let second_variable = variable();
        let mut expr = Expr::Tuple {
            items: vec![
                Expr::Variable(first_variable.clone()),
                Expr::Variable(second_variable.clone()),
                Expr::Variable(first_variable.clone()),
            ],
        };
        let first_replacement = Expr::BoolLiteral(true);
        let second_replacement = Expr::IntLiteral(5.into());

        let mut scanner = SubstScanner::new();
        scanner.add_substitution(first_variable, &first_replacement);
        scanner.add_substitution(second_variable.clone(), &second_replacement);
        assert!(scanner.scan(&mut expr));

        assert!(matches!(
            expr,
            Expr::Tuple {
                items,
            } if matches!(
                items.as_slice(),
                [
                    Expr::BoolLiteral(true),
                    Expr::IntLiteral(i),
                    Expr::BoolLiteral(true),
                ] if i == &5.into()
            )
        ));
    }

    #[test]
    fn substitutes_multiple_variables_simultaneously() {
        let first_variable = variable();
        let second_variable = variable();
        let mut expr = Expr::Tuple {
            items: vec![
                Expr::Variable(first_variable.clone()),
                Expr::Variable(second_variable.clone()),
            ],
        };
        let first_replacement = Expr::Variable(second_variable.clone());
        let second_replacement = Expr::BoolLiteral(true);

        let mut scanner = SubstScanner::new();
        scanner.add_substitution(first_variable, &first_replacement);
        scanner.add_substitution(second_variable.clone(), &second_replacement);
        assert!(scanner.scan(&mut expr));

        assert!(matches!(
            expr,
            Expr::Tuple {
                items,
            } if matches!(
                items.as_slice(),
                [
                    Expr::Variable(variable),
                    Expr::BoolLiteral(true),
                ] if variable == &second_variable
            )
        ));
    }

    #[test]
    fn does_not_substitute_store_targets() {
        let variable = variable();
        let mut expr = Expr::VariableStore(variable.clone(), Box::new(Expr::IntLiteral(1.into())));
        let replacement = Expr::BoolLiteral(true);

        assert!(SubstScanner::subst(variable, &replacement, &mut expr));

        assert!(matches!(expr, Expr::VariableStore(_, _)));
    }
}
