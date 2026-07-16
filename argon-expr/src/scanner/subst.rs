use crate::{Expr, ExprContext, ExprScannerMut, LocatedExpr, Variable};
use alloc::borrow::Cow;
use hashbrown::HashMap;

pub struct SubstScanner<'a, EC: ExprContext + ?Sized> {
    substitutions: HashMap<Variable<EC>, Cow<'a, LocatedExpr<EC>>>,
}

impl<'a, EC: ExprContext + ?Sized> SubstScanner<'a, EC> {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
        }
    }

    pub fn add_substitution(
        &mut self,
        variable: Variable<EC>,
        replacement: Cow<'a, LocatedExpr<EC>>,
    ) {
        self.substitutions.insert(variable, replacement);
    }

    pub fn subst(
        variable: Variable<EC>,
        replacement: Cow<'a, LocatedExpr<EC>>,
        expr: &mut LocatedExpr<EC>,
    ) -> bool {
        let mut scanner = Self::new();
        scanner.add_substitution(variable, replacement);
        scanner.scan(expr)
    }
}

impl<EC: ExprContext + ?Sized> ExprScannerMut for SubstScanner<'_, EC> {
    type EC = EC;

    fn scan(&mut self, expr: &mut LocatedExpr<Self::EC>) -> bool {
        if let Expr::Variable(variable) = &expr.value {
            if let Some(replacement) = self.substitutions.get(variable) {
                *expr = replacement.as_ref().clone();
                return true;
            }
        }

        self.scan_expr(&mut expr.value)
    }
}

#[cfg(test)]
mod tests {
    use super::SubstScanner;
    use crate::{
        ErasureMode, Expr, ExprContext, ExprLocationExt, ExprScannerMut, LocalVariable, Variable,
    };
    use alloc::{borrow::Cow, boxed::Box, vec};
    use argon_util::UniqueIdentifier;
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
        type Instance = ();
    }

    fn location() -> Location {
        Location {
            file: Default::default(),
            start: FilePosition { line: 0, column: 0 },
            end: FilePosition { line: 0, column: 0 },
        }
    }

    fn variable() -> Variable<TestContext> {
        Variable::Local(Box::new(LocalVariable {
            id: UniqueIdentifier::new(),
            name: None,
            var_type: Expr::Error.with_location(location()),
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
                Expr::Variable(variable.clone()).with_location(location()),
                Expr::BoolLiteral(false).with_location(location()),
                Expr::Variable(variable.clone()).with_location(location()),
            ],
        }
        .with_location(location());
        let replacement = Expr::BoolLiteral(true).with_location(location());

        assert!(SubstScanner::subst(
            variable,
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
                    (
                        Expr::BoolLiteral(true),
                        Expr::BoolLiteral(false),
                        Expr::BoolLiteral(true),
                    )
                )
            )
        ));
    }

    #[test]
    fn substitutes_root_variable() {
        let variable = variable();
        let mut expr = Expr::Variable(variable.clone()).with_location(location());
        let replacement = Expr::BoolLiteral(true).with_location(location());

        assert!(SubstScanner::subst(
            variable,
            Cow::Borrowed(&replacement),
            &mut expr
        ));

        assert!(matches!(expr.value, Expr::BoolLiteral(true)));
    }

    #[test]
    fn substitutes_multiple_variable_occurrences() {
        let first_variable = variable();
        let second_variable = variable();
        let mut expr = Expr::Tuple {
            items: vec![
                Expr::Variable(first_variable.clone()).with_location(location()),
                Expr::Variable(second_variable.clone()).with_location(location()),
                Expr::Variable(first_variable.clone()).with_location(location()),
            ],
        }
        .with_location(location());
        let first_replacement = Expr::BoolLiteral(true).with_location(location());
        let second_replacement = Expr::IntLiteral(5.into()).with_location(location());

        let mut scanner = SubstScanner::new();
        scanner.add_substitution(first_variable, Cow::Borrowed(&first_replacement));
        scanner.add_substitution(second_variable.clone(), Cow::Borrowed(&second_replacement));
        assert!(scanner.scan(&mut expr));

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
                    (
                        Expr::BoolLiteral(true),
                        Expr::IntLiteral(i),
                        Expr::BoolLiteral(true),
                    ) if i == &5.into()
                )
            )
        ));
    }

    #[test]
    fn substitutes_multiple_variables_simultaneously() {
        let first_variable = variable();
        let second_variable = variable();
        let mut expr = Expr::Tuple {
            items: vec![
                Expr::Variable(first_variable.clone()).with_location(location()),
                Expr::Variable(second_variable.clone()).with_location(location()),
            ],
        }
        .with_location(location());
        let first_replacement = Expr::Variable(second_variable.clone()).with_location(location());
        let second_replacement = Expr::BoolLiteral(true).with_location(location());

        let mut scanner = SubstScanner::new();
        scanner.add_substitution(first_variable, Cow::Borrowed(&first_replacement));
        scanner.add_substitution(second_variable.clone(), Cow::Borrowed(&second_replacement));
        assert!(scanner.scan(&mut expr));

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
                ] if matches!(
                    (&first.value, &second.value),
                    (Expr::Variable(variable), Expr::BoolLiteral(true))
                        if variable == &second_variable
                )
            )
        ));
    }

    #[test]
    fn does_not_substitute_store_targets() {
        let variable = variable();
        let mut expr = Expr::VariableStore(
            variable.clone(),
            Box::new(Expr::IntLiteral(1.into()).with_location(location())),
        )
        .with_location(location());
        let replacement = Expr::BoolLiteral(true).with_location(location());

        assert!(SubstScanner::subst(
            variable,
            Cow::Borrowed(&replacement),
            &mut expr
        ));

        assert!(matches!(expr.value, Expr::VariableStore(_, _)));
    }
}
