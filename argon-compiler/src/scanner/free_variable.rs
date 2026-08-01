use alloc::boxed::Box;
use argon_expr::{Expr, ExprContext, ExprScanner, LocatedExpr, Pattern, Variable};
use hashbrown::HashSet;

pub struct FreeVariableScanner<EC: ExprContext + ?Sized> {
    bound_variables: HashSet<Variable<EC>>,
    referenced_variables: HashSet<Variable<EC>>,
}

impl<EC> FreeVariableScanner<EC>
where
    EC: ExprContext + ?Sized,
{
    pub fn new() -> Self {
        Self {
            bound_variables: HashSet::new(),
            referenced_variables: HashSet::new(),
        }
    }

    pub fn scan_free_variables(expr: &LocatedExpr<EC>) -> HashSet<Variable<EC>> {
        let mut scanner = Self::new();
        scanner.scan(expr);
        scanner.into_free_variables()
    }

    pub fn into_free_variables(mut self) -> HashSet<Variable<EC>> {
        self.referenced_variables
            .retain(|variable| !self.bound_variables.contains(variable));
        self.referenced_variables
    }

    fn bind_variable(&mut self, variable: Variable<EC>) {
        self.bound_variables.insert(variable);
    }
}

impl<EC> Default for FreeVariableScanner<EC>
where
    EC: ExprContext + ?Sized,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<EC> ExprScanner for FreeVariableScanner<EC>
where
    EC: ExprContext + ?Sized,
{
    type EC = EC;

    fn scan(&mut self, expr: &LocatedExpr<Self::EC>) -> bool {
        match &expr.value {
            Expr::Closure { v, .. } => {
                self.bind_variable(Variable::ClosureParameter(v.clone()));
            }

            Expr::FunctionType { a, .. } => {
                self.bind_variable(Variable::ClosureParameter(a.clone()))
            }

            Expr::Condition {
                when_true_witness,
                when_false_witness,
                ..
            } => {
                for v in [when_true_witness.as_ref(), when_false_witness.as_ref()]
                    .into_iter()
                    .flatten()
                {
                    self.bind_variable(Variable::Local(v.clone()));
                }
            }

            Expr::BindEnsures { variables, .. } => {
                for variable in variables {
                    self.bind_variable(Variable::Local(variable.clone()));
                }
            }

            Expr::BindErasedAlias {
                variable,
                equality_witness,
                ..
            } => {
                self.bind_variable(Variable::Local(variable.clone()));
                if let Some(equality_witness) = equality_witness {
                    self.bind_variable(Variable::Local(equality_witness.clone()));
                }
            }

            Expr::VariableBinding(variable, _) => {
                self.bind_variable(Variable::Local(variable.clone()))
            }

            _ => {}
        }

        argon_expr::default_scan(self, expr)
    }

    fn scan_variable(&mut self, v: &Variable<Self::EC>) -> bool {
        self.referenced_variables.insert(v.clone());

        argon_expr::default_scan_variable(self, v)
    }

    fn scan_pattern_value(&mut self, pattern: &Pattern<Self::EC>) -> bool {
        match pattern {
            Pattern::Binding(variable, pattern) => {
                self.scan(&variable.var_type) && self.scan_pattern(pattern) && {
                    self.bind_variable(Variable::Local(Box::new(variable.clone())));
                    true
                }
            }
            _ => argon_expr::default_scan_pattern_value(self, pattern),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::FreeVariableScanner;
    use alloc::boxed::Box;
    use alloc::vec;
    use argon_expr::{
        ClosureParameterVariable, ErasureMode, Expr, ExprContext, ExprLocationExt, ExprScanner,
        LocalVariable, Variable,
    };
    use argon_util::UniqueIdentifier;
    use mitsein::vec1::Vec1;
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

    fn local_variable() -> Box<LocalVariable<TestContext>> {
        Box::new(LocalVariable {
            id: UniqueIdentifier::new(),
            name: None,
            var_type: Expr::int_type().with_location(location()),
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
            is_mutable: false,
        })
    }

    fn closure_parameter() -> Box<ClosureParameterVariable<TestContext>> {
        Box::new(ClosureParameterVariable {
            id: UniqueIdentifier::new(),
            var_type: Expr::int_type().with_location(location()),
            name: None,
            is_mutable: false,
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
        })
    }

    #[test]
    fn sequence_binding_is_not_free_after_declaration() {
        let bound = local_variable();
        let free = Variable::Local(local_variable());
        let expr = Expr::Sequence(
            Vec1::try_from(vec![
                Expr::VariableBinding(
                    bound.clone(),
                    Box::new(Expr::IntLiteral(1.into()).with_location(location())),
                )
                .with_location(location()),
                Expr::Variable(Variable::Local(bound.clone())).with_location(location()),
                Expr::Variable(free.clone()).with_location(location()),
            ])
            .ok()
            .unwrap(),
        )
        .with_location(location());

        let free_variables = FreeVariableScanner::scan_free_variables(&expr);

        assert!(!free_variables.contains(&Variable::Local(bound)));
        assert!(free_variables.contains(&free));
    }

    #[test]
    fn closure_parameter_is_not_free_in_body() {
        let parameter = closure_parameter();
        let bound = Variable::ClosureParameter(parameter.clone());
        let free = Variable::Local(local_variable());
        let expr = Expr::Closure {
            v: parameter.clone(),
            return_type: Box::new(Expr::Variable(bound.clone()).with_location(location())),
            body: Box::new(
                Expr::Tuple {
                    items: vec![
                        Expr::Variable(bound.clone()).with_location(location()),
                        Expr::Variable(free.clone()).with_location(location()),
                    ],
                }
                .with_location(location()),
            ),
        }
        .with_location(location());

        let mut scanner = FreeVariableScanner::new();
        scanner.scan(&expr);

        let free_variables = scanner.into_free_variables();

        assert!(!free_variables.contains(&bound));
        assert!(free_variables.contains(&free));
    }
}
