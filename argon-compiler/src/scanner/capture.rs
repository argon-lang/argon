use alloc::boxed::Box;
use argon_expr::{ClosureParameterVariable, Expr, ExprContext, ExprScanner, Variable};
use hashbrown::HashSet;

use super::FreeVariableScanner;

pub struct CaptureScanner<EC: ExprContext + ?Sized> {
    captures: HashSet<Variable<EC>>,
}

impl<EC> CaptureScanner<EC>
where
    EC: ExprContext + ?Sized,
{
    pub fn new() -> Self {
        Self {
            captures: HashSet::new(),
        }
    }

    pub fn scan_captures(expr: &Expr<EC>) -> HashSet<Variable<EC>> {
        let mut scanner = Self::new();
        scanner.scan(expr);
        scanner.into_captures()
    }

    pub fn into_captures(self) -> HashSet<Variable<EC>> {
        self.captures
    }

    fn scan_closure(&mut self, parameter: &ClosureParameterVariable<EC>, body: &Expr<EC>) {
        let mut captures = FreeVariableScanner::scan_free_variables(body);
        let closure_parameter = Variable::ClosureParameter(Box::new(parameter.clone()));
        captures.remove(&closure_parameter);
        self.captures.extend(captures);
    }
}

impl<EC> Default for CaptureScanner<EC>
where
    EC: ExprContext + ?Sized,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<EC> ExprScanner for CaptureScanner<EC>
where
    EC: ExprContext + ?Sized,
{
    type EC = EC;

    fn scan(&mut self, expr: &Expr<Self::EC>) -> bool {
        if let Expr::Closure { v, body, .. } = expr {
            self.scan_closure(v, body);
        }

        argon_expr::default_scan(self, expr)
    }
}

#[cfg(test)]
mod tests {
    use super::CaptureScanner;
    use alloc::boxed::Box;
    use alloc::vec;
    use argon_expr::{
        ClosureParameterVariable, ErasureMode, Expr, ExprContext, LocalVariable, Variable,
    };
    use argon_util::UniqueIdentifier;
    use mitsein::vec1::Vec1;

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

    fn local_variable() -> Box<LocalVariable<TestContext>> {
        Box::new(LocalVariable {
            id: UniqueIdentifier::new(),
            name: None,
            var_type: Expr::int_type(),
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
            is_mutable: false,
        })
    }

    fn closure_parameter() -> Box<ClosureParameterVariable<TestContext>> {
        Box::new(ClosureParameterVariable {
            id: UniqueIdentifier::new(),
            var_type: Expr::int_type(),
            name: None,
            is_mutable: false,
            erasure_mode: ErasureMode::Concrete,
            is_witness: false,
        })
    }

    #[test]
    fn closure_captures_free_variables_from_body() {
        let parameter = closure_parameter();
        let parameter_var = Variable::ClosureParameter(parameter.clone());
        let captured = Variable::Local(local_variable());
        let body = Expr::Tuple {
            items: vec![
                Expr::Variable(parameter_var),
                Expr::Variable(captured.clone()),
            ],
        };
        let expr = Expr::Closure {
            v: parameter.clone(),
            return_type: Box::new(Expr::int_type()),
            body: Box::new(body),
        };

        let captures = CaptureScanner::scan_captures(&expr);

        assert!(captures.contains(&captured));
        assert!(!captures.contains(&Variable::ClosureParameter(parameter)));
    }

    #[test]
    fn closure_does_not_capture_locals_bound_in_body() {
        let parameter = closure_parameter();
        let bound = Variable::Local(local_variable());
        let body = Expr::Sequence(
            Vec1::try_from(vec![
                Expr::VariableBinding(bound.clone(), Box::new(Expr::IntLiteral(1.into()))),
                Expr::Variable(bound.clone()),
            ])
            .ok()
            .unwrap(),
        );
        let expr = Expr::Closure {
            v: parameter.clone(),
            return_type: Box::new(Expr::int_type()),
            body: Box::new(body),
        };

        let captures = CaptureScanner::scan_captures(&expr);

        assert!(!captures.contains(&bound));
        assert!(!captures.contains(&Variable::ClosureParameter(parameter)));
    }

    #[test]
    fn outer_closure_captures_variables_referenced_by_nested_closure() {
        let outer_parameter = closure_parameter();
        let inner_parameter = closure_parameter();
        let captured = Variable::Local(local_variable());

        let inner = Expr::Closure {
            v: inner_parameter.clone(),
            return_type: Box::new(Expr::int_type()),
            body: Box::new(Expr::Variable(captured.clone())),
        };
        let outer = Expr::Closure {
            v: outer_parameter.clone(),
            return_type: Box::new(Expr::int_type()),
            body: Box::new(inner),
        };

        let captures = CaptureScanner::scan_captures(&outer);

        assert!(captures.contains(&captured));
        assert!(!captures.contains(&Variable::ClosureParameter(outer_parameter)));
        assert!(!captures.contains(&Variable::ClosureParameter(inner_parameter)));
    }
}
