use crate::{Expr, ExprContext, FunctionArgument, MatchCase, RecordFieldLiteral, Variable};

pub trait ExprScanner {
    type EC: ExprContext + ?Sized;

    fn scan_hole(&mut self, _hole: &<Self::EC as ExprContext>::Hole) -> bool {
        true
    }

    fn scan(&mut self, expr: &Expr<Self::EC>) -> bool {
        default_scan(self, expr)
    }

    fn scan_variable(&mut self, v: &Variable<Self::EC>) -> bool {
        default_scan_variable(self, v)
    }
}

pub fn default_scan<S>(scanner: &mut S, expr: &Expr<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    match expr {
        Expr::Error => true,
        Expr::Hole(hole) => scanner.scan_hole(hole),
        Expr::As { value, value_type } => scanner.scan(value) && scanner.scan(value_type),
        Expr::Assert { t } => scanner.scan(t),
        Expr::Ensures {
            block_body,
            ensures_body,
        } => {
            scanner.scan(block_body) && ensures_body.as_ref().is_none_or(|body| scanner.scan(body))
        }
        Expr::BoolLiteral(_) => true,
        Expr::Break { .. } => true,
        Expr::Builtin { arguments, .. } => arguments.iter().all(|argument| scanner.scan(argument)),
        Expr::Dot { o, .. } => scanner.scan(o),
        Expr::EnumType(_, arguments) => arguments.iter().all(|argument| scanner.scan(argument)),
        Expr::FunctionLiteral { body, .. } => scanner.scan(body),
        Expr::FunctionCall { arguments, .. } => arguments
            .iter()
            .all(|argument: &FunctionArgument<S::EC>| scanner.scan(&argument.arg)),
        Expr::FunctionObjectCall { function, argument } => {
            scanner.scan(function) && scanner.scan(argument)
        }
        Expr::FunctionResultValue => true,
        Expr::FunctionType { a, r } => scanner.scan(a) && scanner.scan(r),
        Expr::IfElse {
            when_true_var,
            when_false_var,
            condition,
            when_true,
            when_false,
        } => {
            when_true_var
                .as_ref()
                .is_none_or(|variable| scanner.scan_variable(variable))
                && when_false_var
                    .as_ref()
                    .is_none_or(|variable| scanner.scan_variable(variable))
                && scanner.scan(condition)
                && scanner.scan(when_true)
                && scanner.scan(when_false)
        }
        Expr::IntLiteral(_) => true,
        Expr::Is { value, .. } => scanner.scan(value),
        Expr::Loop { body, .. } => scanner.scan(body),
        Expr::Match { value, cases } => {
            scanner.scan(value)
                && cases
                    .iter()
                    .all(|case: &MatchCase<S::EC>| scanner.scan(&case.body))
        }
        Expr::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            scanner.scan(receiver)
                && arguments
                    .iter()
                    .all(|argument: &FunctionArgument<S::EC>| scanner.scan(&argument.arg))
        }
        Expr::NewTraitObject { .. } => true,
        Expr::Next { .. } => true,
        Expr::Raise { ex } => scanner.scan(ex),
        Expr::RecordLiteral { fields, .. } => fields
            .iter()
            .all(|field: &RecordFieldLiteral<S::EC>| scanner.scan(&field.value)),
        Expr::RecordType(_, arguments) => arguments.iter().all(|argument| scanner.scan(argument)),
        Expr::Redo { .. } => true,
        Expr::Sequence(exprs) => exprs.iter().all(|expr| scanner.scan(expr)),
        Expr::StoreVariable(variable) => scanner.scan_variable(variable),
        Expr::StringLiteral(_) => true,
        Expr::TraitType(_, arguments) => arguments.iter().all(|argument| scanner.scan(argument)),
        Expr::Tuple { items } => items.iter().all(|item| scanner.scan(item)),
        Expr::TupleElement(value, _) => scanner.scan(value),
        Expr::AnyType => true,
        Expr::Type(t) => scanner.scan(t),
        Expr::BigType(_) => true,
        Expr::Variable(variable) => scanner.scan_variable(variable),
        Expr::VariableStore(variable, value) => {
            scanner.scan_variable(variable) && scanner.scan(value)
        }
        Expr::While {
            condition, body, ..
        } => scanner.scan(condition) && scanner.scan(body),
        Expr::BoxedType { t } => scanner.scan(t),
        Expr::Box { value } => scanner.scan(value),
        Expr::Unbox { value } => scanner.scan(value),
    }
}

pub fn default_scan_variable<S>(scanner: &mut S, v: &Variable<S::EC>) -> bool
where
    S: ExprScanner + ?Sized,
{
    match v {
        Variable::Local(variable) => scanner.scan(&variable.var_type),
        Variable::Parameter(variable) => scanner.scan(&variable.var_type),
    }
}

#[cfg(test)]
mod tests {
    use super::ExprScanner;
    use crate::{Builtin, Expr, ExprContext};

    #[derive(Debug, Eq, Hash, PartialEq)]
    struct TestContext;

    impl ExprContext for TestContext {
        type Hole = u8;
        type Function = ();
        type Record = ();
        type Enum = ();
        type Trait = ();
        type EnumVariant = ();
        type Method = ();
        type Instance = ();
    }

    struct StopAtHole {
        stop_at: u8,
        visited: Vec<u8>,
    }

    impl ExprScanner for StopAtHole {
        type EC = TestContext;

        fn scan_hole(&mut self, hole: &u8) -> bool {
            self.visited.push(*hole);
            *hole != self.stop_at
        }
    }

    #[test]
    fn scanner_short_circuits_when_scan_returns_false() {
        let expr = Expr::Builtin {
            builtin: Builtin::BoolEq,
            arguments: vec![Expr::Hole(1), Expr::Hole(2), Expr::Hole(3)],
        };
        let mut scanner = StopAtHole {
            stop_at: 2,
            visited: Vec::new(),
        };

        assert!(!scanner.scan(&expr));
        assert_eq!(scanner.visited, vec![1, 2]);
    }
}
