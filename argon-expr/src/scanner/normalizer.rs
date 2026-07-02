use alloc::borrow::Cow;
use crate::{Expr, ExprContext, ExprScannerMut, default_scan_mut, SubstScanner, Variable};
use alloc::vec::Vec;
use argon_util::Fuel;
use std::mem;

pub struct NormalizerScanner<S> {
    fuel: Fuel,
    state: S,
}

impl<S> NormalizerScanner<S> {
    pub fn new(fuel: Fuel, state: S) -> Self {
        Self { fuel, state }
    }
}

pub struct FullNormalizer<S> {
    normalizer: NormalizerScanner<S>,
}

impl<S> FullNormalizer<S> {
    pub fn new(fuel: Fuel, state: S) -> Self {
        Self {
            normalizer: NormalizerScanner::new(fuel, state),
        }
    }
}

pub trait Normalizer {
    type EC: ExprContext;

    fn resolve_hole(&mut self, hole: &<Self::EC as ExprContext>::Hole) -> Option<Expr<Self::EC>>;

    fn get_function_body(
        &mut self,
        function: &<Self::EC as ExprContext>::Function,
        arguments: &mut Vec<Expr<Self::EC>>,
    ) -> Option<Expr<Self::EC>>;
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> NormalizerScanner<S> {
    pub fn normalize(&mut self, expr: &mut Expr<S::EC>) {
        let e2 = mem::replace(expr, Expr::Error);
        *expr = self.normalize_impl(e2);
    }

    pub fn normalize_impl(&mut self, expr: Expr<S::EC>) -> Expr<S::EC> {
        let mut expr = expr;

        'updated_no_growth: while !self.fuel.is_empty() {
            'updated: {
                match expr {
                    Expr::Hole(ref hole) => {
                        if let Some(resolved) = self.state.resolve_hole(hole) {
                            expr = resolved;
                            break 'updated;
                        }
                    }

                    Expr::FunctionCall {
                        ref function,
                        ref mut arguments,
                    } => {
                        if let Some(body) = self.state.get_function_body(function, arguments) {
                            expr = body;
                            break 'updated;
                        }
                    }

                    Expr::Condition { value, .. } => {
                        expr = *value;
                        continue 'updated_no_growth;
                    }

                    Expr::Not(value) => {
                        let value = self.normalize_impl(*value);
                        match value {
                            Expr::BoolLiteral(b) => {
                                expr = Expr::BoolLiteral(!b);
                                continue 'updated_no_growth;
                            }
                            Expr::Not(v) => {
                                expr = *v;
                                continue 'updated_no_growth;
                            }
                            Expr::And(mut a, mut b) => {
                                a = Box::new(Expr::Not(a));
                                b = Box::new(Expr::Not(b));
                                expr = Expr::Or(a, b);
                                continue 'updated_no_growth;
                            }
                            Expr::Or(mut a, mut b) => {
                                a = Box::new(Expr::Not(a));
                                b = Box::new(Expr::Not(b));
                                expr = Expr::And(a, b);
                                continue 'updated_no_growth;
                            }
                            _ => {
                                expr = Expr::Not(Box::new(value));
                            }
                        }
                    }

                    Expr::FunctionObjectCall {
                        function,
                        argument,
                    } => {
                        match *function {
                            Expr::Closure { v, return_type, mut body } => {
                                let mut subst = SubstScanner::new();
                                subst.add_substitution(Variable::ClosureParameter(v), Cow::Owned(*argument));
                                subst.scan(&mut *body);
                                expr = *body;
                                continue 'updated_no_growth;
                            }

                            _ => {
                                expr = Expr::FunctionObjectCall {
                                    function,
                                    argument,
                                };
                            }
                        }
                    }

                    _ => {}
                }

                break 'updated_no_growth;
            }

            self.fuel.consume();
        }

        expr
    }
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> FullNormalizer<S> {
    pub fn normalize(&mut self, expr: &mut Expr<S::EC>) {
        self.scan(expr);
    }
}

impl<EC: ExprContext + ?Sized, S: Normalizer<EC = EC>> ExprScannerMut for FullNormalizer<S> {
    type EC = EC;

    fn scan(&mut self, expr: &mut Expr<Self::EC>) -> bool {
        if !default_scan_mut(self, expr) {
            return false;
        }

        let fuel = self.normalizer.fuel.clone();
        self.normalizer.normalize(expr);
        self.normalizer.fuel = fuel;
        true
    }
}

#[cfg(test)]
mod tests {
    use super::{FullNormalizer, Normalizer};
    use crate::{Expr, ExprContext, ExprScannerMut};
    use alloc::vec;
    use alloc::vec::Vec;
    use argon_util::Fuel;

    #[derive(Debug, Eq, Hash, PartialEq)]
    struct TestContext;

    impl ExprContext for TestContext {
        type Hole = u8;
        type Function = u8;
        type Record = ();
        type RecordField = ();
        type Enum = ();
        type Trait = ();
        type EnumVariant = ();
        type Method = ();
        type Instance = ();
    }

    struct TestNormalizer;

    impl Normalizer for TestNormalizer {
        type EC = TestContext;

        fn resolve_hole(&mut self, hole: &u8) -> Option<Expr<Self::EC>> {
            (*hole == 1).then_some(Expr::BoolLiteral(false))
        }

        fn get_function_body(
            &mut self,
            function: &u8,
            arguments: &mut Vec<Expr<Self::EC>>,
        ) -> Option<Expr<Self::EC>> {
            if *function != 1 {
                return None;
            }

            match arguments.as_slice() {
                [Expr::BoolLiteral(false)] => Some(Expr::BoolLiteral(true)),
                [_] => Some(Expr::Error),
                [] => None,
                _ => None,
            }
        }
    }

    #[test]
    fn full_normalizer_normalizes_nested_expressions() {
        let mut expr = Expr::Tuple {
            items: vec![Expr::Hole(1)],
        };
        let mut normalizer = FullNormalizer::new(Fuel::new(10), TestNormalizer);

        normalizer.normalize(&mut expr);

        assert_eq!(
            expr,
            Expr::Tuple {
                items: vec![Expr::BoolLiteral(false)]
            }
        );
    }

    #[test]
    fn full_normalizer_updates_children_before_current_expression() {
        let mut expr = Expr::FunctionCall {
            function: 1,
            arguments: vec![Expr::Hole(1)],
        };
        let mut normalizer = FullNormalizer::new(Fuel::new(10), TestNormalizer);

        assert!(normalizer.scan(&mut expr));

        assert_eq!(expr, Expr::BoolLiteral(true));
    }
}
