use crate::{Expr, ExprContext, ExprContextShifter, Variable};
use std::collections::HashMap;

pub struct FreshVariableShifter<EC: ExprContext + ?Sized> {
    var_mapping: HashMap<Variable<EC>, Variable<EC>>,
}

impl<EC: ExprContext + ?Sized> FreshVariableShifter<EC> {
    pub fn new() -> Self {
        Self {
            var_mapping: HashMap::new(),
        }
    }
}

impl<EC: ExprContext + ?Sized> ExprContextShifter for FreshVariableShifter<EC> {
    type EC1 = EC;
    type EC2 = EC;

    fn shift_hole(&mut self, hole: <Self::EC1 as ExprContext>::Hole) -> Expr<Self::EC2> {
        Expr::Hole(hole)
    }

    fn shift_variable(&mut self, v: Variable<Self::EC1>) -> Variable<Self::EC2> {
        if let Some(v2) = self.var_mapping.get(&v) {
            return v2.clone();
        }

        super::default_shift_variable(self, v)
    }
}
