use crate::Function;
use argon_expr::{ExprContext, Variable};
use std::sync::Arc;

pub enum ImplicitValue<EC: ExprContext + ?Sized> {
    OfVar(Variable<EC>),
    OfFunction(Arc<dyn Function>),
}
