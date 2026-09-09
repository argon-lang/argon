use crate::Function;
use alloc::sync::Arc;
use argon_expr::{ExprContext, Variable};

pub enum ImplicitValue<EC: ExprContext + ?Sized> {
    OfVar(Variable<EC>),
    OfFunction(Arc<dyn Function>),
}
