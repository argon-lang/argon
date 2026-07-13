use crate::{Builtin, Expr, ExprContext};

pub fn is_shared_type<EC: ExprContext + ?Sized>(t: &Expr<EC>) -> bool {
    match t {
        Expr::Shared { .. } | Expr::Share { .. } => true,
        Expr::Builtin(Builtin::IntType { .. }) => true,
        Expr::Builtin(Builtin::BoolType) => true,
        Expr::Builtin(Builtin::StringType) => true,
        Expr::Builtin(Builtin::NeverType) => true,
        _ => false,
    }
}
