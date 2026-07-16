use crate::{Builtin, Expr, ExprContext, LocatedExpr};

pub fn is_shared_type<EC: ExprContext + ?Sized>(t: &LocatedExpr<EC>) -> bool {
    match &t.value {
        Expr::Shared { .. } | Expr::Share { .. } => true,
        Expr::Builtin(Builtin::IntType { .. }) => true,
        Expr::Builtin(Builtin::BoolType) => true,
        Expr::Builtin(Builtin::StringType) => true,
        Expr::Builtin(Builtin::NeverType) => true,
        _ => false,
    }
}
