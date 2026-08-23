use crate::{Builtin, Expr, ExprContext, LocatedExpr};

pub fn is_shared_type<EC: ExprContext + ?Sized>(t: &LocatedExpr<EC>) -> bool {
    match &t.value {
        Expr::Shared { .. } => true,
        Expr::Builtin(Builtin::IntType { .. }) => true,
        Expr::Builtin(Builtin::BoolType) => true,
        Expr::Builtin(Builtin::StringType) => true,
        Expr::Builtin(Builtin::NeverType) => true,
        Expr::Tuple { items } => items.iter().all(is_shared_type),
        Expr::Type(_) | Expr::BigType(_) => true,
        _ => false,
    }
}

pub fn is_use_type<EC: ExprContext + ?Sized>(t: &LocatedExpr<EC>) -> bool {
    match &t.value {
        Expr::Use { is_mutable: false, .. } => true,
        _ => false,
    }
}

pub fn is_use_mutable_type<EC: ExprContext + ?Sized>(t: &LocatedExpr<EC>) -> bool {
    match &t.value {
        Expr::Use { is_mutable: true, .. } => true,
        _ => false,
    }
}
