use crate::{DefaultExprContext, FunctionSignature};
use alloc::boxed::Box;
use alloc::sync::Arc;
use argon_expr::{Builtin, Expr, ExpressionOwner, SubstScanner, Variable};


pub fn get_expr_type(expr: &Expr<DefaultExprContext>) -> Expr<DefaultExprContext> {
    match expr {
        Expr::Error => Expr::Error,
        Expr::Hole(hole) => match *hole {},

        Expr::And(_, _) | Expr::BoolLiteral(_) | Expr::Is { .. } | Expr::Or(_, _) => {
            Expr::bool_type()
        }

        Expr::Loop { .. }
        | Expr::VariableStore(_, _)
        | Expr::While { .. } => Expr::unit_type(),

        Expr::Break { .. } | Expr::Next { .. } | Expr::Raise { .. } | Expr::Redo { .. } => {
            Expr::never_type()
        }

        Expr::BigType(value) => Expr::BigType(value + 1),

        Expr::Box { t, .. } => Expr::BoxedType { t: t.clone() },

        Expr::BoxedType { t } => get_expr_type(&*t),

        Expr::Builtin { builtin, arguments } => get_builtin_type(*builtin, arguments),

        Expr::Finally { block_body, .. } => get_expr_type(&**block_body),

        Expr::FunctionCall {
            function,
            arguments,
        } => {
            let owner = ExpressionOwner::Function(function.clone());
            return_type_for_args(owner, function.clone().signature(), arguments.as_ref())
        }

        Expr::FunctionObjectCall { function, .. } => match get_expr_type(&**function) {
            Expr::FunctionType { r, .. } => *r,
            _ => Expr::Error,
        },

        Expr::FunctionType { .. } => Expr::type_n(0),

        Expr::IfElse { when_true, .. } => get_expr_type(&**when_true),

        Expr::IntLiteral(_) => Expr::int_type(),

        Expr::Match { .. } => todo!(),

        Expr::Sequence(items) => {
            get_expr_type(items.last())
        }

        Expr::StringLiteral(_) => Expr::string_type(),

        Expr::Tuple { items } => Expr::Tuple {
            items: items
                .into_iter()
                .map(|item| get_expr_type(item))
                .collect(),
        },

        Expr::TupleElement(tuple, index) => match get_expr_type(&**tuple) {
            Expr::Tuple { mut items } if *index < items.len() => items.swap_remove(*index),
            _ => Expr::Error,
        },

        Expr::Type(level) => match &**level {
            Expr::IntLiteral(level) => Expr::type_n(level + 1),
            level => Expr::Type(Box::new(Expr::Builtin {
                builtin: Builtin::IntAdd,
                arguments: alloc::vec![level.clone(), Expr::IntLiteral(1.into())],
            })),
        },

        Expr::Unbox { t, .. } => (**t).clone(),

        Expr::Variable(variable) | Expr::StoreVariable(variable) => variable.var_type().clone(),

        Expr::RecordType(record, arguments) => {
            let owner = ExpressionOwner::Record(record.clone());
            return_type_for_args(owner, record.clone().signature(), arguments)
        }

        Expr::EnumType(enum_, arguments) => {
            let owner = ExpressionOwner::Enum(enum_.clone());
            return_type_for_args(owner, enum_.clone().signature(), arguments)
        }

        Expr::TraitType(trait_, arguments) => {
            let owner = ExpressionOwner::Trait(trait_.clone());
            return_type_for_args(owner, trait_.clone().signature(), arguments)
        }

        Expr::FunctionLiteral { .. }
        | Expr::FunctionResultValue
        | Expr::MethodCall { .. }
        | Expr::NewTraitObject { .. }
        | Expr::RecordLiteral { .. } => todo!(),
    }
}

fn get_builtin_type(
    builtin: Builtin,
    arguments: &[Expr<DefaultExprContext>],
) -> Expr<DefaultExprContext> {
    match builtin {
        Builtin::IntType | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => {
            Expr::type_n(0)
        }

        Builtin::ArrayType => arguments
            .into_iter()
            .next()
            .map(|element_type| get_expr_type(element_type))
            .unwrap_or(Expr::Error),

        Builtin::IntNegate
        | Builtin::IntBitNot
        | Builtin::IntAdd
        | Builtin::IntSub
        | Builtin::IntMul
        | Builtin::IntBitAnd
        | Builtin::IntBitOr
        | Builtin::IntBitXor
        | Builtin::IntBitShiftLeft
        | Builtin::IntBitShiftRight
        | Builtin::ArrayLength => Expr::int_type(),

        Builtin::IntEq
        | Builtin::IntNe
        | Builtin::IntLt
        | Builtin::IntLe
        | Builtin::IntGt
        | Builtin::IntGe
        | Builtin::StringEq
        | Builtin::StringNe
        | Builtin::BoolNot
        | Builtin::BoolEq
        | Builtin::BoolNe => Expr::bool_type(),

        Builtin::StringConcat => Expr::string_type(),

        Builtin::ArrayCreateUnsafeUninitialized => arguments
            .into_iter()
            .next()
            .cloned()
            .map(Expr::array_type)
            .unwrap_or(Expr::Error),

        Builtin::ArrayGet => arguments.into_iter().next().cloned().unwrap_or(Expr::Error),

        Builtin::ArraySet => Expr::unit_type(),

        Builtin::ConjunctionType | Builtin::DisjunctionType | Builtin::EqualToType => {
            Expr::Error
        }
    }
}

fn return_type_for_args(
    owner: ExpressionOwner<DefaultExprContext>,
    signature: Arc<FunctionSignature<DefaultExprContext>>,
    arguments: &[Expr<DefaultExprContext>],
) -> Expr<DefaultExprContext> {
    let mut return_type = signature.return_type.clone();

    for (index, (parameter, argument)) in signature.parameters.iter().zip(arguments).enumerate() {
        let variable = Variable::Parameter(Arc::new(
            parameter.clone().to_parameter_var(owner.clone(), index),
        ));
        SubstScanner::subst(variable, &argument, &mut return_type);
    }

    return_type
}
