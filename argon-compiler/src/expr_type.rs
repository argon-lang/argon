use crate::shifter::{DefaultExprAssociatedTypes, DefaultToExprTypeContextShifter};
use crate::{DefaultExprContext, EmptyHole, FunctionSignature};
use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::sync::Arc;
use argon_expr::{
    Builtin, Expr, ExprContextShifter, ExpressionOwner, MethodInstanceType, Pattern, SubstScanner,
    Variable,
};

pub trait ExprTypeContext: DefaultExprAssociatedTypes {
    fn get_hole_type(hole: &Self::Hole) -> Expr<Self>;
}

impl ExprTypeContext for DefaultExprContext {
    fn get_hole_type(hole: &EmptyHole) -> Expr<Self> {
        match *hole {}
    }
}

pub fn get_expr_type<EC: ExprTypeContext + ?Sized>(expr: &Expr<EC>) -> Expr<EC> {
    match expr {
        Expr::Error => Expr::Error,
        Expr::Hole(hole) => EC::get_hole_type(hole),

        Expr::And(_, _)
        | Expr::BoolLiteral(_)
        | Expr::Condition { .. }
        | Expr::Is { .. }
        | Expr::Not(_)
        | Expr::Or(_, _) => Expr::bool_type(),

        Expr::Closure { v, return_type, .. } => Expr::FunctionType {
            a: v.clone(),
            r: return_type.clone(),
        },

        Expr::Block { label, .. } => label.block_result_type.clone(),

        Expr::VariableBinding(_, _) | Expr::VariableStore(_, _) => Expr::unit(),

        Expr::Break { .. } | Expr::Raise { .. } | Expr::Retry { .. } => Expr::never_type(),

        Expr::BigType(value) => Expr::BigType(value + 1),

        Expr::Box { t, .. } => Expr::BoxedType(t.clone()),

        Expr::BoxedType(t) => get_expr_type(&*t),

        Expr::Builtin(builtin) => get_builtin_type(builtin),

        Expr::Finally { block_body, .. } => get_expr_type(&**block_body),

        Expr::FunctionCall {
            function,
            arguments,
        } => {
            let owner = ExpressionOwner::Function(function.clone());
            return_type_for_args(
                owner,
                shift_signature(function.clone().signature()),
                arguments.as_ref(),
            )
        }

        Expr::FunctionObjectCall { function, .. } => match get_expr_type(&**function) {
            Expr::FunctionType { r, .. } => *r,
            _ => Expr::Error,
        },

        Expr::FunctionType { .. } => Expr::type_n(0),

        Expr::IfElse { when_true, .. } => get_expr_type(&**when_true),

        Expr::IntLiteral(_) => Expr::int_type(),

        Expr::Match { .. } => todo!(),

        Expr::NewInstance {
            instance,
            arguments,
        } => {
            let owner = ExpressionOwner::Instance(instance.clone());
            return_type_for_args(
                owner,
                shift_signature(instance.clone().signature()),
                arguments.as_ref(),
            )
        }

        Expr::MethodCall {
            method,
            instance_type,
            arguments,
            ..
        } => method_call_return_type(method.clone(), instance_type, arguments.as_ref()),

        Expr::Sequence(items) => get_expr_type(items.last()),

        Expr::StringLiteral(_) => Expr::string_type(),

        Expr::Tuple { items } => Expr::Tuple {
            items: items.into_iter().map(|item| get_expr_type(item)).collect(),
        },

        Expr::TupleElement(tuple, index) => match get_expr_type(&**tuple) {
            Expr::Tuple { mut items } if *index < items.len() => items.swap_remove(*index),
            _ => Expr::Error,
        },

        Expr::Type(level) => match &**level {
            Expr::IntLiteral(level) => Expr::type_n(level + 1),
            level => Expr::Type(Box::new(Expr::Builtin(Builtin::IntAdd {
                lhs: Box::new(level.clone()),
                rhs: Box::new(Expr::IntLiteral(1.into())),
            }))),
        },

        Expr::Unbox { t, .. } => (**t).clone(),

        Expr::Variable(variable) => variable.var_type().clone(),

        Expr::RecordFieldLoad { field, .. } => {
            shift_default_expr((*field.clone().field_type()).clone())
        }

        Expr::RecordFieldStore { .. } => Expr::unit(),

        Expr::RecordType(record_type) => {
            let owner = ExpressionOwner::Record(record_type.record.clone());
            return_type_for_args(
                owner,
                shift_signature(record_type.record.clone().signature()),
                &record_type.arguments,
            )
        }

        Expr::EnumType(enum_type) => {
            let owner = ExpressionOwner::Enum(enum_type.enum_.clone());
            return_type_for_args(
                owner,
                shift_signature(enum_type.enum_.clone().signature()),
                &enum_type.arguments,
            )
        }

        Expr::EnumVariantLiteral { enum_type, .. } => Expr::EnumType(enum_type.clone()),

        Expr::TraitType(trait_type) => {
            let owner = ExpressionOwner::Trait(trait_type.trait_.clone());
            return_type_for_args(
                owner,
                shift_signature(trait_type.trait_.clone().signature()),
                &trait_type.arguments,
            )
        }

        Expr::InstanceType(instance_type) => {
            let owner = ExpressionOwner::Instance(instance_type.instance.clone());
            let trait_type = return_type_for_args(
                owner,
                shift_signature(instance_type.instance.clone().signature()),
                &instance_type.arguments,
            );
            get_expr_type(&trait_type)
        }

        Expr::FunctionResultValue | Expr::RecordLiteral { .. } => todo!(),
    }
}

fn method_call_return_type<EC: ExprTypeContext + ?Sized>(
    method: EC::Method,
    instance_type: &MethodInstanceType<EC>,
    arguments: &[Expr<EC>],
) -> Expr<EC> {
    let mut sig = shift_signature(method.clone().signature());
    sig.substitute_method_instance_type_parameters(instance_type);
    return_type_for_args(ExpressionOwner::Method(method), sig, arguments)
}

fn shift_signature<EC: ExprTypeContext + ?Sized>(
    signature: Arc<FunctionSignature<DefaultExprContext>>,
) -> FunctionSignature<EC> {
    signature
        .as_ref()
        .clone()
        .shift(&mut DefaultToExprTypeContextShifter::<EC>::default())
}

fn shift_default_expr<EC: ExprTypeContext + ?Sized>(expr: Expr<DefaultExprContext>) -> Expr<EC> {
    DefaultToExprTypeContextShifter::<EC>::default().shift(expr)
}

pub fn get_pattern_type(pattern: &Pattern<DefaultExprContext>) -> Expr<DefaultExprContext> {
    match pattern {
        Pattern::Error => Expr::Error,
        Pattern::Discard { t } => (**t).clone(),
        Pattern::Tuple(items) => Expr::Tuple {
            items: items.iter().map(get_pattern_type).collect(),
        },
        Pattern::Binding(variable, _) => variable.var_type.clone(),
        Pattern::EnumVariant { enum_type, .. } => Expr::EnumType(enum_type.clone()),
        Pattern::String(_) => Expr::string_type(),
        Pattern::Int(_) => Expr::int_type(),
        Pattern::Bool(_) => Expr::bool_type(),
    }
}

fn get_builtin_type<EC: ExprTypeContext + ?Sized>(builtin: &Builtin<EC>) -> Expr<EC> {
    match builtin {
        Builtin::IntType | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => {
            Expr::type_n(0)
        }

        Builtin::ArrayType { element_type } => get_expr_type(element_type),

        Builtin::IntNegate { .. }
        | Builtin::IntBitNot { .. }
        | Builtin::IntAdd { .. }
        | Builtin::IntSub { .. }
        | Builtin::IntMul { .. }
        | Builtin::IntBitAnd { .. }
        | Builtin::IntBitOr { .. }
        | Builtin::IntBitXor { .. }
        | Builtin::IntBitShiftLeft { .. }
        | Builtin::IntBitShiftRight { .. }
        | Builtin::ArrayLength { .. } => Expr::int_type(),

        Builtin::IntEq { .. }
        | Builtin::IntNe { .. }
        | Builtin::IntLt { .. }
        | Builtin::IntLe { .. }
        | Builtin::IntGt { .. }
        | Builtin::IntGe { .. }
        | Builtin::StringEq { .. }
        | Builtin::StringNe { .. }
        | Builtin::BoolEq { .. }
        | Builtin::BoolNe { .. } => Expr::bool_type(),

        Builtin::StringConcat { .. } => Expr::string_type(),

        Builtin::ArrayCreateUnsafeUninitialized { element_type, .. } => {
            Expr::array_type((**element_type).clone())
        }

        Builtin::ArrayGet { element_type, .. } => (**element_type).clone(),

        Builtin::ArraySet { .. } => Expr::unit(),

        Builtin::ConjunctionType { .. }
        | Builtin::DisjunctionType { .. }
        | Builtin::EqualToType { .. } => Expr::Error,
    }
}

fn return_type_for_args<EC: ExprTypeContext + ?Sized>(
    owner: ExpressionOwner<EC>,
    signature: FunctionSignature<EC>,
    arguments: &[Expr<EC>],
) -> Expr<EC> {
    let mut return_type = signature.return_type.clone();

    for (index, (parameter, argument)) in signature.parameters.iter().zip(arguments).enumerate() {
        let variable = Variable::Parameter(Box::new(
            parameter.clone().to_parameter_var(owner.clone(), index),
        ));
        SubstScanner::subst(variable, Cow::Borrowed(&argument), &mut return_type);
    }

    return_type
}
