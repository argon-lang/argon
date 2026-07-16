use crate::shifter::{DefaultExprAssociatedTypes, DefaultToExprTypeContextShifter};
use crate::{DefaultExprContext, EmptyHole, FunctionSignature, SubstFunctionSignature};
use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::sync::Arc;
use argon_expr::{
    Builtin, Expr, ExprContextShifter, ExprLocationExt, ExprScannerMut, ExpressionOwner,
    LocatedExpr, LocatedPattern, MethodInstanceType, Pattern, SubstScanner, Variable,
};
use parse18_runtime::Location;

pub trait ExprTypeContext: DefaultExprAssociatedTypes {
    fn get_hole_type(hole: &Self::Hole) -> LocatedExpr<Self>;
}

impl ExprTypeContext for DefaultExprContext {
    fn get_hole_type(hole: &EmptyHole) -> LocatedExpr<Self> {
        match *hole {}
    }
}

pub fn get_expr_type<EC>(expr: &LocatedExpr<EC>) -> LocatedExpr<EC>
where
    EC: ExprTypeContext + ?Sized,
{
    match &expr.value {
        Expr::Error => LocatedExpr::error(expr.location.clone()),
        Expr::Hole(hole) => EC::get_hole_type(hole),

        Expr::And(_, _)
        | Expr::BoolLiteral(_)
        | Expr::Condition { .. }
        | Expr::Is { .. }
        | Expr::Not(_)
        | Expr::Or(_, _) => Expr::bool_type().with_location(expr.location.clone()),

        Expr::ConjunctionType { .. } | Expr::DisjunctionType { .. } | Expr::EqualToType { .. } => {
            Expr::type_n(0).with_location(expr.location.clone())
        }

        Expr::Closure { v, return_type, .. } => Expr::FunctionType {
            a: v.clone(),
            r: return_type.clone(),
        }
        .with_location(expr.location.clone()),

        Expr::Block { label, .. } => label.block_result_type.clone(),

        Expr::VariableBinding(_, _) | Expr::VariableStore(_, _) => {
            Expr::unit().with_location(expr.location.clone())
        }

        Expr::Break { .. } | Expr::Raise { .. } | Expr::Retry { .. } => {
            Expr::never_type().with_location(expr.location.clone())
        }

        Expr::BigType(value) => Expr::BigType(value + 1).with_location(expr.location.clone()),

        Expr::Box { t, .. } => Expr::BoxedType(t.clone()).with_location(expr.location.clone()),

        Expr::BoxedType(t) => get_expr_type(t),

        Expr::BindEnsures { value, .. } | Expr::BindErasedAlias { value, .. } => {
            get_expr_type(value)
        }

        Expr::Use { inner: value, .. }
        | Expr::Shared { inner: value }
        | Expr::Share { value }
        | Expr::Borrow { value }
        | Expr::BorrowMut { value } => get_expr_type(value),

        Expr::Builtin(builtin) => get_builtin_type(builtin, &expr.location),

        Expr::Finally { block_body, .. } => get_expr_type(block_body),

        Expr::FunctionCall {
            function,
            arguments,
        } => {
            let owner = ExpressionOwner::Function(function.clone());
            return_type_for_args(
                owner,
                shift_signature(function.clone().signature()),
                arguments,
            )
        }

        Expr::FunctionObjectCall { function, .. } => match get_expr_type(function).value {
            Expr::FunctionType { r, .. } => *r,
            _ => LocatedExpr::error(expr.location.clone()),
        },

        Expr::FunctionType { .. } => Expr::type_n(0).with_location(expr.location.clone()),

        Expr::IfElse { when_true, .. } => get_expr_type(when_true),

        Expr::IntLiteral(_) => Expr::int_type().with_location(expr.location.clone()),
        Expr::I8Literal(_) => Expr::i8_type().with_location(expr.location.clone()),
        Expr::U8Literal(_) => Expr::u8_type().with_location(expr.location.clone()),
        Expr::I16Literal(_) => Expr::i16_type().with_location(expr.location.clone()),
        Expr::U16Literal(_) => Expr::u16_type().with_location(expr.location.clone()),
        Expr::I32Literal(_) => Expr::i32_type().with_location(expr.location.clone()),
        Expr::U32Literal(_) => Expr::u32_type().with_location(expr.location.clone()),
        Expr::I64Literal(_) => Expr::i64_type().with_location(expr.location.clone()),
        Expr::U64Literal(_) => Expr::u64_type().with_location(expr.location.clone()),

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
        } => method_call_return_type(method.clone(), instance_type, arguments),

        Expr::Sequence(items) => get_expr_type(items.last()),

        Expr::StringLiteral(_) => Expr::string_type().with_location(expr.location.clone()),

        Expr::Tuple { items } => Expr::Tuple {
            items: items.iter().map(get_expr_type).collect(),
        }
        .with_location(expr.location.clone()),

        Expr::TupleElement(tuple, index) => match get_expr_type(tuple).value {
            Expr::Tuple { mut items } if *index < items.len() => items.swap_remove(*index),
            _ => LocatedExpr::error(expr.location.clone()),
        },

        Expr::Type(level) => match &level.value {
            Expr::IntLiteral(level_num) => {
                let level_expr =
                    Expr::IntLiteral(level_num + 1).with_location(level.location.clone());
                Expr::Type(Box::new(level_expr)).with_location(expr.location.clone())
            }
            level_value => Expr::Type(Box::new(
                Expr::Builtin(Builtin::IntAdd {
                    integer_type: argon_expr::IntegerType::Int,
                    lhs: Box::new(level_value.clone().with_location(level.location.clone())),
                    rhs: Box::new(Expr::IntLiteral(1.into()).with_location(level.location.clone())),
                })
                .with_location(level.location.clone()),
            ))
            .with_location(expr.location.clone()),
        },

        Expr::Unbox { t, .. } => t.value.clone().with_location(expr.location.clone()),

        Expr::Variable(variable) => variable.var_type().clone(),

        Expr::RecordFieldLoad {
            record_type, field, ..
        } => {
            let mut subst = SubstScanner::new();
            subst.add_function_parameter_substitutions(
                ExpressionOwner::Record(record_type.record.clone()),
                &shift_signature(record_type.record.clone().signature()),
                &record_type.arguments,
            );
            let mut field_type = DefaultToExprTypeContextShifter::<EC>::default()
                .shift((*field.clone().field_type()).clone());
            subst.scan(&mut field_type);
            field_type
        }

        Expr::RecordFieldStore { .. } => Expr::unit().with_location(expr.location.clone()),

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

        Expr::EnumVariantLiteral { enum_type, .. } => {
            Expr::EnumType(enum_type.clone()).with_location(expr.location.clone())
        }

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

        Expr::RecordLiteral { record_type, .. } => {
            Expr::RecordType(record_type.clone()).with_location(expr.location.clone())
        }

        Expr::FunctionResultValue { result_type } => result_type
            .value
            .clone()
            .with_location(expr.location.clone()),
    }
}

fn method_call_return_type<EC: ExprTypeContext + ?Sized>(
    method: EC::Method,
    instance_type: &MethodInstanceType<EC>,
    arguments: &[LocatedExpr<EC>],
) -> LocatedExpr<EC> {
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

pub fn get_pattern_type(
    pattern: &LocatedPattern<DefaultExprContext>,
) -> LocatedExpr<DefaultExprContext> {
    match &pattern.value {
        Pattern::Error => LocatedExpr::error(pattern.location.clone()),
        Pattern::Discard { t } => (**t).clone(),
        Pattern::Tuple(items) => Expr::Tuple {
            items: items.iter().map(|item| get_pattern_type(item)).collect(),
        }
        .with_location(pattern.location.clone()),
        Pattern::Binding(variable, _) => variable.var_type.clone(),
        Pattern::EnumVariant { enum_type, .. } => {
            Expr::EnumType(enum_type.clone()).with_location(pattern.location.clone())
        }
        Pattern::String(_) => Expr::string_type().with_location(pattern.location.clone()),
        Pattern::Int(_) => Expr::int_type().with_location(pattern.location.clone()),
        Pattern::Bool(_) => Expr::bool_type().with_location(pattern.location.clone()),
    }
}

fn get_builtin_type<EC: ExprTypeContext + ?Sized>(
    builtin: &Builtin<EC>,
    location: &Location,
) -> LocatedExpr<EC> {
    match builtin {
        Builtin::IntType { .. } | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => {
            Expr::type_n(0).with_location(location.clone())
        }

        Builtin::ArrayType { element_type } => get_expr_type(element_type),

        Builtin::IntNegate { integer_type, .. }
        | Builtin::IntBitNot { integer_type, .. }
        | Builtin::IntAdd { integer_type, .. }
        | Builtin::IntSub { integer_type, .. }
        | Builtin::IntMul { integer_type, .. }
        | Builtin::IntBitAnd { integer_type, .. }
        | Builtin::IntBitOr { integer_type, .. }
        | Builtin::IntBitXor { integer_type, .. }
        | Builtin::IntBitShiftLeft { integer_type, .. }
        | Builtin::IntBitShiftRight { integer_type, .. } => {
            Expr::integer_type(*integer_type).with_location(location.clone())
        }
        Builtin::IntConvert { dest_type, .. } => {
            Expr::integer_type(*dest_type).with_location(location.clone())
        }

        Builtin::ArrayLength { .. } => Expr::int_type().with_location(location.clone()),

        Builtin::IntEq { .. }
        | Builtin::IntLt { .. }
        | Builtin::IntLe { .. }
        | Builtin::IntGt { .. }
        | Builtin::IntGe { .. }
        | Builtin::StringEq { .. }
        | Builtin::BoolEq { .. } => Expr::bool_type().with_location(location.clone()),

        Builtin::StringConcat { .. } => Expr::string_type().with_location(location.clone()),

        Builtin::ArrayCreateUnsafeUninitialized { element_type, .. } => {
            Expr::array_type((**element_type).clone()).with_location(location.clone())
        }

        Builtin::ArrayGet { element_type, .. } => (**element_type).clone(),

        Builtin::ArraySet { .. } => Expr::unit().with_location(location.clone()),

        Builtin::EqualToRefl { r#type, value } => Expr::EqualToType {
            r#type: r#type.clone(),
            lhs: value.clone(),
            rhs: value.clone(),
        }
        .with_location(location.clone()),

        Builtin::UnsafeAssumeErased { r#type } => (**r#type).clone(),
    }
}

fn return_type_for_args<EC: ExprTypeContext + ?Sized>(
    owner: ExpressionOwner<EC>,
    signature: FunctionSignature<EC>,
    arguments: &[LocatedExpr<EC>],
) -> LocatedExpr<EC> {
    let mut return_type = signature.return_type.clone();

    for (index, (parameter, argument)) in signature.parameters.iter().zip(arguments).enumerate() {
        let variable = Variable::Parameter(Box::new(
            parameter.clone().to_parameter_var(owner.clone(), index),
        ));
        SubstScanner::subst(variable, Cow::Borrowed(argument), &mut return_type);
    }

    return_type
}
