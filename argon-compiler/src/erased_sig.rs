use crate::{
    Context, DefaultExprContext, DefaultExprNormalizer, FunctionSignature, ModulePath, TubeName,
};
use alloc::{boxed::Box, vec::Vec};
use argon_expr::{Builtin, ErasureMode, Expr, IntegerType, NormalizerScanner};
use argon_parser::ast::Identifier;
use argon_util::{UniqueIdentifier, Unload};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ErasedSignature {
    pub parameters: Vec<ErasedSignatureType>,
    pub result: ErasedSignatureType,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ErasedSignatureType {
    Int,
    U8,
    Bool,
    String,
    Never,
    Array(Box<ErasedSignatureType>),
    Function(Box<ErasedSignatureType>, Box<ErasedSignatureType>),
    Declared(ImportSpecifier, Vec<ErasedSignatureType>),
    Tuple(Vec<ErasedSignatureType>),
    Erased,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ImportSpecifier {
    Global {
        tube: TubeName,
        module: ModulePath,
        name: Identifier,
        signature: Box<ErasedSignature>,
    },
    Local {
        parent: Box<ImportSpecifier>,
        id: UniqueIdentifier,
    },
}

impl Unload for ImportSpecifier {
    fn unload(&self) {}
}

pub fn erase_signature(
    context: Context,
    sig: &FunctionSignature<DefaultExprContext>,
) -> ErasedSignature {
    ErasedSignature {
        parameters: sig
            .parameters
            .iter()
            .map(|param| match param.erasure_mode {
                ErasureMode::Erased => ErasedSignatureType::Erased,
                ErasureMode::Token | ErasureMode::Concrete => {
                    erase_type(&context, param.param_type.clone())
                }
            })
            .collect(),
        result: erase_type(&context, sig.return_type.clone()),
    }
}

pub fn erase_type(context: &Context, mut t: Expr<DefaultExprContext>) -> ErasedSignatureType {
    let mut normalizer = NormalizerScanner::new(context.normalize_fuel(), DefaultExprNormalizer);
    normalizer.normalize(&mut t);

    match t {
        Expr::Builtin(builtin) => erase_builtin(context, builtin),

        Expr::FunctionType { a, r } => ErasedSignatureType::Function(
            Box::new(erase_type(context, a.var_type)),
            Box::new(erase_type(context, *r)),
        ),

        Expr::EnumType(enum_type) => ErasedSignatureType::Declared(
            enum_type.enum_.clone().import_specifier(),
            enum_type
                .arguments
                .into_iter()
                .map(|arg| erase_type(context, arg))
                .collect::<Vec<_>>(),
        ),

        Expr::RecordType(record_type) => ErasedSignatureType::Declared(
            record_type.record.clone().import_specifier(),
            record_type
                .arguments
                .into_iter()
                .map(|arg| erase_type(context, arg))
                .collect::<Vec<_>>(),
        ),

        Expr::TraitType(trait_type) => ErasedSignatureType::Declared(
            trait_type.trait_.clone().import_specifier(),
            trait_type
                .arguments
                .into_iter()
                .map(|arg| erase_type(context, arg))
                .collect::<Vec<_>>(),
        ),

        Expr::Tuple { items } => ErasedSignatureType::Tuple(
            items
                .into_iter()
                .map(|item| erase_type(context, item))
                .collect::<Vec<ErasedSignatureType>>(),
        ),

        _ => ErasedSignatureType::Erased,
    }
}

fn erase_builtin(context: &Context, builtin: Builtin<DefaultExprContext>) -> ErasedSignatureType {
    match builtin {
        Builtin::IntType { integer_type } => match integer_type {
            IntegerType::Int => ErasedSignatureType::Int,
            IntegerType::U8 => ErasedSignatureType::U8,
        },
        Builtin::BoolType => ErasedSignatureType::Bool,
        Builtin::StringType => ErasedSignatureType::String,
        Builtin::NeverType => ErasedSignatureType::Never,
        Builtin::ArrayType { element_type } => {
            ErasedSignatureType::Array(Box::new(erase_type(context, *element_type)))
        }
        _ => ErasedSignatureType::Erased,
    }
}
