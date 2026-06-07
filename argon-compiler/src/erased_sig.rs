use crate::{
    Context, DefaultExprContext, DefaultExprNormalizer, FunctionSignature, ModulePath, TubeName,
};
use alloc::{boxed::Box, vec::Vec};
use argon_expr::{Builtin, ErasureMode, Expr, ExprScannerMut, NormalizerScanner};
use argon_parser::ast::Identifier;
use argon_util::UniqueIdentifier;

#[derive(Clone, PartialEq, Eq)]
pub struct ErasedSignature {
    pub parameters: Vec<ErasedSignatureType>,
    pub result: ErasedSignatureType,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ErasedSignatureType {
    Builtin(Builtin),
    Function(Box<ErasedSignatureType>, Box<ErasedSignatureType>),
    Declared(ImportSpecifier, Vec<ErasedSignatureType>),
    Tuple(Vec<ErasedSignatureType>),
    Erased,
}

#[derive(Clone, PartialEq, Eq)]
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

fn erase_type(context: &Context, mut t: Expr<DefaultExprContext>) -> ErasedSignatureType {
    let mut normalizer = NormalizerScanner::new(context.normalize_fuel(), DefaultExprNormalizer);
    normalizer.normalize(&mut t);

    match t {
        Expr::Builtin { builtin, arguments } => erase_builtin(builtin, arguments),

        Expr::FunctionType { a, r } => ErasedSignatureType::Function(
            Box::new(erase_type(context, *a)),
            Box::new(erase_type(context, *r)),
        ),

        Expr::EnumType(decl, arguments) => ErasedSignatureType::Declared(
            decl.clone().import_specifier(),
            arguments
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

        Expr::TraitType(decl, arguments) => ErasedSignatureType::Declared(
            decl.clone().import_specifier(),
            arguments
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

fn erase_builtin(
    builtin: Builtin,
    arguments: Vec<Expr<DefaultExprContext>>,
) -> ErasedSignatureType {
    if arguments.is_empty() {
        return ErasedSignatureType::Builtin(builtin);
    }

    match builtin {
        Builtin::ArrayType => todo!(),

        _ => ErasedSignatureType::Erased,
    }
}
