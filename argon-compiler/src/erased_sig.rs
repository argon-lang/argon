use crate::{DefaultExprContext, FunctionSignature, ModulePath, TubeName};
use alloc::{boxed::Box, vec::Vec};
use argon_expr::{Builtin, ErasureMode, Expr};
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

pub fn erase_signature(sig: &FunctionSignature<DefaultExprContext>) -> ErasedSignature {
    ErasedSignature {
        parameters: sig
            .parameters
            .iter()
            .map(|param| match param.erasure_mode {
                ErasureMode::Erased => ErasedSignatureType::Erased,
                ErasureMode::Token | ErasureMode::Concrete => erase_type(&param.param_type),
            })
            .collect(),
        result: erase_type(&sig.return_type),
    }
}

fn erase_type(t: &Expr<DefaultExprContext>) -> ErasedSignatureType {
    match t {
        Expr::Builtin { builtin, arguments } => erase_builtin(*builtin, arguments),

        Expr::FunctionType { a, r } => {
            ErasedSignatureType::Function(Box::new(erase_type(a)), Box::new(erase_type(r)))
        }

        Expr::EnumType(decl, arguments) => ErasedSignatureType::Declared(
            decl.clone().import_specifier(),
            arguments.iter().map(erase_type).collect::<Vec<_>>(),
        ),

        Expr::RecordType(decl, arguments) => ErasedSignatureType::Declared(
            decl.clone().import_specifier(),
            arguments.iter().map(erase_type).collect::<Vec<_>>(),
        ),

        Expr::TraitType(decl, arguments) => ErasedSignatureType::Declared(
            decl.clone().import_specifier(),
            arguments.iter().map(erase_type).collect::<Vec<_>>(),
        ),

        Expr::Tuple { items } => ErasedSignatureType::Tuple(
            items
                .iter()
                .map(erase_type)
                .collect::<Vec<ErasedSignatureType>>(),
        ),

        _ => ErasedSignatureType::Erased,
    }
}

fn erase_builtin(builtin: Builtin, arguments: &[Expr<DefaultExprContext>]) -> ErasedSignatureType {
    if arguments.is_empty() {
        return ErasedSignatureType::Builtin(builtin);
    }

    match builtin {
        Builtin::ArrayType => ErasedSignatureType::Builtin(builtin),

        _ => ErasedSignatureType::Erased,
    }
}
