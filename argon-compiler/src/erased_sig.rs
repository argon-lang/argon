use crate::{DefaultExprContext, FunctionSignature, ModulePath, TubeName};
use argon_expr::Builtin;
use argon_parser::ast::Identifier;
use argon_util::UniqueIdentifier;

#[derive(Clone)]
pub struct ErasedSignature {
    pub parameters: Vec<ErasedSignatureType>,
    pub result: ErasedSignatureType,
}

#[derive(Clone)]
pub enum ErasedSignatureType {
    Builtin(Builtin),
    Function(Box<ErasedSignatureType>, Box<ErasedSignatureType>),
    Record(ImportSpecifier, Vec<ErasedSignatureType>),
    Tuple(Vec<ErasedSignatureType>),
    Erased,
}

#[derive(Clone)]
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
    todo!()
}
