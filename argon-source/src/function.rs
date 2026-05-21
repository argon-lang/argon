use crate::module::DeclarationResult;
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{DefaultExprContext, Function, FunctionImplementation, FunctionMetadata};
use argon_parser::ast;
use std::sync::{Arc, OnceLock};

pub struct SourceFunction {
    metadata: FunctionMetadata,
    signature: OnceLock<Arc<FunctionSignature<DefaultExprContext>>>,
    implementation: OnceLock<Arc<FunctionImplementation>>,
}

impl SourceFunction {
    pub fn from_ast(decl: Box<ast::FunctionDeclarationStmt>) -> DeclarationResult<Self> {
        todo!()
    }
}

impl Function for SourceFunction {
    fn metadata(&self) -> &FunctionMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        self.signature.get_or_init(|| todo!()).clone()
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        Some(self.implementation.get_or_init(|| todo!()).clone())
    }
}
