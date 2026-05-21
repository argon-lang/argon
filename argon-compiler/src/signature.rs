use crate::ErasureMode;
use argon_expr::{Expr, ExprContext};
use argon_parser::ast::{FunctionParameterListType, IdentifierExpr};

pub struct FunctionSignature<EC: ExprContext> {
    pub parameters: Vec<SignatureParameter<EC>>,
    pub return_type: Expr<EC>,
    pub ensures_clauses: Vec<Expr<EC>>,
}

pub struct SignatureParameter<EC: ExprContext> {
    pub list_type: FunctionParameterListType,
    pub erasure_mode: ErasureMode,
    pub bindings: Vec<ParameterBinding<EC>>,
    pub name: Option<IdentifierExpr>,
    pub param_type: Expr<EC>,
}

pub struct ParameterBinding<EC: ExprContext> {
    pub name: Option<IdentifierExpr>,
    pub param_type: Expr<EC>,
}
