use alloc::vec::Vec;
use argon_expr::{ErasureMode, Expr, ExprContext, ExpressionOwner, ParameterVariable};
use argon_parser::ast::{FunctionParameterListType, Identifier};

pub struct FunctionSignature<EC: ExprContext + ?Sized> {
    pub parameters: Vec<SignatureParameter<EC>>,
    pub return_type: Expr<EC>,
    pub ensures_clauses: Vec<Expr<EC>>,
}

pub struct SignatureParameter<EC: ExprContext + ?Sized> {
    pub list_type: FunctionParameterListType,
    pub erasure_mode: ErasureMode,
    pub bindings: Vec<ParameterBinding<EC>>,
    pub name: Option<Identifier>,
    pub param_type: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> SignatureParameter<EC> {
    pub fn to_parameter_var(
        self,
        owner: ExpressionOwner<EC>,
        index: usize,
    ) -> ParameterVariable<EC> {
        ParameterVariable {
            owner,
            parameter_index: index,
            var_type: self.param_type,
            name: self.name,
            erasure_mode: self.erasure_mode,
            is_witness: self.list_type == FunctionParameterListType::RequiresList,
        }
    }
}

impl<EC: ExprContext + ?Sized> Clone for SignatureParameter<EC> {
    fn clone(&self) -> Self {
        SignatureParameter {
            list_type: self.list_type,
            erasure_mode: self.erasure_mode,
            bindings: self.bindings.clone(),
            name: self.name.clone(),
            param_type: self.param_type.clone(),
        }
    }
}

pub struct ParameterBinding<EC: ExprContext + ?Sized> {
    pub name: Option<Identifier>,
    pub param_type: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> Clone for ParameterBinding<EC> {
    fn clone(&self) -> Self {
        ParameterBinding {
            name: self.name.clone(),
            param_type: self.param_type.clone(),
        }
    }
}
