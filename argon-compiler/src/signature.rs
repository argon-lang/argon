use alloc::borrow::Cow;
use alloc::{boxed::Box, vec::Vec};
use argon_expr::{
    ErasureMode, Expr, ExprContext, ExprContextShifter, ExprScannerMut, ExpressionOwner,
    ParameterVariable, SubstScanner, Variable,
};
use argon_parser::ast::{FunctionParameterListType, Identifier};
use derivative::Derivative;

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct FunctionSignature<EC: ExprContext + ?Sized> {
    pub parameters: Vec<SignatureParameter<EC>>,
    pub return_type: Expr<EC>,
    pub ensures_clauses: Vec<Expr<EC>>,
}

impl<EC: ExprContext + ?Sized> FunctionSignature<EC> {
    pub fn scan_mut<S>(&mut self, scanner: &mut S) -> bool
    where
        S: ExprScannerMut<EC = EC> + ?Sized,
    {
        self.parameters.iter_mut().all(|parameter| {
            parameter.scan_mut(scanner)
        }) && scanner.scan(&mut self.return_type)
            && self
                .ensures_clauses
                .iter_mut()
                .all(|ensures_clause| scanner.scan(ensures_clause))
    }

    pub fn shift<Sh>(self, shifter: &mut Sh) -> FunctionSignature<Sh::EC2>
    where
        Sh: ExprContextShifter<EC1 = EC> + ?Sized,
    {
        FunctionSignature {
            parameters: self
                .parameters
                .into_iter()
                .map(|parameter| parameter.shift(shifter))
                .collect(),
            return_type: shifter.shift(self.return_type),
            ensures_clauses: self
                .ensures_clauses
                .into_iter()
                .map(|ensures_clause| shifter.shift(ensures_clause))
                .collect(),
        }
    }
}

pub trait SubstFunctionSignature<'a, EC: ExprContext + ?Sized> {
    fn add_function_parameter_substitutions(
        &mut self,
        owner: ExpressionOwner<EC>,
        signature: &FunctionSignature<EC>,
        arguments: &'a [Expr<EC>],
    );
}

impl<'a, EC: ExprContext + ?Sized> SubstFunctionSignature<'a, EC> for SubstScanner<'a, EC> {
    fn add_function_parameter_substitutions(
        &mut self,
        owner: ExpressionOwner<EC>,
        signature: &FunctionSignature<EC>,
        arguments: &'a [Expr<EC>],
    ) {
        debug_assert_eq!(signature.parameters.len(), arguments.len());

        for ((parameter_index, parameter), argument) in
            signature.parameters.iter().enumerate().zip(arguments)
        {
            let variable = Variable::Parameter(Box::new(
                parameter
                    .clone()
                    .to_parameter_var(owner.clone(), parameter_index),
            ));

            self.add_substitution(variable, Cow::Borrowed(argument));
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct SignatureParameter<EC: ExprContext + ?Sized> {
    pub list_type: FunctionParameterListType,
    pub erasure_mode: ErasureMode,
    pub bindings: Vec<ParameterBinding<EC>>,
    pub name: Option<Identifier>,
    pub param_type: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> SignatureParameter<EC> {
    pub fn shift<Sh>(self, shifter: &mut Sh) -> SignatureParameter<Sh::EC2>
    where
        Sh: ExprContextShifter<EC1 = EC> + ?Sized,
    {
        SignatureParameter {
            list_type: self.list_type,
            erasure_mode: self.erasure_mode,
            bindings: self
                .bindings
                .into_iter()
                .map(|binding| binding.shift(shifter))
                .collect(),
            name: self.name,
            param_type: shifter.shift(self.param_type),
        }
    }

    pub fn scan_mut<S>(&mut self, scanner: &mut S) -> bool
    where
        S: ExprScannerMut<EC = EC> + ?Sized,
    {
        self.bindings.iter_mut().all(|binding| scanner.scan(&mut binding.param_type))
            && scanner.scan(&mut self.param_type)
    }

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

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct ParameterBinding<EC: ExprContext + ?Sized> {
    pub name: Option<Identifier>,
    pub param_type: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> ParameterBinding<EC> {
    pub fn shift<Sh>(self, shifter: &mut Sh) -> ParameterBinding<Sh::EC2>
    where
        Sh: ExprContextShifter<EC1 = EC> + ?Sized,
    {
        ParameterBinding {
            name: self.name,
            param_type: shifter.shift(self.param_type),
        }
    }
}
