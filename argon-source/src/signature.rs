use crate::modifiers::{ERASURE_MODE, ModifierParser};
use crate::module::GlobalScope;
use crate::type_checker::type_check_type_expr;
use argon_compiler::access::AccessToken;
use argon_compiler::scope::ParameterScope;
use argon_compiler::signature::{FunctionSignature, ParameterBinding, SignatureParameter};
use argon_compiler::{Context, DefaultExprContext};
use argon_expr::{ErasureMode, Expr, ExpressionOwner};
use argon_parser::ast;
use argon_util::CompileError;
use parse18_runtime::WithLocation;

pub struct SignatureParser<'a> {
    pub context: Context,
    pub scope: &'a mut GlobalScope,
    pub access_token: AccessToken,
    pub owner: ExpressionOwner<DefaultExprContext>,
}

impl<'a> SignatureParser<'a> {
    pub fn parse(
        &mut self,
        params: &[WithLocation<ast::FunctionParameterList>],
        return_type: &WithLocation<ast::ReturnTypeSpecifier>,
    ) -> FunctionSignature<DefaultExprContext> {
        let allow_concrete_params = self.allow_concrete_params();

        let mut parameters = Vec::with_capacity(params.len());

        for param in params {
            let mut mp = ModifierParser::new(
                self.context.clone(),
                &param.value.modifiers,
                &param.location,
            );
            let erasure_mode = mp.parse(&ERASURE_MODE);
            mp.done();

            if !allow_concrete_params && erasure_mode == ErasureMode::Concrete {
                self.context
                    .reporter()
                    .report_error(CompileError::type_parameter_is_concrete(
                        param.location.clone(),
                    ));
            }

            let mut parameter_scope =
                ParameterScope::new(self.scope, self.owner.clone(), &parameters);

            let bindings = param
                .value
                .parameters
                .iter()
                .map(|param_elem| {
                    let t = type_check_type_expr(
                        self.context.clone(),
                        &mut parameter_scope,
                        &param_elem.value.param_type,
                    );

                    ParameterBinding {
                        name: Some(param_elem.value.name.clone()),
                        param_type: t,
                    }
                })
                .collect::<Vec<_>>();

            let single_binding = match &bindings[..] {
                [binding] if param.value.has_trailing_comma => Some(binding),
                _ => None,
            };

            let name;
            let param_type;
            match single_binding {
                Some(binding) => {
                    name = binding.name.clone();
                    param_type = binding.param_type.clone();
                }
                None => {
                    name = None;
                    param_type = Expr::Tuple {
                        items: bindings
                            .iter()
                            .map(|binding| binding.param_type.clone())
                            .collect::<Vec<_>>(),
                    };
                }
            }

            let param = SignatureParameter {
                list_type: param.value.list_type,
                erasure_mode,
                bindings,
                name,
                param_type,
            };

            parameters.push(param);
        }

        let mut parameter_scope = ParameterScope::new(self.scope, self.owner.clone(), &parameters);

        let conv_return_type = type_check_type_expr(
            self.context.clone(),
            &mut parameter_scope,
            &return_type.value.return_type,
        );

        let ensures_clauses = return_type
            .value
            .ensures_clauses
            .iter()
            .map(|clause| type_check_type_expr(self.context.clone(), &mut parameter_scope, clause))
            .collect::<Vec<_>>();

        FunctionSignature {
            parameters,
            return_type: conv_return_type,
            ensures_clauses,
        }
    }

    fn allow_concrete_params(&self) -> bool {
        match &self.owner {
            ExpressionOwner::Function(f) => f.metadata().erasure_mode != ErasureMode::Token,

            ExpressionOwner::Record(_)
            | ExpressionOwner::Enum(_)
            | ExpressionOwner::Trait(_)
            | ExpressionOwner::EnumVariant(_)
            | ExpressionOwner::Method(_) => false,

            ExpressionOwner::Instance(_) => todo!(),
        }
    }

    fn owner_is_erased(&self) -> bool {
        match &self.owner {
            ExpressionOwner::Function(f) => f.metadata().erasure_mode == ErasureMode::Erased,

            ExpressionOwner::Record(_)
            | ExpressionOwner::Enum(_)
            | ExpressionOwner::Trait(_)
            | ExpressionOwner::EnumVariant(_)
            | ExpressionOwner::Method(_)
            | ExpressionOwner::Instance(_) => false,
        }
    }
}
