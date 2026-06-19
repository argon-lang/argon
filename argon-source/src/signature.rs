use crate::modifiers::{ERASURE_MODE, ModifierParser};
use crate::type_checker::{TypeCheckOptions, type_check_type_expr};
use alloc::vec::Vec;
use argon_compiler::access::AccessToken;
use argon_compiler::scope::{ParameterScope, Scope};
use argon_compiler::signature::{FunctionSignature, ParameterBinding, SignatureParameter};
use argon_compiler::{Context, DefaultExprContext};
use argon_expr::{ErasureMode, Expr, ExpressionOwner, ParameterVariable};
use argon_parser::ast;
use argon_util::{CompileError, MultiSlice};
use parse18_runtime::WithLocation;

pub struct SignatureParser<'a> {
    pub context: Context,
    pub scope: &'a dyn Scope<ExprContext = DefaultExprContext>,
    pub access_token: AccessToken,
    pub owner: ExpressionOwner<DefaultExprContext>,
}

impl<'a> SignatureParser<'a> {
    pub fn parse(
        &self,
        params: MultiSlice<'_, WithLocation<ast::FunctionParameterList>>,
        return_type: &WithLocation<ast::ReturnTypeSpecifier>,
    ) -> FunctionSignature<DefaultExprContext> {
        let allow_concrete_params = self.allow_concrete_params();

        let mut parameters = Vec::with_capacity(params.len());

        for param in &params {
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

            let parent_scope: &dyn Scope<ExprContext = DefaultExprContext> = self.scope;
            let mut parameter_scope =
                ParameterScope::new(parent_scope, self.owner.clone(), &parameters);

            let mut bindings = param
                .value
                .parameters
                .iter()
                .map(|param_elem| {
                    let t = type_check_type_expr(
                        self.context.clone(),
                        TypeCheckOptions::new(
                            &self.access_token,
                            &mut parameter_scope,
                            erasure_mode,
                        ),
                        &param_elem.value.param_type,
                    );

                    ParameterBinding {
                        name: Some(param_elem.value.name.clone()),
                        param_type: t,
                    }
                })
                .collect::<Vec<_>>();

            let single_binding = match &bindings[..] {
                [binding] if !param.value.has_trailing_comma => Some(binding),
                _ => None,
            };

            let name;
            let param_type;
            match single_binding {
                Some(binding) => {
                    name = binding.name.clone();
                    param_type = binding.param_type.clone();
                    bindings.clear();
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
                list_type: param.value.list_type.clone(),
                erasure_mode,
                bindings,
                name,
                param_type,
            };

            parameters.push(param);
        }

        let parent_scope: &dyn Scope<ExprContext = DefaultExprContext> = self.scope;
        let mut parameter_scope =
            ParameterScope::new(parent_scope, self.owner.clone(), &parameters);

        let conv_return_type = type_check_type_expr(
            self.context.clone(),
            TypeCheckOptions::new(&self.access_token, &mut parameter_scope, ErasureMode::Concrete),
            &return_type.value.return_type,
        );

        let ensures_clauses = return_type
            .value
            .ensures_clauses
            .iter()
            .map(|clause| {
                type_check_type_expr(
                    self.context.clone(),
                    TypeCheckOptions::new(&self.access_token, &mut parameter_scope, ErasureMode::Token),
                    clause,
                )
            })
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

            ExpressionOwner::Record(_) | ExpressionOwner::Enum(_) | ExpressionOwner::Trait(_) => {
                false
            }

            ExpressionOwner::EnumVariant(_) | ExpressionOwner::Method(_) => true,

            ExpressionOwner::Instance(i) => i.erasure_mode() != ErasureMode::Token,
        }
    }

    pub fn expr_to_return_type(
        expr: &WithLocation<ast::Expr>,
    ) -> WithLocation<ast::ReturnTypeSpecifier> {
        WithLocation {
            location: expr.location.clone(),
            value: ast::ReturnTypeSpecifier {
                return_type: expr.clone(),
                ensures_clauses: Vec::new(),
            },
        }
    }

    pub fn get_type_sig_return_type(
        name: &WithLocation<ast::Identifier>,
        rt: &Option<WithLocation<ast::Expr>>,
    ) -> WithLocation<ast::ReturnTypeSpecifier> {
        match rt {
            Some(rt) => WithLocation {
                location: rt.location.clone(),
                value: ast::ReturnTypeSpecifier {
                    return_type: rt.clone(),
                    ensures_clauses: Vec::new(),
                },
            },

            None => WithLocation {
                location: name.location.clone(),
                value: ast::ReturnTypeSpecifier {
                    return_type: WithLocation {
                        location: name.location.clone(),
                        value: ast::Expr::Type,
                    },
                    ensures_clauses: Vec::new(),
                },
            },
        }
    }

    pub fn get_parameter_variables<'b>(
        owner: &'a ExpressionOwner<DefaultExprContext>,
        params: &'a [SignatureParameter<DefaultExprContext>],
    ) -> impl Iterator<Item = ParameterVariable<DefaultExprContext>> + 'a {
        params
            .iter()
            .enumerate()
            .map(|(index, param)| param.clone().to_parameter_var(owner.clone(), index))
    }
}
