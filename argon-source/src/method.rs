use crate::modifiers::{ACCESS_MODIFIER, IS_INLINE, IS_WITNESS, METHOD_SLOT_ABSTRACT, METHOD_SLOT_CONCRETE, ModifierParser, ERASURE_MODE_NON_TOKEN};
use crate::module::DeclarationResult;
use crate::signature::SignatureParser;
use crate::type_checker::{TypeCheckOptions, type_check_expr};
use alloc::boxed::Box;
use alloc::sync::Arc;
use argon_compiler::access::{AccessModifier, AccessToken};
use argon_compiler::scope::{InstanceParameterScope, ParameterScope, Scope};
use argon_compiler::{
    Context, DefaultExprContext, EffectInfo, FunctionImplementation, FunctionSignature, Method,
    MethodInstanceParameter, MethodMetadata, MethodOwner, Unload,
};
use argon_expr::{
    ErasureMode, Expr, ExpressionOwner, InstanceParameterVariable, TraitType, Variable,
};
use argon_parser::ast;
use argon_util::{CompileError, MultiSlice};
use argon_util::sync::{Mutex, ThreadSafe, mutex_lock};
use core::fmt::Debug;

pub trait MethodClosure: ThreadSafe {
    fn owner(&self) -> MethodOwner;
    fn scope(&self) -> impl Scope<ExprContext = DefaultExprContext>;
    fn access_token(&self) -> AccessToken;
}

pub struct SourceMethod<MC> {
    context: Context,
    closure: MC,
    decl: ast::MethodDeclarationStmt,
    metadata: MethodMetadata,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    implementation: Mutex<Option<Arc<FunctionImplementation>>>,
}

impl<MC: MethodClosure + 'static> SourceMethod<MC> {
    pub fn from_ast(
        context: Context,
        closure: MC,
        decl: ast::MethodDeclarationStmt,
    ) -> DeclarationResult<dyn Method, AccessModifier> {
        let mut modifiers =
            ModifierParser::new(context.clone(), &decl.modifiers, &decl.name.location);
        let is_abstract = decl.body.is_none();
        let access = modifiers.parse(&ACCESS_MODIFIER);
        let erasure_mode = modifiers.parse(&ERASURE_MODE_NON_TOKEN);
        if erasure_mode == ErasureMode::Erased && !decl.purity {
            context
                .reporter()
                .report_error(CompileError::impure_erased_function(
                    decl.name.location.clone(),
                ));
        }

        let metadata = MethodMetadata {
            name: decl.name.value.clone(),
            access,
            is_abstract,
            is_inline: modifiers.parse(&IS_INLINE),
            erasure_mode,
            is_witness: modifiers.parse(&IS_WITNESS),
            slot: modifiers.parse(if is_abstract {
                &METHOD_SLOT_ABSTRACT
            } else {
                &METHOD_SLOT_CONCRETE
            }),
            effect_info: if decl.purity {
                EffectInfo::Pure
            } else {
                EffectInfo::Effectful
            },
            instance_parameter: MethodInstanceParameter {
                name: decl.instance_name.value.clone(),
            },
        };
        modifiers.done();

        DeclarationResult {
            access,
            result: Arc::new(Self {
                context,
                closure,
                decl,
                metadata,
                signature: Mutex::new(None),
                implementation: Mutex::new(None),
            }),
        }
    }

    fn receiver_type(&self) -> Expr<DefaultExprContext> {
        match self.closure.owner() {
            MethodOwner::Trait(trait_) => {
                let owner = ExpressionOwner::Trait(trait_.clone());
                let signature = trait_.clone().signature();

                Expr::TraitType(TraitType {
                    trait_,
                    arguments: SignatureParser::get_parameter_variables(
                        &owner,
                        &signature.parameters,
                    )
                    .map(|param| Expr::Variable(Variable::Parameter(Box::new(param))))
                    .collect(),
                })
            }
            MethodOwner::Instance(instance) => instance.signature().return_type.clone(),
        }
    }

    fn access_token(&self) -> AccessToken {
        self.closure.access_token()
    }
}

impl<MC> Debug for SourceMethod<MC> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "trait method {}", self.metadata.name)
    }
}

impl<MC> Unload for SourceMethod<MC> {
    fn unload(&self) {
        *mutex_lock(&self.signature) = None;
        *mutex_lock(&self.implementation) = None;
    }
}

impl<MC: MethodClosure + 'static> Method for SourceMethod<MC> {
    fn owner(self: Arc<Self>) -> MethodOwner {
        self.closure.owner()
    }

    fn metadata(&self) -> &MethodMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        let mut sig_store = mutex_lock(&self.signature);
        if let Some(ref sig) = *sig_store {
            return sig.clone();
        }

        let scope = self.closure.scope();
        let access_token = self.access_token();
        let owner_ref: Arc<dyn Method> = self.clone();
        let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Method(owner_ref);

        let sig = SignatureParser {
            context: self.context.clone(),
            scope: &scope,
            access_token,
            owner,
        }
        .parse(
            MultiSlice::from(self.decl.parameters.as_slice()),
            &self.decl.return_type,
        );

        let result = Arc::new(sig);
        *sig_store = Some(result.clone());
        result
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        let body = self.decl.body.as_ref()?;

        let mut implementation_store = mutex_lock(&self.implementation);
        if let Some(ref implementation) = *implementation_store {
            return Some(implementation.clone());
        }

        let implementation = match body {
            ast::FunctionBody::ExprBody(body) => {
                let access_token = self.access_token();

                let signature = self.clone().signature();
                let scope = self.closure.scope();
                let method_owner = ExpressionOwner::<DefaultExprContext>::Method(self.clone());

                let receiver_scope = InstanceParameterScope::new(
                    scope,
                    InstanceParameterVariable {
                        owner: ExpressionOwner::<DefaultExprContext>::Method(self.clone()),
                        var_type: self.receiver_type(),
                        name: self.metadata.instance_parameter.name.clone(),
                    },
                );

                let parameter_scope =
                    ParameterScope::new(receiver_scope, method_owner, &signature.parameters);

                let expr = type_check_expr(
                    self.context.clone(),
                    TypeCheckOptions::new(&access_token, &parameter_scope, self.metadata.erasure_mode),
                    body.as_ref(),
                    &signature.return_type,
                );

                FunctionImplementation::Expr(expr)
            }
            ast::FunctionBody::ExternBody(name) => {
                let externs = self.context.extern_function(name);
                FunctionImplementation::Extern(externs)
            }
        };

        let result = Arc::new(implementation);
        *implementation_store = Some(result.clone());
        Some(result)
    }
}
