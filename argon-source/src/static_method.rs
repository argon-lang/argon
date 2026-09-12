use crate::modifiers::{
    ACCESS_MODIFIER, ERASURE_MODE_NON_TOKEN, IS_INLINE, IS_WITNESS, ModifierParser,
    UNSAFE_ASSUME_PURE,
};
use crate::module::DeclarationResult;
use crate::signature::SignatureParser;
use crate::type_checker::{TypeCheckOptions, type_check_expr};
use alloc::sync::Arc;
use argon_compiler::access::{AccessModifier, AccessToken};
use argon_compiler::scope::{ParameterScope, Scope};
use argon_compiler::{
    Context, DefaultExprContext, EffectInfo, FunctionImplementation, FunctionSignature,
    StaticMethod, StaticMethodMetadata, StaticMethodOwner, Unload,
};
use argon_expr::{ErasureMode, ExpressionOwner};
use argon_parser::ast;
use argon_util::{CompileError, MultiSlice, UnloadCell};
use core::fmt::Debug;

pub trait StaticMethodClosure: argon_util::sync::ThreadSafe {
    fn owner(&self) -> StaticMethodOwner;
    fn scope(&self) -> impl Scope<ExprContext = DefaultExprContext>;
    fn access_token(&self) -> AccessToken;
}

pub struct SourceStaticMethod<MC> {
    context: Context,
    closure: MC,
    decl: ast::StaticMethodDeclarationStmt,
    metadata: StaticMethodMetadata,
    unsafe_assume_pure: bool,
    signature: UnloadCell<Arc<FunctionSignature<DefaultExprContext>>>,
    implementation: UnloadCell<Arc<FunctionImplementation>>,
}

impl<MC: StaticMethodClosure + 'static> SourceStaticMethod<MC> {
    pub fn from_ast(
        context: Context,
        closure: MC,
        decl: ast::StaticMethodDeclarationStmt,
    ) -> DeclarationResult<dyn StaticMethod, AccessModifier> {
        let mut modifiers =
            ModifierParser::new(context.clone(), &decl.modifiers, &decl.name.location);
        let access = modifiers.parse(&ACCESS_MODIFIER);
        let erasure_mode = modifiers.parse(&ERASURE_MODE_NON_TOKEN);
        let unsafe_assume_pure = modifiers.parse(&UNSAFE_ASSUME_PURE);
        if decl.body.is_none() {
            context
                .reporter()
                .report_error(CompileError::abstract_method_error(Some(
                    decl.name.location.clone(),
                )));
        }
        if erasure_mode == ErasureMode::Erased && !(decl.purity || unsafe_assume_pure) {
            context
                .reporter()
                .report_error(CompileError::impure_erased_function(
                    decl.name.location.clone(),
                ));
        }
        let metadata = StaticMethodMetadata {
            access,
            name: decl.name.value.clone(),
            is_inline: modifiers.parse(&IS_INLINE),
            erasure_mode,
            is_witness: modifiers.parse(&IS_WITNESS),
            effect_info: if decl.purity || unsafe_assume_pure {
                EffectInfo::Pure
            } else {
                EffectInfo::Effectful
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
                unsafe_assume_pure,
                signature: UnloadCell::new(),
                implementation: UnloadCell::new(),
            }),
        }
    }
}

impl<MC> Debug for SourceStaticMethod<MC> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "static method {}", self.metadata.name)
    }
}

impl<MC> Unload for SourceStaticMethod<MC> {
    fn unload(&self) {
        self.signature.unload();
        self.implementation.unload();
    }
}

impl<MC: StaticMethodClosure + 'static> StaticMethod for SourceStaticMethod<MC> {
    fn owner(self: Arc<Self>) -> StaticMethodOwner {
        self.closure.owner()
    }
    fn metadata(&self) -> &StaticMethodMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        self.signature.initialize(|| {
            let scope = self.closure.scope();
            let owner: Arc<dyn StaticMethod> = self.clone();
            Arc::new(
                SignatureParser {
                    context: self.context.clone(),
                    scope: &scope,
                    access_token: self.closure.access_token(),
                    owner: ExpressionOwner::StaticMethod(owner),
                }
                .parse(
                    MultiSlice::from(self.decl.parameters.as_slice()),
                    &self.decl.return_type,
                ),
            )
        })
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        let body = self.decl.body.as_ref()?;
        Some(self.implementation.initialize(|| {
            Arc::new(match body {
                ast::FunctionBody::ExprBody(body) => {
                    let signature = self.clone().signature();
                    let owner: Arc<dyn StaticMethod> = self.clone();
                    let scope = self.closure.scope();
                    let scope = ParameterScope::new(
                        scope,
                        ExpressionOwner::StaticMethod(owner),
                        &signature.parameters,
                    );
                    let expr = type_check_expr(
                        self.context.clone(),
                        TypeCheckOptions::new(
                            &self.closure.access_token(),
                            &scope,
                            self.metadata.erasure_mode,
                        )
                        .with_effect_info(if self.unsafe_assume_pure {
                            EffectInfo::Effectful
                        } else {
                            self.metadata.effect_info
                        })
                        .with_ensures_clauses(&signature.ensures_clauses),
                        body.as_ref(),
                        &signature.return_type,
                    );
                    FunctionImplementation::Expr(expr)
                }
                ast::FunctionBody::ExternBody(name) => {
                    FunctionImplementation::Extern(self.context.extern_static_method(name))
                }
            })
        }))
    }
}
