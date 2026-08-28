use crate::modifiers::{
    ModifierParser, ACCESS_MODIFIER_GLOBAL, ERASURE_MODE, IS_INLINE, IS_WITNESS, UNSAFE_ASSUME_PURE,
};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::signature::SignatureParser;
use crate::type_checker::{type_check_expr, TypeCheckOptions};
use alloc::{boxed::Box, sync::Arc};
use argon_compiler::access::AccessToken;
use argon_compiler::erased_sig::{erase_signature, ImportSpecifier};
use argon_compiler::scope::ParameterScope;
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{
    Context, DefaultExprContext, EffectInfo, Function, FunctionImplementation, FunctionMetadata,
    Unload,
};
use argon_expr::{ErasureMode, ExpressionOwner};
use argon_parser::ast;
use argon_util::{CompileError, MultiSlice, UnloadCell};
use core::fmt::Debug;

pub struct SourceFunction {
    context: Context,
    decl: Box<ast::FunctionDeclarationStmt>,
    closure: Box<dyn DeclarationClosure>,
    metadata: FunctionMetadata,
    unsafe_assume_pure: bool,
    signature: UnloadCell<Arc<FunctionSignature<DefaultExprContext>>>,
    implementation: UnloadCell<Arc<FunctionImplementation>>,
}

impl SourceFunction {
    pub fn from_ast(
        context: Context,
        closure: Box<dyn DeclarationClosure>,
        decl: Box<ast::FunctionDeclarationStmt>,
    ) -> DeclarationResult<Self> {
        let mut modifiers =
            ModifierParser::new(context.clone(), &decl.modifiers, &decl.name.location);
        let access = modifiers.parse(&ACCESS_MODIFIER_GLOBAL);
        let erasure_mode = modifiers.parse(&ERASURE_MODE);
        let unsafe_assume_pure = modifiers.parse(&UNSAFE_ASSUME_PURE);
        if erasure_mode == ErasureMode::Erased && !(decl.purity || unsafe_assume_pure) {
            context
                .reporter()
                .report_error(CompileError::impure_erased_function(
                    decl.name.location.clone(),
                ));
        }

        let metadata = FunctionMetadata {
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
                decl,
                closure,
                metadata,
                unsafe_assume_pure,
                signature: UnloadCell::new(),
                implementation: UnloadCell::new(),
            }),
        }
    }

    fn access_token(&self) -> AccessToken {
        self.closure.access_token()
    }
}

impl Debug for SourceFunction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "function {}", self.decl.name.value)
    }
}

impl Unload for SourceFunction {
    fn unload(&self) {
        self.signature.unload();
        self.implementation.unload();
        self.closure.unload();
    }
}

impl Function for SourceFunction {
    fn metadata(&self) -> &FunctionMetadata {
        &self.metadata
    }

    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        let signature = erase_signature(self.context.clone(), self.clone().signature().as_ref());
        self.closure
            .import_specifier(self.decl.name.value.clone(), signature)
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        self.signature.initialize(|| {
            let scope = self.closure.scope();
            let access_token = self.access_token();

            let owner_ref: Arc<dyn Function> = self.clone();
            let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Function(owner_ref);

            Arc::new(
                SignatureParser {
                    context: self.context.clone(),
                    scope: &scope,
                    access_token,
                    owner,
                }
                .parse(
                    MultiSlice::from(self.decl.parameters.as_slice()),
                    &self.decl.return_type,
                ),
            )
        })
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        Some(self.implementation.initialize(|| {
            Arc::new(match &self.decl.body {
                ast::FunctionBody::ExprBody(body) => {
                    let access_token = self.access_token();

                    let signature = self.clone().signature();
                    let scope = self.closure.scope();
                    let scope = ParameterScope::new(
                        scope,
                        ExpressionOwner::<DefaultExprContext>::Function(self.clone()),
                        &signature.parameters,
                    );

                    let expr = type_check_expr(
                        self.context.clone(),
                        TypeCheckOptions::new(&access_token, &scope, self.metadata.erasure_mode)
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
                    let externs = self.context.extern_function(name);
                    FunctionImplementation::Extern(externs)
                }
            })
        }))
    }
}
