use crate::modifiers::{
    ACCESS_MODIFIER_GLOBAL, ERASURE_MODE, IS_INLINE, IS_WITNESS, ModifierParser,
};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::signature::SignatureParser;
use crate::type_checker::{TypeCheckOptions, type_check_expr};
use alloc::{boxed::Box, sync::Arc};
use argon_compiler::erased_sig::{ImportSpecifier, erase_signature};
use argon_compiler::scope::ParameterScope;
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{
    Context, DefaultExprContext, EffectInfo, Function, FunctionImplementation, FunctionMetadata,
    Unload,
};
use argon_expr::ExpressionOwner;
use argon_parser::ast;
use argon_util::sync::{Mutex, mutex_lock};
use core::fmt::Debug;

pub struct SourceFunction {
    context: Context,
    decl: Box<ast::FunctionDeclarationStmt>,
    closure: Box<dyn DeclarationClosure>,
    metadata: FunctionMetadata,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    implementation: Mutex<Option<Arc<FunctionImplementation>>>,
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
        let metadata = FunctionMetadata {
            is_inline: modifiers.parse(&IS_INLINE),
            erasure_mode: modifiers.parse(&ERASURE_MODE),
            is_witness: modifiers.parse(&IS_WITNESS),
            effect_info: if decl.purity {
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
                signature: Mutex::new(None),
                implementation: Mutex::new(None),
            }),
        }
    }
}

impl Debug for SourceFunction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "function {}", self.decl.name.value)
    }
}

impl Unload for SourceFunction {
    fn unload(&self) {
        *self.signature.lock().unwrap() = None;
        *self.implementation.lock().unwrap() = None;
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
        let mut sig_store = mutex_lock(&self.signature);
        if let Some(ref sig) = *sig_store {
            return sig.clone();
        }

        let mut scope = self.closure.scope();
        let access_token = self.closure.access_token();

        let owner_ref: Arc<dyn Function> = self.clone();
        let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Function(owner_ref);

        let sig = SignatureParser {
            context: self.context.clone(),
            scope: &mut scope,
            access_token,
            owner,
        }
        .parse(&self.decl.parameters, &self.decl.return_type);

        let result = Arc::new(sig);
        *sig_store = Some(result.clone());
        result
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        let mut implementation_store = mutex_lock(&self.implementation);
        if let Some(ref implementation) = *implementation_store {
            return Some(implementation.clone());
        }

        let implementation = match &self.decl.body {
            ast::FunctionBody::ExprBody(body) => {
                let access_token = self.closure.access_token();

                let signature = self.clone().signature();
                let mut scope = self.closure.scope();
                let mut scope = ParameterScope::new(
                    &mut scope,
                    ExpressionOwner::<DefaultExprContext>::Function(self.clone()),
                    &signature.parameters,
                );

                let expr = type_check_expr(
                    self.context.clone(),
                    TypeCheckOptions::new(&access_token, &mut scope),
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
