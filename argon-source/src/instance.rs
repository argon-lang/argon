use crate::method::{MethodClosure, SourceMethod};
use crate::modifiers::{ACCESS_MODIFIER_GLOBAL, ModifierParser, ERASURE_MODE_CONCRETE};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::signature::SignatureParser;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::access::AccessToken;
use argon_compiler::erased_sig::{ImportSpecifier, erase_signature};
use argon_compiler::scope::{ParameterScope, Scope};
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{
    Context, DefaultExprContext, ErasureMode, Instance, MethodEntry, MethodOwner, TypeDeclaration,
    Unload,
};
use argon_expr::ExpressionOwner;
use argon_parser::ast;
use argon_util::MultiSlice;
use argon_util::sync::{Mutex, mutex_lock};
use core::fmt::Debug;
use parse18_runtime::WithLocation;

pub struct SourceInstance {
    context: Context,
    decl: Box<ast::InstanceDeclarationStmt>,
    closure: Box<dyn DeclarationClosure>,
    erasure_mode: ErasureMode,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    methods: Mutex<Option<Arc<Vec<MethodEntry>>>>,
}

impl SourceInstance {
    pub fn from_ast(
        context: Context,
        closure: Box<dyn DeclarationClosure>,
        decl: Box<ast::InstanceDeclarationStmt>,
    ) -> DeclarationResult<Self> {
        let mut modifiers =
            ModifierParser::new(context.clone(), &decl.modifiers, &decl.name.location);
        let access = modifiers.parse(&ACCESS_MODIFIER_GLOBAL);
        let erasure_mode = modifiers.parse(&ERASURE_MODE_CONCRETE);
        modifiers.done();

        DeclarationResult {
            access,
            result: Arc::new(Self {
                context,
                decl,
                closure,
                erasure_mode,
                signature: Mutex::new(None),
                methods: Mutex::new(None),
            }),
        }
    }

    fn return_type_specifier(&self) -> WithLocation<ast::ReturnTypeSpecifier> {
        match &self.decl.return_type {
            Some(return_type) => SignatureParser::expr_to_return_type(return_type),
            None => WithLocation {
                location: self.decl.name.location.clone(),
                value: ast::ReturnTypeSpecifier {
                    return_type: WithLocation {
                        location: self.decl.name.location.clone(),
                        value: ast::Expr::Type,
                    },
                    ensures_clauses: Vec::new(),
                },
            },
        }
    }

    fn access_token(self: &Arc<Self>) -> AccessToken {
        let mut access_token = self.closure.access_token();
        let instance_ref: Arc<dyn Instance> = self.clone();
        access_token.add_type_permissions(TypeDeclaration::Instance(instance_ref));
        access_token
    }
}

impl Debug for SourceInstance {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "instance {}", self.decl.name.value)
    }
}

impl Unload for SourceInstance {
    fn unload(&self) {
        *mutex_lock(&self.signature) = None;
        *mutex_lock(&self.methods) = None;
    }
}

impl Instance for SourceInstance {
    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        let signature = erase_signature(self.context.clone(), self.clone().signature().as_ref());
        self.closure
            .import_specifier(self.decl.name.value.clone(), signature)
    }

    fn erasure_mode(&self) -> ErasureMode {
        self.erasure_mode
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        let mut sig_store = mutex_lock(&self.signature);
        if let Some(ref sig) = *sig_store {
            return sig.clone();
        }

        let scope = self.closure.scope();
        let access_token = self.access_token();
        let owner_ref: Arc<dyn Instance> = self.clone();
        let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Instance(owner_ref);
        let return_type = self.return_type_specifier();

        let sig = SignatureParser {
            context: self.context.clone(),
            scope: &scope,
            access_token,
            owner,
        }
        .parse(
            MultiSlice::from(self.decl.parameters.as_slice()),
            &return_type,
        );

        let result = Arc::new(sig);
        *sig_store = Some(result.clone());
        result
    }

    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>> {
        let mut methods_store = mutex_lock(&self.methods);
        if let Some(ref methods) = *methods_store {
            return methods.clone();
        }

        let methods = self
            .decl
            .body
            .iter()
            .filter_map(|stmt| match &stmt.value {
                ast::TraitBodyStmt::MethodDeclaration(method) => {
                    let closure = InstanceMethodClosure {
                        instance: self.clone(),
                    };
                    let method_res =
                        SourceMethod::from_ast(self.context.clone(), closure, (**method).clone());
                    Some(MethodEntry {
                        access: method_res.access,
                        method: method_res.result,
                    })
                }
                ast::TraitBodyStmt::FunctionDeclaration(_) => todo!(),
            })
            .collect::<Vec<_>>();

        let result = Arc::new(methods);
        *methods_store = Some(result.clone());
        result
    }
}

struct InstanceMethodClosure {
    instance: Arc<SourceInstance>,
}

impl MethodClosure for InstanceMethodClosure {
    fn owner(&self) -> MethodOwner {
        MethodOwner::Instance(self.instance.clone())
    }

    fn scope(&self) -> impl Scope<ExprContext = DefaultExprContext> {
        let parent = self.instance.closure.scope();
        let instance_ref: Arc<dyn Instance> = self.instance.clone();
        let owner = ExpressionOwner::Instance(instance_ref);
        let signature = self.instance.clone().signature();

        ParameterScope::new(parent, owner, &signature.parameters)
    }

    fn access_token(&self) -> AccessToken {
        self.instance.access_token()
    }
}
