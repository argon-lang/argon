use crate::method::{MethodClosure, SourceMethod};
use crate::modifiers::{ACCESS_MODIFIER_GLOBAL, ModifierParser};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::signature::SignatureParser;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::access::AccessToken;
use argon_compiler::erased_sig::{ImportSpecifier, erase_signature};
use argon_compiler::scope::{ParameterScope, Scope};
use argon_compiler::signature::FunctionSignature;
use argon_compiler::vtable::{VTable, build_vtable};
use argon_compiler::{
    Context, DefaultExprContext, MethodEntry, MethodOwner, Trait, TypeDeclaration, Unload,
};
use argon_expr::ExpressionOwner;
use argon_parser::ast;
use argon_util::{MultiSlice, UnloadCell};
use core::fmt::Debug;

pub struct SourceTrait {
    context: Context,
    decl: Box<ast::TraitDeclarationStmt>,
    closure: Box<dyn DeclarationClosure>,
    signature: UnloadCell<Arc<FunctionSignature<DefaultExprContext>>>,
    methods: UnloadCell<Arc<Vec<MethodEntry>>>,
    vtable: UnloadCell<Arc<VTable>>,
}

impl SourceTrait {
    pub fn from_ast(
        context: Context,
        closure: Box<dyn DeclarationClosure>,
        decl: Box<ast::TraitDeclarationStmt>,
    ) -> DeclarationResult<Self> {
        let mut modifiers =
            ModifierParser::new(context.clone(), &decl.modifiers, &decl.name.location);
        let access = modifiers.parse(&ACCESS_MODIFIER_GLOBAL);
        modifiers.done();

        DeclarationResult {
            access,
            result: Arc::new(Self {
                context,
                decl,
                closure,
                signature: UnloadCell::new(),
                methods: UnloadCell::new(),
                vtable: UnloadCell::new(),
            }),
        }
    }

    fn access_token(self: &Arc<Self>) -> AccessToken {
        let mut access_token = self.closure.access_token();
        let trait_ref: Arc<dyn Trait> = self.clone();
        access_token.add_type_permissions(TypeDeclaration::Trait(trait_ref));
        access_token
    }
}

impl Debug for SourceTrait {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "trait {}", self.decl.name.value)
    }
}

impl Unload for SourceTrait {
    fn unload(&self) {
        self.signature.unload();
        self.methods.unload();
        self.vtable.unload();
        self.closure.unload();
    }
}

impl Trait for SourceTrait {
    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        let signature = erase_signature(self.context.clone(), self.clone().signature().as_ref());
        self.closure
            .import_specifier(self.decl.name.value.clone(), signature)
    }

    fn location(&self) -> parse18_runtime::Location {
        self.decl.name.location.clone()
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        self.signature.initialize(|| {
            let scope = self.closure.scope();
            let access_token = self.access_token();
            let owner_ref: Arc<dyn Trait> = self.clone();
            let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Trait(owner_ref);
            let return_type =
                SignatureParser::get_type_sig_return_type(&self.decl.name, &self.decl.return_type);

            Arc::new(
                SignatureParser {
                    context: self.context.clone(),
                    scope: &scope,
                    access_token,
                    owner,
                }
                .parse(
                    MultiSlice::from(self.decl.parameters.as_slice()),
                    &return_type,
                ),
            )
        })
    }

    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>> {
        self.methods.initialize(|| {
            Arc::new(
                self.decl
                    .body
                    .iter()
                    .filter_map(|stmt| match &stmt.value {
                        ast::TraitBodyStmt::MethodDeclaration(method) => {
                            let closure = TraitMethodClosure {
                                trait_: self.clone(),
                            };
                            let method_res = SourceMethod::from_ast(
                                self.context.clone(),
                                closure,
                                (**method).clone(),
                            );
                            Some(MethodEntry {
                                access: method_res.access,
                                method: method_res.result,
                            })
                        }
                        ast::TraitBodyStmt::FunctionDeclaration(_) => None,
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn vtable(self: Arc<Self>) -> Arc<VTable> {
        self.vtable.initialize(|| {
            let trait_ref: Arc<dyn Trait> = self.clone();
            Arc::new(build_vtable(
                self.context.clone(),
                MethodOwner::Trait(trait_ref),
                self.access_token(),
                self.location(),
            ))
        })
    }
}

struct TraitMethodClosure {
    trait_: Arc<SourceTrait>,
}

impl MethodClosure for TraitMethodClosure {
    fn owner(&self) -> MethodOwner {
        MethodOwner::Trait(self.trait_.clone())
    }

    fn scope(&self) -> impl Scope<ExprContext = DefaultExprContext> {
        let parent = self.trait_.closure.scope();
        let trait_ref: Arc<dyn Trait> = self.trait_.clone();
        let owner = ExpressionOwner::Trait(trait_ref);
        let signature = self.trait_.clone().signature();

        ParameterScope::new(parent, owner, &signature.parameters)
    }

    fn access_token(&self) -> AccessToken {
        self.trait_.access_token()
    }
}
