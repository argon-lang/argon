use crate::module::{DeclarationResult, DeclarationClosure};
use crate::modifiers::{
    ACCESS_MODIFIER_GLOBAL, ERASURE_MODE, IS_INLINE, IS_WITNESS, ModifierParser,
};
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{
    Context, DefaultExprContext, EffectInfo, Function, FunctionImplementation, FunctionMetadata,
};
use argon_parser::ast;
use std::sync::{Arc, OnceLock};
use argon_expr::ExpressionOwner;
use crate::signature::SignatureParser;

pub struct SourceFunction {
    context: Context,
    decl: Box<ast::FunctionDeclarationStmt>,
    closure: Box<dyn DeclarationClosure>,
    metadata: FunctionMetadata,
    signature: OnceLock<Arc<FunctionSignature<DefaultExprContext>>>,
    implementation: OnceLock<Arc<FunctionImplementation>>,
}

impl SourceFunction {
    pub fn from_ast(
        context: Context,
        closure: Box<dyn DeclarationClosure>,
        decl: Box<ast::FunctionDeclarationStmt>,
    ) -> DeclarationResult<Self> {
        let mut modifiers = ModifierParser::new(context.clone(), &decl.modifiers, &decl.name.location);
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
                signature: OnceLock::new(),
                implementation: OnceLock::new(),
            }),
        }
    }
}

impl Function for SourceFunction {
    fn metadata(&self) -> &FunctionMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        self.signature.get_or_init(|| {
            let mut scope = self.closure.scope();
            let access_token = self.closure.access_token();

            let owner_ref: Arc<dyn Function> = self.clone();
            let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Function(owner_ref);

            let sig = SignatureParser {
                context: self.context.clone(),
                scope: &mut scope,
                access_token,
                owner,
            }.parse(&self.decl.parameters, &self.decl.return_type);

            Arc::new(sig)
        }).clone()
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        Some(self.implementation.get_or_init(|| todo!()).clone())
    }
}
