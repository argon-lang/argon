use crate::modifiers::{ACCESS_MODIFIER_GLOBAL, ModifierParser};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::record::{SourceRecordField, SourceRecordFieldOwner};
use crate::signature::SignatureParser;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::erased_sig::{ImportSpecifier, erase_signature};
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{
    Context, DefaultExprContext, Enum, EnumVariant, EnumVariantMetadata, RecordField, Unload,
};
use argon_expr::{EnumType, Expr, ExpressionOwner, Variable};
use argon_parser::ast;
use argon_util::MultiSlice;
use argon_util::sync::{Mutex, mutex_lock};
use core::fmt::Debug;
use parse18_runtime::WithLocation;

pub struct SourceEnum {
    pub context: Context,
    decl: Box<ast::EnumDeclarationStmt>,
    pub closure: Box<dyn DeclarationClosure>,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    variants: Mutex<Option<Arc<Vec<Arc<dyn EnumVariant>>>>>,
}

impl SourceEnum {
    pub fn from_ast(
        context: Context,
        closure: Box<dyn DeclarationClosure>,
        decl: Box<ast::EnumDeclarationStmt>,
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
                signature: Mutex::new(None),
                variants: Mutex::new(None),
            }),
        }
    }

    fn return_type_specifier(&self) -> WithLocation<ast::ReturnTypeSpecifier> {
        match &self.decl.return_type {
            Some(return_type) => WithLocation {
                location: return_type.location.clone(),
                value: ast::ReturnTypeSpecifier {
                    return_type: return_type.clone(),
                    ensures_clauses: Vec::new(),
                },
            },
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
}

impl Debug for SourceEnum {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "enum {}", self.decl.name.value)
    }
}

impl Unload for SourceEnum {
    fn unload(&self) {
        *mutex_lock(&self.signature) = None;
        *mutex_lock(&self.variants) = None;
    }
}

impl Enum for SourceEnum {
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

        let scope = self.closure.scope();
        let access_token = self.closure.access_token();
        let owner_ref: Arc<dyn Enum> = self.clone();
        let owner = ExpressionOwner::Enum(owner_ref);
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

    fn variants(self: Arc<Self>) -> Arc<Vec<Arc<dyn EnumVariant>>> {
        let mut variants_store = mutex_lock(&self.variants);
        if let Some(ref variants) = *variants_store {
            return variants.clone();
        }

        let variants = self
            .decl
            .body
            .iter()
            .filter_map(|stmt| match &stmt.value {
                ast::EnumBodyStmt::EnumVariant(variant) => Some(SourceEnumVariant::from_ast(
                    self.clone(),
                    (**variant).clone(),
                )),
                ast::EnumBodyStmt::FunctionDeclaration(_)
                | ast::EnumBodyStmt::MethodDeclaration(_) => None,
            })
            .collect::<Vec<_>>();

        let result = Arc::new(variants);
        *variants_store = Some(result.clone());
        result
    }
}

pub struct SourceEnumVariant {
    pub owner: Arc<SourceEnum>,
    decl: ast::EnumVariant,
    metadata: EnumVariantMetadata,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    fields: Mutex<Option<Arc<Vec<Arc<dyn RecordField>>>>>,
}

impl SourceEnumVariant {
    fn from_ast(owner: Arc<SourceEnum>, decl: ast::EnumVariant) -> Arc<dyn EnumVariant> {
        let modifiers;
        let name;
        match &decl {
            ast::EnumVariant::Constructor {
                modifiers: modifiers2,
                name: name2,
                ..
            } => {
                modifiers = modifiers2;
                name = name2;
            }

            ast::EnumVariant::Record(record) => {
                modifiers = &record.modifiers;
                name = &record.name;
            }
        }

        let metadata = EnumVariantMetadata {
            name: name.value.clone(),
        };

        let modifier_parser =
            ModifierParser::new(owner.context.clone(), &modifiers, &name.location);
        modifier_parser.done();

        Arc::new(Self {
            owner,
            decl,
            metadata,
            signature: Mutex::new(None),
            fields: Mutex::new(None),
        })
    }
}

impl Debug for SourceEnumVariant {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "enum variant {:?}", self.metadata.name)
    }
}

impl Unload for SourceEnumVariant {
    fn unload(&self) {
        *mutex_lock(&self.signature) = None;
        *mutex_lock(&self.fields) = None;
    }
}

impl EnumVariant for SourceEnumVariant {
    fn owning_enum(self: Arc<Self>) -> Arc<dyn Enum> {
        self.owner.clone()
    }

    fn metadata(&self) -> &EnumVariantMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        let mut sig_store = mutex_lock(&self.signature);
        if let Some(ref sig) = *sig_store {
            return sig.clone();
        }

        let name;
        let params;
        let return_type;
        match &self.decl {
            ast::EnumVariant::Constructor {
                name: name2,
                parameters,
                return_type: return_type2,
                ..
            } => {
                name = name2;
                params = parameters.as_slice();
                return_type = return_type2;
            }
            ast::EnumVariant::Record(record) => {
                name = &record.name;
                params = record.parameters.as_slice();
                return_type = &record.return_type;
            }
        }

        let scope = self.owner.closure.scope();
        let access_token = self.owner.closure.access_token();
        let param_owner = ExpressionOwner::<DefaultExprContext>::EnumVariant(self.clone());

        let sig = match return_type {
            Some(return_type) => {
                let rt = SignatureParser::expr_to_return_type(return_type);

                let sig_parser = SignatureParser {
                    context: self.owner.context.clone(),
                    scope: &scope,
                    access_token,
                    owner: param_owner,
                };

                sig_parser.parse(MultiSlice::from(params), &rt)
            }

            None => {
                let fake_rt = SignatureParser::get_type_sig_return_type(name, return_type);

                let sig_parser = SignatureParser {
                    context: self.owner.context.clone(),
                    scope: &scope,
                    access_token,
                    owner: param_owner.clone(),
                };

                let mut all_params = MultiSlice::new();
                all_params.push_slice(&self.owner.decl.parameters);
                all_params.push_slice(params);

                let mut sig = sig_parser.parse(all_params, &fake_rt);
                sig.return_type = Expr::<DefaultExprContext>::EnumType(EnumType {
                    enum_: self.owner.clone(),
                    arguments: SignatureParser::get_parameter_variables(
                        &param_owner,
                        &sig.parameters[0..self.owner.decl.parameters.len()],
                    )
                    .map(|param| Expr::Variable(Variable::Parameter(Box::new(param))))
                    .collect(),
                });

                sig
            }
        };

        let result = Arc::new(sig);
        *sig_store = Some(result.clone());
        result
    }

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>> {
        let mut fields_store = mutex_lock(&self.fields);
        if let Some(ref fields) = *fields_store {
            return fields.clone();
        }

        let body = match &self.decl {
            ast::EnumVariant::Constructor { .. } => MultiSlice::new(),
            ast::EnumVariant::Record(record) => MultiSlice::from(record.body.as_slice()),
        };

        let mut fields: Vec<Arc<dyn RecordField>> = Vec::new();
        for stmt in &body {
            match &stmt.value {
                ast::RecordBodyStmt::RecordField(field) => {
                    fields.push(Arc::new(SourceRecordField::new(
                        SourceRecordFieldOwner::EnumVariant(self.clone()),
                        (**field).clone(),
                    )));
                }

                _ => todo!(),
            }
        }

        let result = Arc::new(fields);
        *fields_store = Some(result.clone());
        result
    }
}
