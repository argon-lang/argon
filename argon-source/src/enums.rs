use crate::method::{MethodClosure, SourceMethod};
use crate::modifiers::{ACCESS_MODIFIER_GLOBAL, ModifierParser};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::record::{SourceRecordField, SourceRecordFieldOwner};
use crate::signature::SignatureParser;
use alloc::borrow::Cow;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::access::AccessToken;
use argon_compiler::erased_sig::{ImportSpecifier, erase_signature};
use argon_compiler::scope::{ParameterScope, Scope};
use argon_compiler::signature::FunctionSignature;
use argon_compiler::vtable::{VTable, build_vtable};
use argon_compiler::{
    Context, DefaultExprContext, Enum, EnumVariant, EnumVariantMetadata, MethodEntry, MethodOwner,
    RecordField, TypeDeclaration, Unload,
};
use argon_expr::{EnumType, Expr, ExprLocationExt, ExpressionOwner, SubstScanner, Variable};
use argon_parser::ast;
use argon_parser::ast::FunctionParameterListType;
use argon_util::{MultiSlice, UnloadCell};
use core::fmt::Debug;
use num_bigint::BigUint;
use parse18_runtime::WithLocation;

pub struct SourceEnum {
    pub context: Context,
    decl: Box<ast::EnumDeclarationStmt>,
    pub closure: Box<dyn DeclarationClosure>,
    signature: UnloadCell<Arc<FunctionSignature<DefaultExprContext>>>,
    variants: UnloadCell<Arc<Vec<Arc<dyn EnumVariant>>>>,
    methods: UnloadCell<Arc<Vec<MethodEntry>>>,
    vtable: UnloadCell<Arc<VTable>>,
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
                signature: UnloadCell::new(),
                variants: UnloadCell::new(),
                methods: UnloadCell::new(),
                vtable: UnloadCell::new(),
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

    pub fn access_token(self: &Arc<Self>) -> AccessToken {
        let mut access_token = self.closure.access_token();
        let enum_ref: Arc<dyn Enum> = self.clone();
        access_token.add_type_permissions(TypeDeclaration::Enum(enum_ref));
        access_token
    }
}

impl Debug for SourceEnum {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "enum {}", self.decl.name.value)
    }
}

impl Unload for SourceEnum {
    fn unload(&self) {
        self.signature.unload();
        self.variants.unload();
        self.methods.unload();
        self.vtable.unload();
        self.closure.unload();
    }
}

impl Enum for SourceEnum {
    fn location(&self) -> parse18_runtime::Location {
        self.decl.name.location.clone()
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
            let owner_ref: Arc<dyn Enum> = self.clone();
            let owner = ExpressionOwner::Enum(owner_ref);
            let return_type = self.return_type_specifier();

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

    fn variants(self: Arc<Self>) -> Arc<Vec<Arc<dyn EnumVariant>>> {
        self.variants.initialize(|| {
            Arc::new(
                self.decl
                    .body
                    .iter()
                    .filter_map(|stmt| match &stmt.value {
                        ast::EnumBodyStmt::EnumVariant(variant) => Some(
                            SourceEnumVariant::from_ast(self.clone(), (**variant).clone()),
                        ),
                        ast::EnumBodyStmt::FunctionDeclaration(_)
                        | ast::EnumBodyStmt::MethodDeclaration(_) => None,
                    })
                    .collect::<Vec<_>>(),
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
                        ast::EnumBodyStmt::MethodDeclaration(method) => {
                            let method_res = SourceMethod::from_ast(
                                self.context.clone(),
                                EnumMethodClosure {
                                    enum_: self.clone(),
                                },
                                (**method).clone(),
                            );
                            Some(MethodEntry {
                                access: method_res.access,
                                method: method_res.result,
                            })
                        }
                        ast::EnumBodyStmt::FunctionDeclaration(_)
                        | ast::EnumBodyStmt::EnumVariant(_) => None,
                    })
                    .collect(),
            )
        })
    }

    fn vtable(self: Arc<Self>) -> Arc<VTable> {
        self.vtable.initialize(|| {
            Arc::new(build_vtable(
                self.context.clone(),
                MethodOwner::Enum(self.clone()),
                self.access_token(),
                self.decl.name.location.clone(),
            ))
        })
    }
}

struct EnumMethodClosure {
    enum_: Arc<SourceEnum>,
}

impl MethodClosure for EnumMethodClosure {
    fn owner(&self) -> MethodOwner {
        MethodOwner::Enum(self.enum_.clone())
    }
    fn scope(&self) -> impl Scope<ExprContext = DefaultExprContext> {
        let enum_ref: Arc<dyn Enum> = self.enum_.clone();
        ParameterScope::new(
            self.enum_.closure.scope(),
            ExpressionOwner::Enum(enum_ref),
            &self.enum_.clone().signature().parameters,
        )
    }
    fn access_token(&self) -> AccessToken {
        self.enum_.access_token()
    }
}

pub struct SourceEnumVariant {
    pub owner: Arc<SourceEnum>,
    decl: ast::EnumVariant,
    metadata: EnumVariantMetadata,
    signature: UnloadCell<Arc<FunctionSignature<DefaultExprContext>>>,
    fields: UnloadCell<Arc<Vec<Arc<dyn RecordField>>>>,
    methods: UnloadCell<Arc<Vec<MethodEntry>>>,
    vtable: UnloadCell<Arc<VTable>>,
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
            signature: UnloadCell::new(),
            fields: UnloadCell::new(),
            methods: UnloadCell::new(),
            vtable: UnloadCell::new(),
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
        self.signature.unload();
        self.fields.unload();
        self.methods.unload();
        self.vtable.unload();
    }
}

impl EnumVariant for SourceEnumVariant {
    fn location(&self) -> parse18_runtime::Location {
        self.metadata_location()
    }
    fn owning_enum(&self) -> Arc<dyn Enum> {
        self.owner.clone()
    }

    fn metadata(&self) -> &EnumVariantMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        self.signature.initialize(|| {
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
            let access_token = self.owner.access_token();
            let param_owner = ExpressionOwner::<DefaultExprContext>::EnumVariant(self.clone());

            Arc::new(match return_type {
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

                    let parent_sig = self.owner.clone().signature();

                    let mut parameters = Vec::new();

                    let parent_owner =
                        ExpressionOwner::<DefaultExprContext>::Enum(self.owner.clone());
                    let owner = ExpressionOwner::<DefaultExprContext>::EnumVariant(self.clone());
                    let mut subst = SubstScanner::new();
                    for (i, mut param) in parent_sig.parameters.iter().cloned().enumerate() {
                        let orig_param_var =
                            param.clone().to_parameter_var(parent_owner.clone(), i);

                        match &mut param.list_type {
                            FunctionParameterListType::NormalList => {
                                param.list_type =
                                    FunctionParameterListType::InferrableList(BigUint::ZERO);
                            }
                            FunctionParameterListType::InferrableList(n) => {
                                *n += 1u32;
                            }
                            FunctionParameterListType::RequiresList => {}
                        }
                        param.scan_mut(&mut subst);

                        let param_var = param.clone().to_parameter_var(owner.clone(), i);

                        subst.add_substitution(
                            Variable::Parameter(Box::new(orig_param_var)),
                            Cow::Owned(
                                Expr::Variable(Variable::Parameter(Box::new(param_var)))
                                    .with_location(self.owner.decl.name.location.clone()),
                            ),
                        );

                        parameters.push(param);
                    }

                    let return_type = Expr::<DefaultExprContext>::EnumType(EnumType {
                        enum_: self.owner.clone(),
                        arguments: parameters
                            .iter()
                            .enumerate()
                            .map(|(i, param)| {
                                Expr::Variable(Variable::Parameter(Box::new(
                                    param.clone().to_parameter_var(param_owner.clone(), i),
                                )))
                                .with_location(self.owner.decl.name.location.clone())
                            })
                            .collect(),
                    })
                    .with_location(self.owner.decl.name.location.clone());

                    let scope = ParameterScope::new(scope, owner, &parameters);

                    let sig_parser = SignatureParser {
                        context: self.owner.context.clone(),
                        scope: &scope,
                        access_token,
                        owner: param_owner.clone(),
                    };

                    let mut variant_sig = sig_parser.parse(MultiSlice::from(params), &fake_rt);
                    parameters.append(&mut variant_sig.parameters);

                    FunctionSignature {
                        parameters,
                        return_type,
                        ensures_clauses: Vec::new(),
                    }
                }
            })
        })
    }

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>> {
        self.fields.initialize(|| {
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

                    ast::RecordBodyStmt::FunctionDeclaration(_)
                    | ast::RecordBodyStmt::MethodDeclaration(_) => {}
                }
            }

            Arc::new(fields)
        })
    }

    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>> {
        self.methods.initialize(|| {
            let methods: Vec<&ast::MethodDeclarationStmt> = match &self.decl {
                ast::EnumVariant::Constructor { body, .. } => body
                    .iter()
                    .map(|stmt| match &stmt.value {
                        ast::EnumVariantBodyStmt::MethodDeclaration(method) => method.as_ref(),
                    })
                    .collect(),
                ast::EnumVariant::Record(record) => record
                    .body
                    .iter()
                    .filter_map(|stmt| match &stmt.value {
                        ast::RecordBodyStmt::MethodDeclaration(method) => Some(method.as_ref()),
                        _ => None,
                    })
                    .collect(),
            };
            Arc::new(
                methods
                    .into_iter()
                    .map(|method| {
                        let method_res = SourceMethod::from_ast(
                            self.owner.context.clone(),
                            EnumVariantMethodClosure {
                                variant: self.clone(),
                            },
                            method.clone(),
                        );
                        MethodEntry {
                            access: method_res.access,
                            method: method_res.result,
                        }
                    })
                    .collect(),
            )
        })
    }

    fn vtable(self: Arc<Self>) -> Arc<VTable> {
        self.vtable.initialize(|| {
            Arc::new(build_vtable(
                self.owner.context.clone(),
                MethodOwner::EnumVariant(self.clone()),
                self.owner.access_token(),
                self.metadata_location(),
            ))
        })
    }
}

impl SourceEnumVariant {
    fn metadata_location(&self) -> parse18_runtime::Location {
        match &self.decl {
            ast::EnumVariant::Constructor { name, .. } => name.location.clone(),
            ast::EnumVariant::Record(record) => record.name.location.clone(),
        }
    }
}

struct EnumVariantMethodClosure {
    variant: Arc<SourceEnumVariant>,
}

impl MethodClosure for EnumVariantMethodClosure {
    fn owner(&self) -> MethodOwner {
        MethodOwner::EnumVariant(self.variant.clone())
    }
    fn scope(&self) -> impl Scope<ExprContext = DefaultExprContext> {
        let variant_ref: Arc<dyn EnumVariant> = self.variant.clone();
        ParameterScope::new(
            self.variant.owner.closure.scope(),
            ExpressionOwner::EnumVariant(variant_ref),
            &self.variant.clone().signature().parameters,
        )
    }
    fn access_token(&self) -> AccessToken {
        self.variant.owner.access_token()
    }
}
