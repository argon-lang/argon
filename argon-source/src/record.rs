use crate::modifiers::{ACCESS_MODIFIER_GLOBAL, ModifierParser};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::signature::SignatureParser;
use crate::type_checker::type_check_type_expr;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::erased_sig::{ImportSpecifier, erase_signature};
use argon_compiler::scope::ParameterScope;
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{
    Context, DefaultExprContext, Record, RecordField, RecordFieldMetadata, RecordFieldOwner, Unload,
};
use argon_expr::{Expr, ExpressionOwner};
use argon_parser::ast;
use argon_util::sync::{Mutex, mutex_lock};
use core::fmt::Debug;
use parse18_runtime::WithLocation;

pub struct SourceRecord {
    context: Context,
    decl: Box<ast::RecordDeclarationStmt>,
    closure: Box<dyn DeclarationClosure>,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    fields: Mutex<Option<Arc<Vec<Arc<dyn RecordField>>>>>,
}

impl SourceRecord {
    pub fn from_ast(
        context: Context,
        closure: Box<dyn DeclarationClosure>,
        decl: Box<ast::RecordDeclarationStmt>,
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
                fields: Mutex::new(None),
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

impl Debug for SourceRecord {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "record {}", self.decl.name.value)
    }
}

impl Unload for SourceRecord {
    fn unload(&self) {
        *mutex_lock(&self.signature) = None;
        *mutex_lock(&self.fields) = None;
    }
}

impl Record for SourceRecord {
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
        let owner_ref: Arc<dyn Record> = self.clone();
        let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Record(owner_ref);
        let return_type = self.return_type_specifier();

        let sig = SignatureParser {
            context: self.context.clone(),
            scope: &mut scope,
            access_token,
            owner,
        }
        .parse(&self.decl.parameters, &return_type);

        let result = Arc::new(sig);
        *sig_store = Some(result.clone());
        result
    }

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>> {
        let mut fields_store = mutex_lock(&self.fields);
        if let Some(ref fields) = *fields_store {
            return fields.clone();
        }

        let fields: Vec<Arc<dyn RecordField>> =
            self.decl
                .body
                .iter()
                .filter_map(|stmt| match &stmt.value {
                    ast::RecordBodyStmt::RecordField(field) => Some(Arc::new(
                        SourceRecordField::new(self.clone(), (**field).clone()),
                    )
                        as Arc<dyn RecordField>),
                    ast::RecordBodyStmt::FunctionDeclaration(_)
                    | ast::RecordBodyStmt::MethodDeclaration(_) => None,
                })
                .collect();

        let result = Arc::new(fields);
        *fields_store = Some(result.clone());
        result
    }
}

pub struct SourceRecordField {
    owner: Arc<SourceRecord>,
    field: ast::RecordField,
    metadata: RecordFieldMetadata,
    field_type: Mutex<Option<Arc<Expr<DefaultExprContext>>>>,
}

impl SourceRecordField {
    fn new(owner: Arc<SourceRecord>, field: ast::RecordField) -> Self {
        let metadata = RecordFieldMetadata {
            is_mutable: field.is_mutable,
            name: field.name.value.clone(),
        };

        Self {
            owner,
            field,
            metadata,
            field_type: Mutex::new(None),
        }
    }
}

impl Debug for SourceRecordField {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "record field {:?}", self.metadata.name)
    }
}

impl Unload for SourceRecordField {
    fn unload(&self) {
        *mutex_lock(&self.field_type) = None;
    }
}

impl RecordField for SourceRecordField {
    fn owning_record(&self) -> RecordFieldOwner {
        RecordFieldOwner::Record(self.owner.clone())
    }

    fn metadata(&self) -> &RecordFieldMetadata {
        &self.metadata
    }

    fn field_type(self: Arc<Self>) -> Arc<Expr<DefaultExprContext>> {
        let access_token = self.owner.closure.access_token();

        let mut field_type_store = mutex_lock(&self.field_type);
        if let Some(ref field_type) = *field_type_store {
            return field_type.clone();
        }

        let signature = self.owner.clone().signature();
        let mut scope = self.owner.closure.scope();
        let mut scope = ParameterScope::new(
            &mut scope,
            ExpressionOwner::<DefaultExprContext>::Record(self.owner.clone()),
            &signature.parameters,
        );

        let field_type = type_check_type_expr(
            self.owner.context.clone(),
            &access_token,
            &mut scope,
            &self.field.field_type,
        );

        let result = Arc::new(field_type);
        *field_type_store = Some(result.clone());
        result
    }
}
