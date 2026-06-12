use crate::enums::SourceEnumVariant;
use crate::modifiers::{ACCESS_MODIFIER_GLOBAL, ModifierParser};
use crate::module::{DeclarationClosure, DeclarationResult};
use crate::signature::SignatureParser;
use crate::type_checker::{TypeCheckOptions, type_check_type_expr};
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::erased_sig::{ImportSpecifier, erase_signature};
use argon_compiler::scope::ParameterScope;
use argon_compiler::signature::FunctionSignature;
use argon_compiler::{
    Context, DefaultExprContext, EnumVariant, Record, RecordField, RecordFieldMetadata,
    RecordFieldOwner, Unload,
};
use argon_expr::{Expr, ExpressionOwner};
use argon_parser::ast;
use argon_util::MultiSlice;
use argon_util::sync::{Mutex, mutex_lock};
use core::fmt::Debug;

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

        let scope = self.closure.scope();
        let access_token = self.closure.access_token();
        let owner_ref: Arc<dyn Record> = self.clone();
        let owner: ExpressionOwner<DefaultExprContext> = ExpressionOwner::Record(owner_ref);
        let return_type =
            SignatureParser::get_type_sig_return_type(&self.decl.name, &self.decl.return_type);

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

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>> {
        let mut fields_store = mutex_lock(&self.fields);
        if let Some(ref fields) = *fields_store {
            return fields.clone();
        }

        let fields: Vec<Arc<dyn RecordField>> = self
            .decl
            .body
            .iter()
            .filter_map(|stmt| match &stmt.value {
                ast::RecordBodyStmt::RecordField(field) => Some(Arc::new(SourceRecordField::new(
                    SourceRecordFieldOwner::SourceRecord(self.clone()),
                    (**field).clone(),
                ))
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

pub enum SourceRecordFieldOwner {
    SourceRecord(Arc<SourceRecord>),
    EnumVariant(Arc<SourceEnumVariant>),
}

impl SourceRecordFieldOwner {
    pub fn to_record_field_owner(&self) -> RecordFieldOwner {
        match self {
            SourceRecordFieldOwner::SourceRecord(record) => {
                RecordFieldOwner::Record(record.clone())
            }
            SourceRecordFieldOwner::EnumVariant(variant) => {
                RecordFieldOwner::EnumVariant(variant.clone())
            }
        }
    }

    pub fn to_expression_owner(&self) -> ExpressionOwner<DefaultExprContext> {
        match self {
            SourceRecordFieldOwner::SourceRecord(record) => {
                ExpressionOwner::<DefaultExprContext>::Record(record.clone())
            }
            SourceRecordFieldOwner::EnumVariant(variant) => {
                ExpressionOwner::<DefaultExprContext>::EnumVariant(variant.clone())
            }
        }
    }

    pub fn closure(&self) -> &dyn DeclarationClosure {
        match self {
            SourceRecordFieldOwner::SourceRecord(record) => record.closure.as_ref(),
            SourceRecordFieldOwner::EnumVariant(variant) => variant.owner.closure.as_ref(),
        }
    }

    pub fn context(&self) -> Context {
        match self {
            SourceRecordFieldOwner::SourceRecord(record) => record.context.clone(),
            SourceRecordFieldOwner::EnumVariant(variant) => variant.owner.context.clone(),
        }
    }

    pub fn signature(&self) -> Arc<FunctionSignature<DefaultExprContext>> {
        match self {
            SourceRecordFieldOwner::SourceRecord(record) => record.clone().signature(),
            SourceRecordFieldOwner::EnumVariant(variant) => variant.clone().signature(),
        }
    }
}

pub struct SourceRecordField {
    pub owner: SourceRecordFieldOwner,
    pub field: ast::RecordField,
    pub metadata: RecordFieldMetadata,
    pub field_type: Mutex<Option<Arc<Expr<DefaultExprContext>>>>,
}

impl SourceRecordField {
    pub fn new(owner: SourceRecordFieldOwner, field: ast::RecordField) -> Self {
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
        self.owner.to_record_field_owner()
    }

    fn metadata(&self) -> &RecordFieldMetadata {
        &self.metadata
    }

    fn field_type(self: Arc<Self>) -> Arc<Expr<DefaultExprContext>> {
        let access_token = self.owner.closure().access_token();

        let mut field_type_store = mutex_lock(&self.field_type);
        if let Some(ref field_type) = *field_type_store {
            return field_type.clone();
        }

        let signature = self.owner.signature();
        let scope = self.owner.closure().scope();
        let scope = ParameterScope::new(
            scope,
            self.owner.to_expression_owner(),
            &signature.parameters,
        );

        let field_type = type_check_type_expr(
            self.owner.context().clone(),
            TypeCheckOptions::new(&access_token, &scope),
            &self.field.field_type,
        );

        let result = Arc::new(field_type);
        *field_type_store = Some(result.clone());
        result
    }
}
