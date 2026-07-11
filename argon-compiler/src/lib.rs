use argon_expr::{ExprScannerMut, TypeComparer, Unify};
extern crate alloc;
pub mod access;
pub mod erased_sig;
pub mod erasure;
pub mod expr_type;
mod implicits;
pub mod platform;
pub mod scanner;
pub mod scope;
pub mod shifter;
pub mod signature;
#[cfg(any(test, feature = "test-utils"))]
pub mod test_utils;
pub mod vtable;
pub mod z3expr;

pub use crate::access::AccessModifierGlobal;
use crate::erased_sig::ImportSpecifier;
pub use crate::implicits::ImplicitValue;
use crate::platform::PlatformExtern;
pub use crate::signature::{FunctionSignature, SubstFunctionSignature};
use alloc::{string::String, string::ToString, sync::Arc, vec::Vec};
pub use argon_expr::{Builtin, ErasureMode, Expr};
use argon_expr::{ExprContext, ExpressionOwner, Normalizer, SubstScanner};
pub use argon_parser::ast::{
    BinaryOperator, BinaryOperatorIdentifier, FunctionParameterListType, Identifier, UnaryOperator,
    UnaryOperatorIdentifier,
};
pub use argon_util::Unload;
use argon_util::sync::{
    RwLock, RwLockReadGuard, RwLockWriteGuard, ThreadSafe, rwlock_read, rwlock_write,
};
use argon_util::{CompileError, ErrorReporter, Fuel, InternalCompilerError};
use core::error::Error;
use core::fmt::{Debug, Display, Formatter};
use core::hash::{Hash, Hasher};
use core::mem;
use core::str::FromStr;
use esexpr::ESExpr;
use hashbrown::hash_map::Entry;
use hashbrown::{Equivalent, HashMap};
use mitsein::vec1::Vec1;
use parse18_runtime::WithLocation;

pub trait CompileErrorReporter:
    ErrorReporter<CompileError> + ErrorReporter<InternalCompilerError>
{
}

impl<R> CompileErrorReporter for R where
    R: ErrorReporter<CompileError> + ErrorReporter<InternalCompilerError>
{
}

fn read_lock<T>(lock: &RwLock<T>) -> RwLockReadGuard<'_, T> {
    rwlock_read(lock)
}

fn write_lock<T>(lock: &RwLock<T>) -> RwLockWriteGuard<'_, T> {
    rwlock_write(lock)
}

pub trait ContextObject: ThreadSafe {
    fn reporter(&self) -> &dyn CompileErrorReporter;

    fn extern_function(&self, name: &WithLocation<String>) -> PlatformExtern;

    fn normalize_fuel(&self) -> Fuel;

    fn prolog_fuel(&self) -> Fuel;

    fn z3_rlimit(&self) -> u32;
}

pub type Context = Arc<dyn ContextObject>;

pub struct DefaultExprContext;

impl ExprContext for DefaultExprContext {
    type Hole = EmptyHole;
    type Function = Arc<dyn Function>;
    type Method = Arc<dyn Method>;
    type Record = Arc<dyn Record>;
    type RecordField = Arc<dyn RecordField>;
    type Enum = Arc<dyn Enum>;
    type EnumVariant = Arc<dyn EnumVariant>;
    type Trait = Arc<dyn Trait>;
    type Instance = Arc<dyn Instance>;
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum EffectInfo {
    Pure,
    Effectful,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum EmptyHole {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TubeName(pub Vec1<String>);

#[derive(Debug)]
pub enum TubeNameParseError {
    Empty,
    EmptySegment,
    InvalidSegment(core::str::Utf8Error),
}

impl Display for TubeNameParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            TubeNameParseError::Empty => write!(f, "empty tube name"),
            TubeNameParseError::EmptySegment => write!(f, "empty segment in tube name"),
            TubeNameParseError::InvalidSegment(e) => {
                write!(f, "invalid segment in tube name: {}", e)
            }
        }
    }
}

impl Error for TubeNameParseError {}

impl From<core::str::Utf8Error> for TubeNameParseError {
    fn from(e: core::str::Utf8Error) -> Self {
        TubeNameParseError::InvalidSegment(e)
    }
}

impl FromStr for TubeName {
    type Err = TubeNameParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split('.')
            .map(|s| {
                let part = percent_encoding::percent_decode_str(s).decode_utf8()?;
                if part.is_empty() {
                    return Err(TubeNameParseError::EmptySegment);
                }

                Ok(part.into_owned())
            })
            .collect::<Result<Vec<String>, _>>()
            .and_then(|parts| Vec1::try_from(parts).map_err(|_| TubeNameParseError::Empty))
            .map(TubeName)
    }
}

impl Display for TubeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0.first())?;
        for s in self.0.iter().skip(1) {
            write!(f, ".{}", s)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModulePath(pub Vec<String>);

impl Display for ModulePath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for (i, s) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, "/")?;
            }
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

pub struct TubeCollection {
    tubes: RwLock<HashMap<TubeName, Arc<Tube>>>,
}

impl TubeCollection {
    pub fn tube(&self, name: &TubeName) -> Option<Arc<Tube>> {
        read_lock(&self.tubes).get(name).cloned()
    }
}

pub struct TubeCollectionBuilder {
    context: Context,
    tube_collection: Arc<TubeCollection>,
}

impl TubeCollectionBuilder {
    pub fn new(context: Context) -> Self {
        Self {
            context,
            tube_collection: Arc::new(TubeCollection {
                tubes: RwLock::new(HashMap::new()),
            }),
        }
    }

    pub fn tube_collection(&self) -> Arc<TubeCollection> {
        self.tube_collection.clone()
    }

    pub fn add_tube(
        &self,
        name: TubeName,
        metadata: TubeMetadata,
        referenced_tubes: Vec<TubeName>,
    ) -> TubeBuilder {
        let tube = Arc::new(Tube {
            name: name.clone(),
            metadata,
            referenced_tubes,
            modules: RwLock::new(HashMap::new()),
        });
        let tb = TubeBuilder { tube: tube.clone() };
        match write_lock(&self.tube_collection.tubes).entry(name) {
            Entry::Occupied(oe) => {
                self.context
                    .reporter()
                    .report_error(CompileError::duplicate_tube_definition(
                        oe.key().to_string(),
                    ))
            }
            Entry::Vacant(ve) => {
                ve.insert(tube.clone());
            }
        }
        tb
    }
}

impl Unload for TubeCollection {
    fn unload(&self) {
        for (_, tube) in core::mem::take(&mut *write_lock(&self.tubes)) {
            tube.unload();
        }
    }
}

pub struct Tube {
    name: TubeName,
    metadata: TubeMetadata,
    referenced_tubes: Vec<TubeName>,
    modules: RwLock<HashMap<ModulePath, Arc<Module>>>,
}

impl Tube {
    pub fn name(&self) -> &TubeName {
        &self.name
    }

    pub fn metadata(&self) -> &TubeMetadata {
        &self.metadata
    }

    pub fn referenced_tubes(&self) -> &[TubeName] {
        &self.referenced_tubes
    }

    pub fn modules(&self) -> RwLockReadGuard<'_, HashMap<ModulePath, Arc<Module>>> {
        read_lock(&self.modules)
    }

    pub fn module(&self, path: &ModulePath) -> Option<Arc<Module>> {
        read_lock(&self.modules)
            .get(path)
            .map(|entry| entry.clone())
    }
}

impl PartialEq for Tube {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }
}

impl Eq for Tube {}

impl Hash for Tube {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const Tube as *const ()).hash(state);
    }
}

impl Unload for Tube {
    fn unload(&self) {
        for (_, module) in core::mem::take(&mut *write_lock(&self.modules)) {
            module.unload();
        }
    }
}

pub struct TubeMetadata {
    pub platform: HashMap<String, ESExpr<'static>>,
}

pub struct TubeBuilder {
    tube: Arc<Tube>,
}

impl TubeBuilder {
    pub fn tube(&self) -> Arc<Tube> {
        self.tube.clone()
    }

    pub fn module(&self, path: ModulePath) -> ModuleBuilder {
        let module = write_lock(&self.tube.modules)
            .entry_ref(&path)
            .or_insert_with(|| {
                Arc::new(Module {
                    tube: self.tube.name().clone(),
                    path: path.clone(),
                    exports: RwLock::new(HashMap::new()),
                })
            })
            .clone();

        ModuleBuilder { module }
    }
}

pub struct Module {
    tube: TubeName,
    path: ModulePath,
    exports: RwLock<HashMap<Identifier, Vec1<ModuleExportEntry>>>,
}

impl Module {
    pub fn tube(&self) -> &TubeName {
        &self.tube
    }

    pub fn path(&self) -> &ModulePath {
        &self.path
    }

    pub fn export_groups(
        &self,
    ) -> RwLockReadGuard<'_, HashMap<Identifier, Vec1<ModuleExportEntry>>> {
        read_lock(&self.exports)
    }
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }
}

impl Eq for Module {}

impl Hash for Module {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const Module as *const ()).hash(state);
    }
}

impl Unload for Module {
    fn unload(&self) {
        for (_, group) in core::mem::take(&mut *write_lock(&self.exports)) {
            for entry in group {
                match entry.binding {
                    ModuleExportBinding::Function(f) => f.unload(),
                    ModuleExportBinding::Record(r) => r.unload(),
                    ModuleExportBinding::Enum(e) => e.unload(),
                    ModuleExportBinding::Trait(t) => t.unload(),
                    ModuleExportBinding::Instance(i) => i.unload(),
                }
            }
        }
    }
}

pub struct ModuleBuilder {
    module: Arc<Module>,
}

impl ModuleBuilder {
    pub fn module(&self) -> Arc<Module> {
        self.module.clone()
    }

    pub fn add_export(&self, name: Identifier, entry: ModuleExportEntry) {
        match write_lock(&self.module.exports).entry(name) {
            Entry::Occupied(mut ee) => {
                ee.get_mut().push(entry);
            }
            Entry::Vacant(ee) => {
                ee.insert(Vec1::from_one(entry));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleExportEntry {
    pub access: AccessModifierGlobal,
    pub binding: ModuleExportBinding,
    pub is_reexport: bool,
}

#[derive(Debug, Clone)]
pub enum ModuleExportBinding {
    Function(Arc<dyn Function>),
    Record(Arc<dyn Record>),
    Enum(Arc<dyn Enum>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
}

pub trait Function: Debug + Unload + ThreadSafe {
    fn metadata(&self) -> &FunctionMetadata;

    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;
    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>>;
}

pub struct FunctionMetadata {
    pub is_inline: bool,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
    pub effect_info: EffectInfo,
}

pub enum FunctionImplementation {
    Expr(Expr<DefaultExprContext>),
    Extern(PlatformExtern),
}

impl Unload for FunctionImplementation {
    fn unload(&self) {}
}

pub trait Method: Debug + Unload + ThreadSafe {
    fn owner(self: Arc<Self>) -> MethodOwner;
    fn metadata(&self) -> &MethodMetadata;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;
    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>>;
}

pub struct MethodMetadata {
    pub access: access::AccessModifier,
    pub name: Identifier,
    pub is_abstract: bool,
    pub is_inline: bool,
    pub erasure_mode: ErasureMode,
    pub is_witness: bool,
    pub slot: MethodSlot,
    pub effect_info: EffectInfo,
    pub instance_parameter: MethodInstanceParameter,
}

#[derive(Clone)]
pub enum MethodOwner {
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
}
impl MethodOwner {
    pub fn import_specifier(self) -> ImportSpecifier {
        match self {
            MethodOwner::Trait(t) => t.import_specifier(),
            MethodOwner::Instance(i) => i.import_specifier(),
        }
    }
    pub fn into_expression_owner(self) -> ExpressionOwner<DefaultExprContext> {
        match self {
            MethodOwner::Trait(trait_) => ExpressionOwner::Trait(trait_),
            MethodOwner::Instance(instance) => ExpressionOwner::Instance(instance),
        }
    }

    pub fn signature(&self) -> Arc<FunctionSignature<DefaultExprContext>> {
        match self {
            MethodOwner::Trait(trait_) => trait_.clone().signature(),
            MethodOwner::Instance(instance) => instance.clone().signature(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodSlot {
    Abstract,
    AbstractOverride,
    Virtual,
    Override,
    Final,
    FinalOverride,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodInstanceParameter {
    pub name: Option<Identifier>,
}

#[derive(Clone)]
pub struct MethodEntry {
    pub access: access::AccessModifier,
    pub method: Arc<dyn Method>,
}

impl Unload for MethodEntry {
    fn unload(&self) {
        self.method.unload();
    }
}

pub trait Record: Debug + Unload + ThreadSafe {
    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>>;
}

pub trait RecordField: Debug + Unload + ThreadSafe {
    fn owning_record(&self) -> RecordFieldOwner;
    fn metadata(&self) -> &RecordFieldMetadata;
    fn field_type(self: Arc<Self>) -> Arc<Expr<DefaultExprContext>>;
}

pub enum RecordFieldOwner {
    Record(Arc<dyn Record>),
    EnumVariant(Arc<dyn EnumVariant>),
}

impl RecordFieldOwner {
    pub fn import_specifier(self) -> ImportSpecifier {
        match self {
            RecordFieldOwner::Record(r) => r.import_specifier(),
            RecordFieldOwner::EnumVariant(e) => e.owning_enum().import_specifier(),
        }
    }
}

pub struct RecordFieldMetadata {
    pub is_mutable: bool,
    pub name: Identifier,
}

pub trait Enum: Debug + Unload + ThreadSafe {
    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;

    fn variants(self: Arc<Self>) -> Arc<Vec<Arc<dyn EnumVariant>>>;
}

pub trait EnumVariant: Debug + Unload + ThreadSafe {
    fn owning_enum(&self) -> Arc<dyn Enum>;
    fn metadata(&self) -> &EnumVariantMetadata;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;
    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>>;
}

pub struct EnumVariantMetadata {
    pub name: Identifier,
}

pub trait Trait: Debug + Unload + ThreadSafe {
    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;
    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>>;
    fn vtable(self: Arc<Self>) -> Arc<vtable::VTable>;
}

pub trait Instance: Debug + Unload + ThreadSafe {
    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier;
    fn erasure_mode(&self) -> ErasureMode;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;
    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>>;
    fn vtable(self: Arc<Self>) -> Arc<vtable::VTable>;
}

macro_rules! impl_dyn_stub_traits {
    ($trait_name:ident) => {
        impl PartialEq for dyn $trait_name {
            fn eq(&self, other: &Self) -> bool {
                core::ptr::addr_eq(self, other)
            }
        }

        impl Eq for dyn $trait_name {}

        impl Hash for dyn $trait_name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                (self as *const dyn $trait_name as *const ()).hash(state);
            }
        }
    };
}

impl_dyn_stub_traits!(Function);
impl_dyn_stub_traits!(Method);
impl_dyn_stub_traits!(Record);
impl_dyn_stub_traits!(RecordField);
impl_dyn_stub_traits!(Enum);
impl_dyn_stub_traits!(EnumVariant);
impl_dyn_stub_traits!(Trait);
impl_dyn_stub_traits!(Instance);

const DECL_HASH_FUNCTION: u8 = 0;
const DECL_HASH_RECORD: u8 = 1;
const DECL_HASH_ENUM: u8 = 2;
const DECL_HASH_TRAIT: u8 = 3;
const DECL_HASH_INSTANCE: u8 = 4;
const DECL_HASH_METHOD: u8 = 5;
const DECL_HASH_RECORD_FIELD: u8 = 6;
const DECL_HASH_ENUM_VARIANT: u8 = 7;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeDeclaration {
    Record(Arc<dyn Record>),
    Enum(Arc<dyn Enum>),
    EnumVariant(Arc<dyn EnumVariant>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
}

impl TypeDeclaration {
    pub fn signature(self) -> Arc<FunctionSignature<DefaultExprContext>> {
        match self {
            Self::Record(r) => r.signature(),
            Self::Enum(e) => e.signature(),
            Self::EnumVariant(v) => v.signature(),
            Self::Trait(t) => t.signature(),
            Self::Instance(i) => i.signature(),
        }
    }

    pub fn into_expression_owner(self) -> ExpressionOwner<DefaultExprContext> {
        match self {
            Self::Record(r) => ExpressionOwner::Record(r),
            Self::Enum(e) => ExpressionOwner::Enum(e),
            Self::EnumVariant(v) => ExpressionOwner::EnumVariant(v),
            Self::Trait(t) => ExpressionOwner::Trait(t),
            Self::Instance(i) => ExpressionOwner::Instance(i),
        }
    }
}

impl From<RecordFieldOwner> for TypeDeclaration {
    fn from(owner: RecordFieldOwner) -> Self {
        match owner {
            RecordFieldOwner::Record(r) => TypeDeclaration::Record(r),
            RecordFieldOwner::EnumVariant(e) => TypeDeclaration::Enum(e.owning_enum()),
        }
    }
}

impl From<MethodOwner> for TypeDeclaration {
    fn from(owner: MethodOwner) -> Self {
        match owner {
            MethodOwner::Trait(t) => TypeDeclaration::Trait(t),
            MethodOwner::Instance(i) => TypeDeclaration::Instance(i),
        }
    }
}

impl Hash for TypeDeclaration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TypeDeclaration::Record(r) => {
                DECL_HASH_RECORD.hash(state);
                r.hash(state);
            }
            TypeDeclaration::Enum(e) => {
                DECL_HASH_ENUM.hash(state);
                e.hash(state);
            }
            TypeDeclaration::EnumVariant(v) => {
                DECL_HASH_ENUM_VARIANT.hash(state);
                v.hash(state);
            }
            TypeDeclaration::Trait(t) => {
                DECL_HASH_TRAIT.hash(state);
                t.hash(state);
            }
            TypeDeclaration::Instance(i) => {
                DECL_HASH_INSTANCE.hash(state);
                i.hash(state);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration {
    Function(Arc<dyn Function>),
    Method(Arc<dyn Method>),
    Record(Arc<dyn Record>),
    RecordField(Arc<dyn RecordField>),
    Enum(Arc<dyn Enum>),
    EnumVariant(Arc<dyn EnumVariant>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
}

impl Declaration {
    pub fn closest_type_declaration(self) -> Option<TypeDeclaration> {
        match self {
            Self::Function(_) => None,
            Self::Method(m) => Some(TypeDeclaration::from(m.owner())),
            Self::Record(r) => Some(TypeDeclaration::Record(r)),
            Self::RecordField(f) => Some(TypeDeclaration::from(f.owning_record())),
            Self::Enum(e) => Some(TypeDeclaration::Enum(e)),
            Self::EnumVariant(v) => Some(TypeDeclaration::Enum(v.owning_enum())),
            Self::Trait(t) => Some(TypeDeclaration::Trait(t)),
            Self::Instance(i) => Some(TypeDeclaration::Instance(i)),
        }
    }

    pub fn closest_import_specifier(self) -> ImportSpecifier {
        match self {
            Declaration::Function(f) => f.import_specifier(),
            Declaration::Method(m) => m.owner().import_specifier(),
            Declaration::Record(r) => r.import_specifier(),
            Declaration::RecordField(f) => f.owning_record().import_specifier(),
            Declaration::Enum(e) => e.import_specifier(),
            Declaration::EnumVariant(v) => v.owning_enum().import_specifier(),
            Declaration::Trait(t) => t.import_specifier(),
            Declaration::Instance(i) => i.import_specifier(),
        }
    }
}

impl Hash for Declaration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Declaration::Function(f) => {
                DECL_HASH_FUNCTION.hash(state);
                f.hash(state);
            }
            Declaration::Method(m) => {
                DECL_HASH_METHOD.hash(state);
                m.hash(state);
            }
            Declaration::Record(r) => {
                DECL_HASH_RECORD.hash(state);
                r.hash(state);
            }
            Declaration::RecordField(f) => {
                DECL_HASH_RECORD_FIELD.hash(state);
                f.hash(state);
            }
            Declaration::Enum(e) => {
                DECL_HASH_ENUM.hash(state);
                e.hash(state);
            }
            Declaration::EnumVariant(v) => {
                DECL_HASH_ENUM_VARIANT.hash(state);
                v.hash(state);
            }
            Declaration::Trait(t) => {
                DECL_HASH_TRAIT.hash(state);
                t.hash(state);
            }
            Declaration::Instance(i) => {
                DECL_HASH_INSTANCE.hash(state);
                i.hash(state);
            }
        }
    }
}

impl Equivalent<TypeDeclaration> for Declaration {
    fn equivalent(&self, key: &TypeDeclaration) -> bool {
        match (self, key) {
            (Declaration::Record(decl), TypeDeclaration::Record(key)) => decl == key,
            (Declaration::Enum(decl), TypeDeclaration::Enum(key)) => decl == key,
            (Declaration::Trait(decl), TypeDeclaration::Trait(key)) => decl == key,
            (Declaration::Instance(decl), TypeDeclaration::Instance(key)) => decl == key,
            _ => false,
        }
    }
}

impl Equivalent<Declaration> for TypeDeclaration {
    fn equivalent(&self, key: &Declaration) -> bool {
        key.equivalent(self)
    }
}

impl From<ModuleExportBinding> for Declaration {
    fn from(binding: ModuleExportBinding) -> Self {
        match binding {
            ModuleExportBinding::Function(f) => Declaration::Function(f),
            ModuleExportBinding::Record(r) => Declaration::Record(r),
            ModuleExportBinding::Enum(e) => Declaration::Enum(e),
            ModuleExportBinding::Trait(t) => Declaration::Trait(t),
            ModuleExportBinding::Instance(i) => Declaration::Instance(i),
        }
    }
}

pub struct DefaultExprNormalizer;

impl Normalizer for DefaultExprNormalizer {
    type EC = DefaultExprContext;

    fn resolve_hole(&mut self, hole: &<Self::EC as ExprContext>::Hole) -> Option<Expr<Self::EC>> {
        match *hole {}
    }

    fn get_function_body(
        &mut self,
        function: &Arc<dyn Function>,
        arguments: &mut Vec<Expr<DefaultExprContext>>,
    ) -> Option<Expr<DefaultExprContext>> {
        if !function.metadata().is_inline {
            return None;
        }

        let implementation = function.clone().implementation()?;
        let FunctionImplementation::Expr(body) = implementation.as_ref() else {
            return None;
        };

        let signature = function.clone().signature();
        let owner = ExpressionOwner::Function(function.clone());
        let mut body = body.clone();
        let arguments = mem::take(arguments);
        let mut subst = SubstScanner::new();
        subst.add_function_parameter_substitutions(owner, &signature, &arguments);
        subst.scan(&mut body);
        Some(body)
    }
}

pub struct DefaultExprComparer {
    context: Context,
    model: (),
}

impl DefaultExprComparer {
    pub fn new(context: Context) -> Self {
        Self { context, model: () }
    }
}

impl Unify for DefaultExprComparer {
    type EC = DefaultExprContext;
    type Model = ();

    type Norm<'a>
        = DefaultExprNormalizer
    where
        Self: 'a;

    fn model(&self) -> &Self::Model {
        &self.model
    }

    fn model_mut(&mut self) -> &mut Self::Model {
        &mut self.model
    }

    fn normalize_fuel(&self) -> Fuel {
        self.context.normalize_fuel()
    }

    fn normalizer<'a>(_model: &'a mut Self::Model) -> Self::Norm<'a>
    where
        Self: 'a,
    {
        DefaultExprNormalizer
    }

    fn unify_hole(&mut self, a: <Self::EC as ExprContext>::Hole, _b: Expr<Self::EC>) -> bool {
        match a {}
    }
}

impl TypeComparer for DefaultExprComparer {}
