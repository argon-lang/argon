#![no_std]

use argon_expr::ExprScannerMut;
extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

pub mod access;
pub mod erased_sig;
pub mod expr_type;
pub mod platform;
pub mod scanner;
pub mod scope;
pub mod signature;
#[cfg(any(test, feature = "test-utils"))]
pub mod test_utils;

pub use crate::access::AccessModifierGlobal;
use crate::platform::PlatformExtern;
pub use crate::signature::FunctionSignature;
use alloc::{string::String, string::ToString, sync::Arc, vec::Vec};
use argon_expr::{ExprContext, ExpressionOwner, Normalizer, SubstScanner, Variable};
pub use argon_expr::{Builtin, ErasureMode, Expr};
pub use argon_parser::ast::{
    BinaryOperator, BinaryOperatorIdentifier, FunctionParameterListType, Identifier, UnaryOperator,
    UnaryOperatorIdentifier,
};
use argon_util::sync::{
    RwLock, RwLockReadGuard, RwLockWriteGuard, ThreadSafe, rwlock_read, rwlock_write,
};
use argon_util::{CompileError, ErrorReporter, Fuel, InternalCompilerError};
use core::error::Error;
use core::fmt::{Debug, Display, Formatter};
use core::hash::{Hash, Hasher};
use core::str::FromStr;
use std::mem;
use std::prelude::rust_2015::Box;
use esexpr::ESExpr;
use hashbrown::HashMap;
use hashbrown::hash_map::Entry;
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
}

pub type Context = Arc<dyn ContextObject>;

pub struct DefaultExprContext;

impl ExprContext for DefaultExprContext {
    type Hole = EmptyHole;
    type Function = Arc<dyn Function>;
    type Method = Arc<dyn Method>;
    type Record = Arc<dyn Record>;
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
        for module in read_lock(&self.modules).values() {
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
            .entry(path.clone())
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
        for group in read_lock(&self.exports).values() {
            for entry in group {
                match &entry.binding {
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

pub trait Unload {
    fn unload(&self);
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

pub trait Method: Debug + Unload + ThreadSafe {}

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

pub struct RecordFieldMetadata {
    pub is_mutable: bool,
    pub name: Identifier,
}

pub trait Enum: Debug + Unload + ThreadSafe {
    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;
}

pub trait EnumVariant: Debug + Unload + ThreadSafe {}

pub trait Trait: Debug + Unload + ThreadSafe {
    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier;
    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>>;
}

pub trait Instance: Debug + Unload + ThreadSafe {}

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
impl_dyn_stub_traits!(Enum);
impl_dyn_stub_traits!(EnumVariant);
impl_dyn_stub_traits!(Trait);
impl_dyn_stub_traits!(Instance);

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeDeclaration {
    Record(Arc<dyn Record>),
    Enum(Arc<dyn Enum>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
}



pub struct DefaultExprNormalizer;

impl Normalizer<DefaultExprContext> for DefaultExprNormalizer {
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

        for ((parameter_index, parameter), argument) in
            signature.parameters.iter().enumerate().zip(&arguments)
        {
            let variable = Variable::Parameter(Box::new(
                parameter
                    .clone()
                    .to_parameter_var(owner.clone(), parameter_index),
            ));

            subst.add_substitution(variable, argument);
        }

        subst.scan(&mut body);
        Some(body)
    }
}

