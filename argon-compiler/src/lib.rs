pub mod access;
pub mod scope;
pub mod signature;
#[cfg(any(test, feature = "test-utils"))]
pub mod test_utils;

use crate::access::AccessModifierGlobal;
use crate::signature::FunctionSignature;
use argon_expr::{Expr, ExprContext};
use argon_parser::ast::IdentifierExpr;
use argon_util::{CompileError, ErrorReporter};
use dashmap::{DashMap, Entry};
use nonempty_collections::NEVec;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::{Arc, RwLock};

pub trait Context: Sync {
    type Reporter: ErrorReporter<CompileError>
        + ErrorReporter<std::io::Error>
        + ErrorReporter<walkdir::Error>;

    fn reporter(&self) -> &Self::Reporter;
}

pub struct DefaultExprContext;

impl ExprContext for DefaultExprContext {
    type Hole = EmptyHole;
    type Function = Arc<dyn Function>;
    type Method = Arc<dyn Method>;
    type Record = Arc<dyn Record>;
    type Enum = Arc<dyn Enum>;
    type EnumCase = Arc<dyn EnumCase>;
    type Trait = Arc<dyn Trait>;
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ErasureMode {
    Erased,
    Token,
    Concrete,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum EffectInfo {
    Pure,
    Effectful,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum EmptyHole {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TubeName(pub NEVec<String>);

impl Display for TubeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.first())?;
        for s in self.0.iter().skip(1) {
            write!(f, ".{}", s)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModulePath(pub Vec<String>);

impl Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    tubes: DashMap<TubeName, Arc<Tube>>,
}

pub struct TubeCollectionBuilder<'a, C> {
    context: &'a C,
    tube_collection: Arc<TubeCollection>,
}

impl<'a, C: Context> TubeCollectionBuilder<'a, C> {
    pub fn new(context: &'a C) -> Self {
        Self {
            context,
            tube_collection: Arc::new(TubeCollection {
                tubes: DashMap::new(),
            }),
        }
    }

    pub fn tube_collection(&self) -> Arc<TubeCollection> {
        self.tube_collection.clone()
    }

    pub fn add_tube(&self, name: TubeName, referenced_tubes: Vec<TubeName>) -> TubeBuilder {
        let tube = Arc::new(Tube {
            referenced_tubes,
            modules: DashMap::new(),
        });
        let tb = TubeBuilder { tube: tube.clone() };
        match self.tube_collection.tubes.entry(name) {
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

pub struct Tube {
    referenced_tubes: Vec<TubeName>,
    modules: DashMap<ModulePath, Arc<Module>>,
}

pub struct TubeBuilder {
    tube: Arc<Tube>,
}

impl TubeBuilder {
    pub fn tube(&self) -> Arc<Tube> {
        self.tube.clone()
    }

    pub fn module(&self, path: ModulePath) -> ModuleBuilder {
        let module = self
            .tube
            .modules
            .entry(path)
            .or_insert_with(|| {
                Arc::new(Module {
                    exports: DashMap::new(),
                })
            })
            .clone();

        ModuleBuilder { module }
    }
}

pub struct Module {
    exports: DashMap<Option<IdentifierExpr>, RwLock<NEVec<ModuleExportEntry>>>,
}

impl Module {
    pub fn named_exports(&self, name: &Option<IdentifierExpr>) -> Vec<ModuleExportEntry> {
        self.exports
            .get(name)
            .map(|entries| {
                entries
                    .read()
                    .unwrap_or_else(|e| e.into_inner())
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default()
    }
}

pub struct ModuleBuilder {
    module: Arc<Module>,
}

impl ModuleBuilder {
    pub fn module(&self) -> Arc<Module> {
        self.module.clone()
    }

    pub fn add_export(&self, name: Option<IdentifierExpr>, entry: ModuleExportEntry) {
        match self.module.exports.entry(name) {
            Entry::Occupied(ee) => {
                ee.get()
                    .write()
                    .unwrap_or_else(|e| e.into_inner())
                    .push(entry);
            }
            Entry::Vacant(ee) => {
                ee.insert(RwLock::new(NEVec::new(entry)));
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

pub trait Function: Sync + Send {
    fn metadata(&self) -> &FunctionMetadata;

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
    Extern(String),
}

pub trait Method: Sync + Send {}

pub trait Record: Sync + Send {}

pub trait Enum: Sync + Send {}

pub trait EnumCase: Sync + Send {}

pub trait Trait: Sync + Send {}

pub trait Instance: Sync + Send {}

macro_rules! impl_dyn_stub_traits {
    ($trait_name:ident) => {
        impl Debug for dyn $trait_name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($trait_name)).finish()
            }
        }

        impl PartialEq for dyn $trait_name {
            fn eq(&self, other: &Self) -> bool {
                std::ptr::addr_eq(self, other)
            }
        }

        impl Eq for dyn $trait_name {}

        impl Hash for dyn $trait_name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                std::ptr::from_ref(self).hash(state);
            }
        }
    };
}

impl_dyn_stub_traits!(Function);
impl_dyn_stub_traits!(Method);
impl_dyn_stub_traits!(Record);
impl_dyn_stub_traits!(Enum);
impl_dyn_stub_traits!(EnumCase);
impl_dyn_stub_traits!(Trait);
impl_dyn_stub_traits!(Instance);
