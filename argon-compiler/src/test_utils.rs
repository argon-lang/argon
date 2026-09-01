use crate::access::AccessToken;
use crate::platform::PlatformExtern;
use crate::scope::{ImplicitGivens, Lookup, Scope};
use crate::{
    CompileErrorReporter, Context, ContextObject, DefaultExprContext, Enum, EnumVariant,
    EnumVariantMetadata, FunctionSignature, MethodEntry, RecordField, Tube, TubeName, Unload,
    erased_sig,
};
use crate::vtable::VTable;
use alloc::{string::String, sync::Arc, vec::Vec};
use argon_expr::{
    BlockLabel, BlockLabelDeclaration, Expr, ExprLocationExt, LocatedExpr, LoopLabels, Variable,
};
use argon_parser::ast::Identifier;
use argon_util::sync::{Mutex, mutex_lock};
use argon_util::{CompileError, ErrorReporter, Fuel, InternalCompilerError};
use core::fmt::{Debug, Formatter};
use hashbrown::HashMap;
use parse18_runtime::{FilePosition, Location, WithLocation};

#[derive(Clone, Default)]
pub struct TestReporter {
    errors: Arc<Mutex<Vec<CompileError>>>,
}

impl TestReporter {
    pub fn errors(&self) -> Vec<CompileError> {
        mutex_lock(&self.errors).clone()
    }

    fn push_error(&self, error: CompileError) {
        mutex_lock(&self.errors).push(error);
    }
}

impl ErrorReporter<CompileError> for TestReporter {
    fn report_error(&self, error: CompileError) {
        self.push_error(error);
    }
}

impl ErrorReporter<InternalCompilerError> for TestReporter {
    fn report_error(&self, _error: InternalCompilerError) {}
}

pub struct TestContext {
    reporter: TestReporter,
}

impl TestContext {
    pub fn new(reporter: TestReporter) -> Self {
        Self { reporter }
    }
}

impl Default for TestContext {
    fn default() -> Self {
        Self::new(TestReporter::default())
    }
}

impl ContextObject for TestContext {
    fn reporter(&self) -> &dyn CompileErrorReporter {
        &self.reporter
    }

    fn extern_function(&self, _name: &WithLocation<String>) -> PlatformExtern {
        PlatformExtern {
            externs: HashMap::new(),
        }
    }

    fn normalize_fuel(&self) -> Fuel {
        Fuel::new(5)
    }

    fn prolog_fuel(&self) -> Fuel {
        Fuel::new(5)
    }

    fn z3_rlimit(&self) -> u32 {
        100_000
    }
}

impl From<TestContext> for Context {
    fn from(context: TestContext) -> Self {
        Arc::new(context)
    }
}

#[derive(Debug)]
pub struct TestEnum {
    variants: Arc<Vec<Arc<dyn EnumVariant>>>,
}

impl TestEnum {
    pub fn new(variants: Vec<Arc<dyn EnumVariant>>) -> Self {
        Self {
            variants: Arc::new(variants),
        }
    }
}

impl Unload for TestEnum {
    fn unload(&self) {}
}

impl Enum for TestEnum {
    fn location(&self) -> Location { test_location() }
    fn import_specifier(self: Arc<Self>) -> erased_sig::ImportSpecifier {
        panic!("test enum import_specifier should not be called")
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        panic!("test enum signature should not be called")
    }

    fn variants(self: Arc<Self>) -> Arc<Vec<Arc<dyn EnumVariant>>> {
        self.variants.clone()
    }

    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>> { Arc::new(Vec::new()) }
    fn vtable(self: Arc<Self>) -> Arc<VTable> { Arc::new(VTable::empty()) }
}

pub struct TestEnumVariant {
    metadata: EnumVariantMetadata,
}

impl TestEnumVariant {
    pub fn new(name: &str) -> Self {
        Self {
            metadata: EnumVariantMetadata {
                name: Identifier::Named(name.into()),
            },
        }
    }
}

impl Debug for TestEnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("TestEnumVariant")
            .field("name", &self.metadata.name)
            .finish()
    }
}

impl Unload for TestEnumVariant {
    fn unload(&self) {}
}

impl EnumVariant for TestEnumVariant {
    fn location(&self) -> Location { test_location() }
    fn owning_enum(&self) -> Arc<dyn Enum> {
        panic!("test variant owning_enum should not be called")
    }

    fn metadata(&self) -> &EnumVariantMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        Arc::new(FunctionSignature {
            parameters: Vec::new(),
            return_type: Expr::bool_type().with_location(test_location()),
            ensures_clauses: Vec::new(),
        })
    }

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>> {
        Arc::new(Vec::new())
    }

    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>> { Arc::new(Vec::new()) }
    fn vtable(self: Arc<Self>) -> Arc<VTable> { Arc::new(VTable::empty()) }
}

fn test_location() -> Location {
    Location {
        file: Default::default(),
        start: FilePosition { line: 0, column: 0 },
        end: FilePosition { line: 0, column: 0 },
    }
}

pub struct TestScope;

impl Scope for TestScope {
    type ExprContext = DefaultExprContext;
    fn lookup(&self, _: &Identifier, _: &AccessToken) -> Lookup<Self::ExprContext> {
        Lookup::Empty
    }
    fn lookup_assign(&self, _: &Identifier, _: &AccessToken) -> Lookup<Self::ExprContext> {
        Lookup::Empty
    }
    fn lookup_tube(&self, _: &TubeName) -> Option<Arc<Tube>> {
        None
    }
    fn given_assertions(&self, _: &mut dyn ImplicitGivens<ExprContext = Self::ExprContext>) {}
    fn lookup_block_label(
        &self,
        _: &Identifier,
    ) -> Option<BlockLabelDeclaration<Self::ExprContext>> {
        None
    }
    fn latest_loop_labels(&self) -> Option<LoopLabels<Self::ExprContext>> {
        None
    }
    fn latest_block_label(&self) -> Option<BlockLabel<Self::ExprContext>> {
        None
    }
    fn function_result_value_type(&self) -> Option<LocatedExpr<Self::ExprContext>> {
        None
    }
    fn known_variable_values(
        &self,
    ) -> HashMap<Variable<Self::ExprContext>, LocatedExpr<Self::ExprContext>> {
        HashMap::new()
    }
}
