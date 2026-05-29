use crate::platform::PlatformExtern;
use crate::{CompileErrorReporter, Context, ContextObject};
use alloc::{string::String, sync::Arc, vec::Vec};
use argon_util::sync::{Mutex, mutex_lock};
use argon_util::{CompileError, ErrorReporter, Fuel, InternalCompilerError};
use hashbrown::HashMap;
use parse18_runtime::WithLocation;

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
}

impl From<TestContext> for Context {
    fn from(context: TestContext) -> Self {
        Arc::new(context)
    }
}
