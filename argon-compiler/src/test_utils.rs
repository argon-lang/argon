use crate::{CompileErrorReporter, Context, ContextObject};
use argon_util::sync::Mutex;
use argon_util::{CompileError, ErrorReporter, Fuel, InternalCompilerError};
use std::sync::Arc;

#[derive(Clone, Default)]
pub struct TestReporter {
    errors: Arc<Mutex<Vec<CompileError>>>,
}

impl TestReporter {
    pub fn errors(&self) -> Vec<CompileError> {
        match self.errors.lock() {
            Ok(errors) => errors.clone(),
            Err(poisoned) => poisoned.into_inner().clone(),
        }
    }

    fn push_error(&self, error: CompileError) {
        match self.errors.lock() {
            Ok(mut errors) => errors.push(error),
            Err(poisoned) => poisoned.into_inner().push(error),
        }
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

    fn normalize_fuel(&self) -> Fuel {
        Fuel::new(5)
    }
}

impl From<TestContext> for Context {
    fn from(context: TestContext) -> Self {
        Arc::new(context)
    }
}
