use crate::Context;
use argon_util::{CompileError, ErrorReporter};
use std::sync::{Arc, Mutex};

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

impl ErrorReporter<std::io::Error> for TestReporter {
    fn report_error(&self, _error: std::io::Error) {}
}

impl ErrorReporter<walkdir::Error> for TestReporter {
    fn report_error(&self, _error: walkdir::Error) {}
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

impl Context for TestContext {
    type Reporter = TestReporter;

    fn reporter(&self) -> &Self::Reporter {
        &self.reporter
    }
}
