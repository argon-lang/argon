use argon_compiler::{CompileErrorReporter, Context, ContextObject};
use argon_util::{CompileError, ErrorReporter, InternalCompilerError};
use std::sync::{Arc, Mutex};

pub struct RunnerContext {
    reporter: RunnerErrorReporter,
}

impl RunnerContext {
    pub fn new() -> Self {
        Self {
            reporter: RunnerErrorReporter {
                compile_errors: Mutex::new(vec![]),
                internal_compiler_errors: Mutex::new(vec![]),
            },
        }
    }

    pub fn runner_reporter(&self) -> &RunnerErrorReporter {
        &self.reporter
    }
}

impl Default for RunnerContext {
    fn default() -> Self {
        Self::new()
    }
}

impl ContextObject for RunnerContext {
    fn reporter(&self) -> &dyn CompileErrorReporter {
        &self.reporter
    }
}

impl From<RunnerContext> for Context {
    fn from(context: RunnerContext) -> Self {
        Arc::new(context)
    }
}

pub struct RunnerErrorReporter {
    compile_errors: Mutex<Vec<CompileError>>,
    internal_compiler_errors: Mutex<Vec<InternalCompilerError>>,
}

impl RunnerErrorReporter {
    pub fn compile_errors(&self) -> Vec<CompileError> {
        match self.compile_errors.lock() {
            Ok(errors) => errors.clone(),
            Err(poisoned) => poisoned.into_inner().clone(),
        }
    }

    pub fn has_errors(&self) -> bool {
        !is_empty(&self.compile_errors) || !is_empty(&self.internal_compiler_errors)
    }
}

impl ErrorReporter<CompileError> for RunnerErrorReporter {
    fn report_error(&self, error: CompileError) {
        push_error(&self.compile_errors, error);
    }
}

impl ErrorReporter<InternalCompilerError> for RunnerErrorReporter {
    fn report_error(&self, error: InternalCompilerError) {
        push_error(&self.internal_compiler_errors, error);
    }
}

fn is_empty<E>(errors: &Mutex<Vec<E>>) -> bool {
    match errors.lock() {
        Ok(errors) => errors.is_empty(),
        Err(poisoned) => poisoned.into_inner().is_empty(),
    }
}

fn push_error<E>(errors: &Mutex<Vec<E>>, error: E) {
    match errors.lock() {
        Ok(mut errors) => errors.push(error),
        Err(poisoned) => poisoned.into_inner().push(error),
    }
}
