use alloc::{sync::Arc, vec, vec::Vec};
use argon_compiler::{CompileErrorReporter, Context, ContextObject};
use argon_util::sync::{Mutex, mutex_lock};
use argon_util::{CompileError, ErrorReporter, Fuel, InternalCompilerError};
use embedded_io::{Write, WriteFmtError};

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

    fn normalize_fuel(&self) -> Fuel {
        Fuel::new(5)
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
        mutex_lock(&self.compile_errors).clone()
    }

    pub fn print_error_messages<W>(&self, writer: &mut W) -> Result<(), WriteFmtError<W::Error>>
    where
        W: Write,
    {
        print_error_messages(self, writer)
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
    mutex_lock(errors).is_empty()
}

fn push_error<E>(errors: &Mutex<Vec<E>>, error: E) {
    mutex_lock(errors).push(error);
}

fn print_error_messages<W>(
    reporter: &RunnerErrorReporter,
    writer: &mut W,
) -> Result<(), WriteFmtError<W::Error>>
where
    W: Write,
{
    let internal_errors = mutex_lock(&reporter.internal_compiler_errors);
    if !internal_errors.is_empty() {
        for error in internal_errors.iter() {
            writer.write_fmt(format_args!("internal compiler error: {error}\n"))?;
        }

        return Ok(());
    }
    drop(internal_errors);

    let compile_errors = mutex_lock(&reporter.compile_errors);
    for error in compile_errors.iter() {
        writer.write_fmt(format_args!("compile error: {error}\n"))?;
    }

    Ok(())
}
