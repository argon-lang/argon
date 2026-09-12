use alloc::{string::String, sync::Arc, vec, vec::Vec};
use argon_compiler::platform::{ExternType, PlatformExtern, PlatformMetadata};
use argon_compiler::{CompileErrorReporter, Context, ContextObject};
use argon_util::sync::{Mutex, mutex_lock};
use argon_util::{CompileError, ErrorReporter, Fuel, InternalCompilerError};
use embedded_io::{Write, WriteFmtError};
use esexpr::ESExpr;
use hashbrown::{HashMap, hash_map};
use parse18_runtime::WithLocation;

pub struct RunnerContext {
    reporter: RunnerErrorReporter,
    platform_metadata: HashMap<String, PlatformMetadata>,
}

impl RunnerContext {
    pub fn new() -> Self {
        Self {
            reporter: RunnerErrorReporter {
                compile_errors: Mutex::new(vec![]),
                internal_compiler_errors: Mutex::new(vec![]),
            },
            platform_metadata: HashMap::new(),
        }
    }

    pub fn runner_reporter(&self) -> &RunnerErrorReporter {
        &self.reporter
    }

    pub fn add_platform(&mut self, platform: String, platform_metadata: PlatformMetadata) {
        match self.platform_metadata.entry(platform) {
            hash_map::Entry::Occupied(oe) => {
                self.reporter
                    .report_error(CompileError::duplicate_platform(oe.key().clone()));
            }
            hash_map::Entry::Vacant(entry) => {
                entry.insert(platform_metadata);
            }
        }
    }

    pub fn tube_platform_metadata(&self) -> HashMap<String, ESExpr<'static>> {
        self.platform_metadata
            .iter()
            .map(|(platform, metadata)| (platform.clone(), metadata.metadata.clone()))
            .collect()
    }

    fn extern_of_type(&self, name: &WithLocation<String>, expected: ExternType) -> PlatformExtern {
        if self.platform_metadata.is_empty() {
            self.reporter
                .report_error(CompileError::platform_independent_extern(
                    name.location.clone(),
                    &name.value,
                ));
            return PlatformExtern {
                externs: HashMap::new(),
            };
        }

        let externs = self
            .platform_metadata
            .iter()
            .filter_map(|(platform, metadata)| {
                let Some(ext) = metadata.externs.get(&name.value) else {
                    self.reporter
                        .report_error(CompileError::unknown_platform_extern(
                            name.location.clone(),
                            platform,
                            &name.value,
                        ));
                    return None;
                };
                if ext.extern_type != expected {
                    self.reporter
                        .report_error(CompileError::invalid_platform_extern_type(
                            name.location.clone(),
                            platform,
                            &name.value,
                            expected.name(),
                            ext.extern_type.name(),
                        ));
                    return None;
                }
                Some((platform.clone(), ext.implementation.clone()))
            })
            .collect();
        PlatformExtern { externs }
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

    fn extern_function(&self, name: &WithLocation<String>) -> PlatformExtern {
        self.extern_of_type(name, ExternType::Function)
    }

    fn extern_method(&self, name: &WithLocation<String>) -> PlatformExtern {
        self.extern_of_type(name, ExternType::Method)
    }

    fn extern_static_method(&self, name: &WithLocation<String>) -> PlatformExtern {
        self.extern_of_type(name, ExternType::StaticMethod)
    }

    fn normalize_fuel(&self) -> Fuel {
        Fuel::new(5)
    }

    fn prolog_fuel(&self) -> Fuel {
        Fuel::new(10)
    }

    fn z3_rlimit(&self) -> u32 {
        100000
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
