mod backend_options;
mod cli;
mod direct;
mod staging;

use crate::CompileTargetPlatform;
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile};
use argon_tasks::{
    CompileOptions, GenIrOptions, OptimizeOptions,
    message::{TaskLogger, TaskMessage, TextTaskLogger},
};
use argon_util::sync::ThreadSafe;

pub use cli::CliCommandRunner;
pub use direct::DirectCommandRunner;

#[derive(Debug, Default)]
pub struct TaskMessageLog {
    messages: Vec<TaskMessage>,
}

impl TaskLogger for TaskMessageLog {
    fn log(&mut self, message: TaskMessage) {
        self.messages.push(message);
    }
}

impl TaskMessageLog {
    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }

    pub fn contains_compile_error(&self, expected: &str) -> bool {
        self.messages.iter().any(|message| match message {
            TaskMessage::CompileError { error } => {
                error.message.contains(expected)
                    || format!("AR{:04X}", error.code).contains(expected)
            }
            TaskMessage::TaskError { .. } => false,
        })
    }
}

impl std::fmt::Display for TaskMessageLog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = Vec::new();
        {
            let mut output = argon_tasks::local_io::StdIoWrite::new(&mut output);
            let mut logger = TextTaskLogger::new(&mut output);
            for message in &self.messages {
                logger.log(message.clone());
            }
        }
        f.write_str(&String::from_utf8_lossy(&output))
    }
}

pub trait CommandRunner {
    fn compile<ID, IF, O>(
        &self,
        options: CompileOptions<ID, IF, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        ID: InputDirectory + ThreadSafe,
        ID::File: ThreadSafe,
        IF: InputFile + ThreadSafe,
        O: OutputFile;

    fn gen_ir<IF, O>(&self, options: GenIrOptions<IF, O>, logger: &mut dyn TaskLogger) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile;

    fn optimize<IF, O>(&self, options: OptimizeOptions<IF, O>, logger: &mut dyn TaskLogger) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile;
}

pub trait CommandRunnerPlatform<P: CompileTargetPlatform>: CommandRunner {
    fn platform_metadata<I, O>(
        &self,
        options: P::PlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static;

    fn codegen<I, O>(&self, options: P::CodeGenOptions<I, O>, logger: &mut dyn TaskLogger) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static;
}
