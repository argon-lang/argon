mod backend_options;
mod cli;
mod direct;
mod staging;

use crate::CompileTargetPlatform;
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile, Write};
use argon_tasks::{CompileOptions, GenIrOptions, OptimizeOptions};
use argon_util::sync::ThreadSafe;

pub use cli::CliCommandRunner;
pub use direct::DirectCommandRunner;

pub trait CommandRunner {
    fn compile<ID, IF, O, W>(
        &self,
        options: CompileOptions<ID, IF, O>,
        error_output: &mut W,
    ) -> bool
    where
        ID: InputDirectory + ThreadSafe,
        ID::File: ThreadSafe,
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write;

    fn gen_ir<IF, O, W>(&self, options: GenIrOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write;

    fn optimize<IF, O, W>(&self, options: OptimizeOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write;
}

pub trait CommandRunnerPlatform<P: CompileTargetPlatform>: CommandRunner {
    fn platform_metadata<I, O, W>(
        &self,
        options: P::PlatformMetadataOptions<I, O>,
        error_output: &mut W,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write;

    fn codegen<I, O, W>(&self, options: P::CodeGenOptions<I, O>, error_output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
        W: Write;
}
