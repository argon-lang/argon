use crate::message::{TaskLogger, task_error};
use alloc::string::String;
#[cfg(not(target_family = "wasm"))]
use alloc::string::ToString;
use alloc::vec::Vec;
use argon_io::{InputFile, OutputDirectory, OutputFile};

pub struct PlatformMetadataJSOptions<I, O> {
    pub package_name: Option<String>,
    pub extern_files: Vec<I>,
    pub output_file: O,
}

pub struct CodegenJSOptions<I, O> {
    pub input_file: I,
    pub output_dir: O,
    pub executable: Option<String>,
}

pub trait BackendJS {
    async fn platform_metadata<I, O>(
        &self,
        options: PlatformMetadataJSOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static;

    async fn codegen_js<I, O>(
        &self,
        options: CodegenJSOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static;
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn display_error<E: core::fmt::Display>(error: E) -> String {
    error.to_string()
}

pub(crate) fn report_error(logger: &mut dyn TaskLogger, error: impl core::fmt::Display) -> bool {
    logger.log(task_error(alloc::format!(
        "JavaScript backend error: {error}"
    )));
    false
}
