use crate::message::{TaskLogger, task_error};
#[cfg(not(target_family = "wasm"))]
use alloc::string::String;
use alloc::vec::Vec;
use argon_io::{InputFile, OutputFile};

pub struct JvmPlatformMetadataOptions<I, O> {
    pub extern_files: Vec<I>,
    pub output_file: O,
}

pub struct JvmCodegenOptions<I, O> {
    pub input_file: I,
    pub output_file: O,
    pub executable: bool,
}

pub trait BackendJVM {
    async fn platform_metadata<I, O>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static;

    async fn codegen_jvm<I, O>(
        &self,
        options: JvmCodegenOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static;
}

pub(crate) fn report_error(logger: &mut dyn TaskLogger, error: impl core::fmt::Display) -> bool {
    logger.log(task_error(alloc::format!("JVM backend error: {error}")));
    false
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn display_error<E: core::fmt::Display>(error: E) -> String {
    use alloc::string::ToString;
    error.to_string()
}
