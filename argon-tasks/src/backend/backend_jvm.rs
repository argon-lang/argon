#[cfg(not(target_family = "wasm"))]
use alloc::string::String;
use alloc::vec::Vec;
use argon_io::{InputFile, OutputFile};
use embedded_io::Write;

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
    async fn platform_metadata<I, O, W>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
        output: &mut W,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write;

    async fn codegen_jvm<I, O, W>(&self, options: JvmCodegenOptions<I, O>, output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write;
}

pub(crate) fn report_error<W: Write>(output: &mut W, error: impl core::fmt::Display) -> bool {
    let _ = output.write_fmt(format_args!("JVM backend error: {error}\n"));
    false
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn display_error<E: core::fmt::Display>(error: E) -> String {
    use alloc::string::ToString;
    error.to_string()
}
