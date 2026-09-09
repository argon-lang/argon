use alloc::string::{String, ToString};
use alloc::vec::Vec;
use argon_io::{InputFile, OutputDirectory, OutputFile};
use embedded_io::Write;

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
    async fn platform_metadata<I, O, W>(
        &self,
        options: PlatformMetadataJSOptions<I, O>,
        output: &mut W,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write;

    async fn codegen_js<I, O, W>(&self, options: CodegenJSOptions<I, O>, output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
        W: Write;
}

pub(crate) fn display_error<E: core::fmt::Display>(error: E) -> String {
    error.to_string()
}

pub(crate) fn report_error<W: Write>(output: &mut W, error: impl core::fmt::Display) -> bool {
    let _ = output.write_fmt(format_args!("JavaScript backend error: {error}\n"));
    false
}
