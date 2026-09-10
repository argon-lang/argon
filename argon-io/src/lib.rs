#![no_std]
#![allow(async_fn_in_trait)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

use argon_util::InternalCompilerError;
use parse18_runtime::LocationFileView;
#[cfg(feature = "std")]
use std::path::Path;

pub use embedded_io_async::{Read, Write};

#[cfg(target_family = "wasm")]
pub mod wasm;

pub trait InputStream: Read<Error = InternalCompilerError> {
    async fn close(&mut self) -> Result<(), InternalCompilerError>;

    #[cfg(target_family = "wasm")]
    fn into_js_value(self) -> wasm_bindgen::JsValue
    where
        Self: Sized + 'static,
    {
        wasm::rust_input_stream(self)
    }
}

pub trait OutputStream: Write<Error = InternalCompilerError> {
    async fn close(&mut self) -> Result<(), InternalCompilerError>;

    #[cfg(target_family = "wasm")]
    fn into_js_value(self) -> wasm_bindgen::JsValue
    where
        Self: Sized + 'static,
    {
        wasm::rust_output_stream(self)
    }
}

pub trait InputFile {
    type Reader: InputStream;

    #[cfg(feature = "std")]
    fn path(&self) -> &Path;

    #[cfg(feature = "std")]
    fn location_file(&self) -> &LocationFileView {
        self.path()
    }

    #[cfg(not(feature = "std"))]
    fn location_file(&self) -> &LocationFileView;

    async fn open(&self) -> Result<Self::Reader, InternalCompilerError>;

    #[cfg(target_family = "wasm")]
    fn into_js_value(self) -> wasm_bindgen::JsValue
    where
        Self: Sized + 'static,
    {
        wasm::rust_input_file(self)
    }
}

pub trait InputDirectory {
    type File: InputFile;
    type Files: IntoIterator<Item = Result<Self::File, InternalCompilerError>>;

    async fn list_files(&self) -> Self::Files;
}

pub trait OutputFile {
    type Writer: OutputStream;

    #[cfg(feature = "std")]
    fn path(&self) -> &Path;
    async fn open(&self) -> Result<Self::Writer, InternalCompilerError>;
    async fn delete(&self) -> Result<(), InternalCompilerError>;

    #[cfg(target_family = "wasm")]
    fn into_js_value(self) -> wasm_bindgen::JsValue
    where
        Self: Sized + 'static,
    {
        wasm::rust_output_file(self)
    }
}

pub trait OutputDirectory {
    type File: OutputFile;

    fn create_file(&self, name: &str) -> Result<Self::File, InternalCompilerError>;

    #[cfg(target_family = "wasm")]
    fn into_js_value(self) -> wasm_bindgen::JsValue
    where
        Self: Sized + 'static,
    {
        wasm::rust_output_directory(self)
    }
}
