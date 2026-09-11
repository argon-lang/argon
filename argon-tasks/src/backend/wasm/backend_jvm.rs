use super::super::backend_jvm::{
    BackendJVM, JvmCodegenOptions, JvmPlatformMetadataOptions, report_error,
};
use alloc::borrow::ToOwned;
use alloc::vec::Vec;
use argon_io::{InputFile, OutputFile};
use embedded_io::Write;
use js_sys::{Array, Object, Reflect};
use wasm_bindgen::{JsValue, prelude::wasm_bindgen};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(catch, js_name = __argon_run_jvm_backend)]
    async fn run_jvm_backend(task: &str, options: JsValue) -> Result<JsValue, JsValue>;
}

pub struct WasmBackendJVM;

impl WasmBackendJVM {
    pub fn new() -> Self {
        Self
    }
}

impl BackendJVM for WasmBackendJVM {
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
        W: Write,
    {
        let values = options
            .extern_files
            .into_iter()
            .map(InputFile::into_js_value)
            .collect::<Vec<_>>();
        let object = Object::new();
        set(&object, "externFiles", Array::from_iter(values).into());
        set(&object, "outputFile", options.output_file.into_js_value());
        run("platform-metadata", object.into(), output).await
    }

    async fn codegen_jvm<I, O, W>(&self, options: JvmCodegenOptions<I, O>, output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write,
    {
        let object = Object::new();
        set(&object, "inputFile", options.input_file.into_js_value());
        set(&object, "outputFile", options.output_file.into_js_value());
        set(
            &object,
            "executable",
            JsValue::from_bool(options.executable),
        );
        run("codegen", object.into(), output).await
    }
}

async fn run<W: Write>(task: &str, options: JsValue, output: &mut W) -> bool {
    match run_jvm_backend(task, options).await {
        Ok(_) => true,
        Err(error) => report_error(
            output,
            error
                .as_string()
                .unwrap_or_else(|| "JVM backend failed".to_owned()),
        ),
    }
}

fn set(object: &Object, key: &str, value: JsValue) {
    Reflect::set(object, &JsValue::from_str(key), &value)
        .expect("setting a JVM backend option cannot fail");
}
