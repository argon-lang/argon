use super::super::backend_js::{
    BackendJS, CodegenJSOptions, PlatformMetadataJSOptions, report_error,
};
use crate::message::TaskLogger;
use alloc::borrow::ToOwned;
use alloc::vec::Vec;
use argon_io::{InputFile, OutputDirectory, OutputFile};
use js_sys::{Array, Object, Reflect};
use wasm_bindgen::{JsValue, prelude::wasm_bindgen};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(catch, js_name = __argon_run_backend)]
    async fn run_backend(task: &str, options: JsValue) -> Result<JsValue, JsValue>;
}

pub struct WasmBackendJS;
impl WasmBackendJS {
    pub fn new() -> Self {
        Self
    }
}

impl BackendJS for WasmBackendJS {
    async fn platform_metadata<I, O>(
        &self,
        options: PlatformMetadataJSOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
    {
        let extern_files = options
            .extern_files
            .into_iter()
            .map(InputFile::into_js_value)
            .collect::<Vec<_>>();
        let output_file = options.output_file.into_js_value();
        let js_options = Object::new();
        set(
            &js_options,
            "packageName",
            options
                .package_name
                .map_or(JsValue::UNDEFINED, |value| JsValue::from_str(&value)),
        );
        set(&js_options, "externFiles", array_of(extern_files));
        set(&js_options, "outputFile", output_file);
        run("platform-metadata", js_options.into(), logger).await
    }

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
        <O::File as OutputFile>::Writer: 'static,
    {
        let input = options.input_file.into_js_value();
        let directory = options.output_dir.into_js_value();
        let js_options = Object::new();
        set(&js_options, "inputFile", input);
        set(&js_options, "outputDirectory", directory);
        set(
            &js_options,
            "executable",
            options
                .executable
                .map_or(JsValue::UNDEFINED, |value| JsValue::from_str(&value)),
        );
        run("codegen", js_options.into(), logger).await
    }
}

async fn run(task: &str, options: JsValue, logger: &mut dyn TaskLogger) -> bool {
    match run_backend(task, options).await {
        Ok(_) => true,
        Err(error) => report_error(
            logger,
            error
                .as_string()
                .unwrap_or_else(|| "JavaScript backend failed".to_owned()),
        ),
    }
}

fn array_of(values: Vec<JsValue>) -> JsValue {
    Array::from_iter(values).into()
}
fn set(object: &Object, key: &str, value: JsValue) {
    Reflect::set(object, &JsValue::from_str(key), &value)
        .expect("setting a JS backend option cannot fail");
}
