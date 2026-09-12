use super::super::backend_jvm::{
    BackendJVM, JvmCodegenOptions, JvmPlatformMetadataOptions, report_error,
};
use crate::message::TaskLogger;
use alloc::{borrow::ToOwned, format, string::String};
use alloc::{rc::Rc, vec::Vec};
use argon_io::{InputFile, OutputFile};
use core::cell::RefCell;
use esexpr::ESExprCodec;
use esexpr_binary::ExprParserSync;
use js_sys::{Array, Object, Reflect, Uint8Array};
use wasm_bindgen::{JsValue, closure::Closure, prelude::wasm_bindgen};

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
    async fn platform_metadata<I, O>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
    {
        let values = options
            .extern_files
            .into_iter()
            .map(InputFile::into_js_value)
            .collect::<Vec<_>>();
        let object = Object::new();
        set(&object, "externFiles", Array::from_iter(values).into());
        set(&object, "outputFile", options.output_file.into_js_value());
        run("platform-metadata", object, logger).await
    }

    async fn codegen_jvm<I, O>(
        &self,
        options: JvmCodegenOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
    {
        let object = Object::new();
        set(&object, "inputFile", options.input_file.into_js_value());
        set(&object, "outputFile", options.output_file.into_js_value());
        set(
            &object,
            "executable",
            JsValue::from_bool(options.executable),
        );
        run("codegen", object, logger).await
    }
}

async fn run(task: &str, options: Object, logger: &mut dyn TaskLogger) -> bool {
    let messages = Rc::new(RefCell::new(Vec::new()));
    let callback_messages = messages.clone();
    let log_message =
        Closure::<dyn FnMut(Uint8Array) -> Result<(), JsValue>>::new(move |bytes: Uint8Array| {
            decode_message(bytes.to_vec())
                .map(|message| callback_messages.borrow_mut().push(message))
                .map_err(|error| JsValue::from_str(&error))
        });
    set(&options, "logMessage", log_message.as_ref().clone());

    let result = run_jvm_backend(task, options.into()).await;
    drop(log_message);
    for message in messages.borrow_mut().drain(..) {
        logger.log(message);
    }

    match result {
        Ok(result) => match result_success(result) {
            Ok(success) => success,
            Err(error) => report_error(logger, error),
        },
        Err(error) => report_error(
            logger,
            error
                .as_string()
                .unwrap_or_else(|| "JVM backend failed".to_owned()),
        ),
    }
}

fn result_success(result: JsValue) -> Result<bool, String> {
    Reflect::get(&result, &JsValue::from_str("success"))
        .map_err(|_| "JVM backend result is missing success".to_owned())?
        .as_bool()
        .ok_or_else(|| "JVM backend success result is not a boolean".to_owned())
}

fn decode_message(bytes: Vec<u8>) -> Result<crate::message::TaskMessage, String> {
    let mut input = bytes.as_slice();
    let mut parser = esexpr_binary::parse_sync(&mut input);
    let expression = parser
        .try_read_next_expr()
        .map_err(|error| format!("invalid JVM backend message: {error:?}"))?
        .ok_or_else(|| "empty JVM backend task message".to_owned())?;
    let message = crate::message::TaskMessage::decode_esexpr(expression)
        .map_err(|error| format!("invalid JVM backend task message: {error:?}"))?;
    if parser
        .try_read_next_expr()
        .map_err(|error| format!("invalid JVM backend message: {error:?}"))?
        .is_some()
    {
        return Err("multiple JVM backend task messages in one callback".to_owned());
    }
    Ok(message)
}

fn set(object: &Object, key: &str, value: JsValue) {
    Reflect::set(object, &JsValue::from_str(key), &value)
        .expect("setting a JVM backend option cannot fail");
}
