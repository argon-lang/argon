use super::backend_js::{
    BackendJS, CodegenJSOptions, PlatformMetadataJSOptions, display_error, report_error,
};
use argon_io::{InputFile, OutputDirectory, OutputFile, Read};
use embedded_io::Write;
use js_sys::{Array, Function, Object, Reflect, Uint8Array};
use std::{cell::RefCell, rc::Rc};
use wasm_bindgen::{JsCast, JsValue, closure::Closure, prelude::wasm_bindgen};

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
        W: Write,
    {
        let extern_files = options
            .extern_files
            .into_iter()
            .map(|file| input_file(Rc::new(RefCell::new(Some(file)))))
            .collect::<Vec<_>>();
        let output_file = output_file(Rc::new(RefCell::new(Some(options.output_file))));
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
        run("platform-metadata", js_options.into(), output).await
    }

    async fn codegen_js<I, O, W>(&self, options: CodegenJSOptions<I, O>, output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
        W: Write,
    {
        let input = input_file(Rc::new(RefCell::new(Some(options.input_file))));
        let directory = output_directory(Rc::new(RefCell::new(Some(options.output_dir))));
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
        run("codegen", js_options.into(), output).await
    }
}

async fn run<W: Write>(task: &str, options: JsValue, output: &mut W) -> bool {
    match run_backend(task, options).await {
        Ok(_) => true,
        Err(error) => report_error(
            output,
            error
                .as_string()
                .unwrap_or_else(|| "JavaScript backend failed".to_owned()),
        ),
    }
}

fn input_file<I>(file: Rc<RefCell<Option<I>>>) -> JsValue
where
    I: InputFile + 'static,
    I::Reader: 'static,
{
    let file_name = file
        .borrow()
        .as_ref()
        .map(|file| {
            file.path()
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("input")
                .to_owned()
        })
        .unwrap_or_else(|| "input".to_owned());
    let open_file = file.clone();
    let open = Closure::<dyn FnMut() -> Result<JsValue, JsValue>>::new(move || {
        let reader = open_file
            .borrow()
            .as_ref()
            .ok_or_else(|| js_error("input file is unavailable"))?
            .open()
            .map_err(|error| js_error(display_error(error)))?;
        Ok(input_stream(Rc::new(RefCell::new(Some(reader))).into()).into())
    });
    let object = Object::new();
    set(&object, "fileName", JsValue::from_str(&file_name));
    set(&object, "open", open.as_ref().clone());
    open.forget();
    object.into()
}

fn input_stream<R>(reader: Rc<RefCell<Option<R>>>) -> Object
where
    R: Read + 'static,
{
    let read_state = reader.clone();
    let read = Closure::<dyn FnMut(JsValue) -> Result<JsValue, JsValue>>::new(move |buffer| {
        let array = Uint8Array::new(&buffer);
        let mut bytes = vec![0; array.length() as usize];
        let count = read_state
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| js_error("input stream is closed"))?
            .read(&mut bytes)
            .map_err(|error| js_error(display_error(error)))?;
        array.set(&Uint8Array::from(&bytes[..count]), 0);
        Ok(JsValue::from_f64(count as f64))
    });
    let close_state = reader;
    let close = Closure::<dyn FnMut()>::new(move || {
        close_state.borrow_mut().take();
    });
    let object = Object::new();
    set(&object, "read", read.as_ref().clone());
    set(&object, "close", close.as_ref().clone());
    read.forget();
    close.forget();
    object
}

fn output_file<O>(file: Rc<RefCell<Option<O>>>) -> JsValue
where
    O: OutputFile + 'static,
    O::Writer: 'static,
{
    let open_file = file.clone();
    let open = Closure::<dyn FnMut() -> Result<JsValue, JsValue>>::new(move || {
        let writer = open_file
            .borrow()
            .as_ref()
            .ok_or_else(|| js_error("output file is unavailable"))?
            .open()
            .map_err(|error| js_error(display_error(error)))?;
        Ok(output_stream(Rc::new(RefCell::new(Some(writer))).into()).into())
    });
    let delete_file = file;
    let delete = Closure::<dyn FnMut() -> Result<(), JsValue>>::new(move || {
        delete_file
            .borrow()
            .as_ref()
            .ok_or_else(|| js_error("output file is unavailable"))?
            .delete()
            .map_err(|error| js_error(display_error(error)))
    });
    let object = Object::new();
    set(&object, "open", open.as_ref().clone());
    set(&object, "delete", delete.as_ref().clone());
    open.forget();
    delete.forget();
    object.into()
}

fn output_stream<W>(writer: Rc<RefCell<Option<W>>>) -> Object
where
    W: Write + 'static,
{
    let write_state = writer.clone();
    let write = Closure::<dyn FnMut(JsValue) -> Result<(), JsValue>>::new(move |buffer| {
        let array = Uint8Array::new(&buffer);
        let mut bytes = vec![0; array.length() as usize];
        array.copy_to(&mut bytes);
        write_state
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| js_error("output stream is closed"))?
            .write_all(&bytes)
            .map_err(|error| js_error(display_error(error)))
    });
    let close_state = writer;
    let close = Closure::<dyn FnMut()>::new(move || {
        close_state.borrow_mut().take();
    });
    let object = Object::new();
    set(&object, "write", write.as_ref().clone());
    set(&object, "close", close.as_ref().clone());
    write.forget();
    close.forget();
    object
}

fn output_directory<O>(directory: Rc<RefCell<Option<O>>>) -> JsValue
where
    O: OutputDirectory + 'static,
    O::File: OutputFile + 'static,
    <O::File as OutputFile>::Writer: 'static,
{
    let create_file =
        Closure::<dyn FnMut(Array) -> Result<JsValue, JsValue>>::new(move |parts: Array| {
            let mut name = String::new();
            for index in 0..parts.length() {
                if !name.is_empty() {
                    name.push('/');
                }
                name.push_str(
                    &parts
                        .get(index)
                        .as_string()
                        .ok_or_else(|| js_error("output path part is not a string"))?,
                );
            }
            let file = directory
                .borrow()
                .as_ref()
                .ok_or_else(|| js_error("output directory is unavailable"))?
                .create_file(&name)
                .map_err(|error| js_error(display_error(error)))?;
            Ok(output_file(Rc::new(RefCell::new(Some(file)))))
        });
    let wrapper = Function::new_with_args(
        "createFile",
        "return { getFile(...parts) { return createFile(parts); } };",
    );
    let object = wrapper
        .call1(&JsValue::UNDEFINED, create_file.as_ref())
        .unwrap_or_else(|_| Object::new().into());
    create_file.forget();
    object.unchecked_into()
}

fn array_of(values: Vec<JsValue>) -> JsValue {
    Array::from_iter(values).into()
}
fn set(object: &Object, key: &str, value: JsValue) {
    Reflect::set(object, &JsValue::from_str(key), &value)
        .expect("setting a JS backend option cannot fail");
}
fn js_error(message: impl Into<String>) -> JsValue {
    JsValue::from_str(&message.into())
}
