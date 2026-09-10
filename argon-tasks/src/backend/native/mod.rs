use super::backend_js::{
    BackendJS, CodegenJSOptions, PlatformMetadataJSOptions, display_error, report_error,
};
use argon_io::{InputFile, InputStream, OutputDirectory, OutputFile, OutputStream};
use boa_engine::{
    Context, JsNativeError, JsResult, JsString, JsValue, Module, NativeFunction, Source,
    builtins::promise::PromiseState,
    js_string,
    module::{ModuleLoader, Referrer},
    object::{
        JsObject,
        builtins::{JsArray, JsFunction, JsPromise, JsUint8Array},
    },
    property::Attribute,
};
use boa_runtime::{RuntimeExtension, extensions::EncodingExtension};
use embedded_io::Write as SyncWrite;
use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    future::Future,
    path::{Path, PathBuf},
    pin::Pin,
    rc::Rc,
};

type LocalFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

const RUNNER_SOURCE: &str = r#"
export default (async function(kind, options) {
    if (kind === 'platform-metadata') {
        const metadata = await __argon_load_metadata({
            packageName: options.packageName,
            externFiles: options.externFiles,
        });
        const expr = __argon_metadata_codec.encode({ ...metadata, platform: 'js' });
        const stream = await options.outputFile.open();
        try {
            for await (const chunk of __argon_write_exprs([expr])) await stream.write(chunk);
        }
        finally { await stream.close(); }
    }
    else {
        await __argon_codegen({
            tube: options.inputFile,
            outputDirectory: options.outputDirectory,
            executable: options.executable,
        });
    }
})
"#;

pub struct NativeBackendJS {
    path: PathBuf,
    engine: OnceCell<Result<RefCell<NativeBackendJSEngine>, String>>,
}

struct NativeBackendJSEngine {
    runner: JsFunction,
    context: Context,
}

impl NativeBackendJS {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            engine: OnceCell::new(),
        }
    }

    fn create_engine(&self) -> Result<NativeBackendJSEngine, String> {
        let root = self.path.canonicalize().map_err(|error| {
            format!(
                "failed to resolve backend path {}: {error}",
                self.path.display()
            )
        })?;
        let loader = Rc::new(BackendModuleLoader::new(root.clone()));
        let mut context = Context::builder()
            .module_loader(loader)
            .build()
            .map_err(display_error)?;
        register_host_functions(&mut context)?;

        let backend = load_module(&root.join("lib/index.js"), &mut context)?;
        let metadata = load_module(
            &root.join("node_modules/@argon-lang/js-backend-api/lib/metadata.js"),
            &mut context,
        )?;
        let binary = load_module(
            &root.join("node_modules/@argon-lang/esexpr/lib/binary_format.js"),
            &mut context,
        )?;
        for (name, value) in [
            (
                "__argon_load_metadata",
                module_value(&backend, "loadMetadata", &mut context)?,
            ),
            (
                "__argon_codegen",
                module_value(&backend, "codegen", &mut context)?,
            ),
            (
                "__argon_metadata_codec",
                metadata_codec(&metadata, &mut context)?,
            ),
            (
                "__argon_write_exprs",
                module_value(&binary, "writeExprs", &mut context)?,
            ),
        ] {
            context
                .register_global_property(JsString::from(name), value, Attribute::all())
                .map_err(display_error)?;
        }

        let runner_path = root.join(".argon-task-runner.mjs");
        let runner = Module::parse(
            Source::from_bytes(RUNNER_SOURCE).with_path(&runner_path),
            None,
            &mut context,
        )
        .map_err(display_error)?;
        settle(runner.load_link_evaluate(&mut context), &mut context)?;
        let function = runner
            .get_value(js_string!("default"), &mut context)
            .map_err(display_error)?
            .as_function()
            .ok_or_else(|| "backend task runner is not callable".to_owned())?;

        Ok(NativeBackendJSEngine {
            runner: function,
            context,
        })
    }

    fn run_inner<F>(&self, kind: &str, make_options: F) -> Result<(), String>
    where
        F: FnOnce(&mut Context) -> Result<JsValue, String>,
    {
        let engine = self
            .engine
            .get_or_init(|| self.create_engine().map(RefCell::new))
            .as_ref()
            .map_err(Clone::clone)?;
        let mut engine = engine.borrow_mut();
        let options = make_options(&mut engine.context)?;
        let runner = engine.runner.clone();
        let promise = runner
            .call(
                &JsValue::undefined(),
                &[JsValue::from(JsString::from(kind)), options],
                &mut engine.context,
            )
            .map_err(display_error)?;
        let promise = JsPromise::from_object(
            promise
                .as_object()
                .ok_or_else(|| "backend task did not return a promise".to_owned())?
                .clone(),
        )
        .map_err(display_error)?;
        settle(promise, &mut engine.context).map(|_| ())
    }
}

impl BackendJS for NativeBackendJS {
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
        W: SyncWrite,
    {
        let extern_files = options
            .extern_files
            .into_iter()
            .map(|file| Rc::new(InputAdapter(file)) as Rc<dyn InputBridge>)
            .collect::<Vec<_>>();
        let output_file =
            Rc::new(OutputFileAdapter(options.output_file)) as Rc<dyn OutputFileBridge>;
        let result = self.run_inner("platform-metadata", move |context| {
            let extern_files = extern_files
                .iter()
                .map(|file| input_object(file.clone(), context))
                .collect::<Result<Vec<_>, _>>()?;
            let extern_files =
                JsArray::from_iter(extern_files.into_iter().map(Into::into), context);
            let output_file = output_object(output_file.clone(), context);
            let mut object = boa_engine::object::ObjectInitializer::new(context);
            object.property(
                js_string!("packageName"),
                optional_string(options.package_name),
                Attribute::all(),
            );
            object.property(js_string!("externFiles"), extern_files, Attribute::all());
            object.property(js_string!("outputFile"), output_file, Attribute::all());
            Ok(object.build().into())
        });
        match result {
            Ok(()) => true,
            Err(error) => report_error(output, error),
        }
    }

    async fn codegen_js<I, O, W>(&self, options: CodegenJSOptions<I, O>, output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
        W: SyncWrite,
    {
        let input = Rc::new(InputAdapter(options.input_file)) as Rc<dyn InputBridge>;
        let directory =
            Rc::new(OutputDirectoryAdapter(options.output_dir)) as Rc<dyn OutputDirectoryBridge>;
        let result = self.run_inner("codegen", move |context| {
            let input_file = input_object(input.clone(), context)?;
            let output_directory = directory_object(directory.clone(), context);
            let mut object = boa_engine::object::ObjectInitializer::new(context);
            object.property(js_string!("inputFile"), input_file, Attribute::all());
            object.property(
                js_string!("outputDirectory"),
                output_directory,
                Attribute::all(),
            );
            object.property(
                js_string!("executable"),
                optional_string(options.executable),
                Attribute::all(),
            );
            Ok(object.build().into())
        });
        match result {
            Ok(()) => true,
            Err(error) => report_error(output, error),
        }
    }
}

trait InputBridge {
    fn file_name(&self) -> String;
    fn open(&self) -> LocalFuture<'_, Result<Box<dyn InputReader>, String>>;
}
trait InputReader {
    fn read<'a>(&'a mut self, buffer: &'a mut [u8]) -> LocalFuture<'a, Result<usize, String>>;
    fn close(&mut self) -> LocalFuture<'_, Result<(), String>>;
}
struct InputAdapter<I>(I);
impl<I> InputBridge for InputAdapter<I>
where
    I: InputFile + 'static,
    I::Reader: 'static,
{
    fn file_name(&self) -> String {
        self.0
            .path()
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("input")
            .to_owned()
    }
    fn open(&self) -> LocalFuture<'_, Result<Box<dyn InputReader>, String>> {
        Box::pin(async {
            self.0
                .open()
                .await
                .map(|reader| Box::new(ReaderAdapter(reader)) as Box<dyn InputReader>)
                .map_err(display_error)
        })
    }
}
struct ReaderAdapter<R>(R);
impl<R> InputReader for ReaderAdapter<R>
where
    R: InputStream + 'static,
{
    fn read<'a>(&'a mut self, buffer: &'a mut [u8]) -> LocalFuture<'a, Result<usize, String>> {
        Box::pin(async { self.0.read(buffer).await.map_err(display_error) })
    }
    fn close(&mut self) -> LocalFuture<'_, Result<(), String>> {
        Box::pin(async { self.0.close().await.map_err(display_error) })
    }
}

trait OutputFileBridge {
    fn open(&self) -> LocalFuture<'_, Result<Box<dyn OutputWriter>, String>>;
    fn delete(&self) -> LocalFuture<'_, Result<(), String>>;
}
trait OutputWriter {
    fn write<'a>(&'a mut self, buffer: &'a [u8]) -> LocalFuture<'a, Result<(), String>>;
    fn close(&mut self) -> LocalFuture<'_, Result<(), String>>;
}
struct OutputFileAdapter<O>(O);
impl<O> OutputFileBridge for OutputFileAdapter<O>
where
    O: OutputFile + 'static,
    O::Writer: 'static,
{
    fn open(&self) -> LocalFuture<'_, Result<Box<dyn OutputWriter>, String>> {
        Box::pin(async {
            self.0
                .open()
                .await
                .map(|writer| Box::new(WriterAdapter(writer)) as Box<dyn OutputWriter>)
                .map_err(display_error)
        })
    }
    fn delete(&self) -> LocalFuture<'_, Result<(), String>> {
        Box::pin(async { self.0.delete().await.map_err(display_error) })
    }
}
struct WriterAdapter<W>(W);
impl<W> OutputWriter for WriterAdapter<W>
where
    W: OutputStream + 'static,
{
    fn write<'a>(&'a mut self, buffer: &'a [u8]) -> LocalFuture<'a, Result<(), String>> {
        Box::pin(async { self.0.write_all(buffer).await.map_err(display_error) })
    }
    fn close(&mut self) -> LocalFuture<'_, Result<(), String>> {
        Box::pin(async {
            self.0.flush().await.map_err(display_error)?;
            self.0.close().await.map_err(display_error)
        })
    }
}

trait OutputDirectoryBridge {
    fn create_file(&self, name: &str) -> Result<Rc<dyn OutputFileBridge>, String>;
}
struct OutputDirectoryAdapter<O>(O);
impl<O> OutputDirectoryBridge for OutputDirectoryAdapter<O>
where
    O: OutputDirectory + 'static,
    O::File: OutputFile + 'static,
    <O::File as OutputFile>::Writer: 'static,
{
    fn create_file(&self, name: &str) -> Result<Rc<dyn OutputFileBridge>, String> {
        Ok(Rc::new(OutputFileAdapter(
            self.0.create_file(name).map_err(display_error)?,
        )))
    }
}

fn input_object(input: Rc<dyn InputBridge>, context: &mut Context) -> Result<JsObject, String> {
    let open_input = input.clone();
    let open = unsafe {
        NativeFunction::from_closure(move |_, _, context| {
            let input = open_input.clone();
            Ok(JsPromise::from_async_fn(
                async move |context| {
                    let reader = input.open().await.map_err(js_error)?;
                    Ok(reader_object(
                        Rc::new(RefCell::new(Some(reader))),
                        &mut context.borrow_mut(),
                    )
                    .into())
                },
                context,
            )
            .into())
        })
    };
    let mut object = boa_engine::object::ObjectInitializer::new(context);
    object.property(
        js_string!("fileName"),
        JsString::from(input.file_name()),
        Attribute::all(),
    );
    object.function(open, js_string!("open"), 0);
    Ok(object.build())
}

fn reader_object(
    reader: Rc<RefCell<Option<Box<dyn InputReader>>>>,
    context: &mut Context,
) -> JsObject {
    let read_state = reader.clone();
    let read = unsafe {
        NativeFunction::from_closure(move |_, args, context| {
            let buffer = args
                .first()
                .and_then(JsValue::as_object)
                .ok_or_else(|| JsNativeError::typ().with_message("read expects a buffer"))?
                .clone();
            let buffer = JsUint8Array::from_object(buffer)?;
            let length = buffer.length(context)?;
            let state = read_state.clone();
            Ok(JsPromise::from_async_fn(
                async move |context| {
                    let mut reader = state
                        .borrow_mut()
                        .take()
                        .ok_or_else(|| JsNativeError::typ().with_message("stream is closed"))?;
                    let mut bytes = vec![0; length];
                    let result = reader.read(&mut bytes).await;
                    state.borrow_mut().replace(reader);
                    let count = result.map_err(js_error)?;
                    let context = &mut context.borrow_mut();
                    buffer.set_values(
                        JsArray::from_iter(
                            bytes
                                .into_iter()
                                .take(count)
                                .map(|byte| JsValue::from(u32::from(byte))),
                            context,
                        )
                        .into(),
                        Some(0),
                        context,
                    )?;
                    Ok(JsValue::from(count as u32))
                },
                context,
            )
            .into())
        })
    };
    let close_state = reader;
    let close = unsafe {
        NativeFunction::from_closure(move |_, _, context| {
            let state = close_state.clone();
            Ok(JsPromise::from_async_fn(
                async move |_| {
                    let mut reader = state
                        .borrow_mut()
                        .take()
                        .ok_or_else(|| js_error("stream is closed".to_owned()))?;
                    reader.close().await.map_err(js_error)?;
                    Ok(JsValue::undefined())
                },
                context,
            )
            .into())
        })
    };
    let mut object = boa_engine::object::ObjectInitializer::new(context);
    object.function(read, js_string!("read"), 1);
    object.function(close, js_string!("close"), 0);
    object.build()
}

fn output_object(file: Rc<dyn OutputFileBridge>, context: &mut Context) -> JsObject {
    output_file_object(file, context)
}
fn output_file_object(file: Rc<dyn OutputFileBridge>, context: &mut Context) -> JsObject {
    let open_file = file.clone();
    let open = unsafe {
        NativeFunction::from_closure(move |_, _, context| {
            let file = open_file.clone();
            Ok(JsPromise::from_async_fn(
                async move |context| {
                    let writer = file.open().await.map_err(js_error)?;
                    Ok(writer_object(
                        Rc::new(RefCell::new(Some(writer))),
                        &mut context.borrow_mut(),
                    )
                    .into())
                },
                context,
            )
            .into())
        })
    };
    let delete_file = file;
    let delete = unsafe {
        NativeFunction::from_closure(move |_, _, context| {
            let file = delete_file.clone();
            Ok(JsPromise::from_async_fn(
                async move |_| {
                    file.delete().await.map_err(js_error)?;
                    Ok(JsValue::undefined())
                },
                context,
            )
            .into())
        })
    };
    let mut object = boa_engine::object::ObjectInitializer::new(context);
    object.function(open, js_string!("open"), 0);
    object.function(delete, js_string!("delete"), 0);
    object.build()
}

fn writer_object(
    writer: Rc<RefCell<Option<Box<dyn OutputWriter>>>>,
    context: &mut Context,
) -> JsObject {
    let write_state = writer.clone();
    let write = unsafe {
        NativeFunction::from_closure(move |_, args, context| {
            let buffer = args
                .first()
                .and_then(JsValue::as_object)
                .ok_or_else(|| JsNativeError::typ().with_message("write expects a buffer"))?
                .clone();
            let buffer = JsUint8Array::from_object(buffer)?;
            let bytes = buffer.iter(context).collect::<Vec<_>>();
            let state = write_state.clone();
            Ok(JsPromise::from_async_fn(
                async move |_| {
                    let mut writer = state
                        .borrow_mut()
                        .take()
                        .ok_or_else(|| JsNativeError::typ().with_message("stream is closed"))?;
                    let result = writer.write(&bytes).await;
                    state.borrow_mut().replace(writer);
                    result.map_err(js_error)?;
                    Ok(JsValue::undefined())
                },
                context,
            )
            .into())
        })
    };
    let close_state = writer;
    let close = unsafe {
        NativeFunction::from_closure(move |_, _, context| {
            let state = close_state.clone();
            Ok(JsPromise::from_async_fn(
                async move |_| {
                    let mut writer = state
                        .borrow_mut()
                        .take()
                        .ok_or_else(|| js_error("stream is closed".to_owned()))?;
                    writer.close().await.map_err(js_error)?;
                    Ok(JsValue::undefined())
                },
                context,
            )
            .into())
        })
    };
    let mut object = boa_engine::object::ObjectInitializer::new(context);
    object.function(write, js_string!("write"), 1);
    object.function(close, js_string!("close"), 0);
    object.build()
}

fn directory_object(directory: Rc<dyn OutputDirectoryBridge>, context: &mut Context) -> JsObject {
    let get_file_directory = directory;
    let get_file = unsafe {
        NativeFunction::from_closure(move |_, args, context| {
            if args.is_empty() {
                return Err(JsNativeError::typ()
                    .with_message("getFile expects a path")
                    .into());
            }
            let mut parts = Vec::with_capacity(args.len());
            for arg in args {
                parts.push(arg.to_string(context)?.to_std_string_escaped());
            }
            let file = get_file_directory
                .create_file(&parts.join("/"))
                .map_err(js_error)?;
            Ok(output_file_object(file, context).into())
        })
    };
    let mut object = boa_engine::object::ObjectInitializer::new(context);
    object.function(get_file, js_string!("getFile"), 1);
    object.build()
}

fn optional_string(value: Option<String>) -> JsValue {
    value
        .map(|value| JsValue::from(JsString::from(value)))
        .unwrap_or_else(JsValue::undefined)
}
fn js_error(error: String) -> boa_engine::JsError {
    JsNativeError::typ().with_message(error).into()
}

#[derive(Clone, Debug)]
struct BackendModuleLoader {
    root: PathBuf,
    modules: RefCell<HashMap<PathBuf, Module>>,
}
impl BackendModuleLoader {
    fn new(root: PathBuf) -> Self {
        Self {
            root,
            modules: RefCell::new(HashMap::new()),
        }
    }
    fn resolve(&self, referrer: Option<&Path>, specifier: &str) -> Result<PathBuf, String> {
        let path = if specifier.starts_with('.') {
            referrer
                .and_then(Path::parent)
                .ok_or_else(|| "relative JavaScript import has no referrer".to_owned())?
                .join(specifier)
        } else {
            let (package, subpath) = if specifier.starts_with('@') {
                let mut parts = specifier.splitn(3, '/');
                (
                    format!(
                        "{}/{}",
                        parts.next().unwrap_or_default(),
                        parts.next().unwrap_or_default()
                    ),
                    parts.next().unwrap_or_default().to_owned(),
                )
            } else {
                let mut parts = specifier.splitn(2, '/');
                (
                    parts.next().unwrap_or_default().to_owned(),
                    parts.next().unwrap_or_default().to_owned(),
                )
            };
            let root = self.root.join("node_modules").join(package);
            if subpath.is_empty() {
                first_existing(&[
                    root.join("lib/index.js"),
                    root.join("index.js"),
                    root.join("dist/index.js"),
                    root.join("dist/index.mjs"),
                    root.join("dist/acorn.mjs"),
                    root.join("dist/astring.mjs"),
                ])
            } else {
                first_existing(&[
                    root.join("lib").join(&subpath),
                    root.join(&subpath),
                    root.join("lib").join(&subpath).with_extension("js"),
                    root.join(&subpath).with_extension("js"),
                ])
            }
            .ok_or_else(|| format!("cannot resolve JavaScript import {specifier}"))?
        };
        path.canonicalize()
            .map_err(|error| format!("cannot resolve JavaScript import {specifier}: {error}"))
    }
}
impl ModuleLoader for BackendModuleLoader {
    fn load_imported_module(
        self: Rc<Self>,
        referrer: Referrer,
        specifier: JsString,
        context: &RefCell<&mut Context>,
    ) -> impl Future<Output = JsResult<Module>> {
        let result = (|| {
            let specifier = specifier.to_std_string_escaped();
            let path = self
                .resolve(referrer.path(), &specifier)
                .map_err(|error| JsNativeError::typ().with_message(error))?;
            if let Some(module) = self.modules.borrow().get(&path) {
                return Ok(module.clone());
            }
            let source = Source::from_filepath(&path).map_err(|error| {
                JsNativeError::typ().with_message(format!("could not open {specifier}: {error}"))
            })?;
            let module = Module::parse(source, None, &mut context.borrow_mut())
                .map_err(|error| JsNativeError::syntax().with_cause(error))?;
            self.modules.borrow_mut().insert(path, module.clone());
            Ok(module)
        })();
        async { result }
    }
}
fn first_existing(paths: &[PathBuf]) -> Option<PathBuf> {
    paths.iter().find(|path| path.is_file()).cloned()
}
fn load_module(path: &Path, context: &mut Context) -> Result<Module, String> {
    let module = Module::parse(
        Source::from_filepath(path).map_err(display_error)?,
        None,
        context,
    )
    .map_err(display_error)?;
    settle(module.load_link_evaluate(context), context)?;
    Ok(module)
}
fn settle(promise: JsPromise, context: &mut Context) -> Result<JsValue, String> {
    context.run_jobs().map_err(display_error)?;
    match promise.state() {
        PromiseState::Fulfilled(value) => Ok(value),
        PromiseState::Rejected(error) => Err(error
            .to_string(context)
            .map_err(display_error)?
            .to_std_string_escaped()),
        PromiseState::Pending => Err("JavaScript backend promise remained pending".to_owned()),
    }
}
fn module_value(module: &Module, name: &str, context: &mut Context) -> Result<JsValue, String> {
    module
        .get_value(JsString::from(name), context)
        .map_err(display_error)
}
fn metadata_codec(module: &Module, context: &mut Context) -> Result<JsValue, String> {
    module
        .namespace(context)
        .get(js_string!("PlatformMetadataResult"), context)
        .map_err(display_error)?
        .as_object()
        .ok_or_else(|| "PlatformMetadataResult export is not an object".to_owned())?
        .get(js_string!("codec"), context)
        .map_err(display_error)
}
fn register_host_functions(context: &mut Context) -> Result<(), String> {
    EncodingExtension
        .register(None, context)
        .map_err(display_error)?;
    // The backend uses the standard TextDecoder flush form, `decode()`.
    // Boa's runtime implementation currently requires an explicit buffer and
    // decodes the full backing buffer instead of a typed array's view. Copy
    // each chunk into an exactly-sized typed array so a reusable read buffer
    // does not contribute its zero-filled tail to the decoded source.
    context.eval(Source::from_bytes("const __argon_decode = TextDecoder.prototype.decode; TextDecoder.prototype.decode = function(value, options) { if (arguments.length === 0) return __argon_decode.call(this, new Uint8Array(0), options); const copy = new Uint8Array(value.length); copy.set(value); return __argon_decode.call(this, copy, options); }; const __argon_set_union = function(other) { const result = new Set(); this.forEach(value => result.add(value)); other.forEach(value => result.add(value)); return result; }; const __argon_set_is_disjoint_from = function(other) { let result = true; other.forEach(value => { if (this.has(value)) result = false; }); return result; }; Set.prototype.union = __argon_set_union; Set.prototype.isDisjointFrom = __argon_set_is_disjoint_from;"))
        .map(|_| ()).map_err(display_error)
}
use alloc::borrow::ToOwned;
use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;
