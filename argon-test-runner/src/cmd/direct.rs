use crate::{
    JSPlatform, JVMPlatform,
    cmd::{CommandRunner, CommandRunnerPlatform},
    js_platform::{JsCodeGenOptions, JsPlatformMetadataOptions},
    jvm_platform::{JvmCodeGenOptions, JvmPlatformMetadataOptions},
    workspace::WorkspacePaths,
};
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile};
use argon_tasks::{CompileOptions, GenIrOptions, OptimizeOptions};
use argon_util::sync::ThreadSafe;
use embedded_io::Write;
use std::cell::OnceCell;
use std::mem::ManuallyDrop;

thread_local! {
    // Boa also owns thread-local runtime state. Keep the cached context alive until process exit
    // so Rust's unspecified thread-local destruction order cannot drop it after Boa's state.
    static JS_BACKEND: OnceCell<ManuallyDrop<argon_tasks::backend::NativeBackendJS>> =
        const { OnceCell::new() };
}

#[derive(Clone, Debug)]
pub struct DirectCommandRunner {
    workspace_paths: WorkspacePaths,
}

impl DirectCommandRunner {
    pub fn new(workspace_paths: WorkspacePaths) -> Self {
        Self { workspace_paths }
    }

    fn with_js_backend<T>(&self, f: impl FnOnce(&argon_tasks::backend::NativeBackendJS) -> T) -> T {
        JS_BACKEND.with(|backend| {
            f(backend.get_or_init(|| {
                ManuallyDrop::new(argon_tasks::backend::NativeBackendJS::new(
                    self.workspace_paths.root().join("backend/js/backend"),
                ))
            }))
        })
    }

    fn jvm_backend(&self) -> argon_tasks::backend::NativeBackendJVM {
        argon_tasks::backend::NativeBackendJVM::new(
            self.workspace_paths
                .root()
                .join("backend/jvm/backend/build/install/backend/lib"),
        )
    }
}

impl CommandRunner for DirectCommandRunner {
    fn compile<ID, IF, O, W>(
        &self,
        options: CompileOptions<ID, IF, O>,
        error_output: &mut W,
    ) -> bool
    where
        ID: InputDirectory + ThreadSafe,
        ID::File: ThreadSafe,
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write,
    {
        tokio::runtime::Runtime::new()
            .expect("create Tokio runtime")
            .block_on(argon_tasks::compile(options, error_output))
    }

    fn gen_ir<IF, O, W>(&self, options: GenIrOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write,
    {
        tokio::runtime::Runtime::new()
            .expect("create Tokio runtime")
            .block_on(argon_tasks::gen_ir(options, error_output))
    }

    fn optimize<IF, O, W>(&self, options: OptimizeOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write,
    {
        tokio::runtime::Runtime::new()
            .expect("create Tokio runtime")
            .block_on(argon_tasks::optimize(options, error_output))
    }
}

impl CommandRunnerPlatform<JSPlatform> for DirectCommandRunner {
    fn platform_metadata<I, O, W>(
        &self,
        options: JsPlatformMetadataOptions<I, O>,
        error_output: &mut W,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write,
    {
        self.with_js_backend(|backend| {
            tokio::runtime::Runtime::new()
                .expect("create Tokio runtime")
                .block_on(argon_tasks::platform_metadata_js(
                    backend,
                    argon_tasks::JsPlatformMetadataOptions {
                        package_name: None,
                        extern_files: options.extern_files,
                        output_file: options.output_file,
                    },
                    error_output,
                ))
        })
    }

    fn codegen<I, O, W>(&self, options: JsCodeGenOptions<I, O>, error_output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
        W: Write,
    {
        self.with_js_backend(|backend| {
            tokio::runtime::Runtime::new()
                .expect("create Tokio runtime")
                .block_on(argon_tasks::codegen_js(
                    backend,
                    argon_tasks::JsCodeGenOptions {
                        input_file: options.input_file,
                        output_dir: options.output_dir,
                        executable: options.executable,
                    },
                    error_output,
                ))
        })
    }
}

impl CommandRunnerPlatform<JVMPlatform> for DirectCommandRunner {
    fn platform_metadata<I, O, W>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
        error_output: &mut W,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write,
    {
        tokio::runtime::Runtime::new()
            .expect("create Tokio runtime")
            .block_on(argon_tasks::platform_metadata_jvm(
                &self.jvm_backend(),
                argon_tasks::JvmPlatformMetadataOptions {
                    extern_files: options.extern_files,
                    output_file: options.output_file,
                },
                error_output,
            ))
    }

    fn codegen<I, O, W>(&self, options: JvmCodeGenOptions<I, O>, error_output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
        W: Write,
    {
        let output_file = match options.output_dir.create_file(&options.output_name) {
            Ok(file) => file,
            Err(error) => {
                write_message(error_output, &error.to_string());
                return false;
            }
        };
        tokio::runtime::Runtime::new()
            .expect("create Tokio runtime")
            .block_on(argon_tasks::codegen_jvm(
                &self.jvm_backend(),
                argon_tasks::JvmCodegenOptions {
                    input_file: options.input_file,
                    output_file,
                    executable: options.executable,
                },
                error_output,
            ))
    }
}

fn write_message<W>(output: &mut W, message: &str)
where
    W: Write,
{
    let _ = output.write_all(message.as_bytes());
    if !message.ends_with('\n') {
        let _ = output.write_all(b"\n");
    }
    let _ = output.flush();
}
