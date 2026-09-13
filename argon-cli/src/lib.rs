pub mod backend;
pub mod options;

use crate::options::{
    CodeGenBackendCommand, Command, CommandLineOptions, OutputFormat,
    PlatformMetadataBackendCommand,
};
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use argon_tasks::backend::{NativeBackendJS, NativeBackendJVM};
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use argon_tasks::backend::{WasmBackendJS, WasmBackendJVM};
use argon_tasks::local_io::{LocalInputFile, LocalOutputFile, LocalSourceDirectory};
use clap::Parser;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use embedded_io::ErrorType;
use embedded_io::Write;
use std::ffi::OsString;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use wasm_bindgen::prelude::wasm_bindgen;

/// Run argonc asynchronously with command-line arguments, including the executable name.
pub async fn main<WO, WE>(args: Vec<OsString>, stdout: &mut WO, stderr: &mut WE) -> i32
where
    WO: Write,
    WE: Write,
{
    let options = match CommandLineOptions::try_parse_from(args) {
        Ok(options) => options,
        Err(error) => {
            let rendered = error.render().to_string();
            let _ = stderr.write_all(rendered.as_bytes());
            return error.exit_code();
        }
    };
    match options.output_format {
        OutputFormat::Text => {
            let mut logger = argon_tasks::message::TextTaskLogger::new(stdout);
            run(options.command, &mut logger).await
        }
        OutputFormat::ESExpr => {
            let mut logger = argon_tasks::message::ESExprTaskLogger::new(stdout);
            run(options.command, &mut logger).await
        }
    }
}

async fn run(command: Command, logger: &mut dyn argon_tasks::message::TaskLogger) -> i32 {
    let js_backend = create_js_backend();
    let jvm_backend = create_jvm_backend();
    let perl_backend = argon_tasks::PerlBackend::new();
    match command {
        Command::Compile(cmd) => {
            let output_file = LocalOutputFile::new(cmd.output_file.clone());
            let runner_options = argon_tasks::CompileOptions {
                tube_name: cmd.tube_name,
                input_dirs: cmd
                    .input_dirs
                    .into_iter()
                    .map(LocalSourceDirectory::new)
                    .collect(),
                referenced_tubes: cmd
                    .referenced_tubes
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                platform_metadata: cmd.platform.into_iter().map(LocalInputFile::new).collect(),
                output_file: output_file.clone(),
            };

            if !argon_tasks::compile(runner_options, logger).await {
                return 1;
            }
        }
        Command::GenIR(cmd) => {
            let runner_options = argon_tasks::GenIrOptions {
                input_tube: LocalInputFile::new(cmd.input_tube),
                referenced_tubes: cmd
                    .referenced_tubes
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                output_file: LocalOutputFile::new(cmd.output_file),
                platform: cmd.platform,
            };

            if !argon_tasks::gen_ir(runner_options, logger).await {
                return 1;
            }
        }
        Command::Optimize(cmd) => {
            let runner_options = argon_tasks::OptimizeOptions {
                input_file: LocalInputFile::new(cmd.input),
                referenced_tubes: cmd
                    .referenced_tubes
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                output_file: LocalOutputFile::new(cmd.output),
                optimizations: cmd.optimizations,
            };

            if !argon_tasks::optimize(runner_options, logger).await {
                return 1;
            }
        }
        Command::CodeGen(cmd) => match cmd.backend_command {
            CodeGenBackendCommand::Perl(cmd) => {
                if !argon_tasks::codegen_perl(
                    &perl_backend,
                    argon_tasks::PerlCodegenOptions {
                        input_file: LocalInputFile::new(cmd.input),
                        output_dir: argon_tasks::local_io::LocalOutputDirectory::new(cmd.output),
                        executable: cmd.executable,
                    },
                    logger,
                )
                .await
                {
                    return 1;
                }
            }
            CodeGenBackendCommand::JS(cmd) => {
                let success = argon_tasks::codegen_js(
                    &js_backend,
                    argon_tasks::JsCodeGenOptions {
                        input_file: LocalInputFile::new(cmd.input),
                        output_dir: argon_tasks::local_io::LocalOutputDirectory::new(cmd.output),
                        executable: cmd.executable,
                    },
                    logger,
                )
                .await;
                if !success {
                    return 1;
                }
            }
            CodeGenBackendCommand::JVM(cmd) => {
                let success = argon_tasks::codegen_jvm(
                    &jvm_backend,
                    argon_tasks::JvmCodegenOptions {
                        input_file: LocalInputFile::new(cmd.input),
                        output_file: LocalOutputFile::new(cmd.output),
                        executable: cmd.executable,
                    },
                    logger,
                )
                .await;
                if !success {
                    return 1;
                }
            }
        },
        Command::PlatformMetadata(cmd) => match cmd.backend_command {
            PlatformMetadataBackendCommand::Perl(cmd) => {
                if !argon_tasks::platform_metadata_perl(
                    &perl_backend,
                    argon_tasks::PerlPlatformMetadataOptions {
                        distribution_name: cmd.distribution_name,
                        root_package: cmd.root_package,
                        extern_files: cmd
                            .extern_files
                            .into_iter()
                            .map(LocalInputFile::new)
                            .collect(),
                        output_file: LocalOutputFile::new(cmd.output_file),
                    },
                    logger,
                )
                .await
                {
                    return 1;
                }
            }
            PlatformMetadataBackendCommand::JS(cmd) => {
                let success = argon_tasks::platform_metadata_js(
                    &js_backend,
                    argon_tasks::JsPlatformMetadataOptions {
                        package_name: cmd.package_name,
                        extern_files: cmd
                            .extern_files
                            .into_iter()
                            .map(LocalInputFile::new)
                            .collect(),
                        output_file: LocalOutputFile::new(cmd.output_file),
                    },
                    logger,
                )
                .await;
                if !success {
                    return 1;
                }
            }
            PlatformMetadataBackendCommand::JVM(cmd) => {
                let success = argon_tasks::platform_metadata_jvm(
                    &jvm_backend,
                    argon_tasks::JvmPlatformMetadataOptions {
                        extern_files: cmd
                            .extern_files
                            .into_iter()
                            .map(LocalInputFile::new)
                            .collect(),
                        output_file: LocalOutputFile::new(cmd.output_file),
                    },
                    logger,
                )
                .await;
                if !success {
                    return 1;
                }
            }
        },
    }

    0
}

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub fn js_backend_path() -> std::path::PathBuf {
    let mut path = std::env::current_exe().expect("failed to get current executable path");
    path.pop();
    path.pop();
    path.pop();
    let dist_path = path.join("backend/js");
    if dist_path.join("lib/index.js").is_file() {
        dist_path
    } else {
        path.join("backend/js/backend")
    }
}

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
fn create_js_backend() -> NativeBackendJS {
    NativeBackendJS::new(js_backend_path())
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
fn create_js_backend() -> WasmBackendJS {
    WasmBackendJS::new()
}

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
fn create_jvm_backend() -> NativeBackendJVM {
    let mut path = std::env::current_exe().expect("failed to get current executable path");
    path.pop();
    path.pop();
    path.pop();
    let dist_path = path.join("backend/jvm");
    if dist_path.join("backend-0.1.0.jar").is_file() {
        NativeBackendJVM::new(dist_path)
    } else {
        NativeBackendJVM::new(path.join("backend/jvm/backend/build/install/backend/lib"))
    }
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
fn create_jvm_backend() -> WasmBackendJVM {
    WasmBackendJVM::new()
}

/// Run argonc from JavaScript with command-line arguments represented as strings.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
#[wasm_bindgen(js_name = main)]
pub async fn wasm_main(
    args: Vec<String>,
    stdout: JavaScriptWriter,
    stderr: JavaScriptWriter,
) -> i32 {
    let mut stdout = WasmWriter::new(stdout);
    let mut stderr = WasmWriter::new(stderr);
    main(
        args.into_iter().map(OsString::from).collect(),
        &mut stdout,
        &mut stderr,
    )
    .await
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
struct WasmWriter {
    writer: JavaScriptWriter,
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
impl WasmWriter {
    fn new(writer: JavaScriptWriter) -> Self {
        Self { writer }
    }
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
impl ErrorType for WasmWriter {
    type Error = core::convert::Infallible;
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
impl Write for WasmWriter {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        self.writer.write(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
#[wasm_bindgen]
extern "C" {
    pub type JavaScriptWriter;

    #[wasm_bindgen(method, structural, js_name = write)]
    fn write(this: &JavaScriptWriter, bytes: &[u8]);
}
