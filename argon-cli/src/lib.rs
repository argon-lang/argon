pub mod backend;
pub mod options;

use crate::backend::Backend;
use crate::options::{
    CodeGenBackendCommand, Command, CommandLineOptions, PlatformMetadataBackendCommand,
};
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use argon_tasks::backend::NativeBackendJS;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use argon_tasks::backend::wasm::WasmBackendJS;
use argon_tasks::local_io::{LocalInputFile, LocalOutputFile, LocalSourceDirectory};
use clap::Parser;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use embedded_io::ErrorType;
use embedded_io::Write;
use std::ffi::OsString;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use wasm_bindgen::prelude::wasm_bindgen;

/// Run argonc asynchronously with command-line arguments, including the executable name.
pub async fn main<W>(args: Vec<OsString>, stdout: &mut W) -> i32
where
    W: Write,
{
    let js_backend = create_js_backend();
    let options = match CommandLineOptions::try_parse_from(args.clone()) {
        Ok(options) => options,
        Err(error) => {
            let rendered = error.render().to_string();
            let _ = stdout.write_all(rendered.as_bytes());
            return error.exit_code();
        }
    };
    match options.command {
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

            if !argon_tasks::compile(runner_options, stdout) {
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

            if !argon_tasks::gen_ir(runner_options, stdout) {
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

            if !argon_tasks::optimize(runner_options, stdout) {
                return 1;
            }
        }
        Command::CodeGen(cmd) => match cmd.backend_command {
            CodeGenBackendCommand::JS(cmd) => {
                let success = argon_tasks::codegen_js(
                    &js_backend,
                    argon_tasks::JsCodeGenOptions {
                        input_file: LocalInputFile::new(cmd.input),
                        output_dir: argon_tasks::local_io::LocalOutputDirectory::new(cmd.output),
                        executable: cmd.executable,
                    },
                    stdout,
                )
                .await;
                if !success {
                    return 1;
                }
            }
            CodeGenBackendCommand::JVM(_) => {
                return execute_backend_subcommand(Backend::JVM, &args);
            }
        },
        Command::PlatformMetadata(cmd) => match cmd.backend_command {
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
                    stdout,
                )
                .await;
                if !success {
                    return 1;
                }
            }
            PlatformMetadataBackendCommand::JVM(_) => {
                return execute_backend_subcommand(Backend::JVM, &args);
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

/// Run argonc from JavaScript with command-line arguments represented as strings.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
#[wasm_bindgen(js_name = main)]
pub async fn wasm_main(args: Vec<String>, stdout: JavaScriptWriter) -> i32 {
    let mut stdout = WasmWriter::new(stdout);
    main(args.into_iter().map(OsString::from).collect(), &mut stdout).await
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
        let text = core::str::from_utf8(buf).expect("argonc output must be valid UTF-8");
        self.writer.write(text);
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
    fn write(this: &JavaScriptWriter, text: &str);
}

#[cfg(unix)]
fn execute_backend_subcommand(backend: Backend, args: &[OsString]) -> i32 {
    use std::os::unix::process::CommandExt;
    use std::process::Command;

    let mut command;

    let mut executable_path =
        std::env::current_exe().expect("failed to get current executable path");
    executable_path.pop();
    executable_path.pop();
    executable_path.pop();

    match backend {
        Backend::JavaScript => {
            executable_path.push("backend/js/lib/main.js");

            command = Command::new("node");
            command.arg(executable_path);
        }
        Backend::JVM => {
            executable_path.push("backend/jvm");

            command = Command::new("java");
            command
                .arg("--module-path")
                .arg(executable_path)
                .arg("--module")
                .arg("dev.argon.backend");
        }
    }

    command.args(args.iter().skip(1));
    let error = command.exec();

    eprintln!("unable to execute backend subcommand: {}", error);
    1
}

#[cfg(not(unix))]
fn execute_backend_subcommand(_backend: Backend, _args: &[OsString]) -> i32 {
    eprintln!("backend subcommands are not implemented on this platform");
    1
}
