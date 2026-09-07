pub mod backend;
pub mod options;

use crate::backend::Backend;
use crate::options::{
    CodeGenBackendCommand, Command, CommandLineOptions, PlatformMetadataBackendCommand,
};
use argon_tasks::local_io::{LocalInputFile, LocalOutputFile, LocalSourceDirectory};
use clap::Parser;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use embedded_io::ErrorType;
use embedded_io::Write;
use std::ffi::OsString;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use wasm_bindgen::prelude::wasm_bindgen;

/// Run argonc with command-line arguments, including the executable name.
pub fn main<W>(args: Vec<OsString>, stdout: &mut W)
where
    W: Write,
{
    let options = match CommandLineOptions::try_parse_from(args.clone()) {
        Ok(options) => options,
        Err(error) => {
            let rendered = error.render().to_string();
            let _ = stdout.write_all(rendered.as_bytes());
            #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
            std::process::exit(error.exit_code());
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            return;
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
                std::process::exit(1);
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
                std::process::exit(1);
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
                std::process::exit(1);
            }
        }
        Command::CodeGen(cmd) => match cmd.backend_command {
            CodeGenBackendCommand::JS(_) => {
                execute_backend_subcommand(Backend::JavaScript, &args);
            }
            CodeGenBackendCommand::JVM(_) => {
                execute_backend_subcommand(Backend::JVM, &args);
            }
        },
        Command::PlatformMetadata(cmd) => match cmd.backend_command {
            PlatformMetadataBackendCommand::JS(_) => {
                execute_backend_subcommand(Backend::JavaScript, &args);
            }
            PlatformMetadataBackendCommand::JVM(_) => {
                execute_backend_subcommand(Backend::JVM, &args);
            }
        },
    }
}

/// Run argonc from JavaScript with command-line arguments represented as strings.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
#[wasm_bindgen(js_name = main)]
pub fn wasm_main(args: Vec<String>, stdout: JavaScriptWriter) {
    let mut stdout = WasmWriter::new(stdout);
    main(args.into_iter().map(OsString::from).collect(), &mut stdout);
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
fn execute_backend_subcommand(backend: Backend, args: &[OsString]) {
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
    std::process::exit(1);
}

#[cfg(not(unix))]
fn execute_backend_subcommand(_backend: Backend, _args: &[OsString]) {
    eprintln!("backend subcommands are not implemented on this platform");
}
