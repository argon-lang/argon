use crate::{
    JSPlatform, JVMPlatform,
    cmd::{
        CommandRunner, CommandRunnerPlatform,
        backend_options::{
            js_codegen_args, js_platform_metadata_args, jvm_codegen_args,
            jvm_platform_metadata_args,
        },
        staging::{copy_temp_output, copy_temp_output_dir, stage_input_file, stage_input_files},
    },
    js_platform::{JsCodeGenOptions, JsPlatformMetadataOptions},
    jvm_platform::{JvmCodeGenOptions, JvmPlatformMetadataOptions},
    workspace::WorkspacePaths,
};
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile, Write};
use argon_tasks::{CompileOptions, GenIrOptions, OptimizeOptions};
use argon_util::sync::ThreadSafe;
use std::ffi::OsStr;
use std::process::Command;

#[derive(Clone, Debug)]
pub struct DirectCommandRunner {
    workspace_paths: WorkspacePaths,
}

impl DirectCommandRunner {
    pub fn new(workspace_paths: WorkspacePaths) -> Self {
        Self { workspace_paths }
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
        argon_tasks::compile(options, error_output)
    }

    fn gen_ir<IF, O, W>(&self, options: GenIrOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write,
    {
        argon_tasks::gen_ir(options, error_output)
    }

    fn optimize<IF, O, W>(&self, options: OptimizeOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write,
    {
        argon_tasks::optimize(options, error_output)
    }
}

impl CommandRunnerPlatform<JSPlatform> for DirectCommandRunner {
    fn platform_metadata<I, O, W>(
        &self,
        options: JsPlatformMetadataOptions<I, O>,
        error_output: &mut W,
    ) -> bool
    where
        I: InputFile,
        O: OutputFile,
        W: Write,
    {
        match self.platform_metadata_staged(options) {
            Ok(output) => {
                write_message(error_output, &output);
                true
            }
            Err(err) => {
                write_message(error_output, &err);
                false
            }
        }
    }

    fn codegen<I, O, W>(&self, options: JsCodeGenOptions<I, O>, error_output: &mut W) -> bool
    where
        I: InputFile,
        O: OutputDirectory,
        W: Write,
    {
        match self.codegen_staged(options) {
            Ok(output) => {
                write_message(error_output, &output);
                true
            }
            Err(err) => {
                write_message(error_output, &err);
                false
            }
        }
    }
}

impl CommandRunnerPlatform<JVMPlatform> for DirectCommandRunner {
    fn platform_metadata<I, O, W>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
        error_output: &mut W,
    ) -> bool
    where
        I: InputFile,
        O: OutputFile,
        W: Write,
    {
        match self.jvm_platform_metadata_staged(options) {
            Ok(output) => {
                write_message(error_output, &output);
                true
            }
            Err(err) => {
                write_message(error_output, &err);
                false
            }
        }
    }

    fn codegen<I, O, W>(&self, options: JvmCodeGenOptions<I, O>, error_output: &mut W) -> bool
    where
        I: InputFile,
        O: OutputDirectory,
        W: Write,
    {
        match self.jvm_codegen_staged(options) {
            Ok(output) => {
                write_message(error_output, &output);
                true
            }
            Err(err) => {
                write_message(error_output, &err);
                false
            }
        }
    }
}

impl DirectCommandRunner {
    fn platform_metadata_staged<I, O>(
        &self,
        options: JsPlatformMetadataOptions<I, O>,
    ) -> Result<String, String>
    where
        I: InputFile,
        O: OutputFile,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let extern_files = stage_input_files(temp_path, "extern", options.extern_files)?;
        let output_file = temp_path.join("platform-metadata.esx");

        let command_output = self.run_backend_command(
            BackendCommand::JavaScript,
            &js_platform_metadata_args(extern_files, output_file.clone()),
        )?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(command_output)
    }

    fn codegen_staged<I, O>(&self, options: JsCodeGenOptions<I, O>) -> Result<String, String>
    where
        I: InputFile,
        O: OutputDirectory,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let input_file = stage_input_file(temp_path, "input", 0, options.input_file)?;
        let output_dir = temp_path.join("output");
        std::fs::create_dir_all(&output_dir).map_err(|err| {
            format!(
                "failed to create temporary output directory {}: {err}",
                output_dir.display()
            )
        })?;

        let command_output = self.run_backend_command(
            BackendCommand::JavaScript,
            &js_codegen_args(
                input_file,
                output_dir.clone(),
                options.executable.as_deref(),
            ),
        )?;

        copy_temp_output_dir(&output_dir, options.output_dir)?;

        Ok(command_output)
    }

    fn jvm_platform_metadata_staged<I, O>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
    ) -> Result<String, String>
    where
        I: InputFile,
        O: OutputFile,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let extern_files = stage_input_files(temp_path, "extern", options.extern_files)?;
        let output_file = temp_path.join("platform-metadata.esx");

        let command_output = self.run_backend_command(
            BackendCommand::JVM,
            &jvm_platform_metadata_args(extern_files, output_file.clone()),
        )?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(command_output)
    }

    fn jvm_codegen_staged<I, O>(&self, options: JvmCodeGenOptions<I, O>) -> Result<String, String>
    where
        I: InputFile,
        O: OutputDirectory,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let input_file = stage_input_file(temp_path, "input", 0, options.input_file)?;
        let output_file = temp_path.join("output.jar");

        let command_output = self.run_backend_command(
            BackendCommand::JVM,
            &jvm_codegen_args(input_file, output_file.clone(), options.executable),
        )?;

        copy_temp_output(
            &output_file,
            options
                .output_dir
                .create_file(&options.output_name)
                .map_err(|err| err.to_string())?,
        )?;

        Ok(command_output)
    }

    fn run_backend_command<S: AsRef<OsStr>>(
        &self,
        backend: BackendCommand,
        args: &[S],
    ) -> Result<String, String> {
        let mut command = self.backend_command(backend);
        command.args(args.iter().map(AsRef::as_ref));

        let output = command.output().map_err(|err| {
            format!(
                "failed to run backend command {}: {}",
                format_command(&command),
                err
            )
        })?;

        let command_output = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );

        if output.status.success() {
            Ok(command_output)
        } else {
            Err(format!(
                "backend command failed: {}\n{}",
                format_command(&command),
                command_output
            ))
        }
    }

    fn backend_command(&self, backend: BackendCommand) -> Command {
        match backend {
            BackendCommand::JavaScript => {
                let mut command = Command::new("node");
                command.arg(
                    self.workspace_paths
                        .root()
                        .join("backend/js/backend/lib/main.js"),
                );
                command
            }
            BackendCommand::JVM => {
                let mut command = Command::new("java");
                command
                    .arg("--module-path")
                    .arg(
                        self.workspace_paths
                            .root()
                            .join("backend/jvm/backend/build/install/backend/lib"),
                    )
                    .arg("--module")
                    .arg("dev.argon.backend");
                command
            }
        }
    }
}

#[derive(Clone, Copy)]
enum BackendCommand {
    JavaScript,
    JVM,
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

fn format_command(command: &Command) -> String {
    std::iter::once(command.get_program())
        .chain(command.get_args())
        .map(format_arg)
        .collect::<Vec<_>>()
        .join(" ")
}

fn format_arg(arg: &OsStr) -> String {
    let arg = arg.to_string_lossy();

    if !arg.is_empty()
        && arg.chars().all(|ch| {
            ch.is_ascii_alphanumeric() || matches!(ch, '/' | '\\' | '.' | '-' | '_' | ':' | '=')
        })
    {
        return arg.into_owned();
    }

    format!("'{}'", arg.replace('\'', "'\"'\"'"))
}
