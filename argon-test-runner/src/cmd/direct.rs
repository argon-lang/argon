use crate::{
    JSPlatform,
    cmd::{
        CommandRunner, CommandRunnerPlatform,
        backend_options::{js_codegen_args, js_platform_metadata_args},
        staging::{copy_temp_output, copy_temp_output_dir, stage_input_file, stage_input_files},
    },
    js_platform::{JsCodeGenOptions, JsPlatformMetadataOptions},
    workspace::WorkspacePaths,
};
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile, Write};
use argon_runner::{CompileOptions, GenIrOptions};
use argon_util::sync::ThreadSafe;
use std::ffi::OsStr;
use std::path::PathBuf;
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
        argon_runner::compile(options, error_output)
    }

    fn gen_ir<IF, O, W>(&self, options: GenIrOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write,
    {
        argon_runner::gen_ir(options, error_output)
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

        let command_output = self.run_js_backend_command(&js_platform_metadata_args(
            extern_files,
            output_file.clone(),
        ))?;
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

        let command_output =
            self.run_js_backend_command(&js_codegen_args(input_file, output_dir.clone()))?;

        copy_temp_output_dir(&output_dir, options.output_dir)?;

        Ok(command_output)
    }

    fn run_js_backend_command<S: AsRef<OsStr>>(&self, args: &[S]) -> Result<String, String> {
        let mut command = Command::new("node");
        command.arg(self.js_backend_bin());
        command.args(args.iter().map(AsRef::as_ref));

        let output = command.output().map_err(|err| {
            format!(
                "failed to run JavaScript backend command {}: {}",
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
                "JavaScript backend command failed: {}\n{}",
                format_command(&command),
                command_output
            ))
        }
    }

    fn js_backend_bin(&self) -> PathBuf {
        self.workspace_paths
            .root()
            .join("backend/backends/js/lib/main.js")
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
