use crate::{
    JSPlatform, JVMPlatform,
    cmd::{
        CommandRunner, CommandRunnerPlatform,
        backend_options::{
            js_codegen_args, js_platform_metadata_args, jvm_codegen_args,
            jvm_platform_metadata_args,
        },
        staging::{
            copy_temp_output, copy_temp_output_dir, stage_input_dirs, stage_input_file,
            stage_input_files,
        },
    },
    js_platform::{JsCodeGenOptions, JsPlatformMetadataOptions},
    jvm_platform::{JvmCodeGenOptions, JvmPlatformMetadataOptions},
    workspace::WorkspacePaths,
};
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile, Write};
use argon_runner::{CompileOptions, GenIrOptions};
use argon_util::sync::ThreadSafe;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::process::Command;

pub struct CliCommandRunner {
    workspace_paths: WorkspacePaths,
}

impl CliCommandRunner {
    pub fn new(workspace_paths: WorkspacePaths) -> CliCommandRunner {
        CliCommandRunner { workspace_paths }
    }
}

impl CommandRunner for CliCommandRunner {
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
        match self.compile_staged(options) {
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

    fn gen_ir<IF, O, W>(&self, options: GenIrOptions<IF, O>, error_output: &mut W) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
        W: Write,
    {
        match self.gen_ir_staged(options) {
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

impl CommandRunnerPlatform<JSPlatform> for CliCommandRunner {
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

impl CommandRunnerPlatform<JVMPlatform> for CliCommandRunner {
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

impl CliCommandRunner {
    fn compile_staged<ID, IF, O>(
        &self,
        options: CompileOptions<ID, IF, O>,
    ) -> Result<String, String>
    where
        ID: InputDirectory,
        IF: InputFile,
        O: OutputFile,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let input_dirs = stage_input_dirs(temp_path, options.input_dirs)?;
        let referenced_tubes = stage_input_files(temp_path, "reference", options.referenced_tubes)?;
        let platform_metadata =
            stage_input_files(temp_path, "platform", options.platform_metadata)?;
        let output_file = temp_path.join("output.artube");

        let mut args = vec![
            "compile".into(),
            "--tube-name".into(),
            options.tube_name.to_string().into(),
        ];
        for input_dir in input_dirs {
            args.push("--input".into());
            args.push(input_dir.into_os_string());
        }
        for reference in referenced_tubes {
            args.push("--reference".into());
            args.push(reference.into_os_string());
        }
        for metadata in platform_metadata {
            args.push("--platform".into());
            args.push(metadata.into_os_string());
        }
        args.push("--output".into());
        args.push(output_file.clone().into_os_string());

        let command_output = self.run_argonc_command(&args)?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(command_output)
    }

    fn gen_ir_staged<IF, O>(&self, options: GenIrOptions<IF, O>) -> Result<String, String>
    where
        IF: InputFile,
        O: OutputFile,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let input_tube = stage_input_file(temp_path, "input-tube", 0, options.input_tube)?;
        let referenced_tubes = stage_input_files(temp_path, "reference", options.referenced_tubes)?;
        let output_file = temp_path.join("output.arvm");

        let mut args = vec![
            "genir".into(),
            "--input".into(),
            input_tube.into_os_string(),
            "--platform".into(),
            options.platform.into(),
            "--output".into(),
            output_file.clone().into_os_string(),
        ];
        for reference in referenced_tubes {
            args.push("--reference".into());
            args.push(reference.into_os_string());
        }

        let command_output = self.run_argonc_command(&args)?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(command_output)
    }

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

        let command_output = self.run_argonc_command(&js_platform_metadata_args(
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

        let command_output = self.run_argonc_command(&js_codegen_args(
            input_file,
            output_dir.clone(),
            options.executable.as_deref(),
        ))?;

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

        let command_output = self.run_argonc_command(&jvm_platform_metadata_args(
            extern_files,
            output_file.clone(),
        ))?;
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

        let command_output = self.run_argonc_command(&jvm_codegen_args(
            input_file,
            output_file.clone(),
            options.executable,
        ))?;

        copy_temp_output(
            &output_file,
            options
                .output_dir
                .create_file(&options.output_name)
                .map_err(|err| err.to_string())?,
        )?;

        Ok(command_output)
    }

    fn run_argonc_command<S: AsRef<OsStr>>(&self, args: &[S]) -> Result<String, String> {
        let mut command = Command::new(self.argon_bin());
        command.args(args.iter().map(AsRef::as_ref));

        let output = command.output().map_err(|err| {
            format!(
                "failed to run argon command {}: {}",
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
                "argon command failed: {}\n{}",
                format_command(&command),
                command_output
            ))
        }
    }

    fn argon_bin(&self) -> PathBuf {
        self.workspace_paths.argon_bin()
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
