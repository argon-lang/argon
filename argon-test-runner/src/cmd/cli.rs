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
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile};
use argon_tasks::{
    CompileOptions, GenIrOptions, OptimizeOptions,
    message::{TaskLogger, TaskMessage, task_error},
};
use argon_util::sync::ThreadSafe;
use esexpr::ESExprCodec;
use esexpr_binary::ExprParserSync;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::process::{Command, Stdio};

pub struct CliCommandRunner {
    workspace_paths: WorkspacePaths,
}

impl CliCommandRunner {
    pub fn new(workspace_paths: WorkspacePaths) -> CliCommandRunner {
        CliCommandRunner { workspace_paths }
    }
}

impl CommandRunner for CliCommandRunner {
    fn compile<ID, IF, O>(
        &self,
        options: CompileOptions<ID, IF, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        ID: InputDirectory + ThreadSafe,
        ID::File: ThreadSafe,
        IF: InputFile + ThreadSafe,
        O: OutputFile,
    {
        let result = self.compile_staged(options, logger);
        log_result(logger, result)
    }

    fn gen_ir<IF, O>(&self, options: GenIrOptions<IF, O>, logger: &mut dyn TaskLogger) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
    {
        let result = self.gen_ir_staged(options, logger);
        log_result(logger, result)
    }

    fn optimize<IF, O>(&self, options: OptimizeOptions<IF, O>, logger: &mut dyn TaskLogger) -> bool
    where
        IF: InputFile + ThreadSafe,
        O: OutputFile,
    {
        let result = self.optimize_staged(options, logger);
        log_result(logger, result)
    }
}

impl CommandRunnerPlatform<JSPlatform> for CliCommandRunner {
    fn platform_metadata<I, O>(
        &self,
        options: JsPlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
    {
        let result = self.platform_metadata_staged(options, logger);
        log_result(logger, result)
    }

    fn codegen<I, O>(&self, options: JsCodeGenOptions<I, O>, logger: &mut dyn TaskLogger) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
    {
        let result = self.codegen_staged(options, logger);
        log_result(logger, result)
    }
}

impl CommandRunnerPlatform<JVMPlatform> for CliCommandRunner {
    fn platform_metadata<I, O>(
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
        let result = self.jvm_platform_metadata_staged(options, logger);
        log_result(logger, result)
    }

    fn codegen<I, O>(&self, options: JvmCodeGenOptions<I, O>, logger: &mut dyn TaskLogger) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputDirectory + 'static,
        O::File: OutputFile + 'static,
        <O::File as OutputFile>::Writer: 'static,
    {
        let result = self.jvm_codegen_staged(options, logger);
        log_result(logger, result)
    }
}

impl CliCommandRunner {
    fn compile_staged<ID, IF, O>(
        &self,
        options: CompileOptions<ID, IF, O>,
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String>
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

        self.run_argonc_command(&args, logger)?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(())
    }

    fn gen_ir_staged<IF, O>(
        &self,
        options: GenIrOptions<IF, O>,
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String>
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

        self.run_argonc_command(&args, logger)?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(())
    }

    fn optimize_staged<IF, O>(
        &self,
        options: OptimizeOptions<IF, O>,
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String>
    where
        IF: InputFile,
        O: OutputFile,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let input_file = stage_input_file(temp_path, "input", 0, options.input_file)?;
        let referenced_tubes = stage_input_files(temp_path, "reference", options.referenced_tubes)?;
        let output_file = temp_path.join("output.arvm");

        let mut args = vec![
            "optimize".into(),
            "--input".into(),
            input_file.into_os_string(),
            "--output".into(),
            output_file.clone().into_os_string(),
        ];
        for reference in referenced_tubes {
            args.push("--reference".into());
            args.push(reference.into_os_string());
        }
        for optimization in options.optimizations {
            args.push("--optimization".into());
            args.push(optimization.into());
        }

        self.run_argonc_command(&args, logger)?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(())
    }

    fn platform_metadata_staged<I, O>(
        &self,
        options: JsPlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String>
    where
        I: InputFile,
        O: OutputFile,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let extern_files = stage_input_files(temp_path, "extern", options.extern_files)?;
        let output_file = temp_path.join("platform-metadata.esx");

        self.run_argonc_command(
            &js_platform_metadata_args(extern_files, output_file.clone()),
            logger,
        )?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(())
    }

    fn codegen_staged<I, O>(
        &self,
        options: JsCodeGenOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String>
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

        self.run_argonc_command(
            &js_codegen_args(
                input_file,
                output_dir.clone(),
                options.executable.as_deref(),
            ),
            logger,
        )?;

        copy_temp_output_dir(&output_dir, options.output_dir)?;

        Ok(())
    }

    fn jvm_platform_metadata_staged<I, O>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String>
    where
        I: InputFile,
        O: OutputFile,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let extern_files = stage_input_files(temp_path, "extern", options.extern_files)?;
        let output_file = temp_path.join("platform-metadata.esx");

        self.run_argonc_command(
            &jvm_platform_metadata_args(extern_files, output_file.clone()),
            logger,
        )?;
        copy_temp_output(&output_file, options.output_file)?;

        Ok(())
    }

    fn jvm_codegen_staged<I, O>(
        &self,
        options: JvmCodeGenOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String>
    where
        I: InputFile,
        O: OutputDirectory,
    {
        let temp_dir = tempfile::TempDir::new()
            .map_err(|err| format!("failed to create temporary command directory: {err}"))?;
        let temp_path = temp_dir.path();

        let input_file = stage_input_file(temp_path, "input", 0, options.input_file)?;
        let output_file = temp_path.join("output.jar");

        self.run_argonc_command(
            &jvm_codegen_args(input_file, output_file.clone(), options.executable),
            logger,
        )?;

        copy_temp_output(
            &output_file,
            options
                .output_dir
                .create_file(&options.output_name)
                .map_err(|err| err.to_string())?,
        )?;

        Ok(())
    }

    fn run_argonc_command<S: AsRef<OsStr>>(
        &self,
        args: &[S],
        logger: &mut dyn TaskLogger,
    ) -> Result<(), String> {
        let mut command = Command::new(self.argon_bin());
        command
            .arg("--output-format")
            .arg("esexpr")
            .args(args.iter().map(AsRef::as_ref))
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let command_text = format_command(&command);
        let mut child = command
            .spawn()
            .map_err(|err| format!("failed to run argon command {}: {}", command_text, err))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| "argon command stdout was not piped".to_owned())?;
        let mut stdout = StdRead(stdout);
        let mut parser = esexpr_binary::parse_sync(&mut stdout);
        while let Some(expression) = parser.try_read_next_expr().map_err(|error| {
            format!("invalid task message stream from {command_text}: {error:?}")
        })? {
            let message = TaskMessage::decode_esexpr(expression)
                .map_err(|error| format!("invalid task message from {command_text}: {error:?}"))?;
            logger.log(message);
        }
        let status = child
            .wait()
            .map_err(|err| format!("failed to wait for argon command {command_text}: {err}"))?;

        status
            .success()
            .then_some(())
            .ok_or_else(|| format!("argon command failed: {command_text}"))
    }

    fn argon_bin(&self) -> PathBuf {
        self.workspace_paths.argon_bin()
    }
}

struct StdRead<R>(R);

impl<R> embedded_io::ErrorType for StdRead<R> {
    type Error = std::io::Error;
}

impl<R: std::io::Read> embedded_io::Read for StdRead<R> {
    fn read(&mut self, buffer: &mut [u8]) -> Result<usize, Self::Error> {
        self.0.read(buffer)
    }
}

fn log_result(logger: &mut dyn TaskLogger, result: Result<(), String>) -> bool {
    match result {
        Ok(()) => true,
        Err(error) => {
            logger.log(task_error(error));
            false
        }
    }
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
