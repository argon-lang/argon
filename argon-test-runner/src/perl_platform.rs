use crate::{
    CompileTargetPlatform, LibraryInfo, TestContext, TestExecutionResult,
    cmd::{CommandRunner, CommandRunnerPlatform, TaskMessageLog},
};
use argon_tasks::local_io::{LocalInputFile, LocalOutputDirectory, LocalOutputFile};
use fs_extra::dir::CopyOptions;
use hashbrown::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

pub struct PerlPlatform;

#[derive(Default)]
pub struct PerlPlatformState {
    library_codegen: Mutex<HashMap<String, PathBuf>>,
}

pub struct PerlPlatformMetadataOptions<I, O> {
    pub extern_files: Vec<I>,
    pub output_file: O,
}

pub struct PerlCodeGenOptions<I, O> {
    pub input_file: I,
    pub output_dir: O,
    pub executable: Option<String>,
}

impl PerlPlatform {
    fn codegen_library<R: CommandRunner + CommandRunnerPlatform<Self>>(
        &self,
        library_info: &LibraryInfo<Self, R>,
    ) -> PathBuf {
        let mut generated = library_info
            .test_suite_context
            .platform_state
            .library_codegen
            .lock()
            .unwrap_or_else(|error| error.into_inner());
        let context = library_info.test_suite_context.clone();
        generated
            .entry_ref(&library_info.name)
            .or_insert_with(|| {
                let library = context.library_info(&library_info.name);
                let input_file = library
                    .test_suite_context
                    .clone()
                    .genir_library_tube(&library_info.name);
                let output_dir = library.library_output_path.join("perl");
                std::fs::create_dir_all(&output_dir).unwrap();
                let mut output = TaskMessageLog::default();
                let success = library.test_suite_context.command_runner.codegen(
                    PerlCodeGenOptions {
                        input_file: LocalInputFile::new(input_file),
                        output_dir: LocalOutputDirectory::new(output_dir.clone()),
                        executable: None,
                    },
                    &mut output,
                );
                assert!(
                    success,
                    "Code generation of library {} failed\n{}",
                    library.name, output
                );
                if library.test_suite_context.print_commands && !output.is_empty() {
                    print!("{output}");
                }
                output_dir
            })
            .clone()
    }

    fn copy_tree(source: &Path, destination: &Path) {
        if !source.is_dir() {
            return;
        }
        std::fs::create_dir_all(destination).unwrap();
        let mut options = CopyOptions::new();
        options.content_only = true;
        options.overwrite = true;
        fs_extra::dir::copy(source, destination, &options).unwrap();
    }
}

impl CompileTargetPlatform for PerlPlatform {
    type PlatformState = PerlPlatformState;
    type PlatformMetadataOptions<I, O> = PerlPlatformMetadataOptions<I, O>;
    type CodeGenOptions<I, O> = PerlCodeGenOptions<I, O>;

    const ID: &'static str = "perl";

    fn library_platform_metadata<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        library: &LibraryInfo<Self, R>,
    ) -> Vec<PathBuf> {
        let path = library.library_output_path.join("platform-metadata.esx");
        let mut extern_files = Vec::new();
        let perl_dir = library.library_path.join("perl");
        if perl_dir.is_dir() {
            for entry in walkdir::WalkDir::new(perl_dir) {
                let entry = entry.unwrap();
                if entry.file_type().is_file() {
                    extern_files.push(LocalInputFile::new(entry.into_path()));
                }
            }
        }
        let mut output = TaskMessageLog::default();
        let success = library.test_suite_context.command_runner.platform_metadata(
            PerlPlatformMetadataOptions {
                extern_files,
                output_file: LocalOutputFile::new(path.clone()),
            },
            &mut output,
        );
        assert!(
            success,
            "Getting metadata for library {} failed:\n{}",
            library.name, output
        );
        if library.test_suite_context.print_commands && !output.is_empty() {
            print!("{output}");
        }
        vec![path]
    }

    fn codegen<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        test_context: &TestContext<Self, R>,
    ) {
        let output_dir = test_context.test_data_dir.join("perl");
        std::fs::create_dir_all(&output_dir).unwrap();
        Self::copy_tree(
            &test_context
                .test_suite_context
                .backend_dir
                .join("perl/runtime/lib"),
            &output_dir.join("lib"),
        );
        for library_name in test_context.referenced_libraries() {
            let library_dir = self.codegen_library(
                &test_context
                    .test_suite_context
                    .clone()
                    .library_info(&library_name),
            );
            Self::copy_tree(&library_dir.join("lib"), &output_dir.join("lib"));
        }
        let mut output = TaskMessageLog::default();
        let success = test_context.test_suite_context.command_runner.codegen(
            PerlCodeGenOptions {
                input_file: LocalInputFile::new(test_context.genir_test_case()),
                output_dir: LocalOutputDirectory::new(output_dir),
                executable: Some("test-program".to_owned()),
            },
            &mut output,
        );
        assert!(success, "Code generation of test case failed\n{output}");
        if test_context.test_suite_context.print_commands && !output.is_empty() {
            print!("{output}");
        }
    }

    fn run<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        test_context: &TestContext<Self, R>,
    ) -> TestExecutionResult {
        let output_dir = test_context.test_data_dir.join("perl");
        let output = subprocess::Exec::cmd("perl")
            .arg(format!("-I{}", output_dir.join("lib").display()))
            .arg(output_dir.join("script/test-program"))
            .cwd(test_context.run_dir())
            .stderr(subprocess::Redirection::Merge)
            .capture()
            .unwrap();
        let exit_code = output.exit_status.code().map_or_else(
            || i32::from(!output.exit_status.success()),
            |code| code as i32,
        );
        TestExecutionResult {
            exit_code,
            output: output.stdout_str(),
        }
    }
}
