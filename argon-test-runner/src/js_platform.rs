use crate::{
    CompileTargetPlatform, LibraryInfo, TestContext, TestExecutionResult,
    cmd::{CommandRunner, CommandRunnerPlatform},
};
use argon_runner::local_io::{LocalInputFile, LocalOutputDirectory, LocalOutputFile, StdIoWrite};
use fs_extra::dir::CopyOptions;
use hashbrown::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

pub struct JSPlatform;

#[derive(Default)]
pub struct JSPlatformState {
    library_codegen: Mutex<HashMap<String, PathBuf>>,
}

pub struct JsPlatformMetadataOptions<I, O> {
    pub extern_files: Vec<I>,
    pub output_file: O,
}

pub struct JsCodeGenOptions<I, O> {
    pub input_file: I,
    pub output_dir: O,
    pub executable: Option<String>,
}

impl JSPlatform {
    fn codegen_library<R: CommandRunner + CommandRunnerPlatform<Self>>(
        &self,
        library_info: &LibraryInfo<Self, R>,
    ) -> PathBuf {
        let mut tube_map = library_info
            .test_suite_context
            .platform_state
            .library_codegen
            .lock()
            .unwrap_or_else(|e| e.into_inner());

        let context = library_info.test_suite_context.clone();

        tube_map
            .entry_ref(&library_info.name)
            .or_insert_with(|| {
                let library = context.library_info(&library_info.name);

                let input_file = library
                    .test_suite_context
                    .clone()
                    .genir_library_tube(&library_info.name);
                let output_dir = library.library_output_path.join("js");
                std::fs::create_dir_all(&output_dir).unwrap();

                let mut output = Vec::new();
                let success = library.test_suite_context.command_runner.codegen(
                    JsCodeGenOptions {
                        input_file: LocalInputFile::new(input_file),
                        output_dir: LocalOutputDirectory::new(output_dir.clone()),
                        executable: None,
                    },
                    &mut StdIoWrite::new(&mut output),
                );

                assert!(
                    success,
                    "Code generation of library {} failed\n{}",
                    library.name,
                    String::from_utf8_lossy(&output),
                );

                if library.test_suite_context.print_commands && !output.is_empty() {
                    print!("{}", String::from_utf8_lossy(&output));
                }

                output_dir
            })
            .clone()
    }
}

impl CompileTargetPlatform for JSPlatform {
    type PlatformState = JSPlatformState;
    type PlatformMetadataOptions<I, O> = JsPlatformMetadataOptions<I, O>;
    type CodeGenOptions<I, O> = JsCodeGenOptions<I, O>;

    const ID: &'static str = "js";

    fn library_platform_metadata<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        library: &LibraryInfo<Self, R>,
    ) -> Vec<PathBuf> {
        let path = library.library_output_path.join("platform-metadata.esx");

        let mut extern_files = Vec::new();
        let js_dir = library.library_path.join("js");
        if js_dir.exists() {
            for entry in std::fs::read_dir(js_dir).unwrap() {
                let entry = entry.unwrap();
                let path = entry.path();
                if path.is_file() {
                    extern_files.push(LocalInputFile::new(path));
                }
            }
        }

        let mut output = Vec::new();
        let success = library.test_suite_context.command_runner.platform_metadata(
            JsPlatformMetadataOptions {
                extern_files,
                output_file: LocalOutputFile::new(path.clone()),
            },
            &mut StdIoWrite::new(&mut output),
        );

        assert!(
            success,
            "Getting metadata for library {} failed:\n{}",
            library.name,
            String::from_utf8_lossy(&output),
        );

        if library.test_suite_context.print_commands && !output.is_empty() {
            print!("{}", String::from_utf8_lossy(&output));
        }

        vec![path]
    }

    fn codegen<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        test_context: &TestContext<Self, R>,
    ) {
        let output_dir = test_context.test_data_dir.join("js");
        std::fs::create_dir_all(&output_dir).unwrap();

        {
            let runtime_dir = test_context
                .test_suite_context
                .backend_dir
                .join("js/runtime");
            let runtime_module_dir = output_dir.join("node_modules/@argon-lang/runtime");
            std::fs::create_dir_all(&runtime_module_dir).unwrap();

            let mut options = CopyOptions::new();
            options.content_only = true;
            fs_extra::dir::copy(&runtime_dir, &runtime_module_dir, &options).unwrap();
        }

        for library_name in test_context.referenced_libraries() {
            let lib_dir = self.codegen_library(
                &test_context
                    .test_suite_context
                    .clone()
                    .library_info(&library_name),
            );

            let lib_module_dir =
                output_dir.join(format!("node_modules/@argon-tube/{}", library_name));
            std::fs::create_dir_all(&lib_module_dir).unwrap();

            let mut options = CopyOptions::new();
            options.content_only = true;
            fs_extra::dir::copy(&lib_dir, &lib_module_dir, &options).unwrap();
        }

        let input_file = test_context.genir_test_case();

        let mut output = Vec::new();
        let success = test_context.test_suite_context.command_runner.codegen(
            JsCodeGenOptions {
                input_file: LocalInputFile::new(input_file),
                output_dir: LocalOutputDirectory::new(output_dir.clone()),
                executable: Some("test-program".to_owned()),
            },
            &mut StdIoWrite::new(&mut output),
        );

        assert!(
            success,
            "Code generation of test case failed\n{}",
            String::from_utf8_lossy(&output),
        );

        if test_context.test_suite_context.print_commands && !output.is_empty() {
            print!("{}", String::from_utf8_lossy(&output));
        }
    }

    fn run<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        test_context: &TestContext<Self, R>,
    ) -> TestExecutionResult {
        let main_path = test_context.test_data_dir.join("js/main.js");

        let output = subprocess::Exec::cmd("node")
            .arg(&main_path)
            .stderr(subprocess::Redirection::Merge)
            .capture()
            .unwrap();

        let exit_code = output
            .exit_status
            .code()
            .map(|code| code as i32)
            .unwrap_or_else(|| if output.exit_status.success() { 0 } else { 1 });

        TestExecutionResult {
            exit_code,
            output: output.stdout_str(),
        }
    }
}
