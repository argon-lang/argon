use crate::{
    CompileTargetPlatform, LibraryInfo, TestContext, TestExecutionResult,
    cmd::{CommandRunner, CommandRunnerPlatform},
};
use argon_runner::local_io::{LocalInputFile, LocalOutputDirectory, LocalOutputFile, StdIoWrite};
use hashbrown::HashMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};

pub struct JVMPlatform;

#[derive(Default)]
pub struct JVMPlatformState {
    library_codegen: Mutex<HashMap<String, PathBuf>>,
    extern_codegen: Mutex<HashMap<String, Vec<PathBuf>>>,
}

pub struct JvmPlatformMetadataOptions<I, O> {
    pub extern_files: Vec<I>,
    pub output_file: O,
}

pub struct JvmCodeGenOptions<I, O> {
    pub input_file: I,
    pub output_dir: O,
    pub output_name: String,
    pub executable: bool,
}

impl JVMPlatform {
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
                let output_dir = library.library_output_path.join("jvm");
                std::fs::create_dir_all(&output_dir).unwrap();

                let mut output = Vec::new();
                let output_name = format!("{}.jar", library.name);
                let success = library.test_suite_context.command_runner.codegen(
                    JvmCodeGenOptions {
                        input_file: LocalInputFile::new(input_file),
                        output_dir: LocalOutputDirectory::new(output_dir.clone()),
                        output_name: output_name.clone(),
                        executable: false,
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

                output_dir.join(output_name)
            })
            .clone()
    }

    fn compile_externs<R: CommandRunner + CommandRunnerPlatform<Self>>(
        &self,
        library: &LibraryInfo<Self, R>,
    ) -> Vec<PathBuf> {
        let mut extern_map = library
            .test_suite_context
            .platform_state
            .extern_codegen
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        if let Some(files) = extern_map.get(&library.name) {
            return files.clone();
        }

        let extern_source_dir = library.library_path.join("jvm/extern-generators");
        let output_dir = library.library_output_path.join("jvm/externs");
        let mut command = Command::new(
            library
                .test_suite_context
                .backend_dir
                .join("jvm/extern-compiler/build/install/extern-compiler/bin/extern-compiler"),
        );
        command
            .arg("--sources")
            .arg(&extern_source_dir)
            .arg("--output")
            .arg(&output_dir);

        let output = command.output().unwrap_or_else(|err| {
            panic!(
                "failed to run JVM extern compiler {}: {}",
                format_command(&command),
                err
            )
        });

        if !output.status.success() {
            panic!(
                "JVM extern compilation failed: {}\n{}{}",
                format_command(&command),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            );
        }

        if !output_dir.is_dir() {
            extern_map.insert(library.name.clone(), Vec::new());
            return Vec::new();
        }

        let mut class_files = walkdir::WalkDir::new(&output_dir)
            .into_iter()
            .map(|entry| entry.unwrap())
            .filter(|entry| entry.file_type().is_file())
            .map(|entry| entry.into_path())
            .filter(|path| path.extension() == Some(OsStr::new("class")))
            .collect::<Vec<_>>();
        class_files.sort();
        extern_map.insert(library.name.clone(), class_files.clone());
        class_files
    }
}

impl CompileTargetPlatform for JVMPlatform {
    type PlatformState = JVMPlatformState;
    type PlatformMetadataOptions<I, O> = JvmPlatformMetadataOptions<I, O>;
    type CodeGenOptions<I, O> = JvmCodeGenOptions<I, O>;

    const ID: &'static str = "jvm";

    fn library_platform_metadata<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        library: &LibraryInfo<Self, R>,
    ) -> Vec<PathBuf> {
        let path = library.library_output_path.join("platform-metadata.esx");
        let extern_files = self
            .compile_externs(library)
            .into_iter()
            .map(LocalInputFile::new)
            .collect();

        let mut output = Vec::new();
        let success = library.test_suite_context.command_runner.platform_metadata(
            JvmPlatformMetadataOptions {
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
        let output_dir = test_context.test_data_dir.join("jvm");
        std::fs::create_dir_all(&output_dir).unwrap();

        for library_name in test_context.referenced_libraries() {
            self.codegen_library(
                &test_context
                    .test_suite_context
                    .clone()
                    .library_info(&library_name),
            );
        }

        let input_file = test_context.genir_test_case();

        let mut output = Vec::new();
        let success = test_context.test_suite_context.command_runner.codegen(
            JvmCodeGenOptions {
                input_file: LocalInputFile::new(input_file),
                output_dir: LocalOutputDirectory::new(output_dir),
                output_name: "Argon.TestCase.jar".to_owned(),
                executable: true,
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
        let runtime_path = jvm_runtime_path(&test_context.test_suite_context.backend_dir);
        let test_jar = test_context
            .test_data_dir
            .join("jvm")
            .join("Argon.TestCase.jar");
        let mut module_path = vec![runtime_path];

        for library_name in test_context.referenced_libraries() {
            let jar_path = self.codegen_library(
                &test_context
                    .test_suite_context
                    .clone()
                    .library_info(&library_name),
            );
            module_path.push(jar_path);
        }
        module_path.push(test_jar);

        let module_path = std::env::join_paths(module_path).unwrap();
        let output = subprocess::Exec::cmd("java")
            .cwd(test_context.run_dir())
            .arg("--module-path")
            .arg(module_path)
            .arg("--module")
            .arg("argontube2.Argon.TestCase/argontube2.Argon.TestCase.Main")
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

fn jvm_runtime_path(backend_dir: &Path) -> PathBuf {
    backend_dir.join("jvm/runtime/build/libs/runtime-0.1.0.jar")
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
